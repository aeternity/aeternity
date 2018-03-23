-module(aehttp_int_tx_logic).

-import(aeu_debug, [pp/1]).

-export([sender_and_hash/1]).

-export([ spend/3
       ]).

-export([ oracle_register/6
        , oracle_extend/3
        , oracle_query/7
        , oracle_response/3
        , get_oracles/2
        , get_oracle_questions/3
       ]).

-export([ name_preclaim/2
        , name_claim/3
        , name_update/5
        , name_transfer/3
        , name_revoke/2
       ]).

sender_and_hash(Tx) ->
    Sender = aetx:origin(Tx), 
    TxHash = aetx:hash(Tx),
    {Sender, TxHash}.

spend(EncodedRecipient, Amount, Fee) ->
    create_tx(
        fun(SenderPubkey, Nonce) ->
            case aec_chain:resolve_name(account_pubkey, EncodedRecipient) of
                {ok, DecodedRecipientPubkey} ->
                    aec_spend_tx:new(
                      #{sender    => SenderPubkey,
                        recipient => DecodedRecipientPubkey,
                        amount    => Amount,
                        fee       => Fee,
                        nonce     => Nonce});
                {error, _} ->
                    {error, invalid_key}
            end
        end).

oracle_register(QueryFormat, ResponseFormat, QueryFee, Fee, TTLType, TTLValue) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            aeo_register_tx:new(
              #{account       => Pubkey,
                nonce         => Nonce,
                query_spec    => QueryFormat,
                response_spec => ResponseFormat,
                query_fee     => QueryFee,
                ttl           => {TTLType, TTLValue},
                fee           => Fee})
          end).

oracle_extend(Fee, TTLType, TTLValue) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            aeo_extend_tx:new(
              #{oracle => Pubkey,
                nonce  => Nonce,
                ttl    => {TTLType, TTLValue},
                fee    => Fee})
          end).

oracle_query(EncodedOraclePubkey, Query, QueryFee, QueryTTLType,
             QueryTTLValue, ResponseTTLValue, Fee) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            case aec_chain:resolve_name(oracle_pubkey, EncodedOraclePubkey) of
                {ok, DecodedOraclePubkey} ->
                    {ok, Tx} =
                        aeo_query_tx:new(
                          #{sender       => Pubkey,
                            nonce        => Nonce,
                            oracle       => DecodedOraclePubkey,
                            query        => Query,
                            query_fee    => QueryFee,
                            query_ttl    => {QueryTTLType, QueryTTLValue},
                            response_ttl => {delta, ResponseTTLValue},
                            fee          => Fee}),
                    QId = aeo_query:id(Pubkey, Nonce, DecodedOraclePubkey),
                    {ok, Tx, QId};
                {error, _} -> {error, invalid_key}
              end
          end).

oracle_response(DecodedQueryId, Response, Fee) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            aeo_response_tx:new(
              #{oracle   => Pubkey,
                nonce    => Nonce,
                query_id => DecodedQueryId,
                response => Response,
                fee      => Fee})
          end).

get_oracles(From, Max) ->
    {ok, Oracles} = aec_chain:get_oracles(From, Max),
    FmtOracles =
        lists:map(
            fun(O) -> #{<<"address">> => aeo_oracles:id(O),
                        query_format => aeo_oracles:query_format(O),
                        response_format => aeo_oracles:response_format(O),
                        query_fee => aeo_oracles:query_fee(O),
                        expires_at => aeo_oracles:expires(O)}
            end,
            Oracles),
    {ok, FmtOracles}.

get_oracle_questions(OracleId, From, Max) ->
    {ok, Queries} = aec_chain:get_open_oracle_queries(OracleId, From, Max),
    FmtQueries =
        lists:map(
            fun(Q) -> #{<<"query_id">> => aeo_query:id(Q),
                        query => aeo_query:query(Q),
                        query_fee => aeo_query:fee(Q),
                        expires_at => aeo_query:expires(Q)}
            end,
            Queries),
    {ok, FmtQueries}.

name_preclaim(DecodedCommitment, Fee) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            aens_preclaim_tx:new(
              #{account    => Pubkey,
                nonce      => Nonce,
                commitment => DecodedCommitment,
                fee        => Fee})
          end).

name_claim(Name, NameSalt, Fee) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            case aens:get_name_hash(Name) of
                {ok, NameHash} ->
                    {ok, Tx} =
                        aens_claim_tx:new(
                          #{account   => Pubkey,
                            nonce     => Nonce,
                            name      => Name,
                            name_salt => NameSalt,
                            fee       => Fee}),
                    {ok, Tx, NameHash};
                {error, _Reason} = Err -> Err
            end
          end).

name_update(DecodedNameHash, NameTTL, Pointers, TTL, Fee) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            aens_update_tx:new(
              #{account   => Pubkey,
                nonce     => Nonce,
                name_hash => DecodedNameHash,
                name_ttl  => NameTTL,
                pointers  => jsx:decode(Pointers),
                ttl       => TTL,
                fee       => Fee})
          end).

name_transfer(DecodedNameHash, DecodedRecipientPubKey, Fee) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            aens_transfer_tx:new(
              #{account           => Pubkey,
                nonce             => Nonce,
                name_hash         => DecodedNameHash,
                recipient_account => DecodedRecipientPubKey,
                fee               => Fee})
          end).

name_revoke(DecodedNameHash, Fee) ->
    create_tx(
        fun(Pubkey, Nonce) ->
            aens_revoke_tx:new(
              #{account   => Pubkey,
                nonce     => Nonce,
                name_hash => DecodedNameHash,
                fee       => Fee})
          end).

%% Internals
%%
create_tx(TxFun) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, Pubkey, Nonce} ->
            case TxFun(Pubkey, Nonce) of
                {error, _} = Err -> Err;
                {ok, Tx} ->
                    sign_and_push_to_mempool(Tx),
                    {ok, Tx};
                {ok, Tx, Result} ->
                    sign_and_push_to_mempool(Tx),
                    {ok, Tx, Result}
            end;
        {error, _} = Err -> Err
    end.

-spec get_local_pubkey_with_next_nonce() -> {ok, binary(), integer()} |
                              {error, account_not_found | key_not_found}.
get_local_pubkey_with_next_nonce() ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            lager:debug("SenderPubKey matches ours"),
            case aec_next_nonce:pick_for_account(Pubkey) of
                {ok, Nonce} ->
                    lager:debug("Nonce = ~p", [Nonce]),
                    {ok, Pubkey, Nonce};
                {error, account_not_found} = Error ->
                    lager:debug("Account not found"),
                    %% Account was not found in state trees
                    %% so effectively there have been no funds
                    %% ever granted to it.
                    Error
            end;
        {error, key_not_found} = Error ->
            Error
    end.

sign_and_push_to_mempool(Tx) ->
    lager:debug("Tx = ~p", [pp(Tx)]),
    {ok, SignedTx} = aec_keys:sign(Tx),
    ok = aec_tx_pool:push(SignedTx),
    lager:debug("pushed; peek() -> ~p",
                [pp(aec_tx_pool:peek(10))]).

