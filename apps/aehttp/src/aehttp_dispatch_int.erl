-module(aehttp_dispatch_int).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: map(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: map()}.

handle_request('PostSpendTx', #{'SpendTx' := SpendTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, SenderPubkey, Nonce} ->
            #{<<"recipient_pubkey">> := EncodedRecipientPubkey,
              <<"amount">>           := Amount,
              <<"fee">>              := Fee} = SpendTxObj,
            case aec_conductor:resolve_name(account_pubkey, EncodedRecipientPubkey) of
                {ok, DecodedRecipientPubkey} ->
                    {ok, SpendTx} =
                        aec_spend_tx:new(
                          #{sender    => SenderPubkey,
                            recipient => DecodedRecipientPubkey,
                            amount    => Amount,
                            fee       => Fee,
                            nonce     => Nonce}),
                    sign_and_push_to_mempool(SpendTx),
                    {200, [], #{}};
                {error, __} ->
                    {404, [], #{reason => <<"Invalid key">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleRegisterTx', #{'OracleRegisterTx' := OracleRegisterTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, Pubkey, Nonce} ->
            #{<<"query_format">>    := QueryFormat,
              <<"response_format">> := ResponseFormat,
              <<"query_fee">>       := QueryFee,
              <<"fee">>             := Fee,
              <<"ttl">>             := TTL} = OracleRegisterTxObj,
            TTLType = binary_to_existing_atom(maps:get(<<"type">>, TTL), utf8),
            TTLValue = maps:get(<<"value">>, TTL),
            {ok, OracleRegisterTx} =
                aeo_register_tx:new(
                  #{account       => Pubkey,
                    nonce         => Nonce,
                    query_spec    => QueryFormat,
                    response_spec => ResponseFormat,
                    query_fee     => QueryFee,
                    ttl           => {TTLType, TTLValue},
                    fee           => Fee}),
            sign_and_push_to_mempool(OracleRegisterTx),
            {200, [], #{oracle_id => aec_base58c:encode(oracle_pubkey, Pubkey)}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleQueryTx', #{'OracleQueryTx' := OracleQueryTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, Pubkey, Nonce} ->
            #{<<"oracle_pubkey">> := EncodedOraclePubkey,
              <<"query">>         := Query,
              <<"query_fee">>     := QueryFee,
              <<"query_ttl">>     := QueryTTL,
              <<"response_ttl">>  :=
                  #{<<"type">>    := <<"delta">>,
                    <<"value">>   := ResponseTTLValue},
              <<"fee">>           := Fee} = OracleQueryTxObj,
            QueryTTLType = binary_to_existing_atom(maps:get(<<"type">>, QueryTTL), utf8),
            QueryTTLValue= maps:get(<<"value">>, QueryTTL),
            case aec_conductor:resolve_name(oracle_pubkey, EncodedOraclePubkey) of
                {ok, DecodedOraclePubkey} ->
                    {ok, OracleQueryTx} =
                        aeo_query_tx:new(
                          #{sender       => Pubkey,
                            nonce        => Nonce,
                            oracle       => DecodedOraclePubkey,
                            query        => Query,
                            query_fee    => QueryFee,
                            query_ttl    => {QueryTTLType, QueryTTLValue},
                            response_ttl => {delta, ResponseTTLValue},
                            fee          => Fee}),
                    sign_and_push_to_mempool(OracleQueryTx),
                    QId = aeo_query:id(Pubkey, Nonce, DecodedOraclePubkey),
                    {200, [], #{query_id => aec_base58c:encode(oracle_query_id, QId)}};
                {error, _} ->
                    {404, [], #{reason => <<"Invalid key">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleResponseTx', #{'OracleResponseTx' := OracleResponseTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, Pubkey, Nonce} ->
            #{<<"query_id">> := EncodedQueryId,
              <<"response">> := Response,
              <<"fee">>      := Fee} = OracleResponseTxObj,
            case aec_base58c:safe_decode(oracle_query_id, EncodedQueryId) of
                {ok, DecodedQueryId} ->
                    {ok, OracleResponseTx} =
                        aeo_response_tx:new(
                          #{oracle   => Pubkey,
                            nonce    => Nonce,
                            query_id => DecodedQueryId,
                            response => Response,
                            fee      => Fee}),
                    sign_and_push_to_mempool(OracleResponseTx),
                    {200, [], #{query_id => EncodedQueryId}};
                {error, _} ->
                    {404, [], #{reason => <<"Invalid Query Id">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('GetActiveRegisteredOracles', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('GetOracleQuestions', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('PostOracleSubscribe', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('PostOracleUnsubscribe', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('PostNamePreclaimTx', #{'NamePreclaimTx' := NamePreclaimTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, PubKey, Nonce} ->
            #{<<"commitment">> := Commitment,
              <<"fee">>        := Fee} = NamePreclaimTxObj,
            case aec_base58c:safe_decode(commitment, Commitment) of
                {ok, DecodedCommitment} ->
                    {ok, PreclaimTx} =
                        aens_preclaim_tx:new(
                          #{account    => PubKey,
                            nonce      => Nonce,
                            commitment => DecodedCommitment,
                            fee        => Fee}),
                    sign_and_push_to_mempool(PreclaimTx),
                    {200, [], #{commitment => Commitment}};
                {error, _Reason} ->
                    {400, [], #{reason => <<"Invalid commitment hash">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {400, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostNameClaimTx', #{'NameClaimTx' := NameClaimTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, PubKey, Nonce} ->
            #{<<"name">>      := Name,
              <<"name_salt">> := NameSalt,
              <<"fee">>       := Fee} = NameClaimTxObj,
            {ok, ClaimTx} =
                aens_claim_tx:new(
                  #{account   => PubKey,
                    nonce     => Nonce,
                    name      => Name,
                    name_salt => NameSalt,
                    fee       => Fee}),
            sign_and_push_to_mempool(ClaimTx),
            {200, [], #{name_hash => aec_base58c:encode(name, aens_hash:name_hash(Name))}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {400, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostNameUpdateTx', #{'NameUpdateTx' := NameUpdateTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, PubKey, Nonce} ->
            #{<<"name_hash">> := NameHash,
              <<"name_ttl">>  := NameTTL,
              <<"pointers">>  := Pointers,
              <<"ttl">>       := TTL,
              <<"fee">>       := Fee} = NameUpdateTxObj,
            case aec_base58c:safe_decode(name, NameHash) of
                {ok, DecodedNameHash} ->
                    {ok, UpdateTx} =
                        aens_update_tx:new(
                          #{account   => PubKey,
                            nonce     => Nonce,
                            name_hash => DecodedNameHash,
                            name_ttl  => NameTTL,
                            pointers  => jsx:decode(Pointers),
                            ttl       => TTL,
                            fee       => Fee}),
                    sign_and_push_to_mempool(UpdateTx),
                    {200, [], #{name_hash => NameHash}};
                {error, _Reason} ->
                    {400, [], #{reason => <<"Invalid name hash">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {400, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostNameTransferTx', #{'NameTransferTx' := NameTransferTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, PubKey, Nonce} ->
            #{<<"name_hash">>        := NameHash,
              <<"recipient_pubkey">> := RecipientPubKey,
              <<"fee">>              := Fee} = NameTransferTxObj,
            case aec_base58c:safe_decode(name, NameHash) of
                {ok, DecodedNameHash} ->
                    case aec_base58c:safe_decode(account_pubkey, RecipientPubKey) of
                        {ok, DecodedRecipientPubKey} ->
                            {ok, TransferTx} =
                                aens_transfer_tx:new(
                                  #{account           => PubKey,
                                    nonce             => Nonce,
                                    name_hash         => DecodedNameHash,
                                    recipient_account => DecodedRecipientPubKey,
                                    fee               => Fee}),
                            sign_and_push_to_mempool(TransferTx),
                            {200, [], #{name_hash => NameHash}};
                        {error, _Reason} ->
                            {400, [], #{reason => <<"Invalid recipient pubkey">>}}
                    end;
                {error, _Reason} ->
                    {400, [], #{reason => <<"Invalid name hash">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {400, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostNameRevokeTx', #{'NameRevokeTx' := NameRevokeTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, PubKey, Nonce} ->
            #{<<"name_hash">> := NameHash,
              <<"fee">>       := Fee} = NameRevokeTxObj,
            case aec_base58c:safe_decode(name, NameHash) of
                {ok, DecodedNameHash} ->
                    {ok, RevokeTx} =
                        aens_revoke_tx:new(
                          #{account   => PubKey,
                            nonce     => Nonce,
                            name_hash => DecodedNameHash,
                            fee       => Fee}),
                    sign_and_push_to_mempool(RevokeTx),
                    {200, [], #{name_hash => NameHash}};
                {error, _Reason} ->
                    {400, [], #{reason => <<"Invalid name hash">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {400, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('GetPubKey', _, _Context) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            {200, [], #{pub_key => aec_base58c:encode(account_pubkey, Pubkey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('GetBlockNumber', _Req, _Context) ->
    TopHeader = aec_conductor:top_header(),
    Height = aec_headers:height(TopHeader),
    {200, [], #{height => Height}};

handle_request('GetBlockByHeightInternal', Req, _Context) ->
    Height = maps:get('height', Req),
    get_block(fun() -> aec_conductor:get_block_by_height(Height) end, Req);

handle_request('GetBlockByHashInternal', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block(fun() -> aec_conductor:get_block_by_hash(Hash) end, Req)
    end;

handle_request('GetBlockGenesis', Req, _Context) ->
    get_block(fun aec_conductor:genesis_block/0, Req);

handle_request('GetBlockLatest', Req, _Context) ->
    get_block(
        fun() ->
            TopBlock = aec_conductor:top(),
            {ok, TopBlock}
        end, Req);

handle_request('GetBlockPending', Req, _Context) ->
    get_block(fun aec_conductor:get_block_candidate/0, Req, false);

handle_request('GetBlockTxsCountByHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block_txs_count(fun() -> aec_conductor:get_block_by_hash(Hash) end,
                               Req)
    end;

handle_request('GetBlockTxsCountByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    get_block_txs_count(fun() -> aec_conductor:get_block_by_height(Height) end,
                       Req);

handle_request('GetGenesisBlockTxsCount', Req, _Context) ->
    get_block_txs_count(fun aec_conductor:genesis_block/0, Req);

handle_request('GetLatestBlockTxsCount', Req, _Context) ->
    get_block_txs_count(
        fun() ->
            TopBlock = aec_conductor:top(),
            {ok, TopBlock}
        end, Req);

handle_request('GetPendingBlockTxsCount', Req, _Context) ->
    get_block_txs_count(fun aec_conductor:get_block_candidate/0, Req);

handle_request('GetTransactionFromBlockHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    Index = maps:get('tx_index', Req),
    get_block_tx_by_index(fun() -> aec_conductor:get_block_by_height(Height) end,
                          Index,
                          Req);

handle_request('GetTransactionFromBlockHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            Index = maps:get('tx_index', Req),
            get_block_tx_by_index(
                fun() -> aec_conductor:get_block_by_hash(Hash) end,
                Index,
                Req)
    end;

handle_request('GetTransactionFromBlockLatest', Req, _Context) ->
    Index = maps:get('tx_index', Req),
    get_block_tx_by_index(
        fun() -> 
            TopBlock = aec_conductor:top(),
            {ok, TopBlock}
        end,
        Index,
        Req);

handle_request('GetTxsListFromBlockRangeByHeight', Req, _Context) ->
    HeightFrom = maps:get('from', Req),
    HeightTo = maps:get('to', Req),
    get_block_range(
        fun() ->
            aec_conductor:get_block_range_by_height(HeightFrom, HeightTo)
        end, Req);

handle_request('GetTxsListFromBlockRangeByHash', Req, _Context) ->
    case {aec_base58c:safe_decode(block_hash, maps:get('from', Req)),
          aec_base58c:safe_decode(block_hash, maps:get('to', Req))} of
        {{ok, HashFrom}, {ok, HashTo}} ->
            get_block_range(
                fun() ->
                    aec_conductor:get_block_range_by_hash(HashFrom, HashTo)
                end, Req);
        _ ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;
          

handle_request('GetAccountBalance', Req, _Context) ->
    case aec_base58c:safe_decode(account_pubkey, maps:get('account_pubkey', Req)) of
        {ok, AccountPubkey} ->
            case get_block_hash_optionally_by_hash_or_height(Req) of
                {error, not_found} ->
                    {404, [], #{reason => <<"Block not found">>}};
                {error, invalid_hash} ->
                    {400, [], #{reason => <<"Invalid block hash">>}};
                {error, blocks_mismatch} ->
                    {400, [], #{reason => <<"Invalid height and hash combination">>}};
                {ok, Hash} ->
                      case get_account_balance_at_hash(AccountPubkey, Hash) of
                          {error, account_not_found} ->
                              {404, [], #{reason => <<"Account not found">>}};
                          {error, not_on_main_chain} ->
                              {400, [], #{reason => <<"Block not on the main chain">>}};
                          {ok, Balance} ->
                              {200, [], #{balance => Balance}}
                      end
            end;
        _ ->
            {400, [], #{reason => <<"Invalid account hash">>}}
    end;
          
handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

%% Internals

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

get_block(Fun, Req) ->
    get_block(Fun, Req, true).

get_block(Fun, Req, AddHash) when is_function(Fun, 0) ->
    case get_block_from_chain(Fun) of
        {ok, Block} ->
            case read_tx_encoding_param(Req) of
                {error, Err} ->
                    Err;
                {ok, TxEncoding} ->
                    DataSchema =
                        case TxEncoding of
                            message_pack ->
                                <<"BlockWithMsgPackTxs">>;
                            json ->
                                <<"BlockWithJSONTxs">>
                        end,
                    Resp0 = aehttp_dispatch_ext:cleanup_genesis(
                              aec_blocks:serialize_client_readable(TxEncoding, Block)),
                    % we add swagger's definition name to the object so the
                    % result could be validated against the schema
                    Resp1 = Resp0#{data_schema => DataSchema},
                    Resp =
                        case AddHash of
                            true ->
                                {ok, Hash} = aec_blocks:hash_internal_representation(Block),
                                Resp1#{hash => aec_base58c:encode(block_hash, Hash)};
                            false ->
                                Resp1
                        end,
                    lager:debug("Resp = ~p", [pp(Resp)]),
                    {200, [], Resp}
              end;
        {_Code, _, _Reason} = Err ->
            Err
    end.

get_block_txs_count(Fun, Req) when is_function(Fun, 0) ->
    case get_block_from_chain(Fun) of
        {ok, Block} ->
            case filter_transaction_list(Req, aec_blocks:txs(Block)) of
                {ok, TxsList} ->
                    Count = length(TxsList),
                    {200, [], #{count => Count}};
                {error, unknown_type} ->
                    {400, [], #{reason => <<"Unknown transaction type">>}}
            end;
        {_Code, _, _Reason} = Err ->
            Err
    end.

filter_transaction_list(Req, TxList) ->
    case {parse_filter_param(tx_types, Req),
          parse_filter_param(exclude_tx_types, Req)} of
        {{error, unknown_type} = Err, _} ->
            Err;
        {_, {error, unknown_type} = Err} ->
            Err;
        {{ok, KeepTxTypes}, {ok, DropTxTypes}} ->
            Filtered =
                lists:filter(
                    fun(SignedTx) ->
                        Tx = aec_tx_sign:data(SignedTx), 
                        Mod = aec_tx_dispatcher:handler(Tx),
                        TxType = Mod:type(),
                        Drop = lists:member(TxType, DropTxTypes),
                        Keep = KeepTxTypes =:= []
                            orelse lists:member(TxType, KeepTxTypes),
                        Keep andalso not Drop
                    end,
                    TxList),
            {ok, Filtered}
    end.

-spec parse_filter_param(atom(), map()) -> {ok, list()} | {error, unknown_type}.
parse_filter_param(ParamName, Req) when is_atom(ParamName) ->
    Vals = binary:split(
        read_optional_param(ParamName, Req, <<>>),
        [<<",">>],
        [global]),
    case Vals =:= [<<>>] of
        false ->
            KnownTypes =
                lists:filter(
                    fun(TxType) ->
                        try aec_tx_dispatcher:handler_by_type(TxType) of
                            _ -> true
                        catch
                            error:function_clause -> false
                        end
                    end,
                    Vals),
            case length(Vals) =:= length(KnownTypes) of
                true -> {ok, KnownTypes};
                false -> {error, unknown_type}
            end;
        true ->
            {ok, []}
    end.

get_block_tx_by_index(Fun, Index, Req) when is_function(Fun, 0) ->
    case get_block_from_chain(Fun) of
        {ok, Block} ->
            Txs = aec_blocks:txs(Block),
            case try {ok, lists:nth(Index, Txs)} catch _:_ -> not_found end of
                not_found ->
                    {404, [], #{reason => <<"Transaction not found">>}};
                {ok, Tx} ->
                    case read_tx_encoding_param(Req) of
                        {error, Err} ->
                            Err;
                        {ok, TxEncoding} ->
                            DataSchema =
                                case TxEncoding of
                                    json ->
                                        <<"SingleTxJSON">>;
                                    message_pack ->
                                        <<"SingleTxMsgPack">>
                                end,
                            H = aec_blocks:to_header(Block),
                            {200, [], #{transaction => aec_tx_sign:serialize_for_client(TxEncoding, H, Tx),
                                        data_schema => DataSchema}}
                    end
            end;
        {_Code, _, _Reason} = Err ->
            Err
    end.

get_block_from_chain(Fun) when is_function(Fun, 0) ->
    case Fun() of
        {ok, _Block} = OK->
            OK;
        {error, no_top_header} ->
            {404, [], #{reason => <<"No top header">>}};
        {error, block_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, chain_too_short} ->
            {404, [], #{reason => <<"Chain too short">>}};
        {error, miner_starting} ->
            {404, [], #{reason => <<"Starting mining, pending block not available yet">>}};
        {error, not_mining} ->
            {404, [], #{reason => <<"Not mining, no pending block">>}}
    end.

-spec read_tx_encoding_param(map()) -> {ok, json | message_pack} |
                                       {error, {integer(), list(), map()}}.
read_tx_encoding_param(Req) ->
    case read_optional_enum_param(tx_encoding, Req, message_pack,
                                  [message_pack, json]) of
        {error, unexpected_value} ->
            {error, {404, [], #{reason => <<"Unsupported transaction encoding">>}}};
        {ok, Encoding} ->
            {ok, Encoding}
    end.
            

-spec read_optional_enum_param(atom(), map(), term(), list()) -> {ok, term()} |
                                                                 {error, atom()}.
read_optional_enum_param(Key, Req, Default, Enums) ->
    Val = read_optional_param(Key, Req, Default),
    case lists:member(Val, Enums) of
        true -> {ok, Val};
        false -> {error, unexpected_value}
    end.

-spec read_optional_param(atom(), map(), term()) -> term().
read_optional_param(Key, Req, Default) ->
    %% swagger does not take into consideration the 'default'
    %% if a query param is missing, swagger adds it to Req with a value of
    %% 'undefined'
    case maps:get(Key, Req) of
        undefined -> Default;
        Val -> Val
    end.

get_block_range(GetFun, Req) when is_function(GetFun, 0) ->
    case read_tx_encoding_param(Req) of
        {error, Err} ->
            Err;
        {ok, TxEncoding} ->
            DataSchema =
                case TxEncoding of
                    json ->
                        <<"JSONTxs">>;
                    message_pack ->
                        <<"MsgPackTxs">>
                end,
            case GetFun() of
                {error, invalid_range} ->
                    {400, [], #{reason => <<"From's height is bigger than To's">>}};
                {error, range_too_big} ->
                    {400, [], #{reason => <<"Range too big">>}};
                {error, chain_too_short} ->
                    {404, [], #{reason => <<"Chain too short">>}};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}};
                {error, not_found} ->
                    {404, [], #{reason => <<"Block not found">>}};
                {ok, Blocks} ->
                    Txs = lists:foldl(
                        fun(_Block, {error, _} = Err) ->
                            Err;
                           (Block, Accum) ->
                            case filter_transaction_list(Req, aec_blocks:txs(Block)) of
                                {error, unknown_type} = Err ->
                                    Err;
                                {ok, FilteredTxs} ->
                                    H = aec_blocks:to_header(Block),
                                    lists:map(
                                        fun(Tx) ->
                                            aec_tx_sign:serialize_for_client(TxEncoding, H, Tx)
                                        end,
                                        FilteredTxs) ++ Accum
                            end
                        end,
                        [],
                        Blocks),
                    case Txs of
                        {error, unknown_type} ->
                            {400, [], #{reason => <<"Unknown transaction type">>}};
                        _ ->
                            {200, [], #{transactions => Txs,
                                        data_schema => DataSchema}}
                    end
              end

    end.

get_account_balance_at_hash(AccountPubkey, Hash) ->
    case aec_db:find_block_state(Hash) of
        none ->
            {error, not_on_main_chain};
        {value, Trees} ->
            AccountsMPTree = aec_trees:accounts(Trees),
            case aec_accounts_trees:lookup(AccountPubkey, AccountsMPTree) of
                none ->
                    {error, account_not_found};
                {value, Account} ->
                    {ok, aec_accounts:balance(Account)}
            end
    end.

-spec get_block_hash_optionally_by_hash_or_height(map()) ->
    {ok, binary()} | {error, not_found | invalid_hash | blocks_mismatch}.
get_block_hash_optionally_by_hash_or_height(Req) ->
    GetHashByHeight =
        fun(Height) ->
            case aec_conductor:get_header_by_height(Height) of 
                {error, chain_too_short} ->
                    {error, not_found};
                {ok, Header} ->
                    {ok, _Hash} = aec_headers:hash_header(Header) 
            end
        end,
    case {maps:get('height', Req), maps:get('hash', Req)} of
        {undefined, undefined} ->
            {ok, aec_conductor:top_block_hash()};
        {undefined, EncodedHash} ->
            case aec_base58c:safe_decode(block_hash, EncodedHash) of
                {error, _} ->
                    {error, invalid_hash};
                {ok, Hash} ->
                    case aec_conductor:get_header_by_hash(Hash) of 
                        {error, header_not_found} ->
                            {error, not_found};
                        {ok, _} -> % we do have header
                            {ok, Hash}
                    end
            end;
        {Height, undefined} ->
            GetHashByHeight(Height);
        {Height, EncodedHash} ->
            case GetHashByHeight(Height) of
                {error, _} = Err ->
                    Err;
                {ok, Hash} -> % ensure it is the same hash
                    case aec_base58c:safe_decode(block_hash, EncodedHash) of
                        {error, _} ->
                            {error, invalid_hash};
                        {ok, Hash} -> % same hash
                            {ok, Hash};
                        {ok, _OtherHash} ->
                            {error, blocks_mismatch}
                    end
            end
    end.
