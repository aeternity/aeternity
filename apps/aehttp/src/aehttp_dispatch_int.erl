-module(aehttp_dispatch_int).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('PostSpendTx', #{'SpendTx' := SpendTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, SenderPubkey, Nonce} ->
            #{<<"recipient_pubkey">> := EncodedRecipientPubkey,
              <<"amount">>           := Amount,
              <<"fee">>              := Fee} = SpendTxObj,
            case safe_decode(account_pubkey, EncodedRecipientPubkey) of
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
            {200, [], #{}};
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
            case safe_decode(oracle_pubkey, EncodedOraclePubkey) of
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
                    {200, [], #{}};
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
            #{<<"interaction_id">> := InteractionId,
              <<"response">>       := Response,
              <<"fee">>            := Fee} = OracleResponseTxObj,
            {ok, OracleResponseTx} =
                aeo_response_tx:new(
                  #{oracle         => Pubkey,
                    nonce          => Nonce,
                    interaction_id => InteractionId,
                    response       => Response,
                    fee            => Fee}),
            sign_and_push_to_mempool(OracleResponseTx),
            {200, [], #{}};
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

handle_request('GetPubKey', _, _Context) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            {200, [], #{pub_key => aec_base58c:encode(account_pubkey, Pubkey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
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

safe_decode(Type, Enc) ->
    try aec_base58c:decode(Enc) of
        {Type, Dec} ->
            {ok, Dec};
        {_, _} ->
            {error, invalid_prefix}
    catch
        error:_ ->
            {error, invalid_encoding}
    end.
