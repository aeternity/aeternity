-module(aehttp_dispatch_int).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('PostSpendTx', #{'SpendTx' := SpendTxObj}, _Context) ->
    case aec_keys:pubkey() of
        {ok, SenderPubkey} ->
            lager:debug("SenderPubKey matches ours"),
            RecipientPubkey = base64:decode(maps:get(<<"recipient_pubkey">>,
                                                     SpendTxObj)),
            Amount = maps:get(<<"amount">>, SpendTxObj),
            Fee = maps:get(<<"fee">>, SpendTxObj),
            case aec_next_nonce:pick_for_account(SenderPubkey) of
                {ok, Nonce} ->
                    lager:debug("Nonce = ~p", [Nonce]),
                    {ok, SpendTx} =
                        aec_spend_tx:new(
                          #{sender => SenderPubkey,
                            recipient => RecipientPubkey,
                            amount => Amount,
                            fee => Fee,
                            nonce => Nonce}),
                    sign_and_push_to_mempool(SpendTx),
                    {200, [], #{}};
                {error, account_not_found} ->
                    lager:debug("account not found", []),
                    %% Account was not found in state trees
                    %% so effectively there have been no funds
                    %% ever granted to it.
                    {404, [], #{reason => <<"No funds in an account">>}}
            end;
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleRegisterTx', #{'OracleRegisterTx' := OracleRegisterTxObj}, _Context) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            lager:debug("SenderPubKey matches ours"),
            case aec_next_nonce:pick_for_account(Pubkey) of
                {ok, Nonce} ->
                    lager:debug("Nonce = ~p", [Nonce]),
                    QueryFormat = maps:get(<<"query_format">>, OracleRegisterTxObj),
                    ResponseFormat = maps:get(<<"response_format">>, OracleRegisterTxObj),
                    QueryFee = maps:get(<<"query_fee">>, OracleRegisterTxObj),
                    Fee = maps:get(<<"fee">>, OracleRegisterTxObj),
                    TTL = maps:get(<<"ttl">>, OracleRegisterTxObj),
                    TTLType = binary_to_atom(maps:get(<<"type">>, TTL), utf8),
                    TTLValue = maps:get(<<"value">>, TTL),
                    {ok, OracleRegisterTx} =
                        aeo_register_tx:new(
                          #{account => Pubkey,
                            nonce => Nonce,
                            query_spec => QueryFormat,
                            response_spec => ResponseFormat,
                            query_fee => QueryFee,
                            ttl => {TTLType, TTLValue},
                            fee => Fee}),
                    sign_and_push_to_mempool(OracleRegisterTx),
                    {200, [], #{}};
                {error, account_not_found} ->
                    {404, [], #{reason => <<"No funds in an account">>}}
            end;
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleQueryTx', _Req, _Context) ->
    %% TODO: Implement me
    {200, [], #{}};

handle_request('PostOracleResponseTx', _Req, _Context) ->
    %% TODO: Implement me
    {200, [], #{}};

handle_request('GetPubKey', _, _Context) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            {200, [], #{pub_key => base64:encode(Pubkey)}};
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
sign_and_push_to_mempool(Tx) ->
    lager:debug("Tx = ~p", [pp(Tx)]),
    {ok, SignedTx} = aec_keys:sign(Tx),
    ok = aec_tx_pool:push(SignedTx),
    lager:debug("pushed; peek() -> ~p",
                [pp(aec_tx_pool:peek(10))]).
