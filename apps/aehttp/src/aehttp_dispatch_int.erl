-module(aehttp_dispatch_int).

-export([handle_request/3]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('PostSpendTx', #{'SpendTx' := SpendTxObj}, _Context) ->
    case aec_keys:pubkey() of
        {ok, SenderPubkey} ->
            lager:debug("SenderPubKey matches ours", []),
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
                    lager:debug("SpendTx = ~p", [SpendTx]),
                    {ok, SignedTx} = aec_keys:sign(SpendTx),
                    ok = aec_tx_pool:push(SignedTx),
                    lager:debug("pushed; peek() -> ~p", [aec_tx_pool:peek(10)]),
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

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.
