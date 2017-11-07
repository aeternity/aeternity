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
            RecipientPubkey = maps:get(<<"recipient_pubkey">>, SpendTxObj),
            Amount = maps:get(<<"amount">>, SpendTxObj),
            Fee = maps:get(<<"fee">>, SpendTxObj),

            {ok, _SpendTx} = aec_spend_tx:new(
                #{sender => SenderPubkey,
                    recipient => RecipientPubkey,
                    amount => Amount,
                    fee => Fee,
                    nonce => 10}), %% To be changed in PT-152543561
            %% TODO: sign and push to the mempool
            {200, [], #{}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"keys not configured">>}}
    end;

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
        ">>> Got not implemented request to process: ~p~n",
        [{OperationID, Req, Context}]
    ),
    {501, [], #{}}.
