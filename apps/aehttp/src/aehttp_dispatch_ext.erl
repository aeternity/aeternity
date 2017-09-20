-module(aehttp_dispatch_ext).

-export([handle_request/3]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('Ping', _, _Context) ->
    {200, [], #{pong => <<"pong">>}};

handle_request('GetTop', _, _Context) ->
    {ok, Header} = aec_chain:top_header(),
    {ok, Resp} = aec_headers:serialize_to_map(Header),
    {200, [], Resp};

handle_request('GetBlock', Req, _Context) ->
    Hash = maps:get('BlockHash', Req),
    {ok, Block} = aec_chain:get_block_by_hash(aeu_hex:hex_to_bin(Hash)),
    {ok, Resp} = aec_blocks:serialize_for_network(Block),
    {200, [], Resp};

handle_request('PutBlock', Req, _Context) ->
    _Block = maps:get('Block', Req),
    %TODO: verification and absorbe code
    {200, [], #{}};

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

