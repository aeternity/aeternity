-module(aehttp_dispatch_ext).

-export([handle_request/3]).

-compile({parse_transform, lager_transform}).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('Ping', #{source := Source,
			 share  := Share}, _Context) ->
    aec_peers:update_last_seen(Source),
    Ok = #{pong => <<"pong">>},
    Res = case mk_num(Share) of
	      N when is_integer(N), N > 0 ->
		  Peers = aec_peers:get_random(N, [Source]),
                  PeerUris = [iolist_to_binary(aec_peers:uri(P))
                              || P <- Peers],
		  lager:debug("PeerUris = ~p~n", [PeerUris]),
		  Ok#{peers => PeerUris};
	      _ ->
		  Ok
	  end,
    {200, [], Res};

handle_request('GetTop', _, _Context) ->
    {ok, Header} = aec_chain:top_header(),
    {ok, HH} = aec_headers:hash_header(Header),
    {ok, Resp} = aec_headers:serialize_to_map(Header),
    {200, [], maps:put(hash, base64:encode(HH), Resp)};

handle_request('GetBlockByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    case aec_chain:get_block_by_height(Height) of
        {ok, Block} ->
            %% swagger generated code expects the Resp to be proplist or map
            %% and always runs jsx:encode/1 on it - even if it is already
            %% encoded to a binary; that's why we use
            %% aec_blocks:serialize_to_map/1 instead of
            %% aec_blocks:serialize_for_network/1 
            Resp = aec_blocks:serialize_to_map(Block),
            {200, [], Resp};
        {error, {chain_too_short, _}} ->
            {404, [], #{reason => <<"chain too short">>}}
    end;

handle_request('GetBlockByHash', Req, _Context) ->
    Hash = base64:decode(maps:get('hash', Req)),
    case aec_chain:get_header_by_hash(Hash) of
        {error, {header_not_found, _}} ->
            {404, [], #{reason => <<"block not found">>}};
        {ok, Header} ->
            {ok, HH} = aec_headers:hash_header(Header),
            case aec_chain:get_block_by_hash(HH) of
                {ok, Block} ->
                    %% swagger generated code expects the Resp to be proplist
                    %% or map and always runs jsx:encode/1 on it - even if it
                    %% is already encoded to a binary; that's why we use
                    %% aec_blocks:serialize_to_map/1 instead of
                    %% aec_blocks:serialize_for_network/1 
                    Resp = aec_blocks:serialize_to_map(Block),
                    {200, [], Resp};
                {error, {block_not_found, _}} ->
                    {404, [], #{reason => <<"block not found">>}}
            end
    end;

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


mk_num(undefined) ->
    undefined;
mk_num(I) when is_integer(I) ->
    I;
mk_num(B) when is_binary(B) ->
    try binary_to_integer(B)
    catch
	error:_ ->
	    undefined
    end.

