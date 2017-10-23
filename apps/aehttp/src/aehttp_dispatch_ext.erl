-module(aehttp_dispatch_ext).

-export([handle_request/3]).

-compile({parse_transform, lager_transform}).
-include_lib("aecore/include/common.hrl").
-include_lib("aecore/include/trees.hrl").

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('Ping', #{'Ping' := PingObj}, _Context) ->
    LocalPingObj = aec_sync:local_ping_object(),
    case aec_sync:compare_ping_objects(LocalPingObj, PingObj) of
        {error, different_genesis_blocks} ->
            {404, [], #{reason => <<"Different genesis blocks">>}};
        ok ->
            Source = maps:get(<<"source">>, PingObj),
            aec_peers:update_last_seen(Source),
            TheirPeers = maps:get(<<"peers">>, PingObj, []),
            aec_peers:add_and_ping_peers(TheirPeers),
            Ok = LocalPingObj#{<<"pong">> => <<"pong">>},
            Share = maps:get(<<"share">>, PingObj),
            Res = case mk_num(Share) of
                      N when is_integer(N), N > 0 ->
                          Peers = aec_peers:get_random(N, [Source|TheirPeers]),
                          PeerUris = [iolist_to_binary(aec_peers:uri(P))
                                      || P <- Peers],
                          lager:debug("PeerUris = ~p~n", [PeerUris]),
                          Ok#{<<"peers">> => PeerUris};
                      _ ->
                          Ok
                  end,
            {200, [], Res}
    end;

handle_request('GetTop', _, _Context) ->
    {ok, Header} = aec_chain:top_header(),
    {ok, HH} = aec_headers:hash_header(Header),
    {ok, Top} = aec_headers:serialize_to_map(Header),
    Resp = cleanup_genesis(Top),
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
            Resp = cleanup_genesis(aec_blocks:serialize_to_map(Block)),
            {200, [], Resp};
        {error, {chain_too_short, _}} ->
            {404, [], #{reason => <<"chain too short">>}}
    end;

handle_request('GetBlockByHash' = _Method, Req, _Context) ->
    lager:debug("got ~p; Req = ~p", [_Method, Req]),
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
                    Resp =
                      cleanup_genesis(aec_blocks:serialize_to_map(Block)),
                    {200, [], Resp};
                {error, {block_not_found, _}} ->
                    {404, [], #{reason => <<"block not found">>}}
            end
    end;

handle_request('PostBlock', Req, _Context) ->
    SerializedBlock = add_missing_to_genesis_block(maps:get('Block', Req)),
    {ok, Block} = aec_blocks:deserialize_from_map(SerializedBlock),
    Header = aec_blocks:to_header(Block),
    {ok, HH} = aec_headers:hash_header(Header),
    lager:debug("'PostBlock'; header hash: ~p", [HH]),
    case aec_chain:get_block_by_hash(HH) of
        {ok, _Existing} ->
            lager:debug("Aleady have block", []),
            %% Do not tell sync to re-broadcast block we already know about
            {200, [], #{}};
        {error, _} ->
            case {aec_headers:validate(Header), aec_blocks:validate(Block)} of
                {ok, ok} ->
                    case aec_chain:insert_header(Header) of
                        ok ->
                            Res = aec_chain:write_block(Block),
                            aec_sync:received_block(Block),
                            lager:debug("write_block result: ~p", [Res]);
                        {error, Reason} ->
                            lager:debug("Couldn't insert header (~p)", [Reason])
                    end,
                    %% TODO update swagger.yaml to allow error returns?
                    {200, [], #{}};
                {{error, Reason}, _} ->
                    lager:info("Malformed block posted to the node (~p)", [Reason]),
                    {400, [], #{reason => <<"validation failed">>}};
                {ok, {error, Reason}} ->
                    lager:info("Malformed block posted to the node (~p)", [Reason]),
                    {400, [], #{reason => <<"validation failed">>}}
            end
    end;

handle_request('GetAccountBalance', Req, _Context) ->
    Pubkey =
      case maps:get('pub_key', Req) of
          undefined ->
              {ok, PK} = aec_keys:pubkey(),
              PK;
          PK ->
              PK
      end,
    {ok, LastBlock} = aec_chain:top(),
    Trees = aec_blocks:trees(LastBlock),
    AccountsTree = aec_trees:accounts(Trees),
    case aec_accounts:get(Pubkey, AccountsTree) of
        {ok, #account{balance = B}} ->
            {200, [], #{balance => B}};
        _ ->
            {404, [], #{reason => <<"account not found">>}}
    end;

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

empty_fields_in_genesis() ->
    [ <<"prev_hash">>,
      <<"state_hash">>,
      <<"pow">>,
      <<"txs_hash">>,
      <<"transactions">>].

%% to be used for both headers and blocks
cleanup_genesis(#{<<"height">> := 0} = Genesis) ->
    maps:without(empty_fields_in_genesis(), Genesis);
cleanup_genesis(Val) ->
    Val.

add_missing_to_genesis_block(#{<<"height">> := 0} = Block) ->
    GB = aec_blocks:serialize_to_map(
           aec_block_genesis:genesis_block_as_deserialized_from_network()),
    EmptyFields = maps:with(empty_fields_in_genesis(), GB),
    maps:merge(Block, EmptyFields);
add_missing_to_genesis_block(Val) ->
    Val.
