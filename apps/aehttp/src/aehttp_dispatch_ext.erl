-module(aehttp_dispatch_ext).

-export([handle_request/3]).
-export([cleanup_genesis/1]).

-import(aeu_debug, [pp/1]).

-compile({parse_transform, lager_transform}).
-include_lib("aecore/include/common.hrl").
-include_lib("aecore/include/trees.hrl").

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('Ping', #{'Ping' := PingObj}, _Context) ->
    handle_ping(PingObj);

handle_request('GetTop', _, _Context) ->
    Header = aec_conductor:top_header(),
    {ok, HH} = aec_headers:hash_header(Header),
    {ok, Top} = aec_headers:serialize_to_map(Header),
    Resp = cleanup_genesis(Top),
    {200, [], maps:put(hash, base64:encode(HH), Resp)};

handle_request('GetBlockByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    case aec_conductor:get_block_by_height(Height) of
        {ok, Block} ->
            %% swagger generated code expects the Resp to be proplist or map
            %% and always runs jsx:encode/1 on it - even if it is already
            %% encoded to a binary; that's why we use
            %% aec_blocks:serialize_to_map/1 instead of
            %% aec_blocks:serialize_for_network/1
            Resp = cleanup_genesis(aec_blocks:serialize_to_map(Block)),
            lager:debug("Resp = ~p", [pp(Resp)]),
            {200, [], Resp};
        {error, no_top_header} ->
            {404, [], #{reason => <<"No top header">>}};
        {error, block_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, chain_too_short} ->
            {404, [], #{reason => <<"Chain too short">>}}
    end;

handle_request('GetBlockByHash' = _Method, Req, _Context) ->
    lager:debug("got ~p; Req = ~p", [_Method, pp(Req)]),
    Hash = base64:decode(maps:get('hash', Req)),
    case aec_conductor:get_header_by_hash(Hash) of
        {error, header_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {ok, Header} ->
            {ok, HH} = aec_headers:hash_header(Header),
            case aec_conductor:get_block_by_hash(HH) of
                {ok, Block} ->
                    %% swagger generated code expects the Resp to be proplist
                    %% or map and always runs jsx:encode/1 on it - even if it
                    %% is already encoded to a binary; that's why we use
                    %% aec_blocks:serialize_to_map/1 instead of
                    %% aec_blocks:serialize_for_network/1
                    lager:debug("Block = ~p", [pp(Block)]),
                    Resp =
                      cleanup_genesis(aec_blocks:serialize_to_map(Block)),
                    lager:debug("Resp = ~p", [pp(Resp)]),
                    {200, [], Resp};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end;

handle_request('GetTxs', _Req, _Context) ->
    {ok, Txs0} = aec_tx_pool:peek(infinity),
    lager:debug("GetTxs : ~p", [pp(Txs0)]),
    Txs = [#{<<"tx">> => base64:encode(aec_tx_sign:serialize_to_binary(T))}
           || T <- Txs0],
    {200, [], Txs};

handle_request('PostBlock', Req, _Context) ->
    SerializedBlock = add_missing_to_genesis_block(maps:get('Block', Req)),
    {ok, Block} = aec_blocks:deserialize_from_map(SerializedBlock),

    %% Only for logging
    Header = aec_blocks:to_header(Block),
    {ok, HH} = aec_headers:hash_header(Header),
    lager:debug("'PostBlock'; header hash: ~p", [HH]),
    case aec_conductor:post_block(Block) of
        ok -> {200, [], #{}};
        {error, Reason} ->
            lager:error("Post block failed: ~p", [Reason]),
            {200, [], #{}}
    end;

handle_request('PostTx', #{'Tx' := Tx} = Req, _Context) ->
    lager:debug("Got PostTx; Req = ~p", [pp(Req)]),
    SerializedTx = maps:get(<<"tx">>, Tx),
    SignedTx = aec_tx_sign:deserialize_from_binary(
                 base64:decode(SerializedTx)),
    lager:debug("deserialized: ~p", [pp(SignedTx)]),
    PushRes = aec_tx_pool:push(SignedTx, tx_received),
    lager:debug("PushRes = ~p", [pp(PushRes)]),
    {200, [], #{}};

handle_request('GetAccountBalance', Req, _Context) ->
    Pubkey =
      case maps:get('pub_key', Req) of
          undefined ->
              {ok, PK} = aec_keys:pubkey(),
              PK;
          PK when is_binary(PK) ->
              try base64:decode(PK)
              catch _:_ -> not_base64_encoded
              end
      end,
    case Pubkey of
        not_base64_encoded ->
            {400, [], #{reason => <<"Invalid address">>}};
        _ when is_binary(Pubkey) ->
            case aec_conductor:get_account(Pubkey) of
                {ok, A} ->
                    {200, [], #{balance => aec_accounts:balance(A)}};
                {error, notfound} ->
                    {404, [], #{reason => <<"Account not found">>}}
            end
    end;

handle_request('GetAccountsBalances', _Req, _Context) ->
    case application:get_env(aehttp, enable_debug_endpoints, false) of
        true ->
            AccountsBalances = aec_conductor:get_all_accounts_balances(),
            FormattedAccountsBalances =
                lists:foldl(
                  fun({Pubkey, Balance}, Acc) ->
                          [#{pub_key => base64:encode(Pubkey),
                             balance => Balance} | Acc]
                  end, [], AccountsBalances),
            {200, [], #{accounts_balances => FormattedAccountsBalances}};
        false ->
            {404, [], #{}}
    end;

handle_request('GetInfo', _Req, _Context) ->
    TimeSummary0 = aec_conductor:get_top_30_blocks_time_summary(),
    TimeSummary =
        lists:foldl(
          fun({Height, Ts, Delta}, Acc) ->
                  [#{height => Height,
                     time => Ts,
                     time_delta_to_parent => Delta} | Acc];
             ({Height, Ts}, Acc) ->
                  [#{height => Height,
                     time => Ts} | Acc]
          end, [], TimeSummary0),
    {200, [], #{version => aeu_info:get_version(),
                revision => aeu_info:get_revision(),
                genesis_hash => base64:encode(aec_conductor:genesis_hash()),
                last_30_blocks_time => lists:reverse(TimeSummary)}};

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
      <<"pow">>,
      <<"txs_hash">>,
      <<"transactions">>].

%% to be used for both headers and blocks
cleanup_genesis(#{<<"height">> := 0} = Genesis) ->
    maps:without(empty_fields_in_genesis(), Genesis);
cleanup_genesis(Val) ->
    Val.

add_missing_to_genesis_block(#{<<"height">> := 0} = Block) ->
    {ok, GenesisBlock} = aec_conductor:genesis_block(),
    GB = aec_blocks:serialize_to_map(GenesisBlock),
    EmptyFields = maps:with(empty_fields_in_genesis(), GB),
    maps:merge(Block, EmptyFields);
add_missing_to_genesis_block(Val) ->
    Val.

handle_ping(#{<<"source">> := Src} = PingObj) ->
    IsBlocked = aec_peers:is_blocked(Src),
    case IsBlocked of
        false -> handle_ping_(PingObj);
        true  ->
            abort_sync(Src, <<"Not allowed">>)
    end;
handle_ping(_) ->
    Reason = <<"Missing source attribute">>,
    abort_sync(undefined, Reason).

handle_ping_(PingObj) ->
    LocalPingObj = aec_sync:local_ping_object(),
    case aec_sync:compare_ping_objects(LocalPingObj, PingObj) of
        {error, different_genesis_blocks} ->
            Source = maps:get(<<"source">>, PingObj),
            aec_peers:block_peer(Source),
            abort_sync(Source,  <<"Different genesis blocks">>);
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
    end.

abort_sync(Uri, Reason) ->
    aec_events:publish(
      chain_sync,
      {sync_aborted, #{uri => Uri,
                       reason => Reason}}),
      {404, [], #{reason => Reason}}.
