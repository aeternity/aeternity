-module(aehttp_dispatch_ext).

-export([handle_request/3]).
-export([cleanup_genesis/1]).

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
            {404, [], #{reason => <<"Chain too short">>}}
    end;

handle_request('GetBlockByHash' = _Method, Req, _Context) ->
    lager:debug("got ~p; Req = ~p", [_Method, Req]),
    Hash = base64:decode(maps:get('hash', Req)),
    case aec_chain:get_header_by_hash(Hash) of
        {error, {header_not_found, _}} ->
            {404, [], #{reason => <<"Block not found">>}};
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
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end;

handle_request('GetTxs', Req, _Context) ->
    N = maps:get('N', Req, 30),
    error(nyi);

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
                    {404, [], #{reason => <<"validation failed">>}};
                {ok, {error, Reason}} ->
                    lager:info("Malformed block posted to the node (~p)", [Reason]),
                    {404, [], #{reason => <<"validation failed">>}}
            end
    end;

handle_request('PostTx', Req, _Context) ->
    SerializedTx = maps:get('SignedTx', Req),
    {ok, SignedTx} = aec_tx_sign:deserialize_from_binary(SerializedTx),
    case aec_tx_pool:push(SignedTx) of
        ok ->
            lager:debug("Successfully added tx to pool", []),
            {200, [], #{}};
        {error, _} = Error ->
            %% not necessarily an error condition for the remote side
            lager:debug("Error pushing tx to school: ~p", [Error]),
            {200, [], #{}}
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
            {404, [], #{reason => <<"Account not found">>}}
    end;

handle_request('PostSpendTx', #{'SpendTx' := SpendTxObj}, _Context) ->
    case aec_keys:pubkey() of
        {ok, SenderPubkey} ->
            RecipientPubkey = maps:get(<<"recipient_pubkey">>, SpendTxObj),
            Amount = maps:get(<<"amount">>, SpendTxObj),
            Fee = maps:get(<<"fee">>, SpendTxObj),

            %% TODO: Nonce shall be determined not based on accounts state tree,
            %% so remove below, when next_nonce service is ready
            {ok, LastBlock} = aec_chain:top(),
            Trees = aec_blocks:trees(LastBlock),
            AccountsTrees = aec_trees:accounts(Trees),
            {ok, Account} = aec_accounts:get(SenderPubkey, AccountsTrees),
            Nonce = aec_accounts:nonce(Account) + 1,

            {ok, _SpendTx} = aec_spend_tx:new(
                               #{sender => SenderPubkey,
                                 recipient => RecipientPubkey,
                                 amount => Amount,
                                 fee => Fee,
                                 nonce => Nonce}, Trees),
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
