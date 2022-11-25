%%% @copyright (C) 2022, Aeternity
-module(aehttp_parent_connector_tests).

%% Test interactions with parent chains using chain simulators for AE and BTC nodes

-include_lib("eunit/include/eunit.hrl").

-define(BIG_AMOUNT, 10000000000000000000000000000 * aec_test_utils:min_gas_price()).
-define(PARENT_CHAIN_NETWORK_ID, <<"pc_network_id">>).
-define(SIGN_MODULE, aec_preset_keys).

ae_sim_test_() ->
    {foreach,
     fun() ->
            Apps = [gproc, lager, crypto, enacl, cowboy, inets],
            lists:foreach(fun(App) ->
                            {ok, _} = application:ensure_all_started(App)
                          end, Apps),
             aec_test_utils:mock_genesis_and_forks(),
             ParentHosts = ae_parent_http_specs(),

            %% Create a single account to receive commitments to exist on all
            %% simulated parent chains (there is no sync between chain simulators)
            #{pubkey := CommitmentPubKey, privkey := _} = KeyPair = new_keypair(),
            %% Also create an account on the parent chains that represents a Staker
            %% We will need its private key to sign staking spendTxs on the parent
            #{pubkey := _StakerPubKey, privkey := _} = StakerKeyPair = new_keypair(),
            %% Start a collection of locally simulated parent nodes.
            ParentSims = start_ae_parent_sims(ParentHosts, StakerKeyPair, KeyPair),

             %% Start the process that that regularly checkes the parent chain.
             %% FIXME: this would normally be running in the main ae node
             %% as part of the normal supervisor tree in a HC enabled system
             {ok, _Connector} =
                aec_parent_connector:start_link(aehttpc_aeternity,
                                                on_demand,
                                                ParentHosts,
                                                ?PARENT_CHAIN_NETWORK_ID,
                                                ?SIGN_MODULE,
                                                CommitmentPubKey),
             mock_parent_cache(),
             mock_network_id(?PARENT_CHAIN_NETWORK_ID),
             mock_sign_module(StakerKeyPair),
             {ok, CommitmentPubKey, StakerKeyPair, ParentSims}
     end,
     fun({ok, _CommitmentPubKey, _StakerKeyPair, ParentSims}) ->
             unmock_sign_module(),
             unmock_network_id(),
             unmock_parent_cache(),
             aec_parent_connector:stop(),
             stop_ae_parent_sims(ParentSims),
             aec_test_utils:unmock_genesis_and_forks(),
             ok = application:stop(inets),
             ok = application:stop(enacl),
             ok = application:stop(crypto),
             ok = application:stop(lager),
             ok = application:stop(gproc),
             ok = application:stop(cowboy),
             ok
     end,
     [fun({ok, CommitmentPubKey, StakerKeyPair, _ParentSims}) ->
        #{pubkey := StakerPubKey, privkey := StakerPrivKey} = StakerKeyPair,
        [{"Basic http api operational to each child simulator",
            fun() ->
                    Responses =
                        get_results(
                            fun(Host, Port, User, Password, Seed) ->
                                aehttpc_aeternity:get_latest_block(Host,
                                                                   Port,
                                                                   User,
                                                                   Password,
                                                                   Seed)
                            end),
                    PoolSize = length(ae_parent_http_specs()),
                    [{{ok, Top, PrevHash, Height}, PoolSize}] =
                        maps:to_list(Responses),
                    Self = self(),
                    meck:expect(aec_parent_chain_cache, post_block,
                                fun(Block) -> Self ! {post_block, Block} end),
                    aec_parent_connector:trigger_fetch(),
                    {ok, Block} =
                        receive
                            {post_block, B} -> {ok, B}
                        end,
                    ?assertEqual(Top, aec_parent_chain_block:hash(Block)),
                    ?assertEqual(PrevHash, aec_parent_chain_block:prev_hash(Block)),
                    ?assertEqual(Height, aec_parent_chain_block:height(Block)),
                    ok
            end},
         {"Parent nodes have different tops",
            fun() ->
                    [Spec1, Spec2, Spec3] = ae_parent_http_specs(),
                    PostPCBlock =
                        fun(#{port := Port}) ->
                            SimName = sim_name(Port),
                            aec_chain_sim:add_keyblock(SimName)
                        end,
                    %% post a different amount of blocks on different sims
                    {ok, KB1} = PostPCBlock(Spec1),

                    {ok, KB1} = PostPCBlock(Spec2),
                    {ok, KB2} = PostPCBlock(Spec2),

                    {ok, KB1} = PostPCBlock(Spec3),
                    {ok, KB2} = PostPCBlock(Spec3),
                    {ok, KB3} = PostPCBlock(Spec3),
                    Block1 = keyblock_to_pc_block(KB1),
                    Block2 = keyblock_to_pc_block(KB2),
                    Block3 = keyblock_to_pc_block(KB3),
                    Responses =
                        get_results(
                            fun(Host, Port, User, Password, Seed) ->
                                aehttpc_aeternity:get_latest_block(Host,
                                                                   Port,
                                                                   User,
                                                                   Password,
                                                                   Seed)
                            end),
                    PoolSize = length(ae_parent_http_specs()),
                    PoolSize = length(maps:to_list(Responses)), %% all different answers
                    Self = self(),
                    meck:expect(aec_parent_chain_cache, post_block,
                                fun(Block) -> Self ! {post_block, Block} end),
                    aec_parent_connector:trigger_fetch(),
                    %% no consensus
                    ok =
                        receive
                            {post_block, _B} -> error(should_not_happen)
                        after 100 -> ok
                        end,
                    {ok, Block1} =
                        aec_parent_connector:fetch_block_by_height(1),
                    {ok, Block1} =
                        aec_parent_connector:fetch_block_by_hash(
                            aec_parent_chain_block:hash(Block1)),
                    {ok, Block2} =
                        aec_parent_connector:fetch_block_by_height(2),
                    {ok, Block2} =
                        aec_parent_connector:fetch_block_by_hash(
                            aec_parent_chain_block:hash(Block2)),
                    {error, no_parent_chain_agreement} =
                        aec_parent_connector:fetch_block_by_height(3),
                    {error, no_parent_chain_agreement} =
                        aec_parent_connector:fetch_block_by_hash(
                            aec_parent_chain_block:hash(Block3)),
                    %% get node2 to the same top as node3
                    {ok, KB3} = PostPCBlock(Spec2),
                    {ok, Block3} =
                        aec_parent_connector:fetch_block_by_height(3),
                    %% trigger fetch again - 2 out 3 nodes should agree on block3
                    aec_parent_connector:trigger_fetch(),
                    %% no consensus
                    {ok, Block3} =
                        receive
                            {post_block, B3} -> {ok, B3}
                        end,
                    ok
            end},
          {"Post a suitable spend TX to each parent chain and check it is in the commitment list",
            fun() ->
                    %% aec_parent_connector:trigger_fetch(),
                    lists:foreach(
                        fun(#{host := Host, port := Port,
                            user := User, password := Password}) ->
                            SimName = sim_name(Port),
                            aec_chain_sim:add_keyblock(SimName),
                            aec_chain_sim:add_keyblock(SimName),
                            %% Post our local top hash as the commitment
                            Commitment = <<"kh_deadbeef">>,
                            {ok, TopHash, _PrevHash, Height} = aehttpc_aeternity:get_latest_block(Host, Port, User, Password, <<"Seed">>),
                            Fee = 20000 * aec_test_utils:min_gas_price(),
                            {ok, #{<<"tx_hash">> := _}} =
                                aehttpc_aeternity:post_commitment(Host, Port,
                                                                   StakerPubKey,
                                                                   CommitmentPubKey,
                                                                   1, Fee,
                                                                   Commitment,
                                                                  ?PARENT_CHAIN_NETWORK_ID,
                                                                  ?SIGN_MODULE),
                            ?assertMatch({ok, #{micro_blocks := []}}, aec_chain_sim:get_current_generation(SimName)),
                            %% Call the simulator directly to force our Tx in a block
                            aec_chain_sim:add_microblock(SimName),
                            ?assertMatch({ok, #{micro_blocks := []}}, aec_chain_sim:get_current_generation(SimName)),
                            %% And create a keyblock
                            aec_chain_sim:add_keyblock(SimName),
                            ?assertMatch({ok, #{micro_blocks := []}}, aec_chain_sim:get_current_generation(SimName)),
                            {ok, [{Acct, Payload}]} = aehttpc_aeternity:get_commitment_tx_in_block(Host, Port, User, Password, <<"Seed">>, TopHash, CommitmentPubKey),
                            ?assertMatch(Acct, aeser_api_encoder:encode(account_pubkey, StakerPubKey)),
                            {_Type, Val} =  aeser_api_encoder:decode(Payload),
                            ?assertEqual(<<"kh_deadbeef">>, Val),
                            %% Test we can also get the same commitments by height
                            TopHeight = aec_chain_sim:get_height(SimName),
                            %% Top here is the keyblock we added after the microblock with our Txs, so
                            %% we need to look in the height one below top
                            ?assertEqual(TopHeight, Height + 2),
                            {ok, [{Acct, Payload}]} = aehttpc_aeternity:get_commitment_tx_at_height(Host, Port, User, Password, <<"Seed">>, TopHeight - 2, CommitmentPubKey)
                        end, ae_parent_http_specs()),
                    ok
            end}]
    end]
    }.

ae_parent_http_specs() ->
    lists:map(fun(Index) ->
                #{host => <<"127.0.0.1">>,
                  port => 3013 + Index,
                  user => "test",
                  password => "Pass"
                 } end, lists:seq(0, 2)).

start_ae_parent_sims(ParentHosts, StakerKeyPair, KeyPair) ->
    lists:map(fun(#{port := Port}) ->
                Name = sim_name(Port),
                InitialState = #{accounts => []},
                {ok, Pid} = aehttp_ae_sim:start_link(Name, Port, InitialState),
                {ok, _} = aec_chain_sim:add_existing_account(Name, KeyPair, ?BIG_AMOUNT),
                {ok, _} = aec_chain_sim:add_existing_account(Name, StakerKeyPair, ?BIG_AMOUNT),
                Pid
              end, ParentHosts).

stop_ae_parent_sims(ParentSims) ->
    lists:foreach(fun(SimPid) -> aehttp_ae_sim:stop(SimPid) end, ParentSims).

sim_name(Port) ->
     list_to_atom("ae_sim_" ++ integer_to_list(Port)).

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.

get_results(Fun) ->
    Responses =
        lists:map(
            fun(#{host := Host, port := Port, user := User, password := Password}) ->
                {ok, _TopHash, _PrevHash, _Height} = Fun(Host, Port, User, Password, <<"Seed">>)
            end,
            ae_parent_http_specs()),
    count_duplicates(Responses).

count_duplicates(L) when is_list(L) -> count_duplicates(L, #{}).

count_duplicates([], Accum) -> Accum;
count_duplicates([H | T], Accum) ->
    count_duplicates(T, maps:update_with(H, fun(X) -> X + 1 end, 1, Accum)).


mock_parent_cache() ->
    meck:new(aec_parent_chain_cache, [passthrough]).

unmock_parent_cache() ->
    meck:unload(aec_parent_chain_cache).

mock_sign_module(#{pubkey := ExpectedPubKey, privkey := PrivKey}) ->
    meck:new(?SIGN_MODULE, []),
    meck:expect(?SIGN_MODULE, sign_binary,
        fun(Bin, Pubkey) when Pubkey =:= ExpectedPubKey ->
            Signature = enacl:sign_detached(Bin, PrivKey), 
            {ok, Signature}
        end).

unmock_sign_module() ->
    meck:unload(?SIGN_MODULE).

mock_network_id(NetworkId) ->
    meck:new(aec_governance, [passthrough]),
    meck:expect(aec_governance, get_network_id,
                fun() -> NetworkId end).

unmock_network_id() ->
    meck:unload(aec_governance).

keyblock_to_pc_block(KB) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(KB),
    Hash = aeser_api_encoder:encode(key_block_hash, Hash0),
    PrevHash0 = aec_blocks:prev_hash(KB),
    PrevHash = aeser_api_encoder:encode(key_block_hash, PrevHash0),
    Height = aec_blocks:height(KB),
    aec_parent_chain_block:new(Hash, Height, PrevHash).

