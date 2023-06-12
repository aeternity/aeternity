%%% @copyright (C) 2022, Aeternity
-module(aehttp_parent_connector_tests).

%% Test interactions with parent chains using chain simulators for AE and BTC nodes

-include_lib("eunit/include/eunit.hrl").

-define(BIG_AMOUNT, 10000000000000000000000000000 * aec_test_utils:min_gas_price()).
-define(PARENT_CHAIN_NETWORK_ID, <<"pc_network_id">>).
-define(CHILD_CHAIN_NETWORK_ID, <<"hc_network_id">>).
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
                                                [],
                                                CommitmentPubKey,
                                                8100,
                                                110000000000000),
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
        #{pubkey := StakerPubKey, privkey := _StakerPrivKey} = StakerKeyPair,
        [{"Basic http api operational to each child simulator",
            fun() ->
                    [Spec1, Spec2, Spec3] = ae_parent_http_specs(),
                    PostPCBlock =
                        fun() ->
                            lists:foreach(
                                fun(#{port := Port}) ->
                                    SimName = ae_sim_name(Port),
                                    aec_chain_sim:add_keyblock(SimName)
                                end,
                                [Spec1, Spec2, Spec3])
                        end,
                    PostPCBlock(),
                    Responses =
                        get_results(
                            fun(Host, Port, User, Password, Seed) ->
                                aehttpc_aeternity:get_latest_block(Host,
                                                                   Port,
                                                                   User,
                                                                   Password,
                                                                   Seed)
                            end, ae_parent_http_specs()),
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
                            SimName = ae_sim_name(Port),
                            aec_chain_sim:add_keyblock(SimName)
                        end,
                    %% post a different amount of blocks on different sims
                    {ok, KB2} = PostPCBlock(Spec1),

                    {ok, KB2} = PostPCBlock(Spec2),
                    {ok, KB3} = PostPCBlock(Spec2),

                    {ok, KB2} = PostPCBlock(Spec3),
                    {ok, KB3} = PostPCBlock(Spec3),
                    {ok, KB4} = PostPCBlock(Spec3),
                    Block2 =
                        aec_parent_chain_block:set_commitments(keyblock_to_pc_block(KB2), []),
                    Block3 =
                        aec_parent_chain_block:set_commitments(keyblock_to_pc_block(KB3), []),
                    Block4 =
                        aec_parent_chain_block:set_commitments(keyblock_to_pc_block(KB4), []),
                    Responses =
                        get_results(
                            fun(Host, Port, User, Password, Seed) ->
                                aehttpc_aeternity:get_latest_block(Host,
                                                                   Port,
                                                                   User,
                                                                   Password,
                                                                   Seed)
                            end, ae_parent_http_specs()),
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
                    {ok, Block2} =
                        aec_parent_connector:fetch_block_by_height(2),
                    {ok, Block2} =
                        aec_parent_connector:fetch_block_by_hash(
                            aec_parent_chain_block:hash(Block2)),
                    {ok, Block3} =
                        aec_parent_connector:fetch_block_by_height(3),
                    {ok, Block3} =
                        aec_parent_connector:fetch_block_by_hash(
                            aec_parent_chain_block:hash(Block3)),
                    {error, no_parent_chain_agreement} =
                        aec_parent_connector:fetch_block_by_height(4),
                    {error, no_parent_chain_agreement} =
                        aec_parent_connector:fetch_block_by_hash(
                            aec_parent_chain_block:hash(Block4)),
                    %% get node2 to the same top as node3
                    {ok, KB4} = PostPCBlock(Spec2),
                    {ok, Block4} =
                        aec_parent_connector:fetch_block_by_height(4),
                    %% trigger fetch again - 2 out 3 nodes should agree on block3
                    aec_parent_connector:trigger_fetch(),
                    %% no consensus
                    {ok, Block4} =
                        receive
                            {post_block, B4} -> {ok, B4}
                        end,
                    ok
            end},
          {"Post a suitable spend TX to each parent chain and check it is in the commitment list",
            fun() ->
                    %% aec_parent_connector:trigger_fetch(),
                    lists:foreach(
                        fun(#{host := Host, port := Port,
                            user := User, password := Password}) ->
                            SimName = ae_sim_name(Port),
                            aec_chain_sim:add_keyblock(SimName),
                            aec_chain_sim:add_keyblock(SimName),
                            %% Post our local top hash as the commitment
                            Val = <<42:32/unit:8>>,
                            StakerEnc = aeapi:format(account_pubkey, StakerPubKey),
                            Commitment =  aec_parent_chain_block:encode_commitment_btc(StakerPubKey, Val, ?CHILD_CHAIN_NETWORK_ID),
                            Fee = 20000 * aec_test_utils:min_gas_price(),
                            {ok, #{<<"tx_hash">> := _}} =
                                aehttpc_aeternity:post_commitment(Host, Port, <<>>, <<>>,
                                                                   StakerEnc,
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
                            {ok, TopHash, PrevHash, Height} = aehttpc_aeternity:get_latest_block(Host, Port, User, Password, <<"Seed">>),
                            ?assertMatch({ok, #{micro_blocks := []}}, aec_chain_sim:get_current_generation(SimName)),
                            {ok, [{Signature, AcctHashPrefix, PayloadHashPrefix}]} =
                                aehttpc_aeternity:get_commitment_tx_in_block(Host, Port, User, Password, <<"Seed">>, TopHash, PrevHash, CommitmentPubKey),
                            ?assertMatch(AcctHashPrefix, binary:part(aec_hash:sha256_hash(aeb_fate_encoding:serialize(aeb_fate_data:make_address(StakerPubKey))), 0, 8)),
                            ?assertMatch(PayloadHashPrefix, binary:part(aec_hash:sha256_hash(Val), 0, 7)),
                            %% Test we can also get the same commitments by height
                            TopHeight = aec_chain_sim:get_height(SimName),
                            %% Top here is the keyblock we added after the microblock with our Txs, so
                            %% we need to look in the height one below top
                            ?assertEqual(TopHeight, Height + 1),
                            {ok, [{Signature, AcctHashPrefix, PayloadHashPrefix}]} = aehttpc_aeternity:get_commitment_tx_at_height(Host, Port, User, Password, <<"Seed">>, TopHeight - 1, CommitmentPubKey)
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
                Name = ae_sim_name(Port),
                InitialState = #{accounts => []},
                {ok, Pid} = aehttp_ae_sim:start_link(Name, Port, InitialState),
                {ok, _} = aec_chain_sim:add_existing_account(Name, KeyPair, ?BIG_AMOUNT),
                {ok, _} = aec_chain_sim:add_existing_account(Name, StakerKeyPair, ?BIG_AMOUNT),
                Pid
              end, ParentHosts).

stop_ae_parent_sims(ParentSims) ->
    lists:foreach(fun(SimPid) -> aehttp_ae_sim:stop(SimPid) end, ParentSims).

ae_sim_name(Port) ->
     list_to_atom("ae_sim_" ++ integer_to_list(Port)).

btc_sim_test_() ->
    {foreach,
        fun() ->
            Apps = [gproc, lager, crypto, enacl, cowboy, inets],
            lists:foreach(fun(App) ->
                            {ok, _} = application:ensure_all_started(App)
                            end, Apps),
            #{pubkey := _StakerPubKey, privkey := _} = StakerKeyPair = new_keypair(),
            ParentHosts = btc_parent_http_specs(),

            BTCAccounts = aehttp_btc_sim:btc_accounts(),
            %% Create a single account to receive commitments to exist on all
            %% simulated parent chains (there is no sync between chain simulators)
            %% Also create an account on the parent chains that represents a Staker
            %% We should will need its private key to sign staking spendTxs on the parent
            [{CommitmentPubKey, _}, {BTCStakerPubKey, _} | _] = BTCAccounts,
            %% Start a collection of locally simulated parent nodes.
            ParentSims = start_btc_parent_sims(ParentHosts, BTCAccounts),

            %% Start the process that that regularly checkes the parent chain.
            %% FIXME: this would normally be running in the main ae node
            %% as part of the normal supervisor tree in a HC enabled system
            {ok, _Connector} =
            aec_parent_connector:start_link(aehttpc_btc,
                                            on_demand,
                                            ParentHosts,
                                            ?PARENT_CHAIN_NETWORK_ID,
                                            ?SIGN_MODULE,
                                            [],
                                            CommitmentPubKey,
                                            7500,
                                            8500),
            mock_parent_cache(),
            mock_sign_module(StakerKeyPair),
            {ok, CommitmentPubKey, BTCStakerPubKey, StakerKeyPair, ParentSims}
        end,
        fun({ok, _CommitmentPubKey, _StakerPubKey, _StakerKeyPair, ParentSims}) ->
            unmock_parent_cache(),
            unmock_sign_module(),
            aec_parent_connector:stop(),
            stop_btc_parent_sims(ParentSims),
            ok = application:stop(inets),
            ok = application:stop(enacl),
            ok = application:stop(crypto),
            ok = application:stop(lager),
            ok = application:stop(gproc),
            ok = application:stop(cowboy),
            ok
        end,
        [fun({ok, CommitmentPubKey, BTCStakerPubKey, StakerKeyPair, _ParentSims}) ->
            #{pubkey := StakerPubKey, privkey := _StakerPrivKey} = StakerKeyPair,
        [{"Basic http api operational to each BTC child simulator",
            fun() ->
                    [Spec1, Spec2, Spec3] = btc_parent_http_specs(),
                    PostPCBlock =
                        fun() ->
                            lists:foreach(
                                fun(#{port := Port}) ->
                                    SimName = btc_sim_name(Port),
                                    aehttp_btc_sim:mine_on_fork(SimName, main)
                                end,
                                [Spec1, Spec2, Spec3])
                        end,
                    PostPCBlock(),
                    Responses =
                        get_results(
                            fun(Host, Port, User, Password, Seed) ->
                                aehttpc_btc:get_latest_block(Host,
                                                             Port,
                                                             User,
                                                             Password,
                                                             Seed)
                            end, btc_parent_http_specs()),
                    PoolSize = length(btc_parent_http_specs()),
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
        {"Parent BTC nodes have different tops",
            fun() ->
                [Spec1, Spec2, Spec3] = btc_parent_http_specs(),
                PostPCBlock =
                    fun(#{port := Port}) ->
                        SimName = btc_sim_name(Port),
                        aehttp_btc_sim:mine_on_fork(SimName, main)
                    end,
                %% post a different amount of blocks on different sims
                {ok, KB2} = PostPCBlock(Spec1),


                {ok, KB2} = PostPCBlock(Spec2),
                {ok, KB3} = PostPCBlock(Spec2),

                {ok, KB2} = PostPCBlock(Spec3),
                {ok, KB3} = PostPCBlock(Spec3),
                {ok, KB4} = PostPCBlock(Spec3),
                Block2 =
                    aec_parent_chain_block:set_commitments(keyblock_to_pc_block(KB2), []),
                Block3 =
                    aec_parent_chain_block:set_commitments(keyblock_to_pc_block(KB3), []),
                Block4 =
                    aec_parent_chain_block:set_commitments(keyblock_to_pc_block(KB4), []),
                Responses =
                    get_results(
                        fun(Host, Port, User, Password, Seed) ->
                            aehttpc_btc:get_latest_block(Host,
                                                         Port,
                                                         User,
                                                         Password,
                                                         Seed)
                        end, btc_parent_http_specs()),
                PoolSize = length(btc_parent_http_specs()),
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
                {ok, Block2} =
                    aec_parent_connector:fetch_block_by_height(2),
                {ok, Block2} =
                    aec_parent_connector:fetch_block_by_hash(
                        aec_parent_chain_block:hash(Block2)),
                {ok, Block3} =
                    aec_parent_connector:fetch_block_by_height(3),
                {ok, Block3} =
                    aec_parent_connector:fetch_block_by_hash(
                        aec_parent_chain_block:hash(Block3)),
                {error, no_parent_chain_agreement} =
                    aec_parent_connector:fetch_block_by_height(4),
                {error, no_parent_chain_agreement} =
                    aec_parent_connector:fetch_block_by_hash(
                        aec_parent_chain_block:hash(Block4)),
                %% get node2 to the same top as node3
                {ok, KB4} = PostPCBlock(Spec2),
                {ok, Block4} =
                    aec_parent_connector:fetch_block_by_height(4),
                %% trigger fetch again - 2 out 3 nodes should agree on block3
                aec_parent_connector:trigger_fetch(),
                %% no consensus
                {ok, Block4} =
                    receive
                        {post_block, B4} -> {ok, B4}
                    end,
                ok
        end},
        {"Post a suitable spend TX to each parent chain and check it is in the commitment list",
            fun() ->
                %% aec_parent_connector:trigger_fetch(),
                lists:foreach(
                    fun(#{host := Host, port := Port,
                        user := User, password := Password}) ->
                        SimName = btc_sim_name(Port),
                        aehttp_btc_sim:mine_on_fork(SimName, main),
                        aehttp_btc_sim:mine_on_fork(SimName, main),
                        %% Post our local top hash as the commitment
                        Val = <<42:32/unit:8>>,
                        32 = size(StakerPubKey),
                        Commitment = aec_parent_chain_block:encode_commitment_btc(StakerPubKey, Val, ?CHILD_CHAIN_NETWORK_ID),
                        Fee = 8000,
                        {ok, #{<<"tx_hash">> := _}} =
                            aehttpc_btc:post_commitment(Host, Port, User, Password,
                                                                BTCStakerPubKey,
                                                                CommitmentPubKey,
                                                                1, Fee,
                                                                Commitment,
                                                                ?PARENT_CHAIN_NETWORK_ID,
                                                                ?SIGN_MODULE),
                        %% Call the simulator directly to force our Tx in a block
                        aehttp_btc_sim:mine_on_fork(SimName, main),
                        {ok, TopHash, PrevHash, Height} = aehttpc_btc:get_latest_block(Host, Port, User, Password, <<"Seed">>),

                        {ok, [{Signature, AcctHashPrefix, PayloadHashPrefix}]} =
                                aehttpc_btc:get_commitment_tx_in_block(Host, Port, User, Password, <<"Seed">>, TopHash, PrevHash, CommitmentPubKey),
                        ?assertMatch(AcctHashPrefix, binary:part(aec_hash:sha256_hash(aeb_fate_encoding:serialize(aeb_fate_data:make_address(StakerPubKey))), 0, 8)),
                        ?assertMatch(PayloadHashPrefix, binary:part(aec_hash:sha256_hash(Val), 0, 7)),

                        %% Test we can also get the same commitments by height
                        TopHeight = aehttp_btc_sim:get_height(SimName, main),
                        %% Top here for an ae parent is the keyblock we added after the microblock with our Txs, so
                        %% we need to look in the height one below top.
                        %% But for bitcoin transactions are included in the actual block at the height
                        ?assertEqual(TopHeight, Height),
                        {ok, [{Signature, AcctHashPrefix, PayloadHashPrefix}]} = aehttpc_btc:get_commitment_tx_at_height(Host, Port, User, Password, <<"Seed">>, TopHeight, CommitmentPubKey)
                    end, btc_parent_http_specs()),
                ok
            end}]
    end]
    }.


btc_parent_http_specs() ->
    lists:map(fun(Index) ->
        #{host => "127.0.0.1",
          port => 3513 + Index,
          user => <<"test">>,
          password => <<"Pass">>
         } end, lists:seq(0, 2)).

start_btc_parent_sims(ParentHosts, BTCAccounts) ->
    lists:map(fun(#{port := Port}) ->
                      Name = btc_sim_name(Port),
                      InitialState = aehttp_btc_sim:scenario(BTCAccounts),
                      {ok, Pid} = aehttp_btc_sim:start_link(Name, Port, InitialState),
                      Pid
              end, ParentHosts).

stop_btc_parent_sims(ParentSims) ->
    lists:foreach(fun(SimPid) -> aehttp_btc_sim:stop(SimPid) end, ParentSims).

btc_sim_name(Port) ->
    list_to_atom("btc_sim_" ++ integer_to_list(Port)).

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.

get_results(Fun, HTTPSpecs) ->
    Responses =
        lists:map(
            fun(#{host := Host, port := Port, user := User, password := Password}) ->
                {ok, _TopHash, _PrevHash, _Height} = Fun(Host, Port, User, Password, <<"Seed">>)
            end,
            HTTPSpecs),
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

keyblock_to_pc_block({Hash, Height, PrevHash}) -> % BTC parent
    aec_parent_chain_block:new(Hash, Height, PrevHash);
keyblock_to_pc_block(KB) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(KB),
    Hash = aeser_api_encoder:encode(key_block_hash, Hash0),
    PrevHash0 = aec_blocks:prev_hash(KB),
    PrevHash = aeser_api_encoder:encode(key_block_hash, PrevHash0),
    Height = aec_blocks:height(KB),
    aec_parent_chain_block:new(Hash, Height, PrevHash).

