-module(aehttp_parent_connector_tests).

%% Test interactions with parent chains using chain simulators for AE and BTC nodes

-include_lib("eunit/include/eunit.hrl").

-define(BIG_AMOUNT, 10000000000000000000000000000 * aec_test_utils:min_gas_price()).

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
             {ok, _Connector} = aec_parent_connector:start_link(aehttpc_aeternity, on_demand, ParentHosts),
             {ok, CommitmentPubKey, StakerKeyPair, ParentSims}
     end,
     fun({ok, _CommitmentPubKey, _StakerKeyPair, ParentSims}) ->
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
                    %% aec_parent_connector:trigger_fetch(),
                    lists:foreach(
                        fun(#{host := Host, port := Port,
                            user := User, password := Password}) ->
                            ?assertMatch({ok, _TopHash}, aehttpc_aeternity:get_latest_block(Host, Port, User, Password, <<"Seed">>))
                        end, ae_parent_http_specs()),
                    ok
            end},
          {"Post a suitable spend TX to each parent chain and check it is in the commitment list",
            fun() ->
                    %% aec_parent_connector:trigger_fetch(),
                    lists:foreach(
                        fun(#{host := Host, port := Port,
                            user := User, password := Password}) ->
                            aec_chain_sim:add_keyblock(sim_name(Port)),
                            aec_chain_sim:add_keyblock(sim_name(Port)),
                            %% Post our local top hash as the commitment
                            Commitment = <<"kh_deadbeef">>,
                            {ok, TopHash} = aehttpc_aeternity:get_latest_block(Host, Port, User, Password, <<"Seed">>),
                            ok = aehttpc_aeternity:post_commitment(Host, Port, StakerPubKey, StakerPrivKey, CommitmentPubKey, Commitment),
                            %% Call the simulator directly to force our Tx in a block
                            aec_chain_sim:add_microblock(sim_name(Port)),
                            %% And create a keyblock
                            aec_chain_sim:add_keyblock(sim_name(Port)),
                            {ok, [{Acct, Payload}]} = aehttpc_aeternity:get_commitment_tx_in_block(Host, Port, User, Password, <<"Seed">>, TopHash, CommitmentPubKey),
                            ?assertMatch(Acct, aeser_api_encoder:encode(account_pubkey, StakerPubKey)),
                            {_Type, Val} =  aeser_api_encoder:decode(Payload),
                            ?assertEqual(<<"kh_deadbeef">>, Val)
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