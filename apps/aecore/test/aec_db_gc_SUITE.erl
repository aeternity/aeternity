-module(aec_db_gc_SUITE).

%% common_test exports
-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% test case exports
-export([main_test/1,
         calls_test/1]).

-include_lib("common_test/include/ct.hrl").

-define(MAX_MINED_BLOCKS, 20).
-define(NUM_ACCOUNTS, 20).
-define(NUM_GCED_NODES, 20).
-define(DUMMY_HASH, <<0:256>>).
-define(GC_INTERVAL, 50).

all() ->
    [{group, all_nodes}].

groups() ->
    [{all_nodes, [sequence], [{group, two_nodes},
                              {group, one_node}]},
     {two_nodes, [sequence],
      [main_test]},
     {one_node, [sequence],
      [calls_test]}].

suite() ->
    [].

init_per_suite(Config0) ->
    Config1 = aec_metrics_test_utils:make_port_map([dev1, dev2], Config0),
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks,
                     <<"garbage_collection">> => #{<<"enabled">> => true,
                                                   <<"interval">> => ?GC_INTERVAL,
                                                   <<"history">> => ?GC_INTERVAL + 10}},
               <<"sync">> => #{<<"single_outbound_per_group">> => false},
               <<"mempool">> => #{<<"tx_ttl">> => 100},
               <<"mining">> => #{<<"micro_block_cycle">> => 100,
                                 <<"expected_mine_rate">> => 100}},
    Accounts = [new_pubkey() || _ <- lists:seq(1, ?NUM_ACCOUNTS)],
    Config2 = aecore_suite_utils:init_per_suite([dev1, dev2],
                                                DefCfg,
                                                [{add_peers, true},
                                                 #{dev2 => #{<<"chain">> =>
                                                                 #{<<"db_backend">> => <<"mnesia">>,
                                                                   <<"garbage_collection">> =>
                                                                       #{<<"enabled">> => false}}}}],
                                                [{instant_mining, true},
                                                 {symlink_name, "latest.gc"},
                                                 {test_module, ?MODULE},
                                                 {accounts, Accounts} | Config1]),
    aecore_suite_utils:start_node(dev1, Config2),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ok = aecore_suite_utils:check_for_logs([dev1], Config2),
    [N1, N2] = [aecore_suite_utils:node_name(X) || X <- [dev1, dev2]],

    node_log_details(N1, up),

    aecore_suite_utils:mock_mempool_nonce_offset(N1, 200),
    Fee   = 1500000 * aec_test_utils:min_gas_price(),
    Txs = lists:foldl(
      fun ({Nonce, PK}, Acc) ->
              {ok, Tx} = add_spend_tx(N1, 10, Fee, Nonce, 100, PK),
              [Tx|Acc]
      end, [], lists:zip(lists:seq(1, length(Accounts)), Accounts)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, Txs, ?MAX_MINED_BLOCKS),
    aecore_suite_utils:unmock_mempool_nonce_offset(N1),
    check_accounts(Config2),

    node_log_details(N1, accounts_on_chain),

    ok = rpc:call(N1, mnesia, dirty_write,
                  [{aec_account_state, ?DUMMY_HASH, lists:duplicate(17, <<>>)}]),

    aecore_suite_utils:start_node(dev2, Config2),
    aecore_suite_utils:connect(N2),
    ok = aecore_suite_utils:check_for_logs([dev2], Config2),
    Config2.

end_per_suite(Config) ->
    [catch begin
               {ok, DbCfg} = node_db_cfg(X),
               aecore_suite_utils:stop_node(X, Config),
               aecore_suite_utils:delete_node_db_if_persisted(DbCfg)
           end || X <- [dev1, dev2]].

init_per_group(Group, Config) when Group =:= two_nodes;
                                   Group =:= one_node ->
    Config1 = config(Config),
    InitialApps = {aec_test_utils:running_apps(), aec_test_utils:loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [{initial_apps, InitialApps} | Config1];
init_per_group(all_nodes, Config) ->
    Config.

end_per_group(Group, Config) when Group =:= two_nodes;
                                   Group =:= one_node ->
    ct:log("Metrics: ~p", [aec_metrics_test_utils:fetch_data()]),
    ok = aec_metrics_test_utils:stop_statsd_loggers(),
    {_, {OldRunningApps, OldLoadedApps}} = proplists:lookup(initial_apps, Config),
    ok = aec_test_utils:restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps);
end_per_group(all_nodes, _Config) ->
   ok.


init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

main_test(_Config) ->
    [N1, N2] = [aecore_suite_utils:node_name(X) || X <- [dev1, dev2]],
    H1 = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    true = H1 < ?GC_INTERVAL,
    true = has_key(N1, ?DUMMY_HASH, aec_account_state),

    %% we mine just enough to start first GC phase - collection of reachable hashes
    mine_until_height(N1, N2, ?GC_INTERVAL), % aecore_suite_utils:mine_key_blocks(N2, ?GC_INTERVAL - H1),

    node_log_details(N1, after_mining_0),

    block_while(fun () ->
                        case gc_state(N1) of
                            {ready, Data} ->
                                HashesTab = element(size(Data), Data),
                                true = is_reference(HashesTab),
                                self() ! {n1_hashes, rpc:call(N1, ets, tab2list, [HashesTab])},
                                false;
                            X ->
                                ct:log("////////// GC STATE = ~p~n", [X]),
                                true
                        end
                end, 20, 500),

    GCedHashes = receive {n1_hashes, Xs} -> Xs end,

    ct:log("////////// HASHES count = ~p~n", [length(GCedHashes)]),
    node_log_details(N1, gc_ready),

    monitor_node(N1, true),
    %% Mining of another keyblock starts second GC phase - storing cache to mnesia table and restart
    mine_until_height(N1, N2, ?GC_INTERVAL + 1),
    receive {nodedown, N1} -> ct:log("////////// ~p restarted~n", [N1]) end,

    block_while(fun () -> not net_kernel:hidden_connect_node(N1) end, 300, 300),

    block_while(fun () -> not started(N1) end, 300, 300),

    block_while(fun () -> has_table(N1, aec_account_state_gced) end, 500, 100),

    false = has_table(N1, aec_account_state_gced),
    false = has_key(N1, ?DUMMY_HASH, aec_account_state),

    Hashes = rpc:call(N1, mnesia, dirty_select, [aec_account_state, [{{'_','$1','$2'},[],[{{'$1','$2'}}]}]]),
    [] = GCedHashes -- Hashes,

    ok.


%% ==================================================
%% Private functions
%% ==================================================

gc_state(N) ->
    rpc:call(N, sys, get_state, [aec_db_gc]).

started(N) ->
    case rpc:call(N, init, get_status, []) of
        {started, _} ->
            ct:log("////////// Node started ~p", [N]),
            true;
        _ ->
            false
    end.

mnesia_system_info(N, X) ->
    rpc:call(N, mnesia, system_info, [X]).

has_key(N, Key, Tab) ->
    case rpc:call(N, mnesia, dirty_read, [Tab, Key]) of
        [_|_] -> true;
        _     -> false
    end.

has_table(N, Tab) ->
    Ts = mnesia_system_info(N, tables),
    is_list(Ts) andalso lists:member(Tab, Ts).

node_log_details(N, Prefix) ->
    ct:log("////////// ~p | ~p S = ~p~n", [N, Prefix, catch rpc:call(N, sys, get_state, [aec_db_gc])]),
    ct:log("////////// ~p | ~p H = ~p~n", [N, Prefix, catch rpc:call(N, aec_chain, top_header, [])]),
    ok.


block_while(Test, Repeats, MilliSecs) ->
    block_while_(0, Test, Repeats, MilliSecs).

block_while_(X, _Test, Repeats, _MilliSecs) when is_integer(Repeats), X > Repeats ->
    {exhausted, Repeats};
block_while_(X, Test, Repeats, MilliSecs) ->
    case Test() of
        false -> ok;
        true  ->
            timer:sleep(MilliSecs),
            block_while_(X + 1, Test, Repeats, MilliSecs)
    end.

mine_until_height(ControlNode, MinerNode, TargetHeight) ->
    timer:sleep(500),
    H = aec_headers:height(rpc:call(ControlNode, aec_chain, top_header, [])),
    if H < TargetHeight ->
            aecore_suite_utils:mine_key_blocks(MinerNode, 1),
            mine_until_height(ControlNode, MinerNode, TargetHeight);
       true ->
            ok
    end.


check_accounts(Config) ->
    N1 = aecore_suite_utils:node_name(dev1),
    lists:foreach(
      fun (PK) ->
              {value, _} = rpc:call(N1, aec_chain, get_account, [PK])
      end, ?config(accounts, Config)),
    ok.


config(Config) ->
    [{devs, [dev1]}, {nodes, [aecore_suite_utils:node_tuple(dev1)]} | Config].

node_db_cfg(Node) ->
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(
                    fun (M, F, A)->
                            rpc:call(aecore_suite_utils:node_name(Node), M, F, A, 5000)
                    end),
    {ok, DbCfg}.

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Recipient) ->
    Sender = aecore_suite_utils:patron(),
    add_spend_tx(Node, Amount, Fee, Nonce, TTL, Recipient,
                 maps:get(pubkey, Sender),
                 maps:get(privkey, Sender)).

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Recipient, SenderPubkey,
             SenderPrivkey) ->
    SenderId = aeser_id:create(account, SenderPubkey),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => <<>>,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aec_test_utils:sign_tx(Tx, SenderPrivkey),
    Res = rpc:call(Node, aec_tx_pool, push, [STx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.

new_pubkey() ->
    #{public := PubKey} = enacl:sign_keypair(),
    PubKey.

calls_test(_Config) ->
    [N1] = [aecore_suite_utils:node_name(X) || X <- [dev1]],
    H0 = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    ct:log("Current height is ~p", [H0]),
    %% create a new account and seed it with tokens
    #{ public := OwnerPubkey, secret := OwnerPrivkey } = enacl:sign_keypair(),
    OwnerAddress = aeser_api_encoder:encode(account_pubkey, OwnerPubkey),
    SeedAmt = 1000000 * aec_test_utils:min_gas_price(),
    {ok, SpendNonce} = rpc:call(N1, aec_next_nonce, pick_for_account,
                                [maps:get(pubkey,aecore_suite_utils:patron())]),
    {ok, ESpendHash} =
        add_spend_tx(N1, SeedAmt, 1500000 * aec_test_utils:min_gas_price(), %% fee
                     SpendNonce, 0, OwnerPubkey),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [ESpendHash], ?MAX_MINED_BLOCKS),

    %% create a contract and call it
    {ok, EncodedCode} = aehttp_integration_SUITE:get_contract_bytecode(identity),
    {ok, EncodedInitCallData} = aehttp_integration_SUITE:encode_call_data(identity, "init", []),
    ValidEncoded = #{ owner_id    => OwnerAddress,
                      code        => EncodedCode,
                      vm_version  => latest_sophia_vm(),
                      abi_version => latest_sophia_abi(),
                      deposit     => 2,
                      amount      => 1,
                      gas         => 600,
                      gas_price   => aec_test_utils:min_gas_price(),
                      fee         => 400000 * aec_test_utils:min_gas_price(),
                      call_data   => EncodedInitCallData},

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        aehttp_integration_SUITE:get_contract_create(ValidEncoded),
    {ok, ContractCreateTxHash} = sign_and_post_tx(EncodedUnsignedContractCreateTx, OwnerPrivkey, N1),

    {ok, 200, #{<<"block_height">> := -1}} = aehttp_integration_SUITE:get_transactions_by_hash_sut(ContractCreateTxHash),
    {ok, 404, #{<<"reason">> := <<"Tx not mined">>}} = aehttp_integration_SUITE:get_contract_call_object(ContractCreateTxHash),

    % mine the contract create
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [ContractCreateTxHash], ?MAX_MINED_BLOCKS),
    %% now the call object is available
    {ok, 200, #{<<"block_height">> := ContractCreateHeight,
                <<"block_hash">> := ContractCreateBlockHash }} =
        aehttp_integration_SUITE:get_transactions_by_hash_sut(ContractCreateTxHash),
    ct:log("Contract had been included in a microblock ~p at height ~p",
           [ContractCreateBlockHash, ContractCreateHeight]),
    {ok, 200, _} = aehttp_integration_SUITE:get_contract_call_object(ContractCreateTxHash),

    %% create a call
    {ok, EncodedCallData} = aehttp_integration_SUITE:encode_call_data(identity, "main_", ["42"]),
    ContractCallEncoded = #{ caller_id   => OwnerAddress,
                             contract_id => EncodedContractPubKey,
                             abi_version => latest_sophia_abi(),
                             amount      => 1,
                             gas         => 1000,
                             gas_price   => aec_test_utils:min_gas_price(),
                             fee         => 500000 * aec_test_utils:min_gas_price(),
                             call_data   => EncodedCallData},
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallTx}} = aehttp_integration_SUITE:get_contract_call(ContractCallEncoded),
    {ok, ContractCallTxHash} = sign_and_post_tx(EncodedUnsignedContractCallTx, OwnerPrivkey, N1),
    {ok, 200, #{<<"block_height">> := -1}} = aehttp_integration_SUITE:get_transactions_by_hash_sut(ContractCallTxHash),
    {ok, 404, #{<<"reason">> := <<"Tx not mined">>}} = aehttp_integration_SUITE:get_contract_call_object(ContractCallTxHash),
    %% mine the contract call
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [ContractCallTxHash], ?MAX_MINED_BLOCKS),
    {ok, 200, #{<<"block_height">> := ContractCallHeight}} = aehttp_integration_SUITE:get_transactions_by_hash_sut(ContractCallTxHash),
    ct:log("Contract call had been included in a microblock at height ~p", [ContractCallHeight]),
    {ok, 200, _} = aehttp_integration_SUITE:get_contract_call_object(ContractCallTxHash),
    {ok, ESpendHash2} =
        add_spend_tx(N1, SeedAmt, 1500000 * aec_test_utils:min_gas_price(), %% fee
                     SpendNonce + 1, 0, OwnerPubkey),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [ESpendHash2], ?MAX_MINED_BLOCKS),
    Spend =
        fun(PubKey, PrivKey) ->
            {ok, Nonce} = rpc:call(N1, aec_next_nonce, pick_for_account,
                                        [PubKey]),
            {ok, EncHash} =
                add_spend_tx(N1, 1, 1500000 * aec_test_utils:min_gas_price(), %% fee
                            Nonce, 0, PubKey, PubKey, PrivKey),
            aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [EncHash], ?MAX_MINED_BLOCKS)
        end,
    Spend(OwnerPubkey, OwnerPrivkey),
    Spend(OwnerPubkey, OwnerPrivkey),
    Spend(OwnerPubkey, OwnerPrivkey),
    Spend(OwnerPubkey, OwnerPrivkey),
    %% mine beyond the GC
    aecore_suite_utils:mine_key_blocks(N1, ?GC_INTERVAL + 1),
    H1 = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    ct:log("Current height is ~p", [H1]),
    {ok, 200, _} = aehttp_integration_SUITE:get_contract_call_object(ContractCreateTxHash),
    {ok, 200, _} = aehttp_integration_SUITE:get_contract_call_object(ContractCallTxHash),
    {ok, 200, _} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(OwnerAddress),
    %% this should fail:
    {ok, 200, _} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_and_height_sut(OwnerAddress, ContractCreateHeight),
    ok.

latest_sophia_abi() ->
    aect_test_utils:latest_sophia_abi_version().

latest_sophia_vm() ->
    aect_test_utils:latest_sophia_vm_version().

sign_and_post_tx(EncodedUnsignedTx, SenderPrivkey, Node) ->
    {ok, SerializedUnsignedTx} = aeser_api_encoder:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    SignedTx = aec_test_utils:sign_tx(UnsignedTx, SenderPrivkey),
    Res = rpc:call(Node, aec_tx_pool, push, [SignedTx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))}.


