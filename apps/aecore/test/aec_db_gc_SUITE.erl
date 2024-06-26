-module(aec_db_gc_SUITE).

%% common_test exports
-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% test case exports
-export([main_test/1,
         calls_test/1,
         last_switch_test/1 ]).

-include_lib("common_test/include/ct.hrl").

-define(MAX_MINED_BLOCKS, 20).
-define(NUM_ACCOUNTS, 20).
-define(NUM_GCED_NODES, 20).
-define(DUMMY_HASH, <<0:256>>).
-define(GC_HISTORY, 50).
-define(FORK_RESISTANCE, 5).

all() ->
    [{group, all_nodes}].

groups() ->
    [{all_nodes, [sequence], [{group, two_nodes},
                              {group, one_node}]},
     {two_nodes, [sequence],
      [main_test]},
     {one_node, [sequence],
      [ calls_test
      , last_switch_test ]}].

suite() ->
    [].

init_per_suite(Config0) ->
    Config1 = aec_metrics_test_utils:make_port_map([dev1, dev2], Config0),
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks,
                     <<"garbage_collection">> => #{<<"enabled">> => true,
                                                   <<"history">> => ?GC_HISTORY}},
               <<"sync">> => #{<<"single_outbound_per_group">> => false,
                               <<"sync_allowed_height_from_top">> => ?FORK_RESISTANCE},
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

    Ctxt = rpc:call(N1, aec_db, new_tree_context, [dirty, accounts]),
    ok = rpc:call(N1, aec_db, enter_tree_node, [?DUMMY_HASH, lists:duplicate(17, <<>>), Ctxt]),

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
    ct:log("Height = ~p", [H1]),
    true = H1 < ?GC_HISTORY + ?FORK_RESISTANCE,
    Primary1 = primary(N1),
    ct:log("Primary1 = ~p", [Primary1]),

    true = has_key(N1, ?DUMMY_HASH, Primary1),

    %% Mining of another keyblock starts second GC phase
    mine_until_height(N1, N2, ?GC_HISTORY + ?FORK_RESISTANCE + 2),

    Primary2 = primary(N1),
    ct:log("Primary2 = ~p", [Primary2]),

    {false, _} = {is_primary(N1, Primary1), Primary1},


    false = has_key(N1, ?DUMMY_HASH, Primary2),

    ok.


%% ==================================================
%% Private functions
%% ==================================================

primary(N) ->
    rpc:call(N, aec_db, primary_state_tab, [accounts]).

is_primary(N, P) ->
    primary(N) =:= P.

has_key(N, Key, Tab) ->
    case rpc:call(N, mnesia, dirty_read, [Tab, Key]) of
        [_|_] -> true;
        _     -> false
    end.

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
    timer:sleep(200),
    H = aec_headers:height(rpc:call(ControlNode, aec_chain, top_header, [])),
    if H < TargetHeight ->
            aecore_suite_utils:mine_key_blocks(MinerNode, 1),
            %% If there was a sync delay we might overshoot, so explicitly wait until each block is mined and synced
            block_while(fun () -> aec_headers:height(rpc:call(ControlNode, aec_chain, top_header, [])) == H end, 300, 100),
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
    add_spend_tx(Node, Amount, Fee, Nonce, TTL, Recipient, SenderPubkey,
                SenderPrivkey, <<>>).

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Recipient, SenderPubkey,
             SenderPrivkey, Payload) ->
    SenderId = aeser_id:create(account, SenderPubkey),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => Payload,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aec_test_utils:sign_tx(Tx, SenderPrivkey),
    Res = rpc:call(Node, aec_tx_pool, push, [STx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.

new_pubkey() ->
    #{public := PubKey} = enacl:sign_keypair(),
    PubKey.

calls_test(_Config) ->
    aecore_suite_utils:use_api(oas3),   % GC-related return codes only in OAS3 for now
    [N1] = [aecore_suite_utils:node_name(X) || X <- [dev1]],
    ok = aecore_suite_utils:subscribe(N1, gc),
    H0 = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    ct:log("Current height is ~p", [H0]),
    %% create a new account and seed it with tokens
    #{ public := OwnerPubkey, secret := OwnerPrivkey } = enacl:sign_keypair(),
    OwnerAddress = aeser_api_encoder:encode(account_pubkey, OwnerPubkey),
    SeedAmt = 100000000000 * aec_test_utils:min_gas_price(),
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
                            Nonce, 0, PubKey, PubKey, PrivKey,
                            <<"calls_test">>),
            {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [EncHash], ?MAX_MINED_BLOCKS)
        end,
    Spend(OwnerPubkey, OwnerPrivkey),
    Spend(OwnerPubkey, OwnerPrivkey),
    Spend(OwnerPubkey, OwnerPrivkey),
    Spend(OwnerPubkey, OwnerPrivkey),
    %% mine beyond the GC
    ct:log("Mining beyond the GC point"),
    ct:log("GC server state: ~p", [rpc:call(N1, sys, get_state, [aec_db_gc])]),
    {ok, Mined1} = aecore_suite_utils:mine_key_blocks(N1, ?GC_HISTORY + ?FORK_RESISTANCE + 1),
    ct:log("Blocks mined: ~p", [length(Mined1)]),
    GcSwitch1 = await_gc_switch(),
    0 = GcSwitch1 rem ?GC_HISTORY,
    await_scans_complete(),
    H1 = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    ct:log("Current height is ~p", [H1]),
    ct:log("Last GC switch: ~p", [rpc:call(N1, aec_db_gc, info, [[last_gc]])]),
    {ok, Mined2} = aecore_suite_utils:mine_key_blocks(N1, ?GC_HISTORY),
    ct:log("Mined ~p keyblocks", [length(Mined2)]),
    GcSwitch2 = await_gc_switch(),
    0 = GcSwitch2 rem ?GC_HISTORY,
    ct:log("Second GC switch (and clearing tables)", []),
    {ok, 410, _} = aehttp_integration_SUITE:get_contract_call_object(ContractCreateTxHash),
    {ok, 410, _} = aehttp_integration_SUITE:get_contract_call_object(ContractCallTxHash),
    {ok, 200, _} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(OwnerAddress),
    {ok, 410, _} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_and_height_sut(OwnerAddress, ContractCreateHeight),
    aecore_suite_utils:unsubscribe(N1, gc),
    ok.

%% Simulate the case where we've been GC:ing with an older version, where the height of
%% the last switch wasn't recorded persistently. The important thing is that it doesn't
%% default to a lower height (say, zero), tricking the GC to start sweeping prematurely.
%% The safe bet is therefore to default to the top height.
%%
last_switch_test(Config) ->
    N1 = aecore_suite_utils:node_name(dev1),
    ok = rpc:call(N1, mnesia, dirty_delete, [aec_chain_state, last_gc_switch]),
    ok = aecore_suite_utils:stop_node(dev1, Config),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(N1),
    Top = rpc:call(N1, aec_chain, top_height, []),
    #{last_gc := LastGC} = rpc:call(N1, aec_db_gc, info, [[last_gc]]),
    ct:log("Top = ~p, LastGC = ~p", [Top, LastGC]),
    Top = LastGC,
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


await_scans_complete() ->
    receive
        {gproc_ps_event, gc, #{info := scans_complete}} ->
            ok;
        OtherMsg ->
            ct:log("Got OTHER: ~p", [OtherMsg]),
            error({unexpected_msg, OtherMsg})
    after 10000 ->
            error({timeout, waiting_for_scans_complete})
    end.

await_gc_switch() ->
    receive
        {gproc_ps_event, gc, #{info := {gc_switch, AtHeight}}} ->
            ct:log("Got GC switch notification for height ~p", [AtHeight]),
            AtHeight
    after 10000 ->
            error({timeout, waiting_for_gc_switch})
    end.
