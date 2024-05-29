-module(aec_db_safe_access_SUITE).

%% common_test exports
-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% test case exports
-export([ main_test/1
        ]).

-include_lib("common_test/include/ct.hrl").

-define(MAX_MINED_BLOCKS, 20).
-define(NUM_ACCOUNTS, 3).
-define(DUMMY_HASH, <<0:256>>).

all() ->
    [{group, one_node}].

groups() ->
    [{one_node, [sequence],
      [ main_test
      ]}].

suite() ->
    [].

init_per_suite(Config0) ->
    Config1 = aec_metrics_test_utils:make_port_map([dev1], Config0),
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks},
               <<"mining">> => #{<<"micro_block_cycle">> => 100,
                                 <<"expected_mine_rate">> => 100}},
    Accounts = [new_pubkey() || _ <- lists:seq(1, ?NUM_ACCOUNTS)],
    Config2 = aecore_suite_utils:init_per_suite([dev1],
                                                DefCfg,
                                                [{add_peers, true}],
                                                [{instant_mining, true},
                                                 {symlink_name, "latest.db_safe_access"},
                                                 {test_module, ?MODULE},
                                                 {accounts, Accounts} | Config1]),
    aecore_suite_utils:start_node(dev1, Config2),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ok = aecore_suite_utils:check_for_logs([dev1], Config2),
    N1 = aecore_suite_utils:node_name(dev1),

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

    Config2.

end_per_suite(Config) ->
    catch begin
               {ok, DbCfg} = node_db_cfg(dev1),
               aecore_suite_utils:stop_node(dev1, Config),
               aecore_suite_utils:delete_node_db_if_persisted(DbCfg)
          end.

init_per_group(Group, Config) when Group =:= one_node ->
    Config1 = config(Config),
    InitialApps = {aec_test_utils:running_apps(), aec_test_utils:loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [{initial_apps, InitialApps} | Config1];
init_per_group(all_nodes, Config) ->
    Config.

end_per_group(Group, Config) when Group =:= one_node ->
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

main_test(Config) ->
    N1 = aecore_suite_utils:node_name(dev1),
    H1 = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    ct:log("Height = ~p", [H1]),

    PKs = ?config(accounts, Config),

    %% Depending on how the bitpatterns of the PKs overlap we might not be able
    %% to find a MPT node, but let's try some clever heuristic...

    {ok, PK, PKKey, NodeHash} = find_tree_node(N1, PKs),
    safe_access_test(Config, PK, PKKey, NodeHash).

find_tree_node(_, []) ->
    ct:log("Couldn't find tree node - no test :sadpanda:", []),
    {error, no_tree_node};
find_tree_node(N, [PK | PKs]) ->
    {value, Acc} = rpc:call(N, aec_chain, get_account, [PK]),

    SerAcc = aec_accounts:serialize(Acc),
    PrimaryAccounts = primary(N),

    <<X:8, PKTail/binary>> = PK,
    case find_tree_node(N, PrimaryAccounts, PKTail, SerAcc, [X | lists:seq(32, 63)]) of
        {ok, PKKey, NodeHash} -> {ok, PK, PKKey, NodeHash};
        _ -> find_tree_node(N, PKs)
    end.

find_tree_node(_, _, _, _, []) ->
    none;
find_tree_node(N, PAccs, PKTail, SerAcc, [X | Xs]) ->
    PKKey = <<X:8, PKTail/binary>>,
    NodeHash = aec_hash:hash(header, aeser_rlp:encode([PKKey, SerAcc])),

    case has_key(N, NodeHash, PAccs) of
        true -> {ok, PKKey, NodeHash};
        _ -> find_tree_node(N, PAccs, PKTail, SerAcc, Xs)
    end.

safe_access_test(Config, PK, PKKey, NodeHash) ->
    ct:log("Testing with ~p (hash: ~p)", [PK, NodeHash]),
    N1 = aecore_suite_utils:node_name(dev1),

    {value, Acc} = rpc:call(N1, aec_chain, get_account, [PK]),
    {ok, Acc1} = aec_accounts:earn(Acc, 100000000000000),
    SerAcc = aec_accounts:serialize(Acc1),

    ok = rpc:call(N1, aec_db_gc, db_safe_access_scan, []),
    ct:log("Scanning all trees successfully", []),

    Ctxt = rpc:call(N1, aec_db, new_tree_context, [dirty, accounts]),
    ok = rpc:call(N1, aec_db, enter_tree_node, [NodeHash, [PKKey, SerAcc], Ctxt]),
    {value, Acc1} = rpc:call(N1, aec_chain, get_account, [PK]),

    {error, Reason} = rpc:call(N1, aec_db_gc, db_safe_access_scan, [accounts]),
    ct:log("Scanning 'accounts' failed as expected: ~p", [Reason]),

    aecore_suite_utils:stop_node(dev1, Config),

    aecore_suite_utils:start_node(dev1, Config, [{"AE__CHAIN__DB_SAFE_ACCESS", "true"}]),

    {badrpc, _} = rpc:call(N1, aec_chain, get_account, [PK]),

    timer:sleep(2000), %% Wait for node to fully get up to speed?!
    error = rpc:call(N1, aec_db_gc, db_safe_access_scan, []),

    ok.

%% ==================================================
%% Private functions
%% ==================================================

primary(N) ->
    rpc:call(N, aec_db, primary_state_tab, [accounts]).

has_key(N, Key, Tab) ->
    case rpc:call(N, mnesia, dirty_read, [Tab, Key]) of
        [_|_] -> true;
        _     -> false
    end.

node_log_details(N, Prefix) ->
    ct:log("////////// ~p | ~p S = ~p~n", [N, Prefix, catch rpc:call(N, sys, get_state, [aec_db_gc])]),
    ct:log("////////// ~p | ~p H = ~p~n", [N, Prefix, catch rpc:call(N, aec_chain, top_header, [])]),
    ok.

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
