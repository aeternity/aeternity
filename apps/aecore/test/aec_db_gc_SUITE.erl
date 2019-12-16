-module(aec_db_gc_SUITE).

%% common_test exports
-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% test case exports
-export([main_test/1]).

-include_lib("common_test/include/ct.hrl").

-define(MAX_MINED_BLOCKS, 20).
-define(NUM_ACCOUNTS, 20).
-define(NUM_GCED_NODES, 20).
-define(DUMMY_HASH, <<0:256>>).
-define(GC_INTERVAL, 50).

all() ->
    [{group, all_nodes}].

groups() ->
    [{all_nodes, [sequence], [{group, two_nodes}]},
     {two_nodes, [sequence],
      [main_test]}].

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
               <<"mining">> => #{<<"micro_block_cycle">> => 100}},
    Accounts = [new_pubkey() || _ <- lists:seq(1, ?NUM_ACCOUNTS)],
    Config2 = aecore_suite_utils:init_per_suite([dev1, dev2], DefCfg,
                                                [{add_peers, true}],
                                                [{symlink_name, "latest.sync"},
                                                 {test_module, ?MODULE},
                                                 {accounts, Accounts} | Config1]),
    aecore_suite_utils:start_node(dev1, Config2),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ok = aecore_suite_utils:check_for_logs([dev1], Config2),
    [N1, N2] = [aecore_suite_utils:node_name(X) || X <- [dev1, dev2]],

    node_log_details(N1, up),

    Fee   = 1500000 * aec_test_utils:min_gas_price(),
    lists:foreach(
      fun ({Nonce, PK}) ->
              {ok, Tx} = add_spend_tx(N1, 10, Fee, Nonce, 100, PK),
              aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx], ?MAX_MINED_BLOCKS)
      end, lists:zip(lists:seq(1, length(Accounts)), Accounts)),
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

init_per_group(two_nodes, Config) ->
    Config1 = config(Config),
    InitialApps = {aec_test_utils:running_apps(), aec_test_utils:loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [{initial_apps, InitialApps} | Config1];
init_per_group(all_nodes, Config) ->
    Config.

end_per_group(two_nodes, Config) ->
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
    aecore_suite_utils:mine_key_blocks(N2, ?GC_INTERVAL - H1),
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
    aecore_suite_utils:mine_key_blocks(N2, 1),
    receive {nodedown, N1} -> ct:log("////////// ~p restarted~n", [N1]) end,

    block_while(fun () -> not net_kernel:hidden_connect_node(N1) end, 30, 3000),

    block_while(fun () -> has_table(N1, aec_account_state_gced) end, 50, 1000),

    false = has_table(N1, aec_account_state_gced),
    false = has_key(N1, ?DUMMY_HASH, aec_account_state),

    Hashes = rpc:call(N1, mnesia, dirty_select, [aec_account_state, [{{'_','$1','$2'},[],[{{'$1','$2'}}]}]]),
    [] = GCedHashes -- Hashes,

    ok.


%% ==================================================
%% Private functions
%% ==================================================

%% log(Fmt, Args) ->
%%     file:write_file("/tmp/test.log", io_lib:format(Fmt, Args), [append]).


gc_state(N) ->
    rpc:call(N, sys, get_state, [aec_db_gc]).

%% process_exists(N, ProcName) ->
%%     is_pid(catch rpc:call(N, erlang, whereis, [ProcName])).

%% mnesia_dir(N) ->
%%     mnesia_system_info(N, directory).

mnesia_system_info(N, X) ->
    rpc:call(N, mnesia, system_info, [X]).

%% uniq_key(N, NoCollideTab) ->
%%     H = crypto:hash(sha256, integer_to_binary(erlang:system_time())),
%%     case has_key(N, H, NoCollideTab) of
%%         true  -> uniq_key(N, NoCollideTab);
%%         false -> H
%%     end.

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
    SenderId = aeser_id:create(account, maps:get(pubkey, Sender)),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => <<>>,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aec_test_utils:sign_tx(Tx, maps:get(privkey, Sender)),
    Res = rpc:call(Node, aec_tx_pool, push, [STx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.

new_pubkey() ->
    #{public := PubKey} = enacl:sign_keypair(),
    PubKey.
