-module(aecore_txs_SUITE).

%% This code is brutaly copied form aecore_sync_SUITE and should use joined code base.

%% common_test exports
-export(
   [
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [ txs_gc/1
   ]).


-include_lib("common_test/include/ct.hrl").

all() ->
    [ txs_gc
    ].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.txs"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    aecore_suite_utils:create_configs(Config1),
    aecore_suite_utils:make_multi(Config1),
    [{nodes, [aecore_suite_utils:node_tuple(dev1)]} | Config1].

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(dev1, Config),
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
txs_gc(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),

    %% Mine a block to get some funds. Height=1
    aecore_suite_utils:mine_key_blocks(N1, 1),

    %% Add a bunch of transactions...
    add_spend_tx(N1, 1000, 1,  1,  10), %% Ok
    add_spend_tx(N1, 1000, 1,  2,  10), %% Should expire ?EXPIRE_TX_TTL after
                                        %% next TX is on chain = 2 + 2 = 4
    add_spend_tx(N1, 1000, 10, 2,  10), %% Duplicate should be preferred
    add_spend_tx(N1, 1000, 1,  3,  10), %% Ok

    add_spend_tx(N1, 1000, 1,  5,  10), %% Non consecutive nonce
    add_spend_tx(N1, 1000, 1,  7,  10), %% Non consecutive nonce
    add_spend_tx(N1, 1000, 1,  8,  5),  %% Short TTL - expires at 5 + 2 = 7

    %% Now there should be 7 transactions in mempool
    {ok, Txs1} = rpc:call(N1, aec_tx_pool, peek, [infinity]),
    {7, _} = {length(Txs1), Txs1},

    %% Mine two key blocks to get a micro-block in between. Height=3
    aecore_suite_utils:mine_key_blocks(N1, 2),

    %% Now there should be 4 transactions in mempool
    {ok, Txs2} = rpc:call(N1, aec_tx_pool, peek, [infinity]),
    {4, _} = {length(Txs2), Txs2},

    %% Mine 1 more key blocks then one TX should be GC:ed. Height=4
    aecore_suite_utils:mine_key_blocks(N1, 1),

    %% Now there should be 3 transactions in mempool
    {ok, Txs3} = rpc:call(N1, aec_tx_pool, peek, [infinity]),
    {3, _} = {length(Txs3), Txs3},

    %% Add the missing tx
    add_spend_tx(N1, 1000, 1,  4,  10), %% consecutive nonce

    %% Mine 2 block - should _consume_ two Txs i.e. two left. Height=6
    aecore_suite_utils:mine_key_blocks(N1, 2),

    %% Now there should be 2 transactions in mempool
    {ok, Txs4} = rpc:call(N1, aec_tx_pool, peek, [infinity]),
    {2, _} = {length(Txs4), Txs4},

    %% Mine 1 more blocks then another TX should be GC:ed. Height=7
    aecore_suite_utils:mine_key_blocks(N1, 1),

    %% Now there should be 1 transaction in mempool
    {ok, Txs5} = rpc:call(N1, aec_tx_pool, peek, [infinity]),
    {1, _} = {length(Txs5), Txs5},

    %% Mine 2 more blocks then all TXs should be GC:ed. Height=9
    aecore_suite_utils:mine_key_blocks(N1, 2),

    %% Now there should be no transactions in mempool
    {ok, Txs6} = rpc:call(N1, aec_tx_pool, peek, [infinity]),
    {0, _} = {length(Txs6), Txs6},

    ok = aecore_suite_utils:check_for_logs([dev1], Config).

add_spend_tx(Node, Amount, Fee, Nonce, TTL) ->
    Params = #{ sender => maps:get(pubkey, patron()), recipient => new_pubkey(),
                amount => Amount, nonce => Nonce, ttl => TTL, payload => <<>>, fee => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aetx_sign:sign(Tx, maps:get(privkey, patron())),
    rpc:call(Node, aec_tx_pool, push, [STx]).

new_pubkey() ->
    #{ public := PubKey } = enacl:sign_keypair(),
    PubKey.

patron() ->
    #{ pubkey  => <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,
                    73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
       privkey => <<230,169,29,99,60,119,207,87,113,50,157,51,84,179,188,239,27,
                    197,224,50,196,61,112,182,211,90,249,35,206,30,183,77,206,
                    167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,
                    187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>
      }.
