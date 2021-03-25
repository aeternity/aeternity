-module(aecore_mempool_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    start_node/1,
    mine_a_key_block/1,
    push_7_txs/1,
    transaction_over_the_account_nonce_limit_fails/1,
    push_tx_skipped_nonce/1,
    mine_key_blocks_to_gc_txs/1,
    invalid_GCed_tx_does_not_reenter_pool/1,
    stop_node/1
   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MINE_RATE, 100).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).
-define(NODES, [dev1]).
-define(ACCOUNT_NONCE_LIMIT, 7).
-define(REWARD_DELAY, 2).
-define(GC_TTL, 8). %% HARDCODED IN THE CODE

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [sequence],
      [{group, tx_created},
       {group, tx_received}
       ]},
     {tx_created, [sequence],
      [{group, common_tests}]},
     {tx_received, [sequence],
      [{group, common_tests}]},
     {common_tests, [sequence],
      [{group, tx_push},
       {group, gc}
       ]},
     {tx_push, [sequence],
      [push_7_txs,
       transaction_over_the_account_nonce_limit_fails
      ]},
     {gc, [sequence],
      [push_tx_skipped_nonce,
       mine_key_blocks_to_gc_txs,
       %% this pushes the exact same transaction again
       push_tx_skipped_nonce,
       mine_key_blocks_to_gc_txs,
       invalid_GCed_tx_does_not_reenter_pool
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    %% Do not use 'instant_mining', as it short-cuts header validation/whitelist tests
    aecore_suite_utils:init_per_suite(?NODES,
                                      #{ <<"sync">> =>
                                             #{<<"sync_allowed_height_from_top">> => 0}
                                       , <<"mempool">> =>
                                             #{ <<"tx_ttl">> => ?GC_TTL, %% default 2 weeks
                                                <<"nonce_offset">> => ?ACCOUNT_NONCE_LIMIT, %% default 5
                                                <<"cache_size">> => 2 %% default 200
                                              }
                                       , <<"mining">> =>
                                             #{ <<"expected_mine_rate">> => ?MINE_RATE,
                                                %% this is important so beneficiary can spend
                                                <<"beneficiary_reward_delay">> => ?REWARD_DELAY}},
                                      [{add_peers, true}],
                                      [{symlink_name, "latest.mempool"},
                                       {test_module, ?MODULE}]
                                      ++ Config).

end_per_suite(Config) ->
    [aecore_suite_utils:stop_node(D, Config) || D <- ?NODES],
    ok.

init_per_group(all, Config) ->
    [{nodes, [aecore_suite_utils:node_tuple(D) || D <- ?NODES]} | Config];
init_per_group(EventType, Config) when EventType =:= tx_created;
                                       EventType =:= tx_received ->
    [{push_event, EventType} | Config];
init_per_group(common_tests, Config) ->
    Config;
init_per_group(_Group, Config) ->
    start_node(Config),
    Config.

end_per_group(Group, _Config) when Group =:= all;
                                   Group =:= tx_created;
                                   Group =:= tx_received;
                                   Group =:= common_tests ->
    ok;
end_per_group(_Group, Config) ->
    stop_node(Config),
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

stop_and_check(Ns, Config) ->
    lists:foreach(
      fun(N) ->
              aecore_suite_utils:stop_node(N, Config)
      end, Ns),
    ok = aecore_suite_utils:check_for_logs(Ns, Config).

start_node(Node, Config) ->
    aecore_suite_utils:start_node(Node, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(Node)),
    ok = aecore_suite_utils:check_for_logs([Node], Config),
    ok.

start_node(Config) ->
    Node = dev1,
    start_node(Node, Config),
    mine_blocks_to_receive_reward(Config),
    NodeName = aecore_suite_utils:node_name(Node),
    case rpc:call(NodeName, aec_tx_pool, peek, [infinity]) of
        {ok, []} -> ok;
        {ok, _} ->
            {ok, _} = aecore_suite_utils:mine_blocks(NodeName, ?GC_TTL, ?MINE_RATE, key, #{}),
            {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
            ok
    end.

stop_node(Config) -> stop_and_check([dev1], Config).

mine_a_key_block(_Config) ->
    Node = dev1,
    NName= aecore_suite_utils:node_name(Node),
    {ok, [Block]} = aecore_suite_utils:mine_blocks(NName, 1, ?MINE_RATE, key, #{}),
    Top = rpc:call(NName, aec_chain, top_block, [], 5000),
    ct:log("top of chain ~p: ~p (mined ~p)", [Node, Top, Block]),
    {Top, Top} = {Top, Block},
    Top.

mine_blocks_to_receive_reward(_Config) ->
    Node = dev1,
    NName= aecore_suite_utils:node_name(Node),
    {ok, _} = aecore_suite_utils:mine_blocks(NName, ?REWARD_DELAY + 1, ?MINE_RATE, key, #{}),
    ok.

push_7_txs(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    %% precondition
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    Payloads = [<<"one">>, <<"two">>, <<"three">>, <<"four">>, <<"five">>,
                <<"six">>, <<"seven">>],
    %% ensure we push the maximum allowed txs
    ?ACCOUNT_NONCE_LIMIT = length(Payloads),
    lists:foreach(
        fun(Payload) ->
            STx = prepare_spend_tx(Node, #{payload => Payload}),
            ok = push(NodeName, STx, Config)
        end,
        Payloads),
    {ok, PoolTxs} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ?ACCOUNT_NONCE_LIMIT = length(PoolTxs),
    PoolPayloads =
        lists:map(
            fun(STx) ->
                {spend_tx, SpendTx} = aetx:specialize_type(aetx_sign:tx(STx)),
                aec_spend_tx:payload(SpendTx)
            end,
            PoolTxs),
    SortedPayloads = lists:sort(Payloads),
    SortedPoolPayloads = lists:sort(PoolPayloads),
    SortedPoolPayloads = SortedPayloads,
    ok.

transaction_over_the_account_nonce_limit_fails(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {_, Pub} = aecore_suite_utils:sign_keys(Node),
    %% ensure the transactions are in pool:
    {ok, NextNonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    {value, Acc} = rpc:call(NodeName, aec_chain, get_account, [Pub]),
    ct:log("Account: ~p", [Acc]),
    CurrentNonce = aec_accounts:nonce(Acc),
    {CurrentNonce, NextNonce} = {CurrentNonce, CurrentNonce + 1 + ?ACCOUNT_NONCE_LIMIT},
    SpendTx = prepare_spend_tx(Node),
    ct:log("Spend tx: ~p", [SpendTx]),
    {error, nonce_too_high} = push(NodeName, SpendTx, Config),
    ok.

push_tx_skipped_nonce(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {_, Pub} = aecore_suite_utils:sign_keys(Node),
    {ok, NextNonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    ct:log("NextNonce: ~p", [NextNonce]),
    SpendTx = prepare_spend_tx(Node, #{nonce => NextNonce + 1}),
    ct:log("Spend tx: ~p", [SpendTx]),
    ok = push(NodeName, SpendTx, Config),
    {ok, [SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ok.

mine_key_blocks_to_gc_txs(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {ok, PoolTxs1} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),

    %% mine keyblocks - 1 and ensure txs are not yet GCed
    lists:foreach(
        fun(Idx) ->
            ct:log("Keyblock: ~p", [Idx]),
            {ok, [_]} = aecore_suite_utils:mine_blocks(NodeName, 1, ?MINE_RATE, key, #{}),
            %% ensure txs are not yet GCed
            {ok, PoolTxs2} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
            SortedTxs1 = lists:sort(PoolTxs1),
            SortedTxs2 = lists:sort(PoolTxs2),
            {SortedTxs1, SortedTxs1} = {SortedTxs1, SortedTxs2}
        end,
        lists:seq(1, ?GC_TTL - 1)),
    mine_a_key_block(Config),
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ok.

invalid_GCed_tx_does_not_reenter_pool(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {_, Pub} = aecore_suite_utils:sign_keys(Node),
    %% precondition
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    {ok, NextNonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    ct:log("NextNonce: ~p", [NextNonce]),
    %% prepare a tx that skipps a nonce; we will later make it invalid
    SkippedNonce = NextNonce + 1,
    InvalidSpendTx = prepare_spend_tx(Node, #{nonce => SkippedNonce,
                                              payload => <<"this will be invalid soon">>}),
    ct:log("Spend tx: ~p", [InvalidSpendTx]),
    ok = push(NodeName, InvalidSpendTx, Config),
    {ok, [InvalidSpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ok = mine_key_blocks_to_gc_txs(Config),
    %% ensure InvalidSpendTx is now GCed
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    [Spend1, Spend2] =
        lists:map(
            fun(Payload) ->
                STx = prepare_spend_tx(Node, #{payload => Payload}),
                ok = push(NodeName, STx, Config),
                STx
            end,
            [<<"tx1">>, <<"tx2 that invalidates the GCed tx">>]),
    TxNonceFun = 
        fun(STx) ->
            {spend_tx, SpendTx} = aetx:specialize_type(aetx_sign:tx(STx)),
            aec_spend_tx:nonce(SpendTx)
        end,
    %% assert nonce assumptions and pool transactions
    NextNonce = TxNonceFun(Spend1),
    SkippedNonce = TxNonceFun(Spend2),
    {ok, PoolTxs} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    SortedTxs = lists:sort([Spend1, Spend2]),
    SortedPoolTxs = lists:sort(PoolTxs),
    {SortedTxs, SortedTxs} = {SortedTxs, SortedPoolTxs},
    ok = push(NodeName, InvalidSpendTx, Config),
    ok.


prepare_spend_tx(Node) ->
    prepare_spend_tx(Node, #{}).

prepare_spend_tx(Node, Opts) ->
    {Priv, Pub} = aecore_suite_utils:sign_keys(Node),
    prepare_spend_tx(Node, Opts, Pub, Priv).

prepare_spend_tx(Node, Opts, Pub, Priv) ->
    NodeName = aecore_suite_utils:node_name(Node),
    {ok, Nonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    Params =
        maps:merge(
            #{sender_id    => aeser_id:create(account, Pub),
              recipient_id => aeser_id:create(account, Pub),
              amount       => 1,
              fee          => ?SPEND_FEE,
              nonce        => Nonce,
              payload      => <<"">>},
            Opts),

    {ok, Tx} = aec_spend_tx:new(Params),
    aec_test_utils:sign_tx(Tx, Priv, false).

push(NodeName, SignedTx, Config) ->
    rpc:call(NodeName, aec_tx_pool, push, [SignedTx]).
