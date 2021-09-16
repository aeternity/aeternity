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
    maybe_push_tx_out_cache/1,
    mine_key_blocks_to_gc_txs/1,
    invalid_GCed_tx_does_not_reenter_pool/1,
    repush_tx_skipped_nonce_is_stopped_by_cache/1,
    skipped_nonce_specific_cleanup/1,
    insufficient_funds_specific_cleanup/1,
    name_claim_to_unknown_commitement_cleanup/1,
    test_defaults/1,
    test_disabled/1,
    stop_node/1
   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MINE_RATE, 100).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).
-define(NODES, [dev1]).
-define(ACCOUNT_NONCE_LIMIT, 7).
-define(REWARD_DELAY, 2).
-define(GC_TTL, 20).
-define(CACHE_SIZE, 2). %% HARDCODED IN THE CODE

-define(COMMON_DEFAULT, 4).
-define(SPEND_DEFAULT, 5).
-define(INSUFFICIENT_FUNDS, 6).
-define(NONCE_TOO_HIGH, 7).

%% we use Alice to send txs to it
-define(ALICE, {
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>}).

%% we use Bob to initiate microblocks
-define(BOB, {
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>}).

-define(CAROL, {
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>}).

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [sequence],
      [{group, tx_created},
       {group, tx_received},
       {group, failed_attempts}
       ]},
     {tx_created, [sequence],
      [{group, common_tests}]},
     {tx_received, [sequence],
      [{group, common_tests},
       {group, garbage_collected_tx_can_not_enter_the_pool_if_stopped_by_cache}]},
     {common_tests, [sequence],
      [{group, tx_push},
       {group, garbage_collected_tx_can_enter_the_pool}
       ]},
     {tx_push, [sequence],
      [push_7_txs,
       transaction_over_the_account_nonce_limit_fails
      ]},
     {garbage_collected_tx_can_enter_the_pool, [sequence],
      [push_tx_skipped_nonce,
       maybe_push_tx_out_cache,
       mine_key_blocks_to_gc_txs,
       %% this pushes the exact same transaction again
       push_tx_skipped_nonce,
       mine_key_blocks_to_gc_txs,
       invalid_GCed_tx_does_not_reenter_pool
      ]},
     {garbage_collected_tx_can_not_enter_the_pool_if_stopped_by_cache, [sequence],
      [push_tx_skipped_nonce,
       mine_key_blocks_to_gc_txs,
       repush_tx_skipped_nonce_is_stopped_by_cache,
       %% if other transactions push this one out of cache, it is still accepted
       maybe_push_tx_out_cache,
       mine_key_blocks_to_gc_txs,
       push_tx_skipped_nonce
      ]},
     {failed_attempts, [sequence],
      [
       skipped_nonce_specific_cleanup,
       insufficient_funds_specific_cleanup,
       name_claim_to_unknown_commitement_cleanup,
       test_defaults,
       test_disabled
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
                                                <<"cache_size">> => ?CACHE_SIZE, %% default 200
                                                <<"tx_failures">> =>
                                                    #{<<"common">> =>
                                                        #{<<"fallback">> => ?COMMON_DEFAULT,
                                                          <<"tx_nonce_too_high_for_account">> => ?NONCE_TOO_HIGH
                                                         },
                                                      <<"spend_tx">> =>
                                                        #{<<"fallback">> => ?SPEND_DEFAULT,
                                                          <<"insufficient_funds">> => ?INSUFFICIENT_FUNDS
                                                         }
                                                     }
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
init_per_group(failed_attempts, Config0) ->
    Config = [{push_event, tx_received} | Config0],
    start_node(Config),
    seed_account(pubkey(?BOB), Config),
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
        {ok, Txs} ->
            TxHashes = [aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))
                        || STx <- Txs],
            try
                aecore_suite_utils:mine_blocks_until_txs_on_chain(NodeName,
                                                                  TxHashes,
                                                                  ?GC_TTL)
            catch error:max_reached ->
                ok
            end,
            {ok, _} = aecore_suite_utils:mine_blocks(NodeName, ?GC_TTL, ?MINE_RATE, key, #{}),
            {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
            ok
    end,
    Alice = pubkey(?ALICE),
    SpendTx = prepare_spend_tx(Node, 
                               #{recipient_id => aeser_id:create(account, Alice),
                                 amount => ?SPEND_FEE * 100}),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SpendTx, tx_created]),
    mine_tx(Node, SpendTx),
    ok.

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

transaction_over_the_account_nonce_limit_fails(_Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {_, Pub} = aecore_suite_utils:sign_keys(Node),
    %% ensure the transactions are in pool:
    {ok, NextNonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    {value, Acc} = rpc:call(NodeName, aec_chain, get_account, [Pub]),
    ct:log("Account: ~p", [Acc]),
    CurrentNonce = aec_accounts:nonce(Acc),
    ct:log("Account nonce: ~p", [CurrentNonce]),
    {CurrentNonce, NextNonce} = {CurrentNonce, CurrentNonce + 1 + ?ACCOUNT_NONCE_LIMIT},
    ok.

push_tx_skipped_nonce(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {_, Pub} = aecore_suite_utils:sign_keys(Node),
    %% precondition
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    {ok, NextNonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    ct:log("NextNonce: ~p", [NextNonce]),
    SpendTx = prepare_spend_tx(Node, #{nonce => NextNonce + 1, payload => <<"skiped nonce">>}),
    ct:log("Spend tx: ~p", [SpendTx]),
    ok = push(NodeName, SpendTx, Config),
    {ok, [_SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]).

repush_tx_skipped_nonce_is_stopped_by_cache(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    %% test requirement: empty pool
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    {_, Pub} = aecore_suite_utils:sign_keys(Node),
    {ok, NextNonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    ct:log("NextNonce: ~p", [NextNonce]),
    SpendTx = prepare_spend_tx(Node, #{nonce => NextNonce + 1, payload => <<"skiped nonce">>}),
    ct:log("Spend tx: ~p", [SpendTx]),
    ok = push(NodeName, SpendTx, Config),
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ok.

maybe_push_tx_out_cache(Config) ->
    case ?config(push_event, Config) of
        tx_created -> ok;
        tx_received ->
            Node = dev1,
            NodeName = aecore_suite_utils:node_name(Node),
            Pub = pubkey(?ALICE),
            Priv = privkey(?ALICE),
            Opts = 
                #{sender_id    => aeser_id:create(account, Pub),
                  recipient_id => aeser_id:create(account, Pub)},
            lists:foreach(
                fun(_) ->
                    SpendTx = prepare_spend_tx(Node, Opts, Pub, Priv),
                    ct:log("Spend tx: ~p", [SpendTx]),
                    ok = push(NodeName, SpendTx, Config)
                end,
                lists:seq(1, ?CACHE_SIZE)),
            ok
    end.

mine_key_blocks_to_gc_txs(_Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {ok, PoolTxs1} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    case PoolTxs1 =:= [] of
        false ->
            {ok, _} = aecore_suite_utils:mine_blocks(NodeName, ?GC_TTL, ?MINE_RATE, key, #{}),
            timer:sleep(100), %% give time for a slower environment to GC the txs
            {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]);
        true -> pass
    end,
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
    %% assert nonce assumptions and pool transactions
    NextNonce = tx_nonce(Spend1),
    SkippedNonce = tx_nonce(Spend2),
    {ok, PoolTxs} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    SortedTxs = lists:sort([Spend1, Spend2]),
    SortedPoolTxs = lists:sort(PoolTxs),
    {SortedTxs, SortedTxs} = {SortedTxs, SortedPoolTxs},
    ok = push(NodeName, InvalidSpendTx, Config),
    ok.

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
              payload      => random_hash()},
            Opts),
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    aec_test_utils:sign_tx(Tx, Priv, false).

push(NodeName, SignedTx, Config) ->
    EventType = ?config(push_event, Config),
    rpc:call(NodeName, aec_tx_pool, push, [SignedTx, EventType]).

tx_nonce(SignedTx) ->
    _Nonce = aetx:nonce(aetx_sign:tx(SignedTx)).

pubkey({Pubkey, _}) -> Pubkey.

privkey({_, Privkey}) -> Privkey.

mine_tx(Node, SignedTx) ->
    NodeName = aecore_suite_utils:node_name(Node),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(NodeName,
                                                      [TxHash],
                                                      10). %% max keyblocks

skipped_nonce_specific_cleanup(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {ok, [SkippedNonceTx]} = push_tx_skipped_nonce(Config),
    CleanupTTL = ?NONCE_TOO_HIGH,
    %% assert the assumption
    {ok, CleanupTTL} = rpc:call(NodeName, aec_tx_pool_failures, limit, [SkippedNonceTx, tx_nonce_too_high_for_account]),
    make_microblock_attempts(1, Config),
    {ok, _} = aecore_suite_utils:mine_blocks(NodeName, 1, ?MINE_RATE, key, #{}),
    %% the tx is still here
    {ok, [SkippedNonceTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    %% mine some more blocks to check the tx is not cleaned up too early
    make_microblock_attempts(CleanupTTL - 2, Config),
    {ok, [SkippedNonceTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    %% it should be cleaned up at the next height
    make_microblock_attempts(1, Config),
    %%{ok, _} = aecore_suite_utils:mine_blocks(NodeName, 1, ?MINE_RATE, key, #{}),
    timer:sleep(100), %% provide some time for the tx pool to process the message
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ok.

insufficient_funds_specific_cleanup(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    %% ensure an account for Carol
    seed_account(pubkey(?CAROL), 1, Config),
    SpendTx = prepare_spend_tx(Node, #{amount => 100000000}, pubkey(?CAROL), privkey(?CAROL)),
    ok = push(NodeName, SpendTx, Config),
    CleanupTTL = ?INSUFFICIENT_FUNDS,
    %% assert the assumption
    {ok, CleanupTTL} = rpc:call(NodeName, aec_tx_pool_failures, limit, [SpendTx, insufficient_funds]),
    make_microblock_attempts(1, Config),
    %% the tx is still here
    {ok, [SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    %% mine some more blocks to check the tx is not cleaned up too early
    make_microblock_attempts(CleanupTTL - 2, Config),
    %% it should be cleaned up at the next height
    make_microblock_attempts(1, Config),
    timer:sleep(100), %% provide some time for the tx pool to process the message
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ok.

%% this tests the fallback to the defaults in the schema
name_claim_to_unknown_commitement_cleanup(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    {Priv, Pub} = aecore_suite_utils:sign_keys(Node),
    {ok, NextNonce} = rpc:call(NodeName, aec_next_nonce, pick_for_account, [Pub]),
    {ok, Tx} =
        aens_claim_tx:new(#{account_id => aeser_id:create(account, Pub),
                            nonce => NextNonce,
                            name => <<"asdf.chain">>,
                            name_salt => 123,
                            fee => ?SPEND_FEE * 10}),
    SignedTx = aec_test_utils:sign_tx(Tx, Priv, false),
    ok = push(NodeName, SignedTx, Config),
    {ok, [SignedTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    {ok, 1} = rpc:call(NodeName, aec_tx_pool_failures, limit, [SignedTx, bad_transaction]),
    make_microblock_attempts(1, Config),
    timer:sleep(100), %% provide some time for the tx pool to process the message
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ok.

seed_account(Pubkey, Config) ->
    seed_account(Pubkey, ?SPEND_FEE * 10000, Config).

seed_account(Pubkey, Amount, Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    %% precondition
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ct:log("Seed spend tx", []),
    SpendTx = prepare_spend_tx(Node, #{recipient_id => aeser_id:create(account, Pubkey),
                                       amount => Amount}),
    ok = push(NodeName, SpendTx, Config),
    {ok, [_SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    mine_tx(Node, SpendTx),
    ok.

make_microblock_attempts(Cnt, _Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    lists:foreach(
        fun(_) -> attempt_microblock(NodeName) end,
        lists:seq(1, Cnt)),
    ok.

attempt_microblock(NodeName) ->
    TopHash = rpc:call(NodeName, aec_chain, top_block_hash, []),
    {ok, MicroBlock, _} = rpc:call(NodeName, aec_block_micro_candidate,
                                   create, [TopHash]),
    ok.

test_defaults(Config) ->
    Settings =
        #{<<"enabled">> => true,
          <<"common">> => #{<<"fallback">> => 5}},
    %% there is a generic catch-all fallback for all txs, even if not set
    test_(Settings, 5, Config),
    %% the tx specific fallback wins
    test_(Settings#{<<"spend_tx">> => #{<<"fallback">> => 4}}, 4, Config),
    %% the tx specific error wins over the fallback
    test_(Settings#{<<"spend_tx">> => #{<<"fallback">> => 4, <<"insufficient_funds">> => 3}}, 3, Config),
    %% the generic error wins over the generic fallback
    test_(Settings#{<<"common">> => #{<<"fallback">> => 5, <<"insufficient_funds">> => 3}}, 3, Config),
    %% the generic error messages wins over the tx specific fallback
    test_(Settings#{<<"common">> => #{<<"fallback">> => 5, <<"insufficient_funds">> => 3},
                    <<"spend_tx">> => #{<<"fallback">> => 4}}, 3, Config),
    %% tx specific error still wins over the generic one
    test_(Settings#{<<"common">> => #{<<"fallback">> => 5, <<"insufficient_funds">> => 7},
                    <<"spend_tx">> => #{<<"fallback">> => 4, <<"insufficient_funds">> => 3}}, 3, Config),
    ok.

test_(Settings, ExpectedCleanupTTL, Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    %% cache the defaults so we can set them at the end and we don't corrupt
    %% the state of the node
    DefaultSettings = rpc:call(NodeName, aec_tx_pool_failures, settings, []),
    true = rpc:call(NodeName, aec_tx_pool_failures, set, [Settings]),
    %% ensure new settings are into effect:
    Settings = rpc:call(NodeName, aec_tx_pool_failures, settings, []),
    %% ensure an account for Carol
    seed_account(pubkey(?CAROL), 1, Config),
    SpendTx = prepare_spend_tx(Node, #{amount => 100000000}, pubkey(?CAROL), privkey(?CAROL)),
    ok = push(NodeName, SpendTx, Config),
    {ok, CleanupTTL} = rpc:call(NodeName, aec_tx_pool_failures, limit, [SpendTx, insufficient_funds]),
    %% assert the assumption
    {CleanupTTL, CleanupTTL} = {ExpectedCleanupTTL, CleanupTTL},
    make_microblock_attempts(1, Config),
    %% the tx is still here
    {ok, [SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    %% do some more attempts to check the tx is not cleaned up too early
    make_microblock_attempts(CleanupTTL - 2, Config),
    %% it should be cleaned up at the next attempt
    make_microblock_attempts(1, Config),
    timer:sleep(100), %% provide some time for the tx pool to process the message
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    rpc:call(NodeName, aec_tx_pool_failures, set, [DefaultSettings]),
    ok.

test_disabled(Config) ->
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    %% cache the defaults so we can set them at the end and we don't corrupt
    %% the state of the node
    DefaultSettings = rpc:call(NodeName, aec_tx_pool_failures, settings, []),
    Settings = #{<<"enabled">> => false},
    true = rpc:call(NodeName, aec_tx_pool_failures, set, [Settings]),
    %% ensure new settings are into effect:
    Settings = rpc:call(NodeName, aec_tx_pool_failures, settings, []),
    %% ensure an account for Carol
    seed_account(pubkey(?CAROL), 1, Config),
    SpendTx = prepare_spend_tx(Node, #{amount => 100000000}, pubkey(?CAROL), privkey(?CAROL)),
    ok = push(NodeName, SpendTx, Config),
    no_limit = rpc:call(NodeName, aec_tx_pool_failures, limit, [SpendTx, insufficient_funds]),
    %% assert that no limit is being applied
    lists:foreach(
        fun(_) ->
            make_microblock_attempts(1, Config),
            %% the tx is still here
            {ok, [SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity])
        end,
        lists:seq(1, 2 * ?GC_TTL)),
    {ok, _} = aecore_suite_utils:mine_blocks(NodeName, ?GC_TTL - 1, ?MINE_RATE, key, #{}),
    {ok, [SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    %% it should be cleaned up at the next height
    {ok, _} = aecore_suite_utils:mine_blocks(NodeName, 1, ?MINE_RATE, key, #{}),
    timer:sleep(100), %% provide some time for the tx pool to process the message
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    rpc:call(NodeName, aec_tx_pool_failures, set, [DefaultSettings]),
    ok.


random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).

