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
   [ micro_block_cycle/1
   , missing_tx_gossip/1
   , txs_gc/1
   , check_coinbase_validation/1
   ]).


-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../../aecontract/include/aecontract.hrl").

-import(aecore_suite_utils, [patron/0]).

-define(MAX_MINED_BLOCKS, 20).

all() ->
    [
    %% Bitcoin NG
      missing_tx_gossip
    , micro_block_cycle
    %% CT Consensus
    , txs_gc
    , check_coinbase_validation
    ].

init_per_suite(Config) ->
    MicroBlockCycle = 100,
    DefCfg = #{
        <<"chain">> => #{
            <<"persist">> => false
        },
        <<"mining">> => #{
            <<"micro_block_cycle">> => MicroBlockCycle
        },
        <<"mempool">> => #{ <<"invalid_tx_ttl">> => 2
                          , <<"nonce_baseline">> => 10 }
    },
    Config1 = aecore_suite_utils:init_per_suite([dev1, dev2], DefCfg,
                                                [{add_peers, true}],
                                                [{symlink_name, "latest.txs"},
                                                 {instant_mining, true},
                                                 {test_module, ?MODULE},
                                                 {micro_block_cycle,
                                                  MicroBlockCycle}] ++
                                                Config),
    pforeach(fun(N) ->
        aecore_suite_utils:start_node(N, Config1),
        Node = aecore_suite_utils:node_name(N),
        aecore_suite_utils:connect(Node) end, [dev1, dev2]),
    [{nodes, [aecore_suite_utils:node_tuple(dev1),
              aecore_suite_utils:node_tuple(dev2)]} | Config1].

end_per_suite(Config) ->
    pforeach(fun(Node) -> aecore_suite_utils:stop_node(Node, Config) end, [dev1, dev2]),
    ok.

pforeach(F, List) ->
    MRefs = lists:map(
        fun(E) ->
            {_, MRef} = spawn_monitor(fun() ->
                F(E)
            end),
            MRef
        end,
    List),
    [receive {'DOWN', MRef, process, _, _} -> ok end || MRef <- MRefs].

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
    %% WARNING: GC is triggered only when we receive a keyblock and we are NOT the leader
    %% The old test suite was implicitly calling aec_tx_pool:garbage_collect() when mining stopped.
    %% To simulate receiving blocks from external entities - call aec_tx_pool:garbage_collect() explicitly :)
    aecore_suite_utils:reinit_with_ct_consensus(dev1),
    aecore_suite_utils:reinit_with_ct_consensus(dev2),
    N1 = aecore_suite_utils:node_name(dev1),
    ok = aecore_suite_utils:mock_mempool_nonce_offset(N1, 100),

    %% WARNING: There is another nasty hidden assumption here
    %%          the generation where TxH2 was mined needs to contain at least 2
    %%          microblocks in order for GC1 to be marked for GC at 3
    %% Add a bunch of transactions...
    {ok, _,_TxH1} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  1,  10), %% Ok
    {ok, _, _GC1} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  2,  10), %% Should expire ?EXPIRE_TX_TTL after
                                                         %% TxH2 is on chain = ~1 + 2 = ~3
    {ok, _,_TxH2} = add_spend_tx(N1, 1000, 20001 * aec_test_utils:min_gas_price(),  2,  10), %% Duplicate should be preferred
    {ok, _,_TxH3} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  3,  10), %% Ok

    {ok, _, TxH5} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  5,  10), %% Non consecutive nonce
    {ok, _, _}    = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  7,  10), %% Non consecutive nonce
    {ok, _, _}    = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  8,  7),  %% Short TTL - expires at 7

    %% Now there should be 7 transactions in mempool
    pool_check(N1, 7),

    %% Mine to get TxH1-3 onto chain
    aecore_suite_utils:mine_key_blocks(N1, 1), %% Make us the leader
    aecore_suite_utils:mine_micro_blocks(N1, 2),
    aecore_suite_utils:mine_key_blocks(N1, 1),
    aecore_suite_utils:mine_micro_blocks(N1, 2),
    rpc:call(N1, aec_tx_pool, garbage_collect, []),

    %% At height 2 there should be 4 transactions in mempool
    pool_check(N1, 4),

    %% Mine 1 more key block to ensure one TX is GC:ed.
    aecore_suite_utils:mine_key_blocks(N1, 1),
    rpc:call(N1, aec_tx_pool, garbage_collect, []),
    Height2 = 3,

    %% Now at height 3 there should be 3 transactions in mempool - _GC1 is GC:ed
    pool_check(N1, 3),

    %% Add the missing tx
    {ok, _, TxH4} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  4,  10), %% consecutive nonce

    %% Mine to get TxH4-5 onto chain
    {ok, Blocks2} = mine_blocks_until_txs_on_chain(N1, [TxH4, TxH5]),
    rpc:call(N1, aec_tx_pool, garbage_collect, []),
    Height3 = Height2 + length(Blocks2),

    %% Now if at height 5 or 6 there should be 2 transactions in mempool
    pool_check(N1, 2, {Height3, 6}),

    %% Mine 1 more key block if Height is 5 ensure one TX should be GC:ed.
    [ begin
          aecore_suite_utils:mine_key_blocks(N1, 7 - Height3), rpc:call(N1, aec_tx_pool, garbage_collect, [])
      end || Height3 < 7 ],

    Height4 = max(Height3, 7),

    %% Now there should be 1 transaction in mempool
    pool_check(N1, 1, {Height4, 7}),

    %% Mine 2 more blocks then all TXs should be GC:ed. Height>=10
    aecore_suite_utils:mine_key_blocks(N1, 3),
    rpc:call(N1, aec_tx_pool, garbage_collect, []),

    %% Now there should be no transactions in mempool
    pool_check(N1, 0),

    ok = aecore_suite_utils:check_for_logs([dev1], Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(N1).

pool_check(Node, ExpectedNTxs) ->
    pool_check_(Node, ExpectedNTxs, 5).

pool_check(_Node, _ExpectedNTxs, {Height, MaxHeight}) when Height > MaxHeight ->
    ct:log("Skipping assert; micro fork advanced height to far...");
pool_check(Node, ExpectedNTxs, _) ->
    pool_check_(Node, ExpectedNTxs, 5).

pool_check_(Node, ExpectedNTxs, Retries) ->
    {ok, Txs} = pool_peek(Node),
    ct:log("pool_check: pool has ~p TXs, expected ~p\n", [length(Txs), ExpectedNTxs]),
    case ExpectedNTxs == length(Txs) of
        true ->
            ok;
        false when Retries == 0 ->
            ct:log("TXS: ~p", [Txs]),
            GC = rpc:call(Node, ets, tab2list, [mempool_gc]),
            ct:log("GCTAB: ~p", [GC]),
            ct:fail({pool_check_failed, {expected, ExpectedNTxs, got, length(Txs)}});
        _ ->
            timer:sleep(500),
            pool_check_(Node, ExpectedNTxs, Retries - 1)
    end.

pool_peek(Node) ->
    rpc:call(Node, sys, get_status, [aec_tx_pool_gc]),
    rpc:call(Node, aec_tx_pool, peek, [infinity]).

missing_tx_gossip(_Config) ->
    N1 = aecore_suite_utils:node_name(dev1),
    N2 = aecore_suite_utils:node_name(dev2),

    aecore_suite_utils:reinit_with_bitcoin_ng(dev1),
    aecore_suite_utils:reinit_with_bitcoin_ng(dev2),

    %% Both nodes are up, now turn off gossiping of TXs
    %% Also (virtually) disable ping
    rpc:call(N1, aec_sync, gossip_txs, [false], 5000),
    rpc:call(N2, aec_sync, gossip_txs, [false], 5000),
    rpc:call(N1, application, set_env, [aecore, ping_interval, 1000000], 5000),
    rpc:call(N2, application, set_env, [aecore, ping_interval, 1000000], 5000),

    {ok, _, TxH1} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  1,  100), %% Ok
    {ok, _, TxH2} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  2,  100), %% Ok
    {ok, _, TxH3} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  3,  100), %% Ok
    {ok, _, TxH4} = add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(),  4,  100), %% Ok

    {ok, Tx1Hash} = aeser_api_encoder:safe_decode(tx_hash, TxH1),
    {ok, Tx4Hash} = aeser_api_encoder:safe_decode(tx_hash, TxH4),
    ?assertMatch({mempool, _}, rpc:call(N1, aec_chain, find_tx_with_location, [Tx1Hash])), %% Smoke test approach used in the following for checking txs.

    none = rpc:call(N2, aec_chain, find_tx_with_location, [Tx1Hash]),
    none = rpc:call(N2, aec_chain, find_tx_with_location, [Tx4Hash]),

    %% Set the same mining_rate to validate target
    %% Only needed when chain more than 18 blocks
    ok = rpc:call(N2, application, set_env, [aecore, expected_mine_rate, aecore_suite_utils:expected_mine_rate()], 5000),
    {ok, MinedKeyBlocks1} = mine_blocks_until_txs_on_chain(N1, [TxH1, TxH2, TxH3, TxH4]),

    {ok, Tx5, TxH5} = add_spend_tx(N2, 1000, 20000 * aec_test_utils:min_gas_price(),  5,  100), %% Ok
    {ok, Tx5Hash} = aeser_api_encoder:safe_decode(tx_hash, TxH5),
    aecore_suite_utils:wait_for_height(N2, aec_blocks:height(lists:last(MinedKeyBlocks1))),
    ?assertMatch({X,_} when is_binary(X), rpc:call(N2, aec_chain, find_tx_with_location, [Tx1Hash])),
    ?assertMatch({X,_} when is_binary(X), rpc:call(N2, aec_chain, find_tx_with_location, [Tx4Hash])),
    ok = aecore_suite_utils:wait_for_tx_in_pool(N2, Tx5),
    ?assertMatch({mempool, _}, rpc:call(N2, aec_chain, find_tx_with_location, [Tx5Hash])), %% Smoke test approach used in the following for checking txs.

    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N2, [TxH5], ?MAX_MINED_BLOCKS),
    ok.

%% CT Consensus does not change the coinbase
check_coinbase_validation(_Config) ->
    %% Mine on a node a contract tx using coinbase.
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:reinit_with_ct_consensus(dev1),
    aecore_suite_utils:reinit_with_ct_consensus(dev2),
    {ok, TxH1, Ct1, Code} =
        create_contract_tx(N1, chain, [],  300000 * aec_test_utils:min_gas_price(),  1,  100),
    {ok, TxH2} =
        call_contract_tx(N1, Ct1, Code, <<"save_coinbase">>, [], 600000 * aec_test_utils:min_gas_price(),  2,  100),
    {ok, _} = mine_blocks_until_txs_on_chain(N1, [TxH1, TxH2]),

    %% Check if dev2 syncs with dev1 from genesis
    N2 = aecore_suite_utils:node_name(dev2),
    aecore_suite_utils:reinit_with_ct_consensus(dev2),

    %% Sanity check on the test nodes.
    {ok, Ben1} = rpc:call(N1, aec_conductor, get_beneficiary, []),
    {ok, Ben2} = rpc:call(N2, aec_conductor, get_beneficiary, []),
    ?assertNotEqual(Ben1, Ben2),

    %% Check that the second node syncs the mined tx with the initial node.
    {ok, Tx1Hash} = aeser_api_encoder:safe_decode(tx_hash, TxH1),
    {ok, Tx2Hash} = aeser_api_encoder:safe_decode(tx_hash, TxH2),
    wait_till_hash_in_block_on_node(N2, Tx2Hash, 3000),
    {BlockHash1, _} = rpc:call(N1, aec_chain, find_tx_with_location, [Tx1Hash]),
    {BlockHash2, _} = rpc:call(N1, aec_chain, find_tx_with_location, [Tx2Hash]),
    {BlockHash1, _} = rpc:call(N2, aec_chain, find_tx_with_location, [Tx1Hash]),
    {BlockHash2, _} = rpc:call(N2, aec_chain, find_tx_with_location, [Tx2Hash]),
    true = is_binary(BlockHash1),
    true = is_binary(BlockHash2),
    ok.

wait_till_hash_in_block_on_node(_Node,_TxHash, 0) -> exit(tx_not_syncing);
wait_till_hash_in_block_on_node(Node, TxHash, Limit) ->
    try rpc:call(Node, aec_chain, find_tx_with_location, [TxHash]) of
        {BlockHash, _} when is_binary(BlockHash) -> ok;
        _ -> yield(), wait_till_hash_in_block_on_node(Node, TxHash, Limit-1)
    catch
        _:_ -> yield(), wait_till_hash_in_block_on_node(Node, TxHash, Limit-1)
    end.

yield() -> timer:sleep(10).

%% Test specific to BitcoinNG - don't enable CT consensus
micro_block_cycle(Config) ->
    MBC = ?config(micro_block_cycle, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:reinit_with_bitcoin_ng(dev1),
    aecore_suite_utils:reinit_with_bitcoin_ng(dev2),

    %% Mine a block to get some funds. Height=1
    aecore_suite_utils:mine_key_blocks(N1, 1),

    StartRes = rpc:call(N1, aec_conductor, start_mining, [], 5000),
    ct:log("aec_conductor:start_mining() (~p) -> ~p", [N1, StartRes]),

    timer:sleep(1000), %% Make sure we're leader

    [ begin
        add_spend_tx(N1, 1000, 20000 * aec_test_utils:min_gas_price(), Nonce, 10000),
        timer:sleep(MBC div 3)
      end || Nonce <- lists:seq(1,30) ],

    timer:sleep(2*MBC),
    StopRes = rpc:call(N1, aec_conductor, stop_mining, [], 5000),
    ct:log("aec_conductor:stop_mining() (~p) -> ~p", [N1, StopRes]),

    MicroBlocks = aecore_suite_utils:events_since(N1, micro_block_created, ?config(tc_start, Config)),
    %% Below fails until micro_block_cycle is correctly implemented
    ok = timediff(MBC, [ {aec_blocks:time_in_msecs(B), aec_blocks:height(B)} || #{info := B} <- MicroBlocks ]),

    ok = aecore_suite_utils:check_for_logs([dev1], Config).

add_spend_tx(Node, Amount, Fee, Nonce, TTL) ->
    add_spend_tx(Node, Amount, Fee, Nonce, TTL, patron(), new_pubkey()).

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Sender, Recipient) ->
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
    {Res, STx, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.


create_contract_tx(Node, Name, Args, Fee, Nonce, TTL) ->
    OwnerKey = maps:get(pubkey, patron()),
    Owner    = aeser_id:create(account, OwnerKey),
    {ok, Contract} = aect_test_utils:read_contract(Name),
    {ok, Code} = aect_test_utils:compile_contract(Name),
    {ok, CallData} = aect_test_utils:encode_call_data(Contract, <<"init">>, Args),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    VM  = aect_test_utils:latest_sophia_vm_version(),
    {ok, CreateTx} = aect_create_tx:new(#{ nonce       => Nonce
                                         , vm_version  => VM
                                         , abi_version => ABI
                                         , code        => Code
                                         , call_data   => CallData
                                         , fee         => Fee
                                         , deposit     => 0
                                         , amount      => 0
                                         , gas         => 100000
                                         , owner_id    => Owner
                                         , gas_price   => aec_test_utils:min_gas_price()
                                         , ttl         => TTL
                                         }),
    CTx = aec_test_utils:sign_tx(CreateTx, maps:get(privkey, patron())),
    Res = rpc:call(Node, aec_tx_pool, push, [CTx]),
    ContractKey = aect_contracts:compute_contract_pubkey(OwnerKey, Nonce),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(CTx)), ContractKey, Contract}.

call_contract_tx(Node, Contract, Code, Function, Args, Fee, Nonce, TTL) ->
    Caller       = aeser_id:create(account, maps:get(pubkey, patron())),
    ContractID   = aeser_id:create(contract, Contract),
    {ok, CallData} = aect_test_utils:encode_call_data(Code, Function, Args),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    {ok, CallTx} = aect_call_tx:new(#{ nonce       => Nonce
                                     , caller_id   => Caller
                                     , abi_version => ABI
                                     , contract_id => ContractID
                                     , fee         => Fee
                                     , amount      => 0
                                     , gas         => 100000
                                     , gas_price   => aec_test_utils:min_gas_price()
                                     , call_data   => CallData
                                     , ttl         => TTL
                                     }),
    CTx = aec_test_utils:sign_tx(CallTx, maps:get(privkey, patron())),
    Res = rpc:call(Node, aec_tx_pool, push, [CTx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(CTx))}.


%% We assume that equence is time ordered and only check Micro Block Cycle time
%% for micro blocks in same generation
timediff(_Ms, []) ->
    ok;
timediff(_Ms, [_]) ->
    ok;
timediff(Ms, [{T1, H1}, {T2, H1} | Rest]) ->
    %% Nodes should validate that there is MBC time between micro blocks.
    case T1 + Ms =< T2 of
        true -> timediff(Ms, [{T2, H1} | Rest]);
        false -> {error, {{T1, H1}, {T2, H1}}}
    end;
timediff(Ms, [_ | Rest]) ->
    timediff(Ms, Rest).

new_pubkey() ->
    #{ public := PubKey } = enacl:sign_keypair(),
    PubKey.

mine_blocks_until_txs_on_chain(Node, TxHashes) ->
    aecore_suite_utils:mine_blocks_until_txs_on_chain(
      Node,
      TxHashes,
      aecore_suite_utils:expected_mine_rate(),
      ?MAX_MINED_BLOCKS,
      #{strictly_follow_top => true}).
