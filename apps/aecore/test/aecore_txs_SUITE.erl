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
-include_lib("apps/aecontract/src/aecontract.hrl").

-import(aecore_suite_utils, [patron/0]).

all() ->
    [ micro_block_cycle
    , missing_tx_gossip
    , txs_gc
    , check_coinbase_validation
    ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    MicroBlockCycle = 100,
    Config1 = [{symlink_name, "latest.txs"},
               {top_dir, TopDir},
               {test_module, ?MODULE},
               {micro_block_cycle, MicroBlockCycle}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    DefCfg = #{
        <<"chain">> => #{
            <<"persist">> => false
        },
        <<"mining">> => #{
            <<"micro_block_cycle">> => MicroBlockCycle
        }
    },
    aecore_suite_utils:create_configs(Config1, DefCfg),
    aecore_suite_utils:make_multi(Config1),
    [{nodes, [aecore_suite_utils:node_tuple(dev1),
              aecore_suite_utils:node_tuple(dev2)]} | Config1].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    aecore_suite_utils:stop_node(dev1, Config),
    aecore_suite_utils:stop_node(dev2, Config),
    ok.

%% ============================================================
%% Test cases
%% ============================================================
txs_gc(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    ok = aecore_suite_utils:set_env(N1, aecore, mempool_nonce_offset, 100),

    Height0 = 0,

    %% Add a bunch of transactions...
    {ok, TxH1} = add_spend_tx(N1, 1000, 20000,  1,  10), %% Ok
    {ok, _GC1} = add_spend_tx(N1, 1000, 20000,  2,  10), %% Should expire ?EXPIRE_TX_TTL after
                                                         %% TxH2 is on chain = ~1 + 2 = ~3
    {ok, TxH2} = add_spend_tx(N1, 1000, 20001,  2,  10), %% Duplicate should be preferred
    {ok, TxH3} = add_spend_tx(N1, 1000, 20000,  3,  10), %% Ok

    {ok, TxH5} = add_spend_tx(N1, 1000, 20000,  5,  10), %% Non consecutive nonce
    {ok, _}    = add_spend_tx(N1, 1000, 20000,  7,  10), %% Non consecutive nonce
    {ok, _}    = add_spend_tx(N1, 1000, 20000,  8,  7),  %% Short TTL - expires at 7

    %% Now there should be 7 transactions in mempool
    pool_check(N1, 7),

    %% Mine to get TxH1-3 onto chain
    {ok, Blocks1} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [TxH1, TxH2, TxH3], 10),
    Height1 = Height0 + length(Blocks1), %% Very unlikely to be > 6...

    %% At Height1 there should be 4 transactions in mempool
    case Height1 of
        2                -> pool_check(N1, 4);
        HH1 when HH1 > 2 -> ct:log("Skipping assert; micro fork advanced height to far...")
    end,

    %% Mine 1 more key block to ensure one TX is GC:ed.
    aecore_suite_utils:mine_key_blocks(N1, 1),
    Height2 = Height1 + 1,

    %% Now at Height2 there should be 3 transactions in mempool - _GC1 is GC:ed
    pool_check(N1, 3),

    %% Add the missing tx
    {ok, TxH4} = add_spend_tx(N1, 1000, 20000,  4,  10), %% consecutive nonce

    %% Mine to get TxH4-5 onto chain
    {ok, Blocks2} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [TxH4, TxH5], 10),
    Height3 = Height2 + length(Blocks2),

    %% Now if at height 5 or 6 there should be 2 transactions in mempool
    case Height3 of
        HH3 when HH3 =<6 -> pool_check(N1, 2);
        HH3 when HH3 > 6 -> ct:log("Skipping assert; micro fork advanced height to far...")
    end,

    %% Mine 1 more key block if Height is 5 ensure one TX should be GC:ed.
    [ aecore_suite_utils:mine_key_blocks(N1, 7 - Height3) || Height3 < 7 ],

    Height4 = max(Height3, 7),

    %% Now there should be 1 transaction in mempool
    case Height4 of
        7                -> pool_check(N1, 1);
        HH4 when HH4 > 7 -> ct:log("Skipping assert; micro fork advanced height to far...")
    end,

    %% Mine 2 more blocks then all TXs should be GC:ed. Height>=10
    aecore_suite_utils:mine_key_blocks(N1, 3),

    %% Now there should be no transactions in mempool
    pool_check(N1, 0),

    ok = aecore_suite_utils:check_for_logs([dev1], Config).

pool_check(Node, ExpectedNTxs) ->
    pool_check(Node, ExpectedNTxs, 5).

pool_check(Node, ExpectedNTxs, Retries) ->
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
            pool_check(Node, ExpectedNTxs, Retries - 1)
    end.

pool_peek(Node) ->
    rpc:call(Node, sys, get_status, [aec_tx_pool_gc]),
    rpc:call(Node, aec_tx_pool, peek, [infinity]).

missing_tx_gossip(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:start_node(dev2, Config),

    N1 = aecore_suite_utils:node_name(dev1),
    N2 = aecore_suite_utils:node_name(dev2),

    aecore_suite_utils:connect(N1),
    aecore_suite_utils:connect(N2),

    %% Both nodes are up, now turn off gossiping of TXs
    %% Also (virtually) disable ping
    rpc:call(N1, aec_sync, gossip_txs, [false], 5000),
    rpc:call(N2, aec_sync, gossip_txs, [false], 5000),
    rpc:call(N1, application, set_env, [aecore, ping_interval, 1000000], 5000),
    rpc:call(N2, application, set_env, [aecore, ping_interval, 1000000], 5000),

    %% Ping interval was 500 ms, wait that long
    timer:sleep(2 * 500),

    {ok, TxH1} = add_spend_tx(N1, 1000, 20000,  1,  100), %% Ok
    {ok, TxH2} = add_spend_tx(N1, 1000, 20000,  2,  100), %% Ok
    {ok, TxH3} = add_spend_tx(N1, 1000, 20000,  3,  100), %% Ok
    {ok, TxH4} = add_spend_tx(N1, 1000, 20000,  4,  100), %% Ok
    {ok, TxH5} = add_spend_tx(N2, 1000, 20000,  5,  100), %% Ok

    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [TxH1, TxH2, TxH3, TxH4], 5),
    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N2, [TxH5], 5),

    ok.

check_coinbase_validation(Config) ->
    %% Mine on a node a contract tx using coinbase.
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    {ok, TxH1, Ct1, Code} =
        create_contract_tx(N1, chain, <<"()">>,  300000,  1,  100),
    {ok, TxH2} =
        call_contract_tx(N1, Ct1, Code, <<"save_coinbase">>, <<"()">>, 600000,  2,  100),
    {ok, _} =
        aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [TxH1, TxH2], 10),

    %% Start a second node with distinct beneficiary.
    aecore_suite_utils:start_node(dev2, Config),
    N2 = aecore_suite_utils:node_name(dev2),
    aecore_suite_utils:connect(N2),
    {ok, Ben1} = rpc:call(N1, aec_conductor, get_beneficiary, []),
    {ok, Ben2} = rpc:call(N2, aec_conductor, get_beneficiary, []),
    ?assertNotEqual(Ben1, Ben2), %% Sanity check on the test nodes.

    %% Check that the second node syncs the mined tx with the initial node.
    {ok, Tx1Hash} = aehttp_api_encoder:safe_decode(tx_hash, TxH1),
    {ok, Tx2Hash} = aehttp_api_encoder:safe_decode(tx_hash, TxH2),
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

micro_block_cycle(Config) ->
    MBC = ?config(micro_block_cycle, Config),
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),

    %% Mine a block to get some funds. Height=1
    aecore_suite_utils:mine_key_blocks(N1, 1),

    StartRes = rpc:call(N1, aec_conductor, start_mining, [], 5000),
    ct:log("aec_conductor:start_mining() (~p) -> ~p", [N1, StartRes]),

    timer:sleep(1000), %% Make sure we're leader

    [ begin
        add_spend_tx(N1, 1000, 20000,  Nonce, 10000),
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
    SenderId = aec_id:create(account, maps:get(pubkey, Sender)),
    RecipientId = aec_id:create(account, Recipient),
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
    {Res, aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.


create_contract_tx(Node, Name, Args, Fee, Nonce, TTL) ->
    OwnerKey = maps:get(pubkey, patron()),
    Owner    = aec_id:create(account, OwnerKey),
    Code     = compile_contract(lists:concat(["contracts/", Name, ".aes"])),
    {ok, CallData} = aect_sophia:encode_call_data(Code, <<"init">>, Args),
    {ok, CreateTx} = aect_create_tx:new(#{ nonce      => Nonce
                                         , vm_version => ?CURRENT_AEVM_SOPHIA
                                         , code       => Code
                                         , call_data  => CallData
                                         , fee        => Fee
                                         , deposit    => 0
                                         , amount     => 0
                                         , gas        => 100000
                                         , owner_id   => Owner
                                         , gas_price  => 1
                                         , ttl        => TTL
                                         }),
    CTx = aec_test_utils:sign_tx(CreateTx, maps:get(privkey, patron())),
    Res = rpc:call(Node, aec_tx_pool, push, [CTx]),
    ContractKey = aect_contracts:compute_contract_pubkey(OwnerKey, Nonce),
    {Res, aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(CTx)), ContractKey, Code}.

compile_contract(File) ->
    CodeDir = filename:join(code:lib_dir(aecore), "../../extras/test/"),
    FileName = filename:join(CodeDir, File),
    {ok, ContractBin} = file:read_file(FileName),
    {ok, Serialized} = aect_sophia:compile(ContractBin, <<"pp_icode">>),
    Serialized.

call_contract_tx(Node, Contract, Code, Function, Args, Fee, Nonce, TTL) ->
    Caller       = aec_id:create(account, maps:get(pubkey, patron())),
    ContractID   = aec_id:create(contract, Contract),
    {ok, CallData} = aect_sophia:encode_call_data(Code, Function, Args),
    {ok, CallTx} = aect_call_tx:new(#{ nonce       => Nonce
                                     , caller_id   => Caller
                                     , vm_version  => ?CURRENT_AEVM_SOPHIA
                                     , contract_id => ContractID
                                     , fee         => Fee
                                     , amount      => 0
                                     , gas         => 100000
                                     , gas_price   => 1
                                     , call_data   => CallData
                                     , ttl         => TTL
                                     }),
    CTx = aec_test_utils:sign_tx(CallTx, maps:get(privkey, patron())),
    Res = rpc:call(Node, aec_tx_pool, push, [CTx]),
    {Res, aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(CTx))}.


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

