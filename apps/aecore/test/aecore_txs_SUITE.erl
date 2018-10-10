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

all() ->
    [ micro_block_cycle
    , missing_tx_gossip
    , txs_gc
    , check_coinbase_validation
    ].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
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

    %% Mine a block to get some funds. Height=1
    aecore_suite_utils:mine_key_blocks(N1, 1),
    Height0 = 1,

    %% Add a bunch of transactions...
    {ok, TxH1} = add_spend_tx(N1, 1000, 1,  1,  10), %% Ok
    {ok, _}    = add_spend_tx(N1, 1000, 1,  2,  10), %% Should expire ?EXPIRE_TX_TTL after
                                                     %% next TX is on chain = 2 + 2 = 4
    {ok, TxH2} = add_spend_tx(N1, 1000, 10, 2,  10), %% Duplicate should be preferred
    {ok, TxH3} = add_spend_tx(N1, 1000, 1,  3,  10), %% Ok

    {ok, TxH5} = add_spend_tx(N1, 1000, 1,  5,  10), %% Non consecutive nonce
    {ok, _}    = add_spend_tx(N1, 1000, 1,  7,  10), %% Non consecutive nonce
    {ok, _}    = add_spend_tx(N1, 1000, 1,  8,  5),  %% Short TTL - expires at 5 + 2 = 7

    %% Now there should be 7 transactions in mempool
    {ok, Txs1} = pool_peek(N1),
    {7, _} = {length(Txs1), Txs1},

    %% Mine to get TxH1-3 onto chain
    {ok, Blocks1} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [TxH1, TxH2, TxH3], 10),
    Height1 = Height0 + length(Blocks1), %% Very unlikely to be > 4...

    %% Now there should be 3 or 4 transactions in mempool
    {ok, Txs2} = pool_peek(N1),
    case Height1 of
        3                -> {4, _} = {length(Txs2), Txs2};
        HH1 when HH1 > 3 -> ct:log("Skipping assert; micro fork advanced height to far...")
    end,

    %% Mine 1 more key block if Height is 3 ensure one TX should be GC:ed.
    [ aecore_suite_utils:mine_key_blocks(N1, 1) || Height1 == 3 ],

    Height2 = max(Height1, 4),

    %% Now unless Height2 > 4, there should be 3 transactions in mempool
    {ok, Txs3} = pool_peek(N1),
    case Height2 of
        4                -> {3, _} = {length(Txs3), Txs3};
        HH2 when HH2 > 4 -> ct:log("Skipping assert; micro fork advanced height to far...")
    end,

    %% Add the missing tx
    {ok, TxH4} = add_spend_tx(N1, 1000, 1,  4,  10), %% consecutive nonce

    %% Mine to get TxH4-5 onto chain
    {ok, Blocks2} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [TxH4, TxH5], 10),
    Height3 = Height2 + length(Blocks2),

    %% Now at height 6 there should be 2 transactions in mempool
    {ok, Txs4} = pool_peek(N1),
    case Height3 of
        6                -> {2, _} = {length(Txs4), Txs4};
        HH3 when HH3 > 6 -> ct:log("Skipping assert; micro fork advanced height to far...")
    end,

    %% Mine 1 more key block if Height is 6 ensure one TX should be GC:ed.
    [ aecore_suite_utils:mine_key_blocks(N1, 1) || Height3 == 6 ],

    Height4 = max(Height3, 7),

    %% Now there should be 1 transaction in mempool
    {ok, Txs5} = pool_peek(N1),
    case Height4 of
        7                -> {1, _} = {length(Txs5), Txs5};
        HH4 when HH4 > 7 -> ct:log("Skipping assert; micro fork advanced height to far...")
    end,

    %% Mine 2 more blocks then all TXs should be GC:ed. Height>=9
    aecore_suite_utils:mine_key_blocks(N1, 2),

    %% Now there should be no transactions in mempool
    {ok, Txs6} = pool_peek(N1),
    {0, _} = {length(Txs6), Txs6},

    ok = aecore_suite_utils:check_for_logs([dev1], Config).

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

    {ok, TxH1} = add_spend_tx(N1, 1000, 1,  1,  100), %% Ok
    {ok, TxH2} = add_spend_tx(N1, 1000, 1,  2,  100), %% Ok
    {ok, TxH3} = add_spend_tx(N1, 1000, 1,  3,  100), %% Ok
    {ok, TxH4} = add_spend_tx(N1, 1000, 1,  4,  100), %% Ok
    {ok, TxH5} = add_spend_tx(N2, 1000, 1,  5,  100), %% Ok

    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [TxH1, TxH2, TxH3, TxH4], 5),
    {ok, _} = aecore_suite_utils:mine_blocks_until_tx_on_chain(N2, TxH5, 5),

    ok.

check_coinbase_validation(Config) ->
    %% Mine on a node a contract tx using coinbase.
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    {ok, TxH1, Ct1} =
        create_contract_tx(N1, chain, <<"()">>,  1,  1,  100),
    {ok, TxH2} =
        call_contract_tx(N1, Ct1, "save_coinbase", "()", 1,  2,  100),
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
    {ok, Tx1Hash} = aec_base58c:safe_decode(tx_hash, TxH1),
    {ok, Tx2Hash} = aec_base58c:safe_decode(tx_hash, TxH2),
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
        add_spend_tx(N1, 1000, 1,  Nonce, 10000),
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
    {Res, aec_base58c:encode(tx_hash, aetx_sign:hash(STx))}.


create_contract_tx(Node, Name, Args, Fee, Nonce, TTL) ->
    OwnerKey = maps:get(pubkey, patron()),
    Owner    = aec_id:create(account, OwnerKey),
    Code     = compile_contract(lists:concat(["contracts/", Name, ".aes"])),
    CallData = aect_sophia:create_call(Code, <<"init">>, Args),
    {ok, CreateTx} = aect_create_tx:new(#{ nonce      => Nonce
                                         , vm_version => 1
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
    {Res, aec_base58c:encode(tx_hash, aetx_sign:hash(CTx)), ContractKey}.

compile_contract(File) ->
    CodeDir = code:lib_dir(aesophia, test),
    FileName = filename:join(CodeDir, File),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    aeso_compiler:from_string(Contract, [pp_icode]).

call_contract_tx(Node, Contract, Function, Args, Fee, Nonce, TTL) ->
    Caller       = aec_id:create(account, maps:get(pubkey, patron())),
    ContractID   = aec_id:create(contract, Contract),
    CallData     = aeso_abi:create_calldata(<<>>, Function, Args),
    {ok, CallTx} = aect_call_tx:new(#{ nonce       => Nonce
                                     , caller_id   => Caller
                                     , vm_version  => 1
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
    {Res, aec_base58c:encode(tx_hash, aetx_sign:hash(CTx))}.


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

patron() ->
    #{ pubkey  => <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,
                    73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
       privkey => <<230,169,29,99,60,119,207,87,113,50,157,51,84,179,188,239,27,
                    197,224,50,196,61,112,182,211,90,249,35,206,30,183,77,206,
                    167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,
                    187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>
      }.
