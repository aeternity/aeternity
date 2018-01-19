-module(aehttp_integration_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% Endpoint calls
-export([]).

%% test case exports
%% external endpoints
-export(
   [
    % pings
    broken_pings/1,
    blocked_ping/1,
    correct_ping/1,

    % get block-s
    get_top_empty_chain/1,
    get_top_non_empty_chain/1,
    block_by_height/1,
    block_not_found_by_height/1,
    block_by_hash/1,
    block_not_found_by_broken_hash/1,
    block_not_found_by_hash/1,

    % sync gossip 
    pending_transactions/1,
    post_correct_blocks/1,
    post_broken_blocks/1,
    post_correct_tx/1,
    post_broken_tx/1,
    post_broken_base58_tx/1,

    % balances
    miner_balance/1,
    balance_broken_address/1,
    all_accounts_balances/1,
    all_accounts_balances_empty/1,
    all_accounts_balances_disabled/1,

    % infos
    version/1,
    info_disabled/1,
    info_empty/1,
    info_more_than_30/1,
    info_less_than_30/1
   ]).
%%
%% test case exports
%% internal endpoints
-export(
   [
    broken_spend_tx/1,
    miner_pub_key/1,

    %% requested Endpoints
    block_number/1,

    internal_block_by_height/1,
    internal_block_by_hash/1,
    internal_block_genesis/1,
    internal_block_pending/1,
    internal_block_latest/1,

    internal_block_not_found_by_height/1,
    internal_block_not_found_by_broken_hash/1,
    internal_block_not_found_by_hash/1,

    block_txs_count_by_height/1,
    block_txs_count_by_hash/1,
    block_txs_count_genesis/1,
    block_txs_count_latest/1,
    block_txs_count_pending/1,

    block_txs_count_by_height_not_found/1,
    block_txs_count_by_hash_not_found/1,
    block_txs_count_by_broken_hash/1,

    block_tx_index_by_height/1,
    block_tx_index_by_hash/1,
    block_tx_index_latest/1,
    block_tx_index_not_founds/1,

    block_txs_list_by_height/1,
    block_txs_list_by_hash/1,
    block_txs_list_by_height_invalid_range/1,
    block_txs_list_by_hash_invalid_range/1
   ]).

-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).

all() ->
    [
     {group, all_endpoints}
    ].

groups() ->
    [
     {all_endpoints, [sequence], [{group, external_endpoints},
                                  {group, internal_endpoints}]},
     {external_endpoints, [sequence],
      [
       % pings
       broken_pings,
       blocked_ping,
       correct_ping,

       % get block-s
       get_top_empty_chain,
       get_top_non_empty_chain,
       block_not_found_by_height,
       block_by_height,
       block_by_hash,
       block_not_found_by_broken_hash,
       block_not_found_by_hash,

       % sync APIs
       post_correct_blocks,
       post_broken_blocks,
       pending_transactions,
       post_correct_tx,
       post_broken_tx,
       post_broken_base58_tx,
  
       % balances
       miner_balance,
       balance_broken_address,
       all_accounts_balances,
       all_accounts_balances_empty,
       all_accounts_balances_disabled,

       % infos
       version,
       info_empty,
       info_more_than_30,
       info_less_than_30,
       info_disabled
      ]},
     {internal_endpoints, [sequence],
      [
       broken_spend_tx,
       miner_pub_key,
       block_number,

       internal_block_by_height,
       internal_block_by_hash,
       internal_block_genesis,
       internal_block_pending,
       internal_block_latest,

       internal_block_not_found_by_height,
       internal_block_not_found_by_broken_hash,
       internal_block_not_found_by_hash,

       block_txs_count_by_height,
       block_txs_count_by_hash,
       block_txs_count_genesis,
       block_txs_count_latest,
       block_txs_count_pending,

       block_txs_count_by_height_not_found,
       block_txs_count_by_hash_not_found,
       block_txs_count_by_broken_hash,

       block_tx_index_by_height,
       block_tx_index_by_hash,
       block_tx_index_latest,
       block_tx_index_not_founds,

       block_txs_list_by_height,
       block_txs_list_by_hash,
       block_txs_list_by_height_invalid_range,
       block_txs_list_by_hash_invalid_range
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.http_endpoints"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    aecore_suite_utils:create_configs(Config1, #{<<"chain">> =>
                                                 #{<<"persist">> => false}}),
    aecore_suite_utils:make_multi(Config1, [?NODE]),
    Config1.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]} | Config].

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================
correct_ping(_Config) ->
    #{<<"genesis_hash">> := GHash,
      <<"best_hash">> := TopHash
     } = PingObj = rpc(aec_sync, local_ping_object, []),
    EncGHash = aec_base58c:encode(block_hash, GHash),
    EncTopHash = aec_base58c:encode(block_hash, TopHash),
    {ok, 200, _} =
        post_ping(
          maps:put(<<"source">>, unique_peer(),
                   PingObj#{<<"genesis_hash">> => EncGHash,
                            <<"best_hash">> => EncTopHash})),
    ok.

broken_pings(_Config) ->
    % no 'source'
    {ok, 400, _} = post_ping(#{}),
    % no ping obj data
    {ok, 400, _} = post_ping(#{<<"source">> => unique_peer()}),
    PingObj = rpc(aec_sync, local_ping_object, []),
    % wrong genesis hash
    WrongGenHashPing = maps:merge(PingObj, #{<<"source">> => unique_peer(),
                                             <<"genesis_hash">> => <<"foo">>}),
    {ok, 409, #{<<"reason">> := <<"Different genesis blocks">>}} =
        post_ping(WrongGenHashPing),
    ok.

blocked_ping(_Config) ->
    Peer = unique_peer(),
    PingObj = rpc(aec_sync, local_ping_object, []),
    WrongGenHashPing = maps:merge(PingObj, #{<<"source">> => Peer,
                                             <<"genesis_hash">> => <<"foo">>}),
    {ok, 409, _} = post_ping(WrongGenHashPing),
    % node is blocked now 
    {ok, 403, #{<<"reason">> := <<"Not allowed">>}} =
        post_ping(maps:put(<<"source">>, Peer, PingObj)),
    ok.

get_top_empty_chain(_Config) ->
    {ok, 200, HeaderMap} = get_top(), 
    ct:log("~p returned header = ~p", [?NODE, HeaderMap]),
    {ok, 200, GenBlockMap} = get_block_by_height(0),
    {ok, GenBlock} = aec_blocks:deserialize_from_map(
                      aehttp_dispatch_ext:add_missing_to_genesis_block(GenBlockMap)),
    ExpectedMap = header_to_endpoint_top(aec_blocks:to_header(GenBlock)),
    ct:log("Cleaned top header = ~p", [ExpectedMap]),
    HeaderMap = ExpectedMap,
    ok.

get_top_non_empty_chain(_Config) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    ExpectedH = rpc(aec_conductor, top_header, []), 
    ExpectedMap = header_to_endpoint_top(ExpectedH),
    ct:log("Cleaned top header = ~p", [ExpectedMap]),
    {ok, 200, HeaderMap} = get_top(), 
    HeaderMap = ExpectedMap,
    #{<<"height">> := Height} = HeaderMap,
    true = Height > 0,
    ok.

block_by_height(_Config) ->
    BlocksToCheck = 4,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToCheck),
    TopHeader = rpc(aec_conductor, top_header, []), 
    BlocksToCheck = aec_headers:height(TopHeader),
    lists:foreach(
        fun(Height) ->
            {ok, ExpectedBlock} = rpc(aec_conductor, get_block_by_height, [Height]),
            ExpectedBlockMap = block_to_endpoint_map(ExpectedBlock), 
            {ok, 200, BlockMap} = get_block_by_height(Height),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            BlockMap = ExpectedBlockMap,
            #{<<"height">> := Height} = BlockMap
        end,
        lists:seq(0, BlocksToCheck)), % from genesis
    ok.

block_not_found_by_height(_Config) ->
    TopHeader = rpc(aec_conductor, top_header, []), 
    0 = aec_headers:height(TopHeader), %chain is empty
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            Height = rand:uniform(99) + 1, % random number 1-100
            {ok, 404, #{<<"reason">> := <<"Chain too short">>}} = get_block_by_height(Height)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_not_found_by_hash(_Config) ->
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            H = random_hash(),
            {error, block_not_found} = rpc(aec_conductor, get_block_by_hash, [H]), 
            Hash = aec_base58c:encode(block_hash, H),
            {ok, 404, #{<<"reason">> := <<"Block not found">>}} = get_block_by_hash(Hash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_not_found_by_broken_hash(_Config) ->
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(block_hash, random_hash()),
            {ok, 400, #{<<"reason">> := <<"Invalid hash">>}} = get_block_by_hash(BrokenHash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_by_hash(_Config) ->
    BlocksToCheck = 4,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToCheck),
    TopHeader = rpc(aec_conductor, top_header, []), 
    BlocksToCheck = aec_headers:height(TopHeader),
    lists:foreach(
        fun(Height) ->
            {ok, ExpectedBlock} = rpc(aec_conductor, get_block_by_height, [Height]),
            {ok, H} = aec_blocks:hash_internal_representation(ExpectedBlock),
            Hash = aec_base58c:encode(block_hash, H),
            ExpectedBlockMap = block_to_endpoint_map(ExpectedBlock), 
            {ok, 200, BlockMap} = get_block_by_hash(Hash),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            BlockMap = ExpectedBlockMap,
            #{<<"height">> := Height} = BlockMap
        end,
        lists:seq(0, BlocksToCheck)), % from genesis
    ok.

%% Maybe this test should be broken into a couple of smaller tests
%% it currently tests the possitive cases for
%% GET externalAPI/transactions
%% POST internalAPI/spend-tx
%% GET externalAPI/account/balance
pending_transactions(_Config) ->
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty 
    {ok, 200, []} = get_transactions(), 
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance(),
    BlocksToMine = 10,
    AmountToSpent = 7,
    Fee = 2,
    MineReward = rpc(aec_governance, block_mine_reward, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, 200, #{<<"balance">> := Bal0}} = get_balance(),  

    Bal0 = BlocksToMine * MineReward,
    true = (is_integer(Bal0) andalso Bal0 > AmountToSpent + Fee),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % still empty 
    {ok, 200, []} = get_transactions(), 

    %{ok, SenderPubKey} = rpc:call(?NODE, aec_keys, pubkey, [], 5000),
    ReceiverPubKey = random_hash(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance(aec_base58c:encode(account_pubkey, ReceiverPubKey)),

    {ok, 200, _} = post_spend_tx(ReceiverPubKey, AmountToSpent, Fee),
    {ok, NodeTxs} = rpc(aec_tx_pool, peek, [infinity]),
    true = length(NodeTxs) =:= 1, % not empty anymore
    {ok, 200, ReturnedTxs} = get_transactions(), 
    ExpectedTxs = [#{<<"tx">> => aec_base58c:encode(
                                   transaction,
                                   aec_tx_sign:serialize_to_binary(T))}
           || T <- NodeTxs],
    true = length(ExpectedTxs) =:= length(ReturnedTxs),
    true = lists:all(fun(Tx) -> lists:member(Tx, ExpectedTxs) end, ReturnedTxs),

    {ok, 200, #{<<"balance">> := Bal0}} = get_balance(),  
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance(aec_base58c:encode(account_pubkey, ReceiverPubKey)),


    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty again
    {ok, 200, []} = get_transactions(), 

    {ok, 200, #{<<"balance">> := Bal1}} = get_balance(),  
    Bal1 = Bal0 + MineReward + Fee - AmountToSpent - Fee,
    {ok, 200, #{<<"balance">> := AmountToSpent}} = 
                 get_balance(aec_base58c:encode(account_pubkey, ReceiverPubKey)),

    ok.

post_correct_blocks(Config) ->
    BlocksToPost = 10,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPost),
    Blocks =
        lists:map(
            fun(Height) ->
                {ok, B} = rpc(aec_conductor, get_block_by_height, [Height]),
                B
            end,
            lists:seq(1, BlocksToPost)),
    % consider a rpc call for cleaning the aec_chain_state's state
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    GH = rpc(aec_conductor, top_header, []), 
    0 = aec_headers:height(GH), %chain is empty
    lists:foreach(
        fun(Block) ->
            {ok, 200, _} = post_block(Block),
            H = rpc(aec_conductor, top_header, []), 
            {ok, HH} = aec_headers:hash_header(H),
            {ok, BH} = aec_blocks:hash_internal_representation(Block),
            BH = HH % block accepted
        end,
        Blocks),
    ok.

post_broken_blocks(Config) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   1),
    {ok, CorrectBlock} = rpc(aec_conductor, get_block_by_height, [1]),
    % consider a rpc call for cleaning the aec_chain_state's state
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    GH = rpc(aec_conductor, top_header, []), 
    0 = aec_headers:height(GH), %chain is empty
    CorrectBlockMap = aec_blocks:serialize_to_map(CorrectBlock),
    BrokenBlocks =
        lists:map(
            fun({Key, Value}) ->
                BrokenBlock = maps:put(Key, Value, CorrectBlockMap),
                {Key, BrokenBlock}
            end,
        [{<<"prev_hash">>, aec_base58c:encode(block_hash, <<"foo">>)},
         {<<"state_hash">>, aec_base58c:encode(block_state_hash, <<"foo">>)},
         {<<"txs_hash">>, aec_base58c:encode(block_tx_hash, <<"foo">>)},
         {<<"target">>, 42},
         {<<"nonce">>, 42},
         {<<"time">>, 42},
         {<<"version">>, 42},
         {<<"pow">>, lists:seq(1, 42)},
         {<<"transactions">>, []}
        ]),

    lists:foreach(
        fun({BrokenField, BlockMap}) ->
            ct:log("Testing with a broken ~p", [BrokenField]),
            {ok, Block} = aec_blocks:deserialize_from_map(BlockMap),
            {ok, 400, #{<<"reason">> := <<"Block rejected">>}} = post_block(Block),
            H = rpc(aec_conductor, top_header, []), 
            0 = aec_headers:height(H) %chain is still empty
        end,
        BrokenBlocks),
    ok.

%% Even though a tx with a unknown sender pubkey would be accepted, we need
%% a valid account nonce; that's why we mine a while
post_correct_tx(_Config) ->
    Amount = 7,
    Fee = 2,
    BlocksToMine = 10,
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender => PubKey,
            recipient => random_hash(),
            amount => Amount,
            fee => Fee,
            nonce => Nonce}),
    {ok, SignedTx} = rpc(aec_keys, sign, [SpendTx]),
    {ok, 200, _} = post_tx(aec_base58c:encode(transaction, aec_tx_sign:serialize_to_binary(SignedTx))),
    {ok, [SignedTx]} = rpc(aec_tx_pool, peek, [infinity]), % same tx 
    ok.

post_broken_tx(_Config) ->
    Amount = 7,
    Fee = 2,
    BlocksToMine = 10,
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender => PubKey,
            recipient => random_hash(),
            amount => Amount,
            fee => Fee,
            nonce => Nonce}),
    {ok, SignedTx} = rpc(aec_keys, sign, [SpendTx]),
    [T, V, SerializedTx, Sigs] = aec_tx_sign:serialize(SignedTx),
    lists:foreach(
        fun(Key) ->
            BrokenTx = lists:filter(
                fun(#{Key := _}) -> true;
                (_) -> false
                end,
                SerializedTx),
            EncodedBrokenTx = aec_base58c:encode(
                                transaction,
                                msgpack:pack([T, V, BrokenTx, Sigs])),
            {ok, 400, #{<<"reason">> := <<"Invalid tx">>}} = post_tx(EncodedBrokenTx)
        end,
        [<<"type">>,
         <<"vsn">>,
         <<"sender">>,
         <<"recipient">>,
         <<"amount">>,
         <<"fee">>,
         <<"nonce">>]),

    ok.

post_broken_base58_tx(_Config) ->
    Amount = 7,
    Fee = 2,
    BlocksToMine = 10,
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            {ok, SpendTx} =
                aec_spend_tx:new(
                  #{sender => PubKey,
                    recipient => random_hash(),
                    amount => Amount,
                    fee => Fee,
                    nonce => Nonce}),
            {ok, SignedTx} = rpc(aec_keys, sign, [SpendTx]),
            <<_, BrokenHash/binary>> =
                aec_base58c:encode(transaction,
                                   aec_tx_sign:serialize_to_binary(SignedTx)),
            {ok, 400, #{<<"reason">> := <<"Invalid base58Check encoding">>}} = post_tx(BrokenHash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

%% changing of another account's balance is checked in pending_transactions test
miner_balance(_Config) ->
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance(),
    BlocksToMine = 10,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, Bal} = rpc(aec_mining, get_miner_account_balance, []),
    {ok, 200, #{<<"balance">> := Bal}} = get_balance(),  
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"balance">> := Bal}} =
        get_balance(aec_base58c:encode(account_pubkey, PubKey)),
    ok.

balance_broken_address(_Config) ->
    NumberOfChecks = 10,
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(account_pubkey,
                                                          random_hash()),
            {ok, 400, #{<<"reason">> := <<"Invalid address">>}} = get_balance(BrokenHash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

all_accounts_balances(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    GenesisAccounts = rpc(aec_genesis_block_settings, preset_accounts, []),
    BlocksToMine = 10,
    Receivers = 10,
    Fee = 1,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ReceiversAccounts = [{random_hash(), Idx} || Idx <- lists:seq(1, Receivers)],
    lists:foreach(
        fun({ReceiverPubKey, AmountToSpent}) ->
            {ok, 200, _} = post_spend_tx(ReceiverPubKey, AmountToSpent, Fee)
        end,
        ReceiversAccounts),

    % mine a block to include the txs
    {ok, [Block]} = aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, 200, #{<<"accounts_balances">> := Balances}} = get_all_accounts_balances(),
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, MinerBal} = rpc(aec_mining, get_miner_account_balance, []),
    ExpectedBalances = [{MinerPubKey, MinerBal} | GenesisAccounts] ++  ReceiversAccounts,

    % make sure all spend txs are part of the block
    AllTxs = aec_blocks:txs(Block),
    AllTxsCnt = length(AllTxs),
    AllTxsCnt = Receivers + 1, % all spendTxs and a coinbaseTx

    true = length(Balances) =:= length(ExpectedBalances),
    true =
        lists:all(
            fun(#{<<"pub_key">> := PKEncoded, <<"balance">> := Bal}) ->
                    {account_pubkey, AccDec} = aec_base58c:decode(PKEncoded),
                    Account = {AccDec, Bal},
                lists:member(Account, ExpectedBalances) end,
            Balances),
    ok.

all_accounts_balances_empty(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    GenesisAccounts = rpc(aec_genesis_block_settings, preset_accounts, []),
    {ok, 200, #{<<"accounts_balances">> := Balances}} = get_all_accounts_balances(),
    true = length(Balances) =:= length(GenesisAccounts),
    true =
        lists:all(
            fun(#{<<"pub_key">> := PKEncoded, <<"balance">> := Bal}) ->
                    {account_pubkey, AccDec} = aec_base58c:decode(PKEncoded),
                Account = {AccDec, Bal},
                lists:member(Account, GenesisAccounts) end,
            Balances),
    ok.

all_accounts_balances_disabled(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, false]),
    {ok, 403, #{<<"reason">> := <<"Balances not enabled">>}} = get_all_accounts_balances(),
    ok.

version(_Config) ->
    {ok, 200, #{<<"version">> := V,
                <<"revision">> := Rev,
                <<"genesis_hash">> := EncodedGH}} = get_version(),
    V0 = rpc(aeu_info, get_version, []),
    Rev0 = rpc(aeu_info, get_revision, []),
    GenHash0 = rpc(aec_conductor, genesis_hash, []),
    % asserts
    V = V0,
    Rev = Rev0,
    {block_hash, GenHash0} = aec_base58c:decode(EncodedGH),
    ok.

info_disabled(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, false]),
    {ok, 403, #{<<"reason">> := <<"Info not enabled">>}} = get_info(),
    ok.

info_empty(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    ExpectedEmpty = #{<<"last_30_blocks_time">> => [ #{<<"difficulty">> => 1.0,
                                                       <<"height">> => 0,
                                                       <<"time">>  => 0}]},
    {ok, 200, ExpectedEmpty} = get_info(),
    ok.

info_more_than_30(_Config) ->
    test_info(40).

info_less_than_30(_Config) ->
    test_info(20).

test_info(BlocksToMine) ->
    rpc(application, set_env, [aecore, expected_mine_rate, 100]),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, 200, #{<<"last_30_blocks_time">> := Summary}} = get_info(),
    ExpectedCnt = min(BlocksToMine + 1, 30),
    ExpectedCnt = length(Summary),
    lists:foldl(
        fun(BlockSummary, 0) ->
            #{<<"difficulty">> := 1.0,
              <<"height">> := 0,
              <<"time">>  := 0} = BlockSummary;
        (BlockSummary, ExpectedHeight) ->
            #{<<"height">> := ExpectedHeight} = BlockSummary,
            {ok, 200, BlockMap} = get_block_by_height(ExpectedHeight),
            #{<<"time">> := Time, <<"target">> := Target} = BlockMap,
            #{<<"time">> := Time} = BlockSummary,
            Difficulty = aec_pow:target_to_difficulty(Target),
            #{<<"difficulty">> := Difficulty} = BlockSummary,
            {ok, 200, PreviousBlockMap} = get_block_by_height(ExpectedHeight -1),
            #{<<"time">> := PrevTime} = PreviousBlockMap,
            TimeDelta = Time - PrevTime,
            #{<<"time_delta_to_parent">> := TimeDelta} = BlockSummary,
            ExpectedHeight -1
        end,
        BlocksToMine,
        Summary),

    ok.



    

%% possitive test of spend_tx is handled in pending_transactions test
broken_spend_tx(_Config) ->
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance(),
    ReceiverPubKey = random_hash(),
    {ok, 404, _} = post_spend_tx(ReceiverPubKey, 42, 2),
    ok.

miner_pub_key(_Config) ->
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    ct:log("MinerPubkey = ~p~nEncodedPubKey = ~p", [MinerPubKey,
                                                    EncodedPubKey]),
    {account_pubkey, MinerPubKey} = aec_base58c:decode(EncodedPubKey),
    ok.

block_number(_Config) ->
    TopHeader = rpc(aec_conductor, top_header, []), 
    0 = aec_headers:height(TopHeader),
    {ok, 200, #{<<"height">> := 0}} = get_block_number(),
    lists:foreach(
        fun(ExpectedNum) ->
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            {ok, 200, #{<<"height">> := Num}} = get_block_number(),
            ExpectedNum = Num
        end,
        lists:seq(1, 10)),
    ok.

internal_block_by_height(_Config) ->
    GetExpectedBlockFun =
        fun(H) -> rpc(aec_conductor, get_block_by_height, [H]) end,
    CallApiFun = fun get_internal_block_by_height/2,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

internal_block_not_found_by_height(_Config) ->
    lists:foreach(
        fun(H) ->
            lists:foreach(
                fun(Opt) ->
                    {ok, 404, #{<<"reason">> := <<"Chain too short">>}}
                        = get_internal_block_by_height(H, Opt)
                end,
                [default, false, true])
        end,
        lists:seq(1, 10)),
    ok.

internal_block_by_hash(_Config) ->
    GetExpectedBlockFun =
        fun(H) -> rpc(aec_conductor, get_block_by_height, [H]) end,
    CallApiFun =
        fun(H, Opts) ->
            {ok, Hash} = block_hash_by_height(H),
            get_internal_block_by_hash(Hash, Opts)
        end,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

internal_block_not_found_by_hash(_Config) ->
    lists:foreach(
        fun(_Height) ->
            lists:foreach(
                fun(Opt) ->
                    H = random_hash(),
                    {error, block_not_found} = rpc(aec_conductor,
                                                   get_block_by_hash, [H]), 
                    Hash = aec_base58c:encode(block_hash, H),
                    {ok, 404, #{<<"reason">> := <<"Block not found">>}}
                        = get_internal_block_by_hash(Hash, Opt)
                end,
                [default, false, true])
        end,
        lists:seq(1, 10)),
    ok.

internal_block_not_found_by_broken_hash(_Config) ->
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(block_hash, random_hash()),
            lists:foreach(
                fun(Opt) ->
                    {ok, 400, #{<<"reason">> := <<"Invalid hash">>}} =
                        get_internal_block_by_hash(BrokenHash, Opt)
                end,
                [default, false, true])
        end,
        lists:seq(1, 10)),
    ok.

internal_block_genesis(_Config) ->
    GetExpectedBlockFun =
        fun(_H) -> rpc(aec_conductor, genesis_block, []) end,
    CallApiFun =
        fun(_H, Opts) ->
            get_internal_block_preset("genesis", Opts)
        end,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

internal_block_latest(_Config) ->
    GetExpectedBlockFun =
        fun(_H) ->
            TopBlock = rpc(aec_conductor, top, []),
            {ok, TopBlock}
        end,
    CallApiFun =
        fun(_H, Opts) ->
            get_internal_block_preset("latest", Opts)
        end,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

%% we need really slow mining; since mining speed is not modified for the
%% first X blocks, we need to premine them before the test
internal_block_pending(_Config) ->
    BlocksToPremine = rpc(aec_governance, blocks_to_check_difficulty_count, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPremine),
    lists:foreach(
        fun(Opt) ->
            {ok, 404, #{<<"reason">> := <<"Not mining, no pending block">>}} =
                    get_internal_block_preset("pending", Opt)
        end,
        [default, true, false]),
    rpc(application, set_env, [aecore, expected_mine_rate,
                               60 * 60 * 1000]), % aim at one block an hour
    add_spend_txs(),
    rpc(aec_conductor, start_mining, []),
    timer:sleep(100),% so the miner is started
    {ok, PendingBlock} = get_pending_block(),
    ExpectedPendingTx = maps:put(<<"data_schema">>,
                <<"BlockWithTxsHashes">>,
                block_to_endpoint_map(PendingBlock)),
    ct:log("Expected pending block ~p", [ExpectedPendingTx]),
    GetPending =
        fun TryToGet(_Opt, Attempts) when Attempts < 1 ->
                {error, no_pending_block_returned};
            TryToGet(Opt, Attempts) ->
                case get_internal_block_preset("pending", Opt) of
                    {ok, 200, P} -> {ok, P};
                    {ok, 404, _} -> TryToGet(Opt, Attempts -1)
                end
        end,
    {ok, PendingTxDefault} = GetPending(default, 10),
    {ok, PendingTxHashes} = GetPending(false, 10),
    ValidateKeys =
        fun(Map1, Map2, Key) ->
            true = maps:get(Key, Map1, not_found1) =:=
                   maps:get(Key, Map2, not_found2)
        end,
    % no block should have been mined, so the same prev_hash
    ValidateKeys(ExpectedPendingTx, PendingTxDefault, <<"prev_hash">>),
    ValidateKeys(ExpectedPendingTx, PendingTxDefault, <<"data_schema">>),

    % no block should have been mined, so the same prev_hash
    ValidateKeys(ExpectedPendingTx, PendingTxHashes, <<"prev_hash">>),
    ValidateKeys(ExpectedPendingTx, PendingTxHashes, <<"data_schema">>),

    ExpectedPendingTxsObjects = maps:put(<<"data_schema">>,
                <<"BlockWithTxs">>,
                block_to_endpoint_map(PendingBlock, #{tx_objects => true})),
    ct:log("Expected pending block with tx objects~p",
           [ExpectedPendingTxsObjects]),
    {ok, PendingTxObjects} = GetPending(true, 10),
    
    % no block should have been mined, so the same prev_hash
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxObjects, <<"prev_hash">>),
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxObjects, <<"data_schema">>),
    ok.

internal_get_block_generic(GetExpectedBlockFun, CallApiFun) ->
    BlocksToCheck = 4,
    lists:foreach(
        fun(Height) ->
            {ok, 200, #{<<"height">> := Height}} = get_top(),
            {ok, ExpectedBlock} = GetExpectedBlockFun(Height),
            ExpectedBlockMap = maps:put(<<"data_schema">>,
                <<"BlockWithTxsHashes">>,
                block_to_endpoint_map(ExpectedBlock)),
            {ok, 200, BlockMap} = CallApiFun(Height, default),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            {ok, 200, BlockMap1} = CallApiFun(Height, false),
            true = equal_block_maps(BlockMap, ExpectedBlockMap), 
            true = equal_block_maps(BlockMap1, ExpectedBlockMap), 

            ExpectedBlockMapTxsObjects = maps:put(<<"data_schema">>,
                <<"BlockWithTxs">>,
                block_to_endpoint_map(ExpectedBlock, #{tx_objects => true})),
            {ok, 200, BlockMap2} = CallApiFun(Height, true),
            ct:log("ExpectedBlockMapTxsObjects ~p, BlockMap2: ~p",
                   [ExpectedBlockMapTxsObjects, BlockMap2]),
            true = equal_block_maps(BlockMap2, ExpectedBlockMapTxsObjects), 
            % prepare the next block
            case Height of
                0 -> pass; % no token to spend yet
                _ -> add_spend_txs()
            end,
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1)
        end,
        lists:seq(0, BlocksToCheck)), % from genesis
    ok.

block_txs_count_by_height(_Config) ->
    generic_counts_test(fun(H) -> rpc(aec_conductor, get_block_by_height,
                                     [H]) end,
                        fun get_block_txs_count_by_height/1).

block_txs_count_by_hash(_Config) ->
    CallApiFun =
        fun(H) ->
            {ok, Hash} = block_hash_by_height(H),
            get_block_txs_count_by_hash(Hash)
        end,
    generic_counts_test(fun(H) -> rpc(aec_conductor, get_block_by_height,
                                     [H]) end,
                        CallApiFun).

block_txs_count_genesis(_Config) ->
    generic_counts_test(
        fun(_H) -> rpc(aec_conductor, genesis_block, []) end,
        fun(_) -> get_block_txs_count_preset("genesis") end).

block_txs_count_latest(_Config) ->
    generic_counts_test(
        fun(_H) ->
            TopBlock = rpc(aec_conductor, top, []),
            {ok, TopBlock}
        end,
        fun(_) -> get_block_txs_count_preset("latest") end).

block_txs_count_pending(_Config) ->
    BlocksToPremine = rpc(aec_governance, blocks_to_check_difficulty_count, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPremine),
        {ok, 404, #{<<"reason">> := <<"Not mining, no pending block">>}} =
                    get_block_txs_count_preset("pending"),
    rpc(application, set_env, [aecore, expected_mine_rate,
                               60 * 60 * 1000]), % aim at one block an hour
    InsertedTxsCount = add_spend_txs(),
    rpc(aec_conductor, start_mining, []),
    GetPending =
        fun TryToGet(Attempts) when Attempts < 1 ->
                {error, no_pending_block_returned};
            TryToGet(Attempts) ->
                case get_block_txs_count_preset("pending") of
                    {ok, 200, P} -> {ok, P};
                    {ok, 404, _} ->
                        TryToGet(Attempts -1)
                end
        end,
    {ok, #{<<"count">> := TxsCount}} = GetPending(10),
    ct:log("Inserted transactions count ~p, transactions count in the pending block ~p",
           [InsertedTxsCount, TxsCount]),
    % the assert bellow rellies on no block being mined durring the test run
    % this is achieved by mining BlocksToPremine number of blocks and setting
    % a high value for expected_mine_rate
    true = TxsCount =:= InsertedTxsCount + 1,
    ok.

generic_counts_test(GetBlock, CallApi) ->
    BlocksToMine = 10,
    lists:foreach(
        fun(Height) ->
            {ok, B} = GetBlock(Height),
            TxsCount = length(aec_blocks:txs(B)),
            {ok, 200, #{<<"count">> := TxsCount}} = CallApi(Height),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            add_spend_txs()
        end,
        lists:seq(0, BlocksToMine)), % from genesis
    ok.

block_txs_count_by_height_not_found(_Config) ->
    lists:foreach(
        fun(H) ->
            {ok, 404, #{<<"reason">> := <<"Chain too short">>}}
                        = get_block_txs_count_by_height(H)
        end,
        lists:seq(1, 10)),
    ok.

block_txs_count_by_hash_not_found(_Config) ->
    lists:foreach(
        fun(_Height) ->
            H = random_hash(),
            {error, block_not_found} = rpc(aec_conductor,
                                            get_block_by_hash, [H]), 
            Hash = aec_base58c:encode(block_hash, H),
            {ok, 404, #{<<"reason">> := <<"Block not found">>}}
                = get_block_txs_count_by_hash(Hash)
        end,
        lists:seq(1, 10)),
    ok.

block_txs_count_by_broken_hash(_Config) ->
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(block_hash, random_hash()),
            {ok, 400, #{<<"reason">> := <<"Invalid hash">>}} =
                get_block_txs_count_by_hash(BrokenHash)
        end,
        lists:seq(1, 10)),
    ok.

block_tx_index_by_height(_Config) ->
    generic_block_tx_index_test(fun get_block_tx_by_index_height/3).

block_tx_index_by_hash(_Config) ->
    CallApiFun =
        fun(H, Index, Opts) ->
            {ok, Hash} = block_hash_by_height(H),
            get_block_tx_by_index_hash(Hash, Index, Opts)
        end,
    generic_block_tx_index_test(CallApiFun).

block_tx_index_latest(_Config) ->
    generic_block_tx_index_test(
        fun(_, Index, Opts) ->
            get_block_tx_by_index_latest(Index, Opts)
        end).

block_tx_index_not_founds(_Config) ->
    RandomHeight = rand:uniform(999) + 1, % 1..1000
    Test =
        fun(Code, ErrMsg, Fun, Cases) ->
            lists:foreach(
                fun({H, I}) ->
                    lists:foreach(
                        fun(Opt) ->
                            {ok, Code, #{<<"reason">> := ErrMsg}} = Fun(H, I, Opt) end,
                        [default, false, true])
                end,
                Cases)
        end,
    Test(404, <<"Chain too short">>, fun get_block_tx_by_index_height/3,
         [{1, 0},
          {1, 1},
          {RandomHeight, 0},
          {RandomHeight, 1},
          {RandomHeight + 1, 0},
          {RandomHeight + 1, 1}]),
    Test(404, <<"Block not found">>, fun get_block_tx_by_index_hash/3,
         [{aec_base58c:encode(block_hash, random_hash()), 0},
          {aec_base58c:encode(block_hash, random_hash()), 1}]),
    BlocksToMine = 3,
    lists:foreach(
        fun(Height) ->
            {ok, 200, #{<<"count">> := TxsCount}} = get_block_txs_count_by_height(Height),
            Test(404, <<"Transaction not found">>, fun get_block_tx_by_index_height/3,
                [{Height, TxsCount + 1},
                 {Height, TxsCount + 2},
                 {Height, TxsCount + rand:uniform(1000) + 1}
                ]),
            {ok, Hash} = block_hash_by_height(Height),
            Test(404, <<"Transaction not found">>, fun get_block_tx_by_index_hash/3,
                [{Hash, TxsCount + 1},
                 {Hash, TxsCount + 2},
                 {Hash, TxsCount + rand:uniform(1000) + 1}
                ]),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            add_spend_txs()
        end, lists:seq(0, BlocksToMine)),
    
    ok.


generic_block_tx_index_test(CallApi) when is_function(CallApi, 3)->
    BlocksToMine = 10,
    lists:foreach(
        fun(Height) ->
            lists:foreach(
                fun({Opts, DataSchema}) ->
                    {ok, 200, BlockMap} =
                        get_internal_block_by_height(Height, Opts),
                    AllTxs = maps:get(<<"transactions">>, BlockMap, []),
                    TxsIdxs =
                        case length(AllTxs) of
                            0 ->
                                [];
                            TxsLength ->
                                lists:seq(1, TxsLength)
                        end,
                    lists:foreach(
                        fun({Tx, Index}) ->
                            {ok, 200, #{<<"data_schema">> := DataSchema,
                                        <<"transaction">> := Tx}} = CallApi(Height,
                                                                            Index,
                                                                            Opts)
                        end,
                        lists:zip(AllTxs, TxsIdxs))
                end,
                [{default, <<"SingleTxHash">>},
                 {false,  <<"SingleTxHash">>},
                 {true,  <<"SingleTxObject">>}]),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            add_spend_txs()
        end,
        lists:seq(0, BlocksToMine)), % from genesis
    ok.

block_txs_list_by_height(_Config) ->
    generic_range_test(fun get_block_txs_list_by_height/3,
                        fun(H) -> H end).

block_txs_list_by_hash(_Config) ->
    generic_range_test(fun get_block_txs_list_by_hash/3,
                        fun(H) ->
                            {ok, Hash} = block_hash_by_height(H),
                            Hash
                        end).

generic_range_test(GetTxsApi, HeightToKey) ->
    MaximumRange = rpc(aec_conductor, max_block_range, []),
    BlocksToMine = 2 * MaximumRange,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    lists:foreach(
        fun(From) ->
            lists:foreach(
                fun(Length) ->
                    single_range_test(From, From + Length,
                                       GetTxsApi, HeightToKey)
                end,
                lists:seq(0, MaximumRange))
        end,
        lists:seq(0, MaximumRange)),
    ok.

single_range_test(HeightFrom, HeightTo, GetTxsApi, HeightToKey) ->
    lists:foreach(
        fun(Opts) ->
            From = HeightToKey(HeightFrom),
            To = HeightToKey(HeightTo),
            {ok, 200, Result} = GetTxsApi(From, To, Opts),
            ExpectedResult =
                expected_range_result(HeightFrom, HeightTo, Opts),
            ct:log("Expected Result ~p, Actual result ~p",
                    [ExpectedResult, Result]),
            ExpectedResult = Result
        end,
        [default, false, true]).

expected_range_result(HeightFrom, HeightTo, TxObjects) ->
    Txs =
        lists:foldl(
            fun(Height, Accum) ->
                {ok, 200, BlockMap} =
                    get_internal_block_by_height(Height, TxObjects),
                % genesis block doesn't have transactions
                BlockTxs = maps:get(<<"transactions">>, BlockMap, []),
                BlockTxs ++ Accum
            end,
            [],
            lists:seq(HeightFrom, HeightTo)),
    DataSchema =
        case TxObjects of
            default -> 
                <<"TxMsgPackHashes">>;
            false ->
                <<"TxMsgPackHashes">>;
            true ->
                <<"TxObjects">>
          end,
    #{<<"data_schema">> => DataSchema,
      <<"transactions">> => Txs}.

block_txs_list_by_height_invalid_range(_Config) ->
    MaximumRange = rpc(aec_conductor, max_block_range, []),
    BlocksToMine = 2 * MaximumRange,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ValidateError =
        fun(From, To, Error) ->
            lists:foreach(
                fun(Opts) ->
                    Error = get_block_txs_list_by_height(From, To, Opts)
                end,
                [default, false, true])
        end,
    InvalidRange = {ok, 400, #{<<"reason">> => <<"From's height is bigger than To's">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, InvalidRange) end,
        [{BlocksToMine, 0}, {BlocksToMine, 1}]),
    RangeTooBig = {ok, 400, #{<<"reason">> => <<"Range too big">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, RangeTooBig) end,
        [{0, MaximumRange +1}, {1, MaximumRange + 2}]),
    ChainTooShort = {ok, 404, #{<<"reason">> => <<"Chain too short">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, ChainTooShort) end,
        [{BlocksToMine -1, BlocksToMine + 1},
         {BlocksToMine, BlocksToMine + 1},
         {BlocksToMine +1, BlocksToMine + 1}]),
    ok.

block_txs_list_by_hash_invalid_range(_Config) ->
    MaximumRange = rpc(aec_conductor, max_block_range, []),
    BlocksToMine = 2 * MaximumRange,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ValidateError =
        fun(From, To, Error) ->
            lists:foreach(
                fun(Opts) ->
                    Error = get_block_txs_list_by_hash(From, To, Opts)
                end,
                [default, false, true])
        end,
    {ok, GenBlockHash} = block_hash_by_height(0),
    {ok, Block1Hash} = block_hash_by_height(1),
    {ok, Block2Hash} = block_hash_by_height(2),
    {ok, BlockX1Hash} = block_hash_by_height(MaximumRange + 1),
    {ok, BlockX2Hash} = block_hash_by_height(MaximumRange + 2),
    InvalidRange = {ok, 400, #{<<"reason">> => <<"From's height is bigger than To's">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, InvalidRange) end,
        [{Block1Hash, GenBlockHash},
         {Block2Hash, Block1Hash}]),
    RangeTooBig = {ok, 400, #{<<"reason">> => <<"Range too big">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, RangeTooBig) end,
        [{GenBlockHash, BlockX1Hash},
         {Block1Hash, BlockX2Hash}]),
    ChainTooShort = {ok, 404, #{<<"reason">> => <<"Block not found">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, ChainTooShort) end,
        [{GenBlockHash, aec_base58c:encode(block_hash, random_hash())},
         {Block1Hash, aec_base58c:encode(block_hash, random_hash())},
         {aec_base58c:encode(block_hash, random_hash()), Block1Hash},
         {aec_base58c:encode(block_hash, random_hash()), GenBlockHash}
        ]),
    ok.

%% ============================================================
%% HTTP Requests 
%% ============================================================

get_top() ->
    Host = external_address(),
    http_request(Host, get, "top", []).

post_ping(Body) ->
    Host = external_address(),
    http_request(Host, post, "ping", Body).

get_block_by_height(Height) ->
    Host = external_address(),
    http_request(Host, get, "block-by-height", [{height, Height}]).

get_block_by_hash(Hash) ->
    Host = external_address(),
    http_request(Host, get, "block-by-hash", [{hash, Hash}]).

get_transactions() ->
    Host = external_address(),
    http_request(Host, get, "transactions", []).

post_spend_tx(Recipient, Amount, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "spend-tx",
                 #{recipient_pubkey => aec_base58c:encode(
                                         account_pubkey, Recipient),
                   amount => Amount,
                   fee => Fee}).

get_balance() ->
    Host = external_address(),
    http_request(Host, get, "account/balance", []).

get_balance(PubKey) ->
    Host = external_address(),
    http_request(Host, get, "account/balance", [{pub_key, PubKey}]).

post_block(Block) ->
    Host = external_address(),
    BlockMap = aec_blocks:serialize_to_map(Block),
    http_request(Host, post, "block", BlockMap).

post_tx(TxSerialized) ->
    Host = external_address(),
    http_request(Host, post, "tx", #{tx => TxSerialized}).

get_all_accounts_balances() ->
    Host = external_address(),
    http_request(Host, get, "balances", []).

get_miner_pub_key() ->
    Host = internal_address(),
    http_request(Host, get, "account/pub-key", []).

get_version() ->
    Host = external_address(),
    http_request(Host, get, "version", []).

get_info() ->
    Host = external_address(),
    http_request(Host, get, "info", []).

get_block_number() ->
    Host = internal_address(),
    http_request(Host, get, "block/number", []).

get_internal_block_by_height(Height, TxObjects) ->
    Params = tx_objects_params(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/height/" ++ integer_to_list(Height), Params).

get_internal_block_by_hash(Hash, TxObjects) ->
    Params = tx_objects_params(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/hash/" ++ http_uri:encode(Hash), Params).

get_internal_block_preset(Segment, TxObjects) ->
    Params = tx_objects_params(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/" ++ Segment, Params).

tx_objects_params(default) -> #{};
tx_objects_params(true) -> #{tx_objects => <<"true">>};
tx_objects_params(false) -> #{tx_objects => <<"false">>}.

get_block_txs_count_by_height(Height) ->
    Host = internal_address(),
    http_request(Host, get, "block/txs/count/height/" ++ integer_to_list(Height),
                 []).

get_block_txs_count_by_hash(Hash) ->
    Host = internal_address(),
    http_request(Host, get, "block/txs/count/hash/" ++ http_uri:encode(Hash),
                 []).

get_block_txs_count_preset(Segment) ->
    Host = internal_address(),
    http_request(Host, get, "block/txs/count/" ++ Segment, []).

get_block_tx_by_index_height(Height, Index, TxObjects) ->
    Params = tx_objects_params(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/height/" ++ integer_to_list(Height) ++
                                       "/" ++ integer_to_list(Index), Params).

get_block_tx_by_index_hash(Hash, Index, TxObjects) when is_binary(Hash) ->
    get_block_tx_by_index_hash(binary_to_list(Hash), Index, TxObjects);
get_block_tx_by_index_hash(Hash, Index, TxObjects) ->
    Params = tx_objects_params(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/hash/" ++ http_uri:encode(Hash) ++
                                       "/" ++ integer_to_list(Index), Params).

get_block_tx_by_index_latest(Index, TxObjects) ->
    Params = tx_objects_params(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/latest/" ++ integer_to_list(Index), Params).

get_block_txs_list_by_height(From, To, TxObjects) ->
    Params0 = tx_objects_params(TxObjects),
    Params = maps:merge(Params0, #{from => From, to => To}),
    Host = internal_address(),
    http_request(Host, get, "block/txs/list/height", Params).

get_block_txs_list_by_hash(From, To, TxObjects) ->
    Params0 = tx_objects_params(TxObjects),
    Params = maps:merge(Params0, #{from => From, to => To}),
    Host = internal_address(),
    http_request(Host, get, "block/txs/list/hash", Params).
%% ============================================================
%% private functions
%% ============================================================
rpc(Mod, Fun, Args) ->
    rpc:call(aecore_suite_utils:node_name(?NODE), Mod, Fun, Args, 5000).

external_address() ->
    Port = rpc(aeu_env, user_config_or_env, 
              [ [<<"http">>, <<"external">>, <<"port">>], 
                aehttp, [swagger_port_external], 8043]),
    aeu_requests:pp_uri({http, "127.0.0.1", Port}). % good enough for requests

internal_address() ->
    Port = rpc(aeu_env, user_config_or_env, 
              [ [<<"http">>, <<"internal">>, <<"port">>], 
                aehttp, [internal, swagger_port], 8143]),
    aeu_requests:pp_uri({http, "127.0.0.1", Port}).

http_request(Host, get, Path, Params) ->
    URL = binary_to_list(
            iolist_to_binary([Host, "/v2/", Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc:request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    URL = binary_to_list(iolist_to_binary([Host, "/v2/", Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc:request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R).


encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    http_uri:encode(V).

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result =
                    case iolist_to_binary(Body) of
                        <<>> ->
                            #{};
                        BodyB ->
                            jsx:decode(BodyB, [return_maps])
                    end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.

header_to_endpoint_top(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    {ok, HMap} = aec_headers:serialize_to_map(Header),
    CleanedH = aehttp_dispatch_ext:cleanup_genesis(HMap),
    maps:put(<<"hash">>, aec_base58c:encode(block_hash, Hash), CleanedH).

block_to_endpoint_map(Block) ->
    block_to_endpoint_map(Block, #{tx_objects => false}).

block_to_endpoint_map(Block, Options) ->
    SerializeFun =
        case maps:get(tx_objects, Options, false) of
            true -> fun aec_blocks:serialize_client_readable/1;
            false -> fun aec_blocks:serialize_to_map/1
        end,
    BMap = SerializeFun(Block),
    aehttp_dispatch_ext:cleanup_genesis(BMap).

%% misbehaving peers get blocked so we need unique ones
%% the function bellow is not perfect: there is a slight possibility of having a
%% duplicate
%% P = NumberOfPeersRequested / AllCombinations.
%% AllCombinations = 254 * 255 * 255 * 255 * 65535  = 276 011 744 298 750 
unique_peer() ->
    IPsegments =
        lists:map(
            fun(1) ->
                integer_to_list(rand:uniform(254) + 1);
            (_) ->
                integer_to_list(rand:uniform(255))
            end,
            lists:seq(1, 4)),
    IP = string:join(IPsegments, "."),
    Port = integer_to_list(rand:uniform(65535)),
    Unique = list_to_binary(IP ++ ":" ++ Port),
    <<"http://", Unique/binary >>.

random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).

prepare_for_spending(BlocksToMine) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty 
    {ok, 200, _} = get_balance(), % account present
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [PubKey]),
    {PubKey, Nonce}.

-spec block_hash_by_height(integer()) -> string().
block_hash_by_height(Height) ->
    {ok, B} = rpc(aec_conductor, get_block_by_height, [Height]),
    {ok, HBin} = aec_blocks:hash_internal_representation(B),
    Hash = binary_to_list(aec_base58c:encode(block_hash, HBin)),
    {ok, Hash}.

-spec get_pending_block() -> {error, no_candidate}
                           | {error, not_mining}
                           | {ok, term()}.
get_pending_block() ->
    GetPendingBlock =
        fun TryGetPending(N) when N < 1 -> {error, no_candidate};
            TryGetPending(Attempts) ->
                case rpc(aec_conductor, get_block_candidate, []) of
                    {ok, _} = OK -> OK;
                    {error, not_mining} = Err->
                        Err;
                    {error, miner_starting} ->
                        timer:sleep(200),
                        TryGetPending(Attempts - 1)
                end
        end,
    GetPendingBlock(10).

add_spend_txs() ->
    MineReward = rpc(aec_governance, block_mine_reward, []),
    MinFee = rpc(aec_governance, minimum_tx_fee, []),
    MaxSpendTxsInBlock = rpc(aec_governance, max_txs_in_block, []) - 1, % coinbase
    MinimalAmount = 1,
    MaxTxs = min(MineReward div (MinimalAmount + MinFee), % enough tokens
                 MaxSpendTxsInBlock), % so it can fit in one block
    true = MaxTxs > 0,
    TxsCnt =
        case MaxTxs of
            1 -> 1;
            _ -> rand:uniform(MaxTxs - 1) + 1
        end,
    ct:log("adding ~p spend txs", [TxsCnt]),
    Txs =
        lists:map(
            fun(_) ->
                #{recipient => random_hash(), amount => MinimalAmount, fee => MinFee}
            end,
            lists:seq(0, TxsCnt -1)),
    populate_block(#{spend_txs => Txs}),
    TxsCnt.

populate_block(Txs) ->
    lists:foreach(
        fun(#{recipient := R, amount := A, fee := F}) ->
            {ok, 200, _} = post_spend_tx(R, A, F)
        end,
        maps:get(spend_txs, Txs, [])),
    ok.

%% we don't have any guarantee for the ordering of the txs in the block
equal_block_maps(MapL0, MapR0) ->
    Pop =
      fun(Key, Map0, Default) ->
          Val = maps:get(Key, Map0, Default),
          Map1 = maps:remove(Key, Map0),
          {Val, Map1}
      end,
    {TxsL, MapL1} = Pop(<<"transactions">>, MapL0, []),
    {TxsR, MapR1} = Pop(<<"transactions">>, MapR0, []),
    SortedTxsL = lists:sort(TxsL),
    SortedTxsR = lists:sort(TxsR),
    ct:log("Sorted txs left: ~p", [SortedTxsL]),
    ct:log("Sorted txs right: ~p", [SortedTxsR]),
    MapL1 =:= MapR1 andalso SortedTxsL =:= SortedTxsR. 

