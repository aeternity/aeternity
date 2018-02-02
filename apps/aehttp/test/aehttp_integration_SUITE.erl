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
    not_blocked_ping/1,
    auto_unblocked_peer/1,
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
    block_txs_list_by_hash_invalid_range/1,

    naming_system_manage_name/1,
    naming_system_broken_txs/1,

    balance/1,
    balance_negative_cases/1
   ]).

%% internal endpoints
-export(
   [ws_get_genesis/1,
    ws_block_mined/1,
    ws_refused_on_limit_reached/1,
    ws_oracles/1
   ]).

-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(WS, aehttp_ws_test_utils).

all() ->
    [
     {group, all_endpoints}
    ].

groups() ->
    [
     {all_endpoints, [sequence], [{group, external_endpoints},
                                  {group, internal_endpoints},
                                  {group, websocket},
                                  {group, naming}
                                  ]},
     {external_endpoints, [sequence], 
      [
        % pings
        broken_pings,
        blocked_ping,
        not_blocked_ping,
        auto_unblocked_peer,
        correct_ping,

        % get block-s
        get_top_empty_chain,
        get_top_non_empty_chain,
        block_by_height,
        block_not_found_by_height,
        block_by_hash,
        block_not_found_by_broken_hash,
        block_not_found_by_hash,

        % sync gossip 
        pending_transactions,
        post_correct_blocks,
        post_broken_blocks,
        post_correct_tx,
        post_broken_tx,
        post_broken_base58_tx,

        % balances
        all_accounts_balances,
        all_accounts_balances_empty,
        all_accounts_balances_disabled,

        % infos
        version,
        info_disabled,
        info_empty,
        info_more_than_30,
        info_less_than_30
      ]},
     {internal_endpoints, [sequence], 
      [
        broken_spend_tx,
        naming_system_broken_txs,
        miner_pub_key,

        %% requested Endpoints
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
        block_txs_list_by_hash_invalid_range,

        naming_system_manage_name,

        balance,
        balance_negative_cases
      ]},
     {naming, [sequence], [naming_system_manage_name]},
     {websocket, [sequence],
      [ws_get_genesis,
       ws_block_mined,
       ws_refused_on_limit_reached,
       ws_oracles
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
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]} | Config1].

end_per_suite(_Config) ->
    ok.

init_per_group(all_endpoints, Config) ->
    Config;
init_per_group(_Group, Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    Config.

end_per_group(all_endpoints, _Config) ->
    ok;
end_per_group(_Group, Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    ok.

init_per_testcase(_Case, Config) ->
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
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
    Peer = unique_peer(),
    {ok, 200, _} =
        post_ping(
          maps:put(<<"source">>, Peer,
                   PingObj#{<<"genesis_hash">> => EncGHash,
                            <<"best_hash">> => EncTopHash})),
    rpc(aec_peers, remove, [Peer]),
    ok.

broken_pings(_Config) ->
    % no 'source'
    {ok, 400, _} = post_ping(#{}),
    % no ping obj data
    Peer = unique_peer(),
    {ok, 400, _} = post_ping(#{<<"source">> => Peer}),
    PingObj = rpc(aec_sync, local_ping_object, []),
    % wrong genesis hash
    Peer1 = unique_peer(),
    WrongGenHashPing = maps:merge(PingObj, #{<<"source">> => Peer1,
                                             <<"genesis_hash">> => <<"foo">>}),
    {ok, 409, #{<<"reason">> := <<"Different genesis blocks">>}} =
        post_ping(WrongGenHashPing),
    rpc(aec_peers, remove, [Peer]),
    rpc(aec_peers, remove, [Peer1]),
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
    rpc(aec_peers, remove, [Peer]),
    ok.

not_blocked_ping(_Config) ->
    %% Use one of the "trusted" peers, it shouldn't get blocked!
    Peer = <<"http://localhost:3023">>,
    PingObj = rpc(aec_sync, local_ping_object, []),
    WrongGenHashPing = maps:merge(PingObj, #{<<"source">> => Peer,
                                             <<"genesis_hash">> => <<"foo">>}),
    {ok, 409, _} = post_ping(WrongGenHashPing),
    % node shouldn't be blocked despite sending garbage...
    {ok, 409, #{<<"reason">> := <<"Different genesis blocks">>}} =
        post_ping(WrongGenHashPing),
    rpc(aec_peers, remove, [Peer]),
    ok.

auto_unblocked_peer(_Config) ->
    rpc(application, set_env, [aecore, peer_unblock_interval, 3000]),
    Peer = unique_peer(),
    PingObj = rpc(aec_sync, local_ping_object, []),
    WrongGenHashPing = maps:merge(PingObj, #{<<"source">> => Peer,
                                             <<"genesis_hash">> => <<"foo">>}),
    %% Reset the blocking
    rpc(aec_peers, unblock_all, []),
    {ok, 409, #{<<"reason">> := <<"Different genesis blocks">>}} =
        post_ping(WrongGenHashPing),

    % After 1s the peer should still be blocked.
    timer:sleep(1000),
    {ok, 403, #{<<"reason">> := <<"Not allowed">>}} =
        post_ping(WrongGenHashPing),

    %% Nodes should be unblocked after 3s
    timer:sleep(2000),
    {ok, 409, #{<<"reason">> := <<"Different genesis blocks">>}} =
        post_ping(WrongGenHashPing),

    rpc(aec_peers, remove, [Peer]),
    rpc(application, unset_env, [aecore, peer_unblock_interval]),
    ok.


get_top_empty_chain(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
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
    InitialHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    BlocksToMine = max(BlocksToCheck - InitialHeight, 0),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    TopHeader = rpc(aec_conductor, top_header, []), 
    %% assert there are enough blocks
    case aec_headers:height(TopHeader) of
        TopHeaderHeight when TopHeaderHeight >= BlocksToCheck ->
          pass
    end,
    % fetch and validate blocks
    lists:foreach(
        fun(Height) ->
            {ok, ExpectedBlock} = rpc(aec_conductor, get_block_by_height, [Height]),
            ExpectedBlockMap = block_to_endpoint_gossip_map(ExpectedBlock), 
            {ok, 200, BlockMap} = get_block_by_height(Height),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            BlockMap = ExpectedBlockMap,
            #{<<"height">> := Height} = BlockMap
        end,
        lists:seq(0, BlocksToCheck)), % from genesis
    ok.

block_not_found_by_height(_Config) ->
    InitialHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    % ensure no mining (and thus - no new blocks)
    {error, not_mining}  = rpc(aec_conductor, get_block_candidate, []),
    NumberOfChecks = ?DEFAULT_TESTS_COUNT,
    lists:foreach(
        fun(_) ->
            Height = rand:uniform(99) + 1 + InitialHeight, % random number 1-100 + CurrentTopHeight
            {ok, 404, #{<<"reason">> := <<"Chain too short">>}} = get_block_by_height(Height)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_not_found_by_hash(_Config) ->
    NumberOfChecks = ?DEFAULT_TESTS_COUNT,
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
    NumberOfChecks = ?DEFAULT_TESTS_COUNT,
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(block_hash, random_hash()),
            {ok, 400, #{<<"reason">> := <<"Invalid hash">>}} = get_block_by_hash(BrokenHash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

block_by_hash(_Config) ->
    BlocksToCheck = 4,
    InitialHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    BlocksToMine = max(BlocksToCheck - InitialHeight, 0),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    TopHeader = rpc(aec_conductor, top_header, []), 
    %% assert there are enough blocks
    case aec_headers:height(TopHeader) of
        TopHeaderHeight when TopHeaderHeight >= BlocksToCheck ->
          pass
    end,
    % fetch and validate blocks
    lists:foreach(
        fun(Height) ->
            {ok, ExpectedBlock} = rpc(aec_conductor, get_block_by_height, [Height]),
            {ok, H} = aec_blocks:hash_internal_representation(ExpectedBlock),
            Hash = aec_base58c:encode(block_hash, H),
            ExpectedBlockMap = block_to_endpoint_gossip_map(ExpectedBlock), 
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
    InitialBalance =
        case get_balance_at_top() of
            {ok, 404, #{<<"reason">> := <<"Account not found">>}} -> 0;
            {ok, 200, #{<<"balance">> := Bal00}} -> Bal00
        end,
    AmountToSpent = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(AmountToSpent, 1),
    MineReward = rpc(aec_governance, block_mine_reward, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),  

    Bal0 = InitialBalance + BlocksToMine * MineReward,
    true = (is_integer(Bal0) andalso Bal0 > AmountToSpent + Fee),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % still empty 
    {ok, 200, []} = get_transactions(), 

    %{ok, SenderPubKey} = rpc:call(?NODE, aec_keys, pubkey, [], 5000),
    ReceiverPubKey = random_hash(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance_at_top(aec_base58c:encode(account_pubkey, ReceiverPubKey)),

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

    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),  
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance_at_top(aec_base58c:encode(account_pubkey, ReceiverPubKey)),


    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty again
    {ok, 200, []} = get_transactions(), 

    {ok, 200, #{<<"balance">> := Bal1}} = get_balance_at_top(),  
    Bal1 = Bal0 + MineReward + Fee - AmountToSpent - Fee,
    {ok, 200, #{<<"balance">> := AmountToSpent}} = 
                 get_balance_at_top(aec_base58c:encode(account_pubkey, ReceiverPubKey)),
    ok.

post_correct_blocks(_Config) ->
    ok = rpc(aec_conductor, stop_mining, []),
    ok = rpc(aec_conductor, reinit_chain, []),
    timer:sleep(100),
    BlocksToPost = ?DEFAULT_TESTS_COUNT,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPost),
    Blocks =
        lists:map(
            fun(Height) ->
                {ok, B} = rpc(aec_conductor, get_block_by_height, [Height]),
                B
            end,
            lists:seq(1, BlocksToPost)),
    ok = rpc(aec_conductor, reinit_chain, []),
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
    Amount = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, 1),
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
    Amount = 1,
    FieldsToTest = 
        [<<"type">>,
         <<"vsn">>,
         <<"sender">>,
         <<"recipient">>,
         <<"amount">>,
         <<"fee">>,
         <<"nonce">>],
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, length(FieldsToTest)),
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
        FieldsToTest),
    ok.

post_broken_base58_tx(_Config) ->
    Amount = 1,
    NumberOfChecks = ?DEFAULT_TESTS_COUNT,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, NumberOfChecks),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
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

all_accounts_balances(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    GenesisAccounts = rpc(aec_genesis_block_settings, preset_accounts, []),
    Receivers = ?DEFAULT_TESTS_COUNT,
    AmountToSpent = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(AmountToSpent, Receivers),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ReceiversAccounts = [{random_hash(), AmountToSpent} || _Idx <- lists:seq(1, Receivers)],
    lists:foreach(
        fun({ReceiverPubKey, _}) ->
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
    ok = rpc(aec_conductor, reinit_chain, []),
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
    ok = rpc(aec_conductor, reinit_chain, []),
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
    ok = rpc(aec_conductor, reinit_chain, []),
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    ok = rpc(application, set_env, [aecore, expected_mine_rate, 100]),
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
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance_at_top(),
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
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    ok.

internal_block_by_height(_Config) ->
    GetExpectedBlockFun =
        fun(H) -> rpc(aec_conductor, get_block_by_height, [H]) end,
    CallApiFun = fun get_internal_block_by_height/2,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

internal_block_not_found_by_height(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    lists:foreach(
        fun(H) ->
            lists:foreach(
                fun(Opt) ->
                    {ok, 404, #{<<"reason">> := <<"Chain too short">>}}
                        = get_internal_block_by_height(H, Opt)
                end,
                [default, message_pack, json])
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
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
                [default, message_pack, json])
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
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
                [default, message_pack, json])
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
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
        [default, message_pack, json]),
    ok = rpc(application, set_env, [aecore, expected_mine_rate,
                                    60 * 60 * 1000]), % aim at one block an hour
    add_spend_txs(),
    rpc(aec_conductor, start_mining, []),
    timer:sleep(100),% so the miner is started
    {ok, PendingBlock} = get_pending_block(),
    ExpectedPendingTx = maps:put(<<"data_schema">>,
                <<"BlockWithMsgPackTxs">>,
                block_to_endpoint_map(PendingBlock)),
    ct:log("Expected pending block ~p", [ExpectedPendingTx]),
    GetPending =
        fun(Opt)->
            aec_test_utils:exec_with_timeout(
               fun TryGetting() ->
                   case get_internal_block_preset("pending", Opt) of
                      {ok, 200, B} -> B;
                      {ok, 404, _} ->
                          timer:sleep(100),
                          TryGetting()
                  end
               end,
               10000)
        end,
    {ok, PendingTxDefault} = GetPending(default),
    {ok, PendingTxHashes} = GetPending(message_pack),
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
                <<"BlockWithJSONTxs">>,
                block_to_endpoint_map(PendingBlock, #{tx_encoding => json})),
    ct:log("Expected pending block with tx objects~p",
           [ExpectedPendingTxsObjects]),
    {ok, PendingTxObjects} = GetPending(json),
    
    % no block should have been mined, so the same prev_hash
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxObjects, <<"prev_hash">>),
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxObjects, <<"data_schema">>),
    rpc(aec_conductor, stop_mining, []),
    ok.

internal_get_block_generic(GetExpectedBlockFun, CallApiFun) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    BlocksToCheck = 4,
    lists:foreach(
        fun(Height) ->
            {ok, 200, #{<<"height">> := Height}} = get_top(),
            {ok, ExpectedBlock} = GetExpectedBlockFun(Height),
            Specific =
                fun(DataSchema) ->
                    {ok, Hash} =
                        aec_blocks:hash_internal_representation(ExpectedBlock),
                    #{<<"data_schema">> => DataSchema,
                      <<"hash">> => aec_base58c:encode(block_hash, Hash)}
                end,
            ExpectedBlockMap = maps:merge(Specific(<<"BlockWithMsgPackTxs">>),
                block_to_endpoint_map(ExpectedBlock)),
            {ok, 200, BlockMap} = CallApiFun(Height, default),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            {ok, 200, BlockMap1} = CallApiFun(Height, message_pack),
            true = equal_block_maps(BlockMap, ExpectedBlockMap), 
            true = equal_block_maps(BlockMap1, ExpectedBlockMap), 

            ExpectedBlockMapTxsObjects = maps:merge(Specific(<<"BlockWithJSONTxs">>),
                block_to_endpoint_map(ExpectedBlock, #{tx_encoding => json})),
            {ok, 200, BlockMap2} = CallApiFun(Height, json),
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
    ok = rpc(application, set_env, [aecore, expected_mine_rate,
                                    60 * 60 * 1000]), % aim at one block an hour
    InsertedTxsCount = add_spend_txs(),
    rpc(aec_conductor, start_mining, []),
    GetPending =
        fun()->
            aec_test_utils:exec_with_timeout(
               fun TryGetting() ->
                  case get_block_txs_count_preset("pending") of
                      {ok, 200, B} -> B;
                      {ok, 404, _} ->
                          timer:sleep(100),
                          TryGetting()
                  end
               end,
               10000)
        end,
    {ok, #{<<"count">> := TxsCount}} = GetPending(),
    ct:log("Inserted transactions count ~p, transactions count in the pending block ~p",
           [InsertedTxsCount, TxsCount]),
    % the assert bellow rellies on no block being mined durring the test run
    % this is achieved by mining BlocksToPremine number of blocks and setting
    % a high value for expected_mine_rate
    true = TxsCount =:= InsertedTxsCount + 1,
    rpc(aec_conductor, start_mining, []),
    ok.

generic_counts_test(GetBlock, CallApi) ->
    BlocksToMine = 5,
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
    InitialHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    lists:foreach(
        fun(H) ->
            {ok, 404, #{<<"reason">> := <<"Chain too short">>}}
                        = get_block_txs_count_by_height(H)
        end,
        lists:seq(InitialHeight + 1, InitialHeight + ?DEFAULT_TESTS_COUNT)),
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
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    ok.

block_txs_count_by_broken_hash(_Config) ->
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(block_hash, random_hash()),
            {ok, 400, #{<<"reason">> := <<"Invalid hash">>}} =
                get_block_txs_count_by_hash(BrokenHash)
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
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
    InitialHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    RandomHeight = InitialHeight + rand:uniform(999) + 1, % CurrentTop + 1..1000
    Test =
        fun(Code, ErrMsg, Fun, Cases) ->
            lists:foreach(
                fun({H, I}) ->
                    lists:foreach(
                        fun(Opt) ->
                            {ok, Code, #{<<"reason">> := ErrMsg}} = Fun(H, I, Opt) end,
                        [default, message_pack, json])
                end,
                Cases)
        end,
    Test(404, <<"Chain too short">>, fun get_block_tx_by_index_height/3,
         [{RandomHeight, 0},
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
    ok = rpc(aec_conductor, reinit_chain, []),
    BlocksToMine = ?DEFAULT_TESTS_COUNT,
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
                            ct:log("Index: ~p, Transaction: ~p", [Index, Tx]),
                            {ok, 200, #{<<"data_schema">> := DataSchema,
                                        <<"transaction">> := Tx}} = CallApi(Height,
                                                                            Index,
                                                                            Opts)
                        end,
                        lists:zip(AllTxs, TxsIdxs))
                end,
                [{default, <<"SingleTxMsgPack">>},
                 {message_pack,  <<"SingleTxMsgPack">>},
                 {json,  <<"SingleTxJSON">>}]),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            add_spend_txs()
        end,
        lists:seq(0, BlocksToMine)), % from genesis
    ok.

block_txs_list_by_height(_Config) ->
    generic_range_test(fun get_block_txs_list_by_height/4,
                        fun(H) -> H end).

block_txs_list_by_hash(_Config) ->
    generic_range_test(fun get_block_txs_list_by_hash/4,
                        fun(H) ->
                            {ok, Hash} = block_hash_by_height(H),
                            Hash
                        end).

generic_range_test(GetTxsApi, HeightToKey) ->
    MaximumRange = rpc(aec_conductor, max_block_range, []),
    CurrentHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    BlocksToMine = max(2 * MaximumRange - CurrentHeight, 0),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    lists:foreach(
        fun(From) ->
            lists:foreach(
                fun(Length) ->
                    single_range_test(From, From + Length,
                                       GetTxsApi, HeightToKey)
                end,
                [0, 1, MaximumRange - 1, MaximumRange])
        end,
        [0, 1, MaximumRange - 1, MaximumRange]),
    ok.

single_range_test(HeightFrom, HeightTo, GetTxsApi, HeightToKey) ->
    TxEncoding = [default, message_pack, json],
    AllTestedTxTypes = [[<<"coinbase">>], [<<"coinbase">>, <<"spend">>]],
    lists:foreach(
        fun({Encoding, TxTypes}) ->
            From = HeightToKey(HeightFrom),
            To = HeightToKey(HeightTo),

            % all transactions
            {ok, 200, Result} = GetTxsApi(From, To, Encoding, #{}),
            ExpectedResult =
                expected_range_result(HeightFrom, HeightTo, Encoding, all),
            ct:log("Expected Result ~p, Actual result ~p",
                    [ExpectedResult, Result]),
            ExpectedResult = Result,

            
            {ok, 200, Result1} = GetTxsApi(From, To, Encoding, #{include => TxTypes}),
            ExpectedResult1 =
                expected_range_result(HeightFrom, HeightTo, Encoding, {only, TxTypes}),
            ct:log("Showing only ~p, Expected Result ~p,~n Actual result ~p",
                    [TxTypes, ExpectedResult1, Result1]),
            ExpectedResult1 = Result1,

            {ok, 200, Result2} = GetTxsApi(From, To, Encoding, #{exclude => TxTypes}),
            ExpectedResult2 =
                expected_range_result(HeightFrom, HeightTo, Encoding, {exclude, TxTypes}),
            ct:log("Showing excluded ~p, Expected Result ~p,~n Actual result ~p",
                    [TxTypes, ExpectedResult2, Result2]),
            ExpectedResult2 = Result2,
            ok
        end,
        [{E, TxT} || E <- TxEncoding, TxT <- AllTestedTxTypes]).

expected_range_result(HeightFrom, HeightTo, TxEncoding0, TxTypes) ->
    {ok, Blocks} = rpc(aec_conductor, get_block_range_by_height, [HeightFrom,
                                                                  HeightTo]),
    TxEncoding =
        case TxEncoding0 of
            default -> message_pack;
            Other -> Other
        end,
    SerializedTxs =
        lists:foldl(
            fun(Block, Accum) ->
                AllBlockTxs = aec_blocks:txs(Block),
                FilteredTxs =
                    case TxTypes of
                        all ->
                            AllBlockTxs;
                        {only, Includes} ->
                            lists:filter(
                                fun(Tx) -> lists:member(tx_type(Tx), Includes) end,
                                AllBlockTxs);
                        {exclude, Excludes} ->
                            lists:filter(
                                fun(Tx) -> not lists:member(tx_type(Tx), Excludes) end,
                                AllBlockTxs)
                      end,
                H = aec_blocks:to_header(Block),
                lists:map(
                    fun(Tx) ->
                        aec_tx_sign:serialize_for_client(TxEncoding, H, Tx)
                    end,
                    FilteredTxs) ++ Accum
            end,
            [],
            Blocks),
    DataSchema =
        case TxEncoding of
            default -> 
                <<"MsgPackTxs">>;
            message_pack ->
                <<"MsgPackTxs">>;
            json ->
                <<"JSONTxs">>
          end,
    #{<<"data_schema">> => DataSchema,
      <<"transactions">> => SerializedTxs}.

tx_type(SignedTx) ->
    Tx = aec_tx_sign:data(SignedTx), 
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:type().

block_txs_list_by_height_invalid_range(_Config) ->
    InitialHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    MaximumRange = rpc(aec_conductor, max_block_range, []),
    BlocksToMine = max(2 * MaximumRange - InitialHeight, 0),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ValidateError =
        fun(From, To, Error) ->
            lists:foreach(
                fun(Opts) ->
                    Error = get_block_txs_list_by_height(From, To, Opts, #{})
                end,
                [default, message_pack, json])
        end,
    InvalidRange = {ok, 400, #{<<"reason">> => <<"From's height is bigger than To's">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, InvalidRange) end,
        [{1, 0}, {3, 1}]),
    RangeTooBig = {ok, 400, #{<<"reason">> => <<"Range too big">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, RangeTooBig) end,
        [{0, MaximumRange +1}, {1, MaximumRange + 2}]),
    ChainTooShort = {ok, 404, #{<<"reason">> => <<"Chain too short">>}},
    CurrentHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, ChainTooShort) end,
        [{CurrentHeight - 1, CurrentHeight + 1},
         {CurrentHeight, CurrentHeight + 1},
         {CurrentHeight + 1, CurrentHeight + 1}]),
    ok.

block_txs_list_by_hash_invalid_range(_Config) ->
    InitialHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    MaximumRange = rpc(aec_conductor, max_block_range, []),
    BlocksToMine = 2 * MaximumRange,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ValidateError =
        fun(From, To, Error) ->
            lists:foreach(
                fun(Opts) ->
                    Error = get_block_txs_list_by_hash(From, To, Opts, #{})
                end,
                [default, message_pack, json])
        end,
    {ok, GenBlockHash} = block_hash_by_height(0),
    {ok, Block1Hash} = block_hash_by_height(InitialHeight),
    {ok, Block2Hash} = block_hash_by_height(InitialHeight + 1),
    {ok, BlockX1Hash} = block_hash_by_height(InitialHeight + MaximumRange + 1),
    {ok, BlockX2Hash} = block_hash_by_height(InitialHeight + MaximumRange + 2),
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

naming_system_manage_name(_Config) ->

    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    PubKeyEnc = aec_base58c:encode(account_pubkey, PubKey),
    Name       = <<"fooo.bar.test">>,
    NameSalt   = 12345,
    NameTTL    = 60000,
    Pointers   = <<"{\"account_pubkey\":\"", PubKeyEnc/binary, "\"}">>,
    TTL        = 10,
    NHash      = aens_hash:name_hash(Name),
    CHash      = aens_hash:commitment_hash(Name, NameSalt),
    Fee        = 2,
    MineReward = rpc(aec_governance, block_mine_reward, []),

    %% Mine 10 blocks to get some funds
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 10),
    {ok, 200, #{<<"balance">> := Balance}} = get_balance_at_top(),

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Submit name preclaim tx and check it is in mempool
    {ok, 200, _}       = post_name_preclaim_tx(CHash, Fee),
    {ok, [PreclaimTx]} = rpc(aec_tx_pool, peek, [infinity]),
    CHash              = aens_preclaim_tx:commitment(aec_tx_sign:data(PreclaimTx)),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check fee taken from account, then mine reward and fee added to account
    {ok, 200, #{<<"balance">> := Balance1}} = get_balance_at_top(),
    Balance1 = Balance - Fee + MineReward + Fee,

    %% Submit name claim tx and check it is in mempool
    {ok, 200, _}    = post_name_claim_tx(Name, NameSalt, Fee),
    {ok, [ClaimTx]} = rpc(aec_tx_pool, peek, [infinity]),
    Name            = aens_claim_tx:name(aec_tx_sign:data(ClaimTx)),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check tx fee taken from account, claim fee burned,
    %% then mine reward and fee added to account
    ClaimBurnedFee = rpc(aec_governance, name_claim_burned_fee, []),
    {ok, 200, #{<<"balance">> := Balance2}} = get_balance_at_top(),
    Balance2 = Balance1 - Fee + MineReward + Fee - ClaimBurnedFee,

    %% Check that name entry is present
    {ok, 200, #{<<"name">>     := Name,
                <<"name_ttl">> := 0,
                <<"pointers">> := <<"[]">>}} = get_name(Name),

    %% Submit name updated tx and check it is in mempool
    {ok, 200, _}     = post_name_update_tx(NHash, NameTTL, Pointers, TTL, Fee),
    {ok, [UpdateTx]} = rpc(aec_tx_pool, peek, [infinity]),
    NameTTL          = aens_update_tx:name_ttl(aec_tx_sign:data(UpdateTx)),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check that TTL and pointers got updated in name entry
    {ok, 200, #{<<"name">>     := Name,
                <<"name_ttl">> := NameTTL,
                <<"pointers">> := Pointers}} = get_name(Name),

    {ok, 200, #{<<"balance">> := Balance3}} = get_balance(),
    Host = internal_address(),
    http_request(Host, post, "spend-tx",
        #{recipient_pubkey => Name,
          amount => 77,
          fee => 50}),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% Nothing gets lost as recipient = sender = miner
    %% This tests 'resolve_name' because recipient is expressed by name label
    %% This tests passes with 1 block confirmation due to lack of miner's reward delay
    FinalBalance = Balance3+MineReward,
    {ok, 200, #{<<"balance">> := FinalBalance}} = get_balance(),

    %% Submit name transfer tx and check it is in mempool
    NameRecipient      = random_hash(),
    {ok, 200, _}       = post_name_transfer_tx(NHash, NameRecipient, Fee),
    {ok, [TransferTx]} = rpc(aec_tx_pool, peek, [infinity]),
    NameRecipient      = aens_transfer_tx:recipient_account(aec_tx_sign:data(TransferTx)),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    ok.

naming_system_broken_txs(_Config) ->
    Name     = <<"fooo.test">>,
    NameSalt = 12345,
    NHash    = aens_hash:name_hash(Name),
    CHash    = aens_hash:commitment_hash(Name, NameSalt),
    Fee      = 2,

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Try to submit txs with empty account

    {ok, 404, #{<<"reason">> := <<"No funds in an account">>}} =
        post_name_preclaim_tx(CHash, Fee),
    {ok, 404, #{<<"reason">> := <<"No funds in an account">>}} =
        post_name_claim_tx(Name, NameSalt, Fee),
    {ok, 404, #{<<"reason">> := <<"No funds in an account">>}} =
        post_name_update_tx(NHash, 5, <<"pointers">>, 5, Fee),
    {ok, 404, #{<<"reason">> := <<"No funds in an account">>}} =
        post_name_transfer_tx(NHash, random_hash(), Fee),
    {ok, 404, #{<<"reason">> := <<"No funds in an account">>}} =
        post_name_revoke_tx(NHash, Fee),

    %% Check mempool still empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]).

%% ============================================================
%% Websocket tests 
%% ============================================================

ws_get_genesis(_Config) ->
    {ok, ConnPid} = ws_start_link(),
    ok = ?WS:register_test_for_event(ConnPid, chain, requested_data),
    ?WS:send(ConnPid, chain, get, #{height => 0, type => block}),
    {ok, Payload} = ?WS:wait_for_event(chain, requested_data),
    {ok, Block} = maps:find(<<"block">>, Payload),
    {ok, 200, BlockMap} = get_internal_block_by_height(0, message_pack),
    ExpectedBlockMap = 
        maps:remove(<<"hash">>, maps:remove(<<"data_schema">>, BlockMap)),
    Block = ExpectedBlockMap,
    ok = ?WS:unregister_test_for_event(ConnPid, chain, requested_data),
    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

ws_block_mined(_Config) ->
    {ok, ConnPid} = ws_start_link(),
    ok = ?WS:register_test_for_event(ConnPid, miner, mined_block),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, #{<<"height">> := Height, <<"hash">> := Hash}} = ?WS:wait_for_event(miner, mined_block),
    ok = ?WS:unregister_test_for_event(ConnPid, miner, mined_block),

    ok = ?WS:register_test_for_event(ConnPid, chain, requested_data),
    ?WS:send(ConnPid, chain, get, #{height => Height, type => block}),
    {ok, _} = ?WS:wait_for_event(chain, requested_data),

    ?WS:send(ConnPid, chain, get, #{hash => Hash, type => block}),
    {ok, _} = ?WS:wait_for_event(chain, requested_data),

    ?WS:send(ConnPid, chain, get, #{height => Height, type => header}),
    {ok, _} = ?WS:wait_for_event(chain, requested_data),

    ?WS:send(ConnPid, chain, get, #{hash => Hash, type => header}),
    {ok, _} = ?WS:wait_for_event(chain, requested_data),

    ok = ?WS:unregister_test_for_event(ConnPid, chain, requested_data),
    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

%% Currently the websockets are a queue: they have a maximim amount of
%% acceptors. Every WS trying to connect after all acceptoprs are used
%% goes into a queue. When the queue is full - the node starts rejecting
%% any new incoming WS connections
ws_refused_on_limit_reached(_Config) ->
    %% maximum amount of acceptors
    MaxWsCount = rpc(aeu_env, user_config_or_env,
                          [[<<"websocket">>, <<"internal">>, <<"acceptors">>],
                          aehttp, [internal, websocket, handlers], 10]), 
    %% Maximum WS connections hanging in the queue
    WSQueueSize = rpc(aehttp_app, ws_handlers_queue_max_size, []),
    WSDieTimeout = 1000,
    ct:log("Websocket acceptors: ~p, websocket acceptor's queue size ~p",
           [MaxWsCount, WSQueueSize]),
    %% assert no WS running on the node
    0 = open_websockets_count(), 
    %% start as many WS as needed to consume all acceptors
    WSPids =
        lists:map(
            fun(_) ->
              {ok, ConnPid} = ws_start_link(),
              ConnPid
            end,
            lists:seq(1, MaxWsCount)),
    %% assert expectation for amount of connected WSs
    MaxWsCount = open_websockets_count(), 
    WaitingPids =
        lists:map(
            fun(_) ->
                %% try to connect a WS client; assert it does not connect and
                %% is waiting
                {error, still_connecting, WaitingPid} = ws_start_link(),
                MaxWsCount = open_websockets_count(), 
                WaitingPid
            end,
            lists:seq(1, WSQueueSize)),

    %% Now both the acceptopr pool and queue are full. Try to connect a new WS
    %% client and assert it fails
    {error, rejected} = ws_start_link(),

    %% split currently connected WS clients into two groups: first with as
    %% many as there are WS clients waiting in the queue and all the rest in a
    %% seperate list. The first group would be used for stopping WS clients
    %% one by one while validating that one of the waiting WS clients connects
    %% for each one stopped
    {FirstWSsPids, OtherWSsPids} = lists:split(WSQueueSize, WSPids),
    lists:foreach(
        fun(Pid) ->
            %% stop one
            ?WS:stop(Pid),
            %% another one connects in its place and it doesn't matter which one
            {ok, some_pid} = ?WS:wait_for_connect(some_pid),
            %% total amount of connected WS clients is still the maximum
            MaxWsCount = open_websockets_count()
        end,
        FirstWSsPids),
    %% cleanup
    lists:foreach(fun ?WS:stop/1, OtherWSsPids),
    timer:sleep(100), % wait for all of them to die out
    {ok, WSQueueSize} =
        aec_test_utils:wait_for_it_or_timeout(fun open_websockets_count/0,
                                              WSQueueSize, WSDieTimeout),
    lists:foreach(fun ?WS:stop/1, WaitingPids),
    timer:sleep(100), % wait for all of them to die out
    {ok, 0} =
        aec_test_utils:wait_for_it_or_timeout(fun open_websockets_count/0,
                                              0, WSDieTimeout),
    ok.

ws_oracles(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Mine a block to make sure the Pubkey has some funds!
    ok = ?WS:register_test_for_event(ConnPid, miner, mined_block),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, #{<<"height">> := _Height, <<"hash">> := _Hash}} = ?WS:wait_for_event(miner, mined_block),
    ok = ?WS:unregister_test_for_event(ConnPid, miner, mined_block),

    %% Fetch the pubkey via HTTP
    {ok, 200, #{ <<"pub_key">> := PK }} = get_miner_pub_key(),

    %% Register an oracle
    ok = ?WS:register_test_for_event(ConnPid, oracle, register),
    ?WS:send(ConnPid, oracle, register,
             #{ type => 'OracleRegisterTxObject',
                account => PK,
                query_format => <<"the query spec">>,
                response_format => <<"the response spec">>,
                query_fee => 4,
                ttl => #{ type => delta, value => 500 },
                fee => 5 }
             ),
    {ok, #{<<"result">> := <<"ok">>,
           <<"oracle_id">> := OId }} = ?WS:wait_for_event(oracle, register),
    ok = ?WS:unregister_test_for_event(ConnPid, oracle, register),

    %% Mine a block to get the oracle onto the chain
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% Register for events when the freshly registered oracle is queried!
    ok = ?WS:register_test_for_event(ConnPid, oracle, subscribe),
    ?WS:send(ConnPid, oracle, subscribe,
             #{ type => query,
                oracle_id => OId }),
    {ok, #{<<"result">> := <<"ok">>}} = ?WS:wait_for_event(oracle, subscribe),

    %% Post a query
    ok = ?WS:register_test_for_event(ConnPid, node, new_oracle_query),
    ok = ?WS:register_test_for_event(ConnPid, oracle, query),
    ?WS:send(ConnPid, oracle, query,
             #{ type => 'OracleQueryTxObject',
                oracle_pubkey => OId,
                query_ttl => #{ type => delta, value => 10 },
                response_ttl => #{ type => delta, value => 10 },
                query => <<"How are you doing?">>,
                query_fee => 4,
                fee => 7 }
             ),
    {ok, #{<<"result">> := <<"ok">>,
           <<"query_id">> := QId }} = ?WS:wait_for_event(oracle, query),
    ok = ?WS:unregister_test_for_event(ConnPid, oracle, query),

    %% Mine a block and check that an event is receieved corresponding to
    %% the query.
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    {ok, #{<<"query_id">> := QId }} = ?WS:wait_for_event(node, new_oracle_query),
    ok = ?WS:unregister_test_for_event(ConnPid, node, new_oracle_query),

    %% Subscribe to responses to the query.
    ?WS:send(ConnPid, oracle, subscribe,
             #{ type => response,
                query_id => QId }),
    {ok, #{<<"result">> := <<"ok">>}} = ?WS:wait_for_event(oracle, subscribe),

    %% Post a response to the query
    ok = ?WS:register_test_for_event(ConnPid, node, new_oracle_response),
    ok = ?WS:register_test_for_event(ConnPid, oracle, response),
    ?WS:send(ConnPid, oracle, response,
             #{ type => 'OracleResponseTxObject',
                query_id => QId,
                response => <<"I am fine, thank you!">>,
                fee => 3 }
             ),
    {ok, #{<<"result">> := <<"ok">>,
           <<"query_id">> := QId }} = ?WS:wait_for_event(oracle, response),
    ok = ?WS:unregister_test_for_event(ConnPid, oracle, response),

    %% Finally mine a block and check that an event is received
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, #{<<"query_id">> := QId }} = ?WS:wait_for_event(node, new_oracle_response),
    ok = ?WS:unregister_test_for_event(ConnPid, node, new_oracle_response),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

%% changing of another account's balance is checked in pending_transactions test
balance(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    % height 0, no account
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance_at_top(),
    % get to height 1
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    lists:foreach(
        fun(Height) ->
            % get the balance at height=Height
            {ok, 200, #{<<"balance">> := Bal}} = get_balance_at_top(),
            % mine a block, move the top
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            % same balance at height=Height
            {ok, 200, #{<<"balance">> := Bal}} = get_balance(EncodedPubKey,
                                                             #{height => Height}),
            {ok, HashStr} = block_hash_by_height(Height),
            Hash = list_to_binary(HashStr),
            % same balance by hash
            {ok, 200, #{<<"balance">> := Bal}} = get_balance(EncodedPubKey,
                                                             #{hash => Hash}),  
            % same balance by hash and height
            {ok, 200, #{<<"balance">> := Bal}} = get_balance(EncodedPubKey,
                                                             #{hash => Hash,
                                                               height => Height}),
            ok
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    {ok, Bal} = rpc(aec_mining, get_miner_account_balance, []),
    {ok, 200, #{<<"balance">> := Bal}} = get_balance_at_top(),  
    {ok, 200, #{<<"balance">> := Bal}} = get_balance_at_top(EncodedPubKey),
    ok.

balance_negative_cases(_Config) ->
    MaxHeight = aec_blocks:height(rpc(aec_conductor, top, [])),
    true = MaxHeight > 2,
    % get a random height, where 0 < Height < MaxHeight
    Height = rand:uniform(MaxHeight - 2) + 1,
    {ok, HashStr} = block_hash_by_height(Height),
    BlockHash = list_to_binary(HashStr),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),

    RandAccount = aec_base58c:encode(account_pubkey, random_hash()),
    <<_, BrokenHash/binary>> = RandAccount,
    TestAccHash =
        fun(Code, ErrReson, H) ->
            Res = {ok, Code, #{<<"reason">> => ErrReson}},
            Res = get_balance_at_top(H),
            Res = get_balance(H, #{height => Height}),
            Res = get_balance(H, #{hash => BlockHash}),
            Res = get_balance(H, #{height => Height,
                                   hash => BlockHash})
        end,
    TestAccHash(400, <<"Invalid account hash">>, BrokenHash),
    TestAccHash(404, <<"Account not found">>, RandAccount),

    % block in the future
    {ok, 404, #{<<"reason">> := <<"Block not found">>}} =
              get_balance(EncodedPubKey, #{height => MaxHeight + 1}),
    % block in the future and valid block hash
    {ok, 404, #{<<"reason">> := <<"Block not found">>}} =
              get_balance(EncodedPubKey, #{height => MaxHeight + 1,
                                           hash => BlockHash}),

    % broken block hash
    {ok, 400, #{<<"reason">> := <<"Invalid block hash">>}} =
              get_balance(EncodedPubKey, #{hash => BrokenHash}),
    % broken block hash and valid height
    {ok, 400, #{<<"reason">> := <<"Invalid block hash">>}} =
              get_balance(EncodedPubKey, #{hash => BrokenHash,
                                           height => Height}),
    % blocks mismatch 
    {ok, 400, #{<<"reason">> := <<"Invalid height and hash combination">>}} =
              get_balance(EncodedPubKey, #{hash => BlockHash,
                                           height => Height + 1}),
    {ok, 200, #{<<"balance">> := _}} = 
              get_balance(EncodedPubKey, #{hash => BlockHash,
                                           height => Height}),
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

post_name_preclaim_tx(Commitment, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-preclaim-tx",
                 #{commitment => aec_base58c:encode(commitment, Commitment),
                   fee        => Fee}).

post_name_claim_tx(Name, NameSalt, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-claim-tx",
                 #{name      => Name,
                   name_salt => NameSalt,
                   fee       => Fee}).

post_name_update_tx(NameHash, NameTTL, Pointers, TTL, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-update-tx",
                 #{name_hash => aec_base58c:encode(name, NameHash),
                   name_ttl  => NameTTL,
                   pointers  => Pointers,
                   ttl       => TTL,
                   fee       => Fee}).

post_name_transfer_tx(NameHash, RecipientPubKey, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-transfer-tx",
                 #{name_hash        => aec_base58c:encode(name, NameHash),
                   recipient_pubkey => aec_base58c:encode(account_pubkey, RecipientPubKey),
                   fee              => Fee}).

post_name_revoke_tx(NameHash, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-revoke-tx",
                 #{name_hash => aec_base58c:encode(name, NameHash),
                   fee       => Fee}).

get_name(Name) ->
    Host = external_address(),
    http_request(Host, get, "name", [{name, Name}]).

get_balance_at_top() ->
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    get_balance_at_top(EncodedPubKey).

get_balance_at_top(EncodedPubKey) ->
    get_balance(EncodedPubKey, []).

get_balance(EncodedPubKey, Params) ->
    Host = internal_address(),
    http_request(Host, get, "account/balance/" ++ binary_to_list(EncodedPubKey),
                 Params).

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
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/height/" ++ integer_to_list(Height), Params).

get_internal_block_by_hash(Hash, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/hash/" ++ http_uri:encode(Hash), Params).

get_internal_block_preset(Segment, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/" ++ Segment, Params).

tx_encoding_param(default) -> #{};
tx_encoding_param(json) -> #{tx_encoding => <<"json">>};
tx_encoding_param(message_pack) -> #{tx_encoding => <<"message_pack">>}.

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
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/height/" ++ integer_to_list(Height) ++
                                       "/" ++ integer_to_list(Index), Params).

get_block_tx_by_index_hash(Hash, Index, TxObjects) when is_binary(Hash) ->
    get_block_tx_by_index_hash(binary_to_list(Hash), Index, TxObjects);
get_block_tx_by_index_hash(Hash, Index, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/hash/" ++ http_uri:encode(Hash) ++
                                       "/" ++ integer_to_list(Index), Params).

get_block_tx_by_index_latest(Index, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/latest/" ++ integer_to_list(Index), Params).

get_block_txs_list_by_height(From, To, TxObjects, TxTypes) ->
    Params0 = tx_encoding_param(TxObjects),
    Filter = make_tx_types_filter(TxTypes),
    Params = maps:merge(Params0, maps:merge(Filter, #{from => From, to => To})),
    Host = internal_address(),
    http_request(Host, get, "block/txs/list/height", Params).

get_block_txs_list_by_hash(From, To, TxObjects, TxTypes) ->
    Params0 = tx_encoding_param(TxObjects),
    Filter = make_tx_types_filter(TxTypes),
    Params = maps:merge(Params0, maps:merge(Filter, #{from => From, to => To})),
    Host = internal_address(),
    http_request(Host, get, "block/txs/list/hash", Params).

make_tx_types_filter(Filter) ->
    Includes = maps:get(include, Filter, []),
    Excludes = maps:get(exclude, Filter, []),
    Encode =
        fun(_, [], Res) -> Res;
        (Key, TypesBin, Res) ->
            Types = lists:map(fun binary_to_list/1, TypesBin),
            T = list_to_binary(lists:join(",", Types)),
            maps:put(Key, T, Res)
        end,
    R0 = Encode(tx_types, Includes, #{}),
    R = Encode(exclude_tx_types, Excludes, R0),
    R.

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

ws_host_and_port() ->
    Port = rpc(aeu_env, user_config_or_env, 
              [ [<<"websocket">>, <<"internal">>, <<"port">>],
                aehttp, [internal, websocket, port], 8144]),
    {"127.0.0.1", Port}.

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

block_to_endpoint_gossip_map(Block) ->
    BMap = aec_blocks:serialize_to_map(Block),
    aehttp_dispatch_ext:cleanup_genesis(BMap).

block_to_endpoint_map(Block) ->
    block_to_endpoint_map(Block, #{tx_encoding => message_pack}).

block_to_endpoint_map(Block, Options) ->
    Encoding = maps:get(tx_encoding, Options, message_pack),
    BMap = aec_blocks:serialize_client_readable(Encoding, Block),
    Expected = aehttp_dispatch_ext:cleanup_genesis(BMap),

    %% Validate that all transactions have the correct block height and hash
    ExpectedTxs = maps:get(<<"transactions">>, Expected, []),
    case ExpectedTxs =:= [] of
        true -> 0 = aec_blocks:height(Block); % only allowed for gen block
        false -> pass
    end,
    BlockHeight = aec_blocks:height(Block),
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    lists:foreach(
        fun(EncodedTx) ->
            #{block_hash := TxBlockHash,
              block_height := TxBlockHeight} =
                  aec_tx_sign:meta_data_from_client_serialized(Encoding, EncodedTx), 
            {BlockHeight, TxBlockHeight} = {TxBlockHeight, BlockHeight},
            {BlockHash, TxBlockHash} = {TxBlockHash, BlockHash}
        end,
        ExpectedTxs),
    Expected.

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
    {ok, 200, _} = get_balance_at_top(), % account present
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
    aec_test_utils:exec_with_timeout(
        fun TryGetting() ->
            case rpc(aec_conductor, get_block_candidate, []) of
                {ok, OK} -> OK;
                {error, not_mining} = Err->
                    Err;
                {error, miner_starting} ->
                    timer:sleep(10),
                    TryGetting()
            end
        end,
        10000).

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

minimal_fee_and_blocks_to_mine(Amount, ChecksCnt) ->
    Fee = rpc(aec_governance, minimum_tx_fee, []),
    MineReward = rpc(aec_governance, block_mine_reward, []),
    TokensRequired = (Amount + Fee) * ChecksCnt,
    BlocksToMine = trunc(math:ceil(TokensRequired / MineReward)),
    {BlocksToMine, Fee}.

ws_start_link() ->
    {Host, Port} = ws_host_and_port(),
    ?WS:start_link(Host, Port).

open_websockets_count() ->
    QueueName = ws_handlers_queue,
    % ensure queue exsits
    true = undefined =/= rpc(jobs, queue_info, [QueueName]),
    length([1 || {_, QName} <- rpc(jobs, info, [monitors]),
                 QName =:= QueueName]).
