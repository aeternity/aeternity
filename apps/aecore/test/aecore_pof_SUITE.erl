-module(aecore_pof_SUITE).

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
   [ siblings_on_key_block/1
   , siblings_on_micro_block/1
   ]).


-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(aecore_suite_utils, [patron/0]).

all() ->
    [ siblings_on_key_block
    , siblings_on_micro_block
    ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    MicroBlockCycle = 100,
    Config1 = [{symlink_name, "latest.pof"},
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
            <<"micro_block_cycle">> => MicroBlockCycle,
            <<"beneficiary_reward_delay">> => 2
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
siblings_on_key_block(Config) ->
    ct:pal("Setting up nodes"),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:start_node(dev2, Config),

    N1 = aecore_suite_utils:node_name(dev1),
    N2 = aecore_suite_utils:node_name(dev2),

    aecore_suite_utils:connect(N1),
    aecore_suite_utils:connect(N2),

    aecore_suite_utils:mock_mempool_nonce_offset(N1, 100),
    aecore_suite_utils:mock_mempool_nonce_offset(N2, 100),

    {ok, _} = aecore_suite_utils:mine_key_blocks(N1, 1),

    Account1 = #{ pubkey := PK1 } = new_keypair(),
    Account2 = #{ pubkey := PK2 } = new_keypair(),

    ct:pal("Setting up accounts"),
    {ok, Tx0a} = add_spend_tx(N1, 1000000, 20000, 1, 10, patron(), PK1),
    {ok, Tx0b} = add_spend_tx(N1, 1000000, 20000, 2, 10, patron(), PK2),

    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx0a, Tx0b], 5),

    {ok, N1Blocks2} = aecore_suite_utils:mine_key_blocks(N1, 1),

    Top = lists:last(N1Blocks2),
    ?assertEqual(key, aec_blocks:type(Top)),
    siblings_common(Top, N1, N2, Account1, Account2, _Fraud = false).


siblings_on_micro_block(Config) ->
    ct:pal("Setting up nodes"),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:start_node(dev2, Config),

    N1 = aecore_suite_utils:node_name(dev1),
    N2 = aecore_suite_utils:node_name(dev2),

    aecore_suite_utils:connect(N1),
    aecore_suite_utils:connect(N2),

    aecore_suite_utils:mock_mempool_nonce_offset(N1, 100),
    aecore_suite_utils:mock_mempool_nonce_offset(N2, 100),

    {ok, _} = aecore_suite_utils:mine_key_blocks(N1, 1),

    Account1 = #{ pubkey := PK1 } = new_keypair(),
    Account2 = #{ pubkey := PK2 } = new_keypair(),
    Account3 = #{ pubkey := PK3 } = new_keypair(),

    ct:pal("Setting up accounts"),
    {ok, Tx0a} = add_spend_tx(N1, 100000, 20000, 1, 10, patron(), PK1),
    {ok, Tx0b} = add_spend_tx(N1, 100000, 20000, 2, 10, patron(), PK2),
    {ok, Tx0c} = add_spend_tx(N1, 100000, 20000, 3, 10, patron(), PK3),

    Txs = [Tx0a, Tx0b, Tx0c],
    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, Txs, 5),

    Top = ensure_top_is_a_micro(N1, Account3, 1),

    ?assertEqual(micro, aec_blocks:type(Top)),

    siblings_common(Top, N1, N2, Account1, Account2, _Fraud = true).

ensure_top_is_a_micro(_Node,_Account, Nonce) when Nonce > 5 ->
    %% We give it five attempts then fail.
    error(could_not_ensure_micro_top);
ensure_top_is_a_micro(Node, Account, Nonce) ->
    %% We want to make a top that is a micro block
    {ok, _Tx1} = add_spend_tx(Node, 1000, 20000,  1,  10, Account, new_pubkey()),
    {ok, _} = aecore_suite_utils:mine_micro_blocks(Node, 1),
    Top = rpc:call(Node, aec_chain, top_block, []),
    case aec_blocks:type(Top) of
        micro -> Top;
        key -> ensure_top_is_a_micro(Node, Account, Nonce + 1)
    end.

siblings_common(TopBlock, N1, N2, Account1, Account2, Fraud) ->
    ct:pal("Waiting for sync"),
    %% Wait until the chain is synced.
    {ok, ExpectedTop1} = aec_blocks:hash_internal_representation(TopBlock),
    0 = get_lock_holder_balance(N1),
    0 = get_lock_holder_balance(N2),
    aec_test_utils:wait_for_it_or_timeout(
      fun() -> rpc:call(N2, aec_chain, top_block_hash, []) end,
      ExpectedTop1,
      10000),

    N1KeyBlocksCount = aec_blocks:height(TopBlock),
    SpendFee = 17000,

    ct:pal("Starting to test fraud"),
    %% Add a transaction and create the first micro block
    {ok, _Tx1} = add_spend_tx(N1, 1000, SpendFee,  1,  10, Account1, new_pubkey()),
    {ok, Micro1, _} =  rpc:call(N1, aec_block_micro_candidate, create, [TopBlock]),
    {ok, Micro1S} = rpc:call(N1, aec_keys, sign_micro_block, [Micro1]),

    %% Add another transaction and create the second micro block
    {ok, _Tx2} = add_spend_tx(N1, 1000, SpendFee,  2,  10, Account1, new_pubkey()),
    {ok, Micro2, _} =  rpc:call(N1, aec_block_micro_candidate, create, [TopBlock]),
    {ok, Micro2S} = rpc:call(N1, aec_keys, sign_micro_block, [Micro2]),

    %% Post both blocks to mock that N1 is fraudulent
    ok = rpc:call(N2, aec_conductor, post_block, [Micro1S]),
    ok = rpc:call(N2, aec_conductor, post_block, [Micro2S]),
    FraudHeight = aec_blocks:height(Micro1),

    %% Make N2 mine a key block to start the next generation.
    {ok, [Key2]} = aecore_suite_utils:mine_key_blocks(N2, 1),

    %% Now we need a micro block to report the fraud in.
    {ok, _} = add_spend_tx(N2, 1000, SpendFee,  1,  10, Account2, new_pubkey()),
    {ok, MicroFraud, _} = rpc:call(N2, aec_block_micro_candidate, create, [Key2]),
    {ok, MicroFraudS} = rpc:call(N2, aec_keys, sign_micro_block, [MicroFraud]),

    %% Check that we reported the fraud, and the content of the report.
    case aec_blocks:pof(MicroFraudS) of
        no_fraud -> error({expected_fraud, MicroFraudS});
        PoF ->
            Header1 = aec_blocks:to_header(Micro1S),
            Header2 = aec_blocks:to_header(Micro2S),
            ?assertEqual(lists:sort([Header1, Header2]),
                         lists:sort([aec_pof:header1(PoF), aec_pof:header2(PoF)])),
            ?assertEqual(rpc:call(N1, aec_keys, pubkey, []),
                         {ok, aec_pof:pubkey(PoF)})
    end,

    %% Manually add the report and check that the fraud would not be reported again.
    {ok, _} = add_spend_tx(N2, 1000, SpendFee,  2,  10, Account2, new_pubkey()),
    ok = rpc:call(N2, aec_conductor, post_block, [MicroFraudS]),
    {ok, MicroNoFraud, _} = rpc:call(N2, aec_block_micro_candidate, create, [MicroFraudS]),
    ?assertEqual(no_fraud, aec_blocks:pof(MicroNoFraud)),

    %% Mine some key blocks now to check the rewards
    Delay = rpc:call(N2, aec_governance, beneficiary_reward_delay, []),
    {ok, _} = aecore_suite_utils:mine_key_blocks(N2, Delay),

    {ok, N1MinedBlock} = rpc:call(N2, aec_chain, get_key_block_by_height, [1]),
    Beneficiary1 = aec_blocks:beneficiary(N1MinedBlock),
    {ok, TopKeyBlock} = rpc:call(N2, aec_chain, top_key_block, []),
    N2Height = aec_blocks:height(TopKeyBlock),
    Beneficiary2 = aec_blocks:beneficiary(TopKeyBlock),

    %% Check the rewards
    {value, Acc1} = rpc:call(N2, aec_chain, get_account, [Beneficiary1]),
    {value, Acc2} = rpc:call(N2, aec_chain, get_account, [Beneficiary2]),

    Bal1 = aec_accounts:balance(Acc1),
    Bal2 = aec_accounts:balance(Acc2),

    Reward1 = lists:sum([aec_governance:block_mine_reward(X)
                         || X <- lists:seq(1, N1KeyBlocksCount),
                            X =/= FraudHeight]),
    FraudReward = aec_governance:fraud_report_reward(FraudHeight),
    Reward2 = lists:sum([aec_governance:block_mine_reward(X)
                         || X <- lists:seq(N1KeyBlocksCount + 1, N2Height - Delay),
                            X =/= FraudHeight]) + FraudReward,

    case Bal1 >= Reward1 andalso Bal1 < Reward1 + 100000 of %% should get some fees
        true -> ok;
        false -> error({bad_balance1, Bal1})
    end,
    case Bal2 >= Reward2 andalso Bal2 < Reward2 + 100000 of
        true -> ok;
        false -> error({bad_balance2, Bal2})
    end,

    %% Check nodes are in sync
    {ok, _} =
        aec_test_utils:wait_for_pred_or_timeout(
            fun() -> {rpc:call(N1, aec_chain, top_block, []),
                      rpc:call(N2, aec_chain, top_block, [])} end,
            fun({Top1, Top2}) -> Top1 =:= Top2 end,
            10000),

    Locked = get_lock_holder_balance(N1),
    {Locked, Locked} = {Locked, get_lock_holder_balance(N2)},
    MR = aec_governance:block_mine_reward(FraudHeight),
    {true, _, _} = {Locked >= MR - FraudReward, Locked, {MR, FraudReward}},% some fees
    {true, _, _} = {Locked =< MR, Locked, MR}.


%% ============================================================
%% Helpers
%% ============================================================
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

new_keypair() ->
    #{ public := PK, secret := SK } = enacl:sign_keypair(),
    #{ pubkey => PK, privkey => SK }.

new_pubkey() ->
    #{ public := PubKey } = enacl:sign_keypair(),
    PubKey.

get_lock_holder_balance(N) ->
    HolderPubKey = aec_governance:locked_coins_holder_account(),
    case rpc:call(N, aec_chain, get_account, [HolderPubKey]) of
        {value, Acc} ->
            _Bal = aec_accounts:balance(Acc);
        none -> 0
    end.
