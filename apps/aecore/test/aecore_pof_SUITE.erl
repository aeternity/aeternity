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

all() ->
    [ siblings_on_key_block
    , siblings_on_micro_block
    ].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
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
    MBC = ?config(micro_block_cycle, Config),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:start_node(dev2, Config),

    N1 = aecore_suite_utils:node_name(dev1),
    N2 = aecore_suite_utils:node_name(dev2),

    aecore_suite_utils:connect(N1),
    aecore_suite_utils:connect(N2),

    aecore_suite_utils:mine_key_blocks(N1, 1),

    Account1 = #{ pubkey := PK1 } = new_keypair(),
    Account2 = #{ pubkey := PK2 } = new_keypair(),

    {ok, Tx0a} = add_spend_tx(N1, 100000, 1, 1, 10, patron(), PK1),
    {ok, Tx0b} = add_spend_tx(N1, 100000, 1, 2, 10, patron(), PK2),

    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx0a, Tx0b], 5),

    {ok, _Tx1} = add_spend_tx(N1, 1000, 1,  1,  10, Account1, new_pubkey()),

    {ok, [Key, Micro1 | _]} = aecore_suite_utils:mine_micro_blocks(N1, 1),

    %% Act fraudulent on "behalf" of N1
    timer:sleep(MBC), %% adhere to micro block interval
    {ok, _Tx2} = add_spend_tx(N1, 1000, 1,  1,  10, Account2, new_pubkey()),
    {ok, Micro2, _} =  rpc:call(N1, aec_block_micro_candidate, create, [Key]),
    {ok, Micro2S} = rpc:call(N1, aec_keys, sign_micro_block, [Micro2]),
    rpc:call(N2, aec_conductor, post_block, [Micro2S]),

    %% Add a transaction so there will be a micro block with fraud produced
    {ok, _Tx3} = add_spend_tx(N1, 1000, 1,  3,  10),
    {ok, [_Key3, Micro3 | _]} =  aecore_suite_utils:mine_micro_blocks(N2, 1),
    ct:log("Micro3: ~p", [Micro3]),

    %% Check that the fraud would not be reported again.
    {ok, Micro4, _} =  rpc:call(N2, aec_block_micro_candidate, create, [Micro3]),
    ?assertEqual(no_fraud, aec_blocks:pof(Micro4)),

    Header1 = aec_blocks:to_header(Micro1),
    Header2 = aec_blocks:to_header(Micro2S),

    case aec_blocks:pof(Micro3) of
        no_fraud -> error({expected_fraud, Micro3});
        PoF ->
            ?assertEqual(lists:sort([Header1, Header2]),
                         lists:sort([aec_pof:header1(PoF), aec_pof:header2(PoF)])),
            ?assertEqual(rpc:call(N1, aec_keys, pubkey, []),
                         {ok, aec_pof:pubkey(PoF)})
    end,

    check_post(N1, N2),

    ok.

siblings_on_micro_block(Config) ->
    MBC = ?config(micro_block_cycle, Config),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:start_node(dev2, Config),

    N1 = aecore_suite_utils:node_name(dev1),
    N2 = aecore_suite_utils:node_name(dev2),

    aecore_suite_utils:connect(N1),
    aecore_suite_utils:connect(N2),

    aecore_suite_utils:mine_key_blocks(N1, 1),

    Account1 = #{ pubkey := PK1 } = new_keypair(),
    Account2 = #{ pubkey := PK2 } = new_keypair(),

    {ok, Tx0a} = add_spend_tx(N1, 100000, 1, 1, 10, patron(), PK1),
    {ok, Tx0b} = add_spend_tx(N1, 100000, 1, 2, 10, patron(), PK2),

    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx0a, Tx0b], 5),

    {ok, _Tx1} = add_spend_tx(N1, 1000, 1,  3, 10),

    {ok, [_Key, Micro0 | _]} = aecore_suite_utils:mine_micro_blocks(N1, 1),

    %% Act fraudulent on "behalf" of N1
    timer:sleep(MBC), %% adhere to micro block interval
    {ok, _Tx2} = add_spend_tx(N1, 1000, 1,  1,  10, Account1, new_pubkey()),
    {ok, Micro1, _} =  rpc:call(N1, aec_block_micro_candidate, create, [Micro0]),
    {ok, Micro1S} = rpc:call(N1, aec_keys, sign_micro_block, [Micro1]),
    timer:sleep(MBC), %% adhere to micro block interval
    {ok, _Tx3} = add_spend_tx(N1, 1000, 1,  1,  10, Account2, new_pubkey()),
    {ok, Micro2, _} =  rpc:call(N1, aec_block_micro_candidate, create, [Micro0]),
    {ok, Micro2S} = rpc:call(N1, aec_keys, sign_micro_block, [Micro2]),

    rpc:call(N2, aec_conductor, post_block, [Micro1S]),
    rpc:call(N2, aec_conductor, post_block, [Micro2S]),

    %% Add a transaction so there will be a micro block with fraud produced
    {ok, _Tx4} = add_spend_tx(N1, 1000, 1,  4, 10),
    {ok, [_Key3, Micro3|_]} =  aecore_suite_utils:mine_micro_blocks(N2, 1),
    ct:log("Micro3: ~p", [Micro3]),

    %% Check that the fraud would not be reported again.
    {ok, Micro4, _} =  rpc:call(N2, aec_block_micro_candidate, create, [Micro3]),
    ?assertEqual(no_fraud, aec_blocks:pof(Micro4)),

    Header1 = aec_blocks:to_header(Micro1S),
    Header2 = aec_blocks:to_header(Micro2S),

    case aec_blocks:pof(Micro3) of
        no_fraud -> error({expected_fraud, Micro3});
        PoF ->
            ?assertEqual(lists:sort([Header1, Header2]),
                         lists:sort([aec_pof:header1(PoF), aec_pof:header2(PoF)])),
            ?assertEqual(rpc:call(N1, aec_keys, pubkey, []),
                         {ok, aec_pof:pubkey(PoF)})
    end,

    check_post(N1, N2),

    ok.

check_post(N1, N2) ->
    B1 = <<217,202,108,173,192,99,13,10,129,124,71,86,232,121,148,177,243,254,160,88,174,204,22,114,15,42,51,71,75,19,135,16>>,
    B2 = <<40,25,191,50,209,111,19,239,98,126,125,211,15,133,93,12,13,125,167,137,94,138,27,55,23,50,106,33,28,222,180,102>>,

    %% Check the rewards
    aecore_suite_utils:mine_key_blocks(N2, 2),

    {value, Acc1} = rpc:call(N2, aec_chain, get_account, [B1]),
    {value, Acc2} = rpc:call(N2, aec_chain, get_account, [B2]),
    ct:log("Balances: ~p and ~p", [Acc1, Acc2]),
    Bal1 = aec_accounts:balance(Acc1),
    Bal2 = aec_accounts:balance(Acc2),

    %% Node1 has mined 4 blocks, but should only get 3 rewards (fraud)
    %% Node2 has mined 3 blocks, but should only get 1 reward (delay)
    %%          + 1 fraud reward.
    MR = aec_governance:block_mine_reward(),
    FR = aec_governance:fraud_report_reward(),
    case Bal1 >= 3 * MR andalso Bal1 < 3 * MR + 10 of %% should get some fees
        true -> ok;
        false -> error({bad_balance1, Bal1})
    end,
    case Bal2 >= 1 * MR + FR andalso Bal2 < 1 * MR + FR + 10 of
        true -> ok;
        false -> error({bad_balance2, Bal2})
    end,

    %% Check nodes are in sync
    Top2 = rpc:call(N2, aec_chain, top_block, []),
    aecore_suite_utils:wait_for_height(N1, aec_blocks:height(Top2)),
    Top1 = rpc:call(N1, aec_chain, top_block, []),
    ?assertEqual(Top1, Top2).

%% ============================================================
%% Helpers
%% ============================================================
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

new_keypair() ->
    #{ public := PK, secret := SK } = enacl:sign_keypair(),
    #{ pubkey => PK, privkey => SK }.

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
