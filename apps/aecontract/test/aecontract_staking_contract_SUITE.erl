%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc CT test suite for AE consensus Contracts
%%% @end
%%%-------------------------------------------------------------------
-module(aecontract_staking_contract_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ new_validator/1,
          inspect_validator/1,
          setting_online_and_offline/1,
          single_validator_gets_elected_every_time/1,
          three_validators_election/1,
          unstake_balances/1,
          staking_and_unstaking_effects_election/1,
          can_not_unstake_more_shares_than_owned/1,
          change_name_description_avatar/1
        ]).

-export([ can_not_have_two_validators_with_same_id/1,
          delegate_can_support_multiple_validators/1,
          if_unstake_all_delegate_is_deleted/1,
          can_not_become_validator_below_treshold/1,
          can_not_stake_below_treshold/1
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("aecontract.hrl").
-include_lib("aecontract/test/include/fate_type_macros.hrl").
-include_lib("aecore/include/blocks.hrl").

-include("include/aect_sophia_vsn.hrl").

-define(OWNER_PUBKEY, <<42:32/unit:8>>).

-define(GENESIS_HEIGHT, 5).

-define(ALICE, {
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>}).
%% ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm

-define(BOB, {
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>}).
%% ak_nQpnNuBPQwibGpSJmjAah6r3ktAB7pG9JHuaGWHgLKxaKqEvC

-define(CAROL, {
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>}).

-define(STAKING_CONTRACT, "MainStaking").

-define(GAS, 10000000).
-define(GAS_PRICE, aec_test_utils:min_gas_price()).
-define(FEE, 1000000000000).

-define(VALIDATOR_MIN, 10000).
-define(STAKE_MIN, 100).

all() -> [{group, all}
         ].

groups() ->
    [ {all, [sequence],
       [ new_validator,
         inspect_validator,
         setting_online_and_offline,
         single_validator_gets_elected_every_time,
         three_validators_election,
         unstake_balances,
         staking_and_unstaking_effects_election,
         can_not_unstake_more_shares_than_owned,
         change_name_description_avatar,
         can_not_have_two_validators_with_same_id,
         delegate_can_support_multiple_validators,
         if_unstake_all_delegate_is_deleted,
         can_not_become_validator_below_treshold,
         can_not_stake_below_treshold
       ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    case aect_test_utils:latest_protocol_version() of
        PreCeres when PreCeres < ?CERES_PROTOCOL_VSN ->
            {skip, from_ceres_on};
        _ ->
            aect_test_utils:init_per_group(fate, Config, fun(X) -> X end)
    end.

end_per_group(_Grp, Config) ->
    Config.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

new_validator(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    {ok, _Trees1, AliceRes} = new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees0),
    {contract, _AlicePoolAddress} = AliceRes,
    ok.

inspect_validator(_Config) ->
    Trees0 = genesis_trees(),
    ConsensusContractPubkey = consensus_contract_address(),
    Amount = ?VALIDATOR_MIN,
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Alice = pubkey(?ALICE),
    {ok, Trees1, {contract, AliceContract}} = new_validator_(Alice, Amount, TxEnv, Trees0),
    {ok, _, Balance} = balance_(Alice, Alice, TxEnv, Trees1),
    {Balance, Balance} = {Amount, Balance},
    {ok, _, State} = get_validator_state_(Alice, Alice, TxEnv, Trees1),
    {tuple, {{contract, AliceContract}, %% the pool contract
             Balance, %% staker pool balance
             false, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                     _Name, _Description,
                     _AvatarURL, _Map, Balance}}}} = State,
    {ok, _, IsOnline} = is_validator_online_(Alice, Alice, TxEnv, Trees1),
    false = IsOnline,
    ok.

setting_online_and_offline(_Config) ->
    Alice = pubkey(?ALICE),
    Trees0 = genesis_trees(),
    Amount = ?VALIDATOR_MIN,
    AliceStake = {tuple, {{address, Alice}, Amount}},
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    {ok, Trees1, _AliceRes} = new_validator_(pubkey(?ALICE), Amount, TxEnv, Trees0),
    %% a new validator is offline
    {ok, _, false} = is_validator_online_(Alice, Alice, TxEnv, Trees1),
    {revert, <<"Validator not online">>} =
        set_validator_offline_(Alice, TxEnv, Trees1),
    {ok, _, []} = online_validators_(Alice, TxEnv, Trees1),
    {ok, _, [AliceStake]} = offline_validators_(Alice, TxEnv, Trees1),
    %% set it online
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    {ok, _, true} = is_validator_online_(Alice, Alice, TxEnv, Trees2),
    {revert, <<"Validator not offline">>} =
        set_validator_online_(Alice, TxEnv, Trees2),
    {ok, _, [AliceStake]} = online_validators_(Alice, TxEnv, Trees2),
    {ok, _, []} = offline_validators_(Alice, TxEnv, Trees2),
    %% set it back offline
    {ok, Trees3, {tuple, {}}} = set_validator_offline_(Alice, TxEnv, Trees2),
    {ok, _, false} = is_validator_online_(Alice, Alice, TxEnv, Trees3),
    {revert, <<"Validator not online">>} =
        set_validator_offline_(Alice, TxEnv, Trees3),
    {ok, _, []} = online_validators_(Alice, TxEnv, Trees3),
    {ok, _, [AliceStake]} = offline_validators_(Alice, TxEnv, Trees3),
    ok.


single_validator_gets_elected_every_time(_Config) ->
    Alice = pubkey(?ALICE),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    {ok, Trees1, _} = new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    %% no rewards
    lists:foldl(
        fun(Height, TreesAccum) ->
            %% Alice is expected to be next
            TxEnvPrev = aetx_env:set_height(TxEnv, Height - 1),
            {ok, _ , {address, Alice}} = elect_next_(Alice, TxEnvPrev,
                                                     TreesAccum),
            TxEnv1 = aetx_env:set_height(TxEnv, Height),
            {ok, TreesAccum1, {tuple, {}}} = elect_(?OWNER_PUBKEY, TxEnv1, TreesAccum),
            {ok, _, {address, Alice}} = leader_(Alice, TxEnv1, TreesAccum1),
            TreesAccum1
        end,
        Trees2,
        lists:seq(?GENESIS_HEIGHT, 1000)),
    %% with rewards
    lists:foldl(
        fun(Height, TreesAccum) ->
            %% Alice is expected to be next
            TxEnvPrev = aetx_env:set_height(TxEnv, Height - 1),
            {ok, TreesAccum1, {tuple, {}}} = reward_(Alice, 1000, ?OWNER_PUBKEY, TxEnvPrev, TreesAccum),
            {ok, _ , {address, Alice}} = elect_next_(Alice, TxEnvPrev,
                                                     TreesAccum1),
            TxEnv1 = aetx_env:set_height(TxEnv, Height),
            {ok, TreesAccum2, {tuple, {}}} = elect_(?OWNER_PUBKEY, TxEnv1,
                                                    TreesAccum1),
            {ok, _, {address, Alice}} = leader_(Alice, TxEnv1, TreesAccum2),
            TreesAccum2
        end,
        Trees2,
        lists:seq(?GENESIS_HEIGHT, 1000)),
    ok.

three_validators_election(_Config) ->
    %% Alice will have twice the staking power compared to Bob, she shall get
    %% elected twice as often
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Carol = pubkey(?CAROL), %% will be offline
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, 2 * ?VALIDATOR_MIN},
            {Bob, ?VALIDATOR_MIN},
            {Carol, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees2),
    %% no rewards to check probabilities
    {_, Leaders} = test_elect_calls(?GENESIS_HEIGHT, 1000, TxEnv, Trees3),
    #{Alice := AliceTurns, Bob := BobTurns} = Leaders,
    ExpectedAlice = 667,
    {ExpectedAlice, ExpectedAlice} = {ExpectedAlice, AliceTurns},
    ExpectedBob = 334,
    {ExpectedBob, ExpectedBob} = {ExpectedBob, BobTurns},
    ok.


unstake_balances(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, ?VALIDATOR_MIN},
            {Bob, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees2),
    %% a new staker
    #{public := Sam} = enacl:sign_keypair(),
    SamBalance0 = trunc(math:pow(10, 30)),
    Trees4 = set_up_account({Sam, SamBalance0}, Trees3),
    %% let Sam put his stake in Alice's staking pool. He will have as much
    %% staking power as her so he will get 50% of the rewards
    SamBalance0 = account_balance(Sam, Trees4),
    ct:log("Balance before staking ~p", [SamBalance0]),
    {ok, Trees5, {tuple, {}}} =
        stake_(Alice, ?VALIDATOR_MIN, Sam, TxEnv, Trees4),
    SamBalance1 = account_balance(Sam, Trees5),
    ct:log("Balance after staking ~p, expected fees: ~p", [SamBalance1,
                                                           SamBalance0 - SamBalance1 - ?VALIDATOR_MIN]),
    {ok, Trees5, {tuple, {}}} = stake_(Alice, ?VALIDATOR_MIN, Sam, TxEnv, Trees4),
    true = SamBalance0 - SamBalance1 - ?VALIDATOR_MIN > 0, % some gas fees
    ExpectedSamReward = 1000000000000000000 * aec_governance:minimum_gas_price(?CERES_PROTOCOL_VSN), %% more than gas fees :)
    TotalReward = 2 * ExpectedSamReward,
    {ok, Trees6, {tuple, {}}} =
        reward_(Alice, TotalReward, ?OWNER_PUBKEY, TxEnv, Trees5),
    {ok, Trees7, ActualAmt} = unstake_(Alice, ?VALIDATOR_MIN, Sam, TxEnv, Trees6),
    ActualReward = ActualAmt - ?VALIDATOR_MIN,
    {ActualReward, ActualReward} = {ActualReward, ExpectedSamReward},
    ct:log("Withdrawn amount ~p, rewards collected ~p", [ActualAmt, ActualReward]),
    SamBalance2 = account_balance(Sam, Trees7),
    ct:log("Balance after unstaking ~p", [SamBalance2]),
    true = SamBalance2 > SamBalance1,
    true = SamBalance2 - ActualReward < SamBalance1,
    ok.

staking_and_unstaking_effects_election(_Config) ->
    %% Alice will have twice the staking power compared to Bob, she shall get
    %% elected twice as often
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Carol = pubkey(?CAROL), %% will be offline
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, 2 * ?VALIDATOR_MIN},
            {Bob, ?VALIDATOR_MIN},
            {Carol, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees2),
    %% create a lot of stakers with some significant stake
    %% the following would state 100 stakers, half of which would stake to
    %% Alice, the other half - to Bob
    %% if Alice's initial staking power was 2 * X, then Bob's was X, we would
    %% stake X more to each other so Alice would have 3 * X and Bob - 2 * X
    StakersCnt = 100,
    StakerStake = ?VALIDATOR_MIN div 50,
    Stakers =
        lists:map(
            fun(_) ->
                #{public := P} = enacl:sign_keypair(),
                P
            end,
            lists:seq(1, StakersCnt)),
    %% create staker accounts
    Trees4 =
        lists:foldl(
            fun(Pubkey, TreesAccum) ->
                    set_up_account({Pubkey, trunc(math:pow(10, 30))}, TreesAccum)
            end,
            Trees3,
            Stakers),
    AliceStakers = lists:nthtail(StakersCnt div 2, lists:reverse(Stakers)),
    BobStakers = lists:nthtail(StakersCnt div 2, Stakers),
    StakeFun =
        fun(Who, StakersList, StakeTrees0) ->
            {ok, _, Balance0} = balance_(Who, Who, TxEnv, StakeTrees0),
            StakeTrees =
                lists:foldl(
                    fun(Pubkey, TreesAccum) ->
                        {ok, TreesAccum1, {tuple, {}}} = stake_(Who, StakerStake,
                                                                Pubkey, TxEnv,
                                                                TreesAccum),
                        TreesAccum1
                    end,
                    StakeTrees0,
                    StakersList),
            {ok, _, Balance1} = balance_(Who, Who, TxEnv, StakeTrees),
            ExpectedBalance = length(StakersList) * StakerStake + Balance0,
            {ExpectedBalance, ExpectedBalance} = {ExpectedBalance, Balance1},
            StakeTrees
        end,
    Trees5 = StakeFun(Alice, AliceStakers, Trees4),
    Trees6 = StakeFun(Bob, BobStakers, Trees5),
    {_, Leaders} = test_elect_calls(?GENESIS_HEIGHT, 1000, TxEnv, Trees6),
    #{Alice := AliceTurns, Bob := BobTurns} = Leaders,
    ExpectedAlice = 600,
    {ExpectedAlice, ExpectedAlice} = {ExpectedAlice, AliceTurns},
    ExpectedBob = 401,
    {ExpectedBob, ExpectedBob} = {ExpectedBob, BobTurns},

    %% reward both pools by doubling their total amount. This would yeld 100%
    %% RoI for all stakers: at this point withdrawing X stakes would provide
    %% them with 2 * X aettos
    %% NB: we start from Trees6 (just after staking)

    {ok, Trees7, {tuple, {}}} =
        reward_(Alice, ?VALIDATOR_MIN, ?OWNER_PUBKEY, TxEnv, Trees6),
    {ok, _, AliceBalance} = balance_(Alice, Alice, TxEnv, Trees7),
    ExpectedAliceBalance = 4 * ?VALIDATOR_MIN,
    {ExpectedAliceBalance, ExpectedAliceBalance} =
        {ExpectedAliceBalance, AliceBalance},
    {ok, Trees8, {tuple, {}}} =
        reward_(Bob, ?VALIDATOR_MIN, ?OWNER_PUBKEY, TxEnv, Trees7),
    {ok, _, BobBalance} = balance_(Bob, Bob, TxEnv, Trees8),
    ExpectedBobBalance = 3 * ?VALIDATOR_MIN,
    {ExpectedBobBalance, ExpectedBobBalance} =
        {ExpectedBobBalance, BobBalance},
    UnStakeFun =
        fun(Who, StakersList, StakeTrees0) ->
            {ok, _, Balance0} = balance_(Who, Who, TxEnv, StakeTrees0),
            {StakeTrees, TotalWithdrawnAmt} =
                lists:foldl(
                    fun(Pubkey, {TreesAccum, WithdrawnSum}) ->
                        {ok, TreesAccum1, Amt} = unstake_(Who, StakerStake,
                                                          Pubkey, TxEnv,
                                                          TreesAccum),
                        {TreesAccum1, WithdrawnSum + Amt}
                    end,
                    {StakeTrees0, 0},
                    StakersList),
            {ok, _, Balance1} = balance_(Who, Who, TxEnv, StakeTrees),
            ExpectedBalance = Balance0 - TotalWithdrawnAmt,
            {ExpectedBalance, ExpectedBalance} = {ExpectedBalance, Balance1},
            StakeTrees
        end,
    %% unstake all Bob's stakers
    Trees9 = UnStakeFun(Bob, BobStakers, Trees8),
    {_, _Leaders2} = test_elect_calls(?GENESIS_HEIGHT, 1000, TxEnv, Trees9),
    ok.

can_not_unstake_more_shares_than_owned(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, ?VALIDATOR_MIN},
            {Bob, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    #{public := Sam} = enacl:sign_keypair(),
    Trees3 = set_up_account({Sam, trunc(math:pow(10, 30))}, Trees2),
    {ok, Trees4, {tuple, {}}} =
        stake_(Alice, ?STAKE_MIN, Sam, TxEnv, Trees3),
    Test =
        fun(Trees) ->
            %% Bob is offline and can not unstake more than he has
            {revert, <<"Not enough shares">>} =
                unstake_(Bob, ?VALIDATOR_MIN + 1, Bob, TxEnv, Trees),
            %% Alice is online but she still can not unstake more shares than
            %% she has
            {revert, <<"Not enough shares">>} =
                unstake_(Alice, ?VALIDATOR_MIN + 1, Alice, TxEnv, Trees),
            %% Sam has a ?STAKE_MIN stakes in Alice's pool. He can not unstake
            %% more than he has
            {revert, <<"Not enough shares">>} =
                unstake_(Alice, ?STAKE_MIN + 1, Sam, TxEnv, Trees),
            %% Sam has no stake in Bob's pool
            {revert, <<"Not enough shares">>} =
                unstake_(Bob, 1, Sam, TxEnv, Trees)
        end,
    Test(Trees4),
    %% rewards do not change the amount of stakes
    {ok, Trees5, {tuple, {}}} = reward_(Alice, 100000000000000, ?OWNER_PUBKEY, TxEnv, Trees4),
    Test(Trees5),
    {ok, Trees6, {tuple, {}}} = reward_(Bob, 100000000000000, ?OWNER_PUBKEY, TxEnv, Trees5),
    Test(Trees6),
    ok.

change_name_description_avatar(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, ?VALIDATOR_MIN},
            {Bob, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    #{public := Sam} = enacl:sign_keypair(),
    {ok, _, AliceState0} = get_validator_state_(Alice, Alice, TxEnv, Trees2),
    {tuple, {{contract, _}, %% the pool contract
             _, %% staker pool balance
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                     <<"">>, %% name
                     <<"">>, %% description
                     <<"">>, %% avatarURL,
                     _MapA, ?VALIDATOR_MIN}}}} = AliceState0,
    {ok, _, BobState0} = get_validator_state_(Bob, Bob, TxEnv, Trees2),
    {tuple, {{contract, _}, %% the pool contract
             _, %% staker pool balance
             false, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                     <<"">>, %% name
                     <<"">>, %% description
                     <<"">>, %% avatarURL,
                     _MapB, ?VALIDATOR_MIN}}}} = BobState0,
    AliceName = <<"AL1CE">>,
    {ok, Trees3, {tuple, {}}} = set_name_(AliceName, Alice, TxEnv, Trees2),
    {ok, _, AliceState1} = get_validator_state_(Alice, Alice, TxEnv, Trees3),
    {tuple, {{contract, _}, %% the pool contract
             _, %% staker pool balance
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                     AliceName, %% name
                     <<"">>, %% description
                     <<"">>, %% avatarURL,
                     _MapA, ?VALIDATOR_MIN}}}} = AliceState1,
    %% Bob is unchanged
    {ok, _, BobState0} = get_validator_state_(Bob, Bob, TxEnv, Trees3),
    AliceDescription = <<"Who in the world am I?' Ah, that's the great puzzle!">>,
    {ok, Trees4, {tuple, {}}} = set_description_(AliceDescription, Alice, TxEnv, Trees3),
    {ok, _, AliceState2} = get_validator_state_(Alice, Alice, TxEnv, Trees4),
    {tuple, {{contract, _}, %% the pool contract
             _, %% staker pool balance
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                     AliceName, %% name
                     AliceDescription, %% description
                     <<"">>, %% avatarURL,
                     _MapA, ?VALIDATOR_MIN}}}} = AliceState2,
    %% Bob is unchanged
    {ok, _, BobState0} = get_validator_state_(Bob, Bob, TxEnv, Trees4),
    AliceAvatar = <<"test.test/img.jpg">>,
    {ok, Trees5, {tuple, {}}} = set_avatar_(AliceAvatar, Alice, TxEnv, Trees4),
    {ok, _, AliceState3} = get_validator_state_(Alice, Alice, TxEnv, Trees5),
    {tuple, {{contract, _}, %% the pool contract
             _, %% staker pool balance
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                     AliceName, %% name
                     AliceDescription, %% description
                     AliceAvatar, %% avatarURL,
                     _MapA, ?VALIDATOR_MIN}}}} = AliceState3,
    %% Bob is unchanged
    {ok, _, BobState0} = get_validator_state_(Bob, Bob, TxEnv, Trees5),
    %% Sam has no account 
    {error, account_not_found} = set_name_(AliceName, Sam, TxEnv, Trees5),
    {error, account_not_found} = set_description_(AliceDescription, Sam, TxEnv, Trees5),
    {error, account_not_found} = set_avatar_(AliceAvatar, Sam, TxEnv, Trees5),
    %% Sam is not a validator and can not change anything
    Trees6 = set_up_account({Sam, 10000000000000000000000000}, Trees5),
    {revert, <<"Validator must exists">>} = set_name_(AliceName, Sam, TxEnv, Trees6),
    {revert, <<"Validator must exists">>} = set_description_(AliceDescription, Sam, TxEnv, Trees6),
    {revert, <<"Validator must exists">>} = set_avatar_(AliceAvatar, Sam, TxEnv, Trees6),
    ok.

can_not_have_two_validators_with_same_id(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, ?VALIDATOR_MIN},
            {Bob, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    %% registering a validator for the second time fails
    {revert, <<"Validator exists">>} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees2),
    {revert, <<"Validator exists">>} = new_validator_(Bob, ?VALIDATOR_MIN, TxEnv, Trees2),
    ok.

delegate_can_support_multiple_validators(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Carol = pubkey(?CAROL),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, ?VALIDATOR_MIN},
             {Bob, ?VALIDATOR_MIN},
             {Carol, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    %% assert initial balances
    {ok, _, BalanceA0} = balance_(Alice, Alice, TxEnv, Trees2),
    {BalanceA0, BalanceA0} = {BalanceA0, ?VALIDATOR_MIN},
    {ok, _, BalanceB0} = balance_(Bob, Bob, TxEnv, Trees2),
    {BalanceB0, BalanceB0} = {BalanceB0, ?VALIDATOR_MIN},
    {ok, _, BalanceC0} = balance_(Carol, Carol, TxEnv, Trees2),
    {BalanceC0, BalanceC0} = {BalanceC0, ?VALIDATOR_MIN},
    %% create a delegate
    #{public := Sam} = enacl:sign_keypair(),
    SamBalance0 = trunc(math:pow(10, 30)),
    Trees3 = set_up_account({Sam, SamBalance0}, Trees2),
    %% the delegate supports Alice, only her balance changes
    {ok, Trees4, {tuple, {}}} =
        stake_(Alice, ?STAKE_MIN, Sam, TxEnv, Trees3),
    {ok, _, BalanceA1} = balance_(Alice, Alice, TxEnv, Trees4),
    {BalanceA1, BalanceA1} = {BalanceA1, ?VALIDATOR_MIN + ?STAKE_MIN},
    {ok, _, BalanceB0} = balance_(Bob, Bob, TxEnv, Trees4),
    {ok, _, BalanceC0} = balance_(Carol, Carol, TxEnv, Trees4),
    %% the delegate supports Bob, only his balance changes
    {ok, Trees5, {tuple, {}}} =
        stake_(Bob, 5 * ?STAKE_MIN, Sam, TxEnv, Trees4),
    {ok, _, BalanceA1} = balance_(Alice, Alice, TxEnv, Trees5),
    {ok, _, BalanceB1} = balance_(Bob, Bob, TxEnv, Trees5),
    {BalanceB1, BalanceB1} = {BalanceB1, ?VALIDATOR_MIN + 5 * ?STAKE_MIN},
    {ok, _, BalanceC0} = balance_(Carol, Carol, TxEnv, Trees5),
    {ok, Trees6, {tuple, {}}} =
        stake_(Carol, 10 * ?STAKE_MIN, Sam, TxEnv, Trees5),
    {ok, _, BalanceA1} = balance_(Alice, Alice, TxEnv, Trees6),
    {ok, _, BalanceB1} = balance_(Bob, Bob, TxEnv, Trees6),
    {ok, _, BalanceC1} = balance_(Carol, Carol, TxEnv, Trees6),
    {BalanceC1, BalanceC1} = {BalanceC1, ?VALIDATOR_MIN + 10 * ?STAKE_MIN},
    ok.

if_unstake_all_delegate_is_deleted(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, ?VALIDATOR_MIN},
             {Bob, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    %% assert initial balances
    {ok, _, BalanceA0} = balance_(Alice, Alice, TxEnv, Trees2),
    {BalanceA0, BalanceA0} = {BalanceA0, ?VALIDATOR_MIN},
    {ok, _, BalanceB0} = balance_(Bob, Bob, TxEnv, Trees2),
    {BalanceB0, BalanceB0} = {BalanceB0, ?VALIDATOR_MIN},
    %% create a delegate
    #{public := Sam} = enacl:sign_keypair(),
    SamBalance0 = trunc(math:pow(10, 30)),
    Trees3 = set_up_account({Sam, SamBalance0}, Trees2),
    Test =
        fun(ToWhom) ->
            {ok, _, State0} = get_validator_state_(ToWhom, ToWhom, TxEnv, Trees3),
            {tuple, {{contract, _}, %% the pool contract
                    ?VALIDATOR_MIN, %% staker pool balance
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _Name, _Description,
                            _AvatarURL, Map0, Balance0}}}} = State0,
            %% assert balances
            #{{address, ToWhom} := ?VALIDATOR_MIN} = Map0,
            1 = maps:size(Map0), %% no other keys
            TotalStakedAmt = ?STAKE_MIN + 10,
            TotalAmt = ?VALIDATOR_MIN + TotalStakedAmt,
            {ok, Trees4, {tuple, {}}} =
                stake_(ToWhom, TotalStakedAmt, Sam, TxEnv, Trees3),
            {ok, _, State1} = get_validator_state_(ToWhom, ToWhom, TxEnv,
                                                   Trees4),
            {tuple, {{contract, _}, %% the pool contract
                    TotalAmt, %% staker pool balance
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _Name, _Description,
                            _AvatarURL, Map1, Balance1}}}} = State1,
            {Balance1, Balance1} = {Balance1, Balance0 + TotalStakedAmt},
            #{{address, ToWhom} := ?VALIDATOR_MIN,
              {address, Sam} := TotalStakedAmt } = Map1,
            2 = maps:size(Map1), %% no other keys
            %% withdraw some shares and assert balances
            WithdrawnAmt = 10,
            StakeLeft = TotalStakedAmt - WithdrawnAmt,
            PoolStakeLeft = TotalAmt - WithdrawnAmt,
            {ok, Trees5, WithdrawnAmt} =
                unstake_(ToWhom, WithdrawnAmt, Sam, TxEnv, Trees4),
            {ok, _, State2} = get_validator_state_(ToWhom, ToWhom, TxEnv,
                                                   Trees5),
            {tuple, {{contract, _}, %% the pool contract
                    PoolStakeLeft, %% staker pool balance
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _Name, _Description,
                            _AvatarURL, Map2, Balance2}}}} = State2,
            {Balance2, Balance2} = {Balance2, Balance1 - WithdrawnAmt},
            #{{address, ToWhom} := ?VALIDATOR_MIN,
              {address, Sam} := StakeLeft } = Map2,
            2 = maps:size(Map2), %% no other keys
            %% withdraw what is left, the delegate is being deleted
            {ok, Trees6, StakeLeft} =
                unstake_(ToWhom, StakeLeft, Sam, TxEnv, Trees5),
            {ok, _, State3} = get_validator_state_(ToWhom, ToWhom, TxEnv,
                                                   Trees6),
            {tuple, {{contract, _}, %% the pool contract
                    ?VALIDATOR_MIN, %% staker pool balance
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _Name, _Description,
                            _AvatarURL, Map3, Balance3}}}} = State3,
                    {Balance3, Balance3} = {Balance3, Balance0},
            #{{address, ToWhom} := ?VALIDATOR_MIN} = Map3,
            1 = maps:size(Map3) %% no other keys
        end,
    Test(Alice), %% online
    Test(Bob), %% offline
    ok.

can_not_become_validator_below_treshold(_Config) ->
    Alice = pubkey(?ALICE),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    {revert, <<"A new validator stake the minimum amount">>}
        = new_validator_(Alice, ?VALIDATOR_MIN - 1, TxEnv, Trees0),
    ok.

can_not_stake_below_treshold(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv,
                                                      TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, ?VALIDATOR_MIN},
             {Bob, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    %% create a delegate
    #{public := Sam} = enacl:sign_keypair(),
    SamBalance0 = trunc(math:pow(10, 30)),
    Trees3 = set_up_account({Sam, SamBalance0}, Trees2),
    {revert, <<"Must stake the minimum amount">>} =
        stake_(Alice, ?STAKE_MIN - 1, Sam, TxEnv, Trees3),
    ok.

genesis_trees() ->
    Trees0 = aec_trees:new_without_backend(),
    Trees1 = set_up_accounts(Trees0),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    {ok, SVCD} =
        aeb_fate_abi:create_calldata("init",
                                     [aefa_fate_code:encode_arg({address, ?OWNER_PUBKEY})]),
    {SVPubkey, Trees2} = create_contract("StakingValidator", SVCD, TxEnv, Trees1),
    {ok, MainCD} = aeb_fate_abi:create_calldata("init",
                                              [aefa_fate_code:encode_arg({contract,
                                                                        SVPubkey}),
                                               aefa_fate_code:encode_arg({string,
                                                                          <<"asdf">>}),
                                               aefa_fate_code:encode_arg({integer,
                                                                          ?VALIDATOR_MIN}),
                                               aefa_fate_code:encode_arg({integer,
                                                                          ?STAKE_MIN})
                                              ]),
    {MainPubkey, Trees3} = create_contract(?STAKING_CONTRACT, MainCD, TxEnv, Trees2),
    %% assert expectation:
    MainPubkey = consensus_contract_address(),
    Trees3.


set_up_accounts(Trees) ->
    lists:foldl(fun set_up_account/2,
                Trees,
                [ {?OWNER_PUBKEY, trunc(math:pow(10, 30))},
                  {pubkey(?ALICE), trunc(math:pow(10, 30))},
                  {pubkey(?BOB), trunc(math:pow(10, 30))},
                  {pubkey(?CAROL), trunc(math:pow(10, 30))}]).

set_up_account({Pubkey, Amount}, Trees) ->
    Account = aec_accounts:new(Pubkey, Amount),
    Accs = aec_trees:accounts(Trees),
    aec_trees:set_accounts(Trees, aec_accounts_trees:enter(Account, Accs)).

account_balance(Pubkey, Trees) ->
    Accs = aec_trees:accounts(Trees),
    Acc = aec_accounts_trees:get(Pubkey, Accs),
    aec_accounts:balance(Acc).

create_contract(ContractName, CallData, TxEnv, Trees) ->
    Owner = ?OWNER_PUBKEY,
    Nonce = next_nonce(Owner, Trees),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    {ok, Code}   =
        aect_test_utils:compile_contract(aect_test_utils:sophia_version(),
                                         ContractName),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    VM = aect_test_utils:latest_sophia_vm_version(),
    TxSpec = #{owner_id    => aeser_id:create(account, Owner),
               nonce       => Nonce,
               code        => Code,
               vm_version  => VM,
               abi_version => ABI,
               deposit     => 0,
               amount      => 0,
               gas         => ?GAS,
               gas_price   => ?GAS_PRICE,
               call_data   => CallData,
               fee         => ?FEE},
    {ok, DummyTx} = aect_create_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_create_tx:new(TxSpec#{fee => MinFee}),
    %% Make sure the transaction will give the expected pubkey.
    case aect_contracts:compute_contract_pubkey(Owner, Nonce) of
                    Pubkey -> Tx;
                    Other          -> error({unexpected_pubkey, Other, Pubkey})
                end,
    Trees1 = aec_block_fork:prepare_contract_owner([Tx], TxEnv, Trees),
    {_, Trees2} = aec_block_fork:apply_contract_create_tx(Tx, Trees1, TxEnv),
    {Pubkey, Trees2}.

call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees) ->
    Nonce = next_nonce(Caller, Trees),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    TxSpec = #{ caller_id    => aeser_id:create(account, Caller),
                contract_id  => aeser_id:create(contract, ContractPubkey),
                nonce        => Nonce,
                abi_version  => ABI,
                amount       => Amount,
                gas          => ?GAS,
                gas_price    => ?GAS_PRICE,
                call_data    => CallData,
                fee          => ?FEE},
    {ok, DummyTx} = aect_call_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_call_tx:new(TxSpec#{fee => MinFee}),
    case aetx:process(Tx, Trees, TxEnv) of
        {ok, Trees2, _} ->
            Calls = aec_trees:calls(Trees2),
            {contract_call_tx, CallTx} = aetx:specialize_type(Tx),
            CallId = aect_call_tx:call_id(CallTx),
            Call = aect_call_state_tree:get_call(ContractPubkey, CallId,
                                                 Calls),
            case aect_call:return_type(Call) of
                ok ->
                    %% prune the call being produced. If not done, the fees for it
                    %% would be redistributed to the corresponding leaders
                    {ok, aect_call_state_tree:prune(Height, Trees2),
                        aeb_fate_encoding:deserialize(aect_call:return_value(Call))};
                error -> {error,
                            aeb_fate_encoding:deserialize(aect_call:return_value(Call))};
                revert -> {revert,
                            aeb_fate_encoding:deserialize(aect_call:return_value(Call))}
            end;
        {error, _What} = Err -> Err
    end.

next_nonce(Pubkey, Trees) ->
    Accounts = aec_trees:accounts(Trees),
    LastNonce =
        case aec_accounts_trees:lookup(Pubkey, Accounts) of
            none -> 0;
            {value, Account} ->
                aec_accounts:nonce(Account)
        end,
    LastNonce + 1.

pubkey({P, _}) -> P.

%% black magic: this relies that there are 2 contracts and that the consensus
%% one is the second (having a nonce 2); this allows us not passing around the
%% address of the consensus contract between tests
consensus_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

test_elect_calls(StartHeight, GenerenationsCnt, TxEnv, StartTrees) ->
    lists:foldl(
        fun(Height, {TreesAccum1, Ls}) ->
            TxEnvPrev = aetx_env:set_height(TxEnv, Height - 1),
            {ok, _ , {address, NextLeader}} = elect_next_(?OWNER_PUBKEY, TxEnvPrev,
                                                        TreesAccum1),
            TxEnv1 = aetx_env:set_height(TxEnv, Height),
            {ok, TreesAccum2, {tuple, {}}} = elect_(?OWNER_PUBKEY, TxEnv1,
                                                    TreesAccum1),
            {ok, _, {address, NextLeader}} = leader_(?OWNER_PUBKEY, TxEnv1, TreesAccum2),
            Ls1 =
                maps:update_with(NextLeader, fun(X) -> X + 1 end, 1, Ls),
            {TreesAccum2, Ls1}
        end,
        {StartTrees, #{}},
        lists:seq(StartHeight, StartHeight + GenerenationsCnt)).

%% contract call wrappers
new_validator_(Pubkey, Amount, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("new_validator", []),
    call_contract(ContractPubkey, Pubkey, CallData, Amount, TxEnv, Trees0).

balance_(Who, Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("balance",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

get_validator_state_(Who, Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("get_validator_state",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

is_validator_online_(Who, Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("is_validator_online",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_validator_online_(Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_online", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_validator_offline_(Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_offline", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

online_validators_(Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("online_validators", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

offline_validators_(Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("offline_validators", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

elect_next_(Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("elect_next", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

elect_(Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("elect", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

leader_(Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("leader", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

reward_(Who, Amount, Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("reward",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees0).

stake_(Who, Amount, Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("stake",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees0).

unstake_(Who, Stakes, Caller, TxEnv, Trees0) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("unstake",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who}),
                                                   aefa_fate_code:encode_arg({integer,
                                                                              Stakes})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_name_(Name, Caller, TxEnv, Trees0) when is_list(Name) ->
    set_name_(list_to_binary(Name), Caller, TxEnv, Trees0);
set_name_(Name, Caller, TxEnv, Trees0) when is_binary(Name) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_validator_name",
                                                  [aefa_fate_code:encode_arg({string, Name})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_description_(Description, Caller, TxEnv, Trees0) when is_list(Description) ->
    set_description_(list_to_binary(Description), Caller, TxEnv, Trees0);
set_description_(Description, Caller, TxEnv, Trees0) when is_binary(Description) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_validator_description",
                                                  [aefa_fate_code:encode_arg({string, Description})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_avatar_(Avatar, Caller, TxEnv, Trees0) when is_list(Avatar) ->
    set_avatar_(list_to_binary(Avatar), Caller, TxEnv, Trees0);
set_avatar_(Avatar, Caller, TxEnv, Trees0) when is_binary(Avatar) ->
    ContractPubkey = consensus_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_validator_avatar_url",
                                                  [aefa_fate_code:encode_arg({string, Avatar})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

