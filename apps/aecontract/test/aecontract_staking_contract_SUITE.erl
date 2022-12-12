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
          inspect_two_validators/1,
          validator_withdrawal/1,
          setting_online_delay/1,
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
          can_not_stake_below_treshold/1,
          validator_can_not_unstake_below_30_percent_treshold/1,
          total_stake_limit/1
        ]).

-export([ stake_delay/1,
          stake_delay_respects_upper_stake_limit/1,
          stake_delay_set_to_zero/1,
          unstake_delay/1,
          unstake_delay_set_to_zero/1
        ]).

-export([ staking_without_delay_return_shares/1,
          staking_with_delay_return_shares/1,
          unstaking_return_shares/1,
          unstaking_below_minimum_stake/1
        ]).

-export([ entropy_impacts_leader_election/1
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
-define(POS_ELECTION_CONTRACT, "PoSElection").
-define(HC_ELECTION_CONTRACT, "HCElection").

-define(POS, pos).
-define(HC, hc).

-define(GAS, 10000000).
-define(GAS_PRICE, aec_test_utils:min_gas_price()).
-define(FEE, 1000000000000).

-define(AE, 1000000000000000000).   % math:pow(10,18)
-define(VALIDATOR_MIN, 1000 * ?AE). % 1000 AE
-define(STAKE_MIN, 10 * ?AE).       % 10 AE

-define(VALIDATOR_MIN_PERCENT, 30).
-define(ONLINE_DELAY, 0).
-define(STAKE_DELAY, 0).
-define(UNSTAKE_DELAY, 0).
-define(ENTROPY, <<"asdf">>).

-record(staking_resp, {stake, shares, execution_height}).

all() -> [{group, staking},
          {group, hc_election}
         ].

groups() ->
    [ {staking, [sequence],
       [ new_validator,
         inspect_validator,
         inspect_two_validators,
         validator_withdrawal,
         setting_online_delay,
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
         can_not_stake_below_treshold,
         validator_can_not_unstake_below_30_percent_treshold,
         total_stake_limit,
         stake_delay,
         stake_delay_set_to_zero,
         stake_delay_respects_upper_stake_limit,
         unstake_delay,
         unstake_delay_set_to_zero,
         staking_without_delay_return_shares,
         staking_with_delay_return_shares,
         unstaking_return_shares,
         unstaking_below_minimum_stake
       ]},
      {hc_election, [sequence],
       [ entropy_impacts_leader_election
       ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    case aect_test_utils:latest_protocol_version() of
        PreIris when PreIris < ?IRIS_PROTOCOL_VSN ->
            {skip, from_iris_on};
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
    Trees0 = genesis_trees(?POS),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Alice = pubkey(?ALICE),
    {ok, _, {contract, _}} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees0),
    ok.

inspect_validator(_Config) ->
    Trees0 = genesis_trees(?POS),
    Amount = ?VALIDATOR_MIN,
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Alice = pubkey(?ALICE),
    %% no stakers, empty contract
    {ok, _, ContractState0} = get_staking_contract_state_(Alice, TxEnv, Trees0),
    {tuple, {   StakingValidatorCT,
                [],
                0, %% total stake, only one offline staker
                ValidatorMinStake,
                ValidatorMinPercent,
                StakeMin,
                OnlineDelay,
                StakeDelay,
                UnstakeDelay
                }} = ContractState0,
    {contract, ValidatorContractPubkey} = StakingValidatorCT,
    ValidatorContractPubkey = validator_contract_address(),
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees0),
    {tuple, {   StakingCT,
                Entropy,
                Leader
                }} = ElectionContractState0,
    {contract, StakingContractPubkey} = StakingCT,
    StakingContractPubkey = staking_contract_address(),
    {bytes, _} = Entropy,
    ElectionContractPubkey = election_contract_address(),
    {address, ElectionContractPubkey} = Leader, %% no election yet, the contract is the first leader
    %% create a validator and check the contract state; the newly created
    %% validator is offline
    {ok, Trees1, {contract, AliceContract}} = new_validator_(Alice, Amount, TxEnv, Trees0),
    {ok, _, SPower} = staking_power_(Alice, Alice, TxEnv, Trees1),
    {SPower, SPower} = {Amount, SPower},
    {ok, _, State} = get_validator_state_(Alice, Alice, TxEnv, Trees1),
    ExpectedAliceOfflineState =
        expected_validator_state(AliceContract, Alice, ?GENESIS_HEIGHT, SPower, 0,
                                 calculate_total_stake_limit(SPower),
                                 false, <<>>, <<>>, <<>>,
                                 #{Alice => SPower}),
    assert_equal_states(State, ExpectedAliceOfflineState),
    {ok, _, IsOnline} = is_validator_online_(Alice, Alice, TxEnv, Trees1),
    false = IsOnline,
    {ok, _, ContractState} = get_staking_contract_state_(Alice, TxEnv, Trees1),
    {tuple, {   StakingValidatorCT,
                Validators,
                0, %% total stake, only one offline staker
                ValidatorMinStake,
                ValidatorMinPercent,
                StakeMin,
                OnlineDelay,
                StakeDelay,
                UnstakeDelay
                }} = ContractState,
    ?assertEqual([ExpectedAliceOfflineState], Validators),
    ?VALIDATOR_MIN = ValidatorMinStake,
    ?VALIDATOR_MIN_PERCENT = ValidatorMinPercent,
    ?STAKE_MIN = StakeMin,
    ?ONLINE_DELAY = OnlineDelay,
    ?STAKE_DELAY = StakeDelay,
    ?UNSTAKE_DELAY = UnstakeDelay,
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees1),
    %% set the validator as online; the total stake shall be the total stake
    %% of the validator
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    ExpectedAliceOnlineState =
        expected_validator_state(AliceContract, Alice, ?GENESIS_HEIGHT, SPower, 0,
                                 calculate_total_stake_limit(SPower),
                                 true, <<>>, <<>>, <<>>,
                                 #{Alice => SPower}),
    {ok, _, State2} = get_validator_state_(Alice, Alice, TxEnv, Trees2),
    assert_equal_states(State2, ExpectedAliceOnlineState),
    {ok, _, ContractState1} = get_staking_contract_state_(Alice, TxEnv, Trees2),
    {tuple, {   StakingValidatorCT,
                Validators1,
                TotalStake, %% total stake, only one online staker
                ValidatorMinStake,
                ValidatorMinPercent,
                StakeMin,
                OnlineDelay,
                StakeDelay,
                UnstakeDelay
                }} = ContractState1,
    [ExpectedAliceOnlineState] = Validators1,
    Amount = TotalStake,
    SPower = TotalStake,
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees2),
    %% set the validator back to offline, the state is exactly the same as
    %% before setting it online; it is important that going online/offline is
    %% idempotent
    {ok, Trees3, {tuple, {}}} = set_validator_offline_(Alice, TxEnv, Trees2),
    {ok, _, ContractState} = get_staking_contract_state_(Alice, TxEnv, Trees3),
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees3),
    %% test idempotence to getting back online
    {ok, Trees4, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees3),
    {ok, _, ContractState1} = get_staking_contract_state_(Alice, TxEnv, Trees4),
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees4),
    %% test that election sets Alice as a leader
    {ok, Trees5, {tuple, {}}} = elect_(?OWNER_PUBKEY, TxEnv, Trees4),
    %% no change in staking contract:
    {ok, _, ContractState1} = get_staking_contract_state_(Alice, TxEnv, Trees5),
    {ok, _, ElectionContractState1} = get_election_contract_state_(Alice, TxEnv, Trees5),
    {tuple, {   StakingCT, %% same
                Entropy, %% same
                Leader2
                }} = ElectionContractState1,
    {address, Alice} = Leader2, %% Alice is being elected as a leader
    %% give away some rewards; this changes the total staking power but does
    %% not change the shares distribution
    Reward = 1000,
    {ok, Trees6, {tuple, {}}} = reward_(Alice, Reward, ?OWNER_PUBKEY,
                                        TxEnv, Trees5),
    ExpectedAliceOnlineState1 =
        expected_validator_state(AliceContract, Alice, ?GENESIS_HEIGHT, SPower + Reward, 0,
                                 calculate_total_stake_limit(SPower + Reward),
                                 true, <<>>, <<>>, <<>>,
                                 #{Alice => SPower}), %% share distribution is the same
    {ok, _, State3} = get_validator_state_(Alice, Alice, TxEnv, Trees6),
    assert_equal_states(State3, ExpectedAliceOnlineState1),
    {ok, _, ContractState3} = get_staking_contract_state_(Alice, TxEnv, Trees6),
    ExpectedTotalStake = TotalStake + Reward,
    {tuple, {   StakingValidatorCT,
                Validators2, %% same validator
                ExpectedTotalStake,
                ValidatorMinStake, %% same
                ValidatorMinPercent, %% same
                StakeMin, %% same
                OnlineDelay, %% same
                StakeDelay, %% same
                UnstakeDelay %% same
                }} = ContractState3,
    [ExpectedAliceOnlineState1] = Validators2,
    %% election contract state is unchanged
    {ok, _, ElectionContractState1} = get_election_contract_state_(Alice, TxEnv, Trees6),
    %% test online-offline idemptence after a reward distribution
    {ok, Trees7, {tuple, {}}} = set_validator_offline_(Alice, TxEnv, Trees6),
    %% election contract state is unchanged
    {ok, _, ElectionContractState1} = get_election_contract_state_(Alice, TxEnv, Trees7),
    {ok, _, ContractState4} = get_staking_contract_state_(Alice, TxEnv, Trees7),
    {tuple, {   StakingValidatorCT, %% same
                Validators3,
                0,
                ValidatorMinStake, %% same
                ValidatorMinPercent, %% same
                StakeMin, %% same
                OnlineDelay, %% same
                StakeDelay, %% same
                UnstakeDelay %% same
                }} = ContractState4,
    ExpectedAliceOfflineState1 =
        expected_validator_state(AliceContract, Alice, ?GENESIS_HEIGHT, SPower + Reward, 0,
                                 calculate_total_stake_limit(SPower + Reward),
                                 false, <<>>, <<>>, <<>>,
                                 #{Alice => SPower}), %% share distribution is the same
    [ExpectedAliceOfflineState1] = Validators3,
    {ok, Trees8, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees7),
    {ok, _, ContractState3} = get_staking_contract_state_(Alice, TxEnv, Trees8),
    %% election contract state is unchanged
    {ok, _, ElectionContractState1} = get_election_contract_state_(Alice, TxEnv, Trees7),
    ok.

inspect_two_validators(_Config) ->
    Trees0 = genesis_trees(?POS),
    Amount = ?VALIDATOR_MIN,
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    %% no stakers, empty contract
    {ok, _, ContractState0} = get_staking_contract_state_(Alice, TxEnv, Trees0),
    {tuple, {   StakingValidatorCT,
                [],
                0, %% total stake, only one offline staker
                _ValidatorMinStake,
                _ValidatorMinPercent,
                _StakeMin,
                _OnlineDelay,
                _StakeDelay,
                _UnstakeDelay
                }} = ContractState0,
    {contract, _} = StakingValidatorCT,
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees0),
    {tuple, {   _StakingCT,
                Entropy,
                Leader
                }} = ElectionContractState0,
    {bytes, _} = Entropy,
    ElectionContractPubkey = election_contract_address(),
    {address, ElectionContractPubkey} = Leader, %% no election yet, the contract is the first leader
    %% create a validator and check the contract state; the newly created
    %% validator is offline
    {ok, Trees1, {contract, AliceContract}} = new_validator_(Alice, Amount, TxEnv, Trees0),
    {ok, _, AliceSPower} = staking_power_(Alice, Alice, TxEnv, Trees1),
    ExpectedAliceOfflineState0 =
        expected_validator_state(AliceContract, Alice, ?GENESIS_HEIGHT, AliceSPower, 0,
                                 calculate_total_stake_limit(AliceSPower),
                                 false, <<>>, <<>>, <<>>,
                                 #{Alice => AliceSPower}), %% share distribution is the same
    {ok, Trees2, {contract, BobContract}} = new_validator_(Bob, 2 * Amount,
                                                           TxEnv, Trees1),
    {ok, _, BobSPower} = staking_power_(Bob, Bob, TxEnv, Trees2),
    ?assertEqual(BobSPower, 2 * Amount),
    ExpectedBobOfflineState0 =
        expected_validator_state(BobContract, Bob, ?GENESIS_HEIGHT, BobSPower, 0,
                                 calculate_total_stake_limit(BobSPower),
                                 false, <<>>, <<>>, <<>>,
                                 #{Bob => BobSPower}), %% share distribution is the same
    {ok, _, ContractState1} = get_staking_contract_state_(Alice, TxEnv, Trees2),
    {ok, _, ContractState1} = get_staking_contract_state_(Bob, TxEnv, Trees2),
    {tuple, {StakingValidatorCT, [ExpectedBobOfflineState0, ExpectedAliceOfflineState0],
             0, %% total stake, only offline stakers
             _, _, _, _, _, _ }} = ContractState1,
    %% election contract state is unchanged
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees2),
    %% set Alice online; this changes the total staked amount to Alice's
    %% balance
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees2),
    ExpectedAliceOnlineState0 =
        expected_validator_state(AliceContract, Alice, ?GENESIS_HEIGHT, AliceSPower, 0,
                                 calculate_total_stake_limit(AliceSPower),
                                 true, <<>>, <<>>, <<>>,
                                 #{Alice => AliceSPower}), %% share distribution is the same
    {ok, _, ContractState2} = get_staking_contract_state_(Alice, TxEnv, Trees3),
    {ok, _, ContractState2} = get_staking_contract_state_(Bob, TxEnv, Trees3),
    {tuple, {StakingValidatorCT,
             [ ExpectedAliceOnlineState0, %% Alice is online
               ExpectedBobOfflineState0 ],
             AliceSPower, %% total stake, only Alice is online
             _, _, _, _, _, _ }} = ContractState2,
    %% election contract state is unchanged
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees2),
    %% set Bob online as well
    {ok, Trees4, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees3),
    ExpectedBobOnlineState0 =
        expected_validator_state(BobContract, Bob, ?GENESIS_HEIGHT, BobSPower, 0,
                                 calculate_total_stake_limit(BobSPower),
                                 true, <<>>, <<>>, <<>>,
                                 #{Bob => BobSPower}), %% share distribution is the same
    {ok, _, ContractState3} = get_staking_contract_state_(Alice, TxEnv, Trees4),
    {ok, _, ContractState3} = get_staking_contract_state_(Bob, TxEnv, Trees4),
    CombinedSPower = AliceSPower + BobSPower,
    {tuple, {StakingValidatorCT, %% same
             [ ExpectedBobOnlineState0, ExpectedAliceOnlineState0 ],
             CombinedSPower, %% total stake, only Alice is online
             _, _, _, _, _, _ }} = ContractState3,
    %% election contract state is unchanged
    {ok, _, ElectionContractState0} = get_election_contract_state_(Alice, TxEnv, Trees2),
    ok.

validator_withdrawal(_Config) ->
    Trees0 = genesis_trees(?POS),
    OverheadAmt = 1000,
    Amount = ?VALIDATOR_MIN + OverheadAmt,
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    {ok, Trees1, {contract, _AliceContract}} = new_validator_(Alice, Amount, TxEnv, Trees0),
    {ok, Trees2, {contract, _BobContract}} = new_validator_(Bob, Amount, TxEnv, Trees1),
    %% Alice will be online and Bob will be offline
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees2),
    %% can not withdraw more than OverheadAmt
    {revert, <<"Validator can not withdraw below the treshold">>}
        = unstake_(Alice, OverheadAmt + 1, Alice, TxEnv, Trees3),
    {revert, <<"Validator can not withdraw below the treshold">>}
        = unstake_(Bob, OverheadAmt + 1, Bob, TxEnv, Trees3),
    {ok, _, AliceUnstakeResp3} = unstake_(Alice, OverheadAmt, Alice, TxEnv, Trees3),
    {ok, _, BobUnstakeResp3} = unstake_(Bob, OverheadAmt, Bob, TxEnv, Trees3),
    ?assertEqual(OverheadAmt, AliceUnstakeResp3#staking_resp.stake),
    ?assertEqual(OverheadAmt, BobUnstakeResp3#staking_resp.stake),

    %% reward both parties and now they can unstake down to ?VALIDATOR_MIN amount
    Reward = 1,
    {ok, Trees4, {tuple, {}}} = reward_(Alice, Reward, ?OWNER_PUBKEY, TxEnv, Trees3),
    {ok, Trees5, {tuple, {}}} = reward_(Bob, Reward, ?OWNER_PUBKEY, TxEnv, Trees4),
    {revert, <<"Validator can not withdraw below the treshold">>}
        = unstake_(Alice, OverheadAmt + Reward + 1, Alice, TxEnv, Trees5),
    {revert, <<"Validator can not withdraw below the treshold">>}
        = unstake_(Bob, OverheadAmt + Reward + 1, Bob, TxEnv, Trees5),
    %% TODO: revisit the tests once decision is being made for unstaking AE or
    %% unstaking stake shares
    {ok, Trees6, AliceUnstakeResp} = unstake_(Alice, 1, Alice, TxEnv, Trees5),
    {ok, _, BobUnstakeResp} = unstake_(Bob, 1, Bob, TxEnv, Trees6),
    ?assertEqual(1, AliceUnstakeResp#staking_resp.stake),
    ?assertEqual(1, BobUnstakeResp#staking_resp.stake),
    ok.

setting_online_delay(_Config) ->
    Delay = 100,
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees = genesis_trees(?POS, #{online_delay => Delay}),

    {ok, Trees1, {contract, _}} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees),

    % State contains setting online delay
    ?assertMatch({ok, _, {tuple, {_, _, _, _, _, _, Delay, _, _}}},
                 get_staking_contract_state_(Alice, TxEnv, Trees1)),

    % Validator state contains creation height
    ?assertMatch({ok, _, {tuple, {_, _, ?GENESIS_HEIGHT, _, _, _, _, _}}},
                 get_validator_state_(Alice, Alice, TxEnv, Trees1)),

    % always allow to set online on genesis height
    ?assertEqual(?GENESIS_HEIGHT, aetx_env:height(TxEnv)),
    ?assertMatch({ok, _, _}, set_validator_online_(Alice, TxEnv, Trees1)),

    Reason = <<"Minimum height not reached">>,

    TxEnv1  = aetx_env:set_height(TxEnv, ?GENESIS_HEIGHT + 1),
    ?assertEqual({revert, Reason}, set_validator_online_(Alice, TxEnv1, Trees1)),

    TxEnv2 = aetx_env:set_height(TxEnv1, ?GENESIS_HEIGHT + Delay - 1),
    ?assertEqual({revert, Reason}, set_validator_online_(Alice, TxEnv2, Trees1)),

    TxEnv3 = aetx_env:set_height(TxEnv, ?GENESIS_HEIGHT + Delay),
    ?assertMatch({ok, _, _}, set_validator_online_(Alice, TxEnv3, Trees1)),

    ok.

setting_online_and_offline(_Config) ->
    Alice = pubkey(?ALICE),
    Trees0 = genesis_trees(?POS),
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
    Trees0 = genesis_trees(?POS),
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
            {ok, _ , {address, Alice}} = elect_next_(Alice, TxEnvPrev, TreesAccum1),
            TxEnv1 = aetx_env:set_height(TxEnv, Height),
            {ok, TreesAccum2, {tuple, {}}} = elect_(?OWNER_PUBKEY, TxEnv1, TreesAccum1),
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
    Trees0 = genesis_trees(?POS),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees1 =
        lists:foldl(
            fun({Pubkey,  Amount}, TreesAccum) ->
                {ok, TreesAccum1, _} = new_validator_(Pubkey, Amount, TxEnv, TreesAccum),
                TreesAccum1
            end,
            Trees0,
            [{Alice, 2 * ?VALIDATOR_MIN},
            {Bob, ?VALIDATOR_MIN},
            {Carol, ?VALIDATOR_MIN}]),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    {ok, _, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees2),
    %% no rewards to check probabilities
    ok.

unstake_balances(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(?POS),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),

    {ok, Trees0_1, _} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees0_2, _} = new_validator_(Bob, ?VALIDATOR_MIN, TxEnv, Trees0_1),
    Trees1 = Trees0_2,
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees2),

    %% a new staker
    {Sam, Trees4} = set_up_account(Trees3),

    %% let Sam put his stake in Alice's staking pool. He will have as much
    %% staking power as her so he will get 50% of the rewards
    SamSPower0 = account_balance(Sam, Trees4),
    {ok, Trees5, StakeResp5} = stake_(Alice, ?VALIDATOR_MIN, Sam, TxEnv, Trees4),
    ?assertEqual(?VALIDATOR_MIN, StakeResp5#staking_resp.stake),
    SharesStaked = StakeResp5#staking_resp.shares,
    SamSPower1 = account_balance(Sam, Trees5),
    ?assert(SamSPower0 - SamSPower1 - ?VALIDATOR_MIN > 0), % some gas fees

    {ok, _, {tuple, {_, _, _, _, _, _, _, AliceState5}}}
        = get_validator_state_(Alice, Alice, TxEnv, Trees5),
    {tuple, {_, _, _, _, _, _, _, Stakers5, _}} = AliceState5,
    ?assertEqual(2, maps:size(Stakers5)),
    ?assertEqual(?VALIDATOR_MIN, maps:get({address, Alice}, Stakers5)),
    ?assertEqual(?VALIDATOR_MIN, maps:get({address, Sam}, Stakers5)),

    ExpectedSamReward = ?VALIDATOR_MIN,
    TotalReward = 2 * ExpectedSamReward,
    {ok, Trees6, {tuple, {}}} = reward_(Alice, TotalReward, ?OWNER_PUBKEY, TxEnv, Trees5),
    {ok, _, StakingResp} = unstake_(Alice, SharesStaked, Sam, TxEnv, Trees6),
    ActualReward = StakingResp#staking_resp.stake - ?VALIDATOR_MIN,
    ?assertEqual(ActualReward, ExpectedSamReward),

    ok.

staking_and_unstaking_effects_election(_Config) ->
    Trees0 = genesis_trees(?POS),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    %% Alice will have twice the staking power compared to Bob, she shall get
    %% elected twice as often
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Carol = pubkey(?CAROL), %% will be offline

    {ok, Trees0_1, _} = new_validator_(Alice, 2*?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees0_2, _} = new_validator_(Bob, ?VALIDATOR_MIN, TxEnv, Trees0_1),
    {ok, Trees0_3, _} = new_validator_(Carol, ?VALIDATOR_MIN, TxEnv, Trees0_2),

    Trees1 = Trees0_3,
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees1),
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees2),

    %% create a lot of stakers with some significant stake
    %% the following would state 100 stakers, half of which would stake to
    %% Alice, the other half - to Bob
    %% if Alice's initial staking power was 2 * X, then Bob's was X, we would
    %% stake X more to each other so Alice would have 3 * X and Bob - 2 * X
    StakersCnt = 100,
    StakerStake = ?VALIDATOR_MIN div 50,
    %% create staker accounts
    {Stakers, Trees4} = lists:foldl(fun(_, {AccStakers, AccTrees}) ->
                {Staker, NewAccTrees} = set_up_account(AccTrees),
                {[Staker | AccStakers], NewAccTrees}
            end, {[], Trees3}, lists:seq(1, StakersCnt)),

    AliceStakers = lists:nthtail(StakersCnt div 2, lists:reverse(Stakers)),
    BobStakers = lists:nthtail(StakersCnt div 2, Stakers),

    StakeFun =
        fun(Who, StakersList, StakeTrees0) ->
            {ok, _, SPower0} = staking_power_(Who, Who, TxEnv, StakeTrees0),

            StakeTrees = lists:foldl( fun(Pubkey, TreesAccum) ->
                        {ok, TreesAccum1, _} = stake_(Who, StakerStake, Pubkey, TxEnv, TreesAccum),
                        TreesAccum1
                    end, StakeTrees0, StakersList),

            {ok, _, SPower1} = staking_power_(Who, Who, TxEnv, StakeTrees),
            ?assertEqual(length(StakersList) * StakerStake + SPower0, SPower1),
            StakeTrees
        end,

    Trees5 = StakeFun(Alice, AliceStakers, Trees4),
    Trees6 = StakeFun(Bob, BobStakers, Trees5),

    {ok, _, AliceSPower} = staking_power_(Alice, Alice, TxEnv, Trees6),
    {ok, _, BobSPower}   = staking_power_(Bob, Bob, TxEnv, Trees6),
    ?assertEqual(2*AliceSPower, 3*BobSPower),

    {_, Leaders} = test_elect_calls(?GENESIS_HEIGHT, 1000, TxEnv, Trees6),
    #{Alice := AliceTurns, Bob := BobTurns} = Leaders,
    ?assertEqual(600-6, AliceTurns),
    ?assertEqual(400+6, BobTurns),

    %% reward both pools by doubling their total amount. This would yeld 100%
    %% RoI for all stakers: at this point withdrawing X stakes would provide
    %% them with 2 * X aettos
    %% NB: we start from Trees6 (just after staking)

    {ok, Trees7, {tuple, {}}} = reward_(Alice, ?VALIDATOR_MIN, ?OWNER_PUBKEY, TxEnv, Trees6),
    {ok, _, AliceSPower7} = staking_power_(Alice, Alice, TxEnv, Trees7),
    ?assertMatch(4 * ?VALIDATOR_MIN, AliceSPower7),

    {ok, Trees8, {tuple, {}}} = reward_(Bob, ?VALIDATOR_MIN, ?OWNER_PUBKEY, TxEnv, Trees7),
    {ok, _, BobSPower8} = staking_power_(Bob, Bob, TxEnv, Trees8),
    ?assertEqual(3 * ?VALIDATOR_MIN, BobSPower8),

    UnStakeFun =
        fun(Who, StakersList, StakeTrees0) ->
            {ok, _, SPower0} = staking_power_(Who, Who, TxEnv, StakeTrees0),

            {StakeTrees, TotalWithdrawnAmt} = lists:foldl(
                    fun(Pubkey, {TreesAccum, WithdrawnSum}) ->
                        {ok, TreesAccum1, Resp} = unstake_(Who, StakerStake, Pubkey, TxEnv, TreesAccum),
                        {TreesAccum1, WithdrawnSum + Resp#staking_resp.stake}
                    end, {StakeTrees0, 0}, StakersList),

            {ok, _, SPower1} = staking_power_(Who, Who, TxEnv, StakeTrees),
            ?assertEqual(SPower0 - TotalWithdrawnAmt, SPower1),
            StakeTrees
        end,

    %% unstake all Bob's stakers
    Trees9 = UnStakeFun(Bob, BobStakers, Trees8),
    {_, _Leaders2} = test_elect_calls(?GENESIS_HEIGHT, 1000, TxEnv, Trees9),
    ok.

can_not_unstake_more_shares_than_owned(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(?POS),
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
    {ok, Trees4, StakeResp4} = stake_(Alice, ?STAKE_MIN, Sam, TxEnv, Trees3),
    ?assertEqual(?STAKE_MIN, StakeResp4#staking_resp.stake),
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
    Trees0 = genesis_trees(?POS),
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
             {address, Alice},
             _, %% creation height
             _, %% staker pool balance
             _, %% pending stake
             _, %% total stake limit
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                      0,     %% unstake delay
                      0,     %% pending unstake amount
                      #{},   %% pending unstake
                     <<"">>, %% name
                     <<"">>, %% description
                     <<"">>, %% avatarURL,
                     _MapA, ?VALIDATOR_MIN }}}} = AliceState0,
    {ok, _, BobState0} = get_validator_state_(Bob, Bob, TxEnv, Trees2),
    {tuple, {{contract, _}, %% the pool contract
             {address, Bob},
             _, %% creation height
             _, %% staker pool balance
             _, %% pending stake
             _, %% total stake limit
             false, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                      0,     %% unstake delay
                      0,     %% pending unstake amount
                      #{},   %% pending unstake
                     <<"">>, %% name
                     <<"">>, %% description
                     <<"">>, %% avatarURL,
                     _MapB, ?VALIDATOR_MIN }}}} = BobState0,
    AliceName = <<"AL1CE">>,
    {ok, Trees3, {tuple, {}}} = set_name_(AliceName, Alice, TxEnv, Trees2),
    {ok, _, AliceState1} = get_validator_state_(Alice, Alice, TxEnv, Trees3),
    {tuple, {{contract, _}, %% the pool contract
             {address, Alice},
             _, %% creation height
             _, %% staker pool balance
             _, %% pending stake
             _, %% total stake limit
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                      0,     %% unstake delay
                      0,     %% pending unstake amount
                      #{},   %% pending unstake
                     AliceName, %% name
                     <<"">>, %% description
                     <<"">>, %% avatarURL,
                     MapA, ?VALIDATOR_MIN }}}} = AliceState1,
    %% Bob is unchanged
    {ok, _, BobState0} = get_validator_state_(Bob, Bob, TxEnv, Trees3),
    AliceDescription = <<"Who in the world am I?' Ah, that's the great puzzle!">>,
    {ok, Trees4, {tuple, {}}} = set_description_(AliceDescription, Alice, TxEnv, Trees3),
    {ok, _, AliceState2} = get_validator_state_(Alice, Alice, TxEnv, Trees4),
    {tuple, {{contract, _}, %% the pool contract
             {address, Alice},
             _, %% creation height
             _, %% staker pool balance
             _, %% pending stake
             _, %% total stake limit
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                      0,     %% unstake delay
                      0,     %% pending unstake amount
                      #{},   %% pending unstake
                     AliceName, %% name
                     AliceDescription, %% description
                     <<"">>, %% avatarURL,
                     MapA, ?VALIDATOR_MIN}}}} = AliceState2,
    %% Bob is unchanged
    {ok, _, BobState0} = get_validator_state_(Bob, Bob, TxEnv, Trees4),
    AliceAvatar = <<"test.test/img.jpg">>,
    {ok, Trees5, {tuple, {}}} = set_avatar_(AliceAvatar, Alice, TxEnv, Trees4),
    {ok, _, AliceState3} = get_validator_state_(Alice, Alice, TxEnv, Trees5),
    {tuple, {{contract, _}, %% the pool contract
             {address, Alice},
             _, %% creation height
             _, %% staker pool balance
             _, %% pending stake
             _, %% total stake limit
             true, %% is online
             {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                      0,     %% unstake delay
                      0,     %% pending unstake amount
                      #{},   %% pending unstake
                     AliceName, %% name
                     AliceDescription, %% description
                     AliceAvatar, %% avatarURL,
                     MapA, ?VALIDATOR_MIN}}}} = AliceState3,
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
    Trees0 = genesis_trees(?POS),
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
    Trees0 = genesis_trees(?POS),
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
    {ok, _, SPowerA0} = staking_power_(Alice, Alice, TxEnv, Trees2),
    {SPowerA0, SPowerA0} = {SPowerA0, ?VALIDATOR_MIN},
    {ok, _, SPowerB0} = staking_power_(Bob, Bob, TxEnv, Trees2),
    {SPowerB0, SPowerB0} = {SPowerB0, ?VALIDATOR_MIN},
    {ok, _, SPowerC0} = staking_power_(Carol, Carol, TxEnv, Trees2),
    {SPowerC0, SPowerC0} = {SPowerC0, ?VALIDATOR_MIN},
    %% create a delegate
    #{public := Sam} = enacl:sign_keypair(),
    SamSPower0 = trunc(math:pow(10, 30)),
    Trees3 = set_up_account({Sam, SamSPower0}, Trees2),
    %% the delegate supports Alice, only her balance changes
    {ok, Trees4, _} = stake_(Alice, ?STAKE_MIN, Sam, TxEnv, Trees3),
    {ok, _, SPowerA1} = staking_power_(Alice, Alice, TxEnv, Trees4),
    {SPowerA1, SPowerA1} = {SPowerA1, ?VALIDATOR_MIN + ?STAKE_MIN},
    {ok, _, SPowerB0} = staking_power_(Bob, Bob, TxEnv, Trees4),
    {ok, _, SPowerC0} = staking_power_(Carol, Carol, TxEnv, Trees4),
    %% the delegate supports Bob, only his balance changes
    {ok, Trees5, _} = stake_(Bob, 5 * ?STAKE_MIN, Sam, TxEnv, Trees4),
    {ok, _, SPowerA1} = staking_power_(Alice, Alice, TxEnv, Trees5),
    {ok, _, SPowerB1} = staking_power_(Bob, Bob, TxEnv, Trees5),
    {SPowerB1, SPowerB1} = {SPowerB1, ?VALIDATOR_MIN + 5 * ?STAKE_MIN},
    {ok, _, SPowerC0} = staking_power_(Carol, Carol, TxEnv, Trees5),
    {ok, Trees6, _} = stake_(Carol, 10 * ?STAKE_MIN, Sam, TxEnv, Trees5),
    {ok, _, SPowerA1} = staking_power_(Alice, Alice, TxEnv, Trees6),
    {ok, _, SPowerB1} = staking_power_(Bob, Bob, TxEnv, Trees6),
    {ok, _, SPowerC1} = staking_power_(Carol, Carol, TxEnv, Trees6),
    {SPowerC1, SPowerC1} = {SPowerC1, ?VALIDATOR_MIN + 10 * ?STAKE_MIN},
    ok.

if_unstake_all_delegate_is_deleted(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(?POS),
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
    {ok, _, SPowerA0} = staking_power_(Alice, Alice, TxEnv, Trees2),
    {SPowerA0, SPowerA0} = {SPowerA0, ?VALIDATOR_MIN},
    {ok, _, SPowerB0} = staking_power_(Bob, Bob, TxEnv, Trees2),
    {SPowerB0, SPowerB0} = {SPowerB0, ?VALIDATOR_MIN},
    %% create a delegate
    #{public := Sam} = enacl:sign_keypair(),
    SamSPower0 = trunc(math:pow(10, 30)),
    Trees3 = set_up_account({Sam, SamSPower0}, Trees2),
    Test =
        fun(ToWhom) ->
            {ok, _, State0} = get_validator_state_(ToWhom, ToWhom, TxEnv, Trees3),
            {tuple, {{contract, _}, %% the pool contract
                    {address, ToWhom},
                    _, %% creation height
                    ?VALIDATOR_MIN, %% staker pool balance
                    _, %% pending stake
                    _, %% total stake limit
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _, _, _, _, _, _, Map0, SPower0}}}} = State0,
            %% assert balances
            AddressKey = {address, ToWhom},
            ?assertEqual(?VALIDATOR_MIN, maps:get(AddressKey, Map0)),
            ?assertEqual(1, maps:size(Map0)), %% no other keys
            TotalStakedAmt = ?STAKE_MIN + 10,
            TotalAmt = ?VALIDATOR_MIN + TotalStakedAmt,
            {ok, Trees4, StakeResp4} = stake_(ToWhom, TotalStakedAmt, Sam, TxEnv, Trees3),
            ?assertEqual(TotalStakedAmt, StakeResp4#staking_resp.stake),
            {ok, _, State1} = get_validator_state_(ToWhom, ToWhom, TxEnv,
                                                   Trees4),
            {tuple, {{contract, _}, %% the pool contract
                    {address, ToWhom},
                    _, %% creation height
                    TotalAmt, %% staker pool balance
                    _, %% pending stake
                    _, %% total stake limit
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _, _, _, _Name, _Description,
                            _AvatarURL, Map1, SPower1}}}} = State1,
            ?assertEqual({SPower1, SPower1}, {SPower1, SPower0 + TotalStakedAmt}),
            ?assertEqual(?VALIDATOR_MIN, maps:get({address, ToWhom}, Map1)),
            ?assertEqual(TotalStakedAmt, maps:get({address, Sam}, Map1)),
            ?assertEqual(2, maps:size(Map1)), %% no other keys
            %% withdraw some shares and assert balances
            WithdrawnAmt = 10,
            StakeLeft = TotalStakedAmt - WithdrawnAmt,
            PoolStakeLeft = TotalAmt - WithdrawnAmt,
            {ok, Trees5, Resp5} = unstake_(ToWhom, WithdrawnAmt, Sam, TxEnv, Trees4),
            ?assertEqual(WithdrawnAmt, Resp5#staking_resp.stake),
            {ok, _, State2} = get_validator_state_(ToWhom, ToWhom, TxEnv, Trees5),
            {tuple, {{contract, _}, %% the pool contract
                    {address, ToWhom},
                    _, %% creation height
                    PoolStakeLeft, %% staker pool balance
                    _, %% pending stake
                    _, %% total stake limit
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _, _, _, _, _, _, Map2, SPower2}}}} = State2,
            ?assertEqual({SPower2, SPower2}, {SPower2, SPower1 - WithdrawnAmt}),
            ?assertEqual(?VALIDATOR_MIN, maps:get({address, ToWhom}, Map2)),
            ?assertEqual(StakeLeft, maps:get({address, Sam}, Map2)),
            ?assertEqual(2, maps:size(Map2)), %% no other keys
            %% withdraw what is left, the delegate is being deleted
            {ok, Trees6, Resp6} = unstake_(ToWhom, StakeLeft, Sam, TxEnv, Trees5),
            ?assertEqual(StakeLeft, Resp6#staking_resp.stake),
            {ok, _, State3} = get_validator_state_(ToWhom, ToWhom, TxEnv, Trees6),
            {tuple, {{contract, _}, %% the pool contract
                    {address, ToWhom},
                    _, %% creation height
                    ?VALIDATOR_MIN, %% staker pool balance
                    _, %% pending stake
                    _, %% total stake limit
                    _, %% is online
                    {tuple, {{address, ConsensusContractPubkey}, %% main staking contract
                            _, _, _, _, _, _, Map3, SPower3}}}} = State3,
            ?assertEqual({SPower3, SPower3}, {SPower3, SPower0}),
            ?assertEqual(?VALIDATOR_MIN, maps:get({address, ToWhom}, Map3)),
            ?assertEqual(1, maps:size(Map3)) %% no other keys
        end,
    Test(Alice), %% online
    Test(Bob), %% offline
    ok.

can_not_become_validator_below_treshold(_Config) ->
    Alice = pubkey(?ALICE),
    Trees0 = genesis_trees(?POS),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    {revert, <<"A new validator stake the minimum amount">>}
        = new_validator_(Alice, ?VALIDATOR_MIN - 1, TxEnv, Trees0),
    ok.

can_not_stake_below_treshold(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Trees0 = genesis_trees(?POS),
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
    SamSPower0 = trunc(math:pow(10, 30)),
    Trees3 = set_up_account({Sam, SamSPower0}, Trees2),
    {revert, <<"Must stake the minimum amount">>} =
        stake_(Alice, ?STAKE_MIN - 1, Sam, TxEnv, Trees3),
    ok.

validator_can_not_unstake_below_30_percent_treshold(_Config) ->
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS),
    {Sam, Trees1} = set_up_account(Trees0),

    InitialStake = 2 * ?VALIDATOR_MIN,
    {ok, Trees2, {contract, _}} = new_validator_(Alice, InitialStake, TxEnv, Trees1),
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees2),
    {ok, Trees4, _} = stake_(Alice, InitialStake, Sam, TxEnv, Trees3),

    Bob = pubkey(?BOB),
    {ok, Trees5, {contract, _}} = new_validator_(Bob, InitialStake, TxEnv, Trees4),
    {ok, Trees6, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees5),

    %% Total stake: 6000
    %% Alice total stake: 4000
    %% Alice stake 2000
    %% Sam stake: 2000
    %% 30% treshold for Alice: 1200
    UnstakeAmt = 800*?AE,

    ?assertEqual({revert, <<"Validator can not withdraw below the treshold">>},
                 unstake_(Alice, UnstakeAmt + 1, Alice, TxEnv, Trees6)),
    {ok, _, _} = unstake_(Alice, UnstakeAmt, Alice, TxEnv, Trees6),

    ok.

total_stake_limit(_Config) ->
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees = genesis_trees(?POS),
    {Sam, Trees0} = set_up_account(Trees),

    InitialStake = ?VALIDATOR_MIN,

    %% Online Validator
    BaseStakeLimit = calculate_total_stake_limit(InitialStake),
    {ok, TreesOnline1, {contract, _}} = new_validator_(Alice, InitialStake, TxEnv, Trees0),
    ?assertEqual(BaseStakeLimit, get_total_stake_limit(Alice, TxEnv, TreesOnline1)),

    {ok, TreesOnline2, {tuple, {}}} = set_validator_online_(Alice, TxEnv, TreesOnline1),
    ?assertEqual(BaseStakeLimit, get_total_stake_limit(Alice, TxEnv, TreesOnline2)),
    TreesOnline3 = check_total_stake_limit_cases(Alice, Sam, InitialStake, TxEnv, TreesOnline2),

    %% Validator going offline does not change staking limit
    PreOfflineStakeLimit = get_total_stake_limit(Alice, TxEnv, TreesOnline3),
    {ok, TreesOnline4, {tuple, {}}} = set_validator_offline_(Alice, TxEnv, TreesOnline3),
    ?assertEqual(PreOfflineStakeLimit, get_total_stake_limit(Alice, TxEnv, TreesOnline4)),

    %% Offline validator
    {ok, TreesOffline1, {contract, _}} = new_validator_(Alice, InitialStake, TxEnv, Trees0),
    ?assertEqual(BaseStakeLimit, get_total_stake_limit(Alice, TxEnv, TreesOffline1)),
    check_total_stake_limit_cases(Alice, Sam, InitialStake, TxEnv, TreesOffline1),

    ok.

stake_delay(_Config) ->
    StakeDelay = 11,
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS, #{stake_delay => StakeDelay}),

    ?assertMatch({ok, _, {tuple, {_, _, _, _, _, _, _, StakeDelay, _}}},
                 get_staking_contract_state_(Alice, TxEnv, Trees0)),

    InitialStake = ?VALIDATOR_MIN,

    % Bob is always online
    {ok, Trees1, {contract, _}} = new_validator_(Bob, InitialStake, TxEnv, Trees0),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees1),

    %% Offline validator
    {ok, Trees3, {contract, AliceCt}} = new_validator_(Alice, InitialStake, TxEnv, Trees2),
    check_stake_delay(Alice, AliceCt, InitialStake, StakeDelay, TxEnv, Trees3),

    %% Online validator
    {ok, Trees4, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees3),
    check_stake_delay(Alice, AliceCt, InitialStake, StakeDelay, TxEnv, Trees4),

    ok.

check_stake_delay(Alice, AliceCt, InitialStake, StakeDelay, TxEnv, Trees) ->
    Ct = staking_contract_address(),
    {Sam, Trees1} = set_up_account(Trees),
    {Paul, Trees2} = set_up_account(Trees1),

    SamStake = 100*?AE,
    Sam2Stake = 101*?AE,
    Paul2Stake = 102*?AE,

    %% no stakers
    {ok, _, {tuple, {_, _, _, AliceStake2, AlicePendingStake2, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, Trees2),
    ?assertEqual(InitialStake, AliceStake2),
    ?assertEqual(0, AlicePendingStake2),
    ?assertEqual(0, account_balance(Ct, Trees2)),
    ?assertEqual(InitialStake, account_balance(AliceCt, Trees2)),

    %% single staker with delay
    {ok, Trees3, _} = stake_(Alice, SamStake, Sam, TxEnv, Trees2),
    {ok, _, {tuple, {_, _, _, AliceStake3, AlicePendingStake3, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, Trees3),
    ?assertEqual(InitialStake, AliceStake3),
    ?assertEqual(SamStake, AlicePendingStake3),
    ?assertEqual(SamStake, account_balance(Ct, Trees3)),
    ?assertEqual(InitialStake, account_balance(AliceCt, Trees3)),

    %% two stakers with delay
    TxEnv0_1  = aetx_env:set_height(TxEnv, ?GENESIS_HEIGHT + 1),
    {ok, Trees4, _} = stake_(Alice, Sam2Stake, Sam, TxEnv0_1, Trees3),
    {ok, _, {tuple, {_, _, _, AliceStake4, AlicePendingStake4, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv0_1, Trees4),
    ?assertEqual(InitialStake, AliceStake4),
    ?assertEqual(SamStake+Sam2Stake, AlicePendingStake4),
    ?assertEqual(InitialStake, account_balance(AliceCt, Trees4)),

    {ok, Trees5, _} = stake_(Alice, Paul2Stake, Paul, TxEnv0_1, Trees4),
    {ok, _, {tuple, {_, _, _, AliceStake5, AlicePendingStake5, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv0_1, Trees5),
    ?assertEqual(InitialStake, AliceStake5),
    ?assertEqual(SamStake+Sam2Stake+Paul2Stake, AlicePendingStake5),
    ?assertEqual(SamStake+Sam2Stake+Paul2Stake, account_balance(Ct, Trees5)),
    ?assertEqual(InitialStake, account_balance(AliceCt, Trees5)),

    AssertTotalStake5 = case is_validator_online_(Alice, Alice, TxEnv, Trees5) of
                          {ok, _, true} -> 2*InitialStake; % alice + bob
                          {ok, _, false} -> InitialStake % bob
                        end,
    {ok, _, {tuple, {_, _, TotalStake5, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, Trees5),
    ?assertMatch(AssertTotalStake5, TotalStake5),

    %% stake distribution for single staker
    TxEnv1  = aetx_env:set_height(TxEnv, ?GENESIS_HEIGHT + StakeDelay),
    {ok, Trees6, _} = elect_(?OWNER_PUBKEY, TxEnv1, Trees5),
    {ok, _, {tuple, {_, _, _, AliceStake6, AlicePendingStake6, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv1, Trees6),
    ?assertEqual(InitialStake+SamStake, AliceStake6),
    ?assertEqual(Sam2Stake+Paul2Stake, AlicePendingStake6),
    ?assertEqual(Sam2Stake+Paul2Stake, account_balance(Ct, Trees6)),
    ?assertEqual(InitialStake+SamStake, account_balance(AliceCt, Trees6)),
    AssertTotalStake6 = case is_validator_online_(Alice, Alice, TxEnv1, Trees6) of
                          {ok, _, true} -> 2*InitialStake+SamStake; % alice + bob
                          {ok, _, false} -> InitialStake % bob
                        end,
    {ok, _, {tuple, {_, _, TotalStake6, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv1, Trees6),
    ?assertMatch(AssertTotalStake6, TotalStake6),

    % do not double spend
    {ok, Trees6_1, _} = elect_(?OWNER_PUBKEY, TxEnv1, Trees6),
    ?assertEqual(InitialStake+SamStake, account_balance(AliceCt, Trees6_1)),

    %% stake distribution for multiple stakers
    TxEnv2  = aetx_env:set_height(TxEnv, ?GENESIS_HEIGHT + StakeDelay + 1),
    {ok, Trees7, _} = elect_(?OWNER_PUBKEY, TxEnv2, Trees6),
    %{ok, _, AliceState7} = get_validator_state_(Alice, Alice, TxEnv2, Trees7),
    %?assertMatch({tuple, {_, _, _, ?VALIDATOR_MIN+303, 0, _, _, _}}, AliceState7),
    {ok, _, {tuple, {_, _, _, AliceStake7, AlicePendingStake7, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv2, Trees7),
    ?assertEqual(InitialStake+SamStake+Sam2Stake+Paul2Stake, AliceStake7),
    ?assertEqual(0, AlicePendingStake7),
    ?assertEqual(0, account_balance(Ct, Trees7)),
    ?assertEqual(InitialStake+SamStake+Sam2Stake+Paul2Stake, account_balance(AliceCt, Trees7)),
    AssertTotalStake7 = case is_validator_online_(Alice, Alice, TxEnv2, Trees7) of
                          {ok, _, true} -> 2*InitialStake+SamStake+Sam2Stake+Paul2Stake; %% alice + bob
                          {ok, _, false} -> InitialStake % bob
                        end,
    {ok, _, {tuple, {_, _, TotalStake7, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv2, Trees7),
    ?assertMatch(AssertTotalStake7, TotalStake7),
    ok.

stake_delay_respects_upper_stake_limit(_Config) ->
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS, #{stake_delay => 100}),
    Ct = staking_contract_address(),
    {Sam, Trees1} = set_up_account(Trees0),

    InitialStake = ?VALIDATOR_MIN,
    SamStake = 100*?AE,

    % Online validator
    {ok, OnlineTrees2, {contract, AliceCt}} = new_validator_(Alice, InitialStake, TxEnv, Trees1),
    {ok, OnlineTrees3, {tuple, {}}} = set_validator_online_(Alice, TxEnv, OnlineTrees2),
    {ok, OnlineTrees4, _} = stake_(Alice, SamStake, Sam, TxEnv, OnlineTrees3),
    ?assertEqual(SamStake, account_balance(Ct, OnlineTrees4)),
    ?assertEqual(InitialStake, account_balance(AliceCt, OnlineTrees4)),

    StakeLimit = get_total_stake_limit(Alice, TxEnv, OnlineTrees4),
    ?assertMatch({revert, <<"Total stake limit exceeded">>},
                 stake_(Alice, StakeLimit, Sam, TxEnv, OnlineTrees4)),

    % Offline validator
    {ok, OfflineTrees2, {contract, _}} = new_validator_(Alice, InitialStake, TxEnv, Trees1),
    {ok, OfflineTrees3, _} = stake_(Alice, SamStake, Sam, TxEnv, OfflineTrees2),
    ?assertEqual(SamStake, account_balance(Ct, OfflineTrees3)),
    ?assertEqual(InitialStake, account_balance(AliceCt, OfflineTrees3)),

    StakeLimit = get_total_stake_limit(Alice, TxEnv, OfflineTrees3),
    ?assertMatch({revert, <<"Total stake limit exceeded">>},
                 stake_(Alice, StakeLimit, Sam, TxEnv, OfflineTrees3)),
    ok.

stake_delay_set_to_zero(_Config) ->
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS, #{stake_delay => 0}),
    Ct = staking_contract_address(),
    {Sam, Trees1} = set_up_account(Trees0),

    InitialStake = ?VALIDATOR_MIN,
    SamStake = 100*?AE,

    % Online validator
    {ok, OnlineTrees2, {contract, AliceCt}} = new_validator_(Alice, InitialStake, TxEnv, Trees1),
    {ok, OnlineTrees3, {tuple, {}}} = set_validator_online_(Alice, TxEnv, OnlineTrees2),
    {ok, OnlineTrees4, _} = stake_(Alice, SamStake, Sam, TxEnv, OnlineTrees3),
    ?assertEqual(0, account_balance(Ct, OnlineTrees4)),
    ?assertEqual(InitialStake+SamStake, account_balance(AliceCt, OnlineTrees4)),
    {ok, _, {tuple, {_, _, OnlineTotalStake, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OnlineTrees4),
    ?assertEqual(InitialStake+SamStake, OnlineTotalStake),

    % Offline validator
    {ok, OfflineTrees2, {contract, AliceCt}} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees1),
    {ok, OfflineTrees3, _} = stake_(Alice, SamStake, Sam, TxEnv, OfflineTrees2),
    ?assertEqual(0, account_balance(Ct, OfflineTrees3)),
    ?assertEqual(InitialStake+SamStake, account_balance(AliceCt, OfflineTrees3)),
    {ok, _, {tuple, {_, _, OfflineTotalStake, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OfflineTrees3),
    ?assertEqual(0, OfflineTotalStake),

    ok.

check_total_stake_limit_cases(Validator, Staker, InitialStake, TxEnv, Trees) ->
    BaseStakeLimit = calculate_total_stake_limit(InitialStake),

    %% Staker's stake does not change the staking limit
    Stake = 100*?AE,
    {ok, Trees3, _} = stake_(Validator, Stake, Staker, TxEnv, Trees),
    ?assertEqual(BaseStakeLimit, get_total_stake_limit(Validator, TxEnv, Trees3)),
    StakeToLimit = get_available_stake_limit(Validator, TxEnv, Trees3),

    %% Staker can not stake about the staking limit
    ?assertEqual({revert, <<"Total stake limit exceeded">>},
                 stake_(Validator, StakeToLimit+?AE, Staker, TxEnv, Trees3)),

    %% Staker can stake up to the staking limit
    {ok, Trees4, _} = stake_(Validator, StakeToLimit, Staker, TxEnv, Trees3),
    ?assertEqual(BaseStakeLimit, get_total_stake_limit(Validator, TxEnv, Trees4)),

    %% Validator stake does change the staking limit
    {ok, Trees5, _} = stake_(Validator, InitialStake, Validator, TxEnv, Trees4),
    NewStakeLimit = calculate_total_stake_limit(InitialStake+InitialStake),
    ?assert(BaseStakeLimit < NewStakeLimit),
    ?assertEqual(NewStakeLimit, get_total_stake_limit(Validator, TxEnv, Trees5)),

    %% Validator can stake about the staking limit
    NewStakeToLimit = get_available_stake_limit(Validator, TxEnv, Trees5),
    ?assertEqual({revert, <<"Total stake limit exceeded">>},
                 stake_(Validator, NewStakeToLimit+?AE, Staker, TxEnv, Trees5)),
    {ok, Trees6, _} = stake_(Validator, NewStakeToLimit+?AE, Validator, TxEnv, Trees5),
    IncreasedStakeLimit = get_total_stake_limit(Validator, TxEnv, Trees6),

    %% Staker unstake does not change the staking limit
    {ok, Trees7, _} = unstake_(Validator, 10*?AE, Staker, TxEnv, Trees6),
    ?assertEqual(IncreasedStakeLimit, get_total_stake_limit(Validator, TxEnv, Trees7)),

    %% Validator unstake does change the staking limit
    {ok, Trees8, _} = unstake_(Validator, InitialStake, Validator, TxEnv, Trees7),
    DecreasedStakeLimit = get_total_stake_limit(Validator, TxEnv, Trees8),
    ?assert(IncreasedStakeLimit > DecreasedStakeLimit),

    %% Reward distribution does change the staking limit
    {ok, Trees9, {tuple, {}}} = reward_(Validator, InitialStake, ?OWNER_PUBKEY, TxEnv, Trees8),
    ?assert(DecreasedStakeLimit < get_total_stake_limit(Validator, TxEnv, Trees9)),

    Trees9.

unstake_delay(_Config) ->
    UnstakeDelay = 101,
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS, #{unstake_delay => UnstakeDelay}),

    ?assertMatch({ok, _, {tuple, {_, _, _, _, _, _, _, _, UnstakeDelay}}},
                 get_staking_contract_state_(Alice, TxEnv, Trees0)),

    InitialStake = ?VALIDATOR_MIN,

    % Bob is always online
    {ok, Trees1, {contract, BobCt}} = new_validator_(Bob, InitialStake, TxEnv, Trees0),
    {ok, Trees2, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees1),

    %% Offline validator
    {ok, OfflineTrees, {contract, AliceCt}} = new_validator_(Alice, InitialStake, TxEnv, Trees2),
    check_unstake_delay(Alice, AliceCt, InitialStake, UnstakeDelay, TxEnv, OfflineTrees),

    %% Online validator
    {ok, OnlineTrees, {tuple, {}}} = set_validator_online_(Alice, TxEnv, OfflineTrees),
    check_unstake_delay(Alice, AliceCt, InitialStake, UnstakeDelay, TxEnv, OnlineTrees),

    %% unstake from two validators
    AliceStake = 1001*?AE,
    BobStake = 1002*?AE,
    AliceUnstake = 99*?AE,
    BobUnstake = 101*?AE,

    {Sam, Trees3} = set_up_account(OfflineTrees),
    {ok, Trees4, _} = stake_(Alice, AliceStake, Sam, TxEnv, Trees3),
    {ok, Trees5, _} = stake_(Bob, BobStake, Sam, TxEnv, Trees4),

    % main contract
    {ok, _, {tuple, {_, _, CtStake5, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, Trees5),
    ?assertEqual(InitialStake+BobStake, CtStake5), %% bob

    % alice contract
    {ok, _, {tuple, {_, _, _, AliceStake5, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, Trees5),
    ?assertEqual(InitialStake+AliceStake, AliceStake5),
    ?assertEqual(InitialStake+AliceStake, account_balance(AliceCt, Trees5)),

    % bob contract
    {ok, _, {tuple, {_, _, _, BobStake5, _, _, _, _}}}
        = get_validator_state_(Bob, Bob, TxEnv, Trees5),
    ?assertEqual(InitialStake+BobStake, BobStake5),
    ?assertEqual(InitialStake+BobStake, account_balance(BobCt, Trees5)),

    SamBalancePreUnstake = account_balance(Sam, Trees5),
    {ok, Trees6, _} = unstake_(Alice, AliceUnstake, Sam, TxEnv, Trees5),
    {ok, Trees7, _} = unstake_(Bob, BobUnstake, Sam, TxEnv, Trees6),
    SamBalancePostUnstake = account_balance(Sam, Trees7),
    ?assert(SamBalancePreUnstake > SamBalancePostUnstake),

    % main contract
    {ok, _, {tuple, {_, _, CtStake7, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, Trees7),

    ?assertEqual(InitialStake+BobStake-BobUnstake, CtStake7), %% bob

    % alice contract
    {ok, _, AliceState7} = get_validator_state_(Alice, Alice, TxEnv, Trees7),
    {tuple, {_, _, _, AliceStake7, _, _, _, AliceCtState7}} = AliceState7,
    {tuple, {_, _, AlicePendingUnstakeAmount7, AlicePendingUnstake7, _, _, _, _, _}} = AliceCtState7,
    ?assertEqual(InitialStake+AliceStake-AliceUnstake, AliceStake7),
    ?assertEqual(AliceUnstake, AlicePendingUnstakeAmount7),
    ?assertEqual(1, maps:size(AlicePendingUnstake7)),
    ?assertEqual([{tuple, {{address, Sam}, AliceUnstake}}],
                 maps:get(?GENESIS_HEIGHT+UnstakeDelay, AlicePendingUnstake7)),
    ?assertEqual(InitialStake+AliceStake, account_balance(AliceCt, Trees7)),

    % bob contract
    {ok, _, BobState7} = get_validator_state_(Bob, Bob, TxEnv, Trees7),
    {tuple, {_, _, _, BobStake7, _, _, _, BobCtState7}} = BobState7,
    {tuple, {_, _, BobPendingUnstakeAmount7, BobPendingUnstake7, _, _, _, _, _}} = BobCtState7,
    ?assertEqual(InitialStake+BobStake-BobUnstake, BobStake7),
    ?assertEqual(BobUnstake, BobPendingUnstakeAmount7),
    ?assertEqual(1, maps:size(BobPendingUnstake7)),
    ?assertEqual([{tuple, {{address, Sam}, BobUnstake}}],
                 maps:get(?GENESIS_HEIGHT+UnstakeDelay, BobPendingUnstake7)),
    ?assertEqual(InitialStake+BobStake, account_balance(BobCt, Trees7)),

    %% unstake distribution
    TxEnv1  = aetx_env:set_height(TxEnv, ?GENESIS_HEIGHT + UnstakeDelay),
    {ok, Trees8, _} = elect_(?OWNER_PUBKEY, TxEnv1, Trees7),

    % main contract
    {ok, _, {tuple, {_, _, CtStake8, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv1, Trees8),
    ?assertEqual(InitialStake+BobStake-BobUnstake, CtStake8), %% bob

    % alice contract
    {ok, _, AliceState8} = get_validator_state_(Alice, Alice, TxEnv1, Trees8),
    {tuple, {_, _, _, AliceStake8, _, _, _, AliceCtState8}} = AliceState8,
    {tuple, {_, _, AlicePendingUnstakeAmount8, AlicePendingUnstake8, _, _, _, _, _}} = AliceCtState8,
    ?assertEqual(InitialStake+AliceStake-AliceUnstake, AliceStake8),
    ?assertEqual(0, AlicePendingUnstakeAmount8),
    ?assertEqual(#{}, AlicePendingUnstake8),
    ?assertEqual(InitialStake+AliceStake-AliceUnstake, account_balance(AliceCt, Trees8)),

    % bob contract
    {ok, _, BobState8} = get_validator_state_(Bob, Bob, TxEnv1, Trees8),
    {tuple, {_, _, _, BobStake8, _, _, _, BobCtState8}} = BobState8,
    {tuple, {_, _, BobPendingUnstakeAmount8, BobPendingUnstake8, _, _, _, _, _}} = BobCtState8,
    ?assertEqual(InitialStake+BobStake-BobUnstake, BobStake8),
    ?assertEqual(0, BobPendingUnstakeAmount8),
    ?assertEqual(#{}, BobPendingUnstake8),
    ?assertEqual(InitialStake+BobStake-BobUnstake, account_balance(BobCt, Trees8)),

    ?assertEqual(AliceUnstake+BobUnstake, account_balance(Sam, Trees8) - SamBalancePostUnstake),
    ok.

check_unstake_delay(Alice, AliceCt, InitialStake, UnstakeDelay, TxEnv0, Trees0) ->
    {Sam, Trees1} = set_up_account(Trees0),
    {Paul, Trees2} = set_up_account(Trees1),

    SamStake = ?STAKE_MIN+(1000*?AE),
    PaulStake = ?STAKE_MIN+(1000*?AE),
    SamUnstake = 100*?AE,
    Sam2Unstake = 99*?AE,
    Paul2Unstake = 101*?AE,

    {ok, Trees3, _} = stake_(Alice, SamStake, Sam, TxEnv0, Trees2),
    {ok, Trees4, _} = stake_(Alice, PaulStake, Paul, TxEnv0, Trees3),

    {ok, _, CtState4} = get_staking_contract_state_(Alice, TxEnv0, Trees4),
    AssertTotalStake4 = case is_validator_online_(Alice, Alice, TxEnv0, Trees4) of
                          {ok, _, true} -> 2*InitialStake + SamStake + PaulStake; % alice + bob
                          {ok, _, false} -> InitialStake % bob
                        end,
    ?assertMatch({tuple, {_, _, AssertTotalStake4, _, _, _, _, _, _}}, CtState4),
    {ok, _, {tuple, {_, _, _, AliceStake4, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv0, Trees4),
    ?assertEqual(InitialStake+SamStake+PaulStake, AliceStake4),
    ?assertEqual(AliceStake4, account_balance(AliceCt, Trees4)),

    %% single unstake
    {ok, Trees5, _} = unstake_(Alice, SamUnstake, Sam, TxEnv0, Trees4),

    {ok, _, CtState5} = get_staking_contract_state_(Alice, TxEnv0, Trees5),
    AssertTotalStake5 = case is_validator_online_(Alice, Alice, TxEnv0, Trees5) of
                          {ok, _, true} -> 2*InitialStake + SamStake + PaulStake - SamUnstake; % alice + bob
                          {ok, _, false} -> ?VALIDATOR_MIN % bob
                        end,
    ?assertMatch({tuple, {_, _, AssertTotalStake5, _, _, _, _, _, _}}, CtState5),
    {ok, _, AliceState5} = get_validator_state_(Alice, Alice, TxEnv0, Trees5),
    {tuple, {_, _, _, AliceStake5, _, _, _, AliceCtState5}} = AliceState5,
    {tuple, {_, _, PendingUnstakeAmount5, PendingUnstake5, _, _, _, _, _}} = AliceCtState5,
    ?assertEqual(InitialStake + SamStake + PaulStake - SamUnstake, AliceStake5),
    ?assertEqual(SamUnstake, PendingUnstakeAmount5),
    ?assertEqual(1, maps:size(PendingUnstake5)),
    ?assertEqual([{tuple, {{address, Sam}, SamUnstake}}],
                 maps:get(?GENESIS_HEIGHT+UnstakeDelay, PendingUnstake5)),
    ?assertEqual(InitialStake + SamStake + PaulStake, account_balance(AliceCt, Trees5)),

    %% two unstakes
    TxEnv1  = aetx_env:set_height(TxEnv0, ?GENESIS_HEIGHT + 1),
    {ok, Trees6, _} = unstake_(Alice, Sam2Unstake, Sam, TxEnv1, Trees5),
    {ok, Trees7, _} = unstake_(Alice, Paul2Unstake, Paul, TxEnv1, Trees6),
    {ok, _, CtState7} = get_staking_contract_state_(Alice, TxEnv1, Trees7),
    AssertTotalStake7 = case is_validator_online_(Alice, Alice, TxEnv1, Trees7) of
                          {ok, _, true} -> 2*InitialStake
                                           + SamStake + PaulStake
                                           - SamUnstake - Sam2Unstake - Paul2Unstake; % alice + bob
                          {ok, _, false} -> InitialStake % bob
                        end,
    ?assertMatch({tuple, {_, _, AssertTotalStake7, _, _, _, _, _, _}}, CtState7),
    {ok, _, AliceState7} = get_validator_state_(Alice, Alice, TxEnv1, Trees7),
    {tuple, {_, _, _, AliceStake7, _, _, _, AliceCtState7}} = AliceState7,
    {tuple, {_, _, PendingUnstakeAmount7, PendingUnstake7, _, _, _, _, _}} = AliceCtState7,
    ?assertEqual(InitialStake + SamStake+PaulStake-SamUnstake-Sam2Unstake-Paul2Unstake, AliceStake7),
    ?assertEqual(SamUnstake+Sam2Unstake+Paul2Unstake, PendingUnstakeAmount7),
    ?assertEqual(2, maps:size(PendingUnstake7)),
    ?assertEqual([{tuple, {{address, Sam}, SamUnstake}}],
                 maps:get(?GENESIS_HEIGHT+UnstakeDelay, PendingUnstake7)),
    ?assertEqual(lists:sort([{tuple, {{address, Sam}, Sam2Unstake}},
                             {tuple, {{address, Paul}, Paul2Unstake}}]),
                 lists:sort(maps:get(?GENESIS_HEIGHT+UnstakeDelay+1, PendingUnstake7))),
    ?assertEqual(InitialStake+SamStake+PaulStake, account_balance(AliceCt, Trees7)),

    %% unstake distribution for single staker
    TxEnv2  = aetx_env:set_height(TxEnv1, ?GENESIS_HEIGHT + UnstakeDelay),
    {ok, Trees8, _} = elect_(?OWNER_PUBKEY, TxEnv2, Trees7),
    {ok, _, CtState8} = get_staking_contract_state_(Alice, TxEnv2, Trees8),
    AssertTotalStake8 = case is_validator_online_(Alice, Alice, TxEnv2, Trees8) of
                          {ok, _, true} -> 2*InitialStake
                                           + SamStake + PaulStake
                                           - SamUnstake - Sam2Unstake - Paul2Unstake; % alice + bob
                          {ok, _, false} -> InitialStake % bob
                        end,
    ?assertMatch({tuple, {_, _, AssertTotalStake8, _, _, _, _, _, _}}, CtState8),
    {ok, _, AliceState8} = get_validator_state_(Alice, Alice, TxEnv2, Trees8),
    {tuple, {_, _, _, AliceStake8, _, _, _, AliceCtState8}} = AliceState8,
    {tuple, {_, _, PendingUnstakeAmount8, PendingUnstake8, _, _, _, _, _}} = AliceCtState8,
    ?assertEqual(InitialStake + SamStake+PaulStake-SamUnstake-Sam2Unstake-Paul2Unstake, AliceStake8),
    ?assertEqual(Sam2Unstake+Paul2Unstake, PendingUnstakeAmount8),
    ?assertEqual(1, maps:size(PendingUnstake8)),
    ?assertEqual(lists:sort([{tuple, {{address, Sam}, Sam2Unstake}},
                             {tuple, {{address, Paul}, Paul2Unstake}}]),
                 lists:sort(maps:get(?GENESIS_HEIGHT+UnstakeDelay+1, PendingUnstake8))),
    ?assertEqual(InitialStake+SamStake+PaulStake-SamUnstake, account_balance(AliceCt, Trees8)),

    %% do not double spend
    {ok, Trees8_1, _} = elect_(?OWNER_PUBKEY, TxEnv2, Trees8),
    ?assertEqual(InitialStake+SamStake+PaulStake-SamUnstake, account_balance(AliceCt, Trees8_1)),

    %% unstake distribution for multiple stakers
    TxEnv3  = aetx_env:set_height(TxEnv2, ?GENESIS_HEIGHT + UnstakeDelay + 1),
    {ok, Trees9, _} = elect_(?OWNER_PUBKEY, TxEnv3, Trees8),
    {ok, _, CtState9} = get_staking_contract_state_(Alice, TxEnv3, Trees9),
    AssertTotalStake9 = case is_validator_online_(Alice, Alice, TxEnv3, Trees9) of
                          {ok, _, true} -> 2*InitialStake
                                           + SamStake + PaulStake
                                           - SamUnstake - Sam2Unstake - Paul2Unstake; % alice + bob
                          {ok, _, false} -> InitialStake % bob
                        end,
    ?assertMatch({tuple, {_, _, AssertTotalStake9, _, _, _, _, _, _}}, CtState9),
    {ok, _, AliceState9} = get_validator_state_(Alice, Alice, TxEnv3, Trees9),
    {tuple, {_, _, _, AliceStake9, _, _, _, AliceCtState9}} = AliceState9,
    {tuple, {_, _, PendingUnstakeAmount9, PendingUnstake9, _, _, _, _, _}} = AliceCtState9,
    ?assertEqual(InitialStake + SamStake+PaulStake-SamUnstake-Sam2Unstake-Paul2Unstake, AliceStake9),
    ?assertEqual(0, PendingUnstakeAmount9),
    ?assertEqual(#{}, PendingUnstake9),
    ?assertEqual(
        InitialStake + SamStake + PaulStake - SamUnstake - Sam2Unstake - Paul2Unstake,
        account_balance(AliceCt, Trees9)
    ),

    ok.

unstake_delay_set_to_zero(_Config) ->
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS, #{unstake_delay => 0}),
    {Sam, Trees1} = set_up_account(Trees0),

    InitialStake = ?VALIDATOR_MIN+(1000*?AE),
    SamStake = ?STAKE_MIN+(100*?AE),
    SamUnstake = 100*?AE,
    AliceUnstake = 100*?AE,

    {ok, Trees2, {contract, AliceCt}} = new_validator_(Alice, InitialStake, TxEnv, Trees1),

    % Online validator
    {ok, OnlineTrees3, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees2),

    {ok, OnlineTrees4, _} = stake_(Alice, SamStake, Sam, TxEnv, OnlineTrees3),
    {ok, _, {tuple, {_, _, OnlineCtStake4, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OnlineTrees4),
    {ok, _, {tuple, {_, _, _, OnlineAliceStake4, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, OnlineTrees4),
    ?assertEqual(InitialStake+SamStake, OnlineCtStake4),
    ?assertEqual(InitialStake+SamStake, OnlineAliceStake4),
    ?assertEqual(InitialStake+SamStake, account_balance(AliceCt, OnlineTrees4)),

    {ok, OnlineTrees5, _} = unstake_(Alice, SamUnstake, Sam, TxEnv, OnlineTrees4),
    {ok, _, {tuple, {_, _, OnlineCtStake5, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OnlineTrees5),
    {ok, _, {tuple, {_, _, _, OnlineAliceStake5, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, OnlineTrees5),
    ?assertEqual(InitialStake+SamStake-SamUnstake, OnlineCtStake5),
    ?assertEqual(InitialStake+SamStake-SamUnstake, OnlineAliceStake5),
    ?assertEqual(InitialStake+SamStake-SamUnstake, account_balance(AliceCt, OnlineTrees5)),

    {ok, OnlineTrees6, _} = unstake_(Alice, AliceUnstake, Alice, TxEnv, OnlineTrees5),
    {ok, _, {tuple, {_, _, OnlineCtStake6, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OnlineTrees6),
    {ok, _, {tuple, {_, _, _, OnlineAliceStake6, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, OnlineTrees6),
    ?assertEqual(InitialStake+SamStake-SamUnstake-AliceUnstake, OnlineCtStake6),
    ?assertEqual(InitialStake+SamStake-SamUnstake-AliceUnstake, OnlineAliceStake6),
    ?assertEqual(InitialStake+SamStake-SamUnstake-AliceUnstake, account_balance(AliceCt, OnlineTrees6)),

    % Offline validator
    {ok, OfflineTrees3, _} = stake_(Alice, SamStake, Sam, TxEnv, Trees2),
    {ok, _, {tuple, {_, _, OfflineCtStake3, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OfflineTrees3),
    {ok, _, {tuple, {_, _, _, OfflineAliceStake3, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, OfflineTrees3),
    ?assertEqual(0, OfflineCtStake3),
    ?assertEqual(InitialStake+SamStake, OfflineAliceStake3),
    ?assertEqual(InitialStake+SamStake, account_balance(AliceCt, OfflineTrees3)),

    {ok, OfflineTrees4, _} = unstake_(Alice, SamUnstake, Sam, TxEnv, OfflineTrees3),
    {ok, _, {tuple, {_, _, OfflineCtStake4, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OfflineTrees4),
    {ok, _, {tuple, {_, _, _, OfflineAliceStake4, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, OfflineTrees4),
    ?assertEqual(0, OfflineCtStake4),
    ?assertEqual(InitialStake+SamStake-SamUnstake, OfflineAliceStake4),
    ?assertEqual(InitialStake+SamStake-SamUnstake, account_balance(AliceCt, OfflineTrees4)),

    {ok, OfflineTrees5, _} = unstake_(Alice, AliceUnstake, Alice, TxEnv, OfflineTrees4),
    {ok, _, {tuple, {_, _, OfflineCtStake5, _, _, _, _, _, _}}}
        = get_staking_contract_state_(Alice, TxEnv, OfflineTrees5),
    {ok, _, {tuple, {_, _, _, OfflineAliceStake5, _, _, _, _}}}
        = get_validator_state_(Alice, Alice, TxEnv, OfflineTrees5),
    ?assertEqual(0, OfflineCtStake5),
    ?assertEqual(InitialStake+SamStake-SamUnstake-AliceUnstake, OfflineAliceStake5),
    ?assertEqual(InitialStake+SamStake-SamUnstake-AliceUnstake, account_balance(AliceCt, OfflineTrees5)),
    ok.

staking_without_delay_return_shares(_Config) ->
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS),
    {Sam, Trees1} = set_up_account(Trees0),
    {ok, Trees2, _} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees1),

    AliceStake = ?STAKE_MIN + 1*?AE,
    SamStake = ?STAKE_MIN + 2*?AE,

    {ok, _, {tuple, AliceState2}} = get_validator_state_(Alice, Alice, TxEnv, Trees2),
    {_, _, _, _, _, _, _, {tuple, AliceCtState2}} = AliceState2,
    {_, _, _, _, _, _, _, _, TotalShares2} = AliceCtState2,

    {ok, Trees3, AliceResp} = stake_(Alice, AliceStake, Alice, TxEnv, Trees2),
    {ok, _, {tuple, AliceState3}} = get_validator_state_(Alice, Alice, TxEnv, Trees3),
    {_, _, _, _, _, _, _, {tuple, AliceCtState3}} = AliceState3,
    {_, _, _, _, _, _, _, _, TotalShares3} = AliceCtState3,

    {ok, Trees4, SamResp} = stake_(Alice, SamStake, Sam, TxEnv, Trees3),
    {ok, _, {tuple, AliceState4}} = get_validator_state_(Alice, Alice, TxEnv, Trees4),
    {_, _, _, _, _, _, _, {tuple, AliceCtState4}} = AliceState4,
    {_, _, _, _, _, _, _, Delegates, TotalShares4} = AliceCtState4,

    ?assertEqual(AliceStake, AliceResp#staking_resp.shares),
    ?assertEqual(SamStake, SamResp#staking_resp.shares),

    ?assertEqual(?GENESIS_HEIGHT, AliceResp#staking_resp.execution_height),
    ?assertEqual(?GENESIS_HEIGHT, SamResp#staking_resp.execution_height),

    ?assertEqual(TotalShares3, TotalShares2 + AliceResp#staking_resp.shares),
    ?assertEqual(TotalShares4, TotalShares3 + SamResp#staking_resp.shares),
    ?assertEqual(TotalShares2 + AliceResp#staking_resp.shares, maps:get({address, Alice}, Delegates)),
    ?assertEqual(SamResp#staking_resp.shares, maps:get({address, Sam}, Delegates)),
    ok.

staking_with_delay_return_shares(_Config) ->
    StakeDelay = 10,
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS, #{stake_delay => StakeDelay}),
    {Sam, Trees1} = set_up_account(Trees0),
    {ok, Trees2, _} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees1),

    AliceStake = ?STAKE_MIN + 1*?AE,
    SamStake = ?STAKE_MIN + 2*?AE,

    {ok, _, {tuple, AliceState2}} = get_validator_state_(Alice, Alice, TxEnv, Trees2),
    {_, _, _, _, _, _, _, {tuple, AliceCtState2}} = AliceState2,
    {_, _, _, _, _, _, _, _, TotalShares2} = AliceCtState2,

    {ok, Trees3, AliceResp} = stake_(Alice, AliceStake, Alice, TxEnv, Trees2),
    {ok, _, {tuple, AliceState3}} = get_validator_state_(Alice, Alice, TxEnv, Trees3),
    {_, _, _, _, _, _, _, {tuple, AliceCtState3}} = AliceState3,
    {_, _, _, _, _, _, _, _, TotalShares3} = AliceCtState3,

    {ok, Trees4, SamResp} = stake_(Alice, SamStake, Sam, TxEnv, Trees3),
    {ok, _, {tuple, AliceState4}} = get_validator_state_(Alice, Alice, TxEnv, Trees4),
    {_, _, _, _, _, _, _, {tuple, AliceCtState4}} = AliceState4,
    {_, _, _, _, _, _, _, Delegates, TotalShares4} = AliceCtState4,

    ?assertEqual(AliceStake, AliceResp#staking_resp.shares),
    ?assertEqual(SamStake, SamResp#staking_resp.shares),

    ?assertEqual(?GENESIS_HEIGHT + StakeDelay, AliceResp#staking_resp.execution_height),
    ?assertEqual(?GENESIS_HEIGHT + StakeDelay, SamResp#staking_resp.execution_height),

    ?assertEqual(TotalShares3, TotalShares2),
    ?assertEqual(TotalShares4, TotalShares2),
    ?assertEqual(#{{address, Alice} => TotalShares2}, Delegates),
    ok.

unstaking_return_shares(_Config) ->
    UnstakeDelay = 10,
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS, #{unstake_delay => UnstakeDelay}),

    {ok, Trees1, _} = new_validator_(Alice, 2*?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees2, {tuple, {}}} = reward_(Alice, 2*?VALIDATOR_MIN, ?OWNER_PUBKEY, TxEnv, Trees1),
    {ok, _, UnstakeResp} = unstake_(Alice, ?VALIDATOR_MIN, Alice, TxEnv, Trees2),

    ?assertEqual(2*?VALIDATOR_MIN, UnstakeResp#staking_resp.stake),
    ?assertEqual(?VALIDATOR_MIN, UnstakeResp#staking_resp.shares),
    ?assertEqual(?GENESIS_HEIGHT+UnstakeDelay, UnstakeResp#staking_resp.execution_height),
    ok.

unstaking_below_minimum_stake(_Config) ->
    Alice = pubkey(?ALICE),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    Trees0 = genesis_trees(?POS),
    {Sam, Trees1} = set_up_account(Trees0),
    {ok, Trees, _} = new_validator_(Alice, ?VALIDATOR_MIN, TxEnv, Trees1),

    Reason = <<"Staker can not withdraw below the treshold">>,
    %% Offline validator
    {ok, OfflineTrees1, _} = stake_(Alice, ?STAKE_MIN + 1, Sam, TxEnv, Trees),
    ?assertMatch({ok, _, _}, unstake_(Alice, 1, Sam, TxEnv, OfflineTrees1)),
    ?assertEqual({revert, Reason}, unstake_(Alice, ?STAKE_MIN, Sam, TxEnv, OfflineTrees1)),
    ?assertMatch({ok, _, _}, unstake_(Alice, ?STAKE_MIN + 1, Sam, TxEnv, OfflineTrees1)),

    %% Online validator
    {ok, OnlineTrees1, {tuple, {}}} = set_validator_online_(Alice, TxEnv, Trees),
    {ok, OnlineTrees2, _} = stake_(Alice, ?STAKE_MIN + 1, Sam, TxEnv, OnlineTrees1),
    ?assertMatch({ok, _, _}, unstake_(Alice, 1, Sam, TxEnv, OnlineTrees2)),
    ?assertEqual({revert, Reason}, unstake_(Alice, ?STAKE_MIN, Sam, TxEnv, OnlineTrees2)),
    ?assertMatch({ok, _, _}, unstake_(Alice, ?STAKE_MIN + 1, Sam, TxEnv, OnlineTrees2)),

    ok.

genesis_trees_opts(Type, Key, Opts, Default) ->
    Value = maps:get(Key, Opts, Default),
    aefa_fate_code:encode_arg({Type, Value}).

genesis_trees(Consensus) ->
    genesis_trees(Consensus, #{}).

genesis_trees(Consensus, Opts) ->
    ElectionContract =
        case Consensus of
            ?POS -> ?POS_ELECTION_CONTRACT;
            ?HC -> ?HC_ELECTION_CONTRACT
        end,
    Trees0 = aec_trees:new_without_backend(),
    Trees1 = set_up_accounts(Trees0),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT),
    {ok, SVCD} =
        aeb_fate_abi:create_calldata("init",
                                     [aefa_fate_code:encode_arg({address, ?OWNER_PUBKEY}),
                                      genesis_trees_opts(integer, unstake_delay, Opts, ?UNSTAKE_DELAY)
                                     ]),
    {SVPubkey, Trees2} = create_contract("StakingValidator", SVCD, TxEnv, Trees1),
    {ok, MainCD} = aeb_fate_abi:create_calldata("init",
                       [genesis_trees_opts(contract,  validator_ct, Opts, SVPubkey),
                        genesis_trees_opts(integer,   min_stake,    Opts, ?VALIDATOR_MIN),
                        genesis_trees_opts(integer,   min_percent,  Opts, ?VALIDATOR_MIN_PERCENT),
                        genesis_trees_opts(integer,   stake_min,    Opts, ?STAKE_MIN),
                        genesis_trees_opts(integer,   online_delay, Opts, ?ONLINE_DELAY),
                        genesis_trees_opts(integer,   stake_delay,  Opts, ?STAKE_DELAY),
                        genesis_trees_opts(integer,   unstake_delay, Opts, ?UNSTAKE_DELAY)
                       ]),
    {StakingPubkey, Trees3} = create_contract(?STAKING_CONTRACT, MainCD, TxEnv, Trees2),
    %% assert expectation:
    StakingPubkey = staking_contract_address(),
    {ok, ElectionCD} = aeb_fate_abi:create_calldata("init",
                                              [aefa_fate_code:encode_arg({contract, StakingPubkey}),
                                               aefa_fate_code:encode_arg({string, ?ENTROPY})
                                              ]),
    {ElectionPubkey, Trees4} = create_contract(ElectionContract, ElectionCD, TxEnv, Trees3),
    %% assert expectation:
    ElectionPubkey = election_contract_address(),
    Trees4.

entropy_impacts_leader_election(_Config) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Carol = pubkey(?CAROL), %% will be offline
    Trees0 = genesis_trees(?HC),
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
    {ok, Trees3, {tuple, {}}} = set_validator_online_(Bob, TxEnv, Trees2),
    Hash =
        fun([C]) ->
            list_to_binary(lists:duplicate(32, C));
           (S) when length(S) =:= 32 ->
            list_to_binary(S)
        end,
    Entropy1 = Hash("A"),
    {ok, Trees4, {tuple, {}}} = hc_elect_(Entropy1, ?OWNER_PUBKEY, TxEnv, Trees3),
    {ok, _, {address, Alice}} = leader_(?OWNER_PUBKEY, TxEnv, Trees4),
    %% same context, different entropy leads to different leader
    Entropy2 = Hash("1"),
    {ok, Trees5, {tuple, {}}} = hc_elect_(Entropy2, ?OWNER_PUBKEY, TxEnv, Trees3),
    {ok, _, {address, Bob}} = leader_(?OWNER_PUBKEY, TxEnv, Trees5),
    ok.

set_up_accounts(Trees) ->
    lists:foldl(fun set_up_account/2,
                Trees,
                [ {?OWNER_PUBKEY, trunc(math:pow(10, 30))},
                  {pubkey(?ALICE), trunc(math:pow(10, 30))},
                  {pubkey(?BOB), trunc(math:pow(10, 30))},
                  {pubkey(?CAROL), trunc(math:pow(10, 30))}]).

set_up_account(Trees) ->
    #{public := Account} = enacl:sign_keypair(),
    Trees1 = set_up_account({Account, trunc(math:pow(10, 30))}, Trees),
    {Account, Trees1}.

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
  call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees, fun(X) -> X end).

call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees, TransformFun) ->
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
                    RespTrees = aect_call_state_tree:prune(Height, Trees2),
                    Resp = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
                    {ok, RespTrees, TransformFun(Resp)};
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

%% black magic: this relies that there are 2 contracts and that the validator
%% one is first, then the staking one is the second (having a nonce 2) and the
%% election one is the next one; this allows us not passing around the address
%% of the consensus contract between tests
validator_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 3).

test_elect_calls(StartHeight, GenerenationsCnt, TxEnv, StartTrees) ->
    lists:foldl(
        fun(Height, {TreesAccum1, Ls}) ->
            TxEnvPrev = aetx_env:set_height(TxEnv, Height - 1),
            {ok, _ , {address, ExpectedNextLeader}} = elect_next_(?OWNER_PUBKEY, TxEnvPrev, TreesAccum1),
            TxEnv1 = aetx_env:set_height(TxEnv, Height),
            {ok, TreesAccum2, {tuple, {}}} = elect_(?OWNER_PUBKEY, TxEnv1, TreesAccum1),
            {ok, _, {address, NextLeader}} = leader_(?OWNER_PUBKEY, TxEnv1, TreesAccum2),
            ?assertEqual(ExpectedNextLeader, NextLeader),
            Ls1 = maps:update_with(NextLeader, fun(X) -> X + 1 end, 1, Ls),
            {TreesAccum2, Ls1}
        end,
        {StartTrees, #{}},
        lists:seq(StartHeight, StartHeight + GenerenationsCnt - 1)).

% Total stake limit helpers
calculate_total_stake_limit(Stake) ->
    Stake * 100 div ?VALIDATOR_MIN_PERCENT.

get_total_stake_limit(Validator, TxEnv, Trees) ->
    {ok, _, State} = get_validator_state_(Validator, Validator, TxEnv, Trees),
    {tuple, {_, _, _, _, _, StakeLimit, _, _}} = State,
    StakeLimit.

get_available_stake_limit(Validator, TxEnv, Trees) ->
    {ok, _, State} = get_validator_state_(Validator, Validator, TxEnv, Trees),
    {tuple, {_, _, _, Stake, PendingStake, StakeLimit, _, _}} = State,
    StakeLimit-Stake-PendingStake.

%% contract call wrappers
new_validator_(Pubkey, Amount, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("new_validator", []),
    call_contract(ContractPubkey, Pubkey, CallData, Amount, TxEnv, Trees0).

staking_power_(Who, Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("staking_power",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

get_validator_state_(Who, Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("get_validator_state",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

get_staking_contract_state_(Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("get_state", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

get_election_contract_state_(Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("get_state", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

is_validator_online_(Who, Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("is_validator_online",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_validator_online_(Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_online", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_validator_offline_(Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_offline", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

online_validators_(Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("online_validators", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

offline_validators_(Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("offline_validators", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

elect_next_(Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("elect_next", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

elect_(Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("elect", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

hc_elect_(Entropy, Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("elect",
                                                  [aefa_fate_code:encode_arg({string,
                                                                              Entropy})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

leader_(Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("leader", []),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

reward_(Who, Amount, Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("reward",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees0).

stake_(Who, Amount, Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("stake",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who})]),
    call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees0, fun to_staking_resp/1).

unstake_(Who, Stakes, Caller, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("unstake",
                                                  [aefa_fate_code:encode_arg({address,
                                                                              Who}),
                                                   aefa_fate_code:encode_arg({integer,
                                                                              Stakes})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0, fun to_staking_resp/1).

set_name_(Name, Caller, TxEnv, Trees0) when is_list(Name) ->
    set_name_(list_to_binary(Name), Caller, TxEnv, Trees0);
set_name_(Name, Caller, TxEnv, Trees0) when is_binary(Name) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_validator_name",
                                                  [aefa_fate_code:encode_arg({string, Name})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_description_(Description, Caller, TxEnv, Trees0) when is_list(Description) ->
    set_description_(list_to_binary(Description), Caller, TxEnv, Trees0);
set_description_(Description, Caller, TxEnv, Trees0) when is_binary(Description) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_validator_description",
                                                  [aefa_fate_code:encode_arg({string, Description})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

set_avatar_(Avatar, Caller, TxEnv, Trees0) when is_list(Avatar) ->
    set_avatar_(list_to_binary(Avatar), Caller, TxEnv, Trees0);
set_avatar_(Avatar, Caller, TxEnv, Trees0) when is_binary(Avatar) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("set_validator_avatar_url",
                                                  [aefa_fate_code:encode_arg({string, Avatar})]),
    call_contract(ContractPubkey, Caller, CallData, 0, TxEnv, Trees0).

to_staking_resp({tuple, {Stake, Shares, Height}}) ->
  #staking_resp{
    stake = Stake,
    shares = Shares,
    execution_height = Height}.

expected_validator_state(PoolContract, ValidatorAddr, OnlineDelay,
                         TotalSPower, PendingStake, TotalSLimit, IsOnline, Name,
                         Description, Avatar, PoolMap) ->
    ContractPubkey = staking_contract_address(),
    Pool = maps:to_list(PoolMap),
    Shares =
        lists:foldl(
            fun({_, SPower}, AccumAmt) -> SPower + AccumAmt end,
            0,
            Pool),
    Map =
        maps:from_list(lists:map(
            fun({Address, SPower}) ->
                {{address, Address}, SPower}
            end,
            Pool)),
    {tuple,
          {{contract, PoolContract},
           {address, ValidatorAddr},
           OnlineDelay,
           TotalSPower,
           PendingStake,
           TotalSLimit,
           IsOnline,
           {tuple,
               {{address, ContractPubkey},
                0, 0, #{},
                Name, Description, Avatar,
                Map,
                Shares}}}}.

assert_equal_states(State1, State2) ->
    {tuple,
          {{contract, PoolContract1},
           {address, ValidatorAddr1},
           OnlineDelay1,
           TotalSPower1,
           PendingStake1,
           TotalSLimit1,
           IsOnline1,
           {tuple,
               {{address, ContractPubkey1},
                UnstakeDelay1, PendingUnstakeAmount1, PendingUnstake1,
                Name1, Description1, Avatar1,
                Map1,
                Shares1}}}} = State1,
    {tuple,
          {{contract, PoolContract2},
           {address, ValidatorAddr2},
           OnlineDelay2,
           TotalSPower2,
           PendingStake2,
           TotalSLimit2,
           IsOnline2,
           {tuple,
               {{address, ContractPubkey2},
                UnstakeDelay2, PendingUnstakeAmount2, PendingUnstake2,
                Name2, Description2, Avatar2,
                Map2,
                Shares2}}}} = State2,
    ?assertEqual(PoolContract1, PoolContract2),
    ?assertEqual(ValidatorAddr1, ValidatorAddr2),
    ?assertEqual(OnlineDelay1, OnlineDelay2),
    ?assertEqual(TotalSPower1, TotalSPower2),
    ?assertEqual(PendingStake1, PendingStake2),
    ?assertEqual(TotalSLimit1, TotalSLimit2),
    ?assertEqual(IsOnline1, IsOnline2),
    ?assertEqual(ContractPubkey1, ContractPubkey2),
    ?assertEqual(UnstakeDelay1, UnstakeDelay2),
    ?assertEqual(PendingUnstakeAmount1, PendingUnstakeAmount2),
    ?assertEqual(PendingUnstake1, PendingUnstake2),
    ?assertEqual(Name1, Name2),
    ?assertEqual(Description1, Description2),
    ?assertEqual(Avatar1, Avatar2),
    ?assertEqual(Map1, Map2),
    ?assertEqual(Shares1, Shares2),
    ok.
