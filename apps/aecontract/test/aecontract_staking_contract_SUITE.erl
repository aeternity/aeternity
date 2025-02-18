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
          deposit/1,
          stake/1,
          adjust_stake/1,
          withdraw/1,
          rewards/1,
          penalties/1,
          sorted_validators/1,
          check_withdraw/1
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

-define(GENESIS_HEIGHT, 0).
%% Height to start tests after contracts have been initialized
-define(DEFAULT_HEIGHT, 1).

-define(NETWORK_ID, <<"hc_stake_test">>).

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

-define(MAIN_STAKING_CT, "MainStaking").
-define(STAKING_VALIDATOR_CT, "StakingValidator").
-define(HC_ELECTION_CT, "HCElection").

-define(DEFAULT_GAS, 10000000).
-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(DEFAULT_FEE, 300000 * ?DEFAULT_GAS_PRICE).

-define(AE, 1000000000000000000). % 10^18
-define(VALIDATOR_MIN, 10 * ?AE). % 10 AE

-define(HC_EPOCH_LENGTH, 10).
-define(HC_PIN_REWARD, 4_711_000_000).

-define(UNIT, {tuple, {}}).

all() -> [{group, staking}
         ].

groups() ->
    [ {staking, [sequence],
       [ new_validator,
         deposit,
         stake,
         adjust_stake,
         withdraw,
         rewards,
         penalties,
         sorted_validators,
         check_withdraw
       ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    case aect_test_utils:latest_protocol_version() of
        PreIris when PreIris < ?CERES_PROTOCOL_VSN ->
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
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, _}}} =
        new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees0),
    {revert,<<"Owner must be unique">>} =
        new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees1),
    {revert,<<"A new validator must stake the minimum amount">>} =
        new_validator_(pubkey(?BOB), ?VALIDATOR_MIN - 1, TxEnv, Trees1),
    ok.

deposit(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, AliceCt}}} =
        new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees2, #{res := ?UNIT}} =
        deposit_(AliceCt, ?ALICE, 5 * ?AE, TxEnv, Trees1),

    {ok, _, #{res := TotalBalance}} =
        get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees2),
    {ok, _, #{res := AvailableBalance}} =
        get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees2),

    ?assertEqual(TotalBalance, ?VALIDATOR_MIN + 5 * ?AE),
    ?assertEqual(AvailableBalance, 5 * ?AE),

    ok.

stake(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, AliceCt}}} =
        new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees2, #{res := ?UNIT}} =
        stake_(AliceCt, ?ALICE, 5 * ?AE, TxEnv, Trees1),

    {ok, _, #{res := TotalBalance}} =
        get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees2),
    {ok, _, #{res := AvailableBalance}} =
        get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees2),

    ?assertEqual(TotalBalance, ?VALIDATOR_MIN + 5 * ?AE),
    %% staked isn't locked yet
    ?assertEqual(AvailableBalance, 5 * ?AE),

    {ok, Trees3, _} = lock_stake(2, TxEnv, Trees2),
    {ok, _, #{res := AvailableBalance2}} =
        get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
    ?assertEqual(AvailableBalance2, 0),


    ok.

adjust_stake(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, AliceCt}}} =
        new_validator_(pubkey(?ALICE), 2 * ?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees2, #{res := ?UNIT}} =
        adjust_stake_(AliceCt, ?ALICE, -2 * ?AE, TxEnv, Trees1, 1), %% Lock at 1 to "overwrite"

    {ok, _, #{res := TotalBalance}}     = get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees2),
    {ok, _, #{res := AvailableBalance}} = get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees2),
    ?assertEqual(TotalBalance,     2 * ?VALIDATOR_MIN),
    ?assertEqual(AvailableBalance, 2 * ?AE),

    {ok, Trees3, #{res := ?UNIT}} =
        adjust_stake_(AliceCt, ?ALICE, ?AE, TxEnv, Trees2, 1),

    {ok, _, #{res := TotalBalance2}}     = get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
    {ok, _, #{res := AvailableBalance2}} = get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
    ?assertEqual(TotalBalance2,     2 * ?VALIDATOR_MIN),
    ?assertEqual(AvailableBalance2, ?AE),

    ok.

withdraw(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, AliceCt}}} =
        new_validator_(pubkey(?ALICE), 2 * ?VALIDATOR_MIN, TxEnv, Trees0),

    {ok, Trees2, #{res := ?UNIT}} =
        adjust_stake_(AliceCt, ?ALICE, -2 * ?AE, TxEnv, Trees1, 1),

    {ok, Trees3, #{res := ?UNIT, cost := TxCost}} =
        withdraw_(AliceCt, ?ALICE, 2 * ?AE, TxEnv, Trees2),

    ?assertEqual(account_balance(pubkey(?ALICE), Trees3) - account_balance(pubkey(?ALICE), Trees2),
                 2 * ?AE - TxCost),

    ok.

rewards(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, AliceCt}}} =
        new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees0),
    {ok, Trees2, #{res := {contract, BobCt}}} =
        new_validator_(pubkey(?BOB), ?VALIDATOR_MIN, false, TxEnv, Trees1, undefined),

    Rewards = [{{address, pubkey(?ALICE)}, 4 * ?AE}, {{address, pubkey(?BOB)}, 3 * ?AE}],
    {ok, Trees3, #{res := ?UNIT}} =
        add_rewards_(1, Rewards, 7 * ?AE, TxEnv, Trees2, 2),

    {ok, _, #{res := ABalance}}   = get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
    {ok, _, #{res := AAvailable}} = get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
    {ok, _, #{res := BBalance}}   = get_total_balance_(BobCt, ?BOB, TxEnv, Trees3),
    {ok, _, #{res := BAvailable}} = get_available_balance_(BobCt, ?BOB, TxEnv, Trees3),

    ?assertEqual(ABalance, ?VALIDATOR_MIN + 4 * ?AE),
    ?assertEqual(BBalance, ?VALIDATOR_MIN + 3 * ?AE),

    ?assertEqual(AAvailable, 0),
    ?assertEqual(BAvailable, 3 * ?AE),

    ok.

    penalties(_Config) ->
        Trees0 = genesis_trees(),
        TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
        {ok, Trees1, #{res := {contract, AliceCt}}} =
            new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN, TxEnv, Trees0),
        {ok, Trees2, #{res := {contract, BobCt}}} =
            new_validator_(pubkey(?BOB), ?VALIDATOR_MIN, false, TxEnv, Trees1, undefined),

        {ok, _, #{res := BaseEmptyPool}} = get_penalty_pool_(TxEnv, Trees2),
        ?assertEqual(BaseEmptyPool, 0),

        Rewards = [{{address, pubkey(?ALICE)}, -4 * ?AE}, {{address, pubkey(?BOB)}, 2 * ?AE}],
        {ok, Trees3, #{res := ?UNIT}} =
            add_penalties_(1, Rewards, 0, 2 * ?AE, TxEnv, Trees2, 2),

        {ok, _, #{res := ABalance}}   = get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
        {ok, _, #{res := AAvailable}} = get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
        {ok, _, #{res := BBalance}}   = get_total_balance_(BobCt, ?BOB, TxEnv, Trees3),
        {ok, _, #{res := BAvailable}} = get_available_balance_(BobCt, ?BOB, TxEnv, Trees3),

        {ok, _, #{res := AfterRewardsPool}} = get_penalty_pool_(TxEnv, Trees3),
        ?assertEqual(AfterRewardsPool, 2 *? AE),

        ?assertEqual(ABalance, ?VALIDATOR_MIN - 4 * ?AE),
        ?assertEqual(BBalance, ?VALIDATOR_MIN + 2 * ?AE),

        ?assertEqual(AAvailable, 6 * ?AE),
        ?assertEqual(BAvailable, 2 * ?AE),

        ok.

sorted_validators(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, AliceCt}}} =
        new_validator_(pubkey(?ALICE), ?VALIDATOR_MIN + ?AE, TxEnv, Trees0),
    {ok, Trees2, #{res := {contract, _BobCt}}} =
        new_validator_(pubkey(?BOB), ?VALIDATOR_MIN + 2 * ?AE, TxEnv, Trees1),
    {ok, Trees3, #{res := {contract, CarolCt}}} =
        new_validator_(pubkey(?CAROL), ?VALIDATOR_MIN + 3 * ?AE, TxEnv, Trees2),

    {ok, _, #{res := Validators1}} =
        sorted_validators_(TxEnv, Trees3),

    ?assertEqual(decode_validators(Validators1),
                 [{carol, ?VALIDATOR_MIN + 3 * ?AE}, {bob, ?VALIDATOR_MIN + 2 * ?AE}, {alice, ?VALIDATOR_MIN + ?AE}]),

    {ok, Trees4, #{res := ?UNIT}} =
        adjust_stake_(AliceCt, ?ALICE, -2 * ?AE, TxEnv, Trees3),
    {ok, Trees5, #{res := ?UNIT}} =
        adjust_stake_(CarolCt, ?CAROL, -2 * ?AE, TxEnv, Trees4),

    {ok, _, #{res := Validators2}} =
        sorted_validators_(TxEnv, Trees5),

    ?assertEqual(decode_validators(Validators2),
                 [{bob, ?VALIDATOR_MIN + 2 * ?AE}, {carol, ?VALIDATOR_MIN + ?AE}]),
    ok.

check_withdraw(_Config) ->
    Trees0 = genesis_trees(),
    TxEnv = aetx_env:tx_env(?DEFAULT_HEIGHT),
    {ok, Trees1, #{res := {contract, AliceCt}}} =
        new_validator_(pubkey(?ALICE), 2 * ?VALIDATOR_MIN, TxEnv, Trees0),

    {ok, _, #{res := ABalance}}   = get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees1),
    {ok, _, #{res := AAvailable}} = get_available_balance_(AliceCt, ?ALICE, TxEnv, Trees1),
    ?assertEqual(ABalance, 2 * ?VALIDATOR_MIN),
    ?assertEqual(AAvailable, 0),

    %% This should unlock funds for withdrawal.
    {ok, Trees2, #{res := ?UNIT}} =
        add_rewards_(1, [], 0, TxEnv, Trees1, undefined),

    %% Withdraw and check that current balance is correctly adjusted
    {ok, _, #{res := ACurrStake1}} = get_current_stake_(AliceCt, ?ALICE, TxEnv, Trees2),
    ?assertEqual(ABalance, ACurrStake1),

    {ok, Trees3, #{res := ?UNIT}} =
        withdraw_(AliceCt, ?ALICE, 2 * ?AE, TxEnv, Trees2, undefined),

    {ok, _, #{res := ACurrStake2}} = get_current_stake_(AliceCt, ?ALICE, TxEnv, Trees3),
    {ok, _, #{res := ABalance2}} = get_total_balance_(AliceCt, ?ALICE, TxEnv, Trees3),
    ?assertEqual(ABalance2, 2 * ?VALIDATOR_MIN - 2 * ?AE),
    ?assertEqual(ABalance2, ACurrStake2),

    ok.

decode_validators(Vs) ->
    [ {vname(A), N} || {tuple, {{address, A}, N}} <- Vs ].

vname(Addr) ->
    case {pubkey(?ALICE), pubkey(?BOB)} of
        {Addr, _} -> alice;
        {_, Addr} -> bob;
        _         -> carol
    end.

genesis_trees_opts(Type, Key, Opts, Default) ->
    Value = maps:get(Key, Opts, Default),
    aefa_fate_code:encode_arg({Type, Value}).

genesis_trees() ->
    genesis_trees(#{}).

genesis_trees(Opts) ->
    ElectionContract = ?HC_ELECTION_CT,
    Trees0 = aec_trees:new_without_backend(),
    Trees1 = set_up_accounts(Trees0),
    TxEnv = aetx_env:tx_env(?GENESIS_HEIGHT, aect_test_utils:latest_protocol_version()),

    %% Main staking contract
    {ok, MainCD} = aeb_fate_abi:create_calldata("init",
                       [genesis_trees_opts(integer, min_stake, Opts, ?VALIDATOR_MIN)]),
    {StakingPubkey, Trees2} = create_contract(?MAIN_STAKING_CT, MainCD, TxEnv, Trees1),
    ?assertEqual(StakingPubkey, staking_contract_address()),

    %% HC Election contract
    {ok, ElectionCD} = aeb_fate_abi:create_calldata("init",
                          [aefa_fate_code:encode_arg({contract, StakingPubkey})]),
    {ElectionPubkey, Trees3} = create_contract(ElectionContract, ElectionCD, TxEnv, Trees2),
    ?assertEqual(ElectionPubkey, election_contract_address()),

    {ok, EpochInit} = aeb_fate_abi:create_calldata("init_epochs",
                          [genesis_trees_opts(integer, hc_epoch_length, Opts, ?HC_EPOCH_LENGTH),
                           genesis_trees_opts(integer, hc_pin_reward, Opts, ?HC_PIN_REWARD)]),
    {ok, Trees4, _} = call_contract(ElectionPubkey,  ?OWNER_PUBKEY, EpochInit, 0, TxEnv, Trees3),

    Trees4.

set_up_accounts(Trees) ->
    lists:foldl(fun set_up_account/2,
                Trees,
                [ {?OWNER_PUBKEY,  1_000_000_000_000 * ?AE},
                  {pubkey(?ALICE), 1_000_000_000_000 * ?AE},
                  {pubkey(?BOB),   1_000_000_000_000 * ?AE},
                  {pubkey(?CAROL), 1_000_000_000_000 * ?AE}]).

%% set_up_account(Trees) ->
%%     #{public := Account} = enacl:sign_keypair(),
%%     Trees1 = set_up_account({Account, trunc(math:pow(10, 30))}, Trees),
%%     {Account, Trees1}.

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
               gas         => ?DEFAULT_GAS,
               gas_price   => ?DEFAULT_GAS_PRICE,
               call_data   => CallData,
               fee         => ?DEFAULT_FEE},
    {ok, DummyTx} = aect_create_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    ct:log("Height ~p and consensus ~p\n", [Height, Protocol]),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_create_tx:new(TxSpec#{fee => MinFee}),
    %% Make sure the transaction will give the expected pubkey.
    case aect_contracts:compute_contract_pubkey(Owner, Nonce) of
        Pubkey -> Tx;
        Other  -> error({unexpected_pubkey, Other, Pubkey})
    end,
    Trees1 = aec_block_fork:prepare_contract_owner([Tx], TxEnv, Trees),
    {_, Trees2} = aec_block_fork:apply_contract_create_tx(Tx, Trees1, TxEnv),
    {Pubkey, Trees2}.

call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees, undefined) ->
    call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees);
call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees0, LockEpoch) when is_integer(LockEpoch) ->
    case call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees0) of
        {ok, Trees1, Res} ->
            {ok, Trees2, _} = lock_stake(LockEpoch, TxEnv, Trees1),
            {ok, Trees2, Res};
        Res ->
            Res
    end.

call_contract(ContractPubkey, Caller, CallData, Amount, TxEnv, Trees) ->
    Nonce = next_nonce(Caller, Trees),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    TxSpec = #{ caller_id    => aeser_id:create(account, Caller),
                contract_id  => aeser_id:create(contract, ContractPubkey),
                nonce        => Nonce,
                abi_version  => ABI,
                amount       => Amount,
                gas          => ?DEFAULT_GAS,
                gas_price    => ?DEFAULT_GAS_PRICE,
                call_data    => CallData,
                fee          => ?DEFAULT_FEE},
    {ok, DummyTx} = aect_call_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_call_tx:new(TxSpec#{fee => MinFee}),
    case aetx:process(Tx, Trees, aetx_env:set_dry_run(TxEnv, true)) of
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
                    Cost = aetx:fee(Tx) + aect_call:gas_price(Call) * aect_call:gas_used(Call),
                    {ok, RespTrees, #{res => Resp, cost => Cost}};
                error -> {error, aect_call:return_value(Call)};
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

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

%% TODO match logic of election with HCElection contract logic
%% test_elect_calls(StartHeight, GenerenationsCnt, TxEnv, StartTrees) ->
%%     Alice = pubkey(?ALICE),
%%     lists:foldl(
%%         fun(Height, {TreesAccum1, Ls}) ->
%%             TxEnvPrev = aetx_env:set_height(TxEnv, Height - 1),
%%             {ok, TreesAccum2, {tuple, {}}} = elect_(Alice, ?OWNER_PUBKEY, TxEnvPrev, TreesAccum1),
%%             TxEnv1 = aetx_env:set_height(TxEnv, Height),
%%             {ok, _, {address, NextLeader}} = leader_(?OWNER_PUBKEY, TxEnv1, TreesAccum2),
%%             ?assertEqual(Alice, NextLeader),
%%             Ls1 = maps:update_with(NextLeader, fun(X) -> X + 1 end, 1, Ls),
%%             {TreesAccum2, Ls1}
%%         end,
%%         {StartTrees, #{}},
%%         lists:seq(StartHeight, StartHeight + GenerenationsCnt - 1)).

create_calldata(Fun, Args0) ->
    Args = [ aefa_fate_code:encode_arg(Arg) || Arg <- Args0 ],
    {ok, CallData} = aeb_fate_abi:create_calldata(Fun, Args),
    CallData.

%% contract call wrappers
new_validator_(Pubkey, Amount, TxEnv, Trees0) ->
    new_validator_(Pubkey, Amount, true, TxEnv, Trees0, 1).

new_validator_(Pubkey, Amount, ReStake, TxEnv, Trees0, LockEpoch) ->
    ContractPubkey = staking_contract_address(),
    Args = [aefa_fate_code:encode_arg({address, Pubkey}),
            aefa_fate_code:encode_arg({address, Pubkey}),
            aefa_fate_code:encode_arg(ReStake)],
    {ok, CallData} = aeb_fate_abi:create_calldata("new_validator", Args),
    call_contract(ContractPubkey, Pubkey, CallData, Amount, TxEnv, Trees0, LockEpoch).

lock_stake(Epoch, TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("lock_stake", [aefa_fate_code:encode_arg({integer, Epoch})]),
    call_contract(ContractPubkey, ?OWNER_PUBKEY, CallData, 0, TxEnv, Trees0).

deposit_(ValidatorCt, Validator, Amount, TxEnv, Trees0) ->
    CallData = create_calldata("deposit", []),
    call_contract(ValidatorCt, pubkey(Validator), CallData, Amount, TxEnv, Trees0).

stake_(ValidatorCt, Validator, Amount, TxEnv, Trees0) ->
    stake_(ValidatorCt, Validator, Amount, TxEnv, Trees0, undefined).

stake_(ValidatorCt, Validator, Amount, TxEnv, Trees0, LockEpoch) ->
    CallData = create_calldata("stake", []),
    call_contract(ValidatorCt, pubkey(Validator), CallData, Amount, TxEnv, Trees0, LockEpoch).

adjust_stake_(ValidatorCt, Validator, Amount, TxEnv, Trees0) ->
    adjust_stake_(ValidatorCt, Validator, Amount, TxEnv, Trees0, undefined).

adjust_stake_(ValidatorCt, Validator, Amount, TxEnv, Trees0, LockEpoch) ->
    CallData = create_calldata("adjust_stake", [{integer, Amount}]),
    call_contract(ValidatorCt, pubkey(Validator), CallData, 0, TxEnv, Trees0, LockEpoch).

withdraw_(ValidatorCt, Validator, Amount, TxEnv, Trees0) ->
    withdraw_(ValidatorCt, Validator, Amount, TxEnv, Trees0, undefined).

withdraw_(ValidatorCt, Validator, Amount, TxEnv, Trees0, LockEpoch) ->
    CallData = create_calldata("withdraw", [{integer, Amount}]),
    call_contract(ValidatorCt, pubkey(Validator), CallData, 0, TxEnv, Trees0, LockEpoch).

get_total_balance_(ValidatorCt, Validator, TxEnv, Trees0) ->
    CallData = create_calldata("get_total_balance", []),
    call_contract(ValidatorCt, pubkey(Validator), CallData, 0, TxEnv, Trees0).

get_current_stake_(ValidatorCt, Validator, TxEnv, Trees0) ->
    CallData = create_calldata("get_current_stake", []),
    call_contract(ValidatorCt, pubkey(Validator), CallData, 0, TxEnv, Trees0).

get_available_balance_(ValidatorCt, Validator, TxEnv, Trees0) ->
    CallData = create_calldata("get_available_balance", []),
    call_contract(ValidatorCt, pubkey(Validator), CallData, 0, TxEnv, Trees0).

%% MainStaking

add_rewards_(Epoch, Rewards, TotRewards, TxEnv, Trees0, LockEpoch) ->
    Contract = staking_contract_address(),
    CallData = create_calldata("add_rewards", [{integer, Epoch}, Rewards, {integer, 0}]),
    call_contract(Contract, ?OWNER_PUBKEY, CallData, TotRewards, TxEnv, Trees0, LockEpoch).

add_penalties_(Epoch, Rewards, TotRewards, Pool, TxEnv, Trees0, LockEpoch) ->
    Contract = staking_contract_address(),
    CallData = create_calldata("add_rewards", [{integer, Epoch}, Rewards, {integer, Pool}]),
    call_contract(Contract, ?OWNER_PUBKEY, CallData, TotRewards, TxEnv, Trees0, LockEpoch).

sorted_validators_(TxEnv, Trees0) ->
    Contract = staking_contract_address(),
    CallData = create_calldata("sorted_validators", []),
    call_contract(Contract, ?OWNER_PUBKEY, CallData, 0, TxEnv, Trees0).

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

get_penalty_pool_(TxEnv, Trees0) ->
    ContractPubkey = staking_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("get_penalty_pool", []),
    call_contract(ContractPubkey, ?OWNER_PUBKEY, CallData, 0, TxEnv, Trees0).

%% We don't want "get_state" as an entry point
%% instead we return here the test relevant parts of the state
%% obtained via regular interface
%% - leader
%% - added_stake  (needed??)
%% - epoch info for present epoch
get_election_contract_state_(Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData1} = aeb_fate_abi:create_calldata("leader", []),
    {ok, _, Leader} = call_contract(ContractPubkey, Caller, CallData1, 0, TxEnv, Trees0),
    {ok, CallData2} = aeb_fate_abi:create_calldata("epoch_info", []),
    {ok, T, EpochInfo} = call_contract(ContractPubkey, Caller, CallData2, 0, TxEnv, Trees0),
    {ok, T, {tuple, Leader, EpochInfo}}.

election_staking_contract_(Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData} = aeb_fate_abi:create_calldata("staking_contract", []),
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

elect_(Leader, Caller, TxEnv, Trees0) ->
    ContractPubkey = election_contract_address(),
    {ok, CallData} =
        aeb_fate_abi:create_calldata("step",
                                     [aefa_fate_code:encode_arg({address, Leader})]),
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
