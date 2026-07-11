%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Dev-reward grant equivalence: with dev-reward active, splitting a
%%%    real reward and folding the legs into the account trees yields a
%%%    byte-identical root under batched vs immediate (flush-per-write)
%%%    writes. The second beneficiary is forced to the dev-reward
%%%    beneficiary key, so that key takes two grants in one unflushed
%%%    batch (same-key coalescing).
%%% @end
%%%=============================================================================
-module(aec_dev_reward_batch_equiv_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

equiv_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"batch == immediate for dev-reward grants, incl. a same-key "
        "(dev-beneficiary) double-grant in one batch",
        fun dev_reward_grant_batch_equiv/0}
     ]}.

%%%===================================================================
%%% Fixture — save/restore dev_reward env keys so this is safe to run
%%% alongside suites that mutate them.
%%%===================================================================

-define(ENV_KEYS, [dev_reward_enabled, dev_reward_activated,
                    dev_reward_allocated_shares, dev_reward_beneficiaries]).

setup() ->
    Saved = [ {K, application:get_env(aecore, K)} || K <- ?ENV_KEYS ],
    [ application:unset_env(aecore, K) || K <- ?ENV_KEYS ],
    ok = aec_test_utils:start_chain_db(),
    Saved.

teardown(Saved) ->
    ok = aec_test_utils:stop_chain_db(),
    lists:foreach(
      fun({K, undefined})  -> application:unset_env(aecore, K);
         ({K, {ok, V}})    -> application:set_env(aecore, K, V)
      end, Saved).

dev_reward_grant_batch_equiv() ->
    ok = aec_dev_reward:ensure_env(),
    ?assert(aec_dev_reward:enabled()),

    %% A real protocol version where the mainnet dev-beneficiary is
    %% active (Fortuna..Iris), from this run's own fork schedule.
    Protocols = maps:keys(aec_hard_forks:protocols()),
    Qualifying = [P || P <- Protocols,
                        P >= ?FORTUNA_PROTOCOL_VSN, P =< ?IRIS_PROTOCOL_VSN],
    case Qualifying of
        [] ->
            ?debugFmt(
               "~n~nCANNOT close the dev-reward-under-batching caveat from "
               "this run: no protocol between Fortuna(~p) and Iris(~p) is "
               "offered by network_id ~p (protocols: ~p). Re-run under "
               "`make TEST=~p eunit-fortuna` (or eunit-lima / eunit-iris).~n",
               [?FORTUNA_PROTOCOL_VSN, ?IRIS_PROTOCOL_VSN,
                aec_governance:get_network_id(), aec_hard_forks:protocols(),
                ?MODULE]);
        _ -> ok
    end,
    ?assertNotEqual([], Qualifying),
    ProtocolVsn = lists:max(Qualifying),
    ?assert(aec_dev_reward:activated(ProtocolVsn)),

    ActivationHeight = maps:get(ProtocolVsn, aec_hard_forks:protocols()),
    Height = ActivationHeight + 10,
    GenerationFees = 100000 * aec_test_utils:min_gas_price(),
    MineReward2 = aec_governance:block_mine_reward(Height),
    ?assert(MineReward2 > 0),
    {BeneficiaryReward1, BeneficiaryReward2, _LockAmount} =
        aec_chain_state:calc_rewards(false, false, GenerationFees, MineReward2, 0, false),
    {{AdjustedReward1, AdjustedReward2}, DevRewards} =
        aec_dev_reward:split(BeneficiaryReward1, BeneficiaryReward2, ProtocolVsn),

    %% Guard against a vacuous run: silently-inactive dev-reward would
    %% make the assertions below trivially true.
    ?assertNotEqual([], DevRewards),
    [{DevPK, DevAmt}] = DevRewards,
    ?assert(DevAmt > 0),
    ?assert(AdjustedReward1 > 0),
    ?assert(AdjustedReward2 > 0),
    %% value conservation across the split.
    ?assertEqual(BeneficiaryReward1 + BeneficiaryReward2,
                 AdjustedReward1 + AdjustedReward2 + DevAmt),

    %% Beneficiary2 is forced == DevPK, so the dev-beneficiary key takes
    %% two state_grant_reward calls inside one unflushed batch.
    PK1 = <<1:256>>,
    Beneficiary2 = DevPK,
    InitialBalance1 = 1000000000,
    Grants = [{PK1, AdjustedReward1}, {Beneficiary2, AdjustedReward2} | DevRewards],

    Trees0 = fund(PK1, InitialBalance1, aec_trees:new()),

    {HashBatch, BalBatch} = run(Trees0, Grants, batch),
    {HashImm,   BalImm}   = run(Trees0, Grants, immediate),

    ?assertEqual(HashImm, HashBatch),
    ?assertEqual(BalImm, BalBatch),

    %% Anchor to a concrete expected balance, so two identically-wrong
    %% runs can't both pass by merely agreeing.
    ExpectedPK1Balance = InitialBalance1 + AdjustedReward1,
    ExpectedDevPKBalance = AdjustedReward2 + DevAmt, %% two grants, same key
    ?assertEqual(lists:sort([{PK1, ExpectedPK1Balance},
                             {DevPK, ExpectedDevPKBalance}]),
                 BalBatch),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

fund(PK, Balance, Trees) ->
    AT = aec_trees:accounts(Trees),
    AT1 = aec_accounts_trees:enter(aec_accounts:new(PK, Balance), AT),
    aec_trees:set_accounts(Trees, aec_accounts_trees:flush_account_batch(AT1)).

run(Trees0, Grants, Mode) ->
    F = fun(T) ->
                case Mode of
                    immediate ->
                        aec_trees:set_accounts(
                          T, aec_accounts_trees:flush_account_batch(aec_trees:accounts(T)));
                    batch ->
                        T
                end
        end,
    TreesF = lists:foldl(
               fun({K, Amt}, TAcc) when Amt > 0 ->
                       F(aec_consensus_bitcoin_ng:state_grant_reward(K, undefined, TAcc, Amt));
                  (_, TAcc) ->
                       TAcc
               end, Trees0, Grants),
    Tf = aec_trees:flush_state_batches(TreesF),
    Hash = aec_trees:hash(Tf),
    Keys = lists:usort([K || {K, _} <- Grants]),
    Balances = lists:sort([ {K, balance(K, Tf)} || K <- Keys ]),
    {Hash, Balances}.

balance(PK, Trees) ->
    case aec_accounts_trees:lookup(PK, aec_trees:accounts(Trees)) of
        {value, Account} -> aec_accounts:balance(Account);
        none -> none
    end.
