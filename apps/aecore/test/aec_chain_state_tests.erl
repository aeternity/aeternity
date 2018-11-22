%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_chain_state
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state_tests).

-include_lib("eunit/include/eunit.hrl").
-define(TEST_MODULE, aec_chain_state).
-define(FEES, 10).
-define(MINER_REWARD2, 401).
-define(POW_REWARD1, 20).

-define(B1_FEES, 4 * ?FEES div 10).
-define(B2_FEES, ?FEES - ?B1_FEES).

%%%===================================================================
%%% Test cases
%%%===================================================================

rewards_calculation_test_() ->
    [{"Both miners are honest, previous key block is not genesis -> no locked",
      fun() ->
          {_B1Amt  = ?B1_FEES,
           _B2Amt  = ?B2_FEES + ?MINER_REWARD2,
           _Locked = 0} =
              rewards(false, false, false)
      end},
     {"Both miners are honest, previous key block is genesis -> miner1 fees are locked",
      fun() ->
          {_B1Amt  = 0,
           _B2Amt  = ?B2_FEES + ?MINER_REWARD2,
           _Locked = ?B1_FEES} =
              rewards(false, false, true)
      end},
     {"Miner1 is fraudulent, miner2 is honest, previous key block is genesis -> miner1 fees are locked, miner2 gets pow reward",
      fun() ->
          {_B1Amt  = 0,
           _B2Amt  = ?B2_FEES + ?MINER_REWARD2 + ?POW_REWARD1,
           _Locked = ?B1_FEES - ?POW_REWARD1} =
              rewards(true, false, true)
      end},
     {"Miner1 is fraudulent, miner2 is honest, previous key block is not genesis -> miner1 fees are locked, miner2 gets pow reward",
      fun() ->
          {_B1Amt  = 0,
           _B2Amt  = ?B2_FEES + ?MINER_REWARD2 + ?POW_REWARD1,
           _Locked = ?B1_FEES - ?POW_REWARD1} =
              rewards(true, false, false)
      end},
     {"Miner2 is fraudulent, miner1 is honest, previous key block is genesis -> miner2 fees are locked but not the coinbase reward; miner1 fees are also locked",
      fun() ->
          {_B1Amt  = 0,
           _B2Amt  = 0,
           _Locked = ?B1_FEES + ?B2_FEES + ?MINER_REWARD2} =
              rewards(false, true, true)
      end},
     {"Miner2 is fraudulent, miner1 is honest, previous key block is not genesis -> miner2 fees are locked but not the coinbase reward",
      fun() ->
          {_B1Amt  = ?B1_FEES,
           _B2Amt  = 0,
           _Locked = ?B2_FEES + ?MINER_REWARD2} =
              rewards(false, true, false)
      end},
     {"Both miners are fraudulent -> fees are locked but not the coinbase reward",
      fun() ->
          {_B1Amt  = 0,
           _B2Amt  = 0,
           _Locked = ?B1_FEES + ?B2_FEES + ?MINER_REWARD2} =
              rewards(true, true, true),
          {_B1Amt  = 0,
           _B2Amt  = 0,
           _Locked = ?B1_FEES + ?B2_FEES + ?MINER_REWARD2} =
              rewards(true, true, false)
      end},
     {"Small fees's amount result in a negative lock",
      fun() ->
          BlockReward = 42,
          FraudReward = 7,
          Fees        = 0,
          {_B1Amt  = 0,
           _B2Amt  = 49,
           _Locked = -7} =
              ?TEST_MODULE:calc_rewards(true, false, Fees,
                                        BlockReward, FraudReward, false)
      end},
     {"All fraudulent miner's fees are locked",
      fun() ->
          % fraudulent miner mined keyblock K2
          % amount that should be locked is:
          %   + 60% from K1 generation
          %   + 40% from K2 generation
          %   + K2's coinbase reward
          %   - K2's fraud reward

          % k1
          K1Fees = 100,
          K1FraudReward = 35,
          % k2
          K2Fees = 120,
          K2Coinbase = 1003,
          K2FraudReward = 36,
          % k3
          K3Coinbase = 1007,
          {_, 0, Locked1} =
              ?TEST_MODULE:calc_rewards(false, true, K1Fees,
                                        K2Coinbase, K1FraudReward, false),
          ?assertEqual(Locked1, K1Fees * 6 div 10 + K2Coinbase),
          {0, _, Locked2} =
              ?TEST_MODULE:calc_rewards(true, false, K2Fees,
                                        K3Coinbase, K2FraudReward, false),
          Locked = Locked1 + Locked2,
          ExpectedLocked = (K1Fees * 6 div 10)
                         + (K2Fees * 4 div 10)
                         + K2Coinbase
                         - K2FraudReward,
          ?assertEqual(Locked, ExpectedLocked)
      end}].

rewards(K1Fraud, K2Fraud, IsK1Genesis) ->
    ?TEST_MODULE:calc_rewards(K1Fraud, K2Fraud, ?FEES, ?MINER_REWARD2,
                              ?POW_REWARD1, IsK1Genesis).

