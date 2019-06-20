-module(aestratum_reward).

-export([relative_shares/2,
         relative_payments/1,
         relative_miner_rewards/2,
         absolute_amounts/2
        ]).

-include("aestratum.hrl").

%%%%%%%%%%

relative_shares(ScoredShares, SumScores) ->
    maps:map(fun (_, Score) -> Score / SumScores end, ScoredShares).


relative_payments(#aestratum_reward{} = Reward) ->
    relative_payments(Reward, ?PAYMENT_CONTRACT_BATCH_SIZE).
relative_payments(#aestratum_reward{height = Height, amount = TotalAmount,
                                    pool = PoolRelMap, miners = MinerRelMap},
                  BatchSize) ->
    PoolAmount   = round(TotalAmount * ?POOL_PERCENT_SUM / 100),
    MinersAmount = TotalAmount - PoolAmount,
    MinerBatches = aestratum_fn:idxs(aestratum_conv:map_to_chunks(MinerRelMap, BatchSize), 1),
    if PoolAmount >  0 -> [#aestratum_payment{id = {Height, 0},
                                              total = PoolAmount,
                                              relmap = PoolRelMap}];
       PoolAmount =< 0 -> []
    end ++ batches_to_payments(MinerBatches, MinersAmount, Height).


batches_to_payments(Batches, Amount, Height) ->
    {_, Payments} = lists:foldr(payment_distributor(Height, Amount),
                                {Amount, []}, Batches),
    Payments.


payment_distributor(Height, TotalAmount) ->
    distributor(TotalAmount,
                fun (I, BatchAmount, BatchRelMap, Acc) ->
                        [#aestratum_payment{id = {Height, I},
                                            total = BatchAmount,
                                            relmap = BatchRelMap} | Acc]
                end).

distributor(TotalAmount, F) ->
    fun ({I, BatchShares}, {TokensLeft, Acc}) ->
            BatchSharesSum = aestratum_fn:sum_values(BatchShares),
            case min(round(BatchSharesSum * TotalAmount), TokensLeft) of
                0 ->
                    {TokensLeft, Acc};
                BatchAmount ->
                    RelMap = relative_shares(BatchShares, BatchSharesSum),
                    Acc1   = F(I, BatchAmount, RelMap, Acc),
                    {TokensLeft - BatchAmount, Acc1}
            end
    end.



sum_group_shares(SliceCont, BlockTarget) ->
    sum_group_shares(SliceCont, BlockTarget, 0.0, #{}).

sum_group_shares('$end_of_table', _BlockTarget, SumScores, Groups) ->
    {SumScores, Groups};
sum_group_shares({Shares, Cont}, BlockTarget, SumScores, Groups) ->
    {_, SumScores1, Groups1} = lists:foldl(fun sum_group_share/2,
                                           {BlockTarget, SumScores, Groups},
                                           Shares),
    sum_group_shares(mnesia:select(Cont), BlockTarget, SumScores1, Groups1).

sum_group_share(#aestratum_share{miner = Miner, target = MinerTarget},
                {BlockTarget, SumScore, Groups}) ->
    Total = maps:get(Miner, Groups, 0),
    Score = MinerTarget / BlockTarget,
    {BlockTarget, SumScore + Score, Groups#{Miner => Total + Score}}.


absolute_amounts(NormalizedRelativeShares, Amount) ->
    MapSize = maps:size(NormalizedRelativeShares),
    {AbsoluteAmounts, 0, MapSize} =
        maps:fold(
          fun (Address, RelScore, {Res, TokensLeft, I}) ->
                  Tokens = if I + 1 == MapSize -> TokensLeft; % last iter, give all
                              true -> min(round(RelScore * Amount), TokensLeft)
                           end,
                  case Tokens of
                      0 -> {Res, TokensLeft, I + 1}; % could rounding cause this?
                      _ -> {Res#{Address => Tokens}, TokensLeft - Tokens, I + 1}
                  end
          end,
          {#{}, Amount, 0},
          NormalizedRelativeShares),
    AbsoluteAmounts.


relative_miner_rewards(RewardShareKey, BlockTarget) ->
    case aestratum_db:shares_range(RewardShareKey) of
        {ok, FirstShareKey, LastRoundShareKey} ->
            Selector  = aestratum_db:shares_selector(FirstShareKey, LastRoundShareKey),
            SliceCont = aestratum_db:shares_slices(Selector),
            {SumScores, Groups} = sum_group_shares(SliceCont, BlockTarget),
            {ok, relative_shares(Groups, SumScores), FirstShareKey};
        {error, Reason} ->
            {error, Reason}
    end.
