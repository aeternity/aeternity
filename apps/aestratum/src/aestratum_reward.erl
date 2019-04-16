%%%-------------------------------------------------------------------
%%% @copyright (C) 2019,
%%% @doc
%%%
%%% Aeternity specific PPLNS reward scheme
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aestratum_reward).

-behaviour(gen_server).

-include("aestratum.hrl").
-include("aestratum_log.hrl").
-include_lib("aecore/include/blocks.hrl").

%% API
-export([start_link/2,
         keyblock/0,
         submit_share/3,
         confirm_payout/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-ifdef(COMMON_TEST).
-compile(export_all).
-endif.

-define(KEYBLOCK_ROUNDS_DELAY, 180).
-define(MAX_ROUNDS, 10).

-type reward_return() :: {ok, #aestratum_reward{} | not_our_share} |
                         {error, no_range | cant_get_top_key_block}.

%%%===================================================================
%%% STATE
%%%===================================================================

-record(state,
        {last_n :: non_neg_integer(),
         benefs :: {TotalCutPcts :: number(), #{miner_pubkey() => float()}}}).

-record(reward_key_block,
        {share_key :: aestratum_db:sort_key(),
         height    :: non_neg_integer(),
         target    :: non_neg_integer(),
         tokens    :: aestratum_db:amount(),
         hash      :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(LastN, {BenefSumPcts, Beneficiaries})
  when is_integer(LastN), LastN > 0, LastN =< ?MAX_ROUNDS,
       is_map(Beneficiaries) ->
    Args = [LastN, {BenefSumPcts, Beneficiaries}],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% Called by aestratum_chain process watching over keyblocks added to the chain.
keyblock() ->
    gen_server:cast(?MODULE, keyblock).

submit_share(<<"ak_", _/binary>> = Miner, MinerTarget, <<Hash/binary>>) ->
    true = is_integer(MinerTarget) andalso MinerTarget >= 0,
    gen_server:cast(?MODULE, {submit_share, Miner, MinerTarget, Hash}).

confirm_payout(Height) ->
    gen_server:cast(?MODULE, {confirm_payout, Height}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LastN, {BeneficiariesPctShare, Beneficiaries}]) ->
    State = #state{last_n = LastN,
                   benefs = {BeneficiariesPctShare, Beneficiaries}},
    transaction(fun () ->
                        aestratum_db:is_empty(?ROUNDS_TAB) andalso aestratum_db:store_round()
                end),
    {ok, State}.


handle_cast(keyblock, State) ->
    transaction(fun () -> aestratum_db:store_round() end),
    case maybe_compute_rewards(State) of
        {ok, #aestratum_reward{height = Height,
                               amount = Tokens,
                               pool = PoolRewards,
                               miners = MinersRewards}} ->
            aestratum_chain:payout_rewards(Height, Tokens, PoolRewards, MinersRewards);
        {ok, not_our_share} ->
            ok;
        {error, Reason} ->
            ?ERROR("failed to compute rewards: ~p", [Reason])
    end,
    {noreply, State};

handle_cast({submit_share, Miner, MinerTarget, Hash}, State) ->
    transaction(fun () -> aestratum_db:store_share(Miner, MinerTarget, Hash) end),
    {noreply, State};

handle_cast({confirm_payout, Height}, State) ->
    transaction(fun () -> aestratum_db:delete_reward_records(Height) end),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec maybe_compute_rewards(#state{}) -> reward_return().
maybe_compute_rewards(State) ->
    on_reward_share(
      fun (#reward_key_block{height = Height, hash = Hash, tokens = Amount} = RewardKeyBlock) ->
              case compute_rewards(RewardKeyBlock, State) of
                  {ok, BenefRewards, MinerRewards, LastRoundShareKey} ->
                      aestratum_db:store_reward(Height, Hash,
                                                BenefRewards, MinerRewards,
                                                Amount, LastRoundShareKey);
                  Error ->
                      Error
              end
      end).

-spec on_reward_share(fun((#reward_key_block{}) -> reward_return() | no_return())) ->
                             reward_return() | no_return().
on_reward_share(TxFun) ->
    case aestratum_chain:get_reward_key_header(?KEYBLOCK_ROUNDS_DELAY) of
        {ok, KeyHeader} ->
            Hash = aestratum_chain:hash_header(KeyHeader),
            transaction(
              fun () ->
                      case aestratum_db:get_hash(Hash) of
                          [#aestratum_hash{key = RewardShareKey}] ->
                              {ok, Height, Target, Tokens} =
                                  aestratum_chain:header_info({KeyHeader, Hash}),
                              TxFun(#reward_key_block{share_key = RewardShareKey,
                                                      height = Height,
                                                      target = Target,
                                                      tokens = Tokens,
                                                      hash = Hash});
                          [] ->
                              {ok, not_our_share}
                      end
              end);
        {error, Reason} ->
            {error, Reason}
    end.

-spec compute_rewards(#reward_key_block{}, #state{}) ->
                             {ok, map(), map(), aestratum_db:sort_key()} |
                             {error, no_range}.
compute_rewards(#reward_key_block{share_key = RewardShareKey, target = BlockTarget},
                #state{last_n = N,
                       benefs = {BenefSumPcts, Beneficiaries}}) ->
    case aestratum_db:shares_range(RewardShareKey, N) of
        {ok, FirstShareKey, LastRoundShareKey} ->
            Selector = aestratum_db:shares_selector(FirstShareKey, LastRoundShareKey),
            SliceCont = aestratum_db:shares_slices(Selector),
            {SumScores, MinerGroups} = sum_group_shares(SliceCont, BlockTarget),
            BenefRewards = fold_rewards(BenefSumPcts, Beneficiaries),
            MinerRewards = fold_rewards(SumScores, MinerGroups),
            {ok, BenefRewards, MinerRewards, LastRoundShareKey};
        {error, Reason} ->
            {error, Reason}
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


fold_rewards(SumScores, Beneficiaries) ->
    maps:fold(fun (BenefPK, Score, Rewards) ->
                      RelScore = Score / SumScores,
                      Rewards#{BenefPK => RelScore}
              end, #{}, Beneficiaries).

-spec transaction(fun (() -> any())) -> any() | no_return().
transaction(Fun) when is_function(Fun, 0) ->
    mnesia:activity(transaction, Fun).


-ifdef(COMMON_TEST).

-define(record_to_map(Tag, Datum), maps:from_list(lists:zip(record_info(fields, Tag), tl(tuple_to_list(Datum))))).

to_map(#aestratum_hash{} = X) -> ?record_to_map(aestratum_hash, X);
to_map(#aestratum_share{} = X) -> ?record_to_map(aestratum_share, X);
to_map(#aestratum_round{} = X) -> ?record_to_map(aestratum_round, X);
to_map(#aestratum_reward{} = X) -> ?record_to_map(aestratum_reward, X);
to_map(#reward_key_block{} = X) -> ?record_to_map(reward_key_block, X).

-endif.
