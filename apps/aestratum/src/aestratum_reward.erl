%%%-------------------------------------------------------------------
%%% @author  <x@x1>
%%% @copyright (C) 2019,
%%% @doc
%%%
%%% Aeternity specific PPLNS reward scheme
%%%
%%% @end
%%% Created : 15 Feb 2019 by  <x@x1>
%%%-------------------------------------------------------------------
-module(aestratum_reward).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% Mnesia
-export([check_tables/1,
         create_tables/1]).

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
-define(MIN_TX_FEE, 0). %% for now - revisit later... (in smallest denominated coin unit)
-define(SHARES_BATCH_LENGTH, 1000).

-define(LOG_INFO(Format, Args), io:format("REWARD [INFO]: " ++ Format ++ "\n", Args)).
-define(LOG_ERROR(Format, Args), io:format("REWARD [ERROR]: " ++ Format ++ "\n", Args)).

-define(HASHES_TAB, aestratum_hash).
-define(SHARES_TAB, aestratum_share).
-define(ROUNDS_TAB, aestratum_round).
-define(REWARDS_TAB, aestratum_reward).

-define(TABS, [?HASHES_TAB, ?SHARES_TAB, ?ROUNDS_TAB, ?REWARDS_TAB]).

%%%===================================================================
%%% PERSISTENT STATE
%%%===================================================================

-record(aestratum_hash,
        {hash   :: binary(),
         key    :: non_neg_integer()}).

-record(aestratum_share,
        {key    :: non_neg_integer(),
         hash   :: binary(),
         target :: non_neg_integer(), % miner's target
         miner  :: binary()}).        % public key

-record(aestratum_round,
        {key    :: non_neg_integer(),
         val    :: term()}).          % not used, mnesia needs it

-record(aestratum_reward,
        {height  :: non_neg_integer(),
         hash    :: binary(),
         pool    :: #{binary() => non_neg_integer()},
         miners  :: #{binary() => non_neg_integer()},
         unpaid  :: non_neg_integer(),
         round_key :: non_neg_integer()}).

%%%===================================================================
%%% MNESIA CREATE TABLE HOOKS (driven by aec_db)
%%%===================================================================

-define(TAB_DEF(Name, Type, Mode),
        {Name, [aec_db:tab_copies(Mode),
                {type, Type},
                {record_name, Name},
                {attributes, record_info(fields, Name)},
                {user_properties, [{vsn, table_vsn(Name)}]}]}).

create_tables(Mode) ->
    Specs = [table_specs(Tab, Mode) || {missing_table, Tab} <- check_tables([])],
    [{atomic, ok} = mnesia:create_table(Tab, Spec) || {Tab, Spec} <- Specs].

check_tables(Acc) ->
    lists:foldl(
      fun ({Tab, Spec}, Acc1) ->
              aec_db:check_table(Tab, Spec, Acc1)
      end, Acc, tables_specs(disc)).

table_specs(?HASHES_TAB, Mode) -> ?TAB_DEF(?HASHES_TAB, set, Mode);
table_specs(?SHARES_TAB, Mode) -> ?TAB_DEF(?SHARES_TAB, ordered_set, Mode);
table_specs(?ROUNDS_TAB, Mode) -> ?TAB_DEF(?ROUNDS_TAB, ordered_set, Mode);
table_specs(?REWARDS_TAB, Mode) -> ?TAB_DEF(?REWARDS_TAB, ordered_set, Mode).

tables_specs(Mode) -> [table_specs(Tab, Mode) || Tab <- ?TABS].

table_vsn(_) -> 1.

%%%===================================================================
%%% STATE
%%%===================================================================

-record(state,
        {last_n :: non_neg_integer(),
         benefs :: {TotalCutPcts :: number(), [{Account :: binary(), CutPcts :: float()}]}}).

-record(reward_key_block,
        {share_key :: non_neg_integer(),
         height    :: non_neg_integer(),
         target    :: non_neg_integer(),
         tokens    :: non_neg_integer(),
         hash      :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(LastN, Beneficiaries)
  when is_integer(LastN), LastN > 0, LastN =< ?MAX_ROUNDS,
       is_map(Beneficiaries) ->
    SumPcts = maps:fold(fun (_Account, PctShare, Sum) when PctShare > 0 ->
                                Sum + PctShare
                        end, 0, Beneficiaries),
    true = SumPcts < 100,
    Args = [LastN, {SumPcts, Beneficiaries}],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% Called by aestratum_chain process watching over keyblocks added to the chain.
keyblock() ->
    gen_server:cast(?MODULE, keyblock).

submit_share(Miner, MinerTarget, Hash) ->
    gen_server:cast(?MODULE, {submit_share, Miner, MinerTarget, Hash}).

confirm_payout(Height) ->
    gen_server:cast(?MODULE, {confirm_payout, Height}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LastN, {BeneficiariesPctShare, Beneficiaries}]) ->
    State = #state{last_n = LastN,
                   benefs = {BeneficiariesPctShare, Beneficiaries}},
    transaction(
      fun () ->
              mnesia:table_info(?ROUNDS_TAB, size) == 0 andalso store_round(),
              [aestratum_chain:payout_rewards(R#aestratum_reward.height,
                                              R#aestratum_reward.pool,
                                              R#aestratum_reward.miners) ||
                  R <- mnesia:match_object(#aestratum_reward{_ = '_'})]
      end),
    {ok, State}.


handle_cast(keyblock, State) ->
    transaction(fun () -> store_round() end),
    case maybe_compute_rewards(State) of
        {ok, #aestratum_reward{height = Height,
                               pool   = PoolRewards,
                               miners = MinersRewards}} ->
            aestratum_chain:payout_rewards(Height, PoolRewards, MinersRewards);
        {ok, not_our_share} ->
            ok;
        Error ->
            ?LOG_ERROR("failed to compute rewards: ~p", [Error])
    end,
    {noreply, State};

handle_cast({submit_share, Miner, MinerTarget, Hash}, State) ->
    transaction(fun () -> store_share(Miner, MinerTarget, Hash) end),
    {noreply, State};

handle_cast({confirm_payout, Height}, State) ->
    transaction(fun () -> delete_old_records(Height) end),
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

sort_key(_) ->
    abs(erlang:unique_integer([monotonic])).


store_share(Miner, MinerTarget, Hash) ->
    SortKey = sort_key(Hash),
    Share = #aestratum_share{key = SortKey,
                             hash = Hash,
                             target = MinerTarget,
                             miner = Miner},
    HashR = #aestratum_hash{hash = Hash,
                            key = SortKey},
    ok = mnesia:write(Share),
    ok = mnesia:write(HashR),
    {ok, Share, HashR}.

store_round() ->
    Round = #aestratum_round{key = sort_key(new_round)},
    ok = mnesia:write(Round),
    {ok, Round}.

store_reward(Height, Hash, BenefRewards, MinerRewards, TokensLeft, LastRoundKey) ->
    Reward = #aestratum_reward{height = Height,
                               hash = Hash,
                               pool = BenefRewards,
                               miners = MinerRewards,
                               unpaid = TokensLeft,
                               round_key = LastRoundKey},
    ok = mnesia:write(Reward),
    {ok, Reward}.


maybe_compute_rewards(State) ->
    on_reward_share(
      fun (#reward_key_block{height = Height, hash = Hash} = RewardKeyBlock) ->
              case compute_rewards(RewardKeyBlock, State) of
                  {ok, BenefRewards, MinerRewards, TokensLeft, LastRoundShareKey} ->
                      store_reward(Height, Hash,
                                   BenefRewards, MinerRewards,
                                   TokensLeft, LastRoundShareKey);
                  Error ->
                      Error
              end
      end).


on_reward_share(TxFun) ->
    case aestratum_chain:get_reward_key_header(?KEYBLOCK_ROUNDS_DELAY) of
        {ok, KeyHeader} ->
            Hash = aestratum_chain:hash_header(KeyHeader),
            transaction(
              fun () ->
                      case mnesia:read(?HASHES_TAB, Hash) of
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
        Error ->
            Error
    end.


compute_rewards(#reward_key_block{share_key = RewardShareKey,
                                   target = BlockTarget,
                                   tokens = BlockReward},
                #state{last_n = N,
                       benefs = {BenefSumPcts, Beneficiaries}}) ->
    case shares_range(RewardShareKey, N) of
        {ok, FirstShareKey, LastRoundShareKey} ->
            Selector = shares_selector(FirstShareKey, LastRoundShareKey),
            SliceCont = mnesia:select(?SHARES_TAB, Selector, ?SHARES_BATCH_LENGTH, read),
            {SumScores, MinerGroups} = sum_group_shares(SliceCont, BlockTarget),
            BenefTokens = round(BlockReward * (BenefSumPcts / 100)),
            {_, _, BenefTokensLeft, BenefRewards} =
                fold_rewards(BenefSumPcts, BenefTokens, Beneficiaries),
            MinerTokens = BlockReward - BenefTokens + BenefTokensLeft,
            {_, _, TokensLeft, MinerRewards} =
                fold_rewards(SumScores, MinerTokens, MinerGroups),
            {ok, BenefRewards, MinerRewards, TokensLeft, LastRoundShareKey};
        {error, Reason} ->
            {error, Reason}
    end.


shares_range(RewardShareKey, N) ->
    First = case mnesia:prev(?ROUNDS_TAB, RewardShareKey) of
                '$end_of_table' -> RewardShareKey;
                Val -> Val
            end,
    Selector = ets:fun2ms(fun (#aestratum_round{key = K} = R) when K >= First -> R end),
    case mnesia:select(?ROUNDS_TAB, Selector, N + 1, read) of
        {[_ | _] = Rounds, _Cont} ->
            #aestratum_round{key = Last} = lists:last(Rounds),
            {ok, First, Last};
        _ ->
            {error, no_range}
    end.


shares_selector(FirstKey, LastKey) ->
    ets:fun2ms(fun (#aestratum_share{key = SK} = Share)
                     when SK >= FirstKey, SK =< LastKey -> Share
               end).


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
    Score = MinerTarget / BlockTarget,
    Groups1 = case Groups of
                  #{Miner := TotalScore} ->
                      Groups#{Miner => TotalScore + Score};
                  #{} ->
                      Groups#{Miner => Score}
              end,
    {BlockTarget, SumScore + Score, Groups1}.


fold_rewards(SumScores, Tokens, Beneficiaries) ->
    maps:fold(fun reward/3, {SumScores, Tokens, Tokens, #{}}, Beneficiaries).

reward(_Account, _Score, {SumScores, BlockReward, 0, Rewards}) ->
    {SumScores, BlockReward, 0, Rewards};
reward(Account, Score, {SumScores, BlockReward, TokensLeft, Rewards}) ->
    RelScore = Score / SumScores,
    Tokens = min(round(RelScore * BlockReward), TokensLeft),
    %% io:format("/// SUM = ~g | SCORE = ~8f | GSCORE = ~8f | LEFT = ~5w | TOKENS = ~5w~n",
    %%           [SumScores * 1.0, Score * 1.0, RelScore, TokensLeft, Tokens]),
    if Tokens < ?MIN_TX_FEE ->
            {SumScores, BlockReward, TokensLeft, Rewards};
       true ->
            Rewards1 = maps:put(Account, Tokens, Rewards),
            {SumScores, BlockReward, TokensLeft - Tokens, Rewards1}
    end.


delete_old_records(Height) ->
    case mnesia:read(aestratum_reward, Height) of
        [#aestratum_reward{round_key = RoundKey}] ->
            RoundSpec = ets:fun2ms(fun (#aestratum_round{key = K}) when K > RoundKey -> K end),
            Rounds    = mnesia:select(?ROUNDS_TAB, RoundSpec),
            ShareSpec = ets:fun2ms(fun (#aestratum_share{key = K, hash = H}) when K > RoundKey ->
                                           {K, H}
                                   end),
            {Shares, Hashes} = lists:unzip(mnesia:select(?SHARES_TAB, ShareSpec)),
            [mnesia:delete(?SHARES_TAB, S, write) || S <- Shares],
            [mnesia:delete(?HASHES_TAB, H, write) || H <- Hashes],
            [mnesia:delete(?ROUNDS_TAB, R, write) || R <- Rounds],
            mnesia:delete(?REWARDS_TAB, Height, write),
            ok;
        [] ->
            %% this shouldn't happen, since rewards are deleted one by one
            ok
    end.


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
