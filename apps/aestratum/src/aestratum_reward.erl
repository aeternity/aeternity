%%%-------------------------------------------------------------------
%%% @author  <x@x1>
%%% @copyright (C) 2019,
%%% @doc
%%%
%%% Aeternity specific PPLNS reward scheme
%%% ==================================================
%%%
%%% Stratum server monitors the chain and looks for keyblock inclusion.
%%%
%%% Impact of a new keyblock included to the chain:
%%%
%%% 1.) difficulty of the network is changed
%%% 2.) all shares after keyblock from miners are ignored until Stratum broadcasts new difficulty to miners
%%%     (at the moment we assume that we find keyblock and learn new block difficulty/reward at the same time
%%%      - we may need to decouple these, and send "stop_mining" message (and ignore shares) if we learn
%%%        new block difficulty and reward after finding keyblock)
%%%
%%% Keyblock (winning share) splits the sequence of incoming shares into rounds.
%%%
%%% If keyblock is a winning share from out pool, we compute rewards, if not some other pool found it.
%%%
%%% We settle on number of rounds back we want to reward, typically 2 rounds.
%%%
%%% Since every included keyblock defines a round boundary, we can easily get all the shares
%%% for recent N = 2 rounds.
%%% We then sum all the scores of all the shares, group them by miner (each miner can provide many shares).
%%%
%%% Finally, we compute the rewards proportionally for each miner's sum of his shares score.
%%%
%%% @end
%%% Created : 15 Feb 2019 by  <x@x1>
%%%-------------------------------------------------------------------
-module(aestratum_reward).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/5,
         keyblock/3,
         submit_share/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-compile(export_all).

-define(MAX_ROUNDS, 10).
-define(MIN_TX_FEE, 100). %% in smallest denominated coin unit
-define(SHARES_BATCH_LENGTH, 1000).

-record(state,
        {hashes  :: ets:table(),
         shares  :: ets:table(),
         rounds  :: ets:table(),
         rewards :: ets:table(),
         last_n  :: non_neg_integer(),
         target  :: non_neg_integer(),
         benefs  :: {number(), [{Account :: binary(), float()}]}}).

-record(share,
        {key     :: non_neg_integer(),
         hash    :: binary(),
         target  :: non_neg_integer(), % miner's target
         miner   :: binary(), % public key ?
         score   :: float()}).

-record(round,
        {key     :: non_neg_integer(),
         hash    :: binary(),
         target  :: non_neg_integer(),
         height  :: non_neg_integer(),
         reward  :: non_neg_integer()}).

-record(reward,
        {hash    :: binary(),
         pool    :: #{binary() => non_neg_integer()},
         miners  :: #{binary() => non_neg_integer()},
         tokens_left :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(LastWinHash, Target, BlockReward, LastN, Beneficiaries)
  when is_integer(Target), Target > 0,
       is_integer(BlockReward), BlockReward > 0,
       is_integer(LastN), LastN > 0, LastN =< ?MAX_ROUNDS,
       is_map(Beneficiaries) ->
    SumPcts = maps:fold(fun (_Account, PctShare, Sum) when PctShare > 0 ->
                                Sum + PctShare
                        end, 0, Beneficiaries),
    true = SumPcts < 100,
    Args = [LastWinHash, Target, BlockReward, LastN, {SumPcts, Beneficiaries}],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% Called by some "chain monitor" process watching over keyblocks added to the chain.
%% Every keyblock starts new round, every round has new difficulty and reward.
keyblock(WinHash, NewTarget, NewBlockReward) ->
    gen_server:call(?MODULE, {keyblock, WinHash, NewTarget, NewBlockReward}).

%% Where does MinerDifficulty come from? We (Stratum) need to keep this difficulty,
%% can't come from miner itself.
submit_share(Miner, MinerTarget, Hash) ->
    gen_server:cast(?MODULE, {submit_share, Miner, MinerTarget, Hash}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LastWinHash, Target, BlockReward, LastN, {BeneficiariesPctShare, Beneficiaries}]) ->

    %% ETS for now, later it should be persistent
    Hashes  = ets:new(aestratum_hashes, [named_table, set]),
    Shares  = ets:new(aestratum_shares, [named_table, ordered_set, {keypos, 2}]),
    Rounds  = ets:new(aestratum_rounds, [named_table, ordered_set, {keypos, 2}]),
    Rewards = ets:new(aestratum_rewards, [named_table, ordered_set, {keypos, 2}]),

    State = #state{hashes  = Hashes,
                   shares  = Shares,
                   rounds  = Rounds,
                   rewards = Rewards,
                   last_n  = LastN,
                   target  = Target,
                   benefs  = {BeneficiariesPctShare, Beneficiaries}},

    store_round(LastWinHash, Target, BlockReward, State),

    {ok, State}.


handle_cast({submit_share, Miner, MinerTarget, Hash}, State) ->
    store_share(Miner, MinerTarget, Hash, State),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_call({keyblock, WinHash, NewTarget, NewBlockReward}, _From, State) ->
    store_round(WinHash, NewTarget, NewBlockReward, State),
    Reply = case ets:lookup(State#state.hashes, WinHash) of
                [{_, SortKey}] ->
                    case compute_rewards(SortKey, State) of
                        {ok, BenefRewards, MinerRewards, TokensLeft} ->
                            store_reward(WinHash, BenefRewards, MinerRewards,
                                         TokensLeft, State),
                            {ok, [{hash, WinHash},
                                  {pool_beneficiaries_len, maps:size(BenefRewards)},
                                  {miner_beneficiaries_len, maps:size(MinerRewards)},
                                  {tokens_left, TokensLeft}]};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                [] ->
                    not_our_hash
            end,
    {reply, Reply, State#state{target = NewTarget}};

handle_call(_, _From, State) ->
    {reply, nop, State}.


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

sort_key() ->
    abs(erlang:unique_integer()).


store_share(Miner, MinerTarget, Hash, State) ->
    SortKey = sort_key(),
    Score = MinerTarget / State#state.target,
    Share = #share{key = SortKey,
                   hash = Hash,
                   target = MinerTarget,
                   miner = Miner,
                   score = Score},
    ets:insert_new(State#state.shares, Share),
    %% Can already submitted Hash be overwritten?
    %% Maybe if we have bug in distributing work?
    ets:insert(State#state.hashes, {Hash, SortKey}).

store_round(WinHash, Target, BlockReward, State) ->
    ets:insert(State#state.rounds,
               #round{key = sort_key(),
                      hash = WinHash,
                      target = Target,
                      reward = BlockReward}).

store_reward(WinHash, BenefRewards, MinerRewards, TokensLeft, State) ->
    ets:insert(State#state.rewards,
               #reward{hash = WinHash,
                       pool = BenefRewards,
                       miners = MinerRewards,
                       tokens_left = TokensLeft}).


find_rounds(ShareKey, State) ->
    find_rounds(ShareKey, State#state.last_n, State).
find_rounds(ShareKey, N, State) when is_integer(N), N > 0 ->
    Spec = ets:fun2ms(fun (#round{key = K} = R) when K >= ShareKey -> R end),
    case ets:select(State#state.rounds, Spec, N) of
        {[_ | _] = Rounds, _Cont} ->
            {ok, Rounds};
        _ ->
            {error, no_round}
    end.


shares_selector(FirstKey, LastKey) ->
    ets:fun2ms(fun (#share{key = SK} = Share)
                     when SK > FirstKey, SK < LastKey -> Share
               end).

compute_rewards(WinShareKey, State) ->
    [#share{}] = ets:lookup(State#state.shares, WinShareKey),
    case find_rounds(WinShareKey, State) of
        {ok, [#round{reward = BlockReward} | _] = Rounds} ->
            FirstShareKey = WinShareKey,
            #round{key = LastShareKey} = lists:last(Rounds),
            Selector = shares_selector(FirstShareKey, LastShareKey),
            SliceCont = ets:select(State#state.shares, Selector, ?SHARES_BATCH_LENGTH),
            {SumScores, MinerGroups} = sum_group_shares(SliceCont),
            {BenefSumPcts, Beneficiaries} = State#state.benefs,
            BenefTokens = round(BlockReward * (BenefSumPcts / 100)),
            {_, _, _, BenefTokensLeft, BenefRewards} =
                fold_rewards(BenefSumPcts, BenefTokens, Beneficiaries),
            MinerTokens = BlockReward - BenefTokens + BenefTokensLeft,
            {_, _, _, TokensLeft, MinerRewards} =
                fold_rewards(SumScores, MinerTokens, MinerGroups),
            {ok, BenefRewards, MinerRewards, TokensLeft};
        {error, Reason} ->
            {error, Reason}
    end.

fold_rewards(SumScores, Tokens, Beneficiaries) ->
    maps:fold(fun reward/3, {SumScores, Tokens, Tokens, #{}}, Beneficiaries).


reward(Account, Score, {SumScores, BlockReward, TokensLeft, #{} = Rewards}) ->
    Fun = fun (Account0, Tokens, Rewards0) -> Rewards0#{Account0 => Tokens} end,
    reward(Account, Score, {Fun, SumScores, BlockReward, TokensLeft, Rewards});
reward(_Account, _Score, {Fun, SumScores, BlockReward, TokensLeft = 0, Rewards}) ->
    {Fun, SumScores, BlockReward, TokensLeft = 0, Rewards};
reward(Account, Score, {Fun, SumScores, BlockReward, TokensLeft, Rewards}) ->
    RelScore = Score / SumScores,
    Tokens = min(round(RelScore * BlockReward), TokensLeft),

    io:format("////////// SUM = ~g | SCORE = ~8f | GSCORE = ~8f | LEFT = ~5w | TOKENS = ~5w | TOO_SMALL? = ~p~n",
              [SumScores * 1.0, Score * 1.0, RelScore, TokensLeft, Tokens, Tokens < ?MIN_TX_FEE]),

    if Tokens < ?MIN_TX_FEE ->
            {Fun, SumScores, BlockReward, TokensLeft, Rewards};
       true ->
            Rewards1 = Fun(Account, Tokens, Rewards),
            {Fun, SumScores, BlockReward, TokensLeft - Tokens, Rewards1}
    end.


sum_group_shares(SliceCont) ->
    sum_group_shares(SliceCont, 0.0, #{}).

sum_group_shares('$end_of_table', SumScores, Groups) ->
    {SumScores, Groups};
sum_group_shares({Shares, Cont}, SumScores, Groups) ->
    {SumScores1, Groups1} = lists:foldl(fun sum_group_shares/2,
                                        {SumScores, Groups},
                                        Shares),
    sum_group_shares(ets:select(Cont), SumScores1, Groups1).


sum_group_shares(#share{miner = Miner, score = Score}, {SumScore, Groups}) ->
    Groups1 =
        case Groups of
            #{Miner := TotalScore} ->
                Groups#{Miner => TotalScore + Score};
            #{} ->
                Groups#{Miner => Score}
        end,
    {SumScore + Score, Groups1}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define(MAX_TARGET, 16#ffff000000000000000000000000000000000000000000000000000000000000).

-define(TEST_MINERS, [<<"MINER_A">>,<<"MINER_B">>,<<"MINER_C">>,<<"MINER_D">>,
                      <<"MINER_E">>,<<"MINER_F">>,<<"MINER_G">>,<<"MINER_H">>,
                      <<"MINER_I">>,<<"MINER_J">>,<<"MINER_K">>,<<"MINER_L">>,
                      <<"MINER_M">>,<<"MINER_N">>,<<"MINER_O">>,<<"MINER_P">>,
                      <<"MINER_Q">>,<<"MINER_R">>,<<"MINER_S">>,<<"MINER_T">>,
                      <<"MINER_U">>,<<"MINER_V">>,<<"MINER_W">>,<<"MINER_X">>,
                      <<"MINER_Y">>,<<"MINER_Z">>]).

t_miner_target(<<"MINER_", X:8>>) -> round(?MAX_TARGET / (2 * 0.4 * (X - 63))).

t_submits(NumSubmits) when NumSubmits > 0 ->
    NumMiners = length(?TEST_MINERS),
    RandMiner = fun () -> lists:nth(rand:uniform(NumMiners), ?TEST_MINERS) end,
    [begin
         Miner = RandMiner(),
         Hash = t_hash(Miner, X),
         submit_share(Miner, t_miner_target(Miner), Hash)
     end || X <- lists:seq(1, NumSubmits)].

t_hash(A, B) ->
    Uniq = io_lib:format("~p ~p ~p", [A, erlang:timestamp(), B]),
    crypto:hash(sha, iolist_to_binary(Uniq)).


%% BlockReward is in smallest denominated coin unit
t(SharesPerRound, Rounds, BlockReward, LastN, Beneficiaries) ->

    LastWinHash = undefined,
    Target = 115790322390251417039241401711187164934754157181743688420499462401711837020160, %% MAX TARGET

    start_link(LastWinHash, Target, BlockReward, LastN, Beneficiaries),

    [begin
         keyblock(t_hash(other, miner), Target, BlockReward),
         t_submits(SharesPerRound)
     end || _ <- lists:seq(1, Rounds)],
    Miner = <<"MINER_A">>,
    WinHash = t_hash(our, Miner),
    submit_share(Miner, t_miner_target(Miner), WinHash),
    keyblock(WinHash, Target, BlockReward).
