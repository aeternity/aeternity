%%%-------------------------------------------------------------------
%%% @author  <x@x1>
%%% @copyright (C) 2019,
%%% @doc
%%%
%%% Aeternity specific PPLNS reward scheme
%%% ==================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% NEW, SIMPLIFIED WAY
%%
%% Stratum server monitors the chain and looks for keyblock inclusion.
%%
%% Impact of a new keyblock included to the chain:
%%
%% 1.) difficulty of the network is changed
%% 2.) all shares after keyblock from miners are ignored until Stratum broadcasts new difficulty to miners
%%
%% Keyblock (winning share) splits the sequence of incoming shares into rounds.
%%
%% If keyblock is a winning share from out pool, we compute rewards, if not some other pool found it.
%%
%% We settle on number of rounds back we want to reward, typically it will be 2 rounds.
%%
%%
%% Since every included keyblock defines a round boundary, we can easily get all the shares
%% for recent N = 2 rounds.
%% We then sum all the scores of all the shares, group them by miner (each miner can provide many shares).
%%
%% Finally, we compute the rewards proportionally for each miner's sum of his shares score.
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% OLD WAY:
%%%
%%% We have:
%%%
%%% - variable Block Reward
%%% - variable Block Difficulty of the network (`D` in PPLNS topics)
%%% - variable difficulty for each miner, based on hers hash rate (`d`)
%%%
%%%
%%% Each miner in the pool has different hash rate. To avoid spamming
%%% of the Stratum server with shares coming from powerful miners,
%%% each miner mines at different difficulty d, where d < D.
%%%
%%% Assume miner A, mining on CPU, with difficulty from Stratum server set to `d`.
%%% Assume miner B, mining on GPU, which can mine shares 100x faster than A - hers
%%% difficulty should be set to 100 * d.
%%%
%%% This way, we should receive shares from different miners at roughly same rate.
%%%
%%% Since B mines shares with higher difficulty, closer to actual difficulty
%%% of the network D, the share from B has proportionally larger chance to be
%%% a winning share.
%%%
%%% Therefore reward for shares coming from A or B must be different, respecting
%%% the difficulty of the shares coming from miner (probability the share is winning).
%%%
%%%
%%% Some numbers:
%%%
%%% Assuming actual target of the network = 503866204 (SCI)
%%% (taken from https://explorer.aepps.com/#/ - view last / target)
%%%
%%% Difficulty = HIGHEST_TARGET / Target
%%%
%%% HIGHEST_TARGET = 115790322390251417039241401711187164934754157181743688420499462401711837020160 (INT)
%%%
%%% 4> aeminer_pow:scientific_to_integer(503866204).
%%% Target = 57892694225205930900326289850378044736072287423971459635164847815000064 (INT)
%%%
%%% Difficulty = 2000085.2256041213
%%%
%%% Means, we need roughly 2 Million of hash attempts to find a winning share (block).
%%%
%%% In PPLNS docs, to avoid pool-hopping, there's a constant X (usually set to 2) which
%%% specifies how far back we should distribute rewards.
%%%
%%% With X = 2 (under assumption that miners mine with the same difficulty `d`)
%%% rewards should span over 2 rounds (1 round = 1 mined keyblock), or roughly,
%%% that we should reward last 4 Million shares. (each miner could submit many shares)
%%%
%%%
%%% PROBLEM:
%%%
%%% As shown above, it's easy to come up with a number of `unit` shares to reward.
%%% But the assumption that each share has the same difficulty doesn't hold here.
%%%
%%% To simulate rewarding of latest N unit shares (4 Million in our case), we need
%%% to find out the share mined with smallest difficulty and scale this smallest
%%% difficulty ratio (min(d)/D) to 1.
%%%
%%% The ratio = 1 / (min(d)/D) over a sequence of "recent" shares allows mapping
%%% of share's score (d/D) to units.
%%%
%%% We could then walk the shares sequence from the latest, convert shares to
%%% unit shares and do so until the sum of unit shares so far exceeds N.
%%%
%%% Trouble is, we also don't know in what range do we need to search for
%%% smallest `d`.
%%%
%%% Another, smaller issue is that at some point we need to limit the number
%%% of shares in the DB. Not doing so would cause ever increasing storage,
%%% removing them too soon means not rewarding those shares which should be
%%% rewarded. Since it depends on multiple variables changing often how far
%%% back we go when counting in shares, there will need to be some conservative
%%% heuristics.
%%%
%%%
%%% SOLUTION:
%%%
%%% Assume we have a persistent sequence sorted from newest to oldest like this:
%%%
%%% SortKey | {MinerPubKey, `d`, `d/D`, ShareInfo}
%%% ---------+--------------------------------------------------------------
%%% T + ?   | ...
%%% ...     | ...
%%% T       | {M1-PK, 1 000 000, 0.001, ShareHashT}
%%% T - 1   | {M2-PK, 5 000 000, 0.005, ..}
%%% T - 2   | {M3-PK,   900 000, 0.000953, ...}         %% `D` changed here
%%% ...     | ...
%%%
%%% For each row:
%%% `d` is miner's difficulty at the time of submission
%%% `d/D` is score of the share at the time of submission
%%%
%%% Stratum server is accumulating shares and as they come in from miners, they
%%% are put in the table. (We also keep table which maps ShareHash -> SortKey.)
%%%
%%% With some delay, Stratum server learns about a new block added to the chain
%%% and we need to compute rewards.
%%%
%%% This block has ShareHashT, and we can find a position (SortKey) in shares table.
%%%
%%% We walk down, starting with SortKey = T - 1. (Winning share doesn't get any reward.)
%%%
%%% Initial state would look like this:
%%%
%%% min-d = 1 000 000
%%% amplifier = 1 / (d/D) (a number which when multiplied by score creates 1 unit share)
%%% sum-unit-shares = 1.0
%%% unit-shares = [{MinerPubKey, d/D}]
%%%
%%% Stop condition:
%%%
%%% 1.) Shares table ends. We should pay out only a portion of reward proportional to
%%%     sum-unit-shares / unit-shares (4 Mil)
%%%
%%% 2.) sum-unit-shares * amplifier > N
%%%
%%% When walking down and we find out that share has lower difficulty than seen so far:
%%% a.) set min-d
%%% b.) recompute amplifier
%%% c.) recompute sum-unit-shares = (old-sum-unit-shares * (old-amplifier / new-amplifier))
%%% d.) check new sum-unit-shares < N, if it holds continue
%%%
%%% @end
%%% Created : 15 Feb 2019 by  <x@x1>
%%%-------------------------------------------------------------------
-module(aestratum_reward_pplns).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/3,
         submit_share/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-compile(export_all).

-define(REWARD_ROUNDS, 2).
-define(SHARES_BATCH_LENGTH, 1000).

-record(state,
        {hashes       :: ets:table(),
         shares       :: ets:table(),
         chain_stats  :: ets:table(),
         difficulty   :: non_neg_integer()}).

-record(share,
        {key        :: non_neg_integer(),
         miner      :: binary(), % public key ?
         difficulty :: non_neg_integer(), % miner's difficulty
         score      :: float(),
         hash       :: binary()}).

-record(stat,
        {key          :: non_neg_integer(),
         winning_hash :: binary(),
         difficulty   :: non_neg_integer(),
         block_reward :: non_neg_integer()}).


%%%===================================================================
%%% API
%%%===================================================================

start_link(LastWinningHash, Difficulty, BlockReward)
  when is_integer(Difficulty), Difficulty > 0,
       is_integer(BlockReward), BlockReward > 0 ->
    Args = [LastWinningHash, Difficulty, BlockReward],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% Called by some "chain monitor" process watching over keyblocks added to the chain.
%% Every keyblock starts new round, every round has new difficulty and reward.
keyblock(WinningHash, NewDifficulty, NewBlockReward) ->
    gen_server:call(?MODULE, {keyblock, WinningHash, NewDifficulty, NewBlockReward}).

%% Where does MinerDifficulty come from? We (Stratum) need to keep this difficulty,
%% can't come from miner itself.
submit_share(Miner, MinerDifficulty, Hash) ->
    gen_server:cast(?MODULE, {submit_share, Miner, MinerDifficulty, Hash}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LastWinningHash, Difficulty, BlockReward]) ->

    %% ETS for now, later it should be persistent
    Hashes = ets:new(pplns_hashes, [named_table, set]),
    Shares = ets:new(pplns_shares, [named_table, ordered_set, {keypos, 2}]),
    Stats  = ets:new(pplns_chain_stats, [named_table, ordered_set, {keypos, 2}]),

    State = #state{hashes = Hashes,
                   shares = Shares,
                   chain_stats = Stats,
                   difficulty = Difficulty},

    store_chain_stat(LastWinningHash, Difficulty, BlockReward, State),

    {ok, State}.


%% handle_cast({new_round, Difficulty, BlockReward}, State) ->
%%     store_chain_stat(Difficulty, BlockReward, State),
%%     {noreply, State#state{difficulty = Difficulty}};


handle_cast({submit_share, Miner, MinerDifficulty, Hash}, State) ->
    store_share(Miner, MinerDifficulty, Hash, State),
    {noreply, State};


handle_cast(_Request, State) ->
    {noreply, State}.


handle_cast({keyblock, WinningHash, NewDifficulty, NewBlockReward}, State) ->
    store_chain_stat(WinningHash, NewDifficulty, NewBlockReward, State),
    case ets:lookup(State#state.hashes, WinningHash) of
        [{_, SortKey}] ->
            Rewards = compute_rewards(SortKey, State),
            {reply, Rewards, State#state{difficulty = NewDifficulty}};
        [] ->
            {reply, not_our_hash, State#state{difficulty = NewDifficulty}}
    end.


%% handle_call({rewards, WinningHash}, _From, State) ->
%%     Rewards = compute_rewards(WinningHash, State),
%%     {reply, Rewards, State}.


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


store_chain_stat(WinningHash, Difficulty, BlockReward, State) ->
    ets:insert(State#state.chain_stats,
               #stat{key = sort_key(),
                     winning_hash = WinningHash,
                     difficulty = Difficulty,
                     block_reward = BlockReward}).


store_share(Miner, MinerDifficulty, Hash, State) ->
    SortKey = sort_key(),
    Score = MinerDifficulty / State#state.difficulty,
    Share = #share{key = SortKey,
                   miner = Miner,
                   difficulty = MinerDifficulty,
                   score = Score,
                   hash = Hash},
    ets:insert_new(State#state.shares, Share),
    %% Can already submitted Hash be overwritten?
    %% Maybe if we have bug in distributing work?
    ets:insert(State#state.hashes, {Hash, SortKey}).


find_chain_stats(ShareKey, N, State) when N > 0 ->
    Spec = ets:fun2ms(fun (#stat{key = SK} = Stat) when SK > ShareKey -> Stat end),
    case ets:select(State#state.chain_stats, Spec, N) of
        {[_ | _] = Stats, _Cont} ->
            {ok, Stats};
        _ ->
            {error, no_stat}
    end.


shares_selector(FirstKey, LastKey) ->
    ets:fun2ms(fun (#share{key = SK} = Share)
                     when SK > FirstKey, SK < LastKey -> Share
               end).

compute_rewards(WinningShareKey, State) ->
    [#share{}] = ets:lookup(State#state.shares, WinningShareKey),
    case find_chain_stats(WinningShareKey, ?REWARD_ROUNDS, State) of
        {ok, [#stat{block_reward = BlockReward} | _] = ChainStats} ->
            FirstShareKey = WinningShareKey,
            #stat{key = LastShareKey} = lists:last(ChainStats),
            Selector = shares_selector(FirstShareKey, LastShareKey),
            SliceCont = ets:select(State#state.shares, Selector, ?SHARES_BATCH_LENGTH),
            {ok, MinerGroups, SumScores} = group_sum_scores(SliceCont),

            %% TODO: float rounding could end up rewarding more than is in BlockReward
            maps:fold(fun (Miner, Score, Rewards) ->
                              Rewards#{Miner => (Score / SumScores) * BlockReward}
                      end, #{}, MinerGroups);

        {error, Reason} ->
            {error, Reason}
    end.



group_sum_scores(SliceCont) ->
    group_sum_scores(SliceCont, #{}, 0.0).

group_sum_scores('$end_of_table', Groups, SumScores) ->
    {ok, Groups, SumScores};
group_sum_scores({Shares, Cont}, Groups, SumScores) ->
    {Groups1, SumScores1} = lists:foldl(fun group_sum_scores/2,
                                        {Groups, SumScores},
                                        Shares),
    group_sum_scores(ets:select(Cont), Groups1, SumScores1).


group_sum_scores(#share{miner = Miner, score = Score}, {Groups, SumScore}) ->
    Groups1 =
        case Groups of
            #{Miner := TotalScore} ->
                Groups#{Miner => TotalScore + Score};
            #{} ->
                Groups#{Miner => Score}
        end,
    {Groups1, SumScore + Score}.





%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TEST_MINERS, [<<"MINER_A">>,<<"MINER_B">>,<<"MINER_C">>,<<"MINER_D">>,
                      <<"MINER_E">>,<<"MINER_F">>,<<"MINER_G">>,<<"MINER_H">>,
                      <<"MINER_I">>,<<"MINER_J">>,<<"MINER_K">>,<<"MINER_L">>,
                      <<"MINER_M">>,<<"MINER_N">>,<<"MINER_O">>,<<"MINER_P">>,
                      <<"MINER_Q">>,<<"MINER_R">>,<<"MINER_S">>,<<"MINER_T">>,
                      <<"MINER_U">>,<<"MINER_V">>,<<"MINER_W">>,<<"MINER_X">>,
                      <<"MINER_Y">>,<<"MINER_Z">>]).

t_miner_diff(<<"MINER_", X:8>>) -> X - 60.

t_submits(NumSubmits) when NumSubmits > 0 ->
    NumMiners = length(?TEST_MINERS),
    RandMiner = fun () -> lists:nth(rand:uniform(NumMiners), ?TEST_MINERS) end,
    [begin
         Miner = RandMiner(),
         Uniq = io_lib:format("~p ~p ~p", [Miner, erlang:timestamp(), X]),
         Hash = crypto:hash(sha, iolist_to_binary(Uniq)),
         submit_share(Miner, t_miner_diff(Miner), Hash)
     end || X <- lists:seq(1, NumSubmits)].




%% t(Difficulty, BlockReward, NumShares, {MinMinerDifficulty, MaxMinerDifficulty}) ->
%%     NormMinerDifficulty = MaxMinerDifficulty - MinMinerDifficulty,
%%     MinerDifficulty = fun () -> MinMinerDifficulty + rand:uniform(NormMinerDifficulty) end,
%%     State0 = new(Difficulty, BlockReward),
%%     Shares = [{score(MinerDifficulty(), State0), Miner} ||
%%                  Miner <- lists:seq(1, NumShares)],
%%     WinScore = score(MinerDifficulty(), State0),
%%     io:format("////////// WinScore = ~p~n//////////SHARES = ~p~n", [WinScore, Shares]),
%%     State1 = lists:foldl(fun submit/2, State0, Shares),
%%     rewards(WinScore, State1).


%% %% t() ->
%% %%     Difficulty = 100,
%% %%     BlockReward = 100,

%% %%     State = new(Difficulty, BlockReward),

%% %%     ScoreA = score(10, State),
%% %%     ScoreB = score(13, State),
%% %%     ScoreC = score(12, State),
%% %%     ScoreD = score(25, State),
%% %%     ScoreE = score(65, State),
%% %%     ScoreF = score(32, State),
%% %%     ScoreC1 = score(12, State),
%% %%     ScoreB1 = score(13, State),
%% %%     ScoreF1 = score(32, State),

%% %%     WinScore = score(20, State),

%% %%     State1 = lists:foldl(fun submit/2,
%% %%                          State,
%% %%                          [{ScoreA, miner_a},
%% %%                           {ScoreB, miner_b},
%% %%                           {ScoreC, miner_c},
%% %%                           {ScoreD, miner_d},
%% %%                           {ScoreE, miner_e},
%% %%                           {ScoreF, miner_f},
%% %%                           {ScoreC1, miner_c},
%% %%                           {ScoreB1, miner_b},
%% %%                           {ScoreF1, miner_f}]),

%% %%     rewards(WinScore, State1).





%% compute_rewards(WinningHash, State) ->
%%     case find_first_share(WinningHash, State) of
%%         {ok, '$end_of_table'} ->
%%             {ok, []};
%%         {ok, FirstShareKey} ->
%%             case find_chain_stat(FirstShareKey, State) of
%%                 {ok, #stat{block_reward = BlockReward, difficulty = Difficulty}} ->
%%                     MaxUnitShares = trunc(Difficulty * ?MULTIPLIER),
%%                     LastShareKey = find_last_share(FirstShareKey, MaxUnitShares),

%%                     todo;

%%                 {error, Reson} ->
%%                     {error, Reson}
%%             end;
%%         {error, Reason} ->
%%             {error, Reason}
%%     end.




%% find_last_share(FirstShareKey, MaxUnitShares, State) ->
%%     [#share{score = Score, difficulty = MinD}] = ets:lookup(FirstShareKey),
%%     Spec = ets:fun2ms(fun (#score{key = SK} = Score) when SK > FirstShareKey -> Score end),
%%     SharesCont = ets:select(State#state.shares, Spec, ?SHARES_BATCH_LENGTH),
%%     find_last_share(SharesCont, MinD, 1 / Score, 1.0, MaxUnitShares, FirstShareKey).


%% find_last_share('$end_of_table', _MinD, _Amplifier, _SumUnitShares, _MaxUnitShares, Res) ->
%%     {ok, Res};
%% find_last_share({[], Cont}, MinD, Amplifier, SumUnitShares, MaxUnitShares, Res) ->
%%     find_last_share(ets:select(Cont), MinD, Amplifier, SumUnitShares, MaxUnitShares, Res);
%% find_last_share({[#share{score = Score1, difficulty = MinD1} = Share | Shares], Cont},
%%                 MinD, Amplifier, SumUnitShares, MaxUnitShares, Res)
%%   when MinD1 < MinD ->
%%     Amplifier1 = 1 / Score1,
%%     SumUnitShares1 = ((SumUnitShares / Amplifier) * Amplifier1) + 1.0,
%%     if SumUnitShares1 >= MaxUnitShares ->
%%             {ok, Res};
%%        true ->
%%             find_last_share({Shares, Cont},
%%                             MinD1, Amplifier1,
%%                             SumUnitShares1, MaxUnitShares,
%%                             Share#share.key)
%%     end;
%% find_last_share({[#share{score = Score1, difficulty = MinD1} = Share | Shares], Cont},
%%                 MinD, Amplifier, SumUnitShares, MaxUnitShares, Res) ->
