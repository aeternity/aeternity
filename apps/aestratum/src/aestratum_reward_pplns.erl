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
        {hashes :: ets:table(),
         shares :: ets:table(),
         rounds :: ets:table(),
         target :: non_neg_integer()}).

-record(share,
        {key    :: non_neg_integer(),
         miner  :: binary(), % public key ?
         target :: non_neg_integer(), % miner's difficulty
         score  :: float(),
         hash   :: binary()}).

-record(round,
        {key      :: non_neg_integer(),
         win_hash :: binary(),
         target   :: non_neg_integer(),
         reward   :: non_neg_integer()}).


%%%===================================================================
%%% API
%%%===================================================================

start_link(LastWinHash, Target, BlockReward)
  when is_integer(Target), Target > 0,
       is_integer(BlockReward), BlockReward > 0 ->
    Args = [LastWinHash, Target, BlockReward],
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

init([LastWinHash, Target, BlockReward]) ->

    %% ETS for now, later it should be persistent
    Hashes = ets:new(pplns_hashes, [named_table, set]),
    Shares = ets:new(pplns_shares, [named_table, ordered_set, {keypos, 2}]),
    Rounds = ets:new(pplns_rounds, [named_table, ordered_set, {keypos, 2}]),

    State = #state{hashes = Hashes,
                   shares = Shares,
                   rounds = Rounds,
                   target = Target},

    store_round(LastWinHash, Target, BlockReward, State),

    {ok, State}.



handle_cast({submit_share, Miner, MinerTarget, Hash}, State) ->
    store_share(Miner, MinerTarget, Hash, State),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_call({keyblock, WinHash, NewTarget, NewBlockReward}, _From, State) ->
    store_round(WinHash, NewTarget, NewBlockReward, State),
    case ets:lookup(State#state.hashes, WinHash) of
        [{_, SortKey}] ->
            Rewards = compute_rewards(SortKey, State),
            {reply, Rewards, State#state{target = NewTarget}};
        [] ->
            {reply, not_our_hash, State#state{target = NewTarget}}
    end;

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


store_round(WinHash, Target, BlockReward, State) ->
    ets:insert(State#state.rounds,
               #round{key = sort_key(),
                      win_hash = WinHash,
                      target = Target,
                      reward = BlockReward}).


store_share(Miner, MinerTarget, Hash, State) ->
    SortKey = sort_key(),
    Score = MinerTarget / State#state.target,
    Share = #share{key = SortKey,
                   miner = Miner,
                   target = MinerTarget,
                   score = Score,
                   hash = Hash},
    ets:insert_new(State#state.shares, Share),
    %% Can already submitted Hash be overwritten?
    %% Maybe if we have bug in distributing work?
    ets:insert(State#state.hashes, {Hash, SortKey}).


find_rounds(ShareKey, N, State) when N > 0 ->
    Spec = ets:fun2ms(fun (#round{key = K} = R) when K > ShareKey -> R end),
    case ets:select(State#state.rounds, Spec, N) of
        {[_ | _] = Rounds, _Cont} ->
            {ok, Rounds};
        _ ->
            {error, no_stat}
    end.


shares_selector(FirstKey, LastKey) ->
    ets:fun2ms(fun (#share{key = SK} = Share)
                     when SK > FirstKey, SK < LastKey -> Share
               end).

compute_rewards(WinShareKey, State) ->
    [#share{}] = ets:lookup(State#state.shares, WinShareKey),
    case find_rounds(WinShareKey, ?REWARD_ROUNDS, State) of
        {ok, [#round{reward = BlockReward} | _] = Rounds} ->
            FirstShareKey = WinShareKey,
            #round{key = LastShareKey} = lists:last(Rounds),
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


-define(MAX_TARGET, 16#ffff000000000000000000000000000000000000000000000000000000000000).

-define(TEST_MINERS, [<<"MINER_A">>,<<"MINER_B">>,<<"MINER_C">>,<<"MINER_D">>,
                      <<"MINER_E">>,<<"MINER_F">>,<<"MINER_G">>,<<"MINER_H">>,
                      <<"MINER_I">>,<<"MINER_J">>,<<"MINER_K">>,<<"MINER_L">>,
                      <<"MINER_M">>,<<"MINER_N">>,<<"MINER_O">>,<<"MINER_P">>,
                      <<"MINER_Q">>,<<"MINER_R">>,<<"MINER_S">>,<<"MINER_T">>,
                      <<"MINER_U">>,<<"MINER_V">>,<<"MINER_W">>,<<"MINER_X">>,
                      <<"MINER_Y">>,<<"MINER_Z">>]).

t_miner_target(<<"MINER_", X:8>>) -> ?MAX_TARGET / (X - 63).

t_submits(NumSubmits) when NumSubmits > 0 ->
    NumMiners = length(?TEST_MINERS),
    RandMiner = fun () -> lists:nth(rand:uniform(NumMiners), ?TEST_MINERS) end,
    [begin
         Miner = RandMiner(),
         Uniq = io_lib:format("~p ~p ~p", [Miner, erlang:timestamp(), X]),
         Hash = crypto:hash(sha, iolist_to_binary(Uniq)),
         submit_share(Miner, t_miner_target(Miner), Hash)
     end || X <- lists:seq(1, NumSubmits)].
