%%%=============================================================================
%%% @copyright 2024, Aeternity Anstalt
%%% @doc
%%%    Module handling sync statistics
%%%
%%%    Purpose is two-fold, it produces logs and provides (crude) estimates on
%%%    sync speed
%%% @end
%%%=============================================================================
-module(aec_sync_stats).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , new_sync/2
        , post_stats/2
        , sync_done/1
        , get_estimate/1 ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(sync_stat, { id
                   , target
                   , total
                   , chunks
                   , estimate
                   , last_update }).

-define(ESTIMATE_SIZE, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_sync(Id, Target) ->
    gen_server:cast(?MODULE, {new_sync, Id, Target}).

post_stats(Id, Stats) ->
    gen_server:cast(?MODULE, {post_stats, Id, Stats}).

sync_done(Id) ->
    gen_server:cast(?MODULE, {sync_done, Id}).

get_estimate(Id) ->
    gen_server:call(?MODULE, {get_estimate, Id}).

init([]) ->
    {ok, []}.

handle_call({get_estimate, Id}, _From, State) ->
    {reply, get_estimate(Id, State), State};

handle_call(_Msg, _From, State) ->
    epoch_sync:info("Unexpected call: ~p", [_Msg]),
    {reply, error, State}.

handle_cast({new_sync, Id, Target}, State) ->
    case is_sync(Id, State) of
        false ->
            {noreply, [new_sync_stat(Id, Target) | State]};
        true ->
            epoch_sync:info("Sync stat for ~p already in place, updating target to ~p", [Id, Target]),
            {noreply, update_target(Id, Target, State)}
    end;

handle_cast({post_stats, Id, Stats}, State) ->
    {noreply, do_post_stats(Id, Stats, State)};

handle_cast({sync_done, Id}, State) ->
    {noreply, terminate_sync(Id, State)};

handle_cast(_Msg, State) ->
    epoch_sync:info("Unexpected cast: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Msg, State) ->
    epoch_sync:info("Unexpected info: ~p", [_Msg]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------------
%% -- State modifiers
%% ------------------------------------------------------------------------
new_sync_stat(Id, Target) ->
    #sync_stat{ id = Id, target = Target,
                total = empty_agg(1, infinity),
                chunks = empty_aggs() }.

do_post_stats(Id, Stats, State) ->
    case get_sync_stat(Id, State) of
        SyncStat = #sync_stat{ total = TAgg, chunks = Aggs, estimate = Est } ->
            Aggs1 = add_agg_sync_stats(Aggs, Stats),
            {ok, TAgg1} = agg_sync_stats(TAgg, Stats),
            Est1 = add_estimate(Est, Stats),
            set_sync_stat(Id, SyncStat#sync_stat{ total = TAgg1, chunks = Aggs1, estimate = Est1 }, State);
        false ->
            epoch_sync:info("Add stats failed, no sync_stat for ~p", [Id]),
            State
    end.

update_target(Id, Target, State) ->
    case get_sync_stat(Id, State) of
        SyncStat = #sync_stat{ target = OldTarget } ->
            case OldTarget < Target of
                true ->
                    set_sync_stat(Id, SyncStat#sync_stat{ target = Target }, State);
                false ->
                    State
            end;
        false ->
            epoch_sync:info("Update target failed, no sync_stat for ~p", [Id]),
            State
    end.

terminate_sync(Id, State) ->
    case get_sync_stat(Id, State) of
        #sync_stat{ total = TAgg } ->
            log_agg_stats(TAgg),
            lists:keydelete(Id, #sync_stat.id, State);
        false ->
            epoch_sync:info("Termination of sync stat failed, no sync_stat for ~p", [Id]),
            State
    end.

get_estimate(Id, State) ->
    case get_sync_stat(Id, State) of
        #sync_stat{ estimate = Stats } ->
            #{t1 := T1} = hd(Stats),
            #{t0 := T0} = lists:last(Stats),
            Tx = ((T1 - T0) / ?ESTIMATE_SIZE) / 1_000_000,
            Speed = round(1000 * (60 / Tx)) / 1000,
            {ok, #{ speed => Speed }};
        false ->
            epoch_sync:info("Get estimate failed, no sync_stat for ~p", [Id]),
            {error, no_estimate}
    end.

add_estimate(undefined, Stats) ->
    lists:reverse(Stats);
add_estimate(Est, Stats) ->
    lists:sublist(lists:reverse(Stats) ++ Est, ?ESTIMATE_SIZE).

is_sync(Id, State) ->
    lists:keymember(Id, #sync_stat.id, State).

get_sync_stat(Id, State) ->
    lists:keyfind(Id, #sync_stat.id, State).

set_sync_stat(Id, SyncStat, State) ->
    lists:keystore(Id, #sync_stat.id, State, touch(SyncStat)).

touch(SyncStat) ->
    SyncStat#sync_stat{ last_update = erlang:system_time(millisecond) }.

%% -- Stats and aggregation -----------------------------------------------
empty_agg(Step, Limit) ->
    #{ t => 0, n => 0, gs => 0, step => Step, mbs => 0, txs => 0, limit => Limit }.

empty_aggs() ->
    [empty_agg(1, 1000), empty_agg(1000, 5), empty_agg(5000, 2), empty_agg(10000, 2), empty_agg(20000, 5)].

add_agg_sync_stats([], _) -> [];
add_agg_sync_stats([Agg | Aggs], Stats) ->
    case agg_sync_stats(Agg, Stats) of
        {ok, Agg1} ->
            [Agg1 | Aggs];
        {break, Agg1 = #{step := Step, limit := Limit}, MoreStats} ->
            log_agg_stats(Agg1),
            Aggs1 = add_agg_sync_stats(Aggs, [Agg1]),
            add_agg_sync_stats([empty_agg(Step, Limit) | Aggs1], MoreStats)
    end.

agg_sync_stats(Agg, []) -> {ok, Agg};
agg_sync_stats(Agg = #{n := N, gs := Gs, step := Step, t := T, mbs := MBs, txs := Txs, limit := Limit},
               [GStat = #{t0 := T0, t1 := T1, mbs := GMBs, txs := GTxs} | Stats]) ->
    Agg1 = if N == 0 -> Agg#{t0 => T0}; true -> Agg end,
    GT = maps:get(t, GStat, T1 - T0),
    Agg2 = Agg1#{n := N + 1, t := T + GT, gs := Gs + Step, mbs := MBs + GMBs, txs := Txs + GTxs, t1 => T1},
    case Limit == N + 1 of
        true  -> {break, Agg2, Stats};
        false -> agg_sync_stats(Agg2, Stats)
    end.


log_agg_stats(#{t0 := T0, t1 := T1, t := T, gs := Gs, mbs := MBs, txs := Txs}) ->
    TWall = T1 - T0,
    epoch_sync:info("Synced ~p generations: ~.2f ms/gen (~p mblock(s), ~p tx(s))"
                    " - Walltime: ~.2fs CPU: ~.2fs efficiency ~.2f%",
                    [Gs, T / (1000 * Gs), MBs, Txs, TWall / 1_000_000, T / 1_000_000, (T * 100) / TWall]);
log_agg_stats(_Incomplete) ->
    ok.
