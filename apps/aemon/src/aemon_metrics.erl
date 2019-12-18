%% @doc This module provides helper functions which mask exometer and
%% aec_metric calls for the user.
-module(aemon_metrics).

%% generic API
-export([create/1]).

%% specific metrics
-export([ fork_micro/1
        , confirmation_delay/1
        , gen_stats_tx/1
        , gen_stats_tx_monitoring/1
        , gen_stats_microblocks/1
        , publisher_balance/1
        , publisher_post_tx/1
        , queue_size/1
        , ttl_expired/1
        , block_propagation_time/2
        , block_time_since_prev/2
        , chain_top_difficulty/1
        ]).

-define(HISTOGRAM_TIMESPAN_LONG, 60 * 60 * 1000). %% 1 hour, 60 values for updates every 1 minute
-define(HISTOGRAM_TIMESPAN_SHORT, 10 * 60 * 1000). %% 10 minutes, 200 values for updates every 3 seconds

%% ==================================================================
%% generic API

%% @doc Creates a set of metrics based on the given group identifier.
create(on_chain) ->
    create([forks, micro, count], counter),
    create([forks, micro, height], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_LONG}]),
    create([confirmation, delay], histogram),
    create([block, propagation_time, key], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_LONG}]),
    create([block, propagation_time, micro], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_SHORT}]),
    create([block, time_since_prev, key], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_LONG}]),
    create([block, time_since_prev, micro], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_SHORT}]),
    create([chain, top, difficulty], gauge),
    ok;
create(gen_stats) ->
    create([gen_stats, tx, total], histogram),
    create([gen_stats, tx, monitoring], histogram),
    create([gen_stats, microblocks, total], histogram),
    ok;
create(publisher) ->
    create([publisher, balance], gauge),
    create([publisher, post_tx, success], counter),
    create([publisher, post_tx, max_adjustment], counter),
    create([publisher, post_tx, nonce_too_low], counter),
    create([publisher, post_tx, nonce_too_high], counter),
    ok;
create(ttl) ->
    create([publisher, queue, size], histogram),
    create([publisher, queue, ttl_expired], histogram),
    ok.

%% ==================================================================
%% specific metrics

fork_micro(Height) ->
    log_error(update([forks, micro, count], 1), "Could not update micro fork count"),
    log_error(update([forks, micro, height], Height), "Could not update micro fork height").

confirmation_delay(Count) ->
    update([confirmation, delay], Count).

gen_stats_tx(Count) ->
    update([gen_stats, tx, total], Count).

gen_stats_tx_monitoring(Count) ->
    update([gen_stats, tx, monitoring], Count).

gen_stats_microblocks(Count) ->
    update([gen_stats, microblocks, total], Count).

publisher_balance(Error) when is_atom(Error) ->
    update([publisher, balance, Error], 1);
publisher_balance(Balance) ->
    update([publisher, balance], Balance).

publisher_post_tx(Action) ->
    update([publisher, post_tx, Action], 1).

queue_size(Count) ->
    update([publisher, queue, size], Count).

ttl_expired(Count) ->
    update([publisher, queue, ttl_expired], Count).

block_propagation_time(Type, Time) when Time < 1 ->
    lager:debug("Ignoring metric update of block_propagation_time.~p, Time = ~p", [Type, Time]),
    ok;
block_propagation_time(Type, Time) when Type =:= key; Type =:= micro ->
    update([block, propagation_time, Type], Time).

block_time_since_prev(Type, Time) when Time < 1 ->
    lager:debug("Ignoring metric update of block_time_since_prev.~p, Time = ~p", [Type, Time]),
    ok;
block_time_since_prev(Type, Time) when Type =:= key; Type =:= micro ->
    update([block, time_since_prev, Type], Time).

chain_top_difficulty(Difficulty) ->
    update([chain, top, difficulty], Difficulty).

%% ==================================================================
%% internal functions

create(Name, Type) ->
    create(Name, Type, []).

create(Name, Type, Opts) ->
    log_error(exometer:ensure(metric(Name), Type, Opts), "Could not create metric").

update(Name, Value) ->
    aec_metrics:try_update(metric(Name), Value).

metric(Name) ->
    [ae, epoch, aemon | Name ].

log_error(ok, _) ->
    ok;
log_error(Err, Msg) ->
    lager:error(Msg ++ " with error = ~p", [Err]),
    ok.
