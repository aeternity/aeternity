%% @doc This module provides helper functions which mask exometer and
%% aec_metric calls for the user.
-module(aemon_metrics).

%% generic API
-export([create/1]).

%% specific metrics
-export([ block_gas_per_tx/1
        , block_gas_total/1
        , block_propagation_time/2
        , block_size_per_tx/1
        , block_time_since_prev/2
        , block_tx_total/1
        , chain_top_difficulty/1
        , confirmation_delay/1
        , fork_micro/1
        , gen_stats_microblocks/1
        , gen_stats_tx/1
        , gen_stats_tx_monitoring/1
        , publisher_balance/1
        , publisher_post_tx/1
        , queue_size/1
        , ttl_expired/1
        ]).

-define(HISTOGRAM_TIMESPAN_LONG, 60 * 60 * 1000). %% 1 hour, 60 values for updates every 1 minute
-define(HISTOGRAM_TIMESPAN_SHORT, 10 * 60 * 1000). %% 10 minutes, 200 values for updates every 3 seconds
-define(HISTOGRAM_TIMESPAN_2MIN, 2 * 60 * 1000). %% 2 minutes, 120 values for updates every 1 second

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
    create([block, tx, total, micro], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_SHORT}]),
    create([block, gas, total, micro], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_SHORT}]),
    create([block, gas, per_tx, micro], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_2MIN}]),
    create([block, size, per_tx, micro], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_2MIN}]),
    create([chain, top, difficulty], gauge),
    ok;
create(gen_stats) ->
    create([gen_stats, tx, total], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_LONG}]),
    create([gen_stats, tx, monitoring], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_LONG}]),
    create([gen_stats, microblocks, total], histogram, [{time_span, ?HISTOGRAM_TIMESPAN_LONG}]),
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
    log_error(update([forks, micro, count], 1),
              "Could not update micro fork count"),
    log_error(update([forks, micro, height], Height),
              "Could not update micro fork height").

confirmation_delay(Count) ->
    log_error(update([confirmation, delay], Count),
              "Could not update confirmation delay").

gen_stats_tx(Count) ->
    log_error(update([gen_stats, tx, total], Count),
              "Could not update transactions per generation").

gen_stats_tx_monitoring(Count) ->
    log_error(update([gen_stats, tx, monitoring], Count),
              "Could not update monitoring transactions per generation").

gen_stats_microblocks(Count) ->
    log_error(update([gen_stats, microblocks, total], Count),
              "Could not update microblocks per generation").

publisher_balance(Error) when is_atom(Error) ->
    log_error(update([publisher, balance, Error], 1),
              "Could not update publisher balance error");
publisher_balance(Balance) ->
    log_error(update([publisher, balance], Balance),
              "Could not update publisher balance").

publisher_post_tx(Action) ->
    log_error(update([publisher, post_tx, Action], 1),
              "Could not update publisher post transaction count").

queue_size(Count) ->
    log_error(update([publisher, queue, size], Count),
              "Could not update publisher queue size").

ttl_expired(Count) ->
    log_error(update([publisher, queue, ttl_expired], Count),
              "Could not update publisher queue ttl expired").

block_propagation_time(Type, Time) when Time < 1 ->
    lager:debug("Ignoring metric update of block_propagation_time.~p, Time = ~p", [Type, Time]),
    ok;
block_propagation_time(Type, Time) when Type =:= key; Type =:= micro ->
    log_error(update([block, propagation_time, Type], Time),
              "Could not update block propagation time").

block_time_since_prev(Type, Time) when Time < 1 ->
    lager:debug("Ignoring metric update of block_time_since_prev.~p, Time = ~p", [Type, Time]),
    ok;
block_time_since_prev(Type, Time) when Type =:= key; Type =:= micro ->
    log_error(update([block, time_since_prev, Type], Time),
              "Could not update block time since previous block").

block_tx_total(N) ->
    log_error(update([block, tx, total, micro], N),
              "Could not update transactions per microblock").

block_gas_total(N) ->
    log_error(update([block, gas, total, micro], N),
              "Could not update gas per microblock").

block_gas_per_tx(N) ->
    log_error(update([block, gas, per_tx, micro], N),
              "Could not update gas per transaction in a microblock").

block_size_per_tx(N) ->
    log_error(update([block, size, per_tx, micro], N),
              "Could not update size per transaction in a microblock").

chain_top_difficulty(Difficulty) ->
    log_error(update([chain, top, difficulty], Difficulty),
              "Could not update chain top difficulty").

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
