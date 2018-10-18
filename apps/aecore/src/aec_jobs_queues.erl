-module(aec_jobs_queues).

-export([start/0]).
-export([run/2]).


%% run(Queue, F)
%%
run(Queue, F) when is_function(F, 0) ->
    T0 = erlang:system_time(microsecond),
    case jobs:ask(Queue) of
        {ok, Opaque} ->
            log_outcome(Queue, accepted, T0),
            try F()
            after
                jobs:done(Opaque)
            end;
        {error, Reason} ->
            log_outcome(Queue, rejected, T0),
            erlang:error({rejected, Reason})
    end;
run(Queue, F) when is_function(F, 1) ->
    T0 = erlang:system_time(microsecond),
    case jobs:ask(Queue) of
        {ok, Opaque} ->
            log_outcome(Queue, accepted, T0),
            try F(Opaque)
            after
                jobs:done(Opaque)
            end;
        {error, Reason} ->
            log_outcome(Queue, rejected, T0),
            erlang:error({rejected, Reason})
    end.

log_outcome(Queue, Result, T0) when Result == accepted; Result == rejected ->
    T1 = erlang:system_time(microsecond),
    aec_metrics:try_update(metric(Queue, [Result, wait]), T1-T0),
    aec_metrics:try_update(metric(Queue, [Result]), 1).

create_metrics(Q) ->
    exometer:ensure(metric(Q, [accepted]), counter, []),
    exometer:ensure(metric(Q, [accepted, wait]), histogram, []),
    exometer:ensure(metric(Q, [rejected]), counter, []),
    exometer:ensure(metric(Q, [rejected, wait]), histogram, []).

metric(Queue, Sub) ->
    [ae, epoch, aecore, queues, Queue | Sub].

%% start()
%%
%% This function, called from aecore_app:start(), adds the configurable jobs
%% queues for epoch.
%%
%% The configuration is done in the $EPOCH_CONFIG (.yaml or .json).
%% To define a new queue, add it to the epoch_config_schema.json under
%% the "regulators" section. The name of the object becomes the name of the
%% queue. Currently supported parameters are 'rate' and 'counter' limits,
%% as well as 'max_time' and 'max_size'. Since user-defined values can only
%% be provided for properties that actually exist in the schema, all parameters
%% should be defined for each queue, and the "default" is used in lieu of a
%% user-provided value. To exclude a parameter, set the default to 0.
%% If the user config sets a value to 0, that parameter is excluded from the
%% options passed to jobs.
%%
%% Example, from the schema:
%%
%% "sync_ping_workers" : {
%%     "description" : "sync node pinger worker pool.",
%%     "type" : "object",
%%     "additionalProperties" : false,
%%     "properties" : {
%%         "counter" : {
%%             "type" : "integer",
%%             "default" : 3
%%         },
%%         "rate" : {
%%             "type" : "integer",
%%             "default" : 0
%%         },
%%         "max_size" : {
%%             "type" : "integer",
%%             "default" : 0
%%         },
%%         "max_time" : {
%%             "type" : "integer",
%%             "default" : 0
%%         }
%%     }
%% },
%%
%% Customized in epoch.yaml to be a pure rate-limited queue, at 100 reqs/sec,
%% with a max queue wait time of 3 seconds:
%%
%% regulators:
%%   sync_ping_workers:
%%     counter: 0
%%     rate: 100
%%     max_time: 3000
%%
%% If no properties are found in the schema, and thus no user params given,
%% a lager warning is issued, and the queue is defined as rate-limited at
%% 10 requests/sec (rather than crashing the node.)

start() ->
    {ok, RegulatorDefs} = aeu_env:schema_properties([<<"regulators">>]),
    Queues = check_defs(RegulatorDefs),
    [add_queue(Q, Opts) || {Q, Opts} <- Queues],
    ok.

add_queue(Q, Opts) ->
    lager:debug("add_queue(~p, ~p)", [Q, Opts]),
    Res = jobs:add_queue(Q, Opts),
    create_metrics(Q),
    lager:debug("Res = ~p", [Res]),
    ok.

check_defs(Defs) ->
    lists:map(fun check_def/1, maps:to_list(Defs)).

check_def({Name, Def}) ->
    {binary_to_atom(Name, latin1),
     regulators(Name, Def) ++ max_size(Name, Def) ++ max_time(Name, Def)}.

regulators(Name, Def) ->
    Rate = param(Name, rate   , fun(V) -> {rate   , [{limit, V}]} end, Def),
    Cntr = param(Name, counter, fun(V) -> {counter, [{limit, V}]} end, Def),
    Regs = case Rate ++ Cntr of
               []   ->
                   lager:warning(
                     "No def for queue ~p. Using default (rate:10)", [Name]),
                   [{rate, [{limit,10}]}];
               Rs -> Rs
           end,
    [{regulators, Regs}].


max_size(Name, Def) ->
    param(Name, max_size, fun(V) -> {max_size, V} end, Def).

max_time(Name, Def) ->
    param(Name, max_time, fun(V) -> {max_time, V} end, Def).

param(Name, Key, F, Def) ->
    BinKey = atom_to_binary(Key, latin1),
    case aeu_env:nested_map_get(
           [<<"properties">>, BinKey, <<"default">>], Def) of
        {ok, Default} ->
            case aeu_env:user_map([<<"regulators">>, Name, BinKey]) of
                {ok, 0}                     -> [];
                undefined when Default == 0 -> [];
                undefined                   -> [F(Default)];
                {ok, Limit}                 -> [F(Limit)]
            end;
        undefined ->
            %% if not in the schema, it can't be used
            []
    end.
