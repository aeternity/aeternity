%%% @doc This module implements a simple registry to keep track of the number of FSMs.
%%%
%%% The `aesc_limits' server monitors each FSM, and inserts an entry into an ETS table.
%%% The size of the ETS table reflects the number of active FSMs in the node.
%%% We define two entry points in order to distinguish between reestablish attempts
%%% (`register_returning()'), for channels which we have already agreed to serve, and
%%% attempts to create new channels (`allow_new()'), where a limit check is actually
%%% imposed. This means that in certain corner cases, if lots of channels happen to
%%% leave and later reestablish roughly simultaneously, we might overshoot and end
%%% up with more channels than the configured limit (theoretically, many times as
%%% many). To avoid this, a count of channels which are currently 'on leave' would
%%% need to be kept. This could perhaps be kept current by the state cache.
%%%
-module(aesc_limits).

-behavior(gen_server).

-export([ allow_new/0
        , register_returning/0 ]).

-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, { }).

-define(PIDS, aesc_limits_pids).

allow_new() ->
    gen_server:call(?MODULE, allow_new).

register_returning() ->
    gen_server:call(?MODULE, register_returning).

start_link() ->
    %% We make the ets table public, since it's created by the supervisor.
    %% An alternative would be to set an `heir' option and use `ets:give_away/3`
    %% to give the newly (re-)started server control; this way, the ets table
    %% could be private. In either case, if the server restarts, it will have to
    %% either establish new monitors for all pids in the surviving table, or
    %% find all running FSMs, e.g. via gproc, to set up new monitors.
    ets:new(?PIDS, [set, public, named_table]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% If this is a restart, ensure that all existing pids have working mrefs
    ets:foldl(fun refresh_monitor/2, ok, ?PIDS),
    {ok, #st{}}.

handle_call(register_returning, {Pid, _}, S) ->
    MRef = monitor(process, Pid),
    case ets:insert_new(?PIDS, {Pid, MRef}) of
        true ->
            lager:debug("Returning session (~p) allowed", [Pid]),
            {reply, ok, S};
        false ->
            lager:debug("Returning session (~p) denied: already exists", [Pid]),
            demonitor(MRef),
            {reply, {error, exists}, S}
    end;
handle_call(allow_new, {Pid,_}, S) ->
    Limit = get_limit(),
    case ets:info(?PIDS, size) of
        Sz when Sz >= Limit ->
            lager:debug("New session (~p) denied; Sz = ~p, Limit = ~p",
                        [Pid, Sz, Limit]),
            {reply, {error, channel_count_limit_exceeded}, S};
        Sz ->
            MRef = monitor(process, Pid),
            case ets:insert_new(?PIDS, {Pid, MRef}) of
                true ->
                    lager:debug("New session (~p) allowed; Size = ~p, Limit = ~p",
                                [Pid, Sz, Limit]),
                    {reply, ok, S};
                false ->
                    lager:debug("New session (~p) denied: already exists", [Pid]),
                    demonitor(MRef),
                    {reply, {error, exists}, S}
            end
    end;
handle_call(_, _, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'DOWN', MRef, process, Pid, _Reason}, S) ->
    NDeleted = ets:select_delete(?PIDS, [{ {Pid, MRef}, [], [true] }]),
    lager:debug("'DOWN' received; ~p entries deleted for ~p", [NDeleted, Pid]),
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% This could possibly be optimized. The `find_config/2` function normally doesn't
%% do more than fetching the whole user config (as a map) and extracting an element
%% from it. If no limit has been specified (which may be a common scenario here), the
%% `schema_config' option will be pursued, which involves fetching and traversing the
%% JSON-Schema (hopefully not from disk, as it should be pre-loaded).
%% An issue with caching the value would be that we don't know if it gets changed
%% dynamically. An API function in this module for dynamically updating the limit
%% would be one way of addressing that.
get_limit() ->
    {ok, Max} = aeu_env:find_config([ <<"channels">>, <<"max_count">> ] , [ user_config
                                                                          , schema_default
                                                                          , {value, 1000} ]),
    Max.

refresh_monitor({Pid, _OldMRef}, ok) ->
    MRef = monitor(process, Pid),
    ets:update_element(?PIDS, Pid, {2, MRef}),
    ok.
