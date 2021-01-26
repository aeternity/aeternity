-module(aec_resilience).
-behaviour(gen_server).

%%% This module implements

-export([ start_link/0 ]).

-export([ fork_resistance_active/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, {}).

-define(TAB, ?MODULE).

-type height() :: non_neg_integer().
-type fork_resistance() :: {yes, height()} | no.
-type mode() :: manual | auto.

-spec fork_resistance_active() -> fork_resistance().
fork_resistance_active() ->
    {Res, _} = get_fork_resistance(),
    Res.

start_link() ->
    ensure_tab(),   % ets table will survive process restart
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(chain_sync),
    aec_events:subscribe(update_config),
    maybe_enable_fork_resistance_at_startup(),
    {ok, #st{}}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, chain_sync, #{info := {chain_sync_done, _}}}, St) ->
    %% For now, at least, we try to enable fork resistance as soon as we are in sync
    %% with at least one peer
    note_chain_sync_done(true),
    aec_events:unsubscribe(chain_sync),
    maybe_enable_fork_resistance(),
    {noreply, St};
handle_info({gproc_ps_event, update_config,
             #{info := #{<<"sync">> :=
                             #{<<"sync_allowed_height_from_top">> := New}}}}, St) ->
    lager:debug("Detected config update. New value: ~p", [New]),
    case chain_sync_is_done() of
        true ->
            maybe_enable_fork_resistance();
        false ->
            lager:debug("chain_sync is not done", []),
            skip
    end,
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

maybe_enable_fork_resistance_at_startup() ->
    case lookup(fork_resistance, undefined) of
        undefined ->
            case aeu_env:find_config([<<"sync">>, <<"resist_forks_from_start">>],
                                     [ user_config, schema_default ]) of
                {ok, true} ->
                    maybe_enable_fork_resistance();
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

maybe_enable_fork_resistance() ->
    case get_fork_resistance() of
        {_, manual} -> ok;
        _ ->
            Res = case aeu_env:find_config([<<"sync">>, <<"sync_allowed_height_from_top">>],
                                           [ user_config, schema_default ]) of
                      {ok, Height} when Height > 0 ->
                          lager:debug("Activating fork resistance for Height = ~p", [Height]),
                          {yes, Height};
                      _ ->
                          lager:debug("No fork resistance", []),
                          no
                  end,
            set_fork_resistance({Res, auto})
    end.

-spec get_fork_resistance() -> {fork_resistance(), mode()}.
get_fork_resistance() ->
    lookup(fork_resistance, {no, auto}).

-spec set_fork_resistance({fork_resistance(), mode()}) -> true.
set_fork_resistance({_, _} = Value) ->
    ets:insert(?TAB, {fork_resistance, Value}).

-spec note_chain_sync_done(boolean()) -> true.
note_chain_sync_done(Bool) when is_boolean(Bool) ->
    ets:insert(?TAB, {chain_sync_done, Bool}).

-spec chain_sync_is_done() -> boolean().
chain_sync_is_done() ->
    lookup(chain_sync_done, false).

lookup(Key, Default) ->
    case ets:lookup(?TAB, Key) of
        [{_, Value}] ->
            Value;
        [] ->
            Default
    end.

ensure_tab() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [set, public, named_table]);
        _ ->
            true
    end.
