-module(aec_resilience).
-behaviour(gen_server).

%%% This module implements caching and monitoring of conditions for
%%% enabling fork resistance.
%%%
%%% Workings of fork resistance:
%%% * if config sync: sync_allowed_height_from_top is set to a value > 0,
%%%   fork resistance will kick in as soon as our node has synced with a peer
%%% * The aec_chain_state module persist a finalized_height, which trails the
%%%   fork resistance depth. It checks whether fork_resistance_configured(),
%%%   to determine whether to apply finalized_height checking. This means that if
%%%   the fork resistance config changes to 0 (disabled), the finalized height will
%%%   not be enforced even if present in the database.
%%% * If the config sync: resist_forks_from_start = true, then fork resistance will
%%%   be activated immediately. This is unlikely to ever be of use, and should
%%%   perhaps be removed. It was introduced before the finalized_depth feature.
%%% * The aec_resilience server subscribes to config changes, and will adapt to
%%%   changes in the sync_allowed_height_from_top setting directly.

-export([ start_link/0 ]).

-export([ fork_resistance_active/0
        , fork_resistance_configured/0
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

%% This function checks whether fork resistance is configured AND activated,
%%
-spec fork_resistance_active() -> fork_resistance().
fork_resistance_active() ->
    get_fork_resistance().

%% This function returns 'no' if the config is Height = 0
%%
-spec fork_resistance_configured() -> fork_resistance().
fork_resistance_configured() ->
    get_cached_config().

start_link() ->
    ensure_tab(),   % ets table will survive process restart
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(chain_sync),
    aec_events:subscribe(update_config),
    _ = read_and_cache_config(),                % initialize cached value
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
    Config = read_and_cache_config(),
    case lookup(fork_resistance, undefined) of
        undefined ->
            case aeu_env:find_config([<<"sync">>, <<"resist_forks_from_start">>],
                                     [ user_config, schema_default ]) of
                {ok, true} ->
                    maybe_enable_fork_resistance(Config);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

maybe_enable_fork_resistance() ->
    maybe_enable_fork_resistance(read_and_cache_config()).

maybe_enable_fork_resistance(Config) ->
    lager:debug("Config = ~p", [Config]),
    Res = case Config of
              {yes, Height} when Height > 0 ->
                  lager:debug("Activating fork resistance for Height = ~p", [Height]),
                  ensure_finalized_height(Height),
                  {yes, Height};
              no ->
                  lager:debug("No fork resistance", []),
                  no
          end,
    set_fork_resistance(Res).

read_and_cache_config() ->
    Res = case aeu_env:find_config([<<"sync">>, <<"sync_allowed_height_from_top">>],
                                   [ user_config, schema_default ]) of
              {ok, Height} when Height > 0 ->
                  {yes, Height};
              _ ->
                  no
          end,
    set_cached_config(Res),
    Res.


set_cached_config(Value) ->
    ets:insert(?TAB, {cached_config_key(), Value}).

get_cached_config() ->
    lookup(cached_config_key(), no).

cached_config_key() ->
    sync_allowed_height_config.

-spec get_fork_resistance() -> fork_resistance().
get_fork_resistance() ->
    lookup(fork_resistance, no).

-spec set_fork_resistance(fork_resistance()) -> true.
set_fork_resistance(Value) ->
    ets:insert(?TAB, {fork_resistance, Value}).

-spec note_chain_sync_done(boolean()) -> true.
note_chain_sync_done(Bool) when is_boolean(Bool) ->
    ets:insert(?TAB, {chain_sync_done, Bool}).

-spec chain_sync_is_done() -> boolean().
chain_sync_is_done() ->
    lookup(chain_sync_done, false).

ensure_finalized_height(AllowedHeightFromTop) ->
    Fun = fun() -> ensure_finalized_height_(AllowedHeightFromTop) end,
    aec_db:ensure_transaction(Fun).

ensure_finalized_height_(AllowedHeightFromTop) ->
    #{ header := TopHeader } = aec_db:get_top_block_node(),
    case aec_headers:height(TopHeader) - AllowedHeightFromTop - 1 of
        Height when Height > 0 ->
            case aec_db:get_finalized_height() of
                undefined ->
                    lager:debug("No finalized height; setting to ~p", [Height]),
                    aec_db:write_finalized_height(Height);
                Height ->
                    lager:debug("Already finalized height: ~p", [Height]),
                    ok;
                Other ->
                    lager:debug("Previous finalized height: ~p; changed to ~p",
                                [Other, Height]),
                    aec_db:write_finalized_height(Height)
            end;
        _ ->
            lager:debug("Not yet high enough to set finalized height", []),
            ok
    end.

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
