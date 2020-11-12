%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity developers
%%% @doc
%%% Process managing the database error store ETS table. 
%%% The process also cleans excess table entries by running garbage
%%% collection every 10 seconds, deleting entries older than 10 seconds.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_db_error_store).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , stop/0
        , check/1
	]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
	]).

-define(DB_NAME, ?MODULE).
-define(GC_INTERVAL, 10000). % 10 seconds

-record(state, { gc_interval = ?GC_INTERVAL :: pos_integer()
	       }).

%%====================================================================
%% API
%%====================================================================

%% @doc Check a list of definitions for errors in the store and return the associated errors if any are found.
check([]) ->
    [];
check([{Tab, K} | Rest]) ->
    ets:take(?DB_NAME, {Tab, K}) ++ check(Rest);
check([Tab | Rest]) when is_atom(Tab) ->
    Objects = ets:match_object(?DB_NAME, {{Tab, '_'}, '_', '_'}),
    ets:match_delete(?DB_NAME, {{Tab, '_'}, '_', '_'}),
    Objects ++ check(Rest).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ensure_error_store(),
    St = #state{},
    timer:send_after(St#state.gc_interval, run_gc),
    {ok, St}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(run_gc, #state{gc_interval = GCInterval} = St) ->
    do_garbage_collect(GCInterval),
    timer:send_after(GCInterval, run_gc),
    {noreply, St};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% internal
%%====================================================================

%% @doc Delete store entries which are older than the given maximum time (in milliseconds).
do_garbage_collect(MaxTime) ->
    Now = erlang:system_time(millisecond),
    Limit = Now - MaxTime,
    CountDeleted = ets:select_delete(?DB_NAME, [{{'_', '_', '$1'}, [{'<', '$1', Limit}], [true]}]),
    lager:debug("~p: Garbage collector deleted ~p entries from error store\n", [self(), CountDeleted]),
    ok.

ensure_error_store() ->
    case error_store_exists() of
        false ->
            Opts = [ bag
                   , named_table
                   , public ],
            ?DB_NAME = ets:new(?DB_NAME, Opts),
	    ok;
        true ->
            ok
    end.

error_store_exists() ->
    ets:info(?DB_NAME) =/= undefined.
