-module(aesc_state_cache).
-behaviour(gen_server).

-export([
          start_link/0
        , new/3
        , update/3
        , fetch/2
        , delete/1
        ]).

-export([
          init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, {}).
-record(ch, {id, state}).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

new(ChId, PubKey, State) ->
    gen_server:call(?SERVER, {new, ChId, PubKey, State}).

update(ChId, PubKey, State) ->
    gen_server:cast(?SERVER, {update, ChId, PubKey, State}).

fetch(ChId, PubKey) ->
    gen_server:call(?SERVER, {fetch, ChId, PubKey}).

delete(ChId) ->
    gen_server:cast(?SERVER, {delete, ChId}).

start_link() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?MODULE, [ordered_set, public, named_table,
                              {keypos, #ch.id}]);
        _ ->
            ok
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.

handle_call({new, ChId, PubKey, State}, _From, St) ->
    case ets:insert_new(?TAB, #ch{id = key(ChId, PubKey), state = State}) of
        true ->
            {reply, ok, St};
        false ->
            {reply, {error, exists}, St}
    end;
handle_call({fetch, ChId, PubKey}, _From, St) ->
    case ets:lookup(?TAB, key(ChId, PubKey)) of
        [#ch{state = State}] ->
            {reply, {ok, State}, St};
        [] ->
            {reply, error, St}
    end;
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({update, ChId, PubKey, State}, St) ->
    ets:update_element(?TAB, key(ChId, PubKey), {#ch.state, State}),
    {noreply, St};
handle_cast({delete, ChId}, St) ->
    ets:select_delete(
      ?TAB, [{ #ch{id = key(ChId, '_'), _ = '_'}, [], [true] }]),
    {noreply, St};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

key(ChId, PubKey) ->
    {ChId, PubKey}.
