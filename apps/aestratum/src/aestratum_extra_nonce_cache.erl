-module(aestratum_extra_nonce_cache).

-behaviour(gen_server).

%% TODO: eunit

%% API.
-export([start_link/0,
         get/1,
         free/1
        ]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2
        ]).

-record(state, {
          cache
         }).

-define(SERVER, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(aestratum_nonce:part_nbytes()) ->
    {ok, aestratum_nonce:part_nonce()} | {error, term()}.
get(ExtraNonceNBytes) ->
    gen_server:call(?SERVER, {get, ExtraNonceNBytes}).

-spec free(aestratum_nonce:part_nonce()) -> ok.
free(ExtraNonce) ->
    gen_server:call(?SERVER, {free, ExtraNonce}).

%% Callbacks.

init([]) ->
    {ok, #state{cache = sets:new()}}.

handle_call({get, ExtraNonceNBytes}, _From, State) ->
    case handle_get(ExtraNonceNBytes, State) of
        {ok, ExtraNonce, State1} ->
            {reply, ExtraNonce, State1};
        {error, Rsn, State1} ->
            {reply, Rsn, State1}
    end;
handle_call({free, ExtraNonce}, _From, State) ->
    {ok, State1} = handle_free(ExtraNonce, State),
    {reply, ok, State1}.

handle_cast(_Req, State) ->
    {noreply, State}.

%% Internal functions.

handle_get(ExtraNonceNBytes, #state{cache = Cache} = State) ->
    MaxExtraNonce = aestratum_nonce:max(ExtraNonceNBytes),
    case find_extra_nonce(MaxExtraNonce, Cache, 50) of
        {ok, Value} ->
            State1 = State#state{cache = sets:add_element(Value, Cache)},
            {ok, aestratum_nonce:new(extra, Value, ExtraNonceNBytes), State1};
        {error, Rsn} ->
            {error, Rsn, State}
    end.

handle_free(ExtraNonce, #state{cache = Cache} = State) ->
    Value = aestratum_nonce:value(ExtraNonce),
    {ok, State#state{cache = sets:del_element(Value, Cache)}}.

%% MaxExtraNonce is the max value for the extra nonce. The nonce selected
%% randomly and checked against the cache if it's already in use until
%% an available nonce is found or Retries is exhausted.
find_extra_nonce(MaxExtraNonce, Cache, Retries) when Retries > 0 ->
    %% Random value between 0 and MaxValue.
    Value = rand:uniform(MaxExtraNonce + 1) - 1,
    case sets:is_element(Value, Cache) of
        false -> {ok, Value};
        true  -> find_extra_nonce(MaxExtraNonce, Cache, Retries - 1)
    end;
find_extra_nonce(_MaxExtraNonce, _Cache, 0) ->
    {error, extra_nonce_not_found}.

