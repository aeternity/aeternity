%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module holding a short lived cache for gossiped TXs - the rationale
%%%    being that most TXs arrives close in time and thus the mempool/DB
%%%    can be offloaded by a first level filter.
%%% @end
%%%=============================================================================
-module(aec_tx_gossip_cache).

-behaviour(gen_server).

%% API for supervisor
-export([ start_link/0
        , stop/0
        ]).

%% Cache API
-export([ in_cache/1
        , reset/0
        , stats/0
        ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CACHE_TIMEOUT, 3000).
-define(CACHE_SIZE, 200).

-type tx_hash() :: binary().

-record(state, { size    = 0           :: non_neg_integer()
               , evict_q = queue:new() :: queue:queue(tx_hash())
               , cache   = #{}         :: #{ tx_hash() := term() }
               , hit     = 0           :: non_neg_integer()
               , miss    = 0           :: non_neg_integer() }).
%% -- API --------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

in_cache(TxHash) ->
    call({in_cache, TxHash}, false).

reset() ->
    gen_server:cast(?SERVER, reset).

stats() ->
    call(stats, timeout).

call(Msg, TimeoutResult) ->
    try
        gen_server:call(?SERVER, Msg, ?CACHE_TIMEOUT)
    catch _:_ ->
        lager:debug("TX gossip cache timeout"),
        TimeoutResult
    end.

%% -- gen_server callbacks ---------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({in_cache, TxHash}, _From, State) ->
    {Res, NewState} = in_cache(TxHash, State),
    {reply, Res, NewState};
handle_call(stats, _From, State) ->
    {reply, #{ hit => State#state.hit, miss => State#state.miss }, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(reset, _State) ->
    NewState = init_state(),
    {noreply, NewState};
handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:warning("Ignoring unknown info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- local functions --------------------------------------------------------
init_state() ->
    #state{}.

in_cache(TxHash, State = #state{ size = S, evict_q = Q, cache = C, hit = H, miss = M }) ->
    MaxCacheSize = max_cache_size(),
    case maps:is_key(TxHash, C) of
        true ->
            {true, State#state{ hit = H + 1 }};
        false when S >= MaxCacheSize ->
            {{value, Evicted}, Q1} = queue:out(Q),
            C1 = maps:put(TxHash, x, maps:remove(Evicted, C)),
            {false, State#state{ evict_q = queue:in(TxHash, Q1), cache = C1, miss = M + 1 }};
        false ->
            {false, State#state{ size = S + 1, evict_q = queue:in(TxHash, Q),
                                 cache = maps:put(TxHash, x, C), miss = M + 1 }}
    end.

max_cache_size() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"cache_size">>], aecore,
                               cache_size, ?CACHE_SIZE).
