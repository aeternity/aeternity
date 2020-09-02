%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_aeternity_connector).

-behaviour(aehc_connector).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-export([send_tx/1, get_block/1]).

%% API.

-spec start_link() ->
    {ok, pid()} | ingnore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%%  aehc_connector behaviour
%%%===================================================================

-spec send_tx(binary()) -> binary().
send_tx(_Payload) ->
   throw('not implemented').

-spec get_block(non_neg_integer()) -> aehc_connector:block().
get_block(_Num) ->
    throw('not implemented').

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, {}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{ }}.

handle_call(Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
