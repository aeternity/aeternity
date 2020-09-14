%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_aeternity_connector).

-behaviour(aehc_connector).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-export([send_tx/3, get_block_by_hash/1, get_top_block/0, dry_send_tx/3]).

%% API.

-spec start_link(Args::term()) ->
    {ok, pid()} | ingnore | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%%  aehc_connector behaviour
%%%===================================================================

-spec send_tx(binary(), binary(), binary()) -> ok.
send_tx(Delegate, Commitment, PoGF) ->
    gen_server:call(?MODULE, {send_tx, Delegate, Commitment, PoGF}).

-spec get_top_block() -> aehc_parent_block:parent_block().
get_top_block() ->
    gen_server:call(?MODULE, {get_top_block}).

-spec get_block_by_hash(binary()) -> aehc_parent_block:parent_block().
get_block_by_hash(Hash) ->
    gen_server:call(?MODULE, {get_block_by_hash, Hash}).

-spec dry_send_tx(binary(), binary(), binary()) -> ok.
dry_send_tx(Delegate, Commitment, PoGF) ->
    gen_server:call(?MODULE, {dry_send_tx, Delegate, Commitment, PoGF}).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, { stub::boolean() }).

init(Args) ->
    %% Stub mode allows to pass acceptance procedure without parent node (only for dev purpouses);
    Stub = maps:get(<<"stub">>, Args, false),
    process_flag(trap_exit, true),
    {ok, #state{ stub = Stub }}.

handle_call({send_tx, _Delegate, _Commitment, _PoGF}, _From, #state{ stub = true} = State) ->
    {reply, ok, State};

handle_call({get_top_block}, _From, #state{ stub = true} = State) ->
    {reply, stub_block(), State};

handle_call({get_block_by_hash, _Hash}, _From, #state{ stub = true} = State) ->
    {reply, stub_block(), State};

handle_call({dry_send_tx, Delegate, Commitment, PoGF}, _From, #state{ stub = true} = State) ->
    lager:info("~p: ~p = dry_send_tx(~p, ~p, ~p)", [?MODULE, ok, Delegate, Commitment, PoGF]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

stub_block() ->
    Header = aehc_parent_block:new_header(<<"Hash">>, <<"PrevHash">>, 1000),
    aehc_parent_block:new_block(Header, []).
