%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_aeternity_connector).

-behaviour(aehc_connector).
-behaviour(gen_server).

-include_lib("aehyperchains/include/aehc_types.hrl").


%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% aehc_connector
-export([send_tx/3, get_block_by_hash/1, get_top_block/0, dry_send_tx/3]).

-record(state, {stub :: boolean()}).


%% API.

-spec start_link(Args :: term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%%  aehc_connector behaviour
%%%===================================================================

-spec send_tx(committer_pubkey(), commitment_hash(), pogf_hash()) -> ok.
send_tx(Delegate, Commitment, PoGF) ->
    gen_server:call(?MODULE, {send_tx, Delegate, Commitment, PoGF}).

-spec get_top_block() -> aehc_parent_block:parent_block().
get_top_block() ->
    gen_server:call(?MODULE, {get_top_block}).

-spec get_block_by_hash(commitment_hash()) -> aehc_parent_block:parent_block().
get_block_by_hash(Hash) ->
    gen_server:call(?MODULE, {get_block_by_hash, Hash}).

-spec dry_send_tx(committer_pubkey(), commitment_hash(), pogf_hash()) -> ok.
dry_send_tx(Delegate, Commitment, PoGF) ->
    gen_server:call(?MODULE, {dry_send_tx, Delegate, Commitment, PoGF}).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

init(Args) ->
    process_flag(trap_exit, true),
    %% Stub mode allows to pass acceptance procedure without parent node (only for dev purposes);
    Stub = maps:get(<<"stub">>, Args, false),
    {ok, #state{stub = Stub}}.

handle_call({send_tx, _Delegate, _Commitment, _PoGF},
    _From, #state{stub = true} = State) ->
    {reply, ok, State};

handle_call({get_top_block}, _From, #state{stub = true} = State) ->
    {reply, stub_block(), State};

handle_call({get_block_by_hash, _Hash}, _From, #state{stub = true} = State) ->
    {reply, stub_block(), State};

handle_call({dry_send_tx, Delegate, Commitment, PoGF}, _From, #state{stub = true} = State) ->
    lager:info("~p: ~p = dry_send_tx(~p, ~p, ~p)", [?MODULE, ok, Delegate, Commitment, PoGF]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%% Implementation

%% <<"Hash">> and <<"PrevHash">> are obviously not really hashes,
%% so we don't want to provoke dialyzer into too much plunging here.
-dialyzer({nowarn_function, stub_block/0}).
stub_block() ->
    Header = aehc_parent_block:new_header(<<"Hash">>, <<"PrevHash">>, 1000),
    aehc_parent_block:new_block(Header, []).
