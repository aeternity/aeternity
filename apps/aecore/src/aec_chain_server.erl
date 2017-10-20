%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Service holding blocks (or headers) from the block chain.
%%%
%%% The longest chain is determined according to the amount of work
%%% done on the chain, i.e. according to the total difficulty of the
%%% highest header in the chain (the so-called "top" header).  The
%%% total difficulty of a header is the sum of the difficulty of the
%%% header and the total difficulty of the previous header.
%%%
%%% The difficulty of a header is a linear representation of the
%%% expected average amount of work required for mining the header,
%%% and is derived from the target threshold in the header.
%%%
%%% The longest chain is also called the main chain.
%%% The server will keep the state (account balances) of the main chain.
%%% These states will be kept (checkpointed) at certain points in the
%%% chain in order to quickly rebuild the state when merging and
%%% switching to an alternate chain.
%%%
%%% The server also track blocks in alternative chains.
%%% That is blocks or headers not on the main chain.
%%%
%%% If an alternative chain becomes rooted in a different genesis
%%% block that alternative chain is thrown away.
%%%
%%% If an alternative chain has more work than the current chain
%%% and is rooted in a common ancestor the alternative chain will
%%% become the main chain.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain_server).

-behaviour(gen_server).

%% API
-export([start_link/1,
         stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("common.hrl").
-include("blocks.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API -  the main API for the server is in aec_chain.
%%%===================================================================

start_link(GenesisBlock) ->
    Args = [GenesisBlock],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
 
init(_Args = [GenesisBlock]) ->
    process_flag(trap_exit, true),
    GenesisHeader = aec_blocks:to_header(GenesisBlock),
    {ok, S2} = insert_header(GenesisHeader, new_state()),
    {ok, S3} = insert_block(GenesisBlock, S2),
    {ok, S3}.


%% State preserving functions
handle_call(top, _From, State) ->
    {reply, top(State), State};
handle_call(top_header, _From, State) ->
    {reply, top_header(State), State};
handle_call({get_header, Hash}, _From, State) ->
    {reply, get_header(Hash, State), State};
handle_call({get_block, Hash}, _From, State) ->
    {reply, get_block(Hash, State), State};
handle_call({get_header_by_height, H}, _From, State) ->
    {reply, get_header_by_height(H, State), State};
handle_call({get_block_by_height, H}, _From, State) ->
    {reply, get_block_by_height(H, State), State};
handle_call(difficulty, _From, State) ->
    {reply, difficulty(State), State};

%% Update functions
handle_call({insert_header, H}, _From, State) ->
    {Reply, NewState} = insert_header(H, State),
    {reply, Reply, NewState};
handle_call({write_block, B}, _From, State) ->
    {Reply, NewState} = insert_block(B, State),
    {reply, Reply, NewState};

handle_call(Request, From, State) ->
    lager:warning("Unknown call request from ~p: ~p", [From, Request]),
    {reply, {error, unknown_request}, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_state() ->
    aec_chain_state:new().

insert_block(Block, State) ->
    case aec_chain_state:insert_block(Block, State) of
        {ok, State1} -> {ok, State1};
        {error, What} -> {{error, What}, State}
    end.

insert_header(Header, State) ->
    case aec_chain_state:insert_header(Header, State) of
	{error, height_inconsistent_with_previous_hash} ->
            %% TODO: Special case for test case. Should go away.
	    Top = aec_chain_state:top_header(State),
	    {{error, {height_inconsistent_with_previous_hash,
		     {top_header, Top}}}, State};
        {error, What} -> {{error, What}, State};
	{ok, NewState} -> {ok, NewState}
    end.

top(State) ->
    {ok, aec_chain_state:top_block(State)}.

get_block(Hash, State) ->
     case aec_chain_state:get_block(Hash, State) of
	{ok, Res} -> {ok, Res};
	error ->
	    Top = aec_chain_state:top_header(State),
	    {error, {block_not_found, {top_header, Top}}}
    end.

top_header(State) ->
     {ok, aec_chain_state:top_header(State)}.

get_header(Hash, State) ->
    case aec_chain_state:get_header(Hash, State) of
	{ok, Res} -> {ok, Res};
	error ->
	    Top = aec_chain_state:top_header(State),
	    {error, {header_not_found, {top_header, Top}}}
    end.

difficulty(State) ->
    {ok, X} = aec_chain_state:difficulty_at_top_header(State),
    X.

%% WARNING WARNING WARNING WARNING WARNING WARNING
%% Temporary implementation beyond this point.
%% Should be moved to aec_chain_state.

get_block_by_height(H, State) ->
    case get_header_by_height(H, State) of
	{error, _} = E -> E;
	{ok, Header} -> 
	    {ok, HeaderHash} = aec_headers:hash_header(Header),
	    get_block(HeaderHash, State)
    end.

find_header_at_height(H, HeaderHash, State) ->
    {ok, Header} = aec_chain_state:get_header(HeaderHash, State),
    Current = aec_headers:height(Header),
    if Current > H ->
	    find_header_at_height(H, aec_headers:prev_hash(Header), State);
       Current =:= H -> 
	    {ok, Header}
    end.




get_header_by_height(H, State) ->
    TopHeader  = aec_chain_state:top_header(State),
    CH = aec_headers:height(TopHeader),
    if CH < H ->

	    {error, {chain_too_short, 
		     {{chain_height, CH},
		      {top_header, TopHeader}}}};
       CH > H ->
	    find_header_at_height(H, aec_headers:prev_hash(TopHeader), State);
       true -> 
	    {ok, TopHeader}
    end.

