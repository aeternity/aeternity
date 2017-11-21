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

start_link({_Chain,_TopBlockHash,_TopBlockStateTrees} = Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []);
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
    {_, State} = insert_block(GenesisBlock, new_state()),
    {ok, State};
init({Chain, TopBlockHash, TopBlockStateTrees}) ->
    process_flag(trap_exit, true),
    StateTrees = [{TopBlockHash, TopBlockStateTrees}],
    {_, State} = init_chain(Chain, new_state(Chain, StateTrees)),
    {ok, State}.

init_chain([#block{} = Block | Rest], State) ->
    case aec_chain_state:insert_block(Block, State) of
        {ok, State1} ->
            init_chain(Rest, State1);
        _ ->
            init_chain(Rest, State)
    end;
init_chain([#header{} = Header | Rest], State) ->
    case aec_chain_state:insert_header(Header, State) of
        {ok, State1} ->
            init_chain(Rest, State1);
        _ ->
            init_chain(Rest, State)
    end;
init_chain([], State) -> {ok, State}.

%% State preserving functions
handle_call(top, _From, State) ->
    {reply, top(State), State};
handle_call(top_block_hash, _From, State) ->
    {reply, top_block_hash(State), State};
handle_call(top_header, _From, State) ->
    {reply, top_header(State), State};
handle_call({get_header, Hash}, _From, State) ->
    {reply, get_header(Hash, State), State};
handle_call({get_block, Hash}, _From, State) ->
    {reply, get_block(Hash, State), State};
handle_call({get_header_by_height, H}, _From, State) ->
    {reply, aec_chain_state:get_header_by_height(H, State), State};
handle_call({get_block_by_height, H}, _From, State) ->
    {reply, aec_chain_state:get_block_by_height(H, State), State};
handle_call(difficulty, _From, State) ->
    {reply, difficulty(State), State};
handle_call({common_ancestor, Hash1, Hash2}, _From, State) ->
    {reply,
     aec_chain_state:find_common_ancestor(Hash1, Hash2,State), State};

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

new_state(Chain, StateTrees) ->
    aec_chain_state:new_from_persistance(Chain, StateTrees).

insert_block(Block, State) ->
    case aec_chain_state:insert_block(Block, State) of
        {ok, State1} ->
            store_block(Block, State, State1),
            {ok, State1};
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
        {ok, State1} ->
            store_header(Header, State, State1),
            {ok, State1}
    end.


top(State) ->
    {ok, aec_chain_state:top_block(State)}.

top_block_hash(State) ->
    {ok, aec_chain_state:top_block_hash(State)}.

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

%%%===================================================================
%%% Helper functions for persistence
%%%===================================================================

store_block(Block, StateBefore, State) ->
    try begin
            %% Best effort persistence.
            %% If the server is not there, ignore it.
            aec_persistence:write_block(Block),
            persist_chain(StateBefore, State)
        end
    catch T:E ->
            lager:error("Persistence server error: ~p:~p", [T, E]),
            ok
    end.

store_header(Header, StateBefore, State) ->
    try begin
            %% Best effort persistence.
            %% If the server is not there, ignore it.
            aec_persistence:write_header(Header),
            persist_chain(StateBefore, State)
        end
    catch T:E ->
            lager:error("Persistence server error: ~p:~p", [T, E]),
            ok
    end.

%% NOTE: The order is significant.
%%       The header we can persist right away, but the top block hash
%%       should not be written unless we have persisted the state trees
%%       to avoid problems on restart.
persist_chain(StateBefore, StateAfter) ->
    case aec_chain_state:top_header_hash(StateAfter) of
        undefined -> ok;
        TopHeaderHash ->
            aec_persistence:write_top_header(TopHeaderHash),
            case aec_chain_state:top_block_hash(StateAfter) of
                undefined -> ok;
                TopBlockHash ->
                    persist_state_trees(StateBefore, StateAfter),
                    aec_persistence:write_top_block(TopBlockHash)
            end
    end.

persist_state_trees(StateBefore, StateAfter) ->
    %% Persist the state trees
    Trees1 = aec_chain_state:get_state_trees_for_persistance(StateBefore),
    Trees2 = aec_chain_state:get_state_trees_for_persistance(StateAfter),
    Persist = Trees2 -- Trees1,
    lists:foreach(fun({Hash, Trees}) ->
                          aec_persistence:write_block_state(Hash, Trees)
                  end, Persist).

