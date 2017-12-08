%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Chain interface for aec_conductor. Should not be used by anyone else.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_conductor_chain).

-include("common.hrl").
-include("blocks.hrl").
-include("aec_conductor.hrl").

%% API
-export([ init/1
        , get_block/2
        , get_block_by_height/2
        , get_genesis_block/1
        , get_genesis_hash/1
        , get_genesis_header/1
        , common_ancestor/3
        , get_header/2
        , get_header_by_height/2
        , get_missing_block_hashes/1
        , get_top_30_blocks_time_summary/1
        , get_top_block/1
        , get_top_block_hash/1
        , get_top_header/1
        , get_top_header_hash/1
        , get_adjustment_headers/1
        , get_total_difficulty/1
        , get_transactions_between/3
        , hash_is_connected_to_genesis/2
        , has_block/2
        , has_header/2
        , insert_block/2
        , insert_header/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

init(State) ->
    case aec_persistence:get_chain() of
        [] ->
            GB = aec_block_genesis:genesis_block(),
            State1 = State#state{chain_state = aec_chain_state:new()},
            {ok, State2} = insert_block(GB, State1),
            State2;
        Chain ->
            %% TODO: This needs protection against not finding the block.
            Hash = aec_persistence:get_top_block(),
            TopState = aec_persistence:get_block_state(Hash),
            InitTrees = [{Hash, TopState}],
            ChainState = aec_chain_state:new_from_persistance(Chain, InitTrees),
            State#state{chain_state = ChainState}
    end.

get_genesis_block(State) ->
    case get_block(get_genesis_hash(State), State) of
        {ok, _} = Res -> Res;
        {error, _} -> error
    end.

get_genesis_hash(State) ->
    aec_chain_state:get_genesis_hash(State#state.chain_state).

get_genesis_header(State) ->
    case get_header(get_genesis_hash(State), State) of
        {ok, _} = Res -> Res;
        {error, _} -> error
    end.

common_ancestor(Hash1, Hash2, State) ->
    aec_chain_state:find_common_ancestor(Hash1, Hash2, State#state.chain_state).

get_block(Hash, State) ->
    case aec_chain_state:get_block(Hash, State#state.chain_state) of
        {ok, Res} -> {ok, Res};
        error -> {error, 'block_not_found'}
    end.

get_block_by_height(Height, State) ->
    aec_chain_state:get_block_by_height(Height, State#state.chain_state).

get_header_by_height(Height, State) ->
    aec_chain_state:get_header_by_height(Height, State#state.chain_state).

get_header(Hash, State) ->
    case aec_chain_state:get_header(Hash, State#state.chain_state) of
        {ok, Res} -> {ok, Res};
        error -> {error, header_not_found}
    end.

get_missing_block_hashes(State) ->
    aec_chain_state:get_missing_block_hashes(State#state.chain_state).

get_top_30_blocks_time_summary(State) ->
    aec_chain_state:get_top_N_blocks_time_summary(State#state.chain_state, 30).

get_top_block(State) ->
    aec_chain_state:top_block(State#state.chain_state).

get_top_block_hash(State) ->
    aec_chain_state:top_block_hash(State#state.chain_state).

get_top_header(State) ->
    aec_chain_state:top_header(State#state.chain_state).

get_top_header_hash(State) ->
    aec_chain_state:top_header_hash(State#state.chain_state).

get_adjustment_headers(State) ->
    N = aec_governance:blocks_to_check_difficulty_count() + 1,
    case aec_chain_state:get_n_headers_from_top(N, State#state.chain_state) of
        {ok, [_ | Headers]} -> Headers;
        {error, _} -> []
    end.

get_total_difficulty(State) ->
    aec_chain_state:difficulty_at_top_header(State#state.chain_state).

get_transactions_between(Hash1, Hash2, State) ->
    %% TODO: Remove this hack
    try get_transactions_between(Hash1, Hash2, [], State) of
        Transactions ->
            {ok, Transactions}
    catch throw:Error -> Error
    end.

get_transactions_between(Hash, Hash, Transactions,_State) ->
    Transactions;
get_transactions_between(Hash, Root, Transactions, State) ->
    case get_genesis_hash(State) =:= Hash of
        true  -> Transactions;
        false ->
            case get_block(Hash, State) of
                {ok, #block{prev_hash = Parent,
                            txs = BlockTransactions}} ->
                    NewTransactions = BlockTransactions ++ Transactions,
                    get_transactions_between(Parent, Root,
                                             NewTransactions, State);
                {error,_} -> throw({error, {block_off_chain, Hash}})
            end
    end.

hash_is_connected_to_genesis(Hash, State) ->
    aec_chain_state:hash_is_connected_to_genesis(Hash, State#state.chain_state).

has_block(Hash, State) ->
    aec_chain_state:has_block(Hash, State#state.chain_state).

has_header(Hash, State) ->
    aec_chain_state:has_header(Hash, State#state.chain_state).

insert_block(Block, State) ->
    ChainState1 = State#state.chain_state,
    case aec_chain_state:insert_block(Block, ChainState1) of
        {ok, ChainState2} ->
            persistence_store_block(Block, ChainState1, ChainState2),
            {ok, State#state{chain_state = ChainState2}};
        {error,_Reason} = E -> E
    end.

insert_header(Header, State) ->
    ChainState1 = State#state.chain_state,
    case aec_chain_state:insert_header(Header, ChainState1) of
        {ok, ChainState2} ->
            persistence_store_header(Header, ChainState1, ChainState2),
            {ok, State#state{chain_state = ChainState2}};
        {error,_Reason} = E -> E
    end.

%%%===================================================================
%%% Handle persistence

persistence_store_block(Block, StateBefore, State) ->
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

persistence_store_header(Header, StateBefore, State) ->
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
