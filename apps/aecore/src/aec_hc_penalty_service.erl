%%%-----------------------------------------------------------------------------
%%% @doc
%%% Service for finding and managing penalties for Hyperchains
%%% @end
%%%-----------------------------------------------------------------------------

-module(aec_hc_penalty_service).
-author("mans@happihacking.se").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/1,
    stop/0
]).

%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    test_and_register_block_offence/1
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
    block_height_map = #{} :: map(),
    penalties = [] :: list(),
    cache_level = 0 :: integer()
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(integer()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(CacheLevel) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [CacheLevel], []).

stop() ->
    gen_server:stop(?SERVER).

test_and_register_block_offence(Block) ->
    gen_server:call(?SERVER, { test_and_register_block_offence, Block }).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([CacheLevel]) ->
    lager:debug("init: Cachelevel: ~p", [CacheLevel]),
    LoopState = #loop_state{block_height_map = #{},
                            penalties = [],
                            cache_level = CacheLevel},
    {ok, LoopState}.

handle_call({test_and_register_block_offence, Block}, _From, State) ->
    {Res, NewState} = case test_block_offence(Block, State) of
        {ok, [], NS0} ->
            {ok, NS0};
        {ok, Pens, NS1} ->
            NS2 = register_penalties(Pens, NS1),
            {ok, NS2};
        {error, Reason} ->
            {{error, Reason}, State}
    end,
    %lager:debug("pen_state: ~p", [NewState]),
    {reply, Res, NewState};

handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {reply, Reply, LoopState}.

handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
test_block_offence(Block, State) ->
    lager:debug("test_block_offence: ~p", [Block]),
    Header = aec_blocks:to_header(Block),
    Height = aec_headers:height(Header),
    RegBlocks = get_blocks_for_height(Height, State),
    lager:debug("RegBlocks: ~p", [RegBlocks]),
    {Pens, BlockToAdd} = case aec_headers:type(Header) of
        key ->
            KeyHeader = aec_blocks:to_key_header(Block),
            {P,B} = case find_blocks_produced(RegBlocks, aec_headers:miner(KeyHeader)) of
                [] ->  % no blocks mined by this producer
                    {[], Header};
                [FoundHeader | _] -> % found a double block from this producer
                    case aec_headers:root_hash(Header) =/= aec_headers:root_hash(FoundHeader) of
                        true ->
                            {[{dbl_keyblock, Header, FoundHeader}], none};
                        false ->
                            {[], Header}
                    end
            end,
            {ok, Leader} = aec_consensus_hc:leader_for_height(Height),
            case Leader =:= aec_headers:miner(KeyHeader) of
                true ->
                    {P,B};
                false ->
                    {[{invalid_producer, Header} | P], Header}
            end;
        micro ->
            case validate_micro_signature(Header) of
                {error, _} ->
                    {[{invalid_producer, Header}], none}; %
                ok ->
                    case get_micro_block(RegBlocks) of
                        [] ->
                            {[], Header};
                        [FoundHeader | _] ->
                            case aec_headers:root_hash(Header) =/= aec_headers:root_hash(FoundHeader) of
                                true ->
                                    {[{dbl_microblock, Header, FoundHeader}], none};
                                false ->
                                    {[], Header}
                            end
                    end
            end
    end,
    lager:debug("Pens: ~p", [State#loop_state.penalties]),
    {ok, Pens, add_block_at_height(Height, BlockToAdd, State)}.

get_blocks_for_height(Height, #loop_state{block_height_map = BM}) ->
    maps:get(Height, BM, []).

register_penalties(Pens, State) ->
    lager:debug("register_penalties: ~p", [Pens]),
    State#loop_state{penalties = [Pens | State#loop_state.penalties]}.

add_block_at_height(Height, Block, State) ->
    CacheLevel = State#loop_state.cache_level,
    CleanedMap = case CacheLevel > 0 of
        true ->
            maps:remove(Height-CacheLevel, State#loop_state.block_height_map);
        false ->
            State#loop_state.block_height_map
        end,
    RetMap = case Block of
        none ->
            CleanedMap;
        _ ->
            NewBlocks = [ Block | maps:get(Height, State#loop_state.block_height_map, []) ],
            maps:put(Height, NewBlocks, CleanedMap)
    end,
    %lager:debug("pen_block_cache_size: ~p", [maps:size(RetMap)]),
    State#loop_state{block_height_map = RetMap}.

find_blocks_produced(Blocks, Producer) ->
    %lager:debug("find_blocks_produced_by: ~p", [Producer]),
    lists:filter(fun(Block) ->
        aec_headers:type(Block) =:= key andalso aec_headers:miner(Block) =:= Producer end, Blocks).

validate_micro_signature(Header) ->
    Height = aec_headers:height(Header),
    case aec_consensus_hc:leader_for_height(Height) of
        {ok, Leader} ->
            case aeu_sig:verify(Header, Leader) of
                ok         -> ok;
                {error, _} -> {error, signature_verification_failed}
            end;
        {error, _} ->
            {error, signature_verification_failed}
    end.

get_micro_block(Blocks) ->
    lists:filter(fun(Block) -> aec_headers:type(Block) =:= micro end, Blocks).
