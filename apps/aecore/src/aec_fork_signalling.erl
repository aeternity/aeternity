%%%============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% H1 - height before the the signalling start height;
%%% HS - signalling start height, HS is strictly greater than H1;
%%% HE - signalling end height, HE is strictly greater than HS;
%%% H2 - fork height, H2 is strictly greater than HE;
%%%
%%% The gen_server process (aec_fork_signalling) makes sure that there is a
%%% fork signalling result at H2 ('true', 'false' or 'pending_protocol'). In
%%% order to compute the fork signalling result, it manages asynchronous
%%% processes that walk the chain and compute how many blocks include a
%%% predefined info field value between HE - 1 and HS (inclusive).
%%%
%%% The block at height HE - 1 is the last signalling block, and there must be
%%% a result computed for it. Once a worker process that computes the result is
%%% spawned, a 'pending_protocol' atom for the given block hash is stored in
%%% results map. Once the worker process is done with the computation it will
%%% send a boolean result and the 'pending_protocol' atom is replaced with the
%%% boolean result.
%%%
%%% The blocks with height between HE and H2 - 1 (inclusive) are treated
%%% differently. Once such block arrives, its previous key hash is looked up in
%%% the results map. If it's present, it means that the previous key hash
%%% belongs to the block at height HE - 1 (which has result set to 'true',
%%% 'false' or 'pending_protocol'), or the previous key hash has a pointer (key
%%% block hash) to a block at height HE - 1.
%%%
%%% The pointers are used because the result at height HE - 1 can change
%%% asynchronosly (from 'pending_protocol' to a boolean value). Moreover, it's
%%% not necessary to search a block at HE - 1 height to get the result in case
%%% a new block arrives. Instead, the results map may have an entry for
%%% previous key hash pointing to the block with HE - 1 height.
%%%
%%% The worst case scenario is restarting the node at height H2 - 1. If the
%%% node has the top key block at height H2 - 1 and the results map is empty,
%%% it needs to start a worker process that will search for a block at HE - 1,
%%% and then it will start computing the fork signalling result. Some
%%% components of the system may be waiting for the final result.
%%
%% @end
%%% ============================================================================
-module(aec_fork_signalling).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_fork_result/3,
         compute_fork_result/3,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-export([worker_process/4]).

-type key_block()        :: aec_blocks:key_block().

-type block_hash()       :: aec_blocks:block_header_hash().

-type block_height()     :: aec_blocks:height().

-type fork()             :: aec_hard_forks:fork().

-type worker()           :: {pid(), reference()}.

-type result()           :: true | false | pending_protocol.

%% Result entry apart from the actual fork signalling result contains block
%% hash and a block height associated with a pointer that currently points to
%% this result entry. Based on the block height it is possible to keep just the
%% pointer associated with the top block.
-type result_entry()     :: {result(), block_hash(), block_height()}.

%% The pointer is a key block hash which belongs to the last signalling block
%% (the block at height HE - 1).
-type pointer()          :: block_hash().

-record(state, {results  :: #{block_hash() := result_entry() | pointer()},
                workers  :: #{worker() := block_hash()}
               }).

-define(SERVER, ?MODULE).
-define(IS_RESULT_ENTRY(X), is_tuple(X)).
-define(IS_POINTER(Hash), is_binary(Hash)).

%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_fork_result(key_block(), block_hash(), fork()) ->
                             {ok, result()} | {error, term()}.
get_fork_result(Block, BlockHash, Fork) ->
    gen_server:call(?SERVER, {get_fork_result, Block, BlockHash, Fork}).

-spec compute_fork_result(key_block(), block_hash(), fork()) -> ok | skipped.
compute_fork_result(Block, BlockHash, Fork) ->
    case get_action_from_block_height(Block, Fork) of
        Action when Action =/= skip_block ->
            gen_server:cast(?SERVER, {compute_fork_result, Action, Block,
                                      BlockHash, Fork});
        skip_block ->
            skipped
    end.

stop() ->
    gen_server:stop(?SERVER).

%% gen_server callbacks

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{results = maps:new(), workers = maps:new()}}.

handle_call({get_fork_result, Block, BlockHash, Fork}, _From, State) ->
    {Reply, State1} = handle_get_fork_result(Block, BlockHash, Fork, State),
    {reply, Reply, State1};
handle_call(results, _From, #state{results = Results} = State) ->
    {reply, {ok, Results}, State};
handle_call(_Request, _From, State) ->
    {reply, ingnored, State}.

handle_cast({compute_fork_result, Action, Block, BlockHash, Fork}, State) ->
    {noreply, handle_compute_fork_result(Action, Block, BlockHash, Fork, State)};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({worker_msg, WorkerPid, Msg}, State) ->
    {noreply, handle_worker_msg(WorkerPid, Msg, State)};
handle_info({'DOWN', Ref, process, WorkerPid, Rsn}, State) ->
    {noreply, handle_down({WorkerPid, Ref}, Rsn, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Rsn, #state{workers = Workers} = State) ->
    maps:fold(fun(W, _BH, S) -> kill_worker(W, S) end, State, Workers).

%% Internal functions

get_action_from_block_height(Block, Fork) ->
    case {is_last_signalling_block(Block, Fork),
          is_signalling_result_required(Block, Fork)} of
        {true, _}      -> spawn_worker;
        {false, true}  -> set_prev_result_or_spawn_worker;
        {false, false} -> skip_block
    end.

handle_get_fork_result(Block, BlockHash, Fork, #state{results = Results} = State) ->
    case is_last_block_before_fork(Block, Fork) of
        true ->
            %% Only pointer and 'undefined' are considered. If the actual
            %% result was returned, it would mean HE - 1 == H2 - 1 and so
            %% HE < H2 wouldn't apply (the configuration check handles this
            %% situation).
            case get_result(BlockHash, Results) of
                LastSigBlockHash when ?IS_POINTER(LastSigBlockHash) ->
                    %% The block at height H2 - 1 is supposed to have a pointer
                    %% (in the best case scenario) to the block hash at height
                    %% HE - 1 with the actual result.
                    {Result, _BlockHash, _BlockHeight} = get_result(LastSigBlockHash, Results),
                    {{ok, Result}, State};
                undefined ->
                    %% If there is no entry, a worker needs to be spawned and
                    %% the returned result is 'pending_protocol'.
                    State1 = spawn_worker(Block, BlockHash, Fork, State),
                    {{ok, pending_protocol}, State1}
            end;
        false ->
            {{error, not_last_block_before_fork}, State}
    end.

handle_compute_fork_result(spawn_worker, Block, BlockHash, Fork, State) ->
    spawn_worker(Block, BlockHash, Fork, State);
handle_compute_fork_result(set_prev_result_or_spawn_worker, Block, BlockHash, Fork, State) ->
    set_prev_result_or_spawn_worker(Block, BlockHash, Fork, State).

handle_worker_msg(WorkerPid, {check_result, BlockHash, BlockHeight, LastSigBlockHash},
                  #state{results = Results} = State) ->
    case get_result(LastSigBlockHash, Results) of
        {Result, BlockHash1, BlockHeight1} when BlockHeight > BlockHeight1 ->
            %% Another process computed or is computing the result for the same
            %% LastSigBlockHash (at height HE - 1). Since the process that sent
            %% this message found the same LastSigBlockHash as another process
            %% before, both processes do it on the same chain fork. Since the
            %% results map now has an entry for LastSigBlockHash, which
            %% includes the actual result of the fork signalling computation,
            %% and info about the block at the top of this chain fork pointing
            %% to this result (including the height), it's possible to update
            %% this pointer based on the height - the process that sent this
            %% message tries to compute the fork signalling result for a block
            %% on the same chain fork, but it's at a greater height. Only the
            %% pointer from the top of the chain is kept in the results map. In
            %% other words, each LastSigblockHash has just one pointer pointing
            %% to it, this pointer is associated witht the top block.

            %% The process that sent this message is aborted as another one
            %% computed/is computing the fork signalling result.
            send_server_msg(WorkerPid, abort),
            %% Update the result for LastSigblockHash
            ResultEntry = {Result, BlockHash, BlockHeight},
            Results1 = add_result(LastSigBlockHash, ResultEntry, Results),
            %% Replace the old pointer with a new one (associated with a block
            %% at greater height).
            Results2 = add_result(BlockHash, LastSigBlockHash, Results1),
            Results3 = del_result(BlockHash1, Results2),
            State#state{results = Results3};
        {_Result, _BlockHash1, _BlockHeight1} ->
            State;
        undefined ->
            %% The process that sent this message is the first one to find
            %% LastSigBlockHash and so can continue with computation of the
            %% fork signalling result.
            send_server_msg(WorkerPid, compute),
            ResultEntry = {pending_protocol, BlockHash, BlockHeight},
            Results1 = add_result(LastSigBlockHash, ResultEntry, Results),
            %% If the BlockHash is the same as LastSigblockHash, it means that
            %% the pointer to the LastSigblockHash is not needed (as it would
            %% just point to itself).
            Results2 =
                case BlockHash =:= LastSigBlockHash of
                    true  -> Results1;
                    false -> add_result(BlockHash, LastSigBlockHash, Results1)
                end,
            State#state{results = Results2}
    end;
handle_worker_msg(_WorkerPid, {update_result, LastSigBlockHash, Result},
                  #state{results = Results} = State) ->
    {_Result1, BlockHash, BlockHeight} = get_result(LastSigBlockHash, Results),
    ResultEntry = {Result, BlockHash, BlockHeight},
    Results1 = add_result(LastSigBlockHash, ResultEntry, Results),
    State#state{results = Results1}.

handle_down(Worker, _Rsn, #state{workers = Workers} = State) ->
    State#state{workers = del_worker(Worker, Workers)}.

spawn_worker(Block, BlockHash, Fork, #state{workers = Workers} = State) ->
    Worker = new_worker([Block, BlockHash, Fork]),
    State#state{workers = add_worker(Worker, BlockHash, Workers)}.

kill_worker({Pid, Ref} = Worker, #state{workers = Workers} = State) ->
    cancel_monitor(Ref),
    exit(Pid, shutdown),
    flush_worker_msgs(),
    State#state{workers = del_worker(Worker, Workers)}.

set_prev_result_or_spawn_worker(Block, BlockHash, Fork, #state{results = Results} = State) ->
    PrevKeyBlockHash = aec_blocks:prev_key_hash(Block),
    BlockHeight = aec_blocks:height(Block),
    case get_result(PrevKeyBlockHash, Results) of
        {Result, _BlockHash1, _BlockHeight1} ->
            %% The PrevKeyBlockHash key is associated with a result
            %% (PrevKeyBlockHash is a block hash of block at height HE - 1).
            %% The BlockHash is set to point to the PrevKeyBlockHash (so
            %% it's possible to retrieve the result for BlockHash).
            ResultEntry = {Result, BlockHash, BlockHeight},
            Results1 = add_result(PrevKeyBlockHash, ResultEntry, Results),
            Results2 = add_result(BlockHash, PrevKeyBlockHash, Results1),
            State#state{results = Results2};
        LastSigBlockHash when ?IS_POINTER(LastSigBlockHash) ->
            %% The PrevKeyBlockHash is associated with a pointer that points to
            %% a block hash at height HE - 1 (LastSigBlockHash), which has the
            %% result. The BlockHash is set to point to the same block hash at
            %% height HE - 1. The PrevKeyBlockHash associated with the pointer
            %% can now be discarded as just the pointer associated with the top
            %% block is needed (Block has greater height than PrevKeyBlock).
            {Result, _BlockHash1, _BlockHeight1} = get_result(LastSigBlockHash, Results),
            ResultEntry = {Result, BlockHash, BlockHeight},
            Results1 = add_result(LastSigBlockHash, ResultEntry, Results),
            Results2 = add_result(BlockHash, LastSigBlockHash, Results1),
            Results3 = del_result(PrevKeyBlockHash, Results2),
            State#state{results = Results3};
        undefined ->
            %% There is no result reachable via PrevKeyBlockHash. A new process
            %% is spawned, it will set both a pointer (for BlockHash) and a
            %% result for a block hash at height HE - 1.
            spawn_worker(Block, BlockHash, Fork, State)
    end.

worker_process(ParentPid, Block, BlockHash, Fork) ->
    Self = self(),
    BlockHeight = aec_blocks:height(Block),
    lager:debug("Worker started, block hash: ~p, block height: ~p, pid: ~p",
                [BlockHash, BlockHeight, Self]),
    case search_last_signalling_block(Block, BlockHash, Fork) of
        {ok, LastSigBlock, LastSigBlockHash} ->
            lager:debug("Worker found last signalling block: ~p, pid: ~p", [LastSigBlockHash, Self]),
            send_worker_msg(ParentPid, {check_result, BlockHash, BlockHeight, LastSigBlockHash}),
            case await_server_msg(ParentPid) of
                {ok, compute} ->
                    lager:debug("Worker started fork signalling result computation, pid: ~p", [Self]),
                    case compute_fork_signalling_result(LastSigBlock, LastSigBlockHash, Fork) of
                        {ok, Result} ->
                            lager:debug("Worker computed fork signalling result: ~p, pid: ~p",
                                        [Result, Self]),
                            send_worker_msg(ParentPid, {update_result, LastSigBlockHash, Result});
                        {error, Rsn} = Err ->
                            lager:error("Worker fork signalling result computation error: ~p, pid: ~p",
                                        [Rsn, Self]),
                            Err
                    end;
                {ok, abort} ->
                    lager:debug("Worker fork signalling result computation aborted, pid: ~p", [Self]),
                    aborted;
                {error, Rsn} = Err ->
                    lager:error("Worker await server message error: ~p, pid: ~p", [Rsn, Self]),
                    Err
            end;
        {error, Rsn} = Err ->
            lager:error("Worker search last signalling block error: ~p, pid: ~p", [Rsn, Self]),
            Err
    end.

search_last_signalling_block(Block, BlockHash, Fork) ->
    case is_last_signalling_block(Block, Fork) of
        true ->
            {ok, Block, BlockHash};
        false ->
            PrevKeyBlockHash = aec_blocks:prev_key_hash(Block),
            case get_key_block(PrevKeyBlockHash) of
                Block1 when Block1 =/= not_found ->
                    search_last_signalling_block(Block1, PrevKeyBlockHash, Fork);
                not_found ->
                    {error, block_not_found}
            end
    end.

compute_fork_signalling_result(Block, BlockHash, Fork) ->
    case compute_fork_signalling_result(Block, BlockHash, Fork, 0) of
        {ok, Count}         -> {ok, signalling_result(Count, Fork)};
        {error, _Rsn} = Err -> Err
    end.

compute_fork_signalling_result(Block, _BlockHash, Fork, Count) ->
    case is_first_signalling_block(Block, Fork) of
        true ->
            {ok, Count + count_inc(is_matching_info_present(Block, Fork))};
        false ->
            PrevKeyBlockHash = aec_blocks:prev_key_hash(Block),
            case get_key_block(PrevKeyBlockHash) of
                Block1 when Block1 =/= not_found ->
                    Count1 = Count + count_inc(is_matching_info_present(Block, Fork)),
                    compute_fork_signalling_result(Block1, PrevKeyBlockHash, Fork, Count1);
                not_found ->
                    {error, block_not_found}
            end
    end.

is_first_signalling_block(Block, #{signalling_start_height := StartHeight}) ->
    aec_blocks:height(Block) =:= StartHeight.

is_last_signalling_block(Block, #{signalling_end_height := EndHeight}) ->
    aec_blocks:height(Block) =:= (EndHeight - 1).

is_last_block_before_fork(Block, #{fork_height := ForkHeight}) ->
    aec_blocks:height(Block) =:= (ForkHeight - 1).

is_signalling_result_required(Block, #{signalling_end_height := EndHeight,
                                       fork_height := ForkHeight}) ->
    BlockHeight = aec_blocks:height(Block),
    (BlockHeight >= (EndHeight - 1)) andalso (BlockHeight < ForkHeight).

signalling_result(Count, #{signalling_block_count := BlockCount}) ->
    Count >= BlockCount.

is_matching_info_present(Block, #{info_field := InfoField}) ->
    Info = aec_headers:info(aec_blocks:to_header(Block)),
    Info =:= InfoField.

get_key_block(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} -> Block;
        error       -> not_found
    end.

add_result(BlockHash, LastSigBlockHash, Results) when ?IS_POINTER(LastSigBlockHash) ->
    maps:put(BlockHash, LastSigBlockHash, Results);
add_result(BlockHash, ResultEntry, Results) ->
    maps:put(BlockHash, ResultEntry, Results).

del_result(BlockHash, Results) ->
    maps:remove(BlockHash, Results).

get_result(BlockHash, Results) ->
    maps:get(BlockHash, Results, undefined).

new_worker(Args) ->
    spawn_monitor(?MODULE, worker_process, [self() | Args]).

add_worker(Worker, BlockHash, Workers) ->
    maps:put(Worker, BlockHash, Workers).

del_worker(Worker, Workers) ->
    maps:remove(Worker, Workers).

count_inc(true)  -> 1;
count_inc(false) -> 0.

send_worker_msg(ParentPid, Msg) ->
    ParentPid ! {worker_msg, self(), Msg}.

send_server_msg(WorkerPid, Msg) ->
    WorkerPid ! {server_msg, self(), Msg}.

await_server_msg(ParentPid) ->
    receive
        {server_msg, ParentPid, Msg} ->
            {ok, Msg}
    after
        5000 ->
            {error, timeout}
    end.

cancel_monitor(Ref) when is_reference(Ref) ->
    demonitor(Ref, [flush]).

flush_worker_msgs() ->
    receive
        {worker_msg, _Pid, _Msg} -> ok
    after
        0 -> ok
    end.
