%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Helpers to export chain and transactions to disk.
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_export).

%=== EXPORTS ===================================================================

-export([chain_fold/3]).
-export([to_disklog/1]).

%=== INCLUDES ==================================================================

-include_lib("apps/aecore/include/blocks.hrl").

%=== TYPES =====================================================================

-type header() :: #{
    genesis := binary(),
    height := non_neg_integer(),
    hostname := string(),
    time := calendar:datetime()
}.
-type fold_open_fun() :: fun((header()) ->
    {ok, State :: term()} | {error, Reason :: term()}).
-type fold_close_fun() :: fun((State :: term()) -> {ok, Result :: term()}).
-type fold_block_fun() :: fun((Block :: #block{}, State :: term()) ->
    {ok, State :: term()} | {error, Reason :: term()}).

%=== API FUNCTIONS =============================================================

%% @doc Folds all the chain's blocks.
%% Requires a callback to initialize the exporter, a callback called for
%% each blocks from top to genesis, and a callback to close the exporter.
-spec chain_fold(fold_open_fun(), fold_block_fun(), fold_close_fun()) ->
    {ok, term()} | {error, term()}.
chain_fold(OpenFun, BlockFun, CloseFun) ->
    ProcDef = exporter_start(self(), OpenFun, BlockFun, CloseFun),
    exporter_wait_terminated(ProcDef).

%% @doc Exports the chain to a disklog file.
-spec to_disklog(string() | binary()) -> ok | {error, term()}.
to_disklog(FilePath) ->
    case validate_file_path(FilePath) of
        {error, _Reason} = Error -> Error;
        ok ->
            OpenFun = fun(Header) ->
                LogName = erlang:make_ref(),
                LogOpts = [{name, LogName}, {file, FilePath}, {head, Header}],
                case disk_log:open(LogOpts) of
                    {error, _Reason} = Error -> Error;
                    {ok, Log} -> {ok, {Log, 0}}
                end
            end,
            BlockFun = fun(#block{} = Block, {Log, Count}) ->
                Data = aec_blocks:serialize_to_binary(Block),
                case disk_log:blog(Log, Data) of
                    {error, _Reason} = Error -> Error;
                    ok -> {ok, {Log, Count + 1}}
                end
            end,
            CloseFun = fun({Log, Count}) ->
                ok = disk_log:close(Log),
                {ok, Count}
            end,
            chain_fold(OpenFun, BlockFun, CloseFun)
    end.

%=== INTERNAL FUNCTIONS ========================================================

validate_file_path(FilePath) ->
    case filelib:is_file(FilePath) of
        true -> {error, already_exists};
        false ->
            case filelib:ensure_dir(FilePath) of
                {error, _Reason} = Error -> Error;
                ok -> ok
            end
    end.

prev_block(#block{ prev_hash = Hash }) ->
    {ok, Block} = aec_chain:get_block(Hash),
    Block.

exporter_start(Caller, OpenFun, BlockFun, CloseFun) ->
    CallRef = erlang:make_ref(),
    State = #{
        caller => Caller,
        ref => CallRef,
        open_fun => OpenFun,
        block_fun => BlockFun,
        close_fun => CloseFun
    },
    ProcFun = fun() -> exporter_init(State) end,
    {Pid, MonRef} = erlang:spawn_opt(ProcFun, [monitor]),
    {Pid, MonRef, CallRef}.

exporter_wait_terminated({Pid, MonRef, CallRef}) ->
    receive
        {'DOWN', MonRef, process, Pid, _Info} ->
            {error, exporter_died};
        {done, CallRef, Result} ->
            erlang:demonitor(MonRef, [flush]),
            Result
    end.

exporter_call(#{ caller := Caller, ref := Ref }, Tag, Value) ->
    Caller ! {Tag, Ref, Value}.

exporter_init(#{ open_fun := OpenFun } = State) ->
    GenesisHash = aec_chain:genesis_hash(),
    {ok, Genesis} = aec_chain:get_block(GenesisHash),
    TopBlock = #block{ height = Height } = aec_chain:top_block(),
    {ok, Hostname} = inet:gethostname(),
    DateTime = calendar:universal_time(),
    Header = #{
        genesis => GenesisHash,
        height => Height,
        hostname => Hostname,
        time => DateTime
    },
    case OpenFun(Header) of
        {error, Reason} ->
            exporter_call(State, done, {error, Reason});
        {ok, Sub} ->
            exporter_loop(State#{ sub => Sub }, Genesis, TopBlock)
    end.

exporter_loop(#{ sub := Sub, block_fun := BlockFun } = State, Genesis, Block) ->
    case BlockFun(Block, Sub) of
        {error, Reason} ->
            exporter_error(State, Reason);
        {ok, Sub2} ->
            State2 = State#{ sub := Sub2 },
            case Genesis =:= Block of
                false -> exporter_loop(State2, Genesis, prev_block(Block));
                true -> exporter_done(State2)
            end
    end.

exporter_done(#{ sub := Sub, close_fun := CloseFun } = State) ->
    {ok, Result} = CloseFun(Sub),
    exporter_call(State, done, {ok, Result}).

exporter_error(#{ sub := Sub, close_fun := CloseFun } = State, Reason) ->
    CloseFun(Sub),
    exporter_call(State, done, {error, Reason}).
