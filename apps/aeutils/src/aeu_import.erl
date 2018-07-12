%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Helpers to export chain and transactions to disk.
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_import).

%=== EXPORTS ===================================================================

-export([disklog_fold/3]).

%=== INCLUDES ==================================================================

-include_lib("apps/aecore/include/blocks.hrl").

%=== TYPES =====================================================================

-type header() :: #{
    genesis := binary(),
    height := non_neg_integer(),
    hostname := string(),
    time := calendar:datetime()
}.
-type fold_init_fun() :: fun((header()) ->
    {ok, State :: term()} | {error, Reason :: term()}).
-type fold_block_fun() :: fun((Block :: #block{}, State :: term()) ->
    {ok, State :: term()} | {error, Reason :: term()}).

%=== API FUNCTIONS =============================================================

%% @doc Folds the blocks from a disklog file.
%% Takes a cllaback for initializing with the header and a callback for each
%% blocks called in the order they were stored.
-spec disklog_fold(string() | binary(), fold_init_fun(), fold_block_fun())
    -> {ok, term()} | {error, term()}.
disklog_fold(FilePath, InitFun, BlockFun) ->
    LogName = erlang:make_ref(),
    LogOpts = [{name, LogName}, {file, FilePath}, {mode, read_only}],
    case disk_log:open(LogOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Log} ->
            try fold_log(Log, InitFun, BlockFun)
            after disk_log:close(Log)
            end
    end.

%=== INTERNAL FUNCTIONS ========================================================

fold_log(Log, InitFun, BlockFun) ->
    case fold_header(Log, InitFun) of
        {ok, Cont, State} -> fold_blocks(Log, BlockFun, State, Cont);
        Error -> Error
    end.

fold_header(Log, InitFun) ->
    case disk_log:chunk(Log, start, 1) of
        eof -> {error, no_header};
        {Cont, [Header]} ->
            case InitFun(Header) of
                {ok, State} -> {ok, Cont, State};
                Error -> Error
            end;
        {_, _, _} -> {error, corrupted};
        Error -> Error
    end.

fold_blocks(Log, BlockFun, State, Cont) ->
    case disk_log:bchunk(Log, Cont, 1) of
        eof -> {ok, State};
        {error, _Reason} = Error -> Error;
        {Cont2, [BlockBin], 0} ->
            case aec_blocks:deserialize_from_binary(BlockBin) of
                {error, _Reason} = Error -> Error;
                {ok, BlockRec} ->
                    case BlockFun(BlockRec, State) of
                        {error, _Reason} = Error -> Error;
                        {ok, State2} ->
                            fold_blocks(Log, BlockFun, State2, Cont2)
                    end
            end;
        {_, _, _} ->
            {error, corrupted}
    end.
