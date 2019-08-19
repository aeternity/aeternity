%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc CT hook for restoring a persisted contract cache.
%%%      Due to it subsequent test runs don't need to recompile contracts.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_cache_restorer).

%% Callbacks
-export([id/1]).
-export([init/2]).
-export([terminate/1]).

-include("include/aect_contract_cache.hrl").

-record(state, {cache_dir :: string() | undefined, deps_hash :: binary() | undefined}).

%% Return a unique id for this CTH.
id(_Opts) ->
    aect_cache_restorer.

init(_Id, _Opts) ->
    {ok, Cwd} = file:get_cwd(),
    case string:split(Cwd, "_build") of
        [RootDir, _] ->
            CacheDir = cache_dir(RootDir),
            ensure_cache_dir_exists(CacheDir),

            RebarLockPath = filename:absname_join(RootDir, "rebar.lock"),
            ct:log("RebarPath: ~p Cache: ~p", [RebarLockPath, CacheDir]),
            {ok, RebarLock} = file:read_file(RebarLockPath),
            DepsHash = base58:binary_to_base58(crypto:hash(sha256, RebarLock)),

            code:ensure_loaded(aect_test_utils), %% This should initialize the tables
            [try_load_cache(CacheDir, DepsHash, ETSTable) || {ETSTable, _} <- cached_tables()],

            {ok, #state{cache_dir = CacheDir, deps_hash = DepsHash}};
        _ ->
            ct:log("Ignoring persisted contract cache in system/smoke tests"),
            {ok, #state{}}
    end.

terminate(#state{cache_dir = undefined, deps_hash = undefined}) ->
    ok;
terminate(#state{cache_dir = CacheDir, deps_hash = DepsHash}) ->
    [save_cache(CacheDir, DepsHash, ETSTable, Keypos) || {ETSTable, Keypos} <- cached_tables()],
    ok.

%%% ------------------------ INTERNAL --------------------

cache_dir(RootDir) ->
    filename:absname_join(RootDir, ".contracts_test_cache").

ensure_cache_dir_exists(CacheDir) ->
    case file:make_dir(CacheDir) of
        ok ->
            ok;
        {error, eexist} ->
            ok
    end.

dets_cache_filename(DepsHash, ETSTable) ->
    atom_to_list(ETSTable) ++ "_" ++ DepsHash.

dets_cache_path(CacheDir, DepsHash, ETSTable) ->
    filename:absname_join(CacheDir, dets_cache_filename(DepsHash, ETSTable)).

try_load_cache(CacheDir, DepsHash, ETSTable) ->
    Path = dets_cache_path(CacheDir, DepsHash, ETSTable),
    case dets:open_file(Path) of
        {ok, Handle} ->
            try
                ETSTable = dets:to_ets(Handle, ETSTable)
            after
                dets:close(Handle)
            end,
            ok;
        _ ->
            ok
    end.

save_cache(CacheDir, DepsHash, ETSTable, Keypos) ->
    Path = dets_cache_path(CacheDir, DepsHash, ETSTable),
    {ok, ETSTable} = dets:open_file(ETSTable, [{file, Path}, {keypos, Keypos}, {type, set}]),
    try
        ok = dets:from_ets(ETSTable, ETSTable)
    after
        dets:close(ETSTable)
    end,
    ok.
