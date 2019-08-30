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

            Deps = ["aesophia", "aesophia_cli", "aeserialization", "aebytecode"],
            DepsHashes = [calculate_git_revision(RootDir, Dep) || Dep <- Deps],
            DepsHash = base58:binary_to_base58(crypto:hash(sha256, string:join(DepsHashes, ""))),

            ct:log("Cache: ~p, DepsHash: ~p", [CacheDir, DepsHash]),

            [try_load_cache(CacheDir, DepsHash, ETSTable) || {ETSTable, _} <- cached_tables()],

            {ok, #state{cache_dir = CacheDir, deps_hash = DepsHash}};
        _ ->
            ct:log("Ignoring persisted contract cache in system/smoke tests"),
            {ok, #state{}}
    end.

terminate(#state{cache_dir = undefined, deps_hash = undefined}) ->
    ok;
terminate(#state{cache_dir = CacheDir, deps_hash = DepsHash}) ->
    [save_cache(CacheDir, DepsHash, ETSTable) || {ETSTable, _} <- cached_tables()],
    ok.

%%% ------------------------ INTERNAL --------------------

calculate_git_revision(RootDir, GitDep) ->
    PathComponents = [RootDir, "_build", "test", "lib", GitDep],
    Path = filename:join(PathComponents),
    Cmd = io_lib:format("cd ~p; git rev-parse HEAD", [Path]),
    os:cmd(Cmd).

cache_dir(RootDir) ->
    filename:absname_join(RootDir, ".contracts_test_cache").

ensure_cache_dir_exists(CacheDir) ->
    case file:make_dir(CacheDir) of
        ok ->
            ok;
        {error, eexist} ->
            ok
    end.

ets_cache_filename(DepsHash, ETSTable) ->
    atom_to_list(ETSTable) ++ "_" ++ DepsHash.

ets_cache_path(CacheDir, DepsHash, ETSTable) ->
    filename:absname_join(CacheDir, ets_cache_filename(DepsHash, ETSTable)).

try_load_cache(CacheDir, DepsHash, ETSTable) ->
    Path = ets_cache_path(CacheDir, DepsHash, ETSTable),
    case filelib:is_file(Path) of
        true ->
            ct:log("Restoring persisted cache for: ~p", [ETSTable]),
            case ets:info(ETSTable, name) of
                undefined ->
                    ok;
                _ ->
                    true = ets:delete(ETSTable)
            end,
            {ok, ETSTable} = ets:file2tab(Path, [{verify, true}]),
            ok;
        false ->
            ct:log("Persisted cache for: ~p not available", [ETSTable]),
            ok
    end.

save_cache(CacheDir, DepsHash, ETSTable) ->
    Path = ets_cache_path(CacheDir, DepsHash, ETSTable),
    _ = ets:tab2file(ETSTable, Path, [{sync, true}, {extended_info, [md5sum, object_count]}]),
    ok.
