%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Utils for calling the Cuckoo cycle PoW executables.
%%% @end
%%%=============================================================================
-module(ae_cuckoo_cycle_pow_executables).

%% API
-export([bin_dir/0,
         bin/1,
         lib_dir/0]).

%% Returns the path of the directory where the miner and verifier
%% executables are.
-spec bin_dir() -> file:filename_all().
bin_dir() ->
    filename:join([priv_dir(), "bin"]).

%% Returns the path of the file of the specified executable.
-spec bin(nonempty_string()) -> file:filename_all().
bin(ExecutableBasename) ->
    filename:join([bin_dir(), ExecutableBasename]).

%% Returns the directory where the shared-objects files needed by
%% miner and verifier executables are.
-spec lib_dir() -> file:filename_all().
lib_dir() ->
    filename:join([priv_dir(), "lib"]).

priv_dir() ->
    code:priv_dir(ae_cuckoo_cycle_pow_executables).
