%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Basic sanity checks and examples on Cuckoo cycle PoW executables.
%%% @end
%%%-------------------------------------------------------------------
-module(ae_cc_pow_executables_SUITE).

%% common_test exports
-export([all/0]).

%% test case exports
-export([smoke_test/1]).

-include_lib("common_test/include/ct.hrl").

-define(TEST_MODULE, ae_cuckoo_cycle_pow_executables).

all() ->
    [smoke_test].

smoke_test(_Config) ->
    LibDir = ?TEST_MODULE:lib_dir(),
    MinBin = ?TEST_MODULE:bin("lean16"),
    VerBin = ?TEST_MODULE:bin("verify16"),
    LdLibPathVarName = ld_lib_path_var_name(),
    Cmd =
        io_lib:format(
          "env ~s='~s' '~s' -n 66 | grep '^Solution ' | env ~s='~s' '~s' -n 66 | grep '^Verified '",
          [LdLibPathVarName, LibDir,
           MinBin,
           LdLibPathVarName, LibDir,
           VerBin]),
    ct:log("Command: ~s~n", [Cmd]),
    CmdRes = lib:nonl(os:cmd(Cmd)),
    ExpCmdRes = "Verified with cyclehash 4851c33f03e1a1403ac902b921f8551e35aedeae5bf0a76a0815fb83597ad653",
    ct:log("Command result: ~s~n", [CmdRes]),
    ct:log("Expected command result: ~s~n", [ExpCmdRes]),
    ExpCmdRes = CmdRes,
    ok.

ld_lib_path_var_name() ->
    ld_lib_path_var_name(os:type()).

ld_lib_path_var_name({unix, darwin}) ->
    "DYLD_LIBRARY_PATH";
ld_lib_path_var_name({unix, _}) ->
    "LD_LIBRARY_PATH".
