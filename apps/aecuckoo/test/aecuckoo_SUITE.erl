%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Basic sanity checks and examples on Cuckoo cycle PoW executables.
%%% @end
%%%-------------------------------------------------------------------
-module(aecuckoo_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0,
    init_per_group/2, end_per_group/2
   ]).

%% test case exports
-export([smoke_test/1]).

-include_lib("common_test/include/ct.hrl").

-define(TEST_MODULE, aecuckoo).

all() ->
    [
     {group, smoke_tests_16}
    ].

groups() ->
    [
     {smoke_tests_16, [{group, mean16},
                       {group, lean16}]},
     {mean16, [smoke_test]},
     {lean16, [smoke_test]}
    ].

init_per_group(smoke_tests_16, Config) ->
    [{nonce, 66},
     {cyclehash, "4851c33f03e1a1403ac902b921f8551e35aedeae5bf0a76a0815fb83597ad653"},
     {verifier, verify16} | Config];
init_per_group(mean16, Config) ->
    [{miner, 'mean16s-generic'} | Config];
init_per_group(lean16, Config) ->
    [{miner, 'lean16'} | Config].

end_per_group(_Group, _Config) ->
    ok.

smoke_test(Config) ->
    Nonce = ?config(nonce, Config),
    ExpectedVerifierCycleHash = ?config(cyclehash, Config),
    Verifier = ?config(verifier, Config),
    Miner = ?config(miner, Config),

    LibDir = ?TEST_MODULE:lib_dir(),
    MinBin = ?TEST_MODULE:bin(atom_to_list(Miner)),
    VerBin = ?TEST_MODULE:bin(atom_to_list(Verifier)),
    LdLibPathVarName = ld_lib_path_var_name(),
    Cmd = io_lib:format("env ~s='~s' '~s' -n ~B "
                        "| grep '^Solution '"
                        "| env ~s='~s' '~s' -n ~B "
                        "| grep '^Verified '",
                        [LdLibPathVarName, LibDir,
                         MinBin, Nonce,
                         LdLibPathVarName, LibDir,
                         VerBin, Nonce]),
    ct:log("Command: ~s~n", [Cmd]),
    CmdRes = lib:nonl(os:cmd(Cmd)),
    ExpCmdRes = "Verified with cyclehash " ++ ExpectedVerifierCycleHash,
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
