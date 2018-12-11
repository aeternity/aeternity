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
     {group, smoke_tests_15}
    ].

groups() ->
    [
     {smoke_tests_15, [{group, mean15},
                       {group, lean15}]},
     {mean15, [smoke_test]},
     {lean15, [smoke_test]}
    ].

init_per_group(smoke_tests_15, Config) ->
    [{nonce, 91} | Config];
init_per_group(mean15, Config) ->
    [{miner, 'mean15-generic'} | Config];
init_per_group(lean15, Config) ->
    [{miner, 'lean15-generic'} | Config].

end_per_group(_Group, _Config) ->
    ok.

smoke_test(Config) ->
    Nonce = ?config(nonce, Config),
    ExpectedVerifierCycleHash = ?config(cyclehash, Config),
    Miner = ?config(miner, Config),

    LibDir = ?TEST_MODULE:lib_dir(),
    MinBin = ?TEST_MODULE:bin(atom_to_list(Miner)),
    Cmd = io_lib:format("'~s' -n ~B | grep '^Solution'", [MinBin, Nonce]),
    ct:log("Command: ~s~n", [Cmd]),
    CmdRes = lib:nonl(os:cmd(Cmd)),
    ct:log("Command result: ~s~n", [CmdRes]),

    Solution = lists:map(fun(X) -> list_to_integer(X, 16) end, tl(string:tokens(CmdRes, " "))),
    HeaderEquivalent = <<0:(44*8), (base64:encode(<<Nonce:64/little-unsigned-integer>>))/binary, 0:(24*8)>>,

    42 = length(Solution),
    true = aec_pow_cuckoo:verify_proof_(HeaderEquivalent, Solution, 15),

    ok.

