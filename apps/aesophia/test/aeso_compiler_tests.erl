%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Test Sophia language compiler.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeso_compiler_tests).

-include_lib("eunit/include/eunit.hrl").

%% simple_compile_test_() -> ok.
%%  Very simply test compile the given contracts. Only basic checks
%%  are made on the output, just that it is a binary.

simple_compile_test_() ->
    {setup,
     fun () -> ok end,                          %Setup
     fun (_) -> ok end,                         %Cleanup
     [ {"Testing the " ++ Contract ++ " contract",
        fun() ->
                ?assertMatch(Code when is_binary(Code), aeso_compiler:file(Contract))
        end} ||
         Contract <- compilable_contracts() ]}.

%% compilable_contracts() -> [ContractName].
%%  The currently compilable contracts.

compilable_contracts() ->
    ["complex_types",
     "counter",
     "dutch_auction",
     "environment",
     "factorial",
     "identity",
     "remote_call",
     "simple",
     "spend_test",
     "stack",
     "test"
    ].
