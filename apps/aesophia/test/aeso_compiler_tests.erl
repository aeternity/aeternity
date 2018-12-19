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
%%  are made on the output, just that it is a binary which indicates
%%  that the compilation worked.

simple_compile_test_() ->
    {setup,
     fun () -> ok end,                          %Setup
     fun (_) -> ok end,                         %Cleanup
     [ {"Testing the " ++ ContractName ++ " contract",
        fun() ->
            #{byte_code := ByteCode, 
              contract_source := _, 
              type_info := _} = compile(ContractName),
            ?assertMatch(Code when is_binary(Code), ByteCode)
        end} || ContractName <- compilable_contracts() ] ++
     [ {"Testing error messages of " ++ ContractName,
        fun() ->
            {type_errors, Errors} = compile(ContractName),
            ?assertEqual(lists:sort(ExpectedErrors), lists:sort(Errors))
        end} ||
            {ContractName, ExpectedErrors} <- failing_contracts() ]
    }.

compile(Name) ->
    try
        aeso_compiler:from_string(aeso_test_utils:read_contract(Name), [])
    catch _:{type_errors, _} = E ->
        E
    end.

%% compilable_contracts() -> [ContractName].
%%  The currently compilable contracts.

compilable_contracts() ->
    ["complex_types",
     "counter",
     "dutch_auction",
     "environment",
     "factorial",
     "fundme",
     "identity",
     "maps",
     "oracles",
     "remote_call",
     "simple",
     "simple_storage",
     "spend_test",
     "stack",
     "test",
     "builtin_bug",
     "builtin_map_get_bug"
    ].

%% Contracts that should produce type errors

failing_contracts() ->
    [ {"name_clash",
        ["Duplicate definitions of abort at\n  - (builtin location)\n  - line 14, column 3\n",
         "Duplicate definitions of double_def at\n  - line 10, column 3\n  - line 11, column 3\n",
         "Duplicate definitions of double_proto at\n  - line 4, column 3\n  - line 5, column 3\n",
         "Duplicate definitions of proto_and_def at\n  - line 7, column 3\n  - line 8, column 3\n",
         "Duplicate definitions of put at\n  - (builtin location)\n  - line 15, column 3\n",
         "Duplicate definitions of state at\n  - (builtin location)\n  - line 16, column 3\n"]}
    , {"type_errors",
        ["Unbound variable zz at line 17, column 21\n",
         "Cannot unify int\n"
         "         and list(int)\n"
         "when checking the application at line 26, column 9 of\n"
         "  (::) : (int, list(int)) => list(int)\n"
         "to arguments\n"
         "  x : int\n"
         "  x : int\n",
         "Cannot unify string\n"
         "         and int\n"
         "when checking the assignment of the field\n"
         "  x : map(string, string) (at line 9, column 46)\n"
         "to the old value __x and the new value\n"
         "  __x {[\"foo\"] @ x = x + 1} : map(string, int)\n",
         "Cannot unify int\n"
         "         and string\n"
         "when checking the type of the expression at line 34, column 45\n"
         "  1 : int\n"
         "against the expected type\n"
         "  string\n",
         "Cannot unify string\n"
         "         and int\n"
         "when checking the type of the expression at line 34, column 50\n"
         "  \"bla\" : string\n"
         "against the expected type\n"
         "  int\n",
         "Cannot unify string\n"
         "         and int\n"
         "when checking the type of the expression at line 32, column 18\n"
         "  \"x\" : string\n"
         "against the expected type\n"
         "  int\n",
         "Cannot unify string\n"
         "         and int\n"
         "when checking the type of the expression at line 11, column 56\n"
         "  \"foo\" : string\n"
         "against the expected type\n"
         "  int\n",
         "Cannot unify int\n"
         "         and string\n"
         "when comparing the types of the if-branches\n"
         "  - w : int (at line 38, column 13)\n"
         "  - z : string (at line 39, column 10)\n",
         "Not a record type: string\n"
         "arising from the projection of the field y (at line 22, column 38)\n",
         "Not a record type: string\n"
         "arising from an assignment of the field y (at line 21, column 42)\n",
         "Not a record type: string\n"
         "arising from an assignment of the field y (at line 20, column 38)\n",
         "Not a record type: string\n"
         "arising from an assignment of the field y (at line 19, column 35)\n",
         "Ambiguous record type with field y (at line 13, column 25) could be one of\n"
         "  - r (at line 4, column 10)\n"
         "  - r' (at line 5, column 10)\n",
         "Record type r2 does not have field y (at line 15, column 22)\n",
         "Repeated name x in pattern\n"
         "  x :: x (at line 26, column 7)\n",
         "No record type with fields y, z (at line 14, column 22)\n"]}
    ].
