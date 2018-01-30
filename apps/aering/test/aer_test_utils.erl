%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utilities for the Ring language tests.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aer_test_utils).

-export([read_contract/1, contract_path/0, run_contract/3]).

contract_path() ->
    {ok, Cwd} = file:get_cwd(),
    N   = length(filename:split(Cwd)),
    Rel = ["apps", "aering", "test", "contracts"],
    %% Try the first matching directory (../)*Rel
    Cand = fun(I) -> filename:join(lists:duplicate(I, "..") ++ Rel) end,
    case [ Dir || Dir <- lists:map(Cand, lists:seq(0, N)), filelib:is_dir(Dir) ] of
        [Dir | _] -> Dir;
        []        -> error(failed_to_find_contract_dir)
    end.

%% Read a contract file from the test/contracts directory.
-spec read_contract(string() | atom()) -> string().
read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aer"]))),
    binary_to_list(Bin).

dummy_state(Code, Data) ->
  #{ gas        => 10000,
     code       => Code,
     cp         => 0,
     memory     => #{},
     stack      => [],
     do_trace   => false,    %% set to true for step-by-step tracing
     trace      => [],
     trace_fun  => fun io:format/2,
     data       => Data
  }.

run_contract(Name, Fun, Args) ->
    Code = aer_compiler:file(Name, [pp_ast, pp_icode]),
    io:format("\nCompiled code:\n"),
    io:format("~p\n\n", [Code]),
    ok = aeb_disassemble:pp(Code),
    %% Load the call
    Call = list_to_tuple([list_to_binary(atom_to_list(Fun))|Args]),
    {0, Data} = aer_data:to_binary(Call),
    io:format("Running:\n"),
    State = aevm_eeevm:eval(dummy_state(Code, Data)),
    %%io:format("\nFinal state:\n~p\n", [State]),
    io:format("\nFinal stack: ~p\n", [maps:get(stack, State)++[end_of_stack]]),
    io:format("\nReturn value: ~p\n",[aer_data:binary_to_words(maps:get(out,State))]),
    ok.
