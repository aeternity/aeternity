%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utilities for the Ring language tests.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aer_test_utils).

-export([read_contract/1, contract_path/0, run_contract/4, pp/1, pp/2]).

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

pp(Name) -> pp(Name, []).

pp(Name, Options) ->
    case aer_parser:string(read_contract(Name)) of
        {ok, AST} ->
            [ io:format("~s\n", [prettypr:format(aer_pretty:decls(AST))]) || not lists:member(quiet, Options) ];
        {error, {{L, C}, parse_error, Err}} ->
            io:format("Parse error at ~p:~p:~p\n~s\n", [Name, L, C, Err])
    end.

dummy_state(Code,Data) ->
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

run_contract(Name, Fun, Args, Type) ->
  Code = aer_compiler:file(Name, []), %%[pp_ast,pp_typed,pp_icode]),
%%  io:format("\nCompiled code:\n"),
%%  io:format("~p\n\n",[Code]),
%%  ok = aeb_disassemble:pp(Code),
  %% Load the call
  Call = list_to_tuple([list_to_binary(atom_to_list(Fun))|Args]),
  {0,Data} = aer_data:to_binary(Call),
  io:format("Running:\n"),
  State = aevm_eeevm:eval(dummy_state(Code, Data)),
%%  io:format("\nFinal state:\n~p\n",[State]),
  io:format("\nFinal stack: ~p\n",[maps:get(stack,State)++[end_of_stack]]),
  io:format("\nReturn value: ~p\n",[aer_data:from_binary(Type,maps:get(out,State))]),
%%    io:format("\nReturn value: ~p\n",[aer_data:binary_to_words(maps:get(out,State))]),
  ok.

%% Stack simulator

simulate([],Stack) ->
    Stack;
simulate(['PUSH1',X|More],S) ->
    simulate(More,[X|S]);
simulate([Op|More],Stack) ->
    simulate(More,simulate(Op,Stack));
simulate('MSIZE',S) ->
    A = new_atom(),
    io:format("~p = MSIZE\n",[A]),
    [A|S];
simulate('DUP2',[A,B|S]) ->
    [B,A,B|S];
simulate('DUP3',[A,B,C|S]) ->
    [C,A,B,C|S];
simulate('ADD',[A,B|S]) ->
    [add(A,B)|S];
simulate('MSTORE',[Addr,X|S]) ->
    io:format("mem(~p) <- ~p\n",[Addr,X]),
    S;
simulate('MLOAD',[Addr|S]) ->
    A = new_atom(),
    io:format("~p = mem(~p)\n",[A,Addr]),
    [A|S];
simulate('SWAP1',[A,B|S]) ->
    [B,A|S];
simulate('SWAP2',[A,B,C|S]) ->
    [C,B,A|S];
simulate('SUB',[A,B|S]) ->
    [{A,'-',B}|S];
simulate('POP',[_|S]) ->
    S.

add(0,X) ->
    X;
add(X,0) ->
    X;
add(X,{A,'-',X}) ->
    A;
add(X,{A,'+',B}) ->
    {A,'+',add(X,B)};
add(A,B) ->
    {A,'+',B}.

new_atom() ->
    catch ets:new(names,[set,public,named_table]),
    case ets:lookup(names,index) of
	[] -> I = 0;
	[{index,I}] -> ok
    end,
    ets:insert(names,{index,I+1}),
    list_to_atom([$a+I]).

