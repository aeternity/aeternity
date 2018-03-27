%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utilities for the Sophia language tests.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeso_test_utils).

-export([read_contract/1, contract_path/0, run_contract/4, pp/1, pp/2]).

-export([spend/3, get_balance/1, call_contract/6]).

contract_path() ->
    {ok, Cwd} = file:get_cwd(),
    N   = length(filename:split(Cwd)),
    Rel = ["..", "apps", "aesophia", "test", "contracts"],
    %% Try the first matching directory (../)*Rel
    Cand = fun(I) -> filename:join(lists:duplicate(I, "..") ++ Rel) end,
    case [ Dir || Dir <- lists:map(Cand, lists:seq(0, N)), filelib:is_dir(Dir) ] of
        [Dir | _] -> Dir;
        []        -> error(failed_to_find_contract_dir)
    end.

%% Read a contract file from the test/contracts directory.
-spec read_contract(string() | atom()) -> string().
read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aes"]))),
    binary_to_list(Bin).

pp(Name) -> pp(Name, []).

pp(Name, Options) ->
    case aeso_parser:string(read_contract(Name)) of
        {ok, AST} ->
            [ io:format("~s\n", [prettypr:format(aeso_pretty:decls(AST))]) || not lists:member(quiet, Options) ];
        {error, {{L, C}, parse_error, Err}} ->
            io:format("Parse error at ~p:~p:~p\n~s\n", [Name, L, C, Err])
    end.

dummy_state(Code, Data) ->
  aevm_eeevm_state:init(
    #{ env =>
        #{ currentCoinbase   => 0,
           currentDifficulty => 0,
           currentGasLimit   => 1000000,
           currentNumber     => 7,
           currentTimestamp  => 0,
           chainAPI          => ?MODULE,
           chainState        => no_state
         }
     , exec =>
        #{ gas        => 10000,
           code       => Code,
           address    => 16#add7e55,
           value      => 100,
           caller     => 0,
           origin     => 0,
           call_stack => [],
           gasPrice   => 1,
           data       => Data
         }
     , pre => #{}
     }, #{ trace => true }).

run_contract(Name, Fun, Args, Type) ->
  Code = aeso_compiler:from_string(read_contract(Name), [pp_ring_code, pp_typed, pp_icode]),
%%  io:format("\nCompiled code:\n"),
%%  io:format("~p\n\n",[Code]),
  ok = aeb_disassemble:pp(Code),
  %% Load the call
  Call = list_to_tuple([list_to_binary(atom_to_list(Fun))|Args]),
  {0,Data} = aeso_data:to_binary(Call),
  io:format("Running:\n"),
  {ok, State} = aevm_eeevm:eval(dummy_state(Code, Data)),
%%  io:format("\nFinal state:\n~p\n",[State]),
  io:format("\nFinal stack: ~p\n",[maps:get(stack,State)++[end_of_stack]]),
  io:format("\nReturn value: ~p\n",[aeso_data:from_binary(Type,maps:get(out,State))]),
%%    io:format("\nReturn value: ~p\n",[aeso_data:binary_to_words(maps:get(out,State))]),
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

%% -- Chain API for test -----------------------------------------------------

spend(Recipient, Amount, S) ->
    io:format("+++ SPEND(~p, ~p)\n", [Recipient, Amount]),
    {ok, S}.

get_balance(_) -> 1000000.

call_contract(Contract, Gas, Value, CallData, CallStack, S) ->
    io:format("+++ CALL(~p, ~p, ~p, ~p, ~p)\n", [Contract, Gas, Value, CallData, CallStack]),
    {ok, <<42:256>>, S}.

