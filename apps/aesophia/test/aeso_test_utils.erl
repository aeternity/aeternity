%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utilities for the Sophia language tests.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeso_test_utils).

-include("apps/aecontract/src/aecontract.hrl").

-export([read_contract/1, contract_path/0, run_contract/4, pp/1, pp/2, dump_words/1, compile/1]).

-export([spend/3, get_balance/2, call_contract/6, get_store/1, set_store/2]).

contract_path() ->
    {ok, Cwd} = file:get_cwd(),
    N   = length(filename:split(Cwd)),
    Rel = ["apps", "aesophia", "test", "contracts"],
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
           chainState        => no_state,
           vm_version        => ?AEVM_01_Sophia_01
         }
     , exec =>
        #{ gas        => 1000000,
           code       => Code,
           address    => 91919191,
           value      => 100,
           caller     => 47474747,
           origin     => 0,
           call_stack => [],
           gasPrice   => 1,
           data       => Data
         }
     , pre => #{}
     }, #{ trace => true }).

compile(Name) ->
  aeso_compiler:from_string(read_contract(Name), [pp_ring_code, pp_typed, pp_icode]).

run_contract(Name, Fun, Args, Type) ->
  Code = compile(Name),
%%  io:format("\nCompiled code:\n"),
%%  io:format("~p\n\n",[Code]),
  %% ok = aeb_disassemble:pp(Code),
  %% Load the call
  Call = list_to_tuple([list_to_binary(atom_to_list(Fun))|Args]),
  Data = aeso_data:to_binary(Call),
  io:format("Running:\n"),
  {ok, State} = aevm_eeevm:eval(dummy_state(Code, Data)),
%%  io:format("\nFinal state:\n~p\n",[State]),
  io:format("\nFinal stack: ~p\n",[maps:get(stack,State)++[end_of_stack]]),
  io:format("\nReturn data: ~p\n",[dump_words(maps:get(out,State))]),
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

%% Translate a blob of 256-bit words into readable form. Does a bit of guessing
%% to recover strings. TODO: strings longer than 32 bytes
dump_words(Bin) -> dump_words(Bin, []).

dump_words(<<N:256, W:32/binary, Rest/binary>>, Acc) when N < 32 ->
    NotN = (32 - N) * 8,
    case W of
        <<S:N/binary, 0:NotN>> ->
            Str = binary_to_list(S),
            case lists:member(0, Str) of
                true  -> dump_words(<<W/binary, Rest/binary>>, [N | Acc]);   %% Not a string
                false -> dump_words(Rest, [binary_to_list(S), N | Acc])
            end;
        _ -> dump_words(<<W/binary, Rest/binary>>, [N | Acc])
    end;
dump_words(<<N:256/signed, Rest/binary>>, Acc) ->
    dump_words(Rest, [N | Acc]);
dump_words(<<>>, Acc) -> lists:reverse(Acc);
dump_words(Rest, Acc) -> lists:reverse([{error, Rest} | Acc]).

%% -- Chain API for test -----------------------------------------------------

spend(Recipient, Amount, S) ->
    io:format("+++ SPEND(~p, ~p)\n", [Recipient, Amount]),
    {ok, S}.

get_balance(_, _) -> 1000000.

call_contract(Contract, Gas, Value, CallData, CallStack, S) ->
    io:format("+++ CALL(~p, ~p, ~p, ~p, ~p)\n", [Contract, Gas, Value, CallData, CallStack]),
    {ok, <<42:256>>, S}.

get_store(_) -> #{}.
set_store(_, _) -> ok.
