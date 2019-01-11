%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Basic tests Sophia-to-Fate pipeline
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_sophia_test).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

%% -- Compiling and running --

compile_and_run(Contracts, Contract, Function, Arguments) ->
    Chain = compile_contracts(Contracts),
    run(Chain, Contract, Function, Arguments).

compile_contracts(Contracts) ->
    setup_chain(maps:from_list([ {Name, compile_contract(Code)} || {Name, Code} <- Contracts ])).

run(Chain, Contract, Function, Arguments) ->
    aefa_fate:run(make_call(Contract, Function, Arguments), Chain).

expect(Chain, Contract, Function, Arguments, Expect) ->
    #{ accumulator := Result,
       trace       := Trace } = run(Chain, Contract, Function, Arguments),
    ?assertMatch({Expect, _}, {Result, Trace}).

%% For now, implement pipeline here.
compile_contract(Code) ->
    compile_contract(Code, []).

compile_contract(Code, Options) ->
    {ok, Ast} = aeso_parser:string(Code),
    TypedAst  = aeso_ast_infer_types:infer(Ast, Options),
    ICode     = aeso_ast_to_icode:convert_typed(TypedAst, Options),
    aeso_icode_to_fate:compile(ICode, Options).

setup_chain(Contracts) ->
    #{ contracts => Contracts }.

make_call(Contract, Function, Arguments) ->
    #{ contract => Contract,
       function => Function,
       arguments => Arguments }.

%% -- Actual tests --

identity() ->
    {<<"identity">>,
     "contract Id =\n"
     "  function id(x : int) = x\n"
     "  function idS(s : string) = s\n"
    }.

id_test(Fun, Args, Res) ->
    Chain = compile_contracts([identity()]),
    expect(Chain, <<"identity">>, list_to_binary(Fun), Args, Res).

id_test_() ->
    [{lists:flatten(io_lib:format("~s(~p) -> ~p", [Fun, Arg, Res])),
      fun() -> id_test(Fun, Arg, Res) end}
    || {Fun, Arg, Res} <-
        [ {"id", [42], 42}
        , {"idS", [<<"foo">>], <<"foo">>}
        ] ].

