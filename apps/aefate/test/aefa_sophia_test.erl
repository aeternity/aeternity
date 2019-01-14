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
     "  function id    (x : int) = x\n"
     "  function inc   (x : int) = x + 1\n"
     "  function inc'  (x : int) = 1 + x\n"
     "  function plus2 (x : int) = x + 2\n"
     "  function plus4 (x : int) = x + 2 + 2\n"
     "  function plus4'(x : int) = x + (2 + 2)\n"
    }.

id_tests() ->
    [ {"id",    [142], 142}
    , {"inc",   [142], 143}
    , {"plus2", [142], 144}
    , {"plus4", [142], 146}
    ].

id_test(Chain, Fun, Args, Res) ->
    expect(Chain, <<"identity">>, list_to_binary(Fun), Args, Res).

id_test_() ->
    Chain = compile_contracts([identity()]),
    [{lists:flatten(io_lib:format("~s(~p) -> ~p", [Fun, Arg, Res])),
      fun() -> id_test(Chain, Fun, Arg, Res) end}
    || {Fun, Arg, Res} <- id_tests() ].

run_eunit(Test) ->
    [ begin io:format("~s\n", [Name]), Fun() end || {Name, Fun} <- ?MODULE:Test() ],
    ok.
