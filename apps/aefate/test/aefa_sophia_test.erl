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
    {ok, #{ accumulator := Result,
            trace       := Trace }} = run(Chain, Contract, Function, Arguments),
    ?assertMatch({Expect, _}, {Result, Trace}).

%% For now, implement pipeline here.
compile_contract(Code) ->
    compile_contract(Code, [{debug, [scode, opt, opt_rules, compile]}]).

compile_contract(Code, Options) ->
    {ok, Ast} = aeso_parser:string(Code),
    TypedAst  = aeso_ast_infer_types:infer(Ast, Options),
    FCode     = aeso_ast_to_fcode:ast_to_fcode(TypedAst, Options),
    aeso_fcode_to_fate:compile(FCode, Options).

setup_chain(Contracts) ->
    #{ contracts => Contracts }.

make_call(Contract, Function, Arguments) ->
    EncArgs  = list_to_tuple([aefate_test_utils:encode(A) || A <- Arguments]),
    Calldata = {tuple, {Function, {tuple, EncArgs}}},
    #{ contract => Contract,
       call => aeb_fate_encoding:serialize(Calldata) }.

mk_test(Contracts, Tests) ->
    Main  = element(1, hd(Contracts)),
    Chain = compile_contracts(Contracts),
    Pr    = fun(X) -> io_lib:format("~p", [X]) end,
    [{lists:flatten(io_lib:format("~s(~s) -> ~p", [Fun, string:join(lists:map(Pr, Args), ", "), Res])),
      fun() -> expect(Chain, Main, list_to_binary(Fun), Args, Res) end}
    || {Fun, Args, Res} <- Tests ].

run_eunit(Test) ->
    [ begin io:format("~s\n", [Name]), Fun() end || {Name, Fun} <- ?MODULE:Test() ],
    ok.

%% -- Actual tests --

arithmetic() ->
    {<<"arithmetic">>,
     "contract Arith =\n"
     "  function id    (x : int) = x\n"
     "  function inc   (x : int) = x + 1\n"
     "  function inc'  (x : int) = 1 + x\n"
     "  function plus2 (x : int) = x + 2\n"
     "  function plus4 (x : int) = x + 2 + 2\n"
     "  function plus4'(x : int) = x + (2 + 2)\n"
     "  function dec   (x : int) = x - 1\n"
     "  function sub2  (x : int) = x - 2\n"
     "  function eq0   (x : int) = x == 0\n"
     "  function eq3   (x : int) = x == 3\n"
     "  function pred  (x : int) = if (x == 0) 0 else x - 1\n"
     "  function iadd  (x, y)    = (x + y) + 3\n"
     "  function nest  (x : int, y : int) =\n"
     "    if   (x == 0) 0\n"
     "    elif (y == 0) x + 1\n"
     "    else x + y\n"
     "  function local (x : int) =\n"
     "    let y = x + 1\n"
     "    y + y\n"
    }.

arith_tests() ->
    Nest = fun(0, _) -> 0; (X, 0) -> X + 1; (X, Y) -> X + Y end,
    [ {"id",     [142],  142}
    , {"inc",    [142],  143}
    , {"inc'",   [142],  143}
    , {"plus2",  [142],  144}
    , {"plus4",  [142],  146}
    , {"plus4'", [142],  146}
    , {"dec",    [0],     -1}
    , {"dec",    [14],    13}
    , {"sub2",   [20],    18}
    , {"local",  [20],    42} ] ++
    [ {"eq0",  [X], X == 0} || X <- [0, 99] ] ++
    [ {"eq3",  [X], X == 3} || X <- [3, -100] ] ++
    [ {"pred", [X], max(0, X - 1)} || X <- [0, 100] ] ++
    [ {"nest", [X, Y], Nest(X, Y)} || X <- [0, 10], Y <- [0, -99] ] ++
    [].

arith_test_() -> mk_test([arithmetic()], arith_tests()).

tuples() ->
    {<<"tuples">>,
     "contract Tuples =\n"
     "  function fst(p : (int, string)) =\n"
     "    switch(p)\n"
     "      (x, y) => x\n"
     "  function fst'(p : (int, string)) =\n"
     "    switch(p)\n"
     "      (x, _) => x\n"
     "  function snd(p : (int, string)) =\n"
     "    switch(p)\n"
     "      (x, y) => y\n"
     "  function snd'(p : (int, string)) =\n"
     "    switch(p)\n"
     "      (_, y) => y\n"
     "  function sum(p) =\n"
     "    switch(p)\n"
     "      (x, y) => x + y\n"
     "  function swap(p : (int, string)) =\n"
     "    switch(p)\n"
     "      (x, y) => (y, x)\n"
     "  function id(p : (int, int, string)) =\n"
     "    switch(p)\n"
     "      (x, y, z) => (x, y, z)\n"
     "  function nest(p : ((int, int), string)) =\n"
     "    switch(p)\n"
     "      (xy, z) => switch(xy) (x, y) => (x, y, z)\n"
    "  function deep(p : ((int, int), (int, int))) =\n"
    "    switch(p)\n"
    "      ((x, y), (z, w)) => (x, y, z, w)\n"
    "  function deep_sum(p : ((int, int), (int, int))) =\n"
    "    switch(p)\n"
    "      ((x, y), (z, w)) => x + y + z + w\n"
    }.

tuple_tests() ->
    A    = 42,
    B    = 199,
    S    = <<"forty-two">>,
    lists:flatten(
    [ [],
      [{Fst, [{A, S}], A} || Fst <- ["fst", "fst"]],
      [{Snd, [{A, S}], S} || Snd <- ["snd", "snd'"]],
      [{"sum",  [{A, B}], A + B}],
      [{"swap", [{A, S}], {tuple, {S, A}}}],
      [{"id",   [{A, B, S}],   {tuple, {A, B, S}}}],
      [{"nest", [{{A, B}, S}], {tuple, {A, B, S}}}],
      [{"deep", [{{A, B}, {A + 1, B + 1}}], {tuple, {A, B, A + 1, B + 1}}}],
      []
    ]).

tuple_test_() -> mk_test([tuples()], tuple_tests()).

patterns() ->
    {<<"patterns">>,
     "contract PatternMatching =\n"
     "  function or(p : (bool, bool)) =\n"
     "    switch(p)\n"
     "      (false, y) => y\n"
     "      (true,  _) => true\n"
     "  function and'(p : (bool, bool)) =\n"
     "    switch(p)\n"
     "      (x, false) => false\n"
     "      (x, true)  => x\n"
    "  function tuple_catchall(p : (bool, bool)) =\n"
    "    switch(p)\n"
    "      (true, y) => y\n"
    "      _         => false\n"
    "  function complex_match(p : (bool, bool, bool)) =\n"
    "    switch(p)\n"
    "      (x1,    false, z1)   => (1, x1,    false, z1)\n"
    "      (false, y2,    true) => (2, false, y2,    true)\n"
    "      (true,  true,  z3)   => (3, true,  true,  z3)\n"
    "      (x4,    y4,    z4)   => (4, x4,    y4,    z4)\n"
    "  function lit_match(p : (int, bool)) =\n"
    "    switch(p)\n"
    "      (7, y) => y\n"
    "      _         => false\n"
    "  function even(n : int) =\n"
    "    switch(n)\n"
    "      0 => true\n"
    "      1 => false\n"
    "      2 => true\n"
    "      3 => false\n"
    "      4 => true\n"
    "      5 => false\n"
    "      _ => true\n"
    ""}.

values({tuple, []}) -> [{}];
values({tuple, [T | Ts]}) ->
    [ list_to_tuple([V | tuple_to_list(Vs)])
      || V  <- values(T),
         Vs <- values({tuple, Ts}) ];
values(bool) -> [false, true];
values(int)  -> [0, 7].

pattern_tests() ->
    Boolx2 = {tuple, [bool, bool]},
    Boolx3 = {tuple, [bool, bool, bool]},
    Funs = [{"or", Boolx2, fun({A, B}) -> A or B end},
             {"and'", Boolx2, fun({A, B}) -> A and B end},
             {"tuple_catchall", Boolx2, fun({A, B}) -> A and B end},
             {"lit_match", {tuple, [int, bool]},
              fun({7, Y}) -> Y; (_) -> false end},
             {"complex_match", Boolx3, fun({X, false, Z})    -> {tuple, {1, X, false, Z}};
                                          ({false, Y, true}) -> {tuple, {2, false, Y, true}};
                                          ({true, true, Z})  -> {tuple, {3, true, true, Z}};
                                          ({X, Y, Z})        -> {tuple, {4, X, Y, Z}} end}],
    lists:flatten(
      [ [],
        [{Name, [X], Fun(X)} || {Name, T, Fun} <- Funs, X <- values(T)],
        [{"even", [N], not lists:member(N, [1, 3, 5])} || N <- lists:seq(-1, 7)],
        []
      ]).

pattern_test_() -> mk_test([patterns()], pattern_tests()).

records() ->
    {<<"records">>,
     "contract Records =\n"
     "  type number = int\n"
     "  record r1 = {x : bool, z : bool, w : int}\n"
     "  record r2 = {x : number, y : r1}\n"
     "  function rec_match(a : r2) : (int, r1) =\n"
     "    switch(a)\n"
     "      {x = 4}          => (1, a.y)\n"
     "      {y = {x = true}} => (2, {x = false, z = true, w = 0})\n"
     "      {x = x, y = r}   => (3, r { z = x == 0, w = x })\n"
     "  function rec_modify(a : r1) = a { w @ n = n + 1 }\n"
     ""}.

record_tests() ->
    RecMatch = fun({4, {X, Z, W}})    -> {tuple, {1, {tuple, {X, Z, W}}}};
                  ({_, {true, _, _}}) -> {tuple, {2, {tuple, {false, true, 0}}}};
                  ({X, {X1, _, _}})   -> {tuple, {3, {tuple, {X1, X == 0, X}}}}
               end,
    RecMatchTests = [{4, {true,  false, 10}},
                     {3, {true,  true,  20}},
                     {2, {false, false, 30}}],

    lists:flatten(
      [[],
       [{"rec_match", [Input], RecMatch(Input)}
        || Input <- RecMatchTests],
       []
      ]).

record_test_() -> mk_test([records()], record_tests()).

