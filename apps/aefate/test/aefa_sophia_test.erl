%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Basic tests Sophia-to-Fate pipeline
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_sophia_test).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").

%% -- Compiling and running --

compile_and_run(Contracts, Contract, Function, Arguments) ->
    Chain = compile_contracts(Contracts),
    run(Chain, Contract, Function, Arguments).

compile_contracts(Contracts) ->
    compile_contracts(Contracts, default_options()).

compile_contracts(Contracts, Options) ->
    maps:from_list([ {pad_contract_name(Name), {compile_contract(Code, Options), ?VM_FATE_SOPHIA_2}}
                     || {Name, Code} <- Contracts ]).

make_contract(Name) -> aeb_fate_data:make_contract(pad_contract_name(Name)).

dummy_spec(Cache, Stores) ->
    Caller = <<123:256>>,
    #{ trees     => dummy_trees(Caller, Cache, Stores),
       caller    => Caller,
       origin    => Caller,
       gas_price => 1,
       fee       => 621,
       tx_env    => aetx_env:tx_env(1) }.

dummy_trees(Caller, Cache, Stores) ->
    %% All contracts and the caller must have accounts
    Trees = aec_trees:new_without_backend(),
    Pubkeys = [Caller| [X || X <- maps:keys(Cache)]],
    ATrees = lists:foldl(fun(Pubkey, Acc) ->
                                 Account = aec_accounts:new(Pubkey, 10000),
                                 aec_accounts_trees:enter(Account, Acc)
                         end, aec_trees:accounts(Trees), Pubkeys),
    CTrees = lists:foldl(fun(Pubkey, Acc) ->
                                 Contract0 = aect_contracts:new(Pubkey, 1, #{vm => 5, abi => 3}, <<>>, 0),
                                 Contract1 = aect_contracts:set_pubkey(Pubkey, Contract0),
                                 Contract2 = case maps:get(Pubkey, Stores, none) of
                                                none  -> Contract1;
                                                Store -> aect_contracts:set_state(Store, Contract1)
                                             end,
                                 aect_state_tree:insert_contract(Contract2, Acc)
                         end, aec_trees:contracts(Trees), Pubkeys),
    aec_trees:set_contracts(aec_trees:set_accounts(Trees, ATrees), CTrees).

run(Cache, Contract, Function, Arguments) ->
    {_, Res} = timed_run(Cache, Contract, Function, Arguments),
    Res.

run(Cache, Contract, Function, Arguments, Store) ->
    {_, Res} = timed_run(Cache, Contract, Function, Arguments, Store),
    Res.

timed_run(Cache, Contract, Function, Arguments) ->
    timed_run(Cache, Contract, Function, Arguments, #{}).

timed_run(Cache, Contract, Function, Arguments, Store0) ->
    Spec = #{ store := Store } = make_call_spec(Contract, Function, Arguments, Store0),
    Env = dummy_spec(Cache, #{pad_contract_name(Contract) => Store}),
    try
        timer:tc(fun() -> aefa_fate:run_with_cache(Spec, Env, Cache) end)
    catch _:{error, Err} ->
              {0, {error, Err, []}}
    end.

expect(Chain, Contract, Function, Arguments, Expect) ->
    case run(Chain, Contract, Function, Arguments) of
        {ok, ES} ->
            Result = aefa_engine_state:accumulator(ES),
            Trace  = aefa_engine_state:trace(ES),
            [ io:format("~p\n", [I]) || Result /= Expect, {I, _} <- Trace ],
            ?assertMatch(Expect, Result);
        {Kind, Err, _} when Kind == error; Kind == revert ->
            ?assertMatch(Expect, {Kind, Err})
    end.

%% For now, implement pipeline here.

default_options() ->
    [{debug, [scode, opt, opt_rules, compile]}, pp_fcode].

compile_contract(Code) ->
    compile_contract(Code, default_options()).

compile_contract(Code, Options) ->
    try
        Ast       = aeso_parser:string(Code, Options),
        {_, TypedAst}  = aeso_ast_infer_types:infer(Ast, Options),
        {#{child_con_env := ChildContracts}, FCode}
                       = aeso_ast_to_fcode:ast_to_fcode(TypedAst, Options),
        Fate      = aeso_fcode_to_fate:compile(ChildContracts, FCode, Options),
        case aeb_fate_code:deserialize(aeb_fate_code:serialize(Fate)) of
            Fate  -> Fate;
            Other -> {error, {Other, '/=', Fate}}
        end
    catch _:{errors, Err} ->
        io:format("~s\n", [Err]),
        {error, {type_errors, Err}}
    end.

-define(CALL_GAS, 6000000).
-define(CALL_FEE, 5000000).

make_store(<<"init">>, _) -> aefa_stores:initial_contract_store();
make_store(_, none) ->
    case get(contract_store) of
        undefined -> aefa_stores:initial_contract_store();
        Store     -> Store
    end;
make_store(_, Store) ->
    maps:fold(fun(Reg, Val, S) ->
                Key = <<0, (binary:encode_unsigned(Reg))/binary>>,
                ValBin = aeb_fate_encoding:serialize(Val),
                aect_contracts_store:put(Key, ValBin, S)
              end, aefa_stores:initial_contract_store(), Store).

make_call_spec(Contract, Function0, Arguments, Store) ->
    Function = aeb_fate_code:symbol_identifier(Function0),
    EncArgs  = list_to_tuple([aefa_test_utils:encode(A) || A <- Arguments]),
    Calldata = {tuple, {Function, {tuple, EncArgs}}},
    CtStore  = make_store(Function0, Store),
    #{ contract   => pad_contract_name(Contract),
       gas        => ?CALL_GAS,
       fee        => ?CALL_FEE,
       value      => 0,
       call       => aeb_fate_encoding:serialize(Calldata),
       store      => CtStore,
       vm_version => ?VM_FATE_SOPHIA_2,
       allow_init => true
     }.

pad_contract_name(Name) ->
    PadSize = 32 - byte_size(Name),
    iolist_to_binary([Name, lists:duplicate(PadSize, "_")]).

mk_test(Contracts, Tests) ->
    Main  = element(1, hd(Contracts)),
    Chain = compile_contracts(Contracts),
    Pr    = fun(X) -> io_lib:format("~p", [X]) end,
    [{lists:flatten(io_lib:format("~s(~s) -> ~p", [Fun, string:join(lists:map(Pr, Args), ", "), Res])),
      fun() -> expect(Chain, Main, list_to_binary(Fun), Args, Res) end}
    || {Fun, Args, Res} <- Tests ].

print_run_stats(Time, ES) ->
    GasUsed    = ?CALL_GAS - aefa_engine_state:gas(ES),
    Trace      = aefa_engine_state:trace(ES),
    Red        = fun({_, {reductions, R}}) -> R end,
    Reductions = Red(hd(Trace ++ [{bla, {reductions, 0}}])) - Red(lists:last([{bla, {reductions, 0}} | Trace])),
    Steps      = length(Trace),
    io:format("~p steps / ~p gas / ~p reductions / ~.2fms\n", [Steps, GasUsed, Reductions, Time / 1000]).

print_logs(_, []) -> ok;
print_logs(EventMap, Logs) ->
    io:format("Events:\n"),
    print_logs(EventMap, none, Logs).

print_logs(_, _, []) -> ok;
print_logs(EventMap, Ct, [{Ct, [Hash | Ixs], Payload} | Logs]) ->
    PayloadStr =
        case Payload of
            <<>> -> "";
            _    -> io_lib:format(", ~s", [Payload])
        end,
    io:format("    ~s(~s~s)\n",
        [maps:get(Hash, EventMap, Hash),
         string:join([integer_to_list(N) || <<N:256>> <- Ixs], ", "),
         PayloadStr]),
    print_logs(EventMap, Ct, Logs);
print_logs(EventMap, _, Logs = [{Ct, _, _} | _]) ->
    io:format("  ~p\n", [Ct]),
    print_logs(EventMap, Ct, Logs).

store_from_trees(Pubkey, Trees) ->
    CtTrees      = aec_trees:contracts(Trees),
    Contract     = aect_state_tree:get_contract(Pubkey, CtTrees, [full_store_cache]),
    aect_contracts_store:contents(aect_contracts:state(Contract)).

read_store(Pubkey, ES) ->
    try
        Trees        = aefa_fate:final_trees(ES),
        CtTrees      = aec_trees:contracts(Trees),
        Contract     = aect_state_tree:get_contract(Pubkey, CtTrees, [full_store_cache]),
        CtStore      = aect_contracts:state(Contract),
        Store        = aefa_stores:put_contract_store(Pubkey, CtStore, aefa_stores:new()),
        ES1          = aefa_engine_state:set_stores(Store, ES),
        Keys         = [ binary:decode_unsigned(Reg)
                         || <<0, Reg/binary>> <- maps:keys(aect_contracts_store:contents(CtStore)) ],
        Value = fun(Key) ->
                    {ok, Val, _} = aefa_stores:find_value(Pubkey, Key, Store),
                    {Val1, _}    = aefa_fate:unfold_store_maps(Val, ES1),
                    Val1
                end,
        {maps:from_list([ {Key, Value(Key)} || Key <- Keys, Key > 0 ]), CtStore}
    catch K:Err ->
        io:format("~p:~p\n  ~p\n", [K, Err, erlang:get_stacktrace()]),
        {error, none}
    end.

run_file(File, Fun, Args) ->
    run_file(File, Fun, Args, []).

run_file(File, Fun, Args, Options) ->
    {ok, Code} = file:read_file(File),
    run_call(binary_to_list(Code), Fun, Args, Options).

run_call(Code, Fun, Args) ->
    run_call(Code, Fun, Args, []).

run_call(Code, Fun, Args, Options) ->
    Contract = pad_contract_name(<<"test">>),
    Store = proplists:get_value(store, Options, none),
    Cache = compile_contracts([{Contract, Code}], Options),
    EventMap = maps:from_list(
                 [{element(2, eblake2:blake2b(32, list_to_binary(Con))), Con}
                  || {con, _, Con} <- element(2, aeso_scan:scan(Code))]),
    case timed_run(Cache, Contract, list_to_binary(Fun), Args, Store) of
        {Time, {ok, ES}} ->
            print_run_stats(Time, ES),
            Logs = aefa_engine_state:logs(ES),
            {Store1, CtStore} = read_store(Contract, ES),
            put(contract_store, CtStore),
            io:format("Store:\n  ~p\n", [Store1]),
            print_logs(EventMap, Logs),
            aefa_engine_state:accumulator(ES);
        {Time, {error, <<"Out of gas">>, ES}} ->
            print_run_stats(Time, ES),
            {error, out_of_gas};
        {Time, {error, Err, ES}} ->
            print_run_stats(Time, ES),
            io:format("~s\n", [Err]),
            {error, Err, [I || {I, _} <- aefa_engine_state:trace(ES)]}
    end.

run_eunit(Test) ->
    [ begin io:format("~s\n", [Name]), Fun() end || {Name, Fun} <- ?MODULE:Test() ],
    ok.

%% -- Actual tests --

arithmetic() ->
    {<<"arithmetic">>,
     "contract Arith =\n"
     "  entrypoint id    (x : int) = x\n"
     "  entrypoint inc   (x : int) = x + 1\n"
     "  entrypoint inc'  (x : int) = 1 + x\n"
     "  entrypoint plus2 (x : int) = x + 2\n"
     "  entrypoint plus4 (x : int) = x + 2 + 2\n"
     "  entrypoint plus4'(x : int) = x + (2 + 2)\n"
     "  entrypoint dec   (x : int) = x - 1\n"
     "  entrypoint sub2  (x : int) = x - 2\n"
     "  entrypoint eq0   (x : int) = x == 0\n"
     "  entrypoint eq3   (x : int) = x == 3\n"
     "  entrypoint pred  (x : int) = if (x == 0) 0 else x - 1\n"
     "  entrypoint iadd  (x, y)    = (x + y) + 3\n"
     "  entrypoint nest  (x : int, y : int) =\n"
     "    if   (x == 0) 0\n"
     "    elif (y == 0) x + 1\n"
     "    else x + y\n"
     "  entrypoint local (x : int) =\n"
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
     "  entrypoint fst(p : int * string) =\n"
     "    switch(p)\n"
     "      (x, y) => x\n"
     "  entrypoint fst'(p : int * string) =\n"
     "    switch(p)\n"
     "      (x, _) => x\n"
     "  entrypoint snd(p : int * string) =\n"
     "    switch(p)\n"
     "      (x, y) => y\n"
     "  entrypoint snd'(p : int * string) =\n"
     "    switch(p)\n"
     "      (_, y) => y\n"
     "  entrypoint sum(p) =\n"
     "    switch(p)\n"
     "      (x, y) => x + y\n"
     "  entrypoint swap(p : int * string) =\n"
     "    switch(p)\n"
     "      (x, y) => (y, x)\n"
     "  entrypoint id(p : int * int * string) =\n"
     "    switch(p)\n"
     "      (x, y, z) => (x, y, z)\n"
     "  entrypoint nest(p : (int * int) * string) =\n"
     "    switch(p)\n"
     "      (xy, z) => switch(xy) (x, y) => (x, y, z)\n"
    "  entrypoint deep(p : (int * int) * (int * int)) =\n"
    "    switch(p)\n"
    "      ((x, y), (z, w)) => (x, y, z, w)\n"
    "  entrypoint deep_sum(p : (int * int) * (int * int)) =\n"
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
     "  entrypoint or(p : bool * bool) =\n"
     "    switch(p)\n"
     "      (false, y) => y\n"
     "      (true,  _) => true\n"
     "  entrypoint and'(p : bool * bool) =\n"
     "    switch(p)\n"
     "      (x, false) => false\n"
     "      (x, true)  => x\n"
    "  entrypoint tuple_catchall(p : bool * bool) =\n"
    "    switch(p)\n"
    "      (true, y) => y\n"
    "      _         => false\n"
    "  entrypoint complex_match(p : bool * bool * bool) =\n"
    "    switch(p)\n"
    "      (x1,    false, z1)   => (1, x1,    false, z1)\n"
    "      (false, y2,    true) => (2, false, y2,    true)\n"
    "      (true,  true,  z3)   => (3, true,  true,  z3)\n"
    "      (x4,    y4,    z4)   => (4, x4,    y4,    z4)\n"
    "  entrypoint lit_match(p : int * bool) =\n"
    "    switch(p)\n"
    "      (7, y) => y\n"
    "      _         => false\n"
    "  entrypoint even(n : int) =\n"
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
     "  entrypoint rec_match(a : r2) : int * r1 =\n"
     "    switch(a)\n"
     "      {x = 4}          => (1, a.y)\n"
     "      {y = {x = true}} => (2, {x = false, z = true, w = 0})\n"
     "      {x = x, y = r}   => (3, r { z = x == 0, w = x })\n"
     "  entrypoint rec_modify(a : r1) = a { w @ n = n + 1 }\n"
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

variants() ->
    {<<"variants">>,
     "contract Variants =\n"
     "  type number = int\n"
     "  datatype color('a, 'b) = Red('a, 'b) | Green | Blue(int)\n"
     "  entrypoint scramble(a : color(number, bool)) =\n"
     "    switch(a)\n"
     "      Red(x, true) => Blue(x)\n"
     "      Blue(x)      => Green\n"
     "      _            => Red(0, 1)\n"
     "  datatype r = A(bool, bool)\n"
     "  entrypoint missing1(a : r) : int =\n"
     "    switch(a)\n"
     "      A(false, false) => 1\n"
     "  entrypoint missing2(a : color(r, int)) : int =\n"
     "    switch(a)\n"
     "      Red(A(false, false), y) => y\n"
     "      _ => 0\n"
     "  entrypoint all_red(xs : list(color(int, int))) : bool =\n"
     "    switch(xs)\n"
     "      []              => true\n"
     "      Red(_, _) :: ys => all_red(ys)\n"
     "      _ :: _          => false\n"
     ""}.

-define(Red(X, Y), {variant, [2, 0, 1], 0, {X, Y}}).
-define(Green,     {variant, [2, 0, 1], 1, {}}).
-define(Blue(X),   {variant, [2, 0, 1], 2, {X}}).

variant_tests() ->
    Scramble = fun(?Red(X, true)) -> ?Blue(X);
                  (?Blue(_)) -> ?Green;
                  (_) -> ?Red(0, 1) end,
    ScrambleInput = [ ?Red(2, true), ?Red(1001, false), ?Blue(-99), ?Green ],
    Missing1 = fun({variant, [2], 0, {false, false}}) -> 1;
                  (_) -> {revert, <<"Incomplete patterns">>} end,
    Missing1Input = [{variant, [2], 0, {A, B}} || A <- [false, true], B <- [false, true]],
    Missing2 = fun(?Red({variant, [2], 0, {false, false}}, X)) -> X;
                  (_) -> 0 end,
    Missing2Input = [?Red({variant, [2], 0, {A, B}}, 5) || A <- [false, true], B <- [false, true]] ++
                    [?Green, ?Blue(42)],
    lists:flatten(
      [[],
       [{"scramble", [Input], Scramble(Input)} || Input <- ScrambleInput],
       [{"missing1", [Input], Missing1(Input)} || Input <- Missing1Input],
       [{"missing2", [Input], Missing2(Input)} || Input <- Missing2Input],
       [{"all_red", [[?Red(1, 2), ?Red(3, 4)]], true},
        {"all_red", [[?Red(1, 2), ?Green]], false},
        {"all_red", [[]], true}],
       []]).

variant_test_() -> mk_test([variants()], variant_tests()).

operators() ->
    {<<"operators">>,
     "contract Operators =\n"
     "  entrypoint arith(x, y) = [x + y, x - y, x * y, x / y, x mod y, x ^ y]\n"
     "  entrypoint list(xs, ys : list(int)) = [0 :: xs, xs ++ ys]\n"
     "  entrypoint compare(x, y : int * int) = [x == y, x != y, x < y, x > y, x =< y, x >= y]\n"
     "  entrypoint bool(x, y, z : int) = x == y && x > z || y >= z\n"}.

operator_tests() ->
    Bool = fun(X, Y, Z) -> X == Y andalso X > Z orelse Y >= Z end,
    lists:flatten(
      [[],
       [{"arith", [11, 3], [11 + 3, 11 - 3, 11 * 3, 11 div 3, 11 rem 3, 11 * 11 * 11]}],
       [{"list", [[1, 2, 3], [4, 5, 6]], [[0, 1, 2, 3], [1, 2, 3, 4, 5, 6]]}],
       [{"compare", [{1, 2}, {1, 3}], [false, true, true, false, true, false]}],
       [{"bool", [X, Y, Z], Bool(X, Y, Z)}
        || X <- [0, 1, 5], Y <- [0, 1, 5], Z <- [0, 1, 5]],
       []]).

operator_test_() -> mk_test([operators()], operator_tests()).

funcalls() ->
    {<<"funcalls">>,
     "contract FunCalls =\n"
     "  private function fst(x : 'a, y : 'b) = x\n"
     "  entrypoint call(x : int) =\n"
     "    if(fst(x == 42, 0)) fst(x, false)\n"
     "    else\n"
     "      fst(x + 1, \"__\")\n"}.

funcall_tests() ->
    lists:flatten(
      [[],
       [{"call", [42], 42},
        {"call", [43], 44}],
       []]).

funcall_test_() -> mk_test([funcalls()], funcall_tests()).

maps() ->
    {<<"maps">>,
     "contract Maps =\n"
     "  entrypoint test(m : map(int, int), y : int) =\n"
     "    let m1 = m{ [y] = y }\n"
     "    let m2 = {}\n"
     "    let m3 = m1{ [y] @ n = m2[y = n] + 1 }\n"
     "    (m3[y], {[0] = m3})\n"
     "  entrypoint upd(m : map(int, int)) = m{[77 = 19] @ n = n + 1}\n"
     ""}.

map_tests() ->
    lists:flatten(
      [[],
       [{"test", [#{3 => 5}, 5], {tuple, {6, #{0 => #{3 => 5, 5 => 6}}}}}],
       [{"upd", [#{77 => 100}], #{77 => 101}},
        {"upd", [#{66 => 100}], #{66 => 100, 77 => 20}}],
       []]).

map_test_() -> mk_test([maps()], map_tests()).

higher_order() ->
    {<<"higher_order">>,
     "contract HigherOrder =\n"
     "  private function curry(f : ('a, 'b) => 'c) =\n"
     "    (x) => (y) => f(x, y)\n"
     "  private function map(f : 'a => 'b, xs : list('a)) =\n"
     "    switch(xs)\n"
     "      [] => []\n"
     "      x :: xs => f(x) :: map(f, xs)\n"
     "  private function map'() = map\n"
     "  private function plus(x, y) = x + y\n"
     "  entrypoint test1(xs : list(int)) = map(curry(plus)(5), xs)\n"
     "  entrypoint test2(xs : list(int)) = map'()(((x) => (y) => ((x, y) => x + y)(x, y))(100), xs)\n"
     "  entrypoint test3(xs : list(int)) =\n"
     "    let m(f, xs) = map(f, xs)\n"
     "    m((x) => x + 1, xs)\n"
    }.

higher_order_tests() ->
    lists:flatten(
      [[],
       [{"test1", [[1, 2, 3]], [6, 7, 8]}],
       [{"test2", [[1, 2, 3]], [101, 102, 103]}],
       [{"test3", [[1, 2, 3]], [2, 3, 4]}],
       []]).

higher_order_test_() -> mk_test([higher_order()], higher_order_tests()).

remote() ->
    [{<<"main">>,
      "contract interface Remote =\n"
      "  entrypoint remote : int => int\n"
      "contract Main =\n"
      "  function bla(r : Remote) = r.remote\n"
      "  stateful entrypoint test(r : Remote) =\n"
      "    r.remote(42)\n"
      "      + r.remote(value = 10, 100)\n"
      "      + bla(r)(value = 11, gas = 88, 99)\n"},
     {<<"remote">>,
      "contract Remote =\n"
      "  payable entrypoint remote(x : int) = x * (2 + Call.value)\n"}].

remote_tests() ->
    lists:flatten(
      [[],
       [{"test", [make_contract(<<"remote">>)], 2 * 42 + (2 + 10) * 100 + (2 + 11) * 99}],
       []]).

remote_test_() -> mk_test(remote(), remote_tests()).
