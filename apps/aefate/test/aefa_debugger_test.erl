-module(aefa_debugger_test).

-include_lib("eunit/include/eunit.hrl").

-record(assertion,
    { status      = none
    , location    = none
    , vars        = none
    , stack       = none
    , contract    = none
    , breakpoints = none }).

exec_assertions(ES, Asserts) ->
    #assertion{
        status      = Status,
        location    = Location,
        vars        = Vars,
        stack       = Stack,
        contract    = Contract,
        breakpoints = Breakpoints } = Asserts,
    Info = aefa_engine_state:debug_info(ES),
    
    Status      =/= none andalso ?assertEqual(Status, aefa_debug:debugger_status(Info)),
    Location    =/= none andalso ?assertEqual(Location, aefa_debug:debugger_location(Info)),
    Vars        =/= none andalso [ ?assertEqual(Reg, aefa_debug:get_variable_register(Var, Info)) || {Var, Reg} <- Vars ],
    Stack       =/= none andalso ?assertEqual(Stack, aefa_debug:call_stack(Info)),
    Contract    =/= none andalso ?assertEqual(Contract, aefa_debug:contract_name(aefa_engine_state:current_contract(ES), Info)),
    Breakpoints =/= none andalso ?assertEqual(Breakpoints, aefa_debug:breakpoints(Info)).

run_debug(Cache, Contract, Function, Arguments, BPs) ->
    {_, Res} = timed_run_debug(Cache, Contract, Function, Arguments, BPs),
    Res.

run(Source, Entrypoint, Args, Breakpoints, Asserts) ->
    Contracts = [Source],
    Chain     = aefa_sophia_test:compile_contracts(Contracts, debug_options()),
    Main      = element(1, hd(Contracts)),
    Res       = run_debug(Chain, Main, list_to_binary(Entrypoint), Args, Breakpoints),
    ?assertMatch({ok, _}, Res),
    {ok, ES} = Res,
    exec_assertions(ES, Asserts),
    ES.

resume(ES0, Asserts, ResumeKind) ->
    Info0 = aefa_engine_state:debug_info(ES0),
    ES = aefa_fate:execute(
        aefa_engine_state:set_debug_info(
            aefa_debug:set_debugger_status(ResumeKind, Info0),
            ES0
        )
    ),
    exec_assertions(ES, Asserts),
    ES.

timed_run_debug(Cache, Contract, Function, Arguments, BPs) ->
    timed_run_debug(Cache, Contract, Function, Arguments, #{}, BPs).

timed_run_debug(Cache, Contract, Function, Arguments, Store0, BPs) ->
    Spec = #{ store := Store } = aefa_sophia_test:make_call_spec(Contract, Function, Arguments, Store0),
    Env = aefa_sophia_test:dummy_spec(Cache, #{aefa_sophia_test:pad_contract_name(Contract) => Store}),
    try
        timer:tc(fun() -> aefa_fate:run_debug(Spec, Env, Cache, BPs) end)
    catch _:{error, Err} ->
              {0, {error, Err, []}}
    end.

debug_options() ->
    aefa_sophia_test:default_options() ++ disabled_optimizations_options() ++ [debug_info].

disabled_optimizations_options() ->
    [ {optimize_inliner, false}, {optimize_inline_local_functions, false}
    , {optimize_bind_subexpressions, false}, {optimize_let_floating, false}
    , {optimize_simplifier, false}, {optimize_drop_unused_lets, false}
    , {optimize_push_consume, false}, {optimize_one_shot_var, false}
    , {optimize_write_to_dead_var, false}, {optimize_inline_switch_target, false}
    , {optimize_swap_push, false}, {optimize_swap_pop, false}
    , {optimize_swap_write, false}, {optimize_constant_propagation, false}
    , {optimize_prune_impossible_branches, false}, {optimize_single_successful_branch, false}
    , {optimize_inline_store, false}, {optimize_float_switch_bod, false} ].

%% -- Tests --

basic_contract_source() ->
    {<<"basic_contract">>,
    "contract C =\n"
    "  entrypoint even(x) =\n"
    "    if (x mod 2 == 0)\n"
    "      true\n"
    "    else\n"
    "      false\n"
    "  entrypoint odd(x) =\n"
    "    !even(x)\n"
    ""}.

single_breakpoint_hit_test_() ->
    BPs = [{"", 4}], 

    Asserts = #assertion{status = break, location = {[], 4}},

    {"Test single breakpoint hit",
    fun() ->
        _ = run(basic_contract_source(), "even", [20], BPs, Asserts)
    end}.

single_breakpoint_not_hit_test_() ->
    BPs = [{"", 6}], 

    Asserts = #assertion{status = continue, location = {[], 4}},

    {"Test single breakpoint not hit",
    fun() ->
        _ = run(basic_contract_source(), "even", [30], BPs, Asserts)
    end}.

resume_kinds_source() ->
    {<<"resume_kinds">>,
    "contract C =\n"
    "  function h(x) =\n"
    "    let y = x * (x + 1)\n"
    "    y * y\n"
    "  function g(x) =\n"
    "    let a = 1\n"
    "    let b = 2\n"
    "    h(a * b)\n"
    "  entrypoint f(x) =\n"
    "    let a = g(x)\n"
    "    let b = h(x)\n"
    "    a + b\n"
    ""}.

resume_kinds_test_() ->
    BPs = [{"", 10}],

    Asserts1 = #assertion{status = break, location = {[], 10}},
    Asserts2 = #assertion{status = break, location = {[], 5}},
    Asserts3 = #assertion{status = break, location = {[], 11}},
    Asserts4 = #assertion{status = break, location = {[], 12}},
    Asserts5 = #assertion{status = continue},

    {"Test resume kinds",
    fun() ->
        ES1 = run(resume_kinds_source(), "f", [2], BPs, Asserts1),
        ES2 = resume(ES1, Asserts2, stepin),
        ES3 = resume(ES2, Asserts3, {stepout, aefa_engine_state:call_stack(ES2)}),
        ES4 = resume(ES3, Asserts4, {stepover, aefa_engine_state:call_stack(ES3)}),
        _   = resume(ES4, Asserts5, continue)
    end}.

vars_regs_source() ->
    {<<"vars_regs">>,
    "contract C =\n"
    "  entrypoint f() =\n"
    "    let a = 1\n"
    "    let b = 2\n"
    "    if (a > 0)\n"
    "      let x = 4\n"
    "      let b = 6\n"
    "      x + b\n"
    "    else\n"
    "      1\n"
    "    a + b\n"
    ""}.

vars_regs_test_() ->
    BPs = [{"", 3}, {"", 5}, {"", 8}, {"", 11}],

    Asserts1 = #assertion{
        status = break,
        location = {[], 3},
        vars = [{"a", undefined}, {"b", undefined}, {"x", undefined}] }, 
    Asserts2 = #assertion{
        status = break,
        location = {[], 5},
        vars = [{"a", {var, 0}}, {"b", {var, 1}}, {"x", undefined}] }, 
    Asserts3 = #assertion{
        status = break,
        location = {[], 8},
        vars = [{"a", {var, 0}}, {"b", {var, 4}}, {"x", {var, 3}}] }, 
    Asserts4 = #assertion{
        status = break,
        location = {[], 11},
        vars = [{"a", {var, 0}}, {"b", {var, 1}}, {"x", undefined}] }, 
    Asserts5 = #assertion{ status = continue },

    {"Test variable to register mapping",
    fun() ->
        ES1 = run(vars_regs_source(), "f", [], BPs, Asserts1),
        ES2 = resume(ES1, Asserts2, continue),
        ES3 = resume(ES2, Asserts3, continue),
        ES4 = resume(ES3, Asserts4, continue),
        _   = resume(ES4, Asserts5, continue)
    end}.

callstack_push_pop_source() ->
    {<<"callstack_push_pop">>,
    "contract C =\n"
    "  function f(x) =\n"
    "    x * 2\n"
    "  function g(x) =\n"
    "    f(x) * 3\n"
    "  entrypoint calc(x) =\n"
    "    let a = f(x)\n"
    "    let b = g(x)\n"
    "    a + b\n"
    ""}.

callstack_push_pop_test_() ->
    BPs = [{"", 3}, {"", 5}, {"", 9}],

    Asserts1 = #assertion{
        status = break,
        location = {[], 3},
        stack = [{[], 7}] },
    Asserts2 = #assertion{
        status = break,
        location = {[], 5},
        stack = [{[], 8}] },
    Asserts3 = #assertion{
        status = break,
        location = {[], 3},
        stack = [{[], 5}, {[], 8}] },
    Asserts4 = #assertion{
        status = break,
        location = {[], 9},
        stack = [] },
    Asserts5 = #assertion{
        status = continue,
        location = {[], 9},
        stack = [] },

    {"Test pushing and poping from the callstack",
    fun() ->
        ES1 = run(callstack_push_pop_source(), "calc", [5], BPs, Asserts1),
        ES2 = resume(ES1, Asserts2, continue),
        ES3 = resume(ES2, Asserts3, continue),
        ES4 = resume(ES3, Asserts4, continue),
        _   = resume(ES4, Asserts5, continue)
    end}.

contract_name_source() ->
    {<<"contract_name_change">>,
    "contract C =\n"
    "  entrypoint f(x : int) =\n"
    "    x * x\n"
    "main contract MC =\n"
    "  stateful entrypoint f() =\n"
    "    let c = Chain.create() : C\n"
    "    let a = c.f(5)\n"
    "    let b = a * a\n"
    "    b\n"
    ""}.

contract_name_test_() ->
    BPs = [{"", 6}, {"", 3}],

    Asserts1 = #assertion{ contract = "MC" },
    Asserts2 = #assertion{ contract = "C" },
    Asserts3 = #assertion{ contract = "MC", breakpoints = [{"", 6}, {"", 3}] },

    {"Test the mapping from contract pks to contract names",
    fun() ->
        ES1 = run(contract_name_source(), "f", [], BPs, Asserts1),
        ES2 = resume(ES1, Asserts2, continue),
        _   = resume(ES2, Asserts3, continue)
    end}.
