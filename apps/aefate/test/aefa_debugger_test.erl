-module(aefa_debugger_test).

-include_lib("eunit/include/eunit.hrl").

run_debug(Cache, Contract, Function, Arguments, BPs) ->
    {_, Res} = timed_run_debug(Cache, Contract, Function, Arguments, BPs),
    Res.

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

resume_kinds() ->
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
    Contracts = [resume_kinds()],
    Chain     = aefa_sophia_test:compile_contracts(Contracts, debug_options()),
    Main      = element(1, hd(Contracts)),
    case run_debug(Chain, Main, list_to_binary("f"), [2], [{"", 10}]) of
        {ok, ES} ->
            DbgInf = aefa_engine_state:debug_info(ES),
            ES2 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(stepin, DbgInf),
                    ES
                )
            ),
            DbgInf2 = aefa_engine_state:debug_info(ES2),
            ES3 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status({stepout, aefa_engine_state:call_stack(ES2)} , DbgInf2),
                    ES2
                )
            ),
            DbgInf3 = aefa_engine_state:debug_info(ES3),
            ES4 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status({stepover, aefa_engine_state:call_stack(ES3)}, DbgInf3),
                    ES3
                )
            ),
            DbgInf4 = aefa_engine_state:debug_info(ES4),
            ES5 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf4),
                    ES4
                )
            ),
            DbgInf5 = aefa_engine_state:debug_info(ES5),
            [ ?_assertEqual(break, aefa_debug:debugger_status(DbgInf))
            , ?_assertEqual({[], 10}, aefa_debug:debugger_location(DbgInf))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf2))
            , ?_assertEqual({[], 5}, aefa_debug:debugger_location(DbgInf2))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf3))
            , ?_assertEqual({[], 11}, aefa_debug:debugger_location(DbgInf3))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf4))
            , ?_assertEqual({[], 12}, aefa_debug:debugger_location(DbgInf4))

            , ?_assertEqual(continue, aefa_debug:debugger_status(DbgInf5)) ];
        _ ->
            ?debugMsg("Not fine")
    end.

single_breakpoint_hit_test_() ->
    Contracts = [aefa_sophia_test:patterns()],
    Chain     = aefa_sophia_test:compile_contracts(Contracts, debug_options()),
    Main      = element(1, hd(Contracts)),
    case run_debug(Chain, Main, list_to_binary("even"), [1], [{"", 25}]) of
        {ok, ES} ->
            DbgInf = aefa_engine_state:debug_info(ES),
            [ ?_assertEqual(break, aefa_debug:debugger_status(DbgInf))
            , ?_assertEqual({[], 25}, aefa_debug:debugger_location(DbgInf)) ];
        _ ->
            ?debugMsg("Not fine")
    end.

single_breakpoint_not_hit_test_() ->
    Contracts = [aefa_sophia_test:patterns()],
    Chain     = aefa_sophia_test:compile_contracts(Contracts, debug_options()),
    Main      = element(1, hd(Contracts)),
    case run_debug(Chain, Main, list_to_binary("even"), [1], [{"", 4}]) of
        {ok, ES} ->
            DbgInf = aefa_engine_state:debug_info(ES),
            [ ?_assertEqual(continue, aefa_debug:debugger_status(DbgInf))
            , ?_assertEqual({[], 27}, aefa_debug:debugger_location(DbgInf)) ];
        _ ->
            ?debugMsg("Not fine")
    end.

callstack_push_pop() ->
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

vars_regs() ->
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
    Contracts = [vars_regs()],
    Chain     = aefa_sophia_test:compile_contracts(Contracts, debug_options()),
    Main      = element(1, hd(Contracts)),
    case run_debug(Chain, Main, list_to_binary("f"), [], [{"", 3}, {"", 5}, {"", 8}, {"", 11}]) of
        {ok, ES} ->
            DbgInf = aefa_engine_state:debug_info(ES),
            ES2 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf),
                    ES
                )
            ),
            DbgInf2 = aefa_engine_state:debug_info(ES2),
            ES3 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf2),
                    ES2
                )
            ),
            DbgInf3 = aefa_engine_state:debug_info(ES3),
            ES4 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf3),
                    ES3
                )
            ),
            DbgInf4 = aefa_engine_state:debug_info(ES4),
            ES5 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf4),
                    ES4
                )
            ),
            DbgInf5 = aefa_engine_state:debug_info(ES5),
            [ ?_assertEqual(break, aefa_debug:debugger_status(DbgInf))
            , ?_assertEqual({[], 3}, aefa_debug:debugger_location(DbgInf))
            , ?_assertEqual(undefined, aefa_debug:get_variable_register("a", DbgInf))
            , ?_assertEqual(undefined, aefa_debug:get_variable_register("b", DbgInf))
            , ?_assertEqual(undefined, aefa_debug:get_variable_register("x", DbgInf))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf2))
            , ?_assertEqual({[], 5}, aefa_debug:debugger_location(DbgInf2))
            , ?_assertEqual({var, 0}, aefa_debug:get_variable_register("a", DbgInf2))
            , ?_assertEqual({var, 1}, aefa_debug:get_variable_register("b", DbgInf2))
            , ?_assertEqual(undefined, aefa_debug:get_variable_register("x", DbgInf2))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf3))
            , ?_assertEqual({[], 8}, aefa_debug:debugger_location(DbgInf3))
            , ?_assertEqual({var, 0}, aefa_debug:get_variable_register("a", DbgInf3))
            , ?_assertEqual({var, 4}, aefa_debug:get_variable_register("b", DbgInf3))
            , ?_assertEqual({var, 3}, aefa_debug:get_variable_register("x", DbgInf3))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf4))
            , ?_assertEqual({[], 11}, aefa_debug:debugger_location(DbgInf4))
            , ?_assertEqual({var, 0}, aefa_debug:get_variable_register("a", DbgInf4))
            , ?_assertEqual({var, 1}, aefa_debug:get_variable_register("b", DbgInf4))
            , ?_assertEqual(undefined, aefa_debug:get_variable_register("x", DbgInf4))

            , ?_assertEqual(continue, aefa_debug:debugger_status(DbgInf5)) ];
        _ ->
            ?debugMsg("Not fine")
    end.

callstack_push_pop_test_() ->
    Contracts = [callstack_push_pop()],
    Chain     = aefa_sophia_test:compile_contracts(Contracts, debug_options()),
    Main      = element(1, hd(Contracts)),
    case run_debug(Chain, Main, list_to_binary("calc"), [5], [{"", 3}, {"", 5}, {"", 9}]) of
        {ok, ES} ->
            DbgInf = aefa_engine_state:debug_info(ES),
            ES2 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf),
                    ES
                )
            ),
            DbgInf2 = aefa_engine_state:debug_info(ES2),
            ES3 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf2),
                    ES2
                )
            ),
            DbgInf3 = aefa_engine_state:debug_info(ES3),
            ES4 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf3),
                    ES3
                )
            ),
            DbgInf4 = aefa_engine_state:debug_info(ES4),
            ES5 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf4),
                    ES4
                )
            ),
            DbgInf5 = aefa_engine_state:debug_info(ES5),
            [ ?_assertEqual(break, aefa_debug:debugger_status(DbgInf))
            , ?_assertEqual({[], 3}, aefa_debug:debugger_location(DbgInf))
            , ?_assertMatch([{[], 7}], aefa_debug:call_stack(DbgInf))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf2))
            , ?_assertEqual({[], 5}, aefa_debug:debugger_location(DbgInf2))
            , ?_assertMatch([{[], 8}], aefa_debug:call_stack(DbgInf2))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf3))
            , ?_assertEqual({[], 3}, aefa_debug:debugger_location(DbgInf3))
            , ?_assertMatch([{[], 5}, {[], 8}], aefa_debug:call_stack(DbgInf3))

            , ?_assertEqual(break, aefa_debug:debugger_status(DbgInf4))
            , ?_assertEqual({[], 9}, aefa_debug:debugger_location(DbgInf4))
            , ?_assertMatch([], aefa_debug:call_stack(DbgInf5))

            , ?_assertEqual(continue, aefa_debug:debugger_status(DbgInf5))
            , ?_assertEqual({[], 9}, aefa_debug:debugger_location(DbgInf5))
            , ?_assertMatch([], aefa_debug:call_stack(DbgInf5)) ];
        _ ->
            ?debugMsg("Not fine")
    end.

contract_name_test_() ->
    Contracts = [contract_name_change()],
    Chain     = aefa_sophia_test:compile_contracts(Contracts, debug_options()),
    Main      = element(1, hd(Contracts)),
    case run_debug(Chain, Main, list_to_binary("f"), [], [{"", 6}, {"", 3}]) of
        {ok, ES} ->
            DbgInf = aefa_engine_state:debug_info(ES),
            ES2 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf),
                    ES
                )
            ),
            DbgInf2 = aefa_engine_state:debug_info(ES2),
            ES3 = aefa_fate:execute(
                aefa_engine_state:set_debug_info(
                    aefa_debug:set_debugger_status(continue, DbgInf2),
                    ES2
                )
            ),
            DbgInf3 = aefa_engine_state:debug_info(ES3),
            [ ?_assertEqual("MC", aefa_debug:contract_name(aefa_engine_state:current_contract(ES), DbgInf))
            , ?_assertEqual("C", aefa_debug:contract_name(aefa_engine_state:current_contract(ES2), DbgInf2))
            , ?_assertEqual("MC", aefa_debug:contract_name(aefa_engine_state:current_contract(ES3), DbgInf3))
            , ?_assertEqual([{"", 6}, {"", 3}], aefa_debug:breakpoints(DbgInf3))];
        _ ->
            ?debugMsg("Not fine")
    end.

contract_name_change() ->
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
