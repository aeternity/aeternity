%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Basic tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_eeevm_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Basic tests
%%====================================================================

basic_test_() ->
    {foreachx
    , fun(TestName) -> aevm_test_utils:get_config(TestName) end %% Setup
    , fun(_, _) -> ok end                                       %% End
    , basic_generate(basic_tests())
    }.

basic_generate(Tests) ->
    [{ TestName
     , fun(Name, Spec) -> {atom_to_list(Name), basic_testcase(Name, Spec)}
       end
     } || TestName <- Tests].

basic_testcase(Name, Spec) ->
    fun() ->
            Opts = extra_opts(Name),
            RunSpec = aevm_test_utils:get_eeevm_run_config(Spec, Opts),
            State = aevm_eeevm:run(RunSpec),
            PostMemSpec = aevm_test_utils:get_post_storage(Spec),
            Mem = aevm_eeevm_state:mem(State),
            ?assertEqual(PostMemSpec, Mem)
    end.

basic_tests() ->
    [ add0
    , add1
    , add2
    , add3
    , add4
    , addmod0
    , stop
    ].

extra_opts(add1) ->
    #{trace => true};
extra_opts(_) ->
    #{}.

%%====================================================================
%% Internal functions
%%====================================================================

