%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Basic tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_eeevm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(opt_format(___Opts__, ___Fmt___, ___Args___),
        case maps:get(trace, ___Opts__, false) of
            true -> ?debugFmt(___Fmt___, ___Args___);
            _    -> ok
        end).

-define(wrap_run(___Expr___),
        try ___Expr___
        catch ___X___:___Y___ ->
                error({___X___, ___Y___, erlang:get_stacktrace()})
        end).


%%====================================================================
%% Test harness
%%====================================================================

testcase_generate(Path, Tests) ->
    case aevm_test_utils:is_external_available() of
        true ->
            {foreachx
            , fun get_config/1
            , fun(_, _) -> ok end
            , [{{Path, TestName}, fun testcase/2} || TestName <- Tests]
            };
        false ->
            []
    end.

get_config({Path, TestName}) ->
    aevm_test_utils:get_config(Path, TestName).

testcase({Path, Name}, Spec) ->
    { Path ++ "/" ++ atom_to_list(Name)
    , fun() ->
              Opts = extra_opts(Name),
              ?opt_format(Opts, "Running: ~w~n", [Name]),
              State = ?wrap_run(aevm_eeevm:run(Spec#{opts => Opts})),
              ?opt_format(Opts, "Checking: ~w~n", [Name]),
              PostStorageSpec = aevm_test_utils:get_post_storage(Spec),
              Storage = aevm_eeevm_state:storage(State),
              ?opt_format(Opts, "State of ~w: ~p~n", [Name, State]),
              ?assertEqual(PostStorageSpec, Storage)
      end
    }.

%% To turn on tracing for a test case return a map with trace => true.
%% e.g. extra_opts(mulmod4) -> #{trace => true};
extra_opts(signextend_Overflow_dj42) -> #{trace => true};
extra_opts(_) ->
    #{}.

%%====================================================================
%% Arithmetic tests
%%====================================================================

arithmetic_test_() ->
    testcase_generate("VMTests/vmArithmeticTest", arithmetic_tests()).

arithmetic_tests() ->
    [ add0
    , add1
    , add2
    , add3
    , add4
    , addmod0
    , addmod1
    , addmod1_overflow2
    , addmod1_overflow3
    , addmod1_overflow4
    , addmod1_overflowDiff
    , addmod2_0
    , addmod2_1
    , addmod3
    , addmod3_0
    , addmodBigIntCast
    , addmodDivByZero
    , addmodDivByZero1
    , addmodDivByZero2
    , addmodDivByZero3
    , arith1
    , div1
    , divBoostBug
    , divByNonZero0
    , divByNonZero1
    , divByNonZero2
    , divByNonZero3
    , divByZero
    , divByZero_2
    , exp0
    , exp1
    , exp2
    , exp3
    , exp4
    , exp5
    , exp6
    , exp7
    , expPowerOf256Of256_0
    , expPowerOf2_128
    , expPowerOf2_16
    , expPowerOf2_256
    , expPowerOf2_2
    , expPowerOf2_32
    , expPowerOf2_4
    , expPowerOf256_10
    , expPowerOf256_11
    , expPowerOf256_12
    , expPowerOf256_13
    , expPowerOf256_14
    , expPowerOf256_15
    , expPowerOf256_16
    , expPowerOf256_17
    , expPowerOf256_18
    , expPowerOf256_19
    , expPowerOf256_1
    , expPowerOf256_20
    , expPowerOf256_21
    , expPowerOf256_22
    , expPowerOf256_23
    , expPowerOf256_24
    , expPowerOf256_25
    , expPowerOf256_26
    , expPowerOf256_27
    , expPowerOf256_28
    , expPowerOf256_29
    , expPowerOf256_2
    , expPowerOf256_30
    , expPowerOf256_31
    , expPowerOf256_32
    , expPowerOf256_33
    , expPowerOf256_3
    , expPowerOf256_4
    , expPowerOf256_5
    , expPowerOf256_6
    , expPowerOf256_7
    , expPowerOf256_8
    , expPowerOf256_9
    , expPowerOf256Of256_0
    , expPowerOf256Of256_10
    , expPowerOf256Of256_11
    , expPowerOf256Of256_12
    , expPowerOf256Of256_13
    , expPowerOf256Of256_14
    , expPowerOf256Of256_15
    , expPowerOf256Of256_16
    , expPowerOf256Of256_17
    , expPowerOf256Of256_18
    , expPowerOf256Of256_19
    , expPowerOf256Of256_1
    , expPowerOf256Of256_20
    , expPowerOf256Of256_21
    , expPowerOf256Of256_22
    , expPowerOf256Of256_23
    , expPowerOf256Of256_24
    , expPowerOf256Of256_25
    , expPowerOf256Of256_26
    , expPowerOf256Of256_27
    , expPowerOf256Of256_28
    , expPowerOf256Of256_29
    , expPowerOf256Of256_2
    , expPowerOf256Of256_30
    , expPowerOf256Of256_31
    , expPowerOf256Of256_32
    , expPowerOf256Of256_33
    , expPowerOf256Of256_3
    , expPowerOf256Of256_4
    , expPowerOf256Of256_5
    , expPowerOf256Of256_6
    , expPowerOf256Of256_7
    , expPowerOf256Of256_8
    , expPowerOf256Of256_9
    , expPowerOf2_64
    , expPowerOf2_8
    , expXY
    , expXY_success
    , fibbonacci_unrolled
    , mod0
    , mod1
    , mod2
    , mod3
    , mod4
    , modByZero
    , mul0
    , mul1
    , mul2
    , mul3
    , mul4
    , mul5
    , mul6
    , mul7
      %% , mulUnderFlow %% Missing callcreates
    , mulmod0
    , mulmod1
    , mulmod1_overflow
    , mulmod1_overflow2
    , mulmod1_overflow3
    , mulmod1_overflow4
    , mulmod2_0
    , mulmod2_1
    , mulmod3
    , mulmod3_0
    , mulmod4
    , mulmoddivByZero
    , mulmoddivByZero1
    , mulmoddivByZero2
    , mulmoddivByZero3
    , not1
    , sdiv0
    , sdiv1
    , sdiv2
    , sdiv3
    , sdiv4
    , sdiv5
    , sdiv6
    , sdiv7
    , sdiv8
    , sdiv9
    , sdivByZero0
    , sdivByZero1
    , sdivByZero2
    %% , sdiv_dejavu %% Missing callcreates && post. Illegal pop
    , sdiv_i256min
    , sdiv_i256min2
    , sdiv_i256min3
    , signextend_00
    , signextend_0_BigByte
    , signextend_BigBytePlus1_2
    , signextend_bigBytePlus1
    , signextend_BitIsNotSetInHigherByte
    , signextend_BitIsNotSet
    , signextend_BitIsSetInHigherByte
    , signextend_bitIsSet
    , signextendInvalidByteNumber
    %% , signextend_Overflow_dj42 %% Missing callcreates && post. Illegal pop
    , smod0
    , smod1
    , smod2
    , smod3
    , smod4
    , smod5
    , smod6
    , smod7
    , smod8_byZero
    , smod_i256min1
    , smod_i256min2
    , stop
    , sub0
    , sub1
    , sub2
    , sub3
    , sub4
    ].


%%====================================================================
%% BitwiseLogicOperation tests
%%====================================================================

logic_test_() ->
    testcase_generate("VMTests/vmBitwiseLogicOperation",
		      logic_tests()).

logic_tests() ->
    [ and0
    , and1
    , and2
    , and3
    , and4
    , and5
    , byte0
    , byte1
    , byte2
    , byte3
    , byte4
    , byte5
    , byte6
    , byte7
    , byte8
    , byte9
    , byte10
    , byte11
    , eq0
    , eq1
    , eq2
    , gt0
    , gt1
    , gt2
    , gt3
    , iszeo2 %% !sic
    , iszero0
    , iszero1
    , lt0
    , lt1
    , lt2
    , lt3
    , not0
    , not1
    , not2
    , not3
    , not4
    , not5
    , or0
    , or1
    , or2
    , or3
    , or4
    , or5
    , sgt0
    , sgt1
    , sgt2
    , sgt3
    , sgt4
    , slt0
    , slt1
    , slt2
    , slt3
    , slt4
    , xor0
    , xor1
    , xor2
    , xor3
    , xor4
    , xor5
    ].



%%====================================================================
%% Internal functions
%%====================================================================

