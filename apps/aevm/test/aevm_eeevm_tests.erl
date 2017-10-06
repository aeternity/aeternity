%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Basic tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_eeevm_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test harness
%%====================================================================

%% To turn on tracing for a test case return a map with trace => true.
%% e.g. extra_opts(mulmod4) -> #{trace => true};
extra_opts(_) ->
    #{}.

%%====================================================================
%% Arithmetic tests
%%====================================================================

arithmetic_test_() ->
    Tests = arithmetic_tests(),
    Path  = "VMTests/vmArithmeticTest",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

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
    Tests = logic_tests(),
    Path  = "VMTests/vmBitwiseLogicOperation",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

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
%% VMTests tests
%%====================================================================

vm_test_() ->
    aevm_test_utils:testcase_generate("VMTests/vmTests", vm_tests()).

vm_tests() ->
    [ %% arith    %% Missing post in all of these
      %% boolean 
      %% mktx
      suicide
    ].

%%====================================================================
%% vmPushDupSwapTest tests
%%====================================================================

vm_push_dup_swap_test_() ->
    aevm_test_utils:testcase_generate("VMTests/vmPushDupSwapTest", vm_push_dup_swap_tests()).

vm_push_dup_swap_tests() ->
    [ dup1
    , dup2
    %% , dup2error %% Throws pop on empty. TC-Spec has no post block.
    , dup3
    , dup4
    , dup5
    , dup6
    , dup7
    , dup8
    , dup9
    , dup10
    , dup11
    , dup12
    , dup13
    , dup14
    , dup15
    , dup16
    , push1
    , push2
    , push3
    , push4
    , push5
    , push6
    , push7
    , push8
    , push9
    , push10
    , push11
    , push12
    , push13
    , push14
    , push15
    , push16
    , push17
    , push18
    , push19
    , push20
    , push21
    , push22
    , push23
    , push24
    , push25
    , push26
    , push27
    , push28
    , push29
    , push30
    , push31
    , push32
    , push1_missingStack
    , push32AndSuicide
    , push32FillUpInputWithZerosAtTheEnd
    , push32Undefined
    , push32Undefined2
    , push32Undefined3
      %% , push33 %% No post in TC-spec
    , swap1
    , swap2
    , swap3
    , swap4
    , swap5
    , swap6
    , swap7
    , swap8
    , swap9
    , swap10
    , swap11
    , swap12
    , swap13
    , swap14
    , swap15
    , swap16
      %% , swap2error %% No post in TC-spec
      %% , swapjump1  %% No post in TC-spec
    ].

%%====================================================================
%% VMTests tests
%%====================================================================

vm_sha3_test_() ->
    aevm_test_utils:testcase_generate("VMTests/vmSha3Test",
				      vm_sha3_tests()).

vm_sha3_tests() ->
    [ sha3_0
    , sha3_1
    , sha3_2
    , sha3_3 %% no post
    , sha3_4 %% no post
    %% , sha3_5 %% timeout
    %% , sha3_6 %% timeout
    , sha3_bigOffset %% no post
    , sha3_bigOffset2
    %% , sha3_bigSize %% timeout
    , sha3_memSizeNoQuadraticCost31
    , sha3_memSizeQuadraticCost32
    , sha3_memSizeQuadraticCost32_zeroSize
    , sha3_memSizeQuadraticCost33
    , sha3_memSizeQuadraticCost63
    , sha3_memSizeQuadraticCost64
    , sha3_memSizeQuadraticCost64_2
    , sha3_memSizeQuadraticCost65
    ].

%%====================================================================
%% VMTests tests
%%====================================================================

vm_io_and_flow_operations_test_() ->
    aevm_test_utils:testcase_generate("VMTests/vmIOandFlowOperations",
				      vm_io_and_flow_operations_tests()).


vm_io_and_flow_operations_tests() ->
    [ jump0_foreverOutOfGas
    , jump0_jumpdest0
    , jump0_jumpdest2
    , jumpToUintmaxPlus1
    , kv1
    , loop_stacklimit_1020
    , loop_stacklimit_1021
    , msize0
    , msize1
    , msize2
      %% , msize3 %% TODO: Need new memory handling
    , pop0
    , pop1
    , pc0
    , pc1
    , return2
    , stack_loop
    , 'when'
    ].

%%====================================================================
%% Internal functions
%%====================================================================

