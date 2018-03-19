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

%% For VMTests
%%  "Because the data of the blockchain is not given,
%%   the opcode BLOCKHASH could not return the hashes
%%   of the corresponding blocks. Therefore we define the hash of 
%%   block number n to be SHA3-256("n")."
%% Indicated by the option blockhash->sha3
extra_opts(Name) ->
    maps:merge(default_opts(), extra_opts_tc(Name)).

default_opts() ->
    #{ blockhash => sha3
     , no_recursion => true
     }.

%% To turn on tracing for a test case return a map with trace => true
%% e.g. extra_opts_tc(mulmod4) -> #{trace => true};
extra_opts_tc(Name) ->
    case gas_exception(Name) of
        true  -> #{validate_gas => false};
        false -> #{}
    end.

gas_exception(Name) ->
    lists:member(Name,
                 [ suicide
                 , push32AndSuicide
                 ]).

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
    , mulUnderFlow
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
    , sdiv_dejavu
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
    Tests =  vm_tests(),
    Path  = "VMTests/vmTests",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

vm_tests() ->
    [ arith    %% Missing post in all of these
    , boolean
    , mktx
    , suicide
    ].

%%====================================================================
%% vmPushDupSwapTest tests
%%====================================================================

vm_push_dup_swap_test_() ->
    aevm_test_utils:testcase_generate("VMTests/vmPushDupSwapTest", vm_push_dup_swap_tests(), fun extra_opts/1).

vm_push_dup_swap_tests() ->
    [ dup1
    , dup2
    , dup2error
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
    , push33
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
    , swap2error
    , swapjump1
    ].

%%====================================================================
%% Sha3 tests
%%====================================================================

vm_sha3_test_() ->
    Tests =  vm_sha3_tests(),
    Path  = "VMTests/vmSha3Test",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

vm_sha3_tests() ->
    [ sha3_0
    , sha3_1
    , sha3_2
    , sha3_3
    , sha3_4
    , sha3_5
    , sha3_6
    , sha3_bigOffset
    , sha3_bigOffset2
    , sha3_bigSize
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
%% VM IO and Flow tests
%%====================================================================

vm_io_and_flow_operations_test_() ->
    Tests = vm_io_and_flow_operations_tests(),
    Path  = "VMTests/vmIOandFlowOperations",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

vm_io_and_flow_operations_tests() ->
    [ bad_indirect_jump1
    , bad_indirect_jump2
    , 'BlockNumberDynamicJump0_AfterJumpdest'
    , 'BlockNumberDynamicJump0_AfterJumpdest3'
    , 'BlockNumberDynamicJump0_AfterJumpdest'
    , 'BlockNumberDynamicJump0_AfterJumpdest3'
    , 'BlockNumberDynamicJump0_foreverOutOfGas'
    , 'BlockNumberDynamicJump0_jumpdest0'
    , 'BlockNumberDynamicJump0_jumpdest2'
    , 'BlockNumberDynamicJump0_withoutJumpdest'
    , 'BlockNumberDynamicJump1'
    , 'BlockNumberDynamicJumpi0'
    , 'BlockNumberDynamicJumpi1'
    , 'BlockNumberDynamicJumpi1_jumpdest'
    , 'BlockNumberDynamicJumpiAfterStop'
    , 'BlockNumberDynamicJumpifInsidePushWithJumpDest'
    , 'BlockNumberDynamicJumpifInsidePushWithoutJumpDest'
    , 'BlockNumberDynamicJumpInsidePushWithJumpDest'
    , 'BlockNumberDynamicJumpInsidePushWithoutJumpDest'
    , 'BlockNumberDynamicJumpiOutsideBoundary'
    , byte1
    , calldatacopyMemExp
    , codecopyMemExp
    , deadCode_1
    , dupAt51becameMload
    , 'DyanmicJump0_outOfBoundary'
    , 'DynamicJump0_AfterJumpdest3'
    , 'DynamicJump0_AfterJumpdest'
    , 'DynamicJump0_foreverOutOfGas'
    , 'DynamicJump0_jumpdest0'
    , 'DynamicJump0_jumpdest2'
    , 'DynamicJump0_withoutJumpdest'
    , 'DynamicJump1'
    , 'DynamicJumpAfterStop'
    , 'DynamicJumpi0'
    , 'DynamicJumpi1'
    , 'DynamicJumpi1_jumpdest'
    , 'DynamicJumpiAfterStop'
    , 'DynamicJumpifInsidePushWithJumpDest'
    , 'DynamicJumpifInsidePushWithoutJumpDest'
    , 'DynamicJumpInsidePushWithJumpDest'
    , 'DynamicJumpInsidePushWithoutJumpDest'
    , 'DynamicJumpiOutsideBoundary'
    , 'DynamicJumpJD_DependsOnJumps0'
    , 'DynamicJumpJD_DependsOnJumps1'
    , 'DynamicJumpPathologicalTest0'
    , 'DynamicJumpPathologicalTest1'
    , 'DynamicJumpPathologicalTest2'
    , 'DynamicJumpPathologicalTest3'
    , 'DynamicJumpStartWithJumpDest'
    , 'DynamicJump_value1'
    , 'DynamicJump_value2'
    , 'DynamicJump_value3'
    , 'DynamicJump_valueUnderflow'
    , 'extcodecopyMemExp'
    , for_loop1
    , for_loop2
    , gas0
    , gas1
    , gasOverFlow
    , 'indirect_jump1'
    , 'indirect_jump2'
    , 'indirect_jump3'
    , 'indirect_jump4'
    , 'JDfromStorageDynamicJump0_AfterJumpdest3'
    , 'JDfromStorageDynamicJump0_AfterJumpdest'
    , 'JDfromStorageDynamicJump0_foreverOutOfGas'
    , 'JDfromStorageDynamicJump0_jumpdest0'
    , 'JDfromStorageDynamicJump0_jumpdest2'
    , 'JDfromStorageDynamicJump0_withoutJumpdest'
    , 'JDfromStorageDynamicJump1'
    , 'JDfromStorageDynamicJumpi0'
    , 'JDfromStorageDynamicJumpi1'
    , 'JDfromStorageDynamicJumpi1_jumpdest'
    , 'JDfromStorageDynamicJumpiAfterStop'
    , 'JDfromStorageDynamicJumpifInsidePushWithJumpDest'
    , 'JDfromStorageDynamicJumpifInsidePushWithoutJumpDest'
    , 'JDfromStorageDynamicJumpInsidePushWithJumpDest'
    , 'JDfromStorageDynamicJumpInsidePushWithoutJumpDest'
    , 'JDfromStorageDynamicJumpiOutsideBoundary'
    , jump0_AfterJumpdest
    , jump0_AfterJumpdest3
    , jump0_foreverOutOfGas
    , jump0_jumpdest0
    , jump0_jumpdest2
    , 'jump0_outOfBoundary'
    , 'jump0_withoutJumpdest'
    , 'jump1'
    , 'jumpAfterStop'
    , 'jumpdestBigList'
    , 'jumpDynamicJumpSameDest'
    , 'jumpHigh'
    , 'jumpi0'
    , 'jumpi1'
    , 'jumpi1_jumpdest'
    , 'jumpiAfterStop'
    , 'jumpi_at_the_end'
    , 'jumpifInsidePushWithJumpDest'
    , 'jumpifInsidePushWithoutJumpDest'
    , 'jumpInsidePushWithJumpDest'
    , 'jumpInsidePushWithoutJumpDest'
    , 'jumpiOutsideBoundary'
    , 'jumpiToUint64maxPlus1'
    , 'jumpiToUintmaxPlus1'
    , 'jumpOntoJump'
    , 'jumpTo1InstructionafterJump'
    , 'jumpTo1InstructionafterJump_jumpdestFirstInstruction'
    , 'jumpTo1InstructionafterJump_noJumpDest'
    , 'jumpToUint64maxPlus1'
    , 'jumpToUintmaxPlus1'
    , 'kv1'
    , 'log1MemExp'
    , 'loop_stacklimit_1020'
    , 'loop_stacklimit_1021'
    , 'memory1'
    , 'mloadError0'
    , 'mloadError1'
    , 'mloadMemExp'
    , 'mloadOutOfGasError2'
    , msize0
    , msize1
    , msize2
    , msize3
    , 'mstore0'
    , 'mstore1'
    , 'mstore8_0'
    , 'mstore8_1'
    , 'mstore8MemExp'
    , 'mstore8WordToBigError'
    , 'mstoreMemExp'
    , 'mstore_mload0'
    , 'mstoreWordToBigError'
    , 'pc0'
    , 'pc1'
    , 'pop0'
    , 'pop1'
    , 'return1'
    , 'return2'
    , 'sha3MemExp'
    , 'sstore_load_0'
    , 'sstore_load_1'
    , 'sstore_load_2'
    , 'sstore_underflow'
    , 'stackjump1'
    , 'stack_loop'
    , 'swapAt52becameMstore'
    ].


%%====================================================================
%% VM Get Env Tests
%%====================================================================

vm_environmental_info_test_() ->
    Tests = vm_environmental_info_tests(),
    Path  = "VMTests/vmEnvironmentalInfo",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

vm_environmental_info_tests() ->
    [ address0
    , address1
    , balance0
    , balance1
    , balance01
    , balanceAddress2
    , balanceAddressInputTooBig
    , balanceAddressInputTooBigLeftMyAddress
    , balanceAddressInputTooBigRightMyAddress
    , balanceCaller3
    , calldatacopy0
    , calldatacopy0_return
    , calldatacopy1
    , calldatacopy1_return
    , calldatacopy2
    , calldatacopy2_return
    , calldatacopy_DataIndexTooHigh2
    , calldatacopy_DataIndexTooHigh2_return
    , calldatacopy_DataIndexTooHigh
    , calldatacopy_DataIndexTooHigh_return
    , calldatacopy_sec
    , calldatacopyUnderFlow
    , calldatacopyZeroMemExpansion
    , calldatacopyZeroMemExpansion_return
    , calldataload0
    , calldataload1
    , calldataload2
    , calldataload_BigOffset
    , calldataloadSizeTooHigh
    , calldataloadSizeTooHighPartial
    , calldatasize0
    , calldatasize1
    , calldatasize2
    , caller
    , callvalue
    , codecopy0
    , codecopy_DataIndexTooHigh
    , codecopyZeroMemExpansion
    , codesize
    %% , env1 %% TODO: Tobias: aevm_test_utils,build_config_lists,2
    , extcodecopy0AddressTooBigLeft
    , extcodecopy0AddressTooBigRight
    , extcodecopy0
    , extcodecopy_DataIndexTooHigh
    , extcodecopyZeroMemExpansion
    , extcodesize0
    , extcodesize1
    , 'ExtCodeSizeAddressInputTooBigLeftMyAddress'
    , 'ExtCodeSizeAddressInputTooBigRightMyAddress'
    , extcodesizeUnderFlow
    , gasprice
    , origin
    ].


%%====================================================================
%% VM Block Info Tests
%%====================================================================

vm_block_info_test_() ->
    Tests = vm_block_info_tests(),
    Path  = "VMTests/vmBlockInfoTest",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

vm_block_info_tests() ->
    [ blockhashInRange
    , blockhash257Block
    , blockhash258Block
    , blockhashInRange
    , blockhashMyBlock
    , blockhashNotExistingBlock
    , blockhashOutOfRange
    , blockhashUnderFlow
    , coinbase
    , difficulty
    , gaslimit
    , number
    , timestamp
    ].


%%====================================================================
%% VM Log Test
%%====================================================================

vm_log_test_() ->
    Tests = vm_log_tests(),
    Path  = "VMTests/vmLogTest",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

vm_log_tests() ->
    [ log0_nonEmptyMem
    , log1_nonEmptyMem_logMemSize1_logMemStart31
    , log2_nonEmptyMem_logMemSize1
    , log3_PC
    , log4_PC
      %% TODO: Actually test log results
      %% TODO add the rest of the tests
    ].

%%====================================================================
%% VM System Operations Tests
%%====================================================================

vm_system_operations_test_() ->
    Tests = vm_system_operations_tests(),
    Path  = "VMTests/vmSystemOperations",
    aevm_test_utils:testcase_generate(Path, Tests, fun extra_opts/1).

vm_system_operations_tests() ->
    [ %% createNameRegistrator %% TODO: Tobias: Env setup badmatch
      'ABAcalls0'
    , 'ABAcalls1'
    , 'ABAcalls2'
    , 'ABAcalls3'
    %%, 'ABAcallsSuicide0'
    , 'ABAcallsSuicide1'
    , 'CallRecursiveBomb0'
    %% , 'CallRecursiveBomb1'
    %% , 'CallRecursiveBomb2'
    %% , 'CallRecursiveBomb3'
    , callstatelessToNameRegistrator0
    , callstatelessToReturn1
    , 'CallToNameRegistrator0'
    , 'CallToNameRegistratorNotMuchMemory0'
    , 'CallToNameRegistratorNotMuchMemory1'
    , 'CallToNameRegistratorOutOfGas'
    , 'CallToNameRegistratorTooMuchMemory0'
    , 'CallToNameRegistratorTooMuchMemory1'
    , 'CallToNameRegistratorTooMuchMemory2'
    %%, 'CallToPrecompiledContract'
    , 'CallToReturn1'
    , 'PostToNameRegistrator0'
    , 'PostToReturn1'
    %%, suicideNotExistingAccount
    , callcodeToNameRegistrator0
    , callcodeToReturn1
    , callstatelessToReturn1
    , 'CallToReturn1'
    , 'PostToReturn1'
    ].

%%====================================================================
%% Internal functions
%%====================================================================

