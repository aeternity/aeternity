%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Call dispatcher for running contracts on the right VM.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_dispatch).

-include("aecontract.hrl").
-include("contract_txs.hrl").
-include_lib("apps/aecore/include/common.hrl").


%% API
-export([ call/4
	, encode_call_data/4
	, run_contract/4]).

-define(PUB_SIZE, 65).

-opaque tx() :: #{}.

-export_type([tx/0]).

call(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:simple_call(Code, Function, Argument);
call(<<"evm">>, Code, _, Argument) ->
    aect_evm:call(Code, Argument);
call(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.


encode_call_data(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:encode_call_data(Code, Function, Argument);
encode_call_data(<<"evm">>, Code, Function, Argument) ->
    aect_evm:encode_call_data(Code, Function, Argument);
encode_call_data(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.


%% -- Running contract code --------------------------------------------------


%% Call the contract and update the call object with the return value and gas
%% used.
-spec run_contract(tx(), aect_call:call(), height(), aec_trees:trees()) -> aect_call:call().
run_contract(#contract_call_tx{	caller = Caller
			      , nonce  = _Nonce
			      , contract = ContractPubKey
			      , vm_version = VmVersion
			      , amount     = Amount
			      , gas        = Gas 
			      , gas_price  = GasPrice
			      , call_data  = CallData
			      , call_stack = CallStack
			      } = _Tx, Call, Height, Trees) ->
    ContractsTree = aec_trees:contracts(Trees),
    Contract      = aect_state_tree:get_contract(ContractPubKey, ContractsTree),
    Code          = aect_contracts:code(Contract),
    CallDef = #{ caller     => Caller
	       , contract   => ContractPubKey
	       , gas        => Gas
	       , gas_price  => GasPrice
	       , call_data  => CallData
	       , amount     => Amount
	       , call_stack => CallStack
	       , code       => Code
	       , call       => Call
	       , height     => Height
	       , trees      => Trees
	       },
    run(VmVersion, CallDef);
run_contract(#contract_create_tx{ owner      = Caller
				, nonce      = Nonce
				, code       = Code
				, vm_version = VmVersion
				, amount     = Amount
				, gas        = Gas 
				, gas_price  = GasPrice
				, call_data  = CallData
				} = _Tx, Call, Height, Trees) ->
    ContractPubKey = aect_contracts:compute_contract_pubkey(Caller, Nonce),
    ContractsTree = aec_trees:contracts(Trees),
    Contract      = aect_state_tree:get_contract(ContractPubKey, ContractsTree),
    Code          = aect_contracts:code(Contract),
    CallStack = [], %% TODO: should we have a call stack for create_tx also
                    %% when creating a contract in a contract.

    CallDef = #{ caller     => Caller
	       , contract   => ContractPubKey
	       , gas        => Gas
	       , gas_price  => GasPrice
	       , call_data  => CallData
	       , amount     => Amount
	       , call_stack => CallStack
	       , code       => Code
	       , call       => Call
	       , height     => Height
	       , trees      => Trees
	       },
    run(VmVersion, CallDef).



run(?AEVM_01_Sophia_01, CallDef) ->
    call_AEVM_01_Sophia_01(CallDef);
run(?AEVM_01_Solidity_01, CallDef) ->
    %% TODO:
    %% For now use the same ABI as for Sophia
    call_AEVM_01_Sophia_01(CallDef);
run(_, #{ call := Call} = _CallDef) ->
    %% TODO:
    %% Wrong VM/ABI version just return an unchanged call.
    Call.


call_AEVM_01_Sophia_01(#{ caller     := Caller
			, contract   := ContractPubKey
			, gas        := Gas
			, gas_price  := GasPrice
			, call_data  := CallData
			, amount     := Value
			, call_stack := CallStack
			, code       := Code
			, call       := Call
			, height     := Height
			, trees      := Trees
			}) ->

    ChainState = aec_vm_chain:new_state(Trees, Height, ContractPubKey),
    <<Address:?PUB_SIZE/unit:8>> = ContractPubKey,
    try aevm_eeevm_state:init(
	  #{ exec => #{ code       => Code,
			address    => Address,
			caller     => Caller,
			data       => CallData,
			gas        => Gas,
			gasPrice   => GasPrice,
			origin     => Caller,
			value      => Value,
                        call_stack => CallStack },
             %% TODO: set up the env properly
             env => #{currentCoinbase   => 0,
                      currentDifficulty => 0,
                      currentGasLimit   => Gas,
                      currentNumber     => Height,
                      currentTimestamp  => 0,
                      chainState        => ChainState,
                      chainAPI          => aec_vm_chain},
             pre => #{}},
          #{trace => false})
    of
	InitState ->
	    %% TODO: Nicer error handling - do more in check.
	    %% Update gas_used depending on exit type.x
	    try aevm_eeevm:eval(InitState) of
		%% Succesful execution
		{ok, #{ gas := GasLeft, out := ReturnValue }} ->
		    aect_call:set_gas_used(Gas - GasLeft,
					   aect_call:set_return_value(ReturnValue, Call));
		%% Executinon reulting in VM exeception.
		%% Gas used, but other state not affected.
		%% TODO: Use up the right amount of gas depending on error
		%% TODO: Store errorcode in state tree
		{error,_Error, #{ gas :=_GasLeft}} ->
		    aect_call:set_gas_used(Gas,
					   aect_call:set_return_value(<<>>, Call))

	    catch _:_ -> Call
	    end
    catch _:_ ->
	    Call
    end.

