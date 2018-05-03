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
	, run/2]).

-define(PUB_SIZE, 32).

%% -- Running contract code off chain ---------------------------------------

%% TODO: replace language string with vm_version number.
call(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:simple_call(Code, Function, Argument);
call(<<"evm">>, Code, _, Argument) ->
    aect_evm:call(Code, Argument);
call(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.

%% TODO: replace language string with vm_version number.

encode_call_data(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:encode_call_data(Code, Function, Argument);
encode_call_data(<<"evm">>, Code, Function, Argument) ->
    aect_evm:encode_call_data(Code, Function, Argument);
encode_call_data(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.


%% -- Running contract code on chain ---------------------------------------

%% Call the contract and update the call object with the return value and gas
%% used.

-spec run(byte(), map()) -> aect_call:call().
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
                        call_stack => CallStack
		      },
             %% TODO: set up the env properly
             env => #{currentCoinbase   => 0,
                      currentDifficulty => 0,
                      currentGasLimit   => Gas,
                      currentNumber     => Height,
                      currentTimestamp  => 0,
                      chainState        => ChainState,
                      chainAPI          => aec_vm_chain},
             pre => #{}},
          #{
	     trace_fun  => fun(S,A) -> lager:error(S,A) end,
	     trace => false
	     })
    of
	InitState ->

	    %% TODO: Nicer error handling - do more in check.
	    %% Update gas_used depending on exit type.x
	    try aevm_eeevm:eval(InitState) of
		%% Succesful execution
		{ok, #{ gas := GasLeft, out := ReturnValue } =_State} ->
		    aect_call:set_gas_used(
		      Gas - GasLeft,
		      aect_call:set_return_type(
			ok,
			aect_call:set_return_value(ReturnValue, Call)));
		{revert, #{ gas := GasLeft, out := ReturnValue } = State} ->
		    lager:error("Return state ~p~n", [State]),
		    aect_call:set_gas_used(
		      Gas - GasLeft,
		      aect_call:set_return_type(
			revert,
			aect_call:set_return_value(ReturnValue, Call)));
		%% Execution resulting in VM exeception.
		%% Gas used, but other state not affected.
		%% TODO: Use up the right amount of gas depending on error
		%% TODO: Store errorcode in state tree
		{error, Error, #{ gas :=_GasLeft}} ->
		    lager:error("Return error ~p:~p~n", [error, Error]),
		    aect_call:set_gas_used(
		      Gas,
		      aect_call:set_return_type(
			error,
			aect_call:set_return_value(error_to_binary(Error), Call)))
	    catch T:E ->
		    lager:error("Return error ~p:~p~n", [T,E]),
		    aect_call:set_return_type(error, Call)
	    end
    catch T:E ->
	    lager:error("Return error ~p:~p~n", [T,E]),
	    aect_call:set_return_type(error, Call)
    end.

error_to_binary(out_of_gas) -> <<"out_of_gas">>.
