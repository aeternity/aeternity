%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Call dispatcher for running contracts on the right VM.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_dispatch).

-include("aecontract.hrl").

%% API
-export([ call/4
	, encode_call_data/4
	, run/2]).

-define(PUB_SIZE, 32).

%% -- Running contract code off chain ---------------------------------------

%% TODO: replace language string with vm_version number.
call(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:simple_call(Code, Function, Argument);
call(<<"sophia-address">>, ContractKey, Function, Argument) ->
    aect_sophia:on_chain_call(ContractKey, Function, Argument);
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

-spec run(byte(), map()) -> {aect_call:call(), aec_trees:trees()}.
run(?AEVM_01_Sophia_01, CallDef) ->
    call_AEVM_01_Sophia_01(CallDef);
run(?AEVM_01_Solidity_01, CallDef) ->
    call_AEVM_01_Solidity_01(CallDef);
run(_, #{ call := Call} = _CallDef) ->
    %% TODO:
    %% Wrong VM/ABI version just return an unchanged call.
    Call.

call_AEVM_01_Sophia_01(#{ contract   := ContractPubKey
                        , height     := Height
                        , trees      := Trees
			} = CallDef) ->
    Env = set_env(ContractPubKey, Height, Trees, aec_vm_chain, ?AEVM_01_Sophia_01),
    Spec = #{ env => Env,
              exec => #{},
              pre => #{}},
    call_common(CallDef, Spec).

call_AEVM_01_Solidity_01(#{ contract   := ContractPubKey
                          , height     := Height
                          , trees      := Trees
                          } = CallDef) ->
    Env = set_env(ContractPubKey, Height, Trees, aec_vm_chain, ?AEVM_01_Solidity_01),
    Spec = #{ env => Env,
              exec => #{},
              pre => #{}},
    call_common(CallDef, Spec).

set_env(ContractPubKey, Height, Trees, API, VmVersion) ->
    ChainState = aec_vm_chain:new_state(Trees, Height, ContractPubKey),
    %% {ok, MinerPubkey} = aec_keys:pubkey(),
    #{currentCoinbase   => <<>>, %% MinerPubkey,
      %% TODO: get the right difficulty
      currentDifficulty => 0,
      %% TODO: implement gas limit in governance and blocks.
      currentGasLimit   => 100000000000,
      currentNumber     => Height,
      %% TODO: should be set before starting block candidate.
      currentTimestamp  => aeu_time:now_in_msecs(),
      chainState        => ChainState,
      chainAPI          => API,
      vm_version        => VmVersion}.

call_common(#{ caller     := Caller
             , contract   := ContractPubKey
             , gas        := Gas
             , gas_price  := GasPrice
             , call_data  := CallData
             , amount     := Value
             , call_stack := CallStack
             , code       := Code
             , call       := Call
             , height     :=_Height
             , trees      := Trees
             }, Spec) ->
    <<Address:?PUB_SIZE/unit:8>> = ContractPubKey,
    <<CallerAddr:?PUB_SIZE/unit:8>> = Caller,
    Exec = maps:get(exec, Spec),
    try aevm_eeevm_state:init(
	  Spec#{ exec => Exec#{ code       => Code,
                                address    => Address,
                                caller     => CallerAddr,
                                data       => CallData,
                                gas        => Gas,
                                gasPrice   => GasPrice,
                                origin     => CallerAddr,
                                value      => Value,
                                call_stack => CallStack
                              }
               },
          #{
            trace => false
           })
    of
	InitState ->
	    %% TODO: Nicer error handling - do more in check.
	    %% Update gas_used depending on exit type.
	    try aevm_eeevm:eval(InitState) of
		%% Succesful execution
		{ok, #{ gas := GasLeft
		      , out := ReturnValue
		      , chain_state := ChainState1
		      } =_State} ->
		    {aect_call:set_gas_used(
		       Gas - GasLeft,
		       aect_call:set_return_type(
			 ok,
			 aect_call:set_return_value(ReturnValue, Call))),
		     aec_vm_chain:get_trees(ChainState1)};
		{revert, #{ gas := GasLeft
			  , out := ReturnValue
			  , chain_state := ChainState1
			  } =_State} ->
		    {aect_call:set_gas_used(
		       Gas - GasLeft,
		       aect_call:set_return_type(
			 revert,
			 aect_call:set_return_value(ReturnValue, Call))),
		     aec_vm_chain:get_trees(ChainState1)};
		%% Execution resulting in VM exeception.
		%% Gas used, but other state not affected.
		%% TODO: Use up the right amount of gas depending on error
		%% TODO: Store errorcode in state tree
		{error, Error, #{ gas :=_GasLeft}} ->
		    {aect_call:set_gas_used(
		       Gas,
		       aect_call:set_return_type(
			 error,
			 aect_call:set_return_value(error_to_binary(Error), Call))), Trees}
	    catch T:E ->
                    lager:debug("Return error ~p:~p~n", [T,E]),
                    {aect_call:set_gas_used(
                       Gas,
                       aect_call:set_return_type(error, Call)),
                     Trees}
	    end
    catch T:E ->
            %% TODO: Clarify whether this case can be reached with valid chain state and sanitized input transaction.
            lager:debug("Init error ~p:~p~n", [T,E]),
            {aect_call:set_gas_used(
               Gas,
               aect_call:set_return_type(error, Call)),
             Trees}
    end.

error_to_binary(out_of_gas) -> <<"out_of_gas">>;
error_to_binary(out_of_stack) -> <<"out_of_stack">>;
error_to_binary(E) ->
    io:format("Unknown error: ~p\n", [E]),
    <<"unknown_error">>.
