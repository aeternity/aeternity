%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Call dispatcher for running contracts on the right VM.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_dispatch).

-include_lib("apps/aecore/include/blocks.hrl").
-include("aecontract.hrl").

%% API
-export([ call/4
        , encode_call_data/4
        , run/2]).

-define(PUB_SIZE, 32).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-define(DEBUG_LOG(Format, Data), begin lager:debug(Format, Data), ?TEST_LOG(Format, Data) end).
-else.
-define(TEST_LOG(Format, Data), ok).
-define(DEBUG_LOG(Format, Data), lager:debug(Format, Data)).
-endif.

%% -- Running contract code off chain ---------------------------------------

%% TODO: replace language string with vm_version number.
call(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:simple_call(Code, Function, Argument);
call(<<"sophia-address">>, ContractKey, Function, Argument) ->
    aect_sophia:on_chain_call(ContractKey, Function, Argument);
call(<<"evm">>, Code, _, Argument) ->
    aect_evm:simple_call_solidity(Code, Argument);
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
    run_common(CallDef, ?AEVM_01_Sophia_01);
run(?AEVM_01_Solidity_01, CallDef) ->
    run_common(CallDef, ?AEVM_01_Solidity_01);
run(_, #{ call := Call} = _CallDef) ->
    %% TODO:
    %% Wrong VM/ABI version just return an unchanged call.
    Call.

run_common(#{  amount      := Value
             , beneficiary := <<Beneficiary:?BENEFICIARY_PUB_BYTES/unit:8>> = BeneficiaryBin
             , call        := Call
             , call_data   := CallData
             , call_stack  := CallStack
             , caller      := <<CallerAddr:?PUB_SIZE/unit:8>>
             , code        := Code
             , contract    := <<Address:?PUB_SIZE/unit:8>> = ContractPubKey
             , difficulty  := Difficulty
             , gas         := Gas
             , gas_price   := GasPrice
             , height      := Height
             , time        := Time
             , trees       := Trees
             }, VMVersion) ->
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    TxEnv = aetx_env:contract_env(Height, ConsensusVersion, Time,
                                  BeneficiaryBin, Difficulty),
    ChainState0 = aec_vm_chain:new_state(Trees, TxEnv, ContractPubKey),
    Env = #{currentCoinbase   => Beneficiary,
            currentDifficulty => Difficulty,
            %% TODO: implement gas limit in governance and blocks.
            currentGasLimit   => 100000000000,
            currentNumber     => Height,
            currentTimestamp  => Time,
            chainState        => ChainState0,
            chainAPI          => aec_vm_chain,
            vm_version        => VMVersion},
    Exec = #{code       => Code,
             address    => Address,
             caller     => CallerAddr,
             data       => CallData,
             gas        => Gas,
             gasPrice   => GasPrice,
             origin     => CallerAddr,
             value      => Value,
             call_stack => CallStack
            },
    Spec = #{ env => Env,
              exec => Exec,
              pre => #{}},

    try aevm_eeevm_state:init(Spec, #{trace => false}) of
        InitState ->
            %% TODO: Nicer error handling - do more in check.
            %% Update gas_used depending on exit type.
            try aevm_eeevm:eval(InitState) of
                {ok, ResultState} ->
                    GasLeft = aevm_eeevm_state:gas(ResultState),
                    GasUsed = Gas - GasLeft,
                    Out = aevm_eeevm_state:out(ResultState),
                    ChainState = aevm_eeevm_state:chain_state(ResultState),
                    Log = aevm_eeevm_state:logs(ResultState),
                    {
                      create_call(GasUsed, ok, Out, Log, Call),
                      aec_vm_chain:get_trees(ChainState)
                    };
                {revert, ResultState} ->
                    GasLeft = aevm_eeevm_state:gas(ResultState),
                    GasUsed = Gas - GasLeft,
                    Out = aevm_eeevm_state:out(ResultState),
                    ChainState = aevm_eeevm_state:chain_state(ResultState),
                    Log = aevm_eeevm_state:logs(ResultState),
                    {
                        create_call(GasUsed, revert, Out, Log, Call),
                        aec_vm_chain:get_trees(ChainState)
                    };
                {error, Error, _} ->
                    %% Execution resulting in VM exception.
                    %% Gas used, but other state not affected.
                    %% TODO: Use up the right amount of gas depending on error
                    GasUsed = Gas,
                    %% TODO: Store error code in state tree
                    {create_call(GasUsed, error, Error, [], Call), Trees}
            catch T:E ->
                ?DEBUG_LOG("Return error ~p:~p~n", [T,E]),
                {create_call(Gas, error, [], Call), Trees}
            end
    catch T:E ->
            %% TODO: Clarify whether this case can be reached with valid chain state and sanitized input transaction.
            ?DEBUG_LOG("Init error ~p:~p~n", [T,E]),
            {create_call(Gas, error, [], Call), Trees}
    end.

create_call(GasUsed, Type, Value, Log, Call) when Type == ok; Type == revert ->
    Call1 = aect_call:set_return_value(Value, Call),
    create_call(GasUsed, Type, Log, Call1);
create_call(GasUsed, error = Type, Value, Log, Call)  ->
    Call1 = aect_call:set_return_value(error_to_binary(Value), Call),
    create_call(GasUsed, Type, Log, Call1).

create_call(GasUsed, Type, Log, Call) ->
    Call1 = aect_call:set_log(Log, Call),
    aect_call:set_gas_used(GasUsed, aect_call:set_return_type(Type, Call1)).

error_to_binary(out_of_gas) -> <<"out_of_gas">>;
error_to_binary(out_of_stack) -> <<"out_of_stack">>;
error_to_binary(E) ->
    ?DEBUG_LOG("Unknown error: ~p\n", [E]),
    <<"unknown_error">>.
