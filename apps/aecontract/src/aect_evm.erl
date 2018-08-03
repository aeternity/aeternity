%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Solidity contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_evm).

-export([ call/2
        , encode_call_data/3
        , execute_call/2
        ]).

-include("aecontract.hrl").


-spec encode_call_data(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
encode_call_data(_Contract, Function, Argument) ->
    %% TODO: Check that Function exists in Contract.
    {ok, <<Function/binary, Argument/binary>>}.

-spec call(binary(), binary()) -> {ok, binary()} | {error, binary()}.
call(Code, CallData) ->
    Data = aeu_hex:hexstring_decode(CallData),

    %% TODO: proper setup of chain state!
    Owner = <<123456:32/unit:8>>,
    {Block, Trees} = aec_chain:top_block_with_state(),
    BlockHeight = aec_blocks:height(Block) + 1,
    Amount = 0,
    VmVersion = ?AEVM_01_Solidity_01,
    Deposit = 0,
    Contract = aect_contracts:new(Owner, 1, VmVersion, Code, Deposit),
    DummyPubKey = aect_contracts:pubkey(Contract),
    Trees1 = insert_contract(Contract, Trees),
    ChainState  = aec_vm_chain:new_state(Trees1, BlockHeight, DummyPubKey),
    Spec = #{ code => Code
            , address => 1 %% Address 0 is for primcalls
            , caller => 0
            , data => Data
            , gas => 1000000000000000000000000
            , gasPrice => 1
            , origin => 0
            , value => Amount
            , currentCoinbase => 1
            , currentDifficulty => 1
            , currentGasLimit => 10000000000000000000000
            , currentNumber => 1
            , currentTimestamp => 1
            , chainState => ChainState
            , chainAPI => aec_vm_chain
            , vm_version => VmVersion
            },
    try execute_call(Spec, true) of
        {ok, #{ out := Out }} -> {ok, aeu_hex:hexstring_encode(Out)};
        E -> {error, list_to_binary(io_lib:format("~p", [E]))}
    catch _T:E ->
	{error, list_to_binary(io_lib:format("~p",
                                             [{E, erlang:get_stacktrace()}]))}
    end.

insert_contract(Contract, Trees) ->
    CTrees = aec_trees:contracts(Trees),
    CTrees1 = aect_state_tree:insert_contract(Contract, CTrees),
    aec_trees:set_contracts(Trees, CTrees1).


-spec execute_call(map(), boolean()) -> {ok, map()} | {error, term()}.
execute_call(#{ code := CodeAsHexBinString
              , address := Address
              , caller := Caller
              , data := CallData
              , gas := Gas
              , gasPrice := GasPrice
              , origin := Origin
              , value := Value
              , currentCoinbase := CoinBase
              , currentDifficulty := Diffculty
              , currentGasLimit := GasLimit
              , currentNumber := Number
              , currentTimestamp := TS
              , chainState := ChainState
              , chainAPI := ChainAPI
              , vm_version := VmVersion
              }, Trace) ->
    %% TODO: Handle Contract In State.
    Code = aeu_hex:hexstring_decode(CodeAsHexBinString),
    Spec =
        #{ exec => #{ code => Code
                    , address => Address
                    , caller => Caller
                    , data => CallData
                    , gas => Gas
                    , gasPrice => GasPrice
                    , origin => Origin
                    , value => Value
                    },
           env => #{ currentCoinbase => CoinBase
                   , currentDifficulty => Diffculty
                   , currentGasLimit => GasLimit
                   , currentNumber => Number
                   , currentTimestamp => TS
                   , chainState => ChainState
                   , chainAPI => ChainAPI
                   , vm_version => VmVersion
                   },
           pre => #{}},
    TraceSpec =
        #{ trace_fun =>
               fun(S,A) -> lager:debug(S,A) end
         , trace => Trace
         },
    State = aevm_eeevm_state:init(Spec, TraceSpec),
    Result = aevm_eeevm:eval(State),
    Result.
