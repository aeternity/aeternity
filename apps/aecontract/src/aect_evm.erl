%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Solidity contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_evm).

-export([ simple_call_solidity/2
        , simple_call_common/3
        , call_common/6
        , encode_call_data/3
        , execute_call/2
        ]).

-include("aecontract.hrl").
-include_lib("apps/aecore/include/blocks.hrl").


-spec encode_call_data(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
encode_call_data(_Contract, Function, Argument) ->
    %% TODO: Check that Function exists in Contract.
    {ok, <<Function/binary, Argument/binary>>}.

-spec simple_call_solidity(binary(), binary()) -> {ok, binary()} | {error, binary()}.
simple_call_solidity(EncodedCode, EncodedCallData) ->
    CallData       = aeu_hex:hexstring_decode(EncodedCallData),
    simple_call_common(EncodedCode, CallData, ?AEVM_01_Solidity_01).

-spec simple_call_common(binary(), binary(), VMVersion :: integer()) -> {ok, binary()} | {error, binary()}.
simple_call_common(EncodedCode, CallData, VMVersion) ->
    {Block, Trees} = aec_chain:top_block_with_state(),
    Owner          = <<123456:32/unit:8>>,
    Deposit        = 0,
    Code           = aeu_hex:hexstring_decode(EncodedCode),
    Contract       = aect_contracts:new(Owner, 1, VMVersion, Code, Deposit),
    ContractKey    = aect_contracts:pubkey(Contract),
    Trees1         = aect_utils:insert_contract_in_trees(Contract, Trees),
    call_common(CallData, ContractKey, EncodedCode, Block, Trees1, VMVersion).

-spec call_common(binary(), binary(), binary(), aec_blocks:block(),
                  aec_trees:trees(), VMVersion :: integer()) ->
                     {ok, binary()} | {error, binary()}.
call_common(CallData, ContractKey, EncodedCode, Block, Trees, VMVersion) ->
    <<Address:256>> = ContractKey,
    Time = aeu_time:now_in_msecs(),
    {KeyBlockHash, KeyBlock} = get_key_block(Block),
    {BeneficiaryBin, BeneficiaryInt} = get_beneficiary(KeyBlock),
    Difficulty = aec_blocks:difficulty(KeyBlock),
    BlockHeight = aec_blocks:height(Block),
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(BlockHeight),
    GasLimit = aec_governance:block_gas_limit(),
    Amount = 0,
    TxEnv = aetx_env:contract_env(BlockHeight, ConsensusVersion, Time,
                                  BeneficiaryBin, Difficulty, KeyBlockHash),
    ChainState = aec_vm_chain:new_state(Trees, TxEnv, ContractKey),
    Spec = #{ code => EncodedCode
            , address => Address
            , caller => 0
            , data => CallData
            , gas => 100000000000000000
            , gasPrice => 1
            , origin => 0
            , value => Amount
            , currentCoinbase => BeneficiaryInt
            , currentDifficulty => Difficulty
            , currentGasLimit => GasLimit
            , currentNumber => BlockHeight
            , currentTimestamp => Time
            , chainAPI => aec_vm_chain
            , chainState => ChainState
            , vm_version => VMVersion
            },
    try execute_call(Spec, true) of
        {ok, #{ out := Out } = _RetState} ->
            {ok, aeu_hex:hexstring_encode(Out)};
        E -> {error, list_to_binary(io_lib:format("~p", [E]))}
    catch _T:E ->
            ErrorString = io_lib:format("~p", [{E, erlang:get_stacktrace()}]),
            {error, list_to_binary(ErrorString)}
    end.

get_key_block(Block) ->
    case aec_blocks:type(Block) of
        key   ->
            {ok, Hash} = aec_blocks:hash_internal_representation(Block),
            {Hash, Block};
        micro ->
            Hash = aec_blocks:prev_key_hash(Block),
            {ok, KB} = aec_chain:get_block(Hash),
            {Hash, KB}
    end.

get_beneficiary(KeyBlock) ->
    BeneficiaryBin = aec_blocks:beneficiary(KeyBlock),
    <<BeneficiaryInt:?BENEFICIARY_PUB_BYTES/unit:8>> = BeneficiaryBin,
    {BeneficiaryBin, BeneficiaryInt}.


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
              , currentDifficulty := Difficulty
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
                   , currentDifficulty => Difficulty
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
