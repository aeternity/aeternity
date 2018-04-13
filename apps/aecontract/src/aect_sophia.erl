%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Sophia contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_sophia).

-export([ compile/2
        , create_call/3
        , encode_call_data/3
        , simple_call/3
        ]).

-spec compile(binary(), binary()) -> {ok, binary()} | {error, binary()}.

compile(ContractAsBinString, OptionsAsBinString) ->
    ContractText = binary_to_list(ContractAsBinString),
    Options = parse_options(OptionsAsBinString),
    try aeso_compiler:from_string(ContractText, Options) of
        %% TODO: Handle contract meta data.
        Code -> {ok, aeu_hex:hexstring_encode(Code)}
    catch error:Error ->
            {error, list_to_binary(io_lib:format("~p", [Error]))}
    end.

parse_options(OptionsBinString) ->
    parse_options(OptionsBinString, []).

parse_options(<<" ", Rest/binary>>, Acc) ->
    parse_options(Rest, Acc);
parse_options(<<"pp_sophia_code", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_sophia_code | Acc]);
parse_options(<<"pp_ast", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_ast | Acc]);
parse_options(<<"pp_icode", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_icode | Acc]);
parse_options(<<"pp_assembler", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_assembler | Acc]);
parse_options(<<"pp_bytecode", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_bytecode | Acc]);
parse_options(<<_:8, Rest/binary>>, Acc) ->
    %% TODO: give nice error instead of just ignoring stray chars.
    parse_options(Rest, Acc);
parse_options(<<>>, Acc) -> Acc.



-spec simple_call(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
simple_call(Code, Function, Argument) ->
    case create_call(Code, Function, Argument) of
        {error, E} -> {error, E};
        CallData ->
            %% TODO: proper setup of chain state!
            Owner = <<123456:65/unit:8>>,
            DummyPubKey = aect_contracts:compute_contract_pubkey(Owner, 1),
            {Block, Trees} = aec_chain:top_block_with_state(),
            BlockHeight = aec_blocks:height(Block) + 1,
            Amount = 0,
            VmVersion = 1,
            Deposit = 0,
            Contract = aect_contracts:new(DummyPubKey, BlockHeight, Amount,
                                          Owner, VmVersion, Code, Deposit),
            Trees1 = insert_contract(Contract, Trees),
            ChainState  = aec_vm_chain:new_state(Trees1, BlockHeight, DummyPubKey),
            Spec = #{ code => Code
                    , address => 0
                    , caller => 0
                    , data => CallData
                    , gas => 1000000
                    , gasPrice => 1
                    , origin => 0
                    , value => Amount
                    , currentCoinbase => 1
                    , currentDifficulty => 1
                    , currentGasLimit => 1000000
                    , currentNumber => 1
                    , currentTimestamp => 1
                    , chainAPI => aec_vm_chain
                    , chainState => ChainState
                    },
            case aect_evm:execute_call(Spec, false) of
                {ok, #{ out := Out }} ->
                    {ok, aeu_hex:hexstring_encode(Out)};
                E -> {error, list_to_binary(io_lib:format("~p", [E]))}
            end
    end.

insert_contract(Contract, Trees) ->
    CTrees = aec_trees:contracts(Trees),
    CTrees1 = aect_state_tree:insert_contract(Contract, CTrees),
    aec_trees:set_contracts(Trees, CTrees1).

-spec encode_call_data(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
encode_call_data(Contract, Function, Argument) ->
    try create_call(Contract, Function, Argument) of
        Data when is_binary(Data) ->
            {ok, aeu_hex:hexstring_encode(Data)};
        Error -> Error
    catch _:_ -> {error, <<"bad argument">>}
    end.

-spec create_call(binary(), binary(), binary()) -> binary() | {error, binary()}.
create_call(Contract, Function, Argument) ->
    Res = aeso_abi:create_calldata(Contract,
                                   binary_to_list(Function),
                                   binary_to_list(Argument)),
    case Res of
        {error, Error} ->
            {error, list_to_binary(io_lib:format("~p", [Error]))};
        _ -> Res
    end.

