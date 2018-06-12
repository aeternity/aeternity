%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Sophia contracts and data.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_sophia).

-include("aecontract.hrl").

-export([ compile/2
        , create_call/3
        , decode_data/2
        , encode_call_data/3
        , simple_call/3
        , on_chain_call/3
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
            Owner = <<123456:32/unit:8>>,
            {Block, Trees} = aec_chain:top_block_with_state(),
            BlockHeight = aec_blocks:height(Block) + 1,
            Amount = 0,
            VmVersion = ?AEVM_01_Sophia_01,
            Deposit = 0,
            Contract = aect_contracts:new(Owner, 1, VmVersion, Code, Deposit),
            DummyPubKey = aect_contracts:pubkey(Contract),
            Trees1 = insert_contract(Contract, Trees),
            ChainState  = aec_vm_chain:new_state(Trees1, BlockHeight, DummyPubKey),
            Spec = #{ code => Code
                    , address => 1 %% Address 0 is for primcals.
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
                    , vm_version => VmVersion
                    },
            case aect_evm:execute_call(Spec, true) of
                {ok, #{ out := Out } = _RetState} ->
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


decode_data(Type, Data) ->
    case get_type(Type) of
        {ok, SophiaType} ->
            try aeso_data:from_binary(32, SophiaType,
                                      aeu_hex:hexstring_decode(Data)) of
                {ok, Term} ->
                    try prepare_for_json(SophiaType, Term) of
                        R -> {ok, R}
                    catch throw:R -> R
                    end;
                {error, _} -> {error, <<"bad type/data">>}
            catch _T:_E ->    {error, <<"bad argument">>}
            end;
        {error, _} = E -> E
    end.

get_type(BinaryString) ->
    String = unicode:characters_to_list(BinaryString, utf8),
    case aeso_data:sophia_type_to_typerep(String) of
        {ok, _Type} = R -> R;
        {error, ErrorAtom} ->
            {error, unicode:characters_to_binary(atom_to_list(ErrorAtom))}
    end.


prepare_for_json(word, Integer) when is_integer(Integer) ->
    #{ <<"type">> => <<"word">>,
       <<"value">> => Integer};
prepare_for_json(string, String) when is_binary(String) ->
    #{ <<"type">> => <<"string">>,
       <<"value">> => String};
prepare_for_json({option, _T}, none) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => <<"None">>};
prepare_for_json({option, T}, {some, E}) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => prepare_for_json(T,E) };
prepare_for_json({tuple, Ts}, Es) ->
    #{ <<"type">> => <<"tuple">>,
       <<"value">> => [prepare_for_json(T,E)
                       || {T,E} <-
                              lists:zip(Ts, tuple_to_list(Es))] };
prepare_for_json({list, T}, Es) ->
    #{ <<"type">> => <<"list">>,
       <<"value">> => [prepare_for_json(T,E) || E <- Es]};
prepare_for_json(T, R) ->
    String = io_lib:format("Type: ~p Res:~p", [T,R]),
    Error = << <<B>> || B <- "Invalid Sophia type: " ++ lists:flatten(String) >>,
    throw({error, Error}).

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


-spec on_chain_call(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
on_chain_call(ContractKey, Function, Argument) ->
    {Block, Trees} = aec_chain:top_block_with_state(),
    ContractsTree  = aec_trees:contracts(Trees),
    Contract       = aect_state_tree:get_contract(ContractKey, ContractsTree),
    Code           = aect_contracts:code(Contract),
    <<Address:256>> = ContractKey,
    case create_call(Code, Function, Argument) of
        {error, E} -> {error, E};
        CallData ->
            BlockHeight = aec_blocks:height(Block),
            Amount = 0,
            VmVersion = ?AEVM_01_Sophia_01,
            ChainState  = aec_vm_chain:new_state(Trees, BlockHeight, ContractKey),
            Spec = #{ code => aeu_hex:hexstring_encode(Code)
                    , address => Address
                    , caller => 0
                    , data => CallData
                    , gas => 100000000000000000
                    , gasPrice => 1
                    , origin => 0
                    , value => Amount
		      %% TODO:
                    , currentCoinbase => 1
                    , currentDifficulty => 1
                    , currentGasLimit => 1000000
                    , currentNumber => 1
                    , currentTimestamp => 1
                    , chainAPI => aec_vm_chain
                    , chainState => ChainState
                    , vm_version => VmVersion
                    },
            case aect_evm:execute_call(Spec, true) of
                {ok, #{ out := Out } = _RetState} ->
                    {ok, aeu_hex:hexstring_encode(Out)};
                E -> {error, list_to_binary(io_lib:format("~p", [E]))}
            end
    end.
