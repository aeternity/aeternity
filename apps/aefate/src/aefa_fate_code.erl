-module(aefa_fate_code).

-export([ encode_calldata/3
        , decode_result/3 ]).

-define(FD, aeb_fate_data).

encode_calldata(FCode, Fun, Args) when is_binary(Fun), is_list(Args) ->
    {InTypes, _} = type_sig(Fun, FCode),
    aeb_fate_abi:create_calldata(binary_to_list(Fun),
                                 encode_args(lists:zip(InTypes, Args))).

decode_result(FCode, Fun, Result) ->
    {_, RetType} = type_sig(Fun, FCode),
    {RetType, aeb_fate_encoding:deserialize(Result)}.

type_sig(Fun, FCode) ->
    Id = aeb_fate_code:symbol_identifier(Fun),
    Funs = aeb_fate_code:functions(FCode),
    {_, {_, _} = Types, _} = maps:get(Id, Funs),
    Types.

encode_args(TypeArgs) ->
    lists:map(fun encode_arg/1, TypeArgs).

%% TODO: Go through aefa_test_utils:encode/1, and include all variants
%% that are reasonable to support here. Special-case the others in
%% aefa_test_utils, then have it call this for the rest.
encode_arg({Type, Val}) ->
    case Type of
        integer    when is_integer(Val)      -> aeb_fate_data:make_integer(Val);
        string     when is_binary(Val)       -> aeb_fate_data:make_string(Val);
        {bytes,S}  when byte_size(Val) =:= S -> aeb_fate_data:make_bytes(Val);
        bits       when is_integer(Val)      -> aeb_fate_data:make_bits(Val);
        hash       when is_binary(Val)       -> aeb_fate_data:make_hash(Val);
        bool       when is_boolean(Val)      -> aeb_fate_data:make_boolean(Val);
        {tuple,Es} when is_tuple(Val) -> make_tuple(Es, Val);
        address   -> make_address(Val);
        contract  -> make_contract(Val)
    end.

make_address(<<"ak_", _/binary>> = A) ->
    aeb_fate_data:make_address(aeser_api_decode(account_pubkey, A));
make_address(I) when is_integer(I) ->
    B = <<I:256>>,
    aeb_fate_data:make_address(B);
make_address(B) when bit_size(B) == 256 ->
    aeb_fate_data:make_address(B);
make_address(Id) when element(1, Id) =:= id ->
    aeb_fate_data:make_address(encode_id(account, Id)).

make_contract(<<"ct_", _/binary>> = C) ->
    aeb_fate_data:make_contract(encode_address(contract_pubkey, C));
make_contract("ct_" ++ _ = C) ->
    aeb_fate_data:make_contract(encode_address(contract_pubkey, C));
make_contract(C) when bit_size(C) == 256 ->
    aeb_fate_data:make_contract(C);
make_contract(I) when is_integer(I) ->
    aeb_fate_data:make_contract(<<I:256>>).

make_signature(<<"sg_", _/binary>> = S) ->
    aeb_fate_data:make_signature(encode_address(signature, S));
make_signature("sg_" ++ _ = S) ->
    aeb_fate_data:make_signature(encode_address(signature, S));
make_signature(B) when bit_size(B) =:= 256 ->
    aeb_fate_data:make_signature(B).

encode_address(Type, S) when is_list(S); is_binary(S) ->
    B = iolist_to_binary(S),
    try aeser_api_encoder:decode(B) of
        {Type, Encoding} ->
            Encoding;
        _ -> erlang:error({bad_address_encoding, Type, S})
    catch _:_ ->
            erlang:error({bad_address_encoding, Type, S})
    end.

encode_id(Type, Id) ->
    case aeser_id:specialize(Id) of
        {Type, K}  -> K;
        {Other, Val} ->
            erlang:error({bad_id_type, Other, Val})
    end.


make_tuple(Es, Val) ->
    aeb_fate_data:make_tuple(list_to_tuple(
                               encode_args(lists:zip(Es, tuple_to_list(Val)))
                               )).

aeser_api_decode(Type, K) ->
    {Type, Res} = aeser_api_encoder:decode(K),
    Res.
