-module(aefa_fate_code).

-export([ encode_calldata/3
        , encode_arg/1
        , decode_result/3 ]).

-define(FD, aeb_fate_data).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

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
encode_arg(none)      -> aeb_fate_data:make_variant([0, 1], 0, {});
encode_arg({some, X}) -> aeb_fate_data:make_variant([0, 1], 1, {encode_arg(X)});
encode_arg({integer, Val})    when is_integer(Val) -> aeb_fate_data:make_integer(Val);
encode_arg({string, Val})     when is_binary(Val)  -> aeb_fate_data:make_string(Val);
encode_arg({{bytes,S}, Val})  when byte_size(Val) =:= S -> aeb_fate_data:make_bytes(Val);
encode_arg({bytes, Val})      when is_binary(Val)       -> aeb_fate_data:make_bytes(Val);
encode_arg({bits, Val})       when is_integer(Val)      -> aeb_fate_data:make_bits(Val);
encode_arg({hash, Val})       when is_binary(Val)       -> aeb_fate_data:make_hash(Val);
encode_arg({bool, Val})       when is_boolean(Val)      -> aeb_fate_data:make_boolean(Val);
encode_arg({{tuple,Es}, Val}) when is_tuple(Val)        -> make_tuple(Es, Val);
encode_arg({address, Val})      -> make_address(Val);
encode_arg({contract, Val})     -> make_contract(Val);
encode_arg({signature, Val})    -> make_signature(Val);
encode_arg({oracle, Val})       -> make_oracle(Val);
encode_arg({oracle_query, Val}) -> make_oracle_query(Val);
encode_arg({channel, Val})      -> make_channel(Val);
encode_arg({variant, Arities, Tag, Values}) ->
    aeb_fate_data:make_variant(Arities, Tag,
                               list_to_tuple([encode_arg(V)
                                              || V <- tuple_to_list(Values)]));
encode_arg(Term) when is_integer(Term) -> aeb_fate_data:make_integer(Term);
encode_arg(Term) when is_boolean(Term) -> aeb_fate_data:make_boolean(Term);
encode_arg(Term) when is_list(Term) -> aeb_fate_data:make_list([encode_arg(E) || E <- Term]);
encode_arg(Term) when is_tuple(Term) ->
    aeb_fate_data:make_tuple(list_to_tuple([encode_arg(E)
                                            || E <- erlang:tuple_to_list(Term)]));
encode_arg(Term) when is_map(Term) ->
    aeb_fate_data:make_map(maps:from_list([{encode_arg(K), encode_arg(V)}
                                           || {K,V} <- maps:to_list(Term)]));
encode_arg(Term) when is_binary(Term) -> aeb_fate_data:make_string(Term).

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

make_oracle(<<"ok_", _/binary>> = O) ->
    aeb_fate_data:make_oracle(encode_address(oracle_pubkey, O));
make_oracle("ok_" ++ _ = O) ->
    aeb_fate_data:make_oracle(encode_address(oracle_pubkey, O));
make_oracle(O) when is_binary(O) ->
    aeb_fate_data:make_oracle(O);
make_oracle(I) when is_integer(I) ->
    aeb_fate_data:make_oracle(<<I:256>>);
make_oracle(B) when bit_size(B) =:= 256 ->
    aeb_fate_data:make_oracle(B);
make_oracle(Id) when element(1, Id) =:= id ->
    aeb_fate_data:make_oracle(encode_id(oracle, Id)).

make_oracle_query(<<"ov_", _/binary>> = Q) ->
    aeb_fate_data:make_oracle_query(encode_address(oracle_query, Q));
make_oracle_query("ov_" ++ _ = Q) ->
    aeb_fate_data:make_oracle_query(encode_address(oracle_query, Q));
make_oracle_query(I) when is_integer(I) ->
    aeb_fate_data:make_oracle_query(<<I:256>>);
make_oracle_query(B) when bit_size(B) =:= 256 ->
    aeb_fate_data:make_oracle_query(B).

make_channel(<<"ch_", _/binary>> = C) ->
    aeb_fate_data:make_channel(C);
make_channel("ch_" ++ _ = C) ->
    aeb_fate_data:make_channel(C);
make_channel(I) when is_integer(I) ->
    aeb_fate_data:make_channel(<<I:256>>);
make_channel(Id) when element(1, Id) =:= id ->
    aeb_fate_data:make_channel(encode_id(channel, Id)).

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
