%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Testutils for Fate engine
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_test_utils).

-export([encode/1,
         decode/1,
         decode/2
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

%% Encode is a convinience function for testing, encoding an Erlang term
%% to a Fate term, but it can not distinguish between e.g. 32-byte strings
%% and addresses. Therfore an extra tuple layer on the erlang side for
%% addresses and bits.
encode({bits, Term}) when is_integer(Term) -> aeb_fate_data:make_bits(Term);
%% TODO: check that each byte is in base58
encode({address, B}) when is_binary(B)  -> aeb_fate_data:make_address(B);
encode({address, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_address(B);
encode({address, S}) when is_list(S)  ->
    aeb_fate_data:make_address(encode_address(account_pubkey, S));
encode({hash, H}) when is_binary(H)  -> aeb_fate_data:make_hash(H);
encode({hash, H}) when is_list(H)  -> aeb_fate_data:make_hash(base64:decode(H));
encode({signature, S}) when is_binary(S)  -> aeb_fate_data:make_signature(S);
encode({signature, S}) when is_list(S)  ->
    aeb_fate_data:make_signature(encode_address(signature, S));
encode({bytes, B}) when is_binary(B) -> aeb_fate_data:make_bytes(B);
encode({contract, B}) when is_binary(B)  -> aeb_fate_data:make_contract(B);
encode({contract, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_contract(B);
encode({contract, S}) when is_list(S)  ->
    aeb_fate_data:make_contract(encode_address(contract_pubkey, S));
encode({oracle, B}) when is_binary(B)  -> aeb_fate_data:make_oracle(B);
encode({oracle, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_oracle(B);
encode({oracle_query, B}) when is_binary(B)  -> aeb_fate_data:make_oracle_query(B);
encode({oracle_query, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_oracle_query(B);
encode({oracle, S}) when is_list(S)  ->
   aeb_fate_data:make_oracle(encode_address(oracle_pubkey, S));
encode({channel, B}) when is_binary(B)  -> aeb_fate_data:make_channel(B);
encode({channel, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_channel(B);
encode({channel, S}) when is_list(S)  ->
    aeb_fate_data:make_channel(encode_address(channel, S));
encode({variant, Arities, Tag, Values}) ->
    aeb_fate_data:make_variant(Arities, Tag, list_to_tuple([encode(V) || V <- tuple_to_list(Values)]));
encode(none)      -> aeb_fate_data:make_variant([0, 1], 0, {});
encode({some, X}) -> aeb_fate_data:make_variant([0, 1], 1, {encode(X)});
encode(Term) when is_integer(Term) -> aeb_fate_data:make_integer(Term);
encode(Term) when is_boolean(Term) -> aeb_fate_data:make_boolean(Term);
encode(Term) when is_list(Term) -> aeb_fate_data:make_list([encode(E) || E <- Term]);
encode(Term) when is_tuple(Term) ->
    aeb_fate_data:make_tuple(list_to_tuple([encode(E) || E <- erlang:tuple_to_list(Term)]));
encode(Term) when is_map(Term) ->
    aeb_fate_data:make_map(maps:from_list([{encode(K), encode(V)} || {K,V} <- maps:to_list(Term)]));
encode(Term) when is_binary(Term) -> aeb_fate_data:make_string(Term).

encode_address(Type, S) when is_list(S) ->
    B = list_to_binary(S),
    try aeser_api_encoder:decode(B) of
        {Type, Encoding} ->
            Encoding;
        _ -> erlang:error({bad_address_encoding, Type, S})
    catch _:_ ->
            erlang:error({bad_address_encoding, Type, S})
    end.

decode(I) when ?IS_FATE_INTEGER(I)          -> I;
decode(?FATE_TRUE)                          -> true;
decode(?FATE_FALSE)                         -> false;
decode(L) when ?IS_FATE_LIST(L)             -> [decode(E) || E <- L];
decode(?FATE_ADDRESS(<<Address:256>>))      -> {address, Address};
decode(?FATE_BYTES(B))                      -> {bytes, B};
decode(?FATE_CONTRACT(<<X:256>>))           -> {contract, X};
decode(?FATE_ORACLE(<<X:256>>))             -> {oracle, X};
decode(?FATE_CHANNEL(<<X:256>>))            -> {channel, X};
decode(?FATE_BITS(Bits))                    -> {bits, Bits};
decode(?FATE_TUPLE(T))                      -> list_to_tuple([decode(E) || E <- tuple_to_list(T)]);
decode(?FATE_VARIANT(Arities, Tag, Values)) -> {variant, Arities, Tag, decode(?FATE_TUPLE(Values))};
decode(S) when ?IS_FATE_STRING(S)           -> S;
decode(M) when ?IS_FATE_MAP(M)              ->
    maps:from_list([{decode(K), decode(V)} || {K, V} <- maps:to_list(M)]).

decode(I, word) when ?IS_FATE_INTEGER(I)     -> I;
decode(I, signed_word) when ?IS_FATE_INTEGER(I) -> I;
decode(?FATE_TRUE, bool)                     -> true;
decode(?FATE_FALSE, bool)                    -> false;
decode(L, {list, T}) when ?IS_FATE_LIST(L)   -> [decode(E, T) || E <- L];
decode(?FATE_ADDRESS(<<Address:256>>), word) -> {address, Address};
decode(?FATE_BYTES(B), word)                 -> {bytes, B};
decode(?FATE_CONTRACT(<<X:256>>), word)      -> {contract, X};
decode(?FATE_ORACLE(<<X:256>>), word)        -> {oracle, X};
decode(?FATE_ORACLE_Q(<<X:256>>), word)      -> {oracle_query, X};
decode(?FATE_CHANNEL(<<X:256>>), word)       -> {channel, X};
decode(?FATE_BITS(Bits), word)               -> {bits, Bits};
decode(?FATE_TUPLE({}), word)                -> {tuple, []};
decode(?FATE_TUPLE(Tuple), {tuple, Ts})      ->
    list_to_tuple([decode(E, T) || {E, T} <- lists:zip(tuple_to_list(Tuple), Ts)]);
decode(?FATE_VARIANT(Arities, Tag, Values), {option, Type}) ->
    decode_variant(Arities, [{none, []}, {some, [Type]}], Tag, Values, '$undefined$');
decode(?FATE_VARIANT(Arities, Tag, Values), {variant_t, Cs}) ->
    decode_variant(Arities, Cs, Tag, Values, '$undefined$');
decode(S, string) when ?IS_FATE_STRING(S)           -> S;
decode(M, {map, KeyType, ValType}) when ?IS_FATE_MAP(M)              ->
    maps:from_list([{decode(K, KeyType), decode(V, ValType)} || {K, V} <- maps:to_list(M)]).

decode_variant([Arity|Left1], [{C, Types}|Left2], N = 0, Values, Acc) ->
    %% These are the values that should be returned.
    '$undefined$' = Acc,
    case Types of
        [] when Arity =:= 0 ->
            decode_variant(Left1, Left2, N - 1, Values, C);
        [] when Arity =/= 0 ->
            error(variant_type_error);
        [_|_] when length(Types) =:= Arity ->
            Tuple = decode(?FATE_TUPLE(Values), {tuple, Types}),
            Acc1 = list_to_tuple([C | tuple_to_list(Tuple)]),
            decode_variant(Left1, Left2, N - 1, Values, Acc1);
        _ ->
            error({decode_variant_fail, Types, Arity})
    end;
decode_variant([Arity|Left1], [{_C, Types}|Left2], N, Values, Acc) ->
    case length(Types) =:= Arity of
        true ->
            decode_variant(Left1, Left2, N - 1, Values, Acc);
        false ->
            error({decode_variant_fail, Types, Arity})
    end;
decode_variant([], [], _N, _Values, '$undefined$') ->
    error(decode_variant_fail);
decode_variant([], [], _N, _Values, Acc) ->
    Acc.


