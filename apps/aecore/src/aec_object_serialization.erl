%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Functions for serializing chain objects to binary format.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_object_serialization).

-export([ serialize/4
        , deserialize/4
        , deserialize_type_and_vsn/1
        , decode_fields/2
        ]).

-type template() :: [{field_name(), type()}].
-type field_name() :: atom().
-type type() :: 'int'
              | 'bool'
              | 'binary'
              | [type()] %% Length one in the type. This means a list of any length.
              | tuple(). %% Any arity, containing type(). This means a static size array.

-type encodable_term() :: non_neg_integer()
                        | binary()
                        | boolean()
                        | [encodable_term()] %% Of any length
                        | tuple().  %% Any arity, containing encodable_term().

-type fields() :: [{field_name(), encodable_term()}].

%%%===================================================================
%%% API
%%%===================================================================

-spec serialize(atom(), non_neg_integer(), template(), fields()) -> binary().
serialize(Type, Vsn, Template, Fields) ->
    List = encode_fields([{tag, int}      , {vsn, int}|Template],
                         [{tag, tag(Type)}, {vsn, Vsn}|Fields]),
    aeu_rlp:encode(List).

deserialize_type_and_vsn(Binary) ->
    [TagBin, VsnBin|Fields] = aeu_rlp:decode(Binary),
    Template = [{tag, int}, {vsn, int}],
    [{tag, Tag}, {vsn, Vsn}] = decode_fields(Template, [TagBin, VsnBin]),
    {rev_tag(Tag), Vsn, Fields}.

-spec deserialize(atom(), non_neg_integer(), template(), binary()) -> fields().
deserialize(Type, Vsn, Template0, Binary) ->
    Tag = tag(Type),
    Decoded = aeu_rlp:decode(Binary),
    Template = [{tag, int}, {vsn, int}|Template0],
    case decode_fields(Template, Decoded) of
        [{tag, Tag}, {vsn, Vsn}|Left] ->
            Left;
        Other ->
            error({illegal_serialization, Type, Vsn,
                   Other, Binary, Decoded, Template})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

encode_fields([{Field, Type}|TypesLeft],
               [{Field, Val}|FieldsLeft]) ->
    try encode_field(Type, Val) of
        Encoded -> [Encoded | encode_fields(TypesLeft, FieldsLeft)]
    catch error:{illegal, T, V} ->
        error({illegal_field, Field, Type, Val, T, V})
    end;
encode_fields([], []) ->
    [];
encode_fields(Template, Values) ->
    error({illegal_template_or_values, Template, Values}).

encode_field([Type], L) when is_list(L) ->
    [encode_field(Type, X) || X <- L];
encode_field(Type, T) when tuple_size(Type) =:= tuple_size(T) ->
    Zipped = lists:zip(tuple_to_list(Type), tuple_to_list(T)),
    [encode_field(X, Y) || {X, Y} <- Zipped];
encode_field(int, X) when is_integer(X), X >= 0 ->
    binary:encode_unsigned(X);
encode_field(binary, X) when is_binary(X) -> X;
encode_field(bool, true) -> <<1:8>>;
encode_field(bool, false) -> <<0:8>>;
encode_field(Type, Val) -> error({illegal, Type, Val}).

decode_fields([{Field, Type}|TypesLeft],
              [Bin          |FieldsLeft]) ->
    try decode_field(Type, Bin) of
        Decoded -> [{Field, Decoded} | decode_fields(TypesLeft, FieldsLeft)]
    catch error:{illegal, T, V} ->
        error({illegal_field, Field, Type, Bin, T, V})
    end;
decode_fields([], []) ->
    [];
decode_fields(Template, Values) ->
    error({illegal_template_or_values, Template, Values}).

decode_field([Type], List) when is_list(List) ->
    [decode_field(Type, X) || X <- List];
decode_field(Type, List) when length(List) =:= tuple_size(Type) ->
    Zipped = lists:zip(tuple_to_list(Type), List),
    list_to_tuple([decode_field(X, Y) || {X, Y} <- Zipped]);
decode_field(int, <<0:8, X/binary>> = B) when X =/= <<>> ->
    error({illegal, int, B});
decode_field(int, X) when is_binary(X) -> binary:decode_unsigned(X);
decode_field(binary, X) when is_binary(X) -> X;
decode_field(bool, <<1:8>>) -> true;
decode_field(bool, <<0:8>>) -> false;
decode_field(Type, X) -> error({illegal, Type, X}).

tag(account) -> 10;
tag(signed_tx) -> 11;
tag(spend_tx) -> 12;
tag(coinbase_tx) -> 13;
tag(oracle) -> 20;
tag(oracle_query) -> 21;
tag(oracle_register_tx) -> 22;
tag(oracle_query_tx) -> 23;
tag(oracle_response_tx) -> 24;
tag(oracle_extend_tx) -> 25;
tag(name) -> 30;
tag(name_commitment) -> 31;
tag(name_claim_tx) -> 32;
tag(name_preclaim_tx) -> 33;
tag(name_update_tx) -> 34;
tag(name_revoke_tx) -> 35;
tag(name_transfer_tx) -> 36;
tag(contract) -> 40;
tag(contract_call) -> 41;
tag(contract_create_tx) -> 42;
tag(contract_call_tx) -> 43.

rev_tag(10) -> account;
rev_tag(11) -> signed_tx;
rev_tag(12) -> spend_tx;
rev_tag(13) -> coinbase_tx;
rev_tag(20) -> oracle;
rev_tag(21) -> oracle_query;
rev_tag(22) -> oracle_register_tx;
rev_tag(23) -> oracle_query_tx;
rev_tag(24) -> oracle_response_tx;
rev_tag(25) -> oracle_extend_tx;
rev_tag(30) -> name;
rev_tag(31) -> name_commitment;
rev_tag(32) -> name_claim_tx;
rev_tag(33) -> name_preclaim_tx;
rev_tag(34) -> name_update_tx;
rev_tag(35) -> name_revoke_tx;
rev_tag(36) -> name_transfer_tx;
rev_tag(40) -> contract;
rev_tag(41) -> contract_call;
rev_tag(42) -> contract_create_tx;
rev_tag(43) -> contract_call_tx.
