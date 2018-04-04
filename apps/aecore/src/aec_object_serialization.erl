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

%%%===================================================================
%%% API
%%%===================================================================

serialize(Type, Vsn, Template, Fields) ->
    List = encode_fields([{tag, int}      , {vsn, int}|Template],
                          [{tag, tag(Type)}, {vsn, Vsn}|Fields]),
    aeu_rlp:encode(List).

deserialize_type_and_vsn(Binary) ->
    [TagBin, VsnBin|Fields] = aeu_rlp:decode(Binary),
    Template = [{tag, int}, {vsn, int}],
    [{tag, Tag}, {vsn, Vsn}] = decode_fields(Template, [TagBin, VsnBin]),
    {rev_tag(Tag), Vsn, Fields}.

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

encode_fields([{Field, [Type]}|TypesLeft],
               [{Field, List}|FieldsLeft]) when is_list(List) ->
    [encode_list_field(Field, List, Type)|encode_fields(TypesLeft, FieldsLeft)];
encode_fields([{Field, Type}|TypesLeft],
               [{Field, Val}|FieldsLeft]) ->
    [encode_field(Field, Type, Val)|encode_fields(TypesLeft, FieldsLeft)];
encode_fields([], []) ->
    [];
encode_fields(Template, Values) ->
    error({illegal_template_or_values, Template, Values}).

encode_list_field(Field, [Val|Left], Type) ->
    [encode_field(Field, Type, Val)|encode_list_field(Field, Left, Type)];
encode_list_field(_Field, [],_Type) ->
    [];
encode_list_field(Field, Values, Type) ->
    error({illegal_list, Field, Values, Type}).

encode_field(_Field, int, X) when is_integer(X), X >= 0 ->
    binary:encode_unsigned(X);
encode_field(_Field, binary, X) when is_binary(X) -> X;
encode_field(_Field, bool, true) -> <<1:8>>;
encode_field(_Field, bool, false) -> <<0:8>>;
encode_field(Field, Type, Val) -> error({illegal_field, Field, Type, Val}).

decode_fields([{Field, [Type]}|TypesLeft],
              [List           |FieldsLeft]) when is_list(List) ->
    [{Field, decode_list_field(Field, List, Type)}
     |decode_fields(TypesLeft, FieldsLeft)];
decode_fields([{Field, Type}|TypesLeft],
              [Bin         |FieldsLeft]) ->
    [{Field, decode_field(Field, Type, Bin)}
     |decode_fields(TypesLeft, FieldsLeft)];
decode_fields([], []) ->
    [];
decode_fields(Template, Values) ->
    error({illegal_template_or_values, Template, Values}).

decode_list_field(Field, [Val|Left], Type) ->
    [decode_field(Field, Type, Val)|decode_list_field(Field, Left, Type)];
decode_list_field(_Field, [],_Type) ->
    [].

decode_field(Field, int, <<0:8, X/binary>> = B) when X =/= <<>> ->
    error({illegal, Field, B});
decode_field(_Field, int, X) when is_binary(X) -> binary:decode_unsigned(X);
decode_field(_Field, binary, X) when is_binary(X) -> X;
decode_field(_Field, bool, <<1:8>>) -> true;
decode_field(_Field, bool, <<0:8>>) -> false;
decode_field(Field, Type, X) -> error({illegal_field, Field, Type, X}).

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
tag(contract_call_tx) -> 43;
tag(channel_create_tx) -> 50;
tag(channel_deposit_tx) -> 51;
tag(channel_withdraw_tx) -> 52;
tag(channel_close_mutual_tx) -> 53;
tag(channel_close_solo_tx) -> 54;
tag(channel_slash_tx) -> 55;
tag(channel_settle_tx) -> 56;
tag(channel_offchain_tx) -> 57.

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
rev_tag(43) -> contract_call_tx;
rev_tag(50) -> channel_create_tx;
rev_tag(51) -> channel_deposit_tx;
rev_tag(52) -> channel_withdraw_tx;
rev_tag(53) -> channel_close_mutual_tx;
rev_tag(54) -> channel_close_solo_tx;
rev_tag(55) -> channel_slash_tx;
rev_tag(56) -> channel_settle_tx;
rev_tag(57) -> channel_offchain_tx.
