%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Functions for serializing generic objects to/from binary format.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_serialization).

-export([ decode_fields/2
        , deserialize/5
        , deserialize_tag_and_vsn/1
        , encode_fields/2
        , serialize/4 ]).

%%%===================================================================
%%% API
%%%===================================================================

serialize(Tag, Vsn, Template, Fields) ->
    List = encode_fields([{tag, int}, {vsn, int}|Template],
                         [{tag, Tag}, {vsn, Vsn}|Fields]),
    aeu_rlp:encode(List).

%% Type isn't strictly necessary, but will give a better error reason
deserialize(Type, Tag, Vsn, Template0, Binary) ->
    Decoded = aeu_rlp:decode(Binary),
    Template = [{tag, int}, {vsn, int}|Template0],
    case decode_fields(Template, Decoded) of
        [{tag, Tag}, {vsn, Vsn}|Left] ->
            Left;
        Other ->
            error({illegal_serialization, Type, Vsn,
                   Other, Binary, Decoded, Template})
    end.

deserialize_tag_and_vsn(Binary) ->
    [TagBin, VsnBin|Fields] = aeu_rlp:decode(Binary),
    Template = [{tag, int}, {vsn, int}],
    [{tag, Tag}, {vsn, Vsn}] = decode_fields(Template, [TagBin, VsnBin]),
    {Tag, Vsn, Fields}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

