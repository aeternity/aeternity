%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Implementation of the Recursive Length Prefix.
%%%
%%%     https://github.com/ethereum/wiki/wiki/RLP
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeu_rlp).
-export([ decode/1
	, encode/1
        ]).

-export_type([ encodable/0
             , encoded/0
             ]).

-type encodable() :: [encodable()] | binary().
-type encoded()   :: <<_:8, _:_*8>>.

-define(UNTAGGED_SIZE_LIMIT , 55).
-define(UNTAGGED_LIMIT      , 127).
-define(BYTE_ARRAY_OFFSET   , 128).
-define(LIST_OFFSET         , 192).


-spec encode(encodable()) -> encoded().
encode(X) ->
    encode(X, []).

encode(<<B>> = X,_Opts) when B =< ?UNTAGGED_LIMIT ->
    %% An untagged value
    X;
encode(X,_Opts) when is_binary(X) ->
    %% Byte array
    add_size(?BYTE_ARRAY_OFFSET, X);
encode(L, Opts) when is_list(L) ->
    %% Lists items are encoded and concatenated
    ByteArray = << << (encode(X, Opts))/binary >> || X <- L >>,
    add_size(?LIST_OFFSET, ByteArray).

add_size(Offset, X) when byte_size(X) =< ?UNTAGGED_SIZE_LIMIT ->
    %% The size fits in one tagged byte
    <<(Offset + byte_size(X)), X/binary>>;
add_size(Offset, X) when is_binary(X) ->
    %% The size itself needs to be encoded as a byte array
    %% Add the tagged size of the size byte array
    SizeBin    = binary:encode_unsigned(byte_size(X)),
    TaggedSize = ?UNTAGGED_SIZE_LIMIT + Offset + byte_size(SizeBin),
    true       = (TaggedSize < 256 ), %% Assert
    <<TaggedSize, SizeBin/binary, X/binary>>.

-spec decode(encoded()) -> encodable().
decode(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    case decode_one(Bin) of
        {X, <<>>} -> X;
        {X, Left} -> error({trailing, X, Bin, Left})
    end.

decode_one(<<X, B/binary>>) when X =< ?UNTAGGED_LIMIT ->
    %% Untagged value
    {<<X>>, B};
decode_one(<<L, _/binary>> = B) when L < ?LIST_OFFSET ->
    %% Byte array
    {Size, Rest} = decode_size(B, ?BYTE_ARRAY_OFFSET),
    <<X:Size/binary, Tail/binary>> = Rest,
    {X, Tail};
decode_one(<<_/binary>> = B) ->
    %% List
    {Size, Rest} = decode_size(B, ?LIST_OFFSET),
    <<X:Size/binary, Tail/binary>> = Rest,
    {decode_list(X), Tail}.

decode_size(<<L, B/binary>>, Offset) when L =< Offset + ?UNTAGGED_SIZE_LIMIT->
    %% One byte tagged size.
    {L - Offset, B};
decode_size(<<_, 0, _/binary>>,_Offset) ->
    error(leading_zeroes_in_size);
decode_size(<<L, B/binary>>, Offset) ->
    %% Actual size is in a byte array.
    BinSize = L - Offset - ?UNTAGGED_SIZE_LIMIT,
    <<Size:BinSize/unit:8, Rest/binary>> = B,
    {Size, Rest}.

decode_list(<<>>) -> [];
decode_list(B) ->
    {Element, Rest} = decode_one(B),
    [Element|decode_list(Rest)].
