-module(aens_pointer).

-export([new/2,
         key/1,
         id/1,
         sanitize_pointers/1,
         serialize_for_client/1,
         serialize_pointer_vsn1/1, serialize_pointer_vsn2/1,
         deserialize_pointer_vsn1/1, deserialize_pointer_vsn2/1,
         has_raw_data_pointer/1
        ]).

-type key()  :: binary().
-type id()   :: aeser_id:id().
-type data() :: {data, binary()}.
-type id_type() :: id() | data().

-record(pointer, {
          key :: key(),
          id  :: id_type()
        }).

-opaque pointer() :: #pointer{}.

-export_type([key/0,
              id/0,
              data/0,
              id_type/0,
              pointer/0
             ]).

-include_lib("aecontract/include/hard_forks.hrl").

-spec new(key(), id() | binary()) -> pointer().
new(Key, Data) when is_binary(Data) ->
    ok = assert_key(Key),
    ok = assert_data(Data),
    #pointer{key = Key, id = {data, Data}};
new(Key, Id) ->
    ok = assert_key(Key),
    ok = assert_id(Id),
    #pointer{key = Key, id = Id}.

-spec key(pointer()) -> key().
key(#pointer{key = Key}) ->
    Key.

-spec id(pointer()) -> id() | data().
id(#pointer{id = Id}) ->
    Id.

-spec serialize_for_client(pointer()) -> map().
serialize_for_client(#pointer{key = Key, id = Id}) ->
    #{<<"key">> => Key,
      <<"id">>  => serialize_id_for_client(Id)}.

serialize_id_for_client({data, Data}) ->
    aeser_api_encoder:encode(bytearray, Data);
serialize_id_for_client(Id) ->
    aeser_api_encoder:encode(id_hash, Id).

%% TODO: check max length of key?
assert_key(Key) when is_binary(Key) -> ok.

assert_data(Data) when is_binary(Data), byte_size(Data) =< 1024 -> ok.

assert_id(Id) ->
    assert_id_type(aeser_id:specialize_type(Id)).

assert_id_type(account)  -> ok;
assert_id_type(channel)  -> ok;
assert_id_type(contract) -> ok;
assert_id_type(oracle)   -> ok.

%% From IRIS pointers should be:
%%  - unique (no overlapping keys)
%%  - not more than 32
%%  - keys should be no bigger than 256 bytes.
-spec sanitize_pointers(list(pointer())) -> list(pointer()).
sanitize_pointers(Pts) ->
    MaxCount   = aec_governance:name_pointers_max_count(?IRIS_PROTOCOL_VSN),
    MaxKeySize = aec_governance:name_pointer_max_key_size(?IRIS_PROTOCOL_VSN),
    sanitize_pointers(Pts, #{}, 0, MaxCount, MaxKeySize).

sanitize_pointers([], _, _, _, _) -> [];
sanitize_pointers(_, _, N, N, _) -> [];
sanitize_pointers([Pt | Pts], Seen, N, MaxCount, MaxKeySize) ->
    Key = key(Pt),
    case maps:is_key(Key, Seen) of
        true -> sanitize_pointers(Pts, Seen, N, MaxCount, MaxKeySize);
        false ->
            case byte_size(Key) > MaxKeySize of
                true -> sanitize_pointers(Pts, Seen, N, MaxCount, MaxKeySize);
                false -> [Pt | sanitize_pointers(Pts, Seen#{Key => ok}, N + 1, MaxCount, MaxKeySize)]
            end
    end.

-define(AENS_POINTER_ID_TAG, 1).
-define(AENS_POINTER_DATA_TAG, 2).

-spec serialize_pointer_vsn1(pointer()) -> {key(), id()}.
serialize_pointer_vsn1(#pointer{key = Key, id = Id}) -> {Key, Id}.

-spec serialize_pointer_vsn2(pointer()) -> {key(), binary()}.
serialize_pointer_vsn2(#pointer{key = Key, id = {data, Data}}) ->
    {Key, <<?AENS_POINTER_DATA_TAG:8, Data/binary>>};
serialize_pointer_vsn2(#pointer{key = Key, id = Id}) ->
    EncodedId = aeser_id:encode(Id),
    {Key, <<?AENS_POINTER_ID_TAG:8, EncodedId/binary>>}.

-spec deserialize_pointer_vsn1({key(), id()}) -> pointer().
deserialize_pointer_vsn1({Key, Id}) -> new(Key, Id).

-spec deserialize_pointer_vsn2({key(), binary()}) -> pointer().
deserialize_pointer_vsn2({Key, <<?AENS_POINTER_DATA_TAG:8, Data/binary>>}) ->
    new(Key, Data);
deserialize_pointer_vsn2({Key, <<?AENS_POINTER_ID_TAG:8, EncodedId/binary>>}) ->
    new(Key, aeser_id:decode(EncodedId)).

-spec has_raw_data_pointer([pointer()]) -> boolean().
has_raw_data_pointer([])                             -> false;
has_raw_data_pointer([#pointer{id = {data, _}} | _]) -> true;
has_raw_data_pointer([_ | Ps])                       -> has_raw_data_pointer(Ps).
