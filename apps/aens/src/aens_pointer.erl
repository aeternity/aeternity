-module(aens_pointer).

-export([new/2,
         key/1,
         id/1,
         sanitize_pointers/1,
         serialize_for_client/1
        ]).

-type key() :: binary().
-type id()  :: aeser_id:id().

-record(pointer, {
          key :: key(),
          id  :: id()
        }).

-opaque pointer() :: #pointer{}.

-export_type([key/0,
              id/0,
              pointer/0
             ]).

-include_lib("aecontract/include/hard_forks.hrl").

-spec new(key(), id()) -> pointer().
new(Key, Id) ->
    ok = assert_key(Key),
    ok = assert_id(Id),
    #pointer{key = Key, id = Id}.

-spec key(pointer()) -> key().
key(#pointer{key = Key}) ->
    Key.

-spec id(pointer()) -> id().
id(#pointer{id = Id}) ->
    Id.

-spec serialize_for_client(pointer()) -> map().
serialize_for_client(#pointer{key = Key, id = Id}) ->
    #{<<"key">> => Key,
      <<"id">>  => aeser_api_encoder:encode(id_hash, Id)}.

%% TODO: check max length of key?
assert_key(Key) when is_binary(Key) -> ok.

assert_id(Id) ->
    assert_id_type(aeser_id:specialize_type(Id)).

%% TODO: what to support?
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
