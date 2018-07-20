-module(aens_pointer).

-export([new/2,
         key/1,
         id/1,
         serialize_for_client/1
        ]).

-type key() :: binary().
-type id()  :: aec_id:id().

-record(pointer, {
          key :: key(),
          id  :: id()
        }).

-opaque pointer() :: #pointer{}.

-export_type([key/0,
              id/0,
              pointer/0
             ]).

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
      <<"id">>  => aec_base58c:encode(id_hash, Id)}.

%% TODO: check max lenght of key?
assert_key(Key) when is_binary(Key) -> ok.

assert_id(Id) ->
    assert_id_type(aec_id:specialize_type(Id)).

%% TODO: what to support?
assert_id_type(account)  -> ok;
assert_id_type(channel)  -> ok;
assert_id_type(contract) -> ok;
assert_id_type(oracle)   -> ok.

