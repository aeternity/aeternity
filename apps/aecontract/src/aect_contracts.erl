%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract objects
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([ deserialize/1
        , id/1
        , new/2
        , owner/1
        , serialize/1
        , set_owner/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(contract, { owner             :: pubkey()
                  }).

-opaque contract() :: #contract{}.

-export_type([ contract/0
             ]).

-define(PUB_SIZE, 65).
-define(HASH_SIZE, 32).
-define(CONTRACT_TYPE, <<"contract">>).
-define(CONTRACT_VSN, 0).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(contract()) -> pubkey().
id(C) ->
  owner(C).

-spec new(aect_create_tx:create_tx(), height()) -> contract().
new(RTx, _BlockHeight) ->
    C = #contract{ owner = aect_create_tx:owner(RTx)
                 },
    assert_fields(C).

-spec serialize(contract()) -> binary().
serialize(#contract{} = C) ->
    msgpack:pack([ #{<<"type">>              => ?CONTRACT_TYPE}
                 , #{<<"vsn">>               => ?CONTRACT_VSN}
                 , #{<<"owner">>             => owner(C)}
                 ]).

-spec deserialize(binary()) -> contract().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [ #{<<"type">>              := ?CONTRACT_TYPE}
    , #{<<"vsn">>               := ?CONTRACT_VSN}
    , #{<<"owner">>             := Owner}
    ] = List,
    #contract{ owner             = Owner
             }.

%%%===================================================================
%%% Getters

-spec owner(contract()) -> pubkey().
owner(C) -> C#contract.owner.

%%%===================================================================
%%% Setters

-spec set_owner(pubkey(), contract()) -> contract().
set_owner(X, C) ->
    C#contract{owner = assert_field(owner, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(C) ->
    List = [ {owner, C#contract.owner}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> C;
        Other -> error({missing, Other})
    end.

assert_field(owner, <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(Field, X) -> error({illegal, Field, X}).
