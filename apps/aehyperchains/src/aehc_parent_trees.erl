%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc Parent chain state Merkle trees.
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_trees).

%% API
-export([new/0]).

-export([serialize_for_db/1, deserialize_from_db/1]).
-export([commit_to_db/1]).

-export([delegates/1, set_delegates/2]).

-record(trees, { delegates :: aehc_delegates_trees:tree() }).

-opaque trees() :: #trees{}.

-export_type([trees/0]).

-spec new() -> trees().
new() ->
    #trees{ delegates  = aehc_delegates_trees:empty_with_backend() }.

-spec delegates(trees()) -> aehc_delegates_trees:tree().
delegates(Trees) ->
    Trees#trees.delegates.

-spec set_delegates(trees(), aehc_delegates_trees:tree()) -> trees().
set_delegates(Trees, Delegates) ->
    Trees#trees{ delegates = Delegates }.

%%%=============================================================================
%%% Serialization for db storage
%%%=============================================================================

-define(VSN, 0).
-define(TEMPLATE, [{delegates_hash, [binary]}]).

-spec deserialize_from_db(binary()) -> trees().
deserialize_from_db(Bin) when is_binary(Bin) ->
    List = aeser_chain_objects:deserialize(trees_db, ?VSN, ?TEMPLATE, Bin),
    [{delegates_hash, Hash}] = [db_deserialize_hash(Obj) || Obj <- List],

    #trees{
        delegates = aehc_delegates_trees:new_with_backend(Hash)
    }.

db_deserialize_hash({Field, [Hash]}) -> {Field, Hash};
db_deserialize_hash({Field, []}) -> {Field, empty}.

-spec serialize_for_db(trees()) -> binary().
serialize_for_db(#trees{} = Trees) ->
    List = [db_serialize_hash(Obj) || Obj <- [ {delegates_hash, delegates_hash(Trees)} ]],

    aeser_chain_objects:serialize(trees_db, ?VSN, ?TEMPLATE, List).

db_serialize_hash({Field, {ok, Hash}}) -> {Field, [Hash]};
db_serialize_hash({Field, {error, empty}}) -> {Field, []}.

-spec commit_to_db(trees()) -> trees().
commit_to_db(Trees) ->
    %% Make this in a transaction to get atomicity.
    aec_db:ensure_transaction(
        fun() ->
            Trees#trees{ delegates = aehc_delegates_trees:commit_to_db(delegates(Trees)) }
        end
    ).

delegates_hash(Trees) ->
    aehc_delegates_trees:root_hash(delegates(Trees)).
