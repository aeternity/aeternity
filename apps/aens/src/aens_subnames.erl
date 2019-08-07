%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for subname objects
%%% @end
%%%=============================================================================

-module(aens_subnames).

-include("aens.hrl").

%% API
-export([new/1,
         new/2,
         serialize/1,
         deserialize/2,
         deserialize_from_fields/3,
         serialization_type/0,
         serialization_template/1
        ]).

%% Getters
-export([id/1,
         hash/1,
         pointers/1,
         ttl/1]).

%% Utils
-export([parent_name/1,
         lookup_parent/2]).

-behavior(aens_cache).
%%%===================================================================
%%% Types
%%%===================================================================

-type id()         :: aeser_id:id().
-type serialized() :: binary().

-record(subname,
        {id         :: id(),
         pointers   :: list(aens_pointer:pointer())}).

-opaque subname()      :: #subname{}.

-export_type([subname/0,
              id/0,
              serialized/0]).

-define(SUBNAME_TYPE, subname).
-define(SUBNAME_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(binary()) -> subname().
new(SubNameHash) when is_binary(SubNameHash) ->
    #subname{id       = aeser_id:create(subname, SubNameHash),
             pointers = []}.

-spec new(binary(), list()) -> subname().
new(SubNameHash, Pointers) when is_binary(SubNameHash), is_list(Pointers) ->
    #subname{id       = aeser_id:create(subname, SubNameHash),
             pointers = Pointers}.


-spec serialize(subname()) -> binary().
serialize(#subname{pointers = Pointers}) ->
    aeser_chain_objects:serialize(
      ?SUBNAME_TYPE,
      ?SUBNAME_VSN,
      serialization_template(?SUBNAME_VSN),
      [{pointers, [{aens_pointer:key(P), aens_pointer:id(P)} || P <- Pointers]}]).

-spec deserialize(binary(), binary()) -> subname().
deserialize(SubNameHash, Bin) ->
    Fields = aeser_chain_objects:deserialize(
                  ?SUBNAME_TYPE,
                  ?SUBNAME_VSN,
                  serialization_template(?SUBNAME_VSN),
                  Bin),
    deserialize_from_fields(?SUBNAME_VSN, SubNameHash, Fields).

deserialize_from_fields(?SUBNAME_VSN, SubNameHash, [{pointers, Pointers}]) ->
    #subname{id = aeser_id:create(subname, SubNameHash),
             pointers = [aens_pointer:new(Key, Id) || {Key, Id} <- Pointers]}.

serialization_template(?SUBNAME_VSN) ->
    [{pointers, [{binary, id}]}].

serialization_type() -> ?SUBNAME_TYPE.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec id(subname()) -> id().
id(#subname{id = Id}) ->
    Id.

-spec hash(subname()) -> aens_hash:subname_hash().
hash(#subname{id = Id}) ->
    aeser_id:specialize(Id, subname).

-spec pointers(subname()) -> list(aens_pointer:pointer()).
pointers(#subname{pointers = Pointers}) ->
    Pointers.

-spec ttl(subname()) -> aec_blocks:height().
ttl(#subname{}) ->
    erlang:error(badarg).

%%%===================================================================
%%% Utils
%%%===================================================================

-spec parent_name(binary()) -> {ok, binary()} | error.
parent_name(SubDomainName) when is_binary(SubDomainName) ->
    case binary:split(SubDomainName, ?LABEL_SEPARATOR) of
        [_, Rest] ->
            case aens_utils:is_name_registrar(Rest) of
                true  -> {error, top_name};
                false -> {ok, Rest}
            end;
        [_] ->
            {error, no_registrar}
    end.

lookup_parent(Name, Trees) ->
    case parent_name(Name) of
        {ok, ParentName} ->
            {ok, ParentAscii} = aens_utils:to_ascii(ParentName),
            ParentHash = aens_hash:name_hash(ParentAscii),
            NameTrees = aec_trees:ns(Trees),
            case aens_state_tree:lookup_name(ParentHash, NameTrees) of
                {value, N} -> {ok, N};
                none       -> {error, not_found}
            end;
        Error ->
            Error
    end.
