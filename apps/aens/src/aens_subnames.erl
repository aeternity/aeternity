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
         lookup_parent/2,
         toposort/2]).

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

note_error(Errors, ErrType, Name) ->
    Error = maps:get(ErrType, Errors, gb_sets:new()),
    Errors#{ErrType => gb_sets:add(Name, Error)}.

toposort(<<TopName/binary>>, #{} = Definition) ->
    {ok, TopNameAscii} = aens_utils:name_to_ascii(TopName),
    {ok, name, TopNameParts} = aens_utils:check_split_name(TopName),
    {ResNodes, ResPointers, ResErrors} =
        maps:fold(
          fun (SName, SPointers, {Nodes, Pointers, Errors}) ->
                  case aens_utils:check_split_name(SName) of
                      {error, ErrType} ->
                          {Nodes, Pointers,
                           note_error(Errors, ErrType, SName)};
                      {ok, name, TopNameParts0} when TopNameParts0 /= TopNameParts ->
                          {Nodes, Pointers,
                           note_error(Errors, bad_domain, SName)};
                      {ok, _SType, SNameParts} ->
                          toposort_node(SName, SNameParts, SPointers, TopNameAscii,
                                        {Nodes, Pointers, Errors})
                  end
          end,
          {gb_trees:empty(), #{}, #{}},
          Definition),
    if map_size(ResErrors) > 0 ->
            {error, [{ErrType, gb_sets:to_list(Errs)} ||
                        {ErrType, Errs} <- maps:to_list(ResErrors)]};
       true ->
            {ok, [{Len, [{SName, maps:get(SName, ResPointers, #{})} ||
                            SName <- gb_sets:to_list(LenNodes)]} ||
                     {Len, LenNodes} <- gb_trees:to_list(ResNodes)]}
    end.

toposort_node(SName, SNameParts, SPointers, TopNameAscii, {Nodes, Pointers, Errors}) ->
    {ok, SNameAscii} = aens_utils:name_to_ascii(SName),
    [Registrar, TopNamePart | RevSnameParts] = lists:reverse(SNameParts),
    {ok, TopNameAsciiPart} = aens_utils:ascii_encode(TopNamePart),
    {ok, AsciiRegistrar}   = aens_utils:ascii_encode(Registrar),
    InitLenNode  = binary:list_to_bin([TopNameAsciiPart, ".", AsciiRegistrar]),
    InitLenNodes = gb_sets:union(get_len_node(1, Nodes),
                                 gb_sets:from_list([InitLenNode])),
    InitNodes    = gb_trees:enter(1, InitLenNodes, Nodes),
    {_, _, Nodes1} =
        lists:foldl(
          fun (NamePart, {Len, AccName, Ns}) ->
                  {ok, NamePartAscii} = aens_utils:ascii_encode(NamePart),
                  AccName1  = binary:list_to_bin([NamePartAscii, ".", AccName]),
                  LenNodes  = get_len_node(Len, Ns),
                  LenNodes1 = gb_sets:add(AccName1, LenNodes),
                  {Len + 1, AccName1, gb_trees:enter(Len, LenNodes1, Ns)}
          end,
          {2, TopNameAscii, InitNodes},
          RevSnameParts),
    SPointers0 = maps:get(SNameAscii, Pointers, #{}),
    Pointers1  = Pointers#{SNameAscii => maps:merge(SPointers0, SPointers)},
    {Nodes1, Pointers1, Errors}.

get_len_node(Len, Nodes) ->
    case gb_trees:lookup(Len, Nodes) of
        {value, Ns} -> Ns;
        none        -> gb_sets:new()
    end.
