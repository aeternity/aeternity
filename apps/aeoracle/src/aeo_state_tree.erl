%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of oracles
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_state_tree).

%% API
-export([ get_interaction/2
        , get_oracle/2
        , find_interaction/2
        , find_oracle/2
        , new/0
        , put_interaction/2
        , put_oracle/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type tree() :: gb_merkle_trees:tree().
-type interaction() :: aeo_interaction:interaction().
-type oracle() :: aeo_oracles:oracle().

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> tree().
new() ->
    {ok, Tree} = aec_trees:new_merkle_tree(),
    Tree.

-spec put_interaction(interaction(), tree()) -> tree().
put_interaction(I, Tree) ->
    Id = aeo_interaction:id(I),
    Serialized = aeo_interaction:serialize(I),
    {ok, Tree1} = aec_trees:put(Id, Serialized, Tree),
    Tree1.

-spec get_interaction(binary(), tree()) -> interaction().
get_interaction(Id, Tree) ->
    case aec_trees:get(Id, Tree) of
        {ok, Val}  -> aeo_interaction:deserialize(Val); %% Will fail with exception if invalid
        {error, E} -> error(E)
    end.

-spec find_interaction(binary(), tree()) -> {'ok', interaction()} | {'error', term()}.
find_interaction(Id, Tree) ->
    case aec_trees:get(Id, Tree) of
        {ok, Val}  ->
            try {ok, aeo_interaction:deserialize(Val)}
            catch error:What -> {error, What}
            end;
        {error, _} = E -> E
    end.

-spec put_oracle(oracle(), tree()) -> tree().
put_oracle(O, Tree) ->
    Id = aeo_oracles:id(O),
    Serialized = aeo_oracles:serialize(O),
    {ok, Tree1} = aec_trees:put(Id, Serialized, Tree),
    Tree1.

-spec get_oracle(binary(), tree()) -> oracle().
get_oracle(Id, Tree) ->
    case aec_trees:get(Id, Tree) of
        {ok, Val}  -> aeo_oracles:deserialize(Val); %% Will fail with exception if invalid
        {error, E} -> error(E)
    end.

-spec find_oracle(binary(), tree()) -> {'ok', oracle()} | {'error', term()}.
find_oracle(Id, Tree) ->
    case aec_trees:get(Id, Tree) of
        {ok, Val}  ->
            try {ok, aeo_oracles:deserialize(Val)}
            catch error:What -> {error, What}
            end;
        {error, _} = E -> E
    end.
