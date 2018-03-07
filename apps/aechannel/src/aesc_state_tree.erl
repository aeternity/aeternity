%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for keeping the state of State Channels
%%% @end
%%%=============================================================================
-module(aesc_state_tree).

%% API
-export([commit_to_db/1,
         empty/0,
         empty_with_backend/0,
         root_hash/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type chkey() :: aesc_channels:id().
-type chvalue() :: aesc_channels:serialized().
-type chtree() :: aeu_mtrees:mtree(chkey(), chvalue()).

-type cache_key() :: binary(). %% Sext encoded
-type cache_value() :: binary().
-type cache() :: aeu_mtrees:mtree(cache_key(), cache_value()).

-record(channel_tree, {chtree = aeu_mtrees:empty() :: chtree(),
                       cache  = aeu_mtrees:empty() :: cache()}).

-opaque tree() :: #channel_tree{}.

-export_type([tree/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec commit_to_db(tree()) -> tree().
commit_to_db(#channel_tree{chtree = ChTree, cache = Cache} = Tree) ->
    Tree#channel_tree{chtree = aeu_mtrees:commit_to_db(ChTree),
                      cache  = aeu_mtrees:commit_to_db(Cache)}.

-spec empty() -> tree().
empty() ->
    #channel_tree{chtree = aeu_mtrees:empty(),
                  cache  = aeu_mtrees:empty()}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    ChTree = aeu_mtrees:empty_with_backend(aec_db_backends:channels_backend()),
    Cache  = aeu_mtrees:empty_with_backend(aec_db_backends:channels_cache_backend()),
    #channel_tree{chtree = ChTree,
                  cache  = Cache}.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#channel_tree{chtree = ChTree}) ->
    aeu_mtrees:root_hash(ChTree).
