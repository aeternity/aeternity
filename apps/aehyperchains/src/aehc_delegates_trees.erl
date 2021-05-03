%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of delegates
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_delegates_trees).

-include_lib("aehyperchains/include/aehc_utils.hrl").

-type key() :: commiter_pubkey().
-type value() :: aec_keys:pubkey().

-opaque root_hash() :: eu_mtrees:root_hash().
-opaque tree() :: aeu_mtrees:mtree(key(), value()).

%% API
-export([root_hash/1]).

-export([empty/0]).
-export([empty_with_backend/0]).

-export([new_with_backend/1]).

-export([lookup/2]).
-export([enter/3]).
-export([delete/2]).

-export([to_list/1]).

-export([commit_to_db/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    aeu_mtrees:root_hash(Tree).

-spec empty() -> tree().
empty() ->
    aeu_mtrees:empty().

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    aeu_mtrees:empty_with_backend(aec_db_backends:delegates_backend()).

-spec new_with_backend(root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    aeu_mtrees:new_with_backend(Hash, aec_db_backends:delegates_backend()).

-spec lookup(key(), tree()) -> none | {value, value()}.
lookup(Pubkey, Tree) ->
    aeu_mtrees:lookup(Pubkey, Tree).

-spec enter(key(), value(), tree()) -> tree().
enter(PubKey, Delegate, Tree) ->
    aeu_mtrees:enter(PubKey, Delegate, Tree).

-spec delete(key(), tree()) -> tree().
delete(Pubkey, Tree) ->
    aeu_mtrees:delete(Pubkey, Tree).

-spec to_list(tree()) -> [{key(), value()}].
to_list(Tree) ->
    aeu_mtrees:to_list(Tree).

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    aeu_mtrees:commit_to_db(Tree).
