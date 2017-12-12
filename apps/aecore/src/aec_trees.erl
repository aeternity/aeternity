%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc State trees ADT.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_trees).

-include("common.hrl").
-include("blocks.hrl").

%% API

-export([all_trees_new/0,
         all_trees_hash/1,
         accounts/1,
         set_accounts/2,
         is_trees/1]).

-spec all_trees_new() -> {ok, trees()}.
all_trees_new() ->
    {ok, A} = aec_accounts:empty(),
    {ok, #trees{accounts = A}}.

all_trees_hash(Trees) ->
    %% TODO Consider all state trees - not only accounts.
    case aec_accounts:root_hash(accounts(Trees)) of
      {ok, H} -> H;
      {error, empty} -> <<0:?STATE_HASH_BYTES/unit:8>>
    end.

-spec accounts(trees()) -> aec_accounts:tree() | undefined.
accounts(Trees) ->
    Trees#trees.accounts.

-spec set_accounts(trees(), aec_accounts:tree()) -> trees().
set_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.

-spec is_trees(any()) -> boolean().
is_trees(#trees{}) -> true;
is_trees(_) -> false.
