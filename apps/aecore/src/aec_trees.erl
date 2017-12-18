%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Block state Merkle trees.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_trees).

-include("common.hrl").
-include("blocks.hrl").

%% API

-export([new/0,
         hash/1,
         accounts/1,
         set_accounts/2]).

-spec new() -> trees().
new() ->
    #trees{accounts = aec_accounts_trees:empty()}.

hash(Trees) ->
    %% TODO Consider all state trees - not only accounts.
    case aec_accounts_trees:root_hash(accounts(Trees)) of
      {ok, H} -> H;
      {error, empty} -> <<0:?STATE_HASH_BYTES/unit:8>>
    end.

-spec accounts(trees()) -> aec_accounts_trees:tree().
accounts(Trees) ->
    Trees#trees.accounts.

-spec set_accounts(trees(), aec_accounts_trees:tree()) -> trees().
set_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.
