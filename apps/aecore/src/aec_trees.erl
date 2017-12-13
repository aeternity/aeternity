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
         set_accounts/2,
         oracles/1,
         set_oracles/2]).

-spec new() -> trees().
new() ->
    #trees{accounts = aec_accounts_trees:empty(),
           oracles  = aeo_state_tree:empty()
          }.

hash(Trees) ->
    AccountsHash = pad_empty(aec_accounts_trees:root_hash(accounts(Trees))),
    OraclesHash = pad_empty(aeo_state_tree:root_hash(oracles(Trees))),
    List = lists:sort([ {<<"accounts"/utf8>>, AccountsHash}
                      , {<<"oracles"/utf8>> , OraclesHash}
                      ]),
    TopTree = lists:foldl(fun({Key, Val}, Acc) ->
                                  aeu_mtrees:enter(Key, Val, Acc)
                          end,
                         aeu_mtrees:empty(), List),
    {ok, Hash} = aeu_mtrees:root_hash(TopTree),
    Hash.

pad_empty({ok, H}) when is_binary(H) -> H;
pad_empty({error, empty}) -> <<0:?STATE_HASH_BYTES/unit:8>>.

-spec accounts(trees()) -> aec_accounts_trees:tree().
accounts(Trees) ->
    Trees#trees.accounts.

-spec set_accounts(trees(), aec_accounts_trees:tree()) -> trees().
set_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.

-spec oracles(trees()) -> aeo_state_trees:tree().
oracles(Trees) ->
    Trees#trees.oracles.

-spec set_oracles(trees(), aeo_state_trees:tree()) -> trees().
set_oracles(Trees, Oracles) ->
    Trees#trees{oracles = Oracles}.
