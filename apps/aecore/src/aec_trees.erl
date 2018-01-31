%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Block state Merkle trees.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_trees).

-include("common.hrl").
-include("blocks.hrl").

%% API

-export([accounts/1,
         hash/1,
         new/0,
         ns/1,
         oracles/1,
         contracts/1,
         perform_pre_transformations/2,
         set_accounts/2,
         set_oracles/2,
         set_contracts/2,
         set_ns/2
        ]).

%%%%=============================================================================
%% API
%%%=============================================================================

-spec new() -> trees().
new() ->
    #trees{accounts  = aec_accounts_trees:empty(),
           oracles   = aeo_state_tree:empty(),
           contracts = aect_state_tree:empty(),
           ns        = aens_state_tree:empty()
          }.

hash(Trees) ->
    internal_hash(Trees).

-spec accounts(trees()) -> aec_accounts_trees:tree().
accounts(Trees) ->
    Trees#trees.accounts.

-spec set_accounts(trees(), aec_accounts_trees:tree()) -> trees().
set_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.

-spec ns(trees()) -> aens_state_tree:tree().
ns(Trees) ->
    Trees#trees.ns.

-spec set_ns(trees(), aens_state_tree:tree()) -> trees().
set_ns(Trees, Names) ->
    Trees#trees{ns = Names}.

-spec oracles(trees()) -> aeo_state_tree:tree().
oracles(Trees) ->
    Trees#trees.oracles.

-spec set_oracles(trees(), aeo_state_tree:tree()) -> trees().
set_oracles(Trees, Oracles) ->
    Trees#trees{oracles = Oracles}.

-spec perform_pre_transformations(trees(), non_neg_integer()) -> trees().
perform_pre_transformations(Trees, Height) ->
    Trees1 = set_oracles(Trees, aeo_state_tree:prune(Height, oracles(Trees))),
    set_ns(Trees1, aens_state_tree:prune(Height, ns(Trees1))).

-spec contracts(trees()) -> aect_state_tree:tree().
contracts(Trees) ->
    Trees#trees.contracts.

-spec set_contracts(trees(), aect_state_tree:tree()) -> trees().
set_contracts(Trees, Contracts) ->
    Trees#trees{contracts = Contracts}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

internal_hash(Trees) ->
    AccountsHash = pad_empty(aec_accounts_trees:root_hash(accounts(Trees))),
    ContractsHash = pad_empty(aect_state_tree:root_hash(contracts(Trees))),
    OraclesHash = pad_empty(aeo_state_tree:root_hash(oracles(Trees))),
    NamingSystemHash = pad_empty(aens_state_tree:root_hash(ns(Trees))),
    List = lists:sort([ {<<"accounts"/utf8>> , AccountsHash}
                      , {<<"contracts"/utf8>>, ContractsHash}
                      , {<<"oracles"/utf8>>  , OraclesHash}
                      , {<<"ns"/utf8>>       , NamingSystemHash}
                      ]),
    TopTree = lists:foldl(fun({Key, Val}, Acc) ->
                                  aeu_mtrees:enter(Key, Val, Acc)
                          end,
                         aeu_mtrees:empty(), List),
    {ok, Hash} = aeu_mtrees:root_hash(TopTree),
    Hash.

pad_empty({ok, H}) when is_binary(H) -> H;
pad_empty({error, empty}) -> <<0:?STATE_HASH_BYTES/unit:8>>.
