%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Channel Merkle trees.
%%% @end
%%%-------------------------------------------------------------------
-module(aesc_trees).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/blocks.hrl").

-export([accounts/1,
         get_account/2,
         set_account/2,
         contracts/1,
         calls/1,
         hash/1,
         new/1]).

-record(trees, {
          accounts  :: aec_accounts_trees:tree(),
          calls     :: aect_call_state_tree:tree(),
          contracts :: aect_state_tree:tree()}).

-opaque trees() :: #trees{}.
-export_type([trees/0]).

%%%%=============================================================================
%% API
%%%=============================================================================

-spec new([{pubkey(), integer()}]) -> trees().
new(Accts) ->
    Accounts =
        lists:foldl(
            fun({Pubkey, Amount}, AccTree) ->
                Account = aec_accounts:new(Pubkey, Amount),
                aec_accounts_trees:enter(Account, AccTree)
            end,
            aec_accounts_trees:empty(),
            Accts),
    #trees{accounts  = Accounts,
           calls     = aect_call_state_tree:empty(),
           contracts = aect_state_tree:empty()
          }.

-spec hash(trees()) -> binary().
hash(Trees) ->
    internal_hash(Trees).

-spec accounts(trees()) -> aec_accounts_trees:tree().
accounts(Trees) ->
    Trees#trees.accounts.

-spec get_account(trees(), pubkey()) -> {ok, aec_accounts:account()} |
                                        {error, not_found}.
get_account(#trees{accounts=Accs}, Pubkey) ->
    case aec_accounts_trees:lookup(Pubkey, Accs) of
        none -> {error, not_found};
        {value, Account} -> {ok, Account}
    end.

-spec set_account(trees(), aec_accounts:account()) -> trees().
set_account(#trees{accounts=Accs0}=Trees, Account) ->
    Accs = aec_accounts_trees:enter(Account, Accs0),
    Trees#trees{accounts=Accs}.

-spec calls(trees()) -> aect_call_state_tree:tree().
calls(Trees) ->
    Trees#trees.calls.

-spec contracts(trees()) -> aect_state_tree:tree().
contracts(Trees) ->
    Trees#trees.contracts.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

internal_hash(Trees) ->
    AccountsHash = pad_empty(aec_accounts_trees:root_hash(accounts(Trees))),
    CallsHash = pad_empty(aect_call_state_tree:root_hash(calls(Trees))),
    ContractsHash = pad_empty(aect_state_tree:root_hash(contracts(Trees))),
    List = lists:sort([ {<<"accounts"/utf8>> , AccountsHash}
                      , {<<"calls"/utf8>>    , CallsHash}
                      , {<<"contracts"/utf8>>, ContractsHash}
                      ]),
    TopTree = lists:foldl(fun({Key, Val}, Acc) ->
                                  aeu_mtrees:enter(Key, Val, Acc)
                          end,
                         aeu_mtrees:empty(), List),
    {ok, Hash} = aeu_mtrees:root_hash(TopTree),
    Hash.

pad_empty({ok, H}) when is_binary(H) -> H;
pad_empty({error, empty}) -> <<0:?STATE_HASH_BYTES/unit:8>>.
