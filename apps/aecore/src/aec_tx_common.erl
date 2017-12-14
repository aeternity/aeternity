%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_tx_common).

-export([ensure_account_at_height/3]).

-include("common.hrl").
-include("trees.hrl").

-spec ensure_account_at_height(pubkey(), trees(), height()) ->
                                   {ok, trees()} | {error, account_height_too_big}.
ensure_account_at_height(AccountPubkey, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts_trees:lookup(AccountPubkey, AccountsTrees0) of
        {value, Account} ->
            AccountCurrentHeight = aec_accounts:height(Account),
            case AccountCurrentHeight =< Height of
                true ->
                    {ok, Trees0};
                false ->
                    {error, account_height_too_big}
            end;
        none ->
            %% Add newly referenced account (w/0 amount) to the state
            Account = aec_accounts:new(AccountPubkey, 0, Height),
            AccountsTrees = aec_accounts_trees:enter(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            {ok, Trees}
    end.
