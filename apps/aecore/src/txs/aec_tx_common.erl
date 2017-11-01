-module(aec_tx_common).

-export([ensure_account_at_height/3]).

-include("common.hrl").
-include("trees.hrl").

-spec ensure_account_at_height(pubkey(), trees(), height()) ->
                                   {ok, trees()} | {error, account_height_too_big}.
ensure_account_at_height(AccountPubkey, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts:get(AccountPubkey, AccountsTrees0) of
        {ok, #account{height = AccountCurrentHeight}} ->
            case AccountCurrentHeight =< Height of
                true ->
                    {ok, Trees0};
                false ->
                    {error, account_height_too_big}
            end;
        {error, notfound} ->
            %% Add newly referenced account (w/0 amount) to the state
            Account = aec_accounts:new(AccountPubkey, 0, Height),
            {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            {ok, Trees}
    end.
