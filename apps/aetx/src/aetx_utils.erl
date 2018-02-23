%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Common utility functions for AE transactions
%%% @end
%%%=============================================================================
-module(aetx_utils).

-include_lib("apps/aecore/include/common.hrl").

-export([check_account/5]).

%% Checks that an account (PubKey) exist at this height, has enough funds,
%% and that the Nonce is ok.
-spec check_account(Account :: pubkey(),
                    Trees   :: aec_trees:trees(),
                    Height  :: height(),
                    Nonce   :: non_neg_integer(),
                    Amount  :: non_neg_integer()) -> ok | {error, term()}.
check_account(AccountPubKey, Trees, Height, Nonce, Amount) ->
    AccountsTrees = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(AccountPubKey, AccountsTrees) of
        {value, Account} ->
            BalanceOk = check_balance(Account, Amount),
            HeightOk  = check_height(Account, Height),
            NonceOk   = check_nonce(Account, Nonce),
            checks_ok([BalanceOk, HeightOk, NonceOk]);
        none ->
            {error, account_not_found}
    end.


-spec check_balance(aec_accounts:account(), non_neg_integer()) ->
                           ok | {error, insufficient_funds}.
check_balance(Account, Amount) ->
    case aec_accounts:balance(Account) >= Amount of
        true ->
            ok;
        false ->
            {error, insufficient_funds}
    end.

-spec check_nonce(aec_accounts:account(), non_neg_integer()) ->
                         ok | {error, account_nonce_too_high}.
check_nonce(Account, Nonce) ->
    case Nonce > aec_accounts:nonce(Account) of
        true ->
            ok;
        false ->
            {error, account_nonce_too_high}
    end.

-spec check_height(aec_accounts:account(), height()) ->
                          ok | {error, account_height_too_big}.
check_height(Account, Height) ->
    case aec_accounts:height(Account) =< Height of
        true ->
            ok;
        false ->
            {error, sender_account_height_too_big}
    end.

-spec checks_ok(list(ok | {error, term()})) -> ok | {error, term()}.
checks_ok([])        -> ok;
checks_ok([ok | Xs]) -> checks_ok(Xs);
checks_ok([Err | _]) -> Err.
