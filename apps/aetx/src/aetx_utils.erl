%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Common utility functions for AE transactions
%%% @end
%%%=============================================================================
-module(aetx_utils).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/trees.hrl").

-export([check_account/5]).

%% Checks that an account (PubKey) exist at this height, has enough funds,
%% and that the Nonce is ok.
-spec check_account(Account :: pubkey(),
                    Trees   :: trees(),
                    Height  :: height(),
                    Nonce   :: non_neg_integer(),
                    Amount  :: non_neg_integer()) -> ok | {error, term()}.
check_account(AccountPubKey, Trees, Height, Nonce, Amount) ->
    AccountsTrees = aec_trees:accounts(Trees),
    case aec_accounts:get(AccountPubKey, AccountsTrees) of
        {ok, #account{} = Account} ->
            BalanceOk = check_balance(Account, Amount),
            HeightOk  = check_height(Account, Height),
            NonceOk   = check_nonce(Account, Nonce),
            checks_ok([BalanceOk, HeightOk, NonceOk]);
        {error, notfound} ->
            {error, account_not_found}
    end.


-spec check_balance(account(), non_neg_integer()) ->
                           ok | {error, insufficient_funds}.
check_balance(#account{balance = Balance}, Amount) ->
    case Balance >= Amount of
        true ->
            ok;
        false ->
            {error, insufficient_funds}
    end.

-spec check_nonce(account(), non_neg_integer()) ->
                         ok | {error, account_nonce_too_high}.
check_nonce(#account{nonce = ANonce}, Nonce) ->
    case Nonce > ANonce of
        true ->
            ok;
        false ->
            {error, account_nonce_too_high}
    end.

-spec check_height(account(), height()) ->
                          ok | {error, account_height_too_big}.
check_height(#account{height = AHeight}, Height) ->
    case AHeight =< Height of
        true ->
            ok;
        false ->
            {error, sender_account_height_too_big}
    end.

-spec checks_ok(list(ok | {error, term()})) -> ok | {error, term()}.
checks_ok([])        -> ok;
checks_ok([ok | Xs]) -> checks_ok(Xs);
checks_ok([Err | _]) -> Err.
