%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Common utility functions for AE transactions
%%% @end
%%%=============================================================================
-module(aetx_utils).

%% API
-export([check_account/3,
         check_account/4,
         check_nonce/2,
         check_ttl/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% Checks that an account (PubKey) exist at this height, has enough funds,
%% and that the Nonce is ok.
-spec check_account(Account :: aec_keys:pubkey(),
                    Trees   :: aec_trees:trees(),
                    Nonce   :: non_neg_integer(),
                    Amount  :: non_neg_integer()) -> ok | {error, term()}.
check_account(AccountPubKey, Trees, Nonce, Amount) ->
    case get_account(AccountPubKey, Trees) of
        {value, Account} ->
            BalanceOk = check_balance(Account, Amount),
            NonceOk   = check_nonce(Account, Nonce),
            checks_ok([BalanceOk, NonceOk]);
        none ->
            {error, account_not_found}
    end.

-spec check_account(Account :: aec_keys:pubkey(),
                    Trees   :: aec_trees:trees(),
                    Amount  :: non_neg_integer()) -> ok | {error, term()}.
check_account(AccountPubKey, Trees, Amount) ->
    case get_account(AccountPubKey, Trees) of
        {value, Account} ->
            BalanceOk = check_balance(Account, Amount),
            checks_ok([BalanceOk]);
        none ->
            {error, account_not_found}
    end.

-spec check_ttl(non_neg_integer(), non_neg_integer()) ->
        ok | {error, ttl_expired}.
check_ttl(TTL, Height) ->
    case TTL >= Height of
      	true -> ok;
      	false -> {error, ttl_expired}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_account(aec_keys:pubkey(), aec_trees:trees()) ->
        none | {value, aec_accounts:account()}.
get_account(AccountPubKey, Trees) ->
    AccountsTrees = aec_trees:accounts(Trees),
    aec_accounts_trees:lookup(AccountPubKey, AccountsTrees).

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
        ok | {error, account_nonce_too_high | account_nonce_too_low}.
check_nonce(Account, Nonce) ->
    AccountNonce = aec_accounts:nonce(Account),
    if
        Nonce =:= (AccountNonce + 1) -> ok;
        Nonce =< AccountNonce -> {error, account_nonce_too_high};
        Nonce > AccountNonce -> {error, account_nonce_too_low}
    end.

-spec checks_ok(list(ok | {error, term()})) -> ok | {error, term()}.
checks_ok([])        -> ok;
checks_ok([ok | Xs]) -> checks_ok(Xs);
checks_ok([Err | _]) -> Err.
