%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Common utility functions for AE transactions
%%% @end
%%%=============================================================================
-module(aetx_utils).

%% API
-export([check_account/3,
         check_account/5,
         check_ttl/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% Checks that an account (PubKey) exist at this height, has enough funds,
%% and that the Nonce is ok.
-spec check_account(Account :: aec_keys:pubkey(),
                    Trees   :: aec_trees:trees(),
                    Nonce   :: non_neg_integer(),
                    Amount  :: non_neg_integer(),
                    Env     :: aetx_env:env()   )
    -> ok |
       {error, account_not_found |
               insufficient_funds |
               tx_nonce_already_used_for_account |
               tx_nonce_too_high_for_account |
               nonce_in_ga_tx_should_be_zero |
               bad_ga_tx_env}.
check_account(AccountPubKey, Trees, Nonce, Amount, Env) ->
    case get_account(AccountPubKey, Trees) of
        {value, Account} ->
            BalanceOk = check_balance(Account, Amount),
            NonceOk   = check_nonce(Account, Nonce, Env),
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

-spec check_nonce(aec_accounts:account(), non_neg_integer(), aetx_env:env()) ->
        ok | {error, atom()}.
check_nonce(Account, Nonce, Env) ->
    case aec_accounts:type(Account) of
        basic ->
            ANonce = aec_accounts:nonce(Account),
            if
                Nonce == (ANonce + 1) -> ok;
                Nonce =< ANonce       -> {error, tx_nonce_already_used_for_account};
                Nonce >  ANonce       -> {error, tx_nonce_too_high_for_account}
            end;
        generalized ->
            GANonce = aetx_env:ga_nonce(Env, aec_accounts:pubkey(Account)),
            if Nonce =/= 0     -> {error, nonce_in_ga_tx_should_be_zero};
               GANonce == none -> {error, bad_ga_tx_env};
               true            -> ok
            end
    end.

-spec checks_ok(list(ok | {error, term()})) -> ok | {error, term()}.
checks_ok([])        -> ok;
checks_ok([ok | Xs]) -> checks_ok(Xs);
checks_ok([Err | _]) -> Err.
