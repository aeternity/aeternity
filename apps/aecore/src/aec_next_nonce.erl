-module(aec_next_nonce).

-export([pick_for_account/1,
         pick_for_account/2]).

-include("blocks.hrl").

%% It assumes that in order to pick a nonce for a transaction
%% account has to be present in state tree.
%% It implies that user cannot pick a nonce to create a transaction (e.g. spend tx)
%% to put it into the mempool until either
%% - some funds are transferred to user's account
%% or
%% - user mined a block, which was already added to the chain.
-spec pick_for_account(aec_keys:pubkey()) -> {ok, non_neg_integer()} |
                                    {error, account_not_found}.
pick_for_account(Pubkey) ->
    pick_for_account(Pubkey, max).

-spec pick_for_account(aec_keys:pubkey(), max | continuity) ->
    {ok, non_neg_integer()} | {error, account_not_found}.
pick_for_account(Pubkey, Strategy) ->
    case get_state_tree_nonce(Pubkey) of
        generalized_account -> {ok, 0};
        {ok, StateTreeNonce} ->
            case Strategy of
                max ->
                    MempoolNonce = get_mempool_nonce(Pubkey),
                    NextNonce = pick_higher_nonce(StateTreeNonce, MempoolNonce) + 1,
                    {ok, NextNonce};
                continuity ->
                    {ok, AllTxs} = aec_tx_pool:peek(infinity, Pubkey),
                    %% get all nonces from the pool sorted asc
                    AllNoncesInPool = lists:sort([aetx:nonce(aetx_sign:tx(T)) ||
                                                      T <- AllTxs]),
                    %% prepend the account nonce this relies on it being
                    %% smallest number (we don't keep invalid txs in the pool)
                    AllNonces = [StateTreeNonce | AllNoncesInPool],
                    Missing =
                        lists:foldl(
                            fun(_, {missing, M}) -> {missing, M};
                               (Current, Expected) ->
                                  case Current =:= Expected of
                                      true -> Expected + 1;
                                      false -> {missing, Expected}
                                  end
                            end,
                            StateTreeNonce,
                            AllNonces),
                    case Missing of
                        {missing, M} -> {ok, M};
                        _ ->
                            Greatest = hd(lists:reverse(AllNonces)),
                            {ok, Greatest + 1}
                    end
            end;
        {error, account_not_found} = Error ->
            Error
    end.

%% Internals

-spec get_state_tree_nonce(aec_keys:pubkey()) -> {ok, non_neg_integer()} |
                                                 generalized_account |
                                                 {error, account_not_found}.
get_state_tree_nonce(AccountPubkey) ->
    case aec_chain:get_account(AccountPubkey) of
        {value, Account} ->
            case aec_accounts:type(Account) of
                basic ->
                    {ok, aec_accounts:nonce(Account)};
                generalized -> generalized_account
            end;
        none ->
            {error, account_not_found}
    end.

-spec get_mempool_nonce(aec_keys:pubkey()) -> integer().
get_mempool_nonce(AccountPubkey) ->
    case aec_tx_pool:get_max_nonce(AccountPubkey) of
        {ok, Nonce} ->
            Nonce;
        undefined ->
            -1
    end.

-spec pick_higher_nonce(non_neg_integer(), integer()) -> non_neg_integer().
pick_higher_nonce(Nonce1, Nonce2) when Nonce1 >= Nonce2 ->
    Nonce1;
pick_higher_nonce(_Nonce1, Nonce2) ->
    Nonce2.
