-module(aec_next_nonce).

-export([pick_for_account/1]).

-include("common.hrl").
-include("blocks.hrl").

%% It assumes that in order to pick a nonce for a transaction
%% account has to be present in state tree.
%% It implies that user cannot pick a nonce to create a transaction (e.g. spend tx)
%% to put it into the mempool until either
%% - some funds are transferred to user's account
%% or
%% - user mined a block, which was already added to the chain.
-spec pick_for_account(pubkey()) -> {ok, non_neg_integer()} |
                                    {error, account_not_found}.
pick_for_account(Pubkey) ->
    case get_state_tree_nonce(Pubkey) of
        {ok, StateTreeNonce} ->
            MempoolNonce = get_mempool_nonce(Pubkey),
            NextNonce = pick_higher_nonce(StateTreeNonce, MempoolNonce) + 1,
            {ok, NextNonce};
        {error, account_not_found} = Error ->
            Error
    end.

%% Internals

-spec get_state_tree_nonce(pubkey()) -> {ok, non_neg_integer()} |
                                        {error, account_not_found}.
get_state_tree_nonce(AccountPubkey) ->
    case aec_chain:get_account(AccountPubkey) of
        {value, Account} ->
            {ok, aec_accounts:nonce(Account)};
        none ->
            {error, account_not_found}
    end.

-spec get_mempool_nonce(pubkey()) -> integer().
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
