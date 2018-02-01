%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Test utils for Naming System
%%% @end
%%%=============================================================================

-module(aens_test_utils).

%% API
-export([new_state/0,
         trees/1,
         set_trees/2,
         priv_key/2,
         setup_new_account/1,
         set_account_balance/3,
         revoke_name/2,
         preclaim_tx_spec/3,
         preclaim_tx_spec/4,
         claim_tx_spec/4,
         claim_tx_spec/5,
         update_tx_spec/3,
         update_tx_spec/4,
         transfer_tx_spec/4,
         transfer_tx_spec/5,
         revoke_tx_spec/3,
         revoke_tx_spec/4]).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aens/include/ns_txs.hrl").

%%%===================================================================
%%% Test state
%%%===================================================================

%% TODO: Move test state to a place so that it can be shared between apps,
%% e.g. aeoracle_SUITE uses similar code

new_state() ->
    #{}.

trees(#{} = S) ->
    maps:get(trees, S, aec_trees:new()).

set_trees(Trees, S) ->
    S#{trees => Trees}.

priv_key(PubKey, State) ->
    maps:get(PubKey, key_pairs(State)).

key_pairs(S) ->
    maps:get(key_pairs, S, #{}).

next_nonce(PubKey, S) ->
    Account = aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(S))),
    aec_accounts:nonce(Account) + 1.

insert_key_pair(Pub, Priv, S) ->
    Old = key_pairs(S),
    S#{key_pairs => Old#{Pub => Priv}}.

%%%===================================================================
%%% Accounts utils
%%%===================================================================

%% TODO: Move all account utils to a place so that it can be shared between apps,
%% e.g. aeoracle_SUITE uses similar code

-define(PRIV_SIZE, 32).

setup_new_account(State) ->
    setup_new_account(1000, 1, State).

set_account_balance(PubKey, NewBalance, State) ->
    A        = get_account(PubKey, State),
    Balance  = aec_accounts:balance(A),
    Height   = aec_accounts:height(A),
    Nonce    = aec_accounts:nonce(A),
    {ok, A1} = aec_accounts:spend(A, Balance, Nonce, Height),
    {ok, A2} = aec_accounts:earn(A1, NewBalance, Height),
    set_account(A2, State).

get_account(PubKey, State) ->
    aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(State))).

setup_new_account(Balance, Height, State) ->
    {PubKey, PrivKey} = new_key_pair(),
    State1 = insert_key_pair(PubKey, PrivKey, State),
    State2 = set_account(aec_accounts:new(PubKey, Balance, Height), State1),
    {PubKey, State2}.

new_key_pair() ->
    {Pubkey, PrivKey} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)),
    {Pubkey, pad_privkey(PrivKey)}.

pad_privkey(Bin) ->
    Pad = ?PRIV_SIZE - size(Bin),
    <<0:(Pad*8), Bin/binary>>.

set_account(Account, State) ->
    Trees   = trees(State),
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    set_trees(aec_trees:set_accounts(Trees, AccTree), State).

%%%===================================================================
%%% Names utils
%%%===================================================================

revoke_name(N, State) ->
    Trees = trees(State),
    N1 = aens_names:revoke(N, 5, 10),
    NSTree = aens_state_tree:enter_name(N1, aec_trees:ns(Trees)),
    set_trees(aec_trees:set_ns(Trees, NSTree), State).

%%%===================================================================
%%% Preclaim tx
%%%===================================================================

preclaim_tx_spec(PubKey, Commitment, State) ->
    preclaim_tx_spec(PubKey, Commitment, #{}, State).

preclaim_tx_spec(PubKey, Commitment, Spec0, State) ->
    Spec = maps:merge(preclaim_tx_default_spec(PubKey, State), Spec0),
    #{account    => PubKey,
      nonce      => maps:get(nonce, Spec),
      commitment => Commitment,
      fee        => maps:get(fee, Spec)}.

preclaim_tx_default_spec(PubKey, State) ->
    #{nonce => try next_nonce(PubKey, State) catch _:_ -> 0 end,
      fee   => 3}.

%%%===================================================================
%%% Claim tx
%%%===================================================================

claim_tx_spec(PubKey, Name, NameSalt, State) ->
    claim_tx_spec(PubKey, Name, NameSalt, #{}, State).

claim_tx_spec(PubKey, Name, NameSalt, Spec0, State) ->
    Spec = maps:merge(claim_tx_default_spec(PubKey, State), Spec0),
    #{account   => PubKey,
      nonce     => maps:get(nonce, Spec),
      name      => Name,
      name_salt => NameSalt,
      fee       => maps:get(fee, Spec)}.

claim_tx_default_spec(PubKey, State) ->
    #{nonce => try next_nonce(PubKey, State) catch _:_ -> 0 end,
      fee   => 3}.

%%%===================================================================
%%% Update tx
%%%===================================================================

update_tx_spec(PubKey, NameHash, State) ->
    update_tx_spec(PubKey, NameHash, #{}, State).

update_tx_spec(PubKey, NameHash, Spec0, State) ->
    Spec = maps:merge(update_tx_default_spec(PubKey, State), Spec0),
    #{account   => PubKey,
      nonce     => maps:get(nonce, Spec),
      name_hash => NameHash,
      name_ttl  => maps:get(name_ttl, Spec),
      pointers  => maps:get(pointers, Spec),
      ttl       => maps:get(ttl, Spec),
      fee       => maps:get(fee, Spec)}.

update_tx_default_spec(PubKey, State) ->
    #{nonce    => try next_nonce(PubKey, State) catch _:_ -> 0 end,
      name_ttl => 600000,
      pointers => [{<<"key">>, <<"val">>}],
      ttl      => 15,
      fee      => 3}.

%%%===================================================================
%%% Transfer tx
%%%===================================================================

transfer_tx_spec(PubKey, NameHash, RecipientAccount, State) ->
    transfer_tx_spec(PubKey, NameHash, RecipientAccount, #{}, State).

transfer_tx_spec(PubKey, NameHash, RecipientAccount, Spec0, State) ->
    Spec = maps:merge(transfer_tx_default_spec(PubKey, State), Spec0),
    #{account           => PubKey,
      nonce             => maps:get(nonce, Spec),
      name_hash         => NameHash,
      recipient_account => RecipientAccount,
      fee               => maps:get(fee, Spec)}.

transfer_tx_default_spec(PubKey, State) ->
    #{nonce => try next_nonce(PubKey, State) catch _:_ -> 0 end,
      fee   => 3}.

%%%===================================================================
%%% Revoke tx
%%%===================================================================

revoke_tx_spec(PubKey, NameHash, State) ->
    revoke_tx_spec(PubKey, NameHash, #{}, State).

revoke_tx_spec(PubKey, NameHash, Spec0, State) ->
    Spec = maps:merge(revoke_tx_default_spec(PubKey, State), Spec0),
    #{account   => PubKey,
      nonce     => maps:get(nonce, Spec),
      name_hash => NameHash,
      fee       => maps:get(fee, Spec)}.

revoke_tx_default_spec(PubKey, State) ->
    #{nonce => try next_nonce(PubKey, State) catch _:_ -> 0 end,
      fee   => 3}.
