%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Test utils for State Channels
%%% @end
%%%=============================================================================

-module(aesc_test_utils).

%% API
-export([new_state/0,
         trees/1,
         set_trees/2,
         priv_key/2,
         set_account_balance/3,
         set_account_nonce/3,
         setup_new_account/1]).
-export([create_tx_spec/3,
         create_tx_spec/4]).

-include_lib("apps/aechannel/include/channel_txs.hrl").

%%%===================================================================
%%% Test state
%%%===================================================================

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

set_account_nonce(PubKey, Nonce, State) ->
    A  = get_account(PubKey, State),
    A2 = aec_accounts:set_nonce(A, Nonce),
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
%%% Create tx
%%%===================================================================

create_tx_spec(InitiatorPubKey, ParticipantPubKey, State) ->
    create_tx_spec(InitiatorPubKey, ParticipantPubKey, #{}, State).

create_tx_spec(InitiatorPubKey, ParticipantPubKey, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(InitiatorPubKey, State), Spec0),
    #{initiator          => InitiatorPubKey,
      initiator_amount   => maps:get(initiator_amount, Spec),
      participant        => ParticipantPubKey,
      participant_amount => maps:get(participant_amount, Spec),
      channel_reserve    => maps:get(channel_reserve, Spec),
      lock_period        => maps:get(lock_period, Spec),
      channel_reserve    => maps:get(channel_reserve, Spec),
      fee                => maps:get(fee, Spec),
      nonce              => maps:get(nonce, Spec)}.

create_tx_default_spec(InitiatorPubKey, State) ->
    #{initiator_amount   => 50,
      participant_amount => 50,
      channel_reserve    => 20,
      lock_period        => 100,
      channel_reserve    => 10,
      fee                => 3,
      nonce              => try next_nonce(InitiatorPubKey, State) catch _:_ -> 0 end}.
