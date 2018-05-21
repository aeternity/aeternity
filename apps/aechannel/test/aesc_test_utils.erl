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
         get_account/2,
         set_account_balance/3,
         set_account_nonce/3,
         setup_new_account/1]).

-export([close_solo/1,
         close_solo/2,
         get_channel/2,
         lookup_channel/2,
         set_channel/2]).
-export([apply_on_trees_without_sigs_check/4]).

-export([create_tx_spec/3,
         create_tx_spec/4,

         close_mutual_tx_spec/3,

         payload/5,

         close_solo_tx_spec/4,
         close_solo_tx_spec/5,

         slash_tx_spec/4,
         slash_tx_spec/5,

         deposit_tx_spec/3,
         deposit_tx_spec/4,

         withdraw_tx_spec/3,
         withdraw_tx_spec/4,

         settle_tx_spec/3,
         settle_tx_spec/4]).

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
    setup_new_account(1000, State).

set_account_balance(PubKey, NewBalance, State) ->
    A        = get_account(PubKey, State),
    Balance  = aec_accounts:balance(A),
    Nonce    = aec_accounts:nonce(A),
    {ok, A1} = aec_accounts:spend(A, Balance, Nonce),
    {ok, A2} = aec_accounts:earn(A1, NewBalance),
    set_account(A2, State).

set_account_nonce(PubKey, Nonce, State) ->
    A  = get_account(PubKey, State),
    A2 = aec_accounts:set_nonce(A, Nonce),
    set_account(A2, State).

get_account(PubKey, State) ->
    aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(State))).

setup_new_account(Balance, State) ->
    {PubKey, PrivKey} = new_key_pair(),
    State1 = insert_key_pair(PubKey, PrivKey, State),
    State2 = set_account(aec_accounts:new(PubKey, Balance), State1),
    {PubKey, State2}.

new_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.

set_account(Account, State) ->
    Trees   = trees(State),
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    set_trees(aec_trees:set_accounts(Trees, AccTree), State).

%%%===================================================================
%%% Channel
%%%===================================================================

close_solo(Ch) ->
    close_solo(Ch, #{}).

close_solo(Ch, Params) ->
    DummyStateTx = state_tx(aesc_channels:id(Ch),
                            aesc_channels:initiator(Ch),
                            aesc_channels:responder(Ch),
                            Params),
    {_Type, Tx} = aetx:specialize_type(DummyStateTx),
    aesc_channels:close_solo(Ch, Tx, 11).

get_channel(ChannelId, State) ->
    aesc_state_tree:get(ChannelId, aec_trees:channels(trees(State))).

lookup_channel(ChannelId, State) ->
    aesc_state_tree:lookup(ChannelId, aec_trees:channels(trees(State))).

set_channel(Channel, State) ->
    Trees  = trees(State),
    ChTree = aesc_state_tree:enter(Channel, aec_trees:channels(Trees)),
    set_trees(aec_trees:set_channels(Trees, ChTree), State).

%%%===================================================================
%%% Other utils
%%%===================================================================

apply_on_trees_without_sigs_check([SignedTx], Trees, Height, ConsensusVersion) ->
    Tx = aetx_sign:tx(SignedTx),
    {ok, Trees1} = aetx:check(Tx, Trees, Height, ConsensusVersion),
    {ok, Trees2} = aetx:process(Tx, Trees1, Height, ConsensusVersion),
    {ok, [SignedTx], Trees2}.

%%%===================================================================
%%% Create tx
%%%===================================================================

create_tx_spec(InitiatorPubKey, ResponderPubKey, State) ->
    create_tx_spec(InitiatorPubKey, ResponderPubKey, #{}, State).

create_tx_spec(InitiatorPubKey, ResponderPubKey, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(InitiatorPubKey, State), Spec0),
    #{initiator          => InitiatorPubKey,
      initiator_amount   => maps:get(initiator_amount, Spec),
      responder          => ResponderPubKey,
      responder_amount   => maps:get(responder_amount, Spec),
      channel_reserve    => maps:get(channel_reserve, Spec),
      lock_period        => maps:get(lock_period, Spec),
      ttl                => maps:get(ttl, Spec),
      fee                => maps:get(fee, Spec),
      nonce              => maps:get(nonce, Spec)}.

create_tx_default_spec(InitiatorPubKey, State) ->
    #{initiator_amount   => 50,
      responder_amount   => 50,
      channel_reserve    => 20,
      lock_period        => 100,
      ttl                => 100,
      fee                => 3,
      nonce              => try next_nonce(InitiatorPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Close mutual tx
%%%===================================================================

close_mutual_tx_spec(ChannelId, Spec0, State) ->
    Initiator = maps:get(initiator_account, Spec0),
    Spec = maps:merge(close_mutual_tx_default_spec(Initiator, State), Spec0),
    #{channel_id        => ChannelId,
      initiator_amount  => maps:get(initiator_amount, Spec),
      responder_amount  => maps:get(responder_amount, Spec),
      ttl               => maps:get(ttl, Spec),
      fee               => maps:get(fee, Spec),
      nonce             => maps:get(nonce, Spec)}.

close_mutual_tx_default_spec(Initiator, State) ->
    #{initiator_amount => 10,
      responder_amount => 10,
      ttl              => 100,
      fee              => 3,
      nonce            => try next_nonce(Initiator, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Close solo tx
%%%===================================================================

close_solo_tx_spec(ChannelId, FromPubKey, Payload, State) ->
    close_solo_tx_spec(ChannelId, FromPubKey, Payload, #{}, State).

close_solo_tx_spec(ChannelId, FromPubKey, Payload, Spec0, State) ->
    Spec = maps:merge(close_solo_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id  => ChannelId,
      from        => FromPubKey,
      payload     => Payload,
      ttl         => maps:get(ttl, Spec),
      fee         => maps:get(fee, Spec),
      nonce       => maps:get(nonce, Spec)}.

close_solo_tx_default_spec(FromPubKey, State) ->
    #{ttl     => 100,
      fee     => 3,
      nonce   => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Deposit tx
%%%===================================================================

deposit_tx_spec(ChannelId, FromPubKey, State) ->
    deposit_tx_spec(ChannelId, FromPubKey, #{}, State).

deposit_tx_spec(ChannelId, FromPubKey, Spec0, State) ->
    Spec = maps:merge(deposit_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id => ChannelId,
      from       => FromPubKey,
      amount     => maps:get(amount, Spec),
      ttl        => maps:get(ttl, Spec),
      fee        => maps:get(fee, Spec),
      nonce      => maps:get(nonce, Spec)}.

deposit_tx_default_spec(FromPubKey, State) ->
    #{amount => 10,
      ttl    => 100,
      fee    => 3,
      nonce  => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Withdraw tx
%%%===================================================================

withdraw_tx_spec(ChannelId, ToPubKey, State) ->
    withdraw_tx_spec(ChannelId, ToPubKey, #{}, State).

withdraw_tx_spec(ChannelId, ToPubKey, Spec0, State) ->
    Spec = maps:merge(withdraw_tx_spec(ToPubKey, State), Spec0),
    #{channel_id => ChannelId,
      to         => ToPubKey,
      amount     => maps:get(amount, Spec),
      ttl        => maps:get(ttl, Spec),
      fee        => maps:get(fee, Spec),
      nonce      => maps:get(nonce, Spec)}.

withdraw_tx_spec(ToPubKey, State) ->
    #{amount => 10,
      ttl    => 100,
      fee    => 3,
      nonce  => try next_nonce(ToPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Slash tx
%%%===================================================================

slash_tx_spec(ChannelId, FromPubKey, Payload, State) ->
    slash_tx_spec(ChannelId, FromPubKey, Payload, #{}, State).

slash_tx_spec(ChannelId, FromPubKey, Payload, Spec0, State) ->
    Spec = maps:merge(slash_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id  => ChannelId,
      from        => FromPubKey,
      payload     => Payload,
      ttl         => maps:get(ttl, Spec),
      fee         => maps:get(fee, Spec),
      nonce       => maps:get(nonce, Spec)}.

slash_tx_default_spec(FromPubKey, State) ->
    #{ttl     => 100,
      fee     => 3,
      nonce   => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Settle tx
%%%===================================================================

settle_tx_spec(ChannelId, FromPubKey, State) ->
    settle_tx_spec(ChannelId, FromPubKey, #{}, State).

settle_tx_spec(ChannelId, FromPubKey, Spec0, State) ->
    Spec = maps:merge(settle_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id        => ChannelId,
      from              => FromPubKey,
      initiator_amount  => maps:get(initiator_amount, Spec),
      responder_amount  => maps:get(responder_amount, Spec),
      ttl               => maps:get(ttl, Spec),
      fee               => maps:get(fee, Spec),
      nonce             => maps:get(nonce, Spec)}.

settle_tx_default_spec(FromPubKey, State) ->
    #{initiator_amount => 10,
      responder_amount => 10,
      ttl              => 100,
      fee              => 3,
      nonce            => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.


state_tx(ChannelId, Initiator, Responder) ->
    state_tx(ChannelId, Initiator, Responder, #{}).

state_tx(ChannelId, Initiator, Responder, Spec0) ->
    Spec = maps:merge(state_tx_spec(), Spec0),
    {ok, StateTx} =
        aesc_offchain_tx:new(
            #{channel_id         => ChannelId,
              initiator          => Initiator,
              responder          => Responder,
              updates            => maps:get(updates, Spec, []),
              initiator_amount   => maps:get(initiator_amount, Spec),
              responder_amount   => maps:get(responder_amount, Spec),
              state              => maps:get(state, Spec, <<>>),
              previous_round     => maps:get(previous_round, Spec),
              round              => maps:get(round, Spec)}),
    StateTx.

state_tx_spec() ->
    #{initiator_amount   => 3,
      responder_amount   => 4,
      state              => <<"state..">>,
      previous_round     => 10,
      round              => 11}.

payload(ChannelId, Initiator, Responder, SignersPrivKeys, Spec) ->
    StateTx = state_tx(ChannelId, Initiator, Responder, Spec),
    aetx_sign:serialize_to_binary(aetx_sign:sign(StateTx, SignersPrivKeys)). %% No signatures
