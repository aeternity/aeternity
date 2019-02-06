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
         lookup_account/2,
         set_account_balance/3,
         set_account_nonce/3,
         next_nonce/2,
         setup_new_account/1]).

-export([close_solo/1,
         close_solo/2,
         get_channel/2,
         lookup_channel/2,
         set_channel/2]).
-export([apply_on_trees_without_sigs_check/3,
         apply_on_trees_without_sigs_check_with_env/3]).

-export([create_tx_spec/3,
         create_tx_spec/4,

         close_mutual_tx_spec/3,

         payload/5,
         proof_of_inclusion/1,

         close_solo_tx_spec/5,
         close_solo_tx_spec/6,

         slash_tx_spec/5,
         slash_tx_spec/6,

         deposit_tx_spec/3,
         deposit_tx_spec/4,

         withdraw_tx_spec/3,
         withdraw_tx_spec/4,

         settle_tx_spec/3,
         settle_tx_spec/4,

         snapshot_solo_tx_spec/4,
         snapshot_solo_tx_spec/5,

         force_progress_tx_spec/8,
         force_progress_tx_spec/9
        ]).

-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).

%%%===================================================================
%%% Test state
%%%===================================================================

new_state() ->
    #{}.

trees(#{} = S) ->
    maps:get(trees, S, aec_trees:new_without_backend()).

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
    setup_new_account(1000000000000, State).

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

lookup_account(PubKey, State) ->
    aec_accounts_trees:lookup(PubKey, aec_trees:accounts(trees(State))).

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

    {InitiatorEndBalance, ResponderEndBalance} =
        case Params of
            #{initiator_amount := IAmt, responder_amount := RAmt} ->
                {IAmt, RAmt};
            _ ->
                ChannelAmount = aesc_channels:channel_amount(Ch),
                IAmt = rand:uniform(ChannelAmount),
                RAmt = ChannelAmount - IAmt,
                {IAmt, RAmt}
        end,

    DummyState = state_tx(aesc_channels:pubkey(Ch),
                          aesc_channels:initiator(Ch),
                          aesc_channels:responder(Ch),
                          maps:merge(Params, #{initiator_amount => InitiatorEndBalance,
                                                 responder_amount => ResponderEndBalance})),
    PoI = proof_of_inclusion([{aesc_channels:initiator(Ch), InitiatorEndBalance},
                              {aesc_channels:responder(Ch), ResponderEndBalance}]),

    {channel_offchain_tx, DummyStateTx} = aetx:specialize_type(DummyState),
    aesc_channels:close_solo(Ch, DummyStateTx, PoI, 11).

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

apply_on_trees_without_sigs_check([SignedTx], Trees, Height) ->
    Env0 = aetx_env:tx_env(Height),
    apply_on_trees_without_sigs_check_with_env([SignedTx], Trees, Env0).

apply_on_trees_without_sigs_check_with_env([SignedTx], Trees, Env0) ->
    Tx = aetx_sign:tx(SignedTx),
    Env = aetx_env:set_signed_tx(Env0, {value, SignedTx}),
    {ok, Trees1} = aetx:process(Tx, Trees, Env),
    {ok, [SignedTx], Trees1}.

%%%===================================================================
%%% Create tx
%%%===================================================================

create_tx_spec(InitiatorPubKey, ResponderPubKey, State) ->
    create_tx_spec(InitiatorPubKey, ResponderPubKey, #{}, State).

create_tx_spec(InitiatorPubKey, ResponderPubKey, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(InitiatorPubKey, State), Spec0),
    InitiatorAmount = maps:get(initiator_amount, Spec),
    ResponderAmount = maps:get(responder_amount, Spec),
    Accounts = [aec_accounts:new(Pubkey, Balance) ||
                {Pubkey, Balance} <- [{InitiatorPubKey, InitiatorAmount},
                                      {ResponderPubKey, ResponderAmount}]],
    Trees = aec_test_utils:create_state_tree_with_accounts(Accounts, no_backend),
    StateHash = aec_trees:hash(Trees),
    Spec#{initiator_id       => aec_id:create(account, InitiatorPubKey),
          responder_id       => aec_id:create(account, ResponderPubKey),
          state_hash         => StateHash}.

create_tx_default_spec(InitiatorPubKey, State) ->
    #{initiator_amount   => 50,
      responder_amount   => 50,
      channel_reserve    => 20,
      lock_period        => 100,
      ttl                => 0,
      fee                => 50000,
      nonce              => try next_nonce(InitiatorPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Close mutual tx
%%%===================================================================

close_mutual_tx_spec(ChannelId, Spec0, State) ->
    Initiator = maps:get(initiator_account, Spec0),
    Spec = maps:merge(close_mutual_tx_default_spec(Initiator, State), Spec0),
    #{channel_id              => aec_id:create(channel, ChannelId),
      from_id                 => aec_id:create(account, Initiator),
      initiator_amount_final  => maps:get(initiator_amount_final, Spec),
      responder_amount_final  => maps:get(responder_amount_final, Spec),
      ttl                     => maps:get(ttl, Spec, 0),
      fee                     => maps:get(fee, Spec),
      nonce                   => maps:get(nonce, Spec)}.

close_mutual_tx_default_spec(Initiator, State) ->
    #{initiator_amount_final => 10,
      responder_amount_final => 10,
      fee                    => 50000,
      nonce                  => try next_nonce(Initiator, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Close solo tx
%%%===================================================================

close_solo_tx_spec(ChannelPubKey, FromPubKey, Payload, PoI, State) ->
    close_solo_tx_spec(ChannelPubKey, FromPubKey, Payload, PoI, #{}, State).

close_solo_tx_spec(ChannelPubKey, FromPubKey, Payload, PoI, Spec0, State) ->
    Spec = maps:merge(close_solo_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id  => aec_id:create(channel, ChannelPubKey),
      from_id     => aec_id:create(account, FromPubKey),
      payload     => Payload,
      poi         => PoI,
      ttl         => maps:get(ttl, Spec, 0),
      fee         => maps:get(fee, Spec),
      nonce       => maps:get(nonce, Spec)}.

close_solo_tx_default_spec(FromPubKey, State) ->
    #{fee         => 50000,
      nonce       => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Deposit tx
%%%===================================================================

deposit_tx_spec(ChannelId, FromPubKey, State) ->
    deposit_tx_spec(ChannelId, FromPubKey, #{}, State).

deposit_tx_spec(ChannelId, FromPubKey, Spec0, State) ->
    Spec = maps:merge(deposit_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id => aec_id:create(channel, ChannelId),
      from_id    => aec_id:create(account, FromPubKey),
      amount     => maps:get(amount, Spec),
      ttl        => maps:get(ttl, Spec, 0),
      fee        => maps:get(fee, Spec),
      state_hash => maps:get(state_hash, Spec),
      round      => maps:get(round, Spec),
      nonce      => maps:get(nonce, Spec)}.

deposit_tx_default_spec(FromPubKey, State) ->
    #{amount      => 10,
      fee         => 50000,
      state_hash  => ?BOGUS_STATE_HASH,
      round       => 42,
      nonce       => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Withdraw tx
%%%===================================================================

withdraw_tx_spec(ChannelId, ToPubKey, State) ->
    withdraw_tx_spec(ChannelId, ToPubKey, #{}, State).

withdraw_tx_spec(ChannelId, ToPubKey, Spec0, State) ->
    Spec = maps:merge(withdraw_tx_spec(ToPubKey, State), Spec0),
    #{channel_id => aec_id:create(channel, ChannelId),
      to_id      => aec_id:create(account, ToPubKey),
      amount     => maps:get(amount, Spec),
      ttl        => maps:get(ttl, Spec, 0),
      fee        => maps:get(fee, Spec),
      state_hash => maps:get(state_hash, Spec),
      round      => maps:get(round, Spec),
      nonce      => maps:get(nonce, Spec)}.

withdraw_tx_spec(ToPubKey, State) ->
    #{amount      => 10,
      fee         => 50000,
      state_hash  => ?BOGUS_STATE_HASH,
      round       => 42,
      nonce       => try next_nonce(ToPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Slash tx
%%%===================================================================

slash_tx_spec(ChannelId, FromPubKey, Payload, PoI, State) ->
    slash_tx_spec(ChannelId, FromPubKey, Payload, PoI, #{}, State).

slash_tx_spec(ChannelId, FromPubKey, Payload, PoI, Spec0, State) ->
    Spec = maps:merge(slash_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id  => aec_id:create(channel, ChannelId),
      from_id     => aec_id:create(account, FromPubKey),
      payload     => Payload,
      poi         => PoI,
      ttl         => maps:get(ttl, Spec, 0),
      fee         => maps:get(fee, Spec),
      nonce       => maps:get(nonce, Spec)}.

slash_tx_default_spec(FromPubKey, State) ->
    #{fee         => 50000,
      nonce       => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Settle tx
%%%===================================================================

settle_tx_spec(ChannelId, FromPubKey, State) ->
    settle_tx_spec(ChannelId, FromPubKey, #{}, State).

settle_tx_spec(ChannelId, FromPubKey, Spec0, State) ->
    Spec = maps:merge(settle_tx_default_spec(FromPubKey, State), Spec0),
    #{channel_id              => aec_id:create(channel, ChannelId),
      from_id                 => aec_id:create(account, FromPubKey),
      initiator_amount_final  => maps:get(initiator_amount, Spec),
      responder_amount_final  => maps:get(responder_amount, Spec),
      ttl                     => maps:get(ttl, Spec, 0),
      fee                     => maps:get(fee, Spec),
      nonce                   => maps:get(nonce, Spec)}.

settle_tx_default_spec(FromPubKey, State) ->
    #{initiator_amount => 10,
      responder_amount => 10,
      fee              => 50000,
      nonce            => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

%%%===================================================================
%%% Snapshot solo tx
%%%===================================================================

snapshot_solo_tx_spec(ChannelPubKey, FromPubKey, Payload, State) ->
    snapshot_solo_tx_spec(ChannelPubKey, FromPubKey, Payload, #{}, State).

snapshot_solo_tx_spec(ChannelPubKey, FromPubKey, Payload, Spec0, State) ->
    Spec = maps:merge(snapshot_solo_tx_default_spec(FromPubKey, State), Spec0),
    Spec#{channel_id  => aec_id:create(channel, ChannelPubKey),
          from_id     => aec_id:create(account, FromPubKey),
          payload     => Payload,
          ttl         => maps:get(ttl, Spec, 0)}.

snapshot_solo_tx_default_spec(FromPubKey, State) ->
    #{initiator_amount => 10,
      responder_amount => 10,
      fee              => 50000,
      nonce            => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

state_tx(ChannelPubKey, Initiator, Responder, Spec0) ->
    Spec = maps:merge(state_tx_spec(), Spec0),
    StateHash =
        case maps:get(state_hash, Spec, <<>>) of
            <<>> -> %not set, calculate
                InitiatorAmount = maps:get(initiator_amount, Spec),
                ResponderAmount = maps:get(responder_amount, Spec),
                Trees0 = aec_trees:new_without_backend(),
                Accounts =
                    lists:foldl(
                        fun({Pubkey, Amount}, AccTree) ->
                        Account = aec_accounts:new(Pubkey, Amount),
                        aec_accounts_trees:enter(Account, AccTree)
                    end,
                    aec_trees:accounts(Trees0),
                    [{Initiator, InitiatorAmount},
                     {Responder, ResponderAmount}]),
                Trees = aec_trees:set_accounts(Trees0, Accounts),
                aec_trees:hash(Trees);
            V -> V
        end,
    {ok, StateTx} =
        aesc_offchain_tx:new(
            #{channel_id         => aec_id:create(channel, ChannelPubKey),
              updates            => maps:get(updates, Spec, []),
              state_hash         => StateHash,
              round              => maps:get(round, Spec)}),
    StateTx.

state_tx_spec() ->
    #{initiator_amount   => 3,
      responder_amount   => 4,
      state              => <<"state..">>,
      round              => 11}.

payload(ChannelId, Initiator, Responder, SignersPrivKeys, Spec) ->
    StateTx = state_tx(ChannelId, Initiator, Responder, Spec),
    aetx_sign:serialize_to_binary(aec_test_utils:sign_tx(StateTx, SignersPrivKeys)). %% No signatures

proof_of_inclusion(Participants) ->
    Accounts = [aec_accounts:new(Pubkey, Balance) ||
                {Pubkey, Balance} <- Participants],
    Trees = aec_test_utils:create_state_tree_with_accounts(Accounts, no_backend),
    lists:foldl(
        fun({Pubkey, _}, AccumPoI) ->
            {ok, AccumPoI1} = aec_trees:add_poi(accounts, Pubkey,
                                                Trees, AccumPoI),
            AccumPoI1
        end,
        aec_trees:new_poi(Trees),
        Participants).

%%%===================================================================
%%% Force progress
%%%===================================================================

force_progress_tx_spec(ChannelId, FromPubKey, Payload, Update, StateHash,
                       Round, OffChainTrees, State) ->
    force_progress_tx_spec(ChannelId, FromPubKey, Payload, Update, StateHash,
                       Round, OffChainTrees, #{}, State).

force_progress_tx_spec(ChannelId, FromPubKey, Payload, Update, StateHash,
                       Round, OffChainTrees, Spec0, State) ->
    Spec = maps:merge(force_progress_default_spec(FromPubKey, State), Spec0),
    Spec#{channel_id      => aec_id:create(channel, ChannelId),
          from_id         => aec_id:create(account, FromPubKey),
          payload         => Payload,
          update          => Update,
          state_hash      => StateHash,
          round           => Round,
          offchain_trees  => OffChainTrees,
          ttl             => maps:get(ttl, Spec, 0)}.

force_progress_default_spec(FromPubKey, State) ->
    #{fee              => 50000,
      nonce            => try next_nonce(FromPubKey, State) catch _:_ -> 0 end}.

