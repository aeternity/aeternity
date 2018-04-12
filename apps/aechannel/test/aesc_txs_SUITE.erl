%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    CT test suite for AE State Channels on-chain transactions
%%% @end
%%%=============================================================================

-module(aesc_txs_SUITE).

%% common_test exports
-export([all/0,
         groups/0]).

%% test case exports
-export([create/1,
         create_negative/1,
         deposit/1,
         deposit_negative/1,
         withdraw/1,
         withdraw_negative/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [{group, transactions}]},
     {transactions, [sequence],
      [create,
       create_negative,
       deposit,
       deposit_negative,
       withdraw]
     }
    ].

%%%===================================================================
%%% Create
%%%===================================================================

create(Cfg) ->
    S = case proplists:get_value(state, Cfg) of
            undefined -> aesc_test_utils:new_state();
            State0    -> State0
        end,
    {PubKey1, S1} = aesc_test_utils:setup_new_account(S),
    {PubKey2, S2} = aesc_test_utils:setup_new_account(S1),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S2),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S2),

    %% Create Channel Create tx and apply it on trees
    Trees = aesc_test_utils:trees(S2),
    Height = 1,
    TxSpec = aesc_test_utils:create_tx_spec(PubKey1, PubKey2, S2),
    {ok, Tx} = aesc_create_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, [PrivKey1, PrivKey2]),
    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),
    S3 = aesc_test_utils:set_trees(Trees1, S2),

    %% Check channel created
    Trees2 = aesc_test_utils:trees(S3),
    ChannelId = aesc_channels:id(PubKey1, 1, PubKey2),
    {value, Ch} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees2)),
    PubKey1 = aesc_channels:initiator(Ch),
    PubKey2 = aesc_channels:participant(Ch),
    0       = aesc_channels:sequence_number(Ch),
    true    = aesc_channels:is_active(Ch),
    {PubKey1, PubKey2, ChannelId, S3}.

create_negative(Cfg) ->
    {PubKey1, S1} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    {PubKey2, S2} = aesc_test_utils:setup_new_account(S1),
    Trees = aesc_test_utils:trees(S2),
    Height = 1,

    %% Test bad initiator account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aesc_test_utils:create_tx_spec(BadPubKey, PubKey2, S2),
    {ok, Tx1} = aesc_create_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height),

    %% Test bad responder account key
    TxSpec2 = aesc_test_utils:create_tx_spec(PubKey1, BadPubKey, S2),
    {ok, Tx2} = aesc_create_tx:new(TxSpec2),
    {error, account_not_found} =
        aetx:check(Tx2, Trees, Height),

    %% Test insufficient initiator funds
    S3 = aesc_test_utils:set_account_balance(PubKey1, 11, S2),
    Trees3 = aesc_test_utils:trees(S3),
    TxSpec3 = aesc_test_utils:create_tx_spec(
                PubKey1, PubKey2,
                #{initiator_amount => 10,
                  fee => 2}, S3),
    {ok, Tx3} = aesc_create_tx:new(TxSpec3),
    {error, insufficient_funds} =
        aetx:check(Tx3, Trees3, Height),

    %% Test insufficient responder funds
    S4 = aesc_test_utils:set_account_balance(PubKey2, 11, S2),
    Trees4 = aesc_test_utils:trees(S4),
    TxSpec4 = aesc_test_utils:create_tx_spec(
                PubKey1, PubKey2,
                #{responder_amount => 12}, S4),
    {ok, Tx4} = aesc_create_tx:new(TxSpec4),
    {error, insufficient_funds} =
        aetx:check(Tx4, Trees4, Height),

    %% Test too high initiator nonce
    TxSpec5 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2, #{nonce => 0}, S2),
    {ok, Tx5} = aesc_create_tx:new(TxSpec5),
    {error, account_nonce_too_high} =
        aetx:check(Tx5, Trees, Height),

    %% Test initiator funds lower than channel reserve
    TxSpec6 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2,
                                             #{initiator_amount => 5,
                                               channel_reserve => 8}, S2),
    {ok, Tx6} = aesc_create_tx:new(TxSpec6),
    {error, insufficient_initiator_amount} =
        aetx:check(Tx6, Trees, Height),

    %% Test responder funds lower than channel reserve
    TxSpec7 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2,
                                             #{responder_amount => 5,
                                               channel_reserve => 8}, S2),
    {ok, Tx7} = aesc_create_tx:new(TxSpec7),
    {error, insufficient_responder_amount} =
        aetx:check(Tx7, Trees, Height),

    %% Test channel already present
    {PubKey3, PubKey4, _ChannelId, S5} = create(Cfg),
    S6 = aesc_test_utils:set_account_nonce(PubKey3, 0, S5),
    Trees5 = aens_test_utils:trees(S6),
    TxSpec8 = aesc_test_utils:create_tx_spec(PubKey3, PubKey4, S6),
    {ok, Tx8} = aesc_create_tx:new(TxSpec8),
    {error, channel_exists} = aetx:check(Tx8, Trees5, Height),
    ok.

%%%===================================================================
%%% Deposit
%%%===================================================================

deposit(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, S} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    Height = 2,

    %% Get channel and account funds
    Trees = aens_test_utils:trees(S),
    Acc1 = aesc_test_utils:get_account(PubKey1, S),
    Acc1Balance = aec_accounts:balance(Acc1),
    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    %% Create deposit tx and apply it on state trees
    TxSpec = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey1,
                                             #{amount => 13,
                                               fee    => 4}, S),
    {ok, Tx} = aesc_deposit_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, [PrivKey1]),
    {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                 [SignedTx], Trees, Height),

    %% Test channel and account funds
    {value, UpdatedCh} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees1)),
    UpdatedAmount = aesc_channels:total_amount(UpdatedCh),
    UpdatedAmount = ChannelAmount + 13,

    UpdatedAcc1 = aec_accounts_trees:get(PubKey1, aec_trees:accounts(Trees1)),
    UpdatedAcc1Balance = aec_accounts:balance(UpdatedAcc1),
    UpdatedAcc1Balance = Acc1Balance - 13 - 4,
    ok.

deposit_negative(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, S} = create(Cfg),
    Trees = aesc_test_utils:trees(S),
    Height = 2,

    %% Test bad from account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aesc_test_utils:deposit_tx_spec(ChannelId, BadPubKey, S),
    {ok, Tx1} = aesc_deposit_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height),

    %% Test insufficient from account funds
    S2 = aesc_test_utils:set_account_balance(PubKey1, 5, S),
    Trees2 = aesc_test_utils:trees(S2),
    TxSpec2 = aesc_test_utils:deposit_tx_spec(
                ChannelId, PubKey1,
                #{amount => 10,
                  fee    => 2}, S2),
    {ok, Tx2} = aesc_deposit_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey1, #{nonce => 0}, S),
    {ok, Tx3} = aesc_deposit_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:deposit_tx_spec(<<"abcdefghi">>, PubKey1, S),
    {ok, Tx4} = aesc_deposit_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height),

    %% Test channel not active
    Ch51 = aesc_test_utils:get_channel(ChannelId, S),
    Ch52 = aesc_test_utils:close_solo(Ch51),
    S5   = aesc_test_utils:set_channel(Ch52, S),
    TxSpec5 = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey1,
                                              #{amount => 13,
                                                fee    => 4}, S5),
    Trees5 = aesc_test_utils:trees(S5),
    {ok, Tx5} = aesc_deposit_tx:new(TxSpec5),
    {error, channel_not_active} =
        aetx:check(Tx5, Trees5, Height),

    %% Test from account not peer
    {PubKey3, S6} = aesc_test_utils:setup_new_account(S),
    TxSpec6 = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey3, S6),
    Trees6 = aesc_test_utils:trees(S6),
    {ok, Tx6} = aesc_deposit_tx:new(TxSpec6),
    {error, account_not_peer} =
        aetx:check(Tx6, Trees6, Height),
    ok.

%%%===================================================================
%%% Withdraw
%%%===================================================================

withdraw(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, S} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    Height = 2,

    %% Get channel and account funds
    Trees = aens_test_utils:trees(S),
    Acc1 = aec_accounts_trees:get(PubKey1, aec_trees:accounts(Trees)),
    Acc1Balance = aec_accounts:balance(Acc1),
    {value, Ch} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees)),
    ChannelAmount = aesc_channels:total_amount(Ch),

    %% Create withdraw tx and apply it on state trees
    TxSpec = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1,
                                              #{amount => 13,
                                                fee    => 4}, S),
    {ok, Tx} = aesc_withdraw_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, [PrivKey1]),
    {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                 [SignedTx], Trees, Height),

    %% Test channel and account funds
    {value, UpdatedCh} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees1)),
    UpdatedAmount = aesc_channels:total_amount(UpdatedCh),
    UpdatedAmount = ChannelAmount - 13,

    UpdatedAcc1 = aec_accounts_trees:get(PubKey1, aec_trees:accounts(Trees1)),
    UpdatedAcc1Balance = aec_accounts:balance(UpdatedAcc1),
    UpdatedAcc1Balance = Acc1Balance + 13 - 4,
    ok.

withdraw_negative(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, S} = create(Cfg),
    Trees = aesc_test_utils:trees(S),
    Height = 2,

    %% Test bad from account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aesc_test_utils:withdraw_tx_spec(ChannelId, BadPubKey, S),
    {ok, Tx1} = aesc_withdraw_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height),

    %% Test insufficient from account funds
    S2 = aesc_test_utils:set_account_balance(PubKey1, 5, S),
    Trees2 = aesc_test_utils:trees(S2),
    TxSpec2 = aesc_test_utils:withdraw_tx_spec(
                ChannelId, PubKey1,
                #{amount => 10,
                  fee    => 2}, S2),
    {ok, Tx2} = aesc_withdraw_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1, #{nonce => 0}, S),
    {ok, Tx3} = aesc_withdraw_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:withdraw_tx_spec(<<"abcdefghi">>, PubKey1, S),
    {ok, Tx4} = aesc_withdraw_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height),

    %% Test channel not active
    Ch51 = aesc_test_utils:get_channel(ChannelId, S),
    Ch52 = aesc_test_utils:close_solo(Ch51),
    S5   = aesc_test_utils:set_channel(Ch52, S),
    TxSpec5 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1,
                                               #{amount => 13,
                                                 fee    => 4}, S5),
    Trees5 = aesc_test_utils:trees(S5),
    {ok, Tx5} = aesc_withdraw_tx:new(TxSpec5),
    {error, channel_not_active} =
        aetx:check(Tx5, Trees5, Height),

    %% Test from account not peer
    {PubKey3, S6} = aesc_test_utils:setup_new_account(S),
    TxSpec6 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey3, S6),
    Trees6 = aesc_test_utils:trees(S6),
    {ok, Tx6} = aesc_withdraw_tx:new(TxSpec6),
    {error, account_not_peer} =
        aetx:check(Tx6, Trees6, Height),

    %% Test withdrawn amount exceeds channel funds
    TxSpec7 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1, S),
    {ok, Tx7} = aesc_withdraw_tx:new(TxSpec7),
    {error, not_enough_channel_funds} =
        aetx:check(Tx7, Trees, Height),
    ok.
