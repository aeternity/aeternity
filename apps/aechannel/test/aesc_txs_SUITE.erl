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
         create_negative/1]).

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
       create_negative]
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
    {PubKey1, PubKey2, S3}.

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

    %% Test bad participant account key
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

    %% Test insufficient participant funds
    S4 = aesc_test_utils:set_account_balance(PubKey2, 11, S2),
    Trees4 = aesc_test_utils:trees(S4),
    TxSpec4 = aesc_test_utils:create_tx_spec(
                PubKey1, PubKey2,
                #{participant_amount => 12}, S4),
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

    %% Test participant funds lower than channel reserve
    TxSpec7 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2,
                                             #{participant_amount => 5,
                                               channel_reserve => 8}, S2),
    {ok, Tx7} = aesc_create_tx:new(TxSpec7),
    {error, insufficient_participant_amount} =
        aetx:check(Tx7, Trees, Height),

    %% Test channel already present
    {PubKey3, PubKey4, S5} = create(Cfg),
    S6 = aesc_test_utils:set_account_nonce(PubKey3, 0, S5),
    Trees5 = aens_test_utils:trees(S6),
    TxSpec8 = aesc_test_utils:create_tx_spec(PubKey3, PubKey4, S6),
    {ok, Tx8} = aesc_create_tx:new(TxSpec8),
    {error, channel_exists} = aetx:check(Tx8, Trees5, Height),
    ok.
