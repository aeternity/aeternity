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
         close_solo/1,
         close_solo_negative/1,
         close_solo_payload_create_tx/1,
         close_solo_payload_deposit_tx/1,
         close_solo_payload_withdraw_tx/1,
         close_mutual/1,
         close_mutual_negative/1,
         slash/1,
         slash_negative/1,
         deposit/1,
         deposit_negative/1,
         withdraw/1,
         withdraw_negative/1,
         settle/1,
         settle_negative/1,
         snapshot_solo/1]).

% negative snapshot solo
-export([snapshot_closed_channel/1,
         snapshot_closing_channel/1,
         snapshot_older_state/1,
         snapshot_missing_channel/1,
         snapshot_payload_from_another_channel/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(BOGUS_CHANNEL, <<0:?MINER_PUB_BYTES/unit:8>>).
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
       close_solo,
       close_solo_negative,
       close_solo_payload_create_tx,
       close_solo_payload_deposit_tx,
       close_solo_payload_withdraw_tx,
       close_mutual,
       close_mutual_negative,
       slash,
       slash_negative,
       deposit,
       deposit_negative,
       withdraw,
       settle,
       settle_negative,
       snapshot_solo,
       {group, snapshot_solo_negative}]
     },
     {snapshot_solo_negative, [sequence],
      [snapshot_closed_channel,
       snapshot_closing_channel,
       snapshot_older_state,
       snapshot_missing_channel,
       snapshot_payload_from_another_channel
      ]}
    ].

%%%===================================================================

create(Cfg) -> create(Cfg, #{}).

create(Cfg, Spec0) ->
    create_from_state(get_state(Cfg), Spec0).

get_state(Cfg) ->
    case proplists:get_value(state, Cfg) of
        undefined -> aesc_test_utils:new_state();
        State0    -> State0
    end.

%% Returns a default spec (w/ delegates) and an updated Cfg.
new_spec_with_delegates(N, Cfg) when N > 0 ->
    {Delegates, NewS} =
        lists:mapfoldl(
          fun(_, Sx) ->
                  {PubKey, Sx1} = aesc_test_utils:setup_new_account(Sx),
                  {aec_id:create(account, PubKey), Sx1}
          end, get_state(Cfg), lists:seq(1, N)),
    {#{delegates => Delegates}, lists:keystore(state, 1, Cfg, {state, NewS})}.

create_from_state(S) ->
    create_from_state(S, #{}).

create_from_state(S, DefaultSpec) ->
    {PubKey1, S1} = aesc_test_utils:setup_new_account(S),
    {PubKey2, S2} = aesc_test_utils:setup_new_account(S1),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S2),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S2),

    %% Create Channel Create tx and apply it on trees
    Trees = aesc_test_utils:trees(S2),
    Height = 1,
    TxSpec = aesc_test_utils:create_tx_spec(PubKey1, PubKey2, DefaultSpec, S2),
    {ok, Tx} = aesc_create_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1, PrivKey2]),
    {ok, [SignedTx], Trees1} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees,
                                                  Height, ?PROTOCOL_VERSION),
    S3 = aesc_test_utils:set_trees(Trees1, S2),

    %% Check channel created
    Trees2 = aesc_test_utils:trees(S3),
    ChannelId = aesc_channels:id(PubKey1, 1, PubKey2),
    {value, Ch} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees2)),
    PubKey1 = aesc_channels:initiator(Ch),
    PubKey2 = aesc_channels:responder(Ch),
    0       = aesc_channels:round(Ch),
    true    = aesc_channels:is_active(Ch),

    %% Check that the nonce was bumped for the initiator, but not for
    %% the responder.
    ATreesBefore = aec_trees:accounts(Trees),
    {value, AccountBefore1} = aec_accounts_trees:lookup(PubKey1, ATreesBefore),
    {value, AccountBefore2} = aec_accounts_trees:lookup(PubKey2, ATreesBefore),
    ATreesAfter = aec_trees:accounts(Trees2),
    {value, AccountAfter1} = aec_accounts_trees:lookup(PubKey1, ATreesAfter),
    {value, AccountAfter2} = aec_accounts_trees:lookup(PubKey2, ATreesAfter),
    ?assertEqual(aec_accounts:nonce(AccountBefore1) + 1,
                 aec_accounts:nonce(AccountAfter1)),
    ?assertEqual(aec_accounts:nonce(AccountBefore2),
                 aec_accounts:nonce(AccountAfter2)),

    {PubKey1, PubKey2, ChannelId, SignedTx, S3}.

create_negative(Cfg) ->
    {PubKey1, S1} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    {PubKey2, S2} = aesc_test_utils:setup_new_account(S1),
    Trees = aesc_test_utils:trees(S2),
    Height = 1,

    %% Test bad initiator account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aesc_test_utils:create_tx_spec(BadPubKey, PubKey2, S2),
    {ok, Tx1} = aesc_create_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Test bad responder account key
    TxSpec2 = aesc_test_utils:create_tx_spec(PubKey1, BadPubKey, S2),
    {ok, Tx2} = aesc_create_tx:new(TxSpec2),
    {error, account_not_found} =
        aetx:check(Tx2, Trees, Height, ?PROTOCOL_VERSION),

    %% Test insufficient initiator funds
    S3 = aesc_test_utils:set_account_balance(PubKey1, 11, S2),
    Trees3 = aesc_test_utils:trees(S3),
    TxSpec3 = aesc_test_utils:create_tx_spec(
                PubKey1, PubKey2,
                #{initiator_amount => 10,
                  fee => 2}, S3),
    {ok, Tx3} = aesc_create_tx:new(TxSpec3),
    {error, insufficient_funds} =
        aetx:check(Tx3, Trees3, Height, ?PROTOCOL_VERSION),

    %% Test insufficient responder funds
    S4 = aesc_test_utils:set_account_balance(PubKey2, 11, S2),
    Trees4 = aesc_test_utils:trees(S4),
    TxSpec4 = aesc_test_utils:create_tx_spec(
                PubKey1, PubKey2,
                #{responder_amount => 12}, S4),
    {ok, Tx4} = aesc_create_tx:new(TxSpec4),
    {error, insufficient_funds} =
        aetx:check(Tx4, Trees4, Height, ?PROTOCOL_VERSION),

    %% Test too high initiator nonce
    TxSpec5 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2, #{nonce => 0}, S2),
    {ok, Tx5} = aesc_create_tx:new(TxSpec5),
    {error, account_nonce_too_high} =
        aetx:check(Tx5, Trees, Height, ?PROTOCOL_VERSION),

    %% Test initiator funds lower than channel reserve
    TxSpec6 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2,
                                             #{initiator_amount => 5,
                                               channel_reserve => 8}, S2),
    {ok, Tx6} = aesc_create_tx:new(TxSpec6),
    {error, insufficient_initiator_amount} =
        aetx:check(Tx6, Trees, Height, ?PROTOCOL_VERSION),

    %% Test responder funds lower than channel reserve
    TxSpec7 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2,
                                             #{responder_amount => 5,
                                               channel_reserve  => 8}, S2),
    {ok, Tx7} = aesc_create_tx:new(TxSpec7),
    {error, insufficient_responder_amount} =
        aetx:check(Tx7, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel already present
    {PubKey3, PubKey4, _ChannelId, _, S5} = create(Cfg),
    S6 = aesc_test_utils:set_account_nonce(PubKey3, 0, S5),
    Trees5 = aens_test_utils:trees(S6),
    TxSpec8 = aesc_test_utils:create_tx_spec(PubKey3, PubKey4, S6),
    {ok, Tx8} = aesc_create_tx:new(TxSpec8),
    {error, channel_exists} = aetx:check(Tx8, Trees5, Height, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Close solo
%%%===================================================================
close_solo(Cfg) ->
    {PubKey1, PubKey2, ChannelId, _, S} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S),
    Height = 2,
    Fee = 3,

    %% Get channel and account funds
    Trees = aens_test_utils:trees(S),

    {Acc1Balance0, Acc2Balance0} = get_balances(PubKey1, PubKey2, S),
    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    InitiatorEndBalance = rand:uniform(ChannelAmount),
    ResponderEndBalance = ChannelAmount - InitiatorEndBalance,

    PoI = aesc_test_utils:proof_of_inclusion([{PubKey1, InitiatorEndBalance},
                                              {PubKey2, ResponderEndBalance}]),
    %% Create close_solo tx and apply it on state trees
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance},
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),
    Test =
        fun(From, FromPrivKey) ->
            TxSpec = aesc_test_utils:close_solo_tx_spec(ChannelId, From, Payload,
                                                    PoI, #{fee => Fee}, S),
            {ok, Tx} = aesc_close_solo_tx:new(TxSpec),
            SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivKey]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, Height, ?PROTOCOL_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            case From =:= PubKey1 of
                true ->
                    Acc1Balance1 = Acc1Balance0 - Fee,
                    Acc2Balance1 = Acc2Balance0;
                false ->
                    Acc1Balance1 = Acc1Balance0,
                    Acc2Balance1 = Acc2Balance0 - Fee
            end,
            ClosedCh = aesc_test_utils:get_channel(ChannelId, S1),
            false = aesc_channels:is_active(ClosedCh)
        end,
    Test(PubKey1, PrivKey1),
    Test(PubKey2, PrivKey2),
    ok.

close_solo_negative(Cfg0) ->
    {Spec0, Cfg} = new_spec_with_delegates(2, Cfg0),
    {PubKey1, PubKey2, ChannelId, _, S} = create(Cfg, Spec0),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S),
    Height = 2,

    %% Get channel and account funds
    Trees = aens_test_utils:trees(S),

    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    InitiatorEndBalance = rand:uniform(ChannelAmount - 2) + 1,
    ResponderEndBalance = ChannelAmount - InitiatorEndBalance,

    PoI = aesc_test_utils:proof_of_inclusion([{PubKey1, InitiatorEndBalance},
                                              {PubKey2, ResponderEndBalance}]),
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance},
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),
    %% Test bad from account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aesc_test_utils:close_solo_tx_spec(ChannelId, BadPubKey,
                                                 PoI, Payload, S),
    {ok, Tx1} = aesc_close_solo_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Test wrong amounts (different than channel balance)
    TestWrongAmounts =
        fun(IAmt, RAmt) ->
            PayloadSpecW = #{initiator_amount => IAmt,
                            responder_amount => RAmt},
            PayloadW = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpecW),
            PoI2 = aesc_test_utils:proof_of_inclusion([{PubKey1, IAmt},
                                              {PubKey2, RAmt}]),
            TxSpecW = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                         PayloadW, PoI2, S),
            {ok, TxW} = aesc_close_solo_tx:new(TxSpecW),
            {error, poi_amounts_change_channel_funds} =
                aetx:check(TxW, Trees, Height, ?PROTOCOL_VERSION)
        end,
    TestWrongAmounts(InitiatorEndBalance -1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance +1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance - 1),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance + 1),

    %% Test from account not peer
    {PubKey3, SNotPeer} = aesc_test_utils:setup_new_account(S),
    PrivKey3 = aesc_test_utils:priv_key(PubKey3, SNotPeer),
    ok = verify_pubkey_cannot_close_solo(PubKey3, ChannelId, Payload, PoI, Height, SNotPeer),

    [ok = verify_pubkey_cannot_close_solo(D, ChannelId, Payload, PoI, Height, S)
     || D <- aesc_channels:delegates(Ch)],

    %% Test too high from account nonce
    TxSpecWrongNonce = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                          Payload, PoI, #{nonce => 0}, S),
    {ok, TxWrongNonce} = aesc_close_solo_tx:new(TxSpecWrongNonce),
    {error, account_nonce_too_high} =
        aetx:check(TxWrongNonce, Trees, Height, ?PROTOCOL_VERSION),

    %% Test payload has different channelId
    BadPayloadSpec = #{initiator_amount => InitiatorEndBalance,
                       responder_amount => ResponderEndBalance},
    BadPayload = aesc_test_utils:payload(?BOGUS_CHANNEL, PubKey1, PubKey2,
                                         [PrivKey1, PrivKey2], BadPayloadSpec),
    TxSpecDiffChanId = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                          BadPayload, PoI, S),
    {ok, TxDiffChanId} = aesc_close_solo_tx:new(TxSpecDiffChanId),
    {error, bad_state_channel_id} =
        aetx:check(TxDiffChanId, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel missing
    MissingChannelId = ?BOGUS_CHANNEL,
    PayloadMissingChanId = aesc_test_utils:payload(MissingChannelId, PubKey1, PubKey2,
                                                  [PrivKey1, PrivKey2], PayloadSpec),
    TxSpecNoChan = aesc_test_utils:close_solo_tx_spec(MissingChannelId, PubKey1,
                                               PayloadMissingChanId, PoI, S),
    {ok, TxNoChan} = aesc_close_solo_tx:new(TxSpecNoChan),
    {error, channel_does_not_exist} =
        aetx:check(TxNoChan, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel not active
    Ch1 = aesc_test_utils:get_channel(ChannelId, S),
    Ch2 = aesc_test_utils:close_solo(Ch1),
    SClosed = aesc_test_utils:set_channel(Ch2, S),
    TxSpecClosed = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                      Payload, PoI, SClosed),
    TreesClosed = aesc_test_utils:trees(SClosed),
    {ok, TxClosed} = aesc_close_solo_tx:new(TxSpecClosed),
    {error, channel_not_active} =
        aetx:check(TxClosed, TreesClosed, Height, ?PROTOCOL_VERSION),

    %% Test reject payload with missing signatures
    TestPayloadSigners =
        fun(PrivKeys) ->
            PayloadMissingS = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                                  PrivKeys, PayloadSpec),
            TxSpecMissingS = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                      PayloadMissingS, PoI, S),
            {ok, TxMissingS} = aesc_close_solo_tx:new(TxSpecMissingS),
            {error, signature_check_failed} =
                aetx:check(TxMissingS, Trees, Height, ?PROTOCOL_VERSION)
        end,
    TestPayloadSigners([]),
    TestPayloadSigners([PrivKey1]),
    TestPayloadSigners([PrivKey2]),

    %% Test reject payload with wrong signers
    TestPayloadWrongPeers =
        fun(I, P, PrivKeys) ->
            ChannelAmount = aesc_channels:total_amount(Ch),
            IAmt = rand:uniform(ChannelAmount),
            RAmt = ChannelAmount - IAmt,
            PoI3 = aesc_test_utils:proof_of_inclusion([{I, IAmt}, {P, RAmt}]),
            PayloadMissingS = aesc_test_utils:payload(ChannelId, I, P,
                                                  PrivKeys,
                                                  PayloadSpec#{initiator_amount => IAmt,
                                                               responder_amount => RAmt}),
            TxSpecMissingS = aesc_test_utils:close_solo_tx_spec(ChannelId, I,
                                                      PayloadMissingS, PoI3, S),
            {ok, TxMissingS} = aesc_close_solo_tx:new(TxSpecMissingS),
            {error, signature_check_failed} =
                aetx:check(TxMissingS, Trees, Height, ?PROTOCOL_VERSION)
        end,
    TestPayloadWrongPeers(PubKey1, PubKey3, [PrivKey1, PrivKey3]),
    TestPayloadWrongPeers(PubKey2, PubKey3, [PrivKey2, PrivKey3]),

    %% Test existing channel's payload
    {PubKey21, PubKey22, ChannelId2, _, S2} = create_from_state(S),
    Trees2 = aens_test_utils:trees(S2),
    PrivKey21 = aesc_test_utils:priv_key(PubKey21, S2),
    PrivKey22 = aesc_test_utils:priv_key(PubKey22, S2),
    Payload2 = aesc_test_utils:payload(ChannelId2, PubKey21, PubKey22,
                                      [PrivKey21, PrivKey22], PayloadSpec),
    TxPayload2Spec = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey21,
                                                      Payload2, PoI, S2),
    {ok, TxPayload2} = aesc_close_solo_tx:new(TxPayload2Spec),
    {error, bad_state_channel_id} =
                aetx:check(TxPayload2, Trees2, Height + 2,
                           ?PROTOCOL_VERSION),
    ok.

verify_pubkey_cannot_close_solo(PubKey, ChannelId, Payload, PoI, Height, S) ->
    TxSpec = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey,
                                                Payload, PoI, S),
    Trees = aesc_test_utils:trees(S),
    {ok, Tx} = aesc_close_solo_tx:new(TxSpec),
    {error, account_not_peer} =
        aetx:check(Tx, Trees, Height, ?PROTOCOL_VERSION),
    ok.

close_solo_payload_create_tx(Cfg) ->
    {PubKey1, PubKey2, ChannelId, CreateTx, S} = create(Cfg),
    {_, CrTxI} = aetx:specialize_type(aetx_sign:tx(CreateTx)),
    InitiatorEndBalance = aesc_create_tx:initiator_amount(CrTxI),
    ResponderEndBalance = aesc_create_tx:responder_amount(CrTxI),

    PoI = aesc_test_utils:proof_of_inclusion([{PubKey1, InitiatorEndBalance},
                                              {PubKey2, ResponderEndBalance}]),
    Payload = <<>>,
    close_solo_after_(PubKey1, PubKey2, ChannelId, Payload, PoI, S).

close_solo_payload_deposit_tx(Cfg) ->
    {PubKey1, PubKey2, ChannelId, CreateTx, S} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S),

    {_, CrTxI} = aetx:specialize_type(aetx_sign:tx(CreateTx)),
    InitiatorEndBalance = aesc_create_tx:initiator_amount(CrTxI),
    ResponderEndBalance = aesc_create_tx:responder_amount(CrTxI),

    Accounts = [{PubKey1, InitiatorEndBalance + 1},
                {PubKey2, ResponderEndBalance}],
    Accs = [aec_accounts:new(Pubkey, Balance) || {Pubkey, Balance} <- Accounts],

    ChannelTrees = aec_test_utils:create_state_tree_with_accounts(Accs, no_backend),
    StateHash = aec_trees:hash(ChannelTrees),
    TxSpec = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey1,
                                             #{amount => 1,
                                               fee    => 4,
                                               state_hash => StateHash}, S),
    {ok, DepositTx} = aesc_deposit_tx:new(TxSpec),
    SignedDepositTx = aec_test_utils:sign_tx(DepositTx, [PrivKey1, PrivKey2]),
    Payload = <<>>,
    Trees = aens_test_utils:trees(S),
    PoI = aesc_test_utils:proof_of_inclusion(Accounts),
    {ok, [_], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedDepositTx], Trees, 2, ?PROTOCOL_VERSION),
    S1 = aesc_test_utils:set_trees(Trees1, S),
    close_solo_after_(PubKey1, PubKey2, ChannelId, Payload, PoI, S1).

close_solo_payload_withdraw_tx(Cfg) ->
    {PubKey1, PubKey2, ChannelId, CreateTx, S} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S),

    {_, CrTxI} = aetx:specialize_type(aetx_sign:tx(CreateTx)),
    InitiatorEndBalance = aesc_create_tx:initiator_amount(CrTxI),
    ResponderEndBalance = aesc_create_tx:responder_amount(CrTxI),

    Accounts = [{PubKey1, InitiatorEndBalance - 1},
                {PubKey2, ResponderEndBalance}],
    Accs = [aec_accounts:new(Pubkey, Balance) || {Pubkey, Balance} <- Accounts],

    ChannelTrees = aec_test_utils:create_state_tree_with_accounts(Accs, no_backend),
    StateHash = aec_trees:hash(ChannelTrees),
    TxSpec = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1,
                                             #{amount => 1,
                                               fee    => 4,
                                               state_hash => StateHash}, S),
    {ok, WithdrawTx} = aesc_withdraw_tx:new(TxSpec),
    SignedWithdrawTx = aec_test_utils:sign_tx(WithdrawTx, [PrivKey1, PrivKey2]),
    Payload = <<>>,
    Trees = aens_test_utils:trees(S),
    PoI = aesc_test_utils:proof_of_inclusion(Accounts),
    {ok, [_], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedWithdrawTx], Trees, 2, ?PROTOCOL_VERSION),
    S1 = aesc_test_utils:set_trees(Trees1, S),
    close_solo_after_(PubKey1, PubKey2, ChannelId, Payload, PoI, S1).

close_solo_after_(PubKey1, PubKey2, ChannelId, Payload, PoI, S) ->
    Trees = aens_test_utils:trees(S),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    Height = 3,
    Fee = 3,

    %% Get channel and account funds
    {Acc1Balance0, Acc2Balance0} = get_balances(PubKey1, PubKey2, S),

    %% Create close_solo tx and apply it on state trees
    Test =
        fun(From, FromPrivKey) ->
            TxSpec = aesc_test_utils:close_solo_tx_spec(ChannelId, From, Payload,
                                                    PoI, #{fee => Fee}, S),
            {ok, Tx} = aesc_close_solo_tx:new(TxSpec),
            SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivKey]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, Height, ?PROTOCOL_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            case From =:= PubKey1 of
                true ->
                    Acc1Balance1 = Acc1Balance0 - Fee,
                    Acc2Balance1 = Acc2Balance0;
                false ->
                    Acc1Balance1 = Acc1Balance0,
                    Acc2Balance1 = Acc2Balance0 - Fee
            end,
            ClosedCh = aesc_test_utils:get_channel(ChannelId, S1),
            false = aesc_channels:is_active(ClosedCh)
        end,
    Test(PubKey1, PrivKey1),
    ok.

%%%===================================================================
%%% Close mutual
%%%===================================================================

close_mutual(Cfg) ->
    {PubKey1, PubKey2, ChannelId, _, S} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S),
    Height = 2,

    %% Get channel and account funds
    Trees = aens_test_utils:trees(S),

    {Acc1Balance0, Acc2Balance0} = get_balances(PubKey1, PubKey2, S),
    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    %% Create close_mutual tx and apply it on state trees
    Test =
        fun(IAmt, RAmt, Fee) ->
            TxSpec = aesc_test_utils:close_mutual_tx_spec(ChannelId,
                                                    #{initiator_amount => IAmt,
                                                      initiator_account => PubKey1,
                                                      responder_amount => RAmt,
                                                      fee    => Fee}, S),
            {ok, Tx} = aesc_close_mutual_tx:new(TxSpec),
            SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1, PrivKey2]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, Height, ?PROTOCOL_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            % ensure balances are updated
            {_, {_, _, Acc1Balance1, _}, {_, _, Acc2Balance1, _}} =
             {Fee,  {Acc1Balance0, IAmt, Acc1Balance0 + IAmt, Acc1Balance1},
                    {Acc2Balance0, RAmt, Acc2Balance0 + RAmt, Acc2Balance1}},
            none = aesc_test_utils:lookup_channel(ChannelId, S1),
            {IAmt, RAmt}
        end,
    100 = ChannelAmount, % expectation on aesc_test_utils:create_tx_spec/3

    Fee = 10,

    %% normal cases
    {45, 45} = Test(45, ChannelAmount - 45 - Fee, Fee),
    {15, 75} = Test(15, ChannelAmount - 15 - Fee, Fee),

    %% fee edge cases
    %% amount - HalfFee = 0
    {0, 90} = Test(0, ChannelAmount - Fee, Fee),
    {90, 0} = Test(ChannelAmount - Fee, 0, Fee),

    %% amount - HalfFee < 0
    {1, 89} = Test(1 , ChannelAmount - Fee - 1, Fee),
    {89, 1} = Test(ChannelAmount - Fee - 1, 1, Fee),

    ok.


close_mutual_negative(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, _, S} = create(Cfg),
    Trees = aesc_test_utils:trees(S),
    Height = 2,

    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    %% Test insufficient tokens in channel
    TxSpec2 = aesc_test_utils:close_mutual_tx_spec(
                ChannelId,
                #{initiator_amount => 1,
                  initiator_account => PubKey1,
                  responder_amount => ChannelAmount,
                  fee    => 2}, S),
    {ok, Tx2} = aesc_close_mutual_tx:new(TxSpec2),
    {error, wrong_amounts} =
        aetx:check(Tx2, Trees, Height, ?PROTOCOL_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:close_mutual_tx_spec(
                ChannelId, #{nonce => 0, initiator_account => PubKey1},
                S),
    {ok, Tx3} = aesc_close_mutual_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:close_mutual_tx_spec(
                ?BOGUS_CHANNEL,
                #{initiator_account => PubKey1},
                S),
    {ok, Tx4} = aesc_close_mutual_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel not active
    Ch51 = aesc_test_utils:get_channel(ChannelId, S),
    Ch52 = aesc_test_utils:close_solo(Ch51),
    S5   = aesc_test_utils:set_channel(Ch52, S),
    TxSpec5 = aesc_test_utils:close_mutual_tx_spec(
                ChannelId,
                #{initiator_account => PubKey1},
                S5),
    Trees5 = aesc_test_utils:trees(S5),
    {ok, Tx5} = aesc_close_mutual_tx:new(TxSpec5),
    {error, channel_not_active} =
        aetx:check(Tx5, Trees5, Height, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Deposit
%%%===================================================================

deposit(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, _, S} = create(Cfg),
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
    SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1]),
    {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                 [SignedTx], Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel and account funds
    {value, UpdatedCh} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees1)),
    UpdatedAmount = aesc_channels:total_amount(UpdatedCh),
    UpdatedAmount = ChannelAmount + 13,

    UpdatedAcc1 = aec_accounts_trees:get(PubKey1, aec_trees:accounts(Trees1)),
    UpdatedAcc1Balance = aec_accounts:balance(UpdatedAcc1),
    UpdatedAcc1Balance = Acc1Balance - 13 - 4,
    ok.

deposit_negative(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, _, S} = create(Cfg),
    Trees = aesc_test_utils:trees(S),
    Height = 2,

    %% Test bad from account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aesc_test_utils:deposit_tx_spec(ChannelId, BadPubKey, S),
    {ok, Tx1} = aesc_deposit_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Test insufficient from account funds
    S2 = aesc_test_utils:set_account_balance(PubKey1, 5, S),
    Trees2 = aesc_test_utils:trees(S2),
    TxSpec2 = aesc_test_utils:deposit_tx_spec(
                ChannelId, PubKey1,
                #{amount => 10,
                  fee    => 2}, S2),
    {ok, Tx2} = aesc_deposit_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height, ?PROTOCOL_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey1, #{nonce => 0}, S),
    {ok, Tx3} = aesc_deposit_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:deposit_tx_spec(?BOGUS_CHANNEL, PubKey1, S),
    {ok, Tx4} = aesc_deposit_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height, ?PROTOCOL_VERSION),

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
        aetx:check(Tx5, Trees5, Height, ?PROTOCOL_VERSION),

    %% Test from account not peer
    {PubKey3, S6} = aesc_test_utils:setup_new_account(S),
    TxSpec6 = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey3, S6),
    Trees6 = aesc_test_utils:trees(S6),
    {ok, Tx6} = aesc_deposit_tx:new(TxSpec6),
    {error, account_not_peer} =
        aetx:check(Tx6, Trees6, Height, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Withdraw
%%%===================================================================

withdraw(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, _, S} = create(Cfg),
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
    SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1]),
    {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                 [SignedTx], Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel and account funds
    {value, UpdatedCh} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees1)),
    UpdatedAmount = aesc_channels:total_amount(UpdatedCh),
    UpdatedAmount = ChannelAmount - 13,

    UpdatedAcc1 = aec_accounts_trees:get(PubKey1, aec_trees:accounts(Trees1)),
    UpdatedAcc1Balance = aec_accounts:balance(UpdatedAcc1),
    UpdatedAcc1Balance = Acc1Balance + 13 - 4,
    ok.

withdraw_negative(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, _, S} = create(Cfg),
    Trees = aesc_test_utils:trees(S),
    Height = 2,

    %% Test bad from account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aesc_test_utils:withdraw_tx_spec(ChannelId, BadPubKey, S),
    {ok, Tx1} = aesc_withdraw_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Test insufficient from account funds
    S2 = aesc_test_utils:set_account_balance(PubKey1, 5, S),
    Trees2 = aesc_test_utils:trees(S2),
    TxSpec2 = aesc_test_utils:withdraw_tx_spec(
                ChannelId, PubKey1,
                #{amount => 10,
                  fee    => 2}, S2),
    {ok, Tx2} = aesc_withdraw_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height, ?PROTOCOL_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1, #{nonce => 0}, S),
    {ok, Tx3} = aesc_withdraw_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:withdraw_tx_spec(?BOGUS_CHANNEL, PubKey1, S),
    {ok, Tx4} = aesc_withdraw_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height, ?PROTOCOL_VERSION),

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
        aetx:check(Tx5, Trees5, Height, ?PROTOCOL_VERSION),

    %% Test from account not peer
    {PubKey3, S6} = aesc_test_utils:setup_new_account(S),
    TxSpec6 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey3, S6),
    Trees6 = aesc_test_utils:trees(S6),
    {ok, Tx6} = aesc_withdraw_tx:new(TxSpec6),
    {error, account_not_peer} =
        aetx:check(Tx6, Trees6, Height, ?PROTOCOL_VERSION),

    %% Test withdrawn amount exceeds channel funds
    TxSpec7 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1, S),
    {ok, Tx7} = aesc_withdraw_tx:new(TxSpec7),
    {error, not_enough_channel_funds} =
        aetx:check(Tx7, Trees, Height, ?PROTOCOL_VERSION),
    ok.

get_balances(K1, K2, S) ->
    {get_balance(K1, S), get_balance(K2, S)}.

get_balance(K, S) ->
    Acc = aesc_test_utils:get_account(K, S),
    aec_accounts:balance(Acc).

%%%===================================================================
%%% Slash
%%%===================================================================
slash(Cfg0) ->
    {Spec0, Cfg} = new_spec_with_delegates(2, Cfg0),
    {PubKey1, PubKey2, ChannelId, _, S0} = create(Cfg, Spec0),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S0),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S0),
    Height = 2,
    Fee = 3,

    %% close the channel
    Ch0 = aesc_test_utils:get_channel(ChannelId, S0),
    Ch = aesc_test_utils:close_solo(Ch0),
    S   = aesc_test_utils:set_channel(Ch, S0),

    %% Get channel and account funds
    Trees = aens_test_utils:trees(S),

    {Acc1Balance0, Acc2Balance0} = get_balances(PubKey1, PubKey2, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    InitiatorEndBalance = rand:uniform(ChannelAmount),
    ResponderEndBalance = ChannelAmount - InitiatorEndBalance,

    PoI = aesc_test_utils:proof_of_inclusion([{PubKey1, InitiatorEndBalance},
                                              {PubKey2, ResponderEndBalance}]),

    %% Create close_solo tx and apply it on state trees
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance,
                    round => 12}, % greater than default of 11
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),
    Test =
        fun(From, FromPrivKey) ->
            TxSpec = aesc_test_utils:slash_tx_spec(ChannelId, From, Payload,
                                                    PoI, #{fee    => Fee}, S),
            {ok, Tx} = aesc_slash_tx:new(TxSpec),
            SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivKey]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, Height, ?PROTOCOL_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            case From of
                PubKey1 ->
                    Acc1Balance1 = Acc1Balance0 - Fee,
                    Acc2Balance1 = Acc2Balance0;
                PubKey2 ->
                    Acc1Balance1 = Acc1Balance0,
                    Acc2Balance1 = Acc2Balance0 - Fee;
                Delegate ->
                    Bal0 = get_balance(Delegate, S),
                    Bal1 = get_balance(Delegate, S1),
                    Bal1 = Bal0 - Fee
            end
        end,
    Test(PubKey1, PrivKey1),
    Test(PubKey2, PrivKey2),
    [Test(D, PrivKeyD) || D <- aesc_channels:delegates(Ch),
                          PrivKeyD <- [aesc_test_utils:priv_key(D, S)]],
    ok.

slash_negative(Cfg0) ->
    {Spec0, Cfg} = new_spec_with_delegates(2, Cfg0),
    {PubKey1, PubKey2, ChannelId, _, S0} = create(Cfg, Spec0),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S0),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S0),

    %% close the channel
    Ch0 = aesc_test_utils:get_channel(ChannelId, S0),
    Ch = aesc_test_utils:close_solo(Ch0),
    S   = aesc_test_utils:set_channel(Ch, S0),
    Height = 2,

    %% Get channel and account funds
    Trees0 = aens_test_utils:trees(S0),
    Trees = aens_test_utils:trees(S),

    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),
    ChannelRound = aesc_channels:round(Ch),

    InitiatorEndBalance = rand:uniform(ChannelAmount - 2) + 1,
    ResponderEndBalance = ChannelAmount - InitiatorEndBalance,
    PoI = aesc_test_utils:proof_of_inclusion([{PubKey1, InitiatorEndBalance},
                                              {PubKey2, ResponderEndBalance}]),
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance,
                    round            => ChannelRound + 1
                   },
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),

    %% Test not closed channel
    TxSpec0 = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                 Payload, PoI, S0),
    {ok, Tx0} = aesc_slash_tx:new(TxSpec0),
    {error, channel_not_closing} =
        aetx:check(Tx0, Trees0, Height, ?PROTOCOL_VERSION),

    %% Test bad from account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aesc_test_utils:slash_tx_spec(ChannelId, BadPubKey,
                                                 Payload, PoI, S),
    {ok, Tx1} = aesc_slash_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Test wrong amounts (different than channel balance)
    TestWrongAmounts =
        fun(IAmt, RAmt) ->
            PayloadSpecW = #{initiator_amount => IAmt,
                             responder_amount => RAmt,
                             round            => ChannelRound + 2
                            },
            PayloadW = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpecW),
            PoI2 = aesc_test_utils:proof_of_inclusion([{PubKey1, IAmt},
                                              {PubKey2, RAmt}]),
            TxSpecW = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                         PayloadW, PoI2, S),
            {ok, TxW} = aesc_slash_tx:new(TxSpecW),
            {error, poi_amounts_change_channel_funds} =
                aetx:check(TxW, Trees, Height, ?PROTOCOL_VERSION)
        end,
    TestWrongAmounts(InitiatorEndBalance -1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance +1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance - 1),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance + 1),

    %% Test from account not peer
    {PubKey3, SNotPeer} = aesc_test_utils:setup_new_account(S),
    PrivKey3 = aesc_test_utils:priv_key(PubKey3, SNotPeer),

    TxSpecNotPeer = aesc_test_utils:slash_tx_spec(ChannelId, PubKey3, Payload,
                                                  PoI, SNotPeer),
    TreesNotPeer = aesc_test_utils:trees(SNotPeer),
    {ok, TxNotPeer} = aesc_slash_tx:new(TxSpecNotPeer),
    {error, account_not_peer_or_delegate} =
        aetx:check(TxNotPeer, TreesNotPeer, Height, ?PROTOCOL_VERSION),

    %% Test too high from account nonce
    TxSpecWrongNonce = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                          Payload, PoI, #{nonce => 0}, S),
    {ok, TxWrongNonce} = aesc_slash_tx:new(TxSpecWrongNonce),
    {error, account_nonce_too_high} =
        aetx:check(TxWrongNonce, Trees, Height, ?PROTOCOL_VERSION),

    %% Test payload has different channelId
    TxSpecDiffChanId = aesc_test_utils:slash_tx_spec(?BOGUS_CHANNEL, PubKey1,
                                                          Payload, PoI, S),
    {ok, TxDiffChanId} = aesc_slash_tx:new(TxSpecDiffChanId),
    {error, channel_does_not_exist} =
        aetx:check(TxDiffChanId, Trees, Height, ?PROTOCOL_VERSION),

    %% Test channel missing
    MissingChannelId = ?BOGUS_CHANNEL,
    PayloadMissingChanId = aesc_test_utils:payload(MissingChannelId, PubKey1, PubKey2,
                                                  [PrivKey1, PrivKey2], PayloadSpec),
    TxSpecNoChan = aesc_test_utils:slash_tx_spec(MissingChannelId, PubKey1,
                                               PayloadMissingChanId, PoI, S),
    {ok, TxNoChan} = aesc_slash_tx:new(TxSpecNoChan),
    {error, channel_does_not_exist} =
        aetx:check(TxNoChan, Trees, Height, ?PROTOCOL_VERSION),

    %% Test reject payload with missing signatures
    TestPayloadSigners =
        fun(PrivKeys) ->
            PayloadSpec1 = PayloadSpec#{round => ChannelRound + 2},
            PayloadMissingS = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                                  PrivKeys, PayloadSpec1),
            TxSpecMissingS = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                      PayloadMissingS, PoI, S),
            {ok, TxMissingS} = aesc_slash_tx:new(TxSpecMissingS),
            {error, signature_check_failed} =
                aetx:check(TxMissingS, Trees, Height, ?PROTOCOL_VERSION)
        end,
    TestPayloadSigners([]),
    TestPayloadSigners([PrivKey1]),
    TestPayloadSigners([PrivKey2]),

    %% Test reject payload with wrong signers
    TestPayloadWrongPeers =
        fun(I, P, PrivKeys) ->
            ChannelAmount = aesc_channels:total_amount(Ch),
            IAmt = rand:uniform(ChannelAmount),
            RAmt = ChannelAmount - IAmt,
            PoI3 = aesc_test_utils:proof_of_inclusion([{I, IAmt}, {P, RAmt}]),
            PayloadMissingS = aesc_test_utils:payload(ChannelId, I, P,
                                                  PrivKeys,
                                                  PayloadSpec#{initiator_amount => IAmt,
                                                               responder_amount => RAmt,
                                                               round => ChannelRound + 2
                                                              }),
            TxSpecMissingS = aesc_test_utils:slash_tx_spec(ChannelId, I,
                                                      PayloadMissingS, PoI3, S),
            {ok, TxMissingS} = aesc_slash_tx:new(TxSpecMissingS),
            {error, signature_check_failed} =
                aetx:check(TxMissingS, Trees, Height, ?PROTOCOL_VERSION)
        end,
    TestPayloadWrongPeers(PubKey1, PubKey3, [PrivKey1, PrivKey3]),
    TestPayloadWrongPeers(PubKey2, PubKey3, [PrivKey2, PrivKey3]),

    %% Test existing channel's payload
    {PubKey21, PubKey22, ChannelId2, _, S2} = create(Cfg0),  % no pre-stuffed delegates!
    Trees2 = aens_test_utils:trees(S2),
    PrivKey21 = aesc_test_utils:priv_key(PubKey21, S2),
    PrivKey22 = aesc_test_utils:priv_key(PubKey22, S2),
    Payload2 = aesc_test_utils:payload(ChannelId2, PubKey21, PubKey22,
                                      [PrivKey21, PrivKey22], PayloadSpec),
    TxPayload2Spec = aesc_test_utils:slash_tx_spec(ChannelId, PubKey21,
                                                      Payload2, PoI, S2),
    {ok, TxPayload2} = aesc_slash_tx:new(TxPayload2Spec),
    {error, channel_does_not_exist} =
                aetx:check(TxPayload2, Trees2, Height + 2, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Settle
%%%===================================================================

settle(Cfg) ->
    {PubKey1, PubKey2, ChannelId, _, S0} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S0),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S0),

    %% Get channel and account funds

    {Acc1Balance0, Acc2Balance0} = get_balances(PubKey1, PubKey2, S0),
    Ch0 = aesc_test_utils:get_channel(ChannelId, S0),


    100 = ChannelAmount = aesc_channels:total_amount(Ch0),

    %% Create close_mutual tx and apply it on state trees
    Test =
        fun(From, IAmt, RAmt, Fee) ->
            Ch = aesc_test_utils:close_solo(Ch0, #{initiator_amount => IAmt,
                                                   responder_amount => RAmt}),
            ClosesAt = aesc_channels:closes_at(Ch),
            ChannelAmount = IAmt + RAmt, %% assert

            S = aesc_test_utils:set_channel(Ch, S0),
            Trees = aens_test_utils:trees(S),

            TxSpec = aesc_test_utils:settle_tx_spec(ChannelId, From,
                                                    #{initiator_amount => IAmt,
                                                      responder_amount => RAmt,
                                                      ttl => 1001,
                                                      fee    => Fee}, S),
            {ok, Tx} = aesc_settle_tx:new(TxSpec),
            SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1, PrivKey2]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, ClosesAt, ?PROTOCOL_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            {IFee, RFee} =
                case From of
                    PubKey1 -> {Fee, 0};
                    PubKey2 -> {0, Fee}
                end,
            % ensure balances are updated
            {_, {_, _, _, Acc1Balance1, _}, {_, _, _, Acc2Balance1, _}} =
             {Fee,  {Acc1Balance0, IFee, IAmt, Acc1Balance0 + IAmt - IFee, Acc1Balance1},
                    {Acc2Balance0, RFee, RAmt, Acc2Balance0 + RAmt - RFee, Acc2Balance1}},
            none = aesc_test_utils:lookup_channel(ChannelId, S1),
            {IAmt, RAmt}
        end,
    100 = ChannelAmount, % expectation on aesc_test_utils:create_tx_spec/3
    lists:foreach(
        fun(From) ->
            Fee = 10,
            % normal cases
            {50, 50} = Test(From, 50, ChannelAmount - 50, Fee),
            {20, 80} = Test(From, 20, ChannelAmount - 20, Fee)
        end,
        [PubKey1, PubKey2]),
    ok.

settle_negative(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, _, S0} = create(Cfg),
    Trees0 = aesc_test_utils:trees(S0),
    Height = 2,

    Ch0 = aesc_test_utils:get_channel(ChannelId, S0),
    100 = ChannelAmount = aesc_channels:total_amount(Ch0),

    %% Test not closed at all
    TxSpec0 = aesc_test_utils:settle_tx_spec(ChannelId, PubKey1,
                                        #{initiator_amount => ChannelAmount,
                                          responder_amount => 0}, S0),
    {ok, Tx0} = aesc_settle_tx:new(TxSpec0),
    {error, channel_not_closed} =
        aetx:check(Tx0, Trees0, Height, ?PROTOCOL_VERSION),

    %% Test not closed yet
    Ch = aesc_test_utils:close_solo(Ch0, #{initiator_amount => ChannelAmount,
                                           responder_amount => 0}),
    ClosesAt = aesc_channels:closes_at(Ch),
    S   = aesc_test_utils:set_channel(Ch, S0),
    ChannelAmount = aesc_channels:total_amount(Ch),
    TxSpec = aesc_test_utils:settle_tx_spec(ChannelId, PubKey1,
                                            #{initiator_amount => ChannelAmount,
                                              responder_amount => 0,
                                              ttl => ClosesAt + 1}, S),
    Trees = aesc_test_utils:trees(S),
    {ok, Tx} = aesc_settle_tx:new(TxSpec),
    {error, channel_not_closed} =
        aetx:check(Tx, Trees, ClosesAt - 1, ?PROTOCOL_VERSION),

    %% Test bad from account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aesc_test_utils:settle_tx_spec(ChannelId, BadPubKey,
                                                   #{nonce => 2}, S),
    {ok, Tx1} = aesc_settle_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, ClosesAt, ?PROTOCOL_VERSION),

    %% Test insufficient different tokens distribution than in channel
    TxSpec2 = aesc_test_utils:settle_tx_spec(
                ChannelId, PubKey1,
                #{initiator_amount => 1,
                  responder_amount => ChannelAmount - 1,
                  fee    => 2}, S),
    {ok, Tx2} = aesc_settle_tx:new(TxSpec2),
    {error, wrong_amt} =
        aetx:check(Tx2, Trees, ClosesAt, ?PROTOCOL_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:settle_tx_spec(ChannelId, PubKey1,
                                                   #{nonce => 0}, S),
    {ok, Tx3} = aesc_settle_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, ClosesAt, ?PROTOCOL_VERSION),

    %% Test too low TTL
    TxSpec4 = aesc_test_utils:settle_tx_spec(ChannelId, PubKey1, #{ttl => ClosesAt - 1}, S),
    {ok, Tx4} = aesc_settle_tx:new(TxSpec4),
    {error, ttl_expired} =
        aetx:check(Tx4, Trees, ClosesAt, ?PROTOCOL_VERSION),

    %% Test channel does not exist
    TxSpec5 = aesc_test_utils:settle_tx_spec(?BOGUS_CHANNEL, PubKey1,
                                             #{ttl => ClosesAt}, S),
    {ok, Tx5} = aesc_settle_tx:new(TxSpec5),
    {error, channel_does_not_exist} =
        aetx:check(Tx5, Trees, ClosesAt, ?PROTOCOL_VERSION),

    %% Test only one settle
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1]),
    {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                [SignedTx], Trees, ClosesAt, ?PROTOCOL_VERSION),
    S5 = aesc_test_utils:set_trees(Trees1, S),

    TxSpec6 = aesc_test_utils:settle_tx_spec(ChannelId, PubKey1,
                                      #{initiator_amount => ChannelAmount,
                                        responder_amount => 0}, S5),
    {ok, Tx6} = aesc_settle_tx:new(TxSpec6),
    {error, channel_does_not_exist} =
        aetx:check(Tx6, Trees1, ClosesAt + 2, ?PROTOCOL_VERSION),
  ok.

%%%===================================================================
%%% Snapshot solo
%%%===================================================================

snapshot_solo(Cfg) ->
    Round = 43,
    StateHashSize = aec_base58c:byte_size_for_type(state),
    StateHash = <<43:StateHashSize/unit:8>>,
    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                set_from(Snapshoter),
                set_prop(round, Round),
                set_prop(state_hash, StateHash),
                positive(fun snapshot_solo_/2),
                fun(#{channel_id := ChannelId, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelId, S),
                    Round = aesc_channels:round(Channel),
                    StateHash = aesc_channels:state_hash(Channel),
                    Props
                end
                ])
        end,
    Test(initiator),
    Test(responder).

% no one can post a snapshot_tx to a closed channel
snapshot_closed_channel(Cfg) ->
    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                set_from(initiator),
                positive(fun close_mutual_/2),
                fun(#{channel_id := ChannelId, state := S} = Props) ->
                    % ensure channel is closed
                    none = aesc_test_utils:lookup_channel(ChannelId, S),
                    Props
                end,
                set_from(Snapshoter),
                negative(fun snapshot_solo_/2, {error, channel_does_not_exist})])
        end,
    Test(initiator),
    Test(responder).

% no one can post a snapshot_tx to a closing channel, not even the one that
% initiated the close
snapshot_closing_channel(Cfg) ->
    Test =
        fun(Closer, Snapshoter) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                set_from(Closer),
                positive(fun close_solo_/2),
                fun(#{channel_id := ChannelId, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelId, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_from(Snapshoter),
                negative(fun snapshot_solo_/2, {error, channel_not_active})])
        end,
    Test(initiator, initiator),
    Test(initiator, responder),
    Test(responder, initiator),
    Test(responder, responder),
    ok.

% no one can overwrite a state, not even the one that posted it
snapshot_older_state(Cfg) ->
    Test =
        fun(First, Second) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                set_prop(round, 42),
                set_from(First),
                positive(fun snapshot_solo_/2),
                set_from(Second),
                set_prop(round, 41),
                negative(fun snapshot_solo_/2, {error, old_round})])
        end,
    Test(initiator, initiator),
    Test(initiator, responder),
    Test(responder, initiator),
    Test(responder, responder),
    ok.

% snapshot_tx calls to a missing channel are rejected
snapshot_missing_channel(Cfg) ->
    ChannelHashSize = aec_base58c:byte_size_for_type(channel),
    FakeChannelId = <<42:ChannelHashSize/unit:8>>,
    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                set_prop(channel_id, FakeChannelId),
                set_from(Snapshoter),
                negative(fun snapshot_solo_/2, {error, channel_does_not_exist})])
        end,
    Test(initiator),
    Test(responder).

% snapshot_tx calls from another channel are rejected
snapshot_payload_from_another_channel(Cfg) ->
    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2), % create a channelA
                 create_payload(), % produce a payload for channelA
                 % create another channelB and replace the old one with the
                 % participansts as well
                 positive(fun create_channel_/2),
                 set_from(Snapshoter),
                 % use the payload of channelA in a snapshot_tx for channelB
                 negative(fun snapshot_solo_/2, {error, bad_state_channel_id})])
        end,
    Test(initiator),
    Test(responder).


close_solo_(#{channel_id        := ChannelId,
              from              := From,
              from_privkey      := FromPrivkey,
              initiator_amount  := IAmt,
              responder_amount  := RAmt,
              initiator_pubkey  := IPubkey,
              responder_pubkey  := RPubkey,
              fee               := Fee,
              state             := S,
              initiator_privkey := IPrivkey,
              responder_privkey := RPrivkey} = Props, Expected) ->

    %% Create close_solo tx and apply it on state trees
    PayloadSpec = #{initiator_amount => IAmt,
                    responder_amount => RAmt},
    Payload = aesc_test_utils:payload(ChannelId, IPubkey, RPubkey,
                                    [IPrivkey, RPrivkey], PayloadSpec),
    PoI = aesc_test_utils:proof_of_inclusion([{IPubkey, IAmt}, {RPubkey, RAmt}]),
    TxSpec = aesc_test_utils:close_solo_tx_spec(ChannelId, From, Payload,
                                            PoI, #{fee => Fee}, S),
    {ok, Tx} = aesc_close_solo_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

create_payload() ->
    fun(#{channel_id        := ChannelId,
          initiator_amount  := IAmt,
          responder_amount  := RAmt,
          initiator_pubkey  := IPubkey,
          responder_pubkey  := RPubkey,
          initiator_privkey := IPrivkey,
          responder_privkey := RPrivkey} = Props) ->
        PayloadSpec = #{initiator_amount => IAmt,
                        responder_amount => RAmt},
        Payload = aesc_test_utils:payload(ChannelId, IPubkey, RPubkey,
                                        [IPrivkey, RPrivkey], PayloadSpec),
        Props#{payload => Payload}
    end.

close_mutual_(#{channel_id      := ChannelId,
                initiator_amount  := IAmt,
                responder_amount  := RAmt,
                initiator_pubkey  := IPubkey,
                fee               := Fee,
                state             := S,
                initiator_privkey := PrivKey1,
                responder_privkey := PrivKey2} = Props, Expected) ->
        TxSpec = aesc_test_utils:close_mutual_tx_spec(ChannelId,
                                                #{initiator_amount => IAmt - Fee,
                                                  initiator_account => IPubkey,
                                                  responder_amount => RAmt,
                                                  fee    => Fee}, S),
        {ok, Tx} = aesc_close_mutual_tx:new(TxSpec),
        SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1, PrivKey2]),
        apply_on_trees_(Props, SignedTx, S, Expected).


create_channel_(#{cfg := Cfg} = Props, _) ->
    CreateOpts =
        case maps:get(state, Props, undefined) of
            undefined -> #{};
            State -> #{state => State}
        end,
    {PubKey1, PubKey2, ChannelId, _, S0} = create(Cfg, CreateOpts),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S0),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S0),

    Height = 2,

    %% Get channel and account funds

    IAmt = 30,
    RAmt = 70,
    Fee = 2,
    %% expected amounts
    100 = IAmt + RAmt,
    Props#{ channel_id        => ChannelId,
            initiator_amount  => IAmt,
            responder_amount  => RAmt,
            initiator_pubkey  => PubKey1,
            responder_pubkey  => PubKey2,
            fee               => Fee,
            height            => Height,
            state             => S0,
            initiator_privkey => PrivKey1,
            responder_privkey => PrivKey2}.

snapshot_solo_(#{ channel_id        := ChannelId,
                  from              := From,
                  from_privkey      := FromPrivkey,
                  initiator_amount  := IAmt,
                  responder_amount  := RAmt,
                  initiator_pubkey  := IPubkey,
                  responder_pubkey  := RPubkey,
                  fee               := Fee,
                  state             := S,
                  initiator_privkey := IPrivkey,
                  responder_privkey := RPrivkey} = Props, Expected) ->

    Round = maps:get(round, Props, 42),
    StateHashSize = aec_base58c:byte_size_for_type(state),
    StateHash = maps:get(state_hash, Props, <<42:StateHashSize/unit:8>>),
    PayloadSpec = #{initiator_amount => IAmt,
                    responder_amount => RAmt,
                    state_hash       => StateHash,
                    round            => Round},
    Payload = maps:get(payload, Props,
                       aesc_test_utils:payload(ChannelId, IPubkey, RPubkey,
                                      [IPrivkey, RPrivkey], PayloadSpec)),


    SnapshotTxSpec = aesc_test_utils:snapshot_solo_tx_spec(ChannelId, From,
                           Payload, #{fee => Fee},S),
    {ok, SnapshotTx} = aesc_snapshot_solo_tx:new(SnapshotTxSpec),

    SignedTx = aec_test_utils:sign_tx(SnapshotTx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).


positive(Fun) ->
    fun(Props) -> Fun(Props, positive) end.

negative(Fun, ErrMsg) ->
    fun(Props) -> Fun(Props, {negative, ErrMsg}) end.

set_from(Role) ->
    fun(Props) ->
        {KeyPub, KeyPriv} =
            case Role of
                initiator -> {initiator_pubkey, initiator_privkey};
                responder -> {responder_pubkey, responder_privkey}
            end,
        PubKey = maps:get(KeyPub, Props),
        PrivKey = maps:get(KeyPriv, Props),
        Props#{from => PubKey, from_privkey => PrivKey}
    end.

set_prop(Key, Value) ->
    fun(Props) ->
        maps:put(Key, Value, Props)
    end.

run(Cfg, Funs) ->
    lists:foldl(
        fun(Fun, Props) -> Fun(Props) end,
        Cfg,
        Funs).

apply_on_trees_(#{height := Height} = Props, SignedTx, S, positive) ->
    Trees = aens_test_utils:trees(S),
    {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                [SignedTx], Trees, Height, ?PROTOCOL_VERSION),
    S1 = aesc_test_utils:set_trees(Trees1, S),
    Props#{state => S1};
apply_on_trees_(#{height := Height} = Props, SignedTx, S, {negative, ExpectedError}) ->
    Trees = aens_test_utils:trees(S),
    Tx = aetx_sign:tx(SignedTx),
    ExpectedError =  aetx:check(Tx, Trees, Height, ?PROTOCOL_VERSION),
    Props.
