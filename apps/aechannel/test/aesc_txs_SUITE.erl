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
         close_mutual/1,
         close_mutual_negative/1,
         slash/1,
         slash_negative/1,
         deposit/1,
         deposit_negative/1,
         withdraw/1,
         withdraw_negative/1,
         settle/1,
         settle_negative/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/blocks.hrl").

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
       close_mutual,
       close_mutual_negative,
       slash,
       slash_negative,
       deposit,
       deposit_negative,
       withdraw,
       settle,
       settle_negative]
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
    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees,
                                                          Height,
                                                          ?CONSENSUS_V_0_11_0_VERSION),
    S3 = aesc_test_utils:set_trees(Trees1, S2),

    %% Check channel created
    Trees2 = aesc_test_utils:trees(S3),
    ChannelId = aesc_channels:id(PubKey1, 1, PubKey2),
    {value, Ch} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees2)),
    PubKey1 = aesc_channels:initiator(Ch),
    PubKey2 = aesc_channels:responder(Ch),
    0       = aesc_channels:round(Ch),
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
        aetx:check(Tx1, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test bad responder account key
    TxSpec2 = aesc_test_utils:create_tx_spec(PubKey1, BadPubKey, S2),
    {ok, Tx2} = aesc_create_tx:new(TxSpec2),
    {error, account_not_found} =
        aetx:check(Tx2, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test insufficient initiator funds
    S3 = aesc_test_utils:set_account_balance(PubKey1, 11, S2),
    Trees3 = aesc_test_utils:trees(S3),
    TxSpec3 = aesc_test_utils:create_tx_spec(
                PubKey1, PubKey2,
                #{initiator_amount => 10,
                  fee => 2}, S3),
    {ok, Tx3} = aesc_create_tx:new(TxSpec3),
    {error, insufficient_funds} =
        aetx:check(Tx3, Trees3, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test insufficient responder funds
    S4 = aesc_test_utils:set_account_balance(PubKey2, 11, S2),
    Trees4 = aesc_test_utils:trees(S4),
    TxSpec4 = aesc_test_utils:create_tx_spec(
                PubKey1, PubKey2,
                #{responder_amount => 12}, S4),
    {ok, Tx4} = aesc_create_tx:new(TxSpec4),
    {error, insufficient_funds} =
        aetx:check(Tx4, Trees4, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test too high initiator nonce
    TxSpec5 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2, #{nonce => 0}, S2),
    {ok, Tx5} = aesc_create_tx:new(TxSpec5),
    {error, account_nonce_too_high} =
        aetx:check(Tx5, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test initiator funds lower than channel reserve
    TxSpec6 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2,
                                             #{initiator_amount => 5,
                                               channel_reserve => 8}, S2),
    {ok, Tx6} = aesc_create_tx:new(TxSpec6),
    {error, insufficient_initiator_amount} =
        aetx:check(Tx6, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test responder funds lower than channel reserve
    TxSpec7 = aesc_test_utils:create_tx_spec(PubKey1, PubKey2,
                                             #{responder_amount => 5,
                                               channel_reserve  => 8}, S2),
    {ok, Tx7} = aesc_create_tx:new(TxSpec7),
    {error, insufficient_responder_amount} =
        aetx:check(Tx7, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel already present
    {PubKey3, PubKey4, _ChannelId, S5} = create(Cfg),
    S6 = aesc_test_utils:set_account_nonce(PubKey3, 0, S5),
    Trees5 = aens_test_utils:trees(S6),
    TxSpec8 = aesc_test_utils:create_tx_spec(PubKey3, PubKey4, S6),
    {ok, Tx8} = aesc_create_tx:new(TxSpec8),
    {error, channel_exists} = aetx:check(Tx8, Trees5, Height, ?CONSENSUS_V_0_11_0_VERSION),
    ok.

%%%===================================================================
%%% Close solo
%%%===================================================================
close_solo(Cfg) ->
    {PubKey1, PubKey2, ChannelId, S} = create(Cfg),
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
    %% Create close_solo tx and apply it on state trees
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance},
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),
    Test =
        fun(From, FromPrivKey) ->
            TxSpec = aesc_test_utils:close_solo_tx_spec(ChannelId, From, Payload,
                                                    #{fee => Fee}, S),
            {ok, Tx} = aesc_close_solo_tx:new(TxSpec),
            SignedTx = aetx_sign:sign(Tx, [FromPrivKey]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),
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

close_solo_negative(Cfg) ->
    {PubKey1, PubKey2, ChannelId, S} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S),
    Height = 2,

    %% Get channel and account funds
    Trees = aens_test_utils:trees(S),

    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    InitiatorEndBalance = rand:uniform(ChannelAmount - 2) + 1,
    ResponderEndBalance = ChannelAmount - InitiatorEndBalance,
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance},
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),
    %% Test bad from account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aesc_test_utils:close_solo_tx_spec(ChannelId, BadPubKey,
                                                 Payload, S),
    {ok, Tx1} = aesc_close_solo_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test wrong amounts (different than channel balance)
    TestWrongAmounts =
        fun(IAmt, PAmt) ->
            PayloadSpecW = #{initiator_amount => IAmt,
                            responder_amount => PAmt},
            PayloadW = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpecW),
            TxSpecW = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                         PayloadW, S),
            {ok, TxW} = aesc_close_solo_tx:new(TxSpecW),
            {error, payload_amounts_change_channel_funds} =
                aetx:check(TxW, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION)
        end,
    TestWrongAmounts(InitiatorEndBalance -1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance +1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance - 1),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance + 1),

    %% Test from account not peer
    {PubKey3, SNotPeer} = aesc_test_utils:setup_new_account(S),
    PrivKey3 = aesc_test_utils:priv_key(PubKey3, SNotPeer),

    TxSpecNotPeer = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey3, Payload, SNotPeer),
    TreesNotPeer = aesc_test_utils:trees(SNotPeer),
    {ok, TxNotPeer} = aesc_close_solo_tx:new(TxSpecNotPeer),
    {error, account_not_peer} =
        aetx:check(TxNotPeer, TreesNotPeer, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test too high from account nonce
    TxSpecWrongNonce = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                          Payload, #{nonce => 0}, S),
    {ok, TxWrongNonce} = aesc_close_solo_tx:new(TxSpecWrongNonce),
    {error, account_nonce_too_high} =
        aetx:check(TxWrongNonce, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test payload has different channelId
    TxSpecDiffChanId = aesc_test_utils:close_solo_tx_spec(<<"abcdefghi">>, PubKey1,
                                                          Payload, S),
    {ok, TxDiffChanId} = aesc_close_solo_tx:new(TxSpecDiffChanId),
    {error, bad_state_channel_id} =
        aetx:check(TxDiffChanId, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel missing
    MissingChannelId = <<"abcdefghi">>,
    PayloadMissingChanId = aesc_test_utils:payload(MissingChannelId, PubKey1, PubKey2,
                                                  [PrivKey1, PrivKey2], PayloadSpec),
    TxSpecNoChan = aesc_test_utils:close_solo_tx_spec(MissingChannelId, PubKey1,
                                               PayloadMissingChanId, S),
    {ok, TxNoChan} = aesc_close_solo_tx:new(TxSpecNoChan),
    {error, channel_does_not_exist} =
        aetx:check(TxNoChan, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel not active
    Ch1 = aesc_test_utils:get_channel(ChannelId, S),
    Ch2 = aesc_test_utils:close_solo(Ch1),
    SClosed = aesc_test_utils:set_channel(Ch2, S),
    TxSpecClosed = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                      Payload, SClosed),
    TreesClosed = aesc_test_utils:trees(SClosed),
    {ok, TxClosed} = aesc_close_solo_tx:new(TxSpecClosed),
    {error, channel_not_active} =
        aetx:check(TxClosed, TreesClosed, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test reject payload with missing signatures
    TestPayloadSigners =
        fun(PrivKeys) ->
            PayloadMissingS = aesc_test_utils:payload(MissingChannelId, PubKey1, PubKey2,
                                                  PrivKeys, PayloadSpec),
            TxSpecMissingS = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey1,
                                                      PayloadMissingS, S),
            {ok, TxMissingS} = aesc_close_solo_tx:new(TxSpecMissingS),
            {error, signature_check_failed} =
                aetx:check(TxMissingS, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION)
        end,
    TestPayloadSigners([]),
    TestPayloadSigners([PrivKey1]),
    TestPayloadSigners([PrivKey2]),

    %% Test reject payload with wrong signers
    TestPayloadWrongPeers =
        fun(I, P, PrivKeys) ->
            PayloadMissingS = aesc_test_utils:payload(ChannelId, I, P,
                                                  PrivKeys, PayloadSpec),
            TxSpecMissingS = aesc_test_utils:close_solo_tx_spec(ChannelId, I,
                                                      PayloadMissingS, S),
            {ok, TxMissingS} = aesc_close_solo_tx:new(TxSpecMissingS),
            {error, wrong_channel_peers} =
                aetx:check(TxMissingS, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION)
        end,
    TestPayloadWrongPeers(PubKey1, PubKey3, [PrivKey1, PrivKey3]),
    TestPayloadWrongPeers(PubKey2, PubKey3, [PrivKey2, PrivKey3]),

    %% Test existing channel's payload
    Cfg2 = lists:keyreplace(state, 1, Cfg, {state, S}),
    {PubKey21, PubKey22, ChannelId2, S2} = create(Cfg2),
    Trees2 = aens_test_utils:trees(S2),
    PrivKey21 = aesc_test_utils:priv_key(PubKey21, S2),
    PrivKey22 = aesc_test_utils:priv_key(PubKey22, S2),
    Payload2 = aesc_test_utils:payload(ChannelId2, PubKey21, PubKey22,
                                      [PrivKey21, PrivKey22], PayloadSpec),
    TxPayload2Spec = aesc_test_utils:close_solo_tx_spec(ChannelId, PubKey21,
                                                      Payload2, S2),
    {ok, TxPayload2} = aesc_close_solo_tx:new(TxPayload2Spec),
    {error, bad_state_channel_id} =
                aetx:check(TxPayload2, Trees2, Height + 2,
                           ?CONSENSUS_V_0_11_0_VERSION),
    ok.

%%%===================================================================
%%% Close mutual
%%%===================================================================

close_mutual(Cfg) ->
    {PubKey1, PubKey2, ChannelId, S} = create(Cfg),
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
        fun(From, IAmt0, PAmt0, Fee) ->
            TxSpec = aesc_test_utils:close_mutual_tx_spec(ChannelId, From,
                                                    #{initiator_amount => IAmt0,
                                                      responder_amount => PAmt0,
                                                      fee    => Fee}, S),
            {ok, Tx} = aesc_close_mutual_tx:new(TxSpec),
            SignedTx = aetx_sign:sign(Tx, [PrivKey1, PrivKey2]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {IAmt, PAmt} = calc_mutual_amounts(IAmt0, PAmt0, Fee),
            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            % ensure balances are updated
            {_, {_, _, _, Acc1Balance1, _}, {_, _, _, Acc2Balance1, _}} =
             {Fee,  {Acc1Balance0, IAmt0, IAmt, Acc1Balance0 + IAmt, Acc1Balance1},
                    {Acc2Balance0, PAmt0, PAmt, Acc2Balance0 + PAmt, Acc2Balance1}},
            none = aesc_test_utils:lookup_channel(ChannelId, S1),
            {IAmt, PAmt}
        end,
    100 = ChannelAmount, % expectation on aesc_test_utils:create_tx_spec/3
    lists:foreach(
        fun(From) ->
            Fee = 10,
            FalfFee = trunc(Fee/2),
            % normal cases
            {45, 45} = Test(From, 50, ChannelAmount - 50, Fee),
            {15, 75} = Test(From, 20, ChannelAmount - 20, Fee),
            % fee edge cases

            % amount - HalfFee = 0
            {0, 90} = Test(From, FalfFee, ChannelAmount - FalfFee, Fee),
            {90, 0} = Test(From, ChannelAmount - FalfFee, FalfFee, Fee),

            % amount - HalfFee < 0
            {0, 90} = Test(From, FalfFee - 1 , ChannelAmount - FalfFee + 1, Fee),
            {90, 0} = Test(From, ChannelAmount - FalfFee + 1, FalfFee - 1, Fee),

            {0, 90} = Test(From, 0, ChannelAmount, Fee),
            {90, 0} = Test(From, ChannelAmount, 0, Fee),

            OddFee = 11,
            SmallerFee = 5,
            BiggerFee = 6,

            {0, 89} = Test(From, SmallerFee, ChannelAmount - SmallerFee, OddFee),
            {0, 89} = Test(From, SmallerFee - 1, ChannelAmount - SmallerFee + 1, OddFee),

            {89, 0} = Test(From, ChannelAmount - SmallerFee, SmallerFee, OddFee),
            {89, 0} = Test(From, ChannelAmount - SmallerFee + 1, SmallerFee - 1, OddFee),

            {0, 89} = Test(From, BiggerFee, ChannelAmount - BiggerFee, OddFee),
            {88, 1} = Test(From, ChannelAmount - BiggerFee, BiggerFee, OddFee),

            {44, 45} = Test(From, 50, 50, OddFee),
            {54, 35} = Test(From, 60, 40, OddFee),

            {39, 0} = Test(From, 90, 10, 61),
            {0, 39} = Test(From, 10, 90, 61)
        end,
        [PubKey1, PubKey2]),
    ok.

calc_mutual_amounts(IAmt, PAmt, Fee) ->
    CFee = ceil(Fee / 2),
    FFee = floor(Fee / 2),
    case 1 of
        _ when IAmt >= CFee andalso PAmt >= FFee ->
            {IAmt - CFee, PAmt - FFee};
        _ when IAmt >= FFee andalso PAmt >= CFee ->
            {IAmt - FFee, PAmt - CFee};
        _ when IAmt > PAmt ->
            {IAmt + PAmt - Fee, 0};
        _ ->
            {0, IAmt + PAmt - Fee}
    end.


close_mutual_negative(Cfg) ->
    {PubKey1, _PubKey2, ChannelId, S} = create(Cfg),
    Trees = aesc_test_utils:trees(S),
    Height = 2,

    Ch = aesc_test_utils:get_channel(ChannelId, S),
    ChannelAmount = aesc_channels:total_amount(Ch),

    %% Test bad from account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aesc_test_utils:close_mutual_tx_spec(ChannelId, BadPubKey,
                                                   #{nonce => 2}, S),
    {ok, Tx1} = aesc_close_mutual_tx:new(TxSpec1),
    {error, account_not_peer} =
        aetx:check(Tx1, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test insufficient tokens in channel
    TxSpec2 = aesc_test_utils:close_mutual_tx_spec(
                ChannelId, PubKey1,
                #{initiator_amount => 1,
                  responder_amount => ChannelAmount,
                  fee    => 2}, S),
    {ok, Tx2} = aesc_close_mutual_tx:new(TxSpec2),
    {error, wrong_amounts} =
        aetx:check(Tx2, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:close_mutual_tx_spec(ChannelId, PubKey1,
                                                   #{nonce => 0}, S),
    {ok, Tx3} = aesc_close_mutual_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:close_mutual_tx_spec(<<"abcdefghi">>, PubKey1, S),
    {ok, Tx4} = aesc_close_mutual_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel not active
    Ch51 = aesc_test_utils:get_channel(ChannelId, S),
    Ch52 = aesc_test_utils:close_solo(Ch51),
    S5   = aesc_test_utils:set_channel(Ch52, S),
    TxSpec5 = aesc_test_utils:close_mutual_tx_spec(ChannelId, PubKey1, S5),
    Trees5 = aesc_test_utils:trees(S5),
    {ok, Tx5} = aesc_close_mutual_tx:new(TxSpec5),
    {error, channel_not_active} =
        aetx:check(Tx5, Trees5, Height, ?CONSENSUS_V_0_11_0_VERSION),
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
                                 [SignedTx], Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

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
        aetx:check(Tx1, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test insufficient from account funds
    S2 = aesc_test_utils:set_account_balance(PubKey1, 5, S),
    Trees2 = aesc_test_utils:trees(S2),
    TxSpec2 = aesc_test_utils:deposit_tx_spec(
                ChannelId, PubKey1,
                #{amount => 10,
                  fee    => 2}, S2),
    {ok, Tx2} = aesc_deposit_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey1, #{nonce => 0}, S),
    {ok, Tx3} = aesc_deposit_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:deposit_tx_spec(<<"abcdefghi">>, PubKey1, S),
    {ok, Tx4} = aesc_deposit_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

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
        aetx:check(Tx5, Trees5, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test from account not peer
    {PubKey3, S6} = aesc_test_utils:setup_new_account(S),
    TxSpec6 = aesc_test_utils:deposit_tx_spec(ChannelId, PubKey3, S6),
    Trees6 = aesc_test_utils:trees(S6),
    {ok, Tx6} = aesc_deposit_tx:new(TxSpec6),
    {error, account_not_peer} =
        aetx:check(Tx6, Trees6, Height, ?CONSENSUS_V_0_11_0_VERSION),
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
                                 [SignedTx], Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

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
        aetx:check(Tx1, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test insufficient from account funds
    S2 = aesc_test_utils:set_account_balance(PubKey1, 5, S),
    Trees2 = aesc_test_utils:trees(S2),
    TxSpec2 = aesc_test_utils:withdraw_tx_spec(
                ChannelId, PubKey1,
                #{amount => 10,
                  fee    => 2}, S2),
    {ok, Tx2} = aesc_withdraw_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1, #{nonce => 0}, S),
    {ok, Tx3} = aesc_withdraw_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:withdraw_tx_spec(<<"abcdefghi">>, PubKey1, S),
    {ok, Tx4} = aesc_withdraw_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

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
        aetx:check(Tx5, Trees5, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test from account not peer
    {PubKey3, S6} = aesc_test_utils:setup_new_account(S),
    TxSpec6 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey3, S6),
    Trees6 = aesc_test_utils:trees(S6),
    {ok, Tx6} = aesc_withdraw_tx:new(TxSpec6),
    {error, account_not_peer} =
        aetx:check(Tx6, Trees6, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test withdrawn amount exceeds channel funds
    TxSpec7 = aesc_test_utils:withdraw_tx_spec(ChannelId, PubKey1, S),
    {ok, Tx7} = aesc_withdraw_tx:new(TxSpec7),
    {error, not_enough_channel_funds} =
        aetx:check(Tx7, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),
    ok.

get_balances(K1, K2, S) ->
    Acc1 = aesc_test_utils:get_account(K1, S),
    Acc1Balance = aec_accounts:balance(Acc1),
    Acc2 = aesc_test_utils:get_account(K2, S),
    Acc2Balance = aec_accounts:balance(Acc2),
    {Acc1Balance, Acc2Balance}.

%%%===================================================================
%%% Slash
%%%===================================================================
slash(Cfg) ->
    {PubKey1, PubKey2, ChannelId, S0} = create(Cfg),
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
    %% Create close_solo tx and apply it on state trees
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance,
                    previous_round => 11,
                    round => 12}, % greater than default of 11
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),
    Test =
        fun(From, FromPrivKey) ->
            TxSpec = aesc_test_utils:slash_tx_spec(ChannelId, From, Payload,
                                                    #{fee    => Fee}, S),
            {ok, Tx} = aesc_slash_tx:new(TxSpec),
            SignedTx = aetx_sign:sign(Tx, [FromPrivKey]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            case From =:= PubKey1 of
                true ->
                    Acc1Balance1 = Acc1Balance0 - Fee,
                    Acc2Balance1 = Acc2Balance0;
                false ->
                    Acc1Balance1 = Acc1Balance0,
                    Acc2Balance1 = Acc2Balance0 - Fee
            end
        end,
    Test(PubKey1, PrivKey1),
    Test(PubKey2, PrivKey2),
    ok.

slash_negative(Cfg) ->
    {PubKey1, PubKey2, ChannelId, S0} = create(Cfg),
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

    InitiatorEndBalance = rand:uniform(ChannelAmount - 2) + 1,
    ResponderEndBalance = ChannelAmount - InitiatorEndBalance,
    PayloadSpec = #{initiator_amount => InitiatorEndBalance,
                    responder_amount => ResponderEndBalance},
    Payload = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpec),

    %% Test not closed channel
    TxSpec0 = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                 Payload, S0),
    {ok, Tx0} = aesc_slash_tx:new(TxSpec0),
    {error, channel_not_closing} =
        aetx:check(Tx0, Trees0, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test bad from account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aesc_test_utils:slash_tx_spec(ChannelId, BadPubKey,
                                                 Payload, S),
    {ok, Tx1} = aesc_slash_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test wrong amounts (different than channel balance)
    TestWrongAmounts =
        fun(IAmt, PAmt) ->
            PayloadSpecW = #{initiator_amount => IAmt,
                            responder_amount => PAmt},
            PayloadW = aesc_test_utils:payload(ChannelId, PubKey1, PubKey2,
                                      [PrivKey1, PrivKey2], PayloadSpecW),
            TxSpecW = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                         PayloadW, S),
            {ok, TxW} = aesc_slash_tx:new(TxSpecW),
            {error, wrong_state_amount} =
                aetx:check(TxW, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION)
        end,
    TestWrongAmounts(InitiatorEndBalance -1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance +1, ResponderEndBalance),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance - 1),
    TestWrongAmounts(InitiatorEndBalance, ResponderEndBalance + 1),

    %% Test from account not peer
    {PubKey3, SNotPeer} = aesc_test_utils:setup_new_account(S),
    PrivKey3 = aesc_test_utils:priv_key(PubKey3, SNotPeer),

    TxSpecNotPeer = aesc_test_utils:slash_tx_spec(ChannelId, PubKey3, Payload, SNotPeer),
    TreesNotPeer = aesc_test_utils:trees(SNotPeer),
    {ok, TxNotPeer} = aesc_slash_tx:new(TxSpecNotPeer),
    {error, account_not_peer} =
        aetx:check(TxNotPeer, TreesNotPeer, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test too high from account nonce
    TxSpecWrongNonce = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                          Payload, #{nonce => 0}, S),
    {ok, TxWrongNonce} = aesc_slash_tx:new(TxSpecWrongNonce),
    {error, account_nonce_too_high} =
        aetx:check(TxWrongNonce, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test payload has different channelId
    TxSpecDiffChanId = aesc_test_utils:slash_tx_spec(<<"abcdefghi">>, PubKey1,
                                                          Payload, S),
    {ok, TxDiffChanId} = aesc_slash_tx:new(TxSpecDiffChanId),
    {error, bad_state_channel_id} =
        aetx:check(TxDiffChanId, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel missing
    MissingChannelId = <<"abcdefghi">>,
    PayloadMissingChanId = aesc_test_utils:payload(MissingChannelId, PubKey1, PubKey2,
                                                  [PrivKey1, PrivKey2], PayloadSpec),
    TxSpecNoChan = aesc_test_utils:slash_tx_spec(MissingChannelId, PubKey1,
                                               PayloadMissingChanId, S),
    {ok, TxNoChan} = aesc_slash_tx:new(TxSpecNoChan),
    {error, channel_does_not_exist} =
        aetx:check(TxNoChan, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test reject payload with missing signatures
    TestPayloadSigners =
        fun(PrivKeys) ->
            PayloadMissingS = aesc_test_utils:payload(MissingChannelId, PubKey1, PubKey2,
                                                  PrivKeys, PayloadSpec),
            TxSpecMissingS = aesc_test_utils:slash_tx_spec(ChannelId, PubKey1,
                                                      PayloadMissingS, S),
            {ok, TxMissingS} = aesc_slash_tx:new(TxSpecMissingS),
            {error, signature_check_failed} =
                aetx:check(TxMissingS, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION)
        end,
    TestPayloadSigners([]),
    TestPayloadSigners([PrivKey1]),
    TestPayloadSigners([PrivKey2]),

    %% Test reject payload with wrong signers
    TestPayloadWrongPeers =
        fun(I, P, PrivKeys) ->
            PayloadMissingS = aesc_test_utils:payload(ChannelId, I, P,
                                                  PrivKeys, PayloadSpec),
            TxSpecMissingS = aesc_test_utils:slash_tx_spec(ChannelId, I,
                                                      PayloadMissingS, S),
            {ok, TxMissingS} = aesc_slash_tx:new(TxSpecMissingS),
            {error, wrong_channel_peers} =
                aetx:check(TxMissingS, Trees, Height, ?CONSENSUS_V_0_11_0_VERSION)
        end,
    TestPayloadWrongPeers(PubKey1, PubKey3, [PrivKey1, PrivKey3]),
    TestPayloadWrongPeers(PubKey2, PubKey3, [PrivKey2, PrivKey3]),

    %% Test existing channel's payload
    Cfg2 = lists:keyreplace(state, 1, Cfg, {state, S}),
    {PubKey21, PubKey22, ChannelId2, S2} = create(Cfg2),
    Trees2 = aens_test_utils:trees(S2),
    PrivKey21 = aesc_test_utils:priv_key(PubKey21, S2),
    PrivKey22 = aesc_test_utils:priv_key(PubKey22, S2),
    Payload2 = aesc_test_utils:payload(ChannelId2, PubKey21, PubKey22,
                                      [PrivKey21, PrivKey22], PayloadSpec),
    TxPayload2Spec = aesc_test_utils:slash_tx_spec(ChannelId, PubKey21,
                                                      Payload2, S2),
    {ok, TxPayload2} = aesc_slash_tx:new(TxPayload2Spec),
    {error, bad_state_channel_id} =
                aetx:check(TxPayload2, Trees2, Height + 2, ?CONSENSUS_V_0_11_0_VERSION),
    ok.

%%%===================================================================
%%% Settle
%%%===================================================================

settle(Cfg) ->
    {PubKey1, PubKey2, ChannelId, S0} = create(Cfg),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S0),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S0),

    %% Get channel and account funds

    {Acc1Balance0, Acc2Balance0} = get_balances(PubKey1, PubKey2, S0),
    Ch0 = aesc_test_utils:get_channel(ChannelId, S0),


    100 = ChannelAmount = aesc_channels:total_amount(Ch0),
    %% Create close_mutual tx and apply it on state trees
    Test =
        fun(From, IAmt, PAmt, Fee) ->
            Ch = aesc_test_utils:close_solo(Ch0, #{initiator_amount => IAmt,
                                                   responder_amount => PAmt}),
            ClosesAt = aesc_channels:closes_at(Ch),
            ChannelAmount = IAmt + PAmt, %% assert

            S = aesc_test_utils:set_channel(Ch, S0),
            Trees = aens_test_utils:trees(S),

            TxSpec = aesc_test_utils:settle_tx_spec(ChannelId, From,
                                                    #{initiator_amount => IAmt,
                                                      responder_amount => PAmt,
                                                      ttl => 1001,
                                                      fee    => Fee}, S),
            {ok, Tx} = aesc_settle_tx:new(TxSpec),
            SignedTx = aetx_sign:sign(Tx, [PrivKey1, PrivKey2]),
            {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                        [SignedTx], Trees, ClosesAt, ?CONSENSUS_V_0_11_0_VERSION),
            S1 = aesc_test_utils:set_trees(Trees1, S),

            {Acc1Balance1, Acc2Balance1} = get_balances(PubKey1, PubKey2, S1),
            {IFee, PFee} =
                case From of
                    PubKey1 -> {Fee, 0};
                    PubKey2 -> {0, Fee}
                end,
            % ensure balances are updated
            {_, {_, _, _, Acc1Balance1, _}, {_, _, _, Acc2Balance1, _}} =
             {Fee,  {Acc1Balance0, IFee, IAmt, Acc1Balance0 + IAmt - IFee, Acc1Balance1},
                    {Acc2Balance0, PFee, PAmt, Acc2Balance0 + PAmt - PFee, Acc2Balance1}},
            none = aesc_test_utils:lookup_channel(ChannelId, S1),
            {IAmt, PAmt}
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
    {PubKey1, _PubKey2, ChannelId, S0} = create(Cfg),
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
        aetx:check(Tx0, Trees0, Height, ?CONSENSUS_V_0_11_0_VERSION),

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
        aetx:check(Tx, Trees, ClosesAt - 1, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test bad from account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aesc_test_utils:settle_tx_spec(ChannelId, BadPubKey,
                                                   #{nonce => 2,
                                                    ttl => ClosesAt + 1}, S),
    {ok, Tx1} = aesc_settle_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, ClosesAt, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test insufficient different tokens distribution than in channel
    TxSpec2 = aesc_test_utils:settle_tx_spec(
                ChannelId, PubKey1,
                #{initiator_amount => 1,
                  responder_amount => ChannelAmount - 1,
                  ttl    => ClosesAt + 1,
                  fee    => 2}, S),
    {ok, Tx2} = aesc_settle_tx:new(TxSpec2),
    {error, wrong_amt} =
        aetx:check(Tx2, Trees, ClosesAt, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test too high from account nonce
    TxSpec3 = aesc_test_utils:settle_tx_spec(ChannelId, PubKey1,
                                                   #{nonce => 0}, S),
    {ok, Tx3} = aesc_settle_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, ClosesAt, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test channel does not exist
    TxSpec4 = aesc_test_utils:settle_tx_spec(<<"abcdefghi">>, PubKey1,
                                             #{ttl => ClosesAt}, S),
    {ok, Tx4} = aesc_settle_tx:new(TxSpec4),
    {error, channel_does_not_exist} =
        aetx:check(Tx4, Trees, ClosesAt, ?CONSENSUS_V_0_11_0_VERSION),

    %% Test only one settle 
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S),
    SignedTx = aetx_sign:sign(Tx, [PrivKey1]),
    {ok, [SignedTx], Trees1} = aesc_test_utils:apply_on_trees_without_sigs_check(
                                [SignedTx], Trees, ClosesAt, ?CONSENSUS_V_0_11_0_VERSION),
    S5 = aesc_test_utils:set_trees(Trees1, S),

    TxSpec5 = aesc_test_utils:settle_tx_spec(ChannelId, PubKey1,
                                      #{initiator_amount => ChannelAmount,
                                        ttl => ClosesAt + 2,
                                        responder_amount => 0}, S5),
    {ok, Tx5} = aesc_close_mutual_tx:new(TxSpec5),
    {error, channel_does_not_exist} =
        aetx:check(Tx5, Trees1, ClosesAt + 2, ?CONSENSUS_V_0_11_0_VERSION),
  ok.
