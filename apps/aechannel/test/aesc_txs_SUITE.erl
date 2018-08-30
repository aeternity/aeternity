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
         close_solo/1,
         close_mutual/1,
         slash/1,
         slash_by_delegate/1,
         deposit/1,
         withdraw/1,
         settle/1,
         snapshot_solo/1]).

% negative create
-export([create_missing_account/1,
         create_insufficient_funds/1,
         create_wrong_nonce/1,
         create_insufficient_funds_reserve/1,
         create_exsisting/1
         ]).

% negative close solo
-export([close_solo_unknown_from/1,
         close_solo_wrong_amounts/1,
         close_solo_not_participant/1,
         close_solo_wrong_nonce/1,
         close_solo_payload_from_another_channel/1,
         close_solo_payload_not_co_signed/1,
         close_solo_invalid_state_hash/1,
         close_solo_older_payload/1,
         close_solo_missing_channel/1,
         close_solo_already_closing/1,
         close_solo_delegate_not_allowed/1
         ]).

% negative close mutual
% close mutual does not have a `from` - it is always implicitly the initiator
% thus we can not test the tx being posted from another account (not
% participant or a delegate). If it is signed by non-participant - the
% signature test will fail
-export([close_mutual_wrong_amounts/1,
         close_mutual_wrong_nonce/1,
         close_mutual_missing_channel/1,
         close_mutual_already_closing/1
        ]).

% negative slash
-export([slash_not_closing/1,
         slash_unknown_from/1,
         slash_wrong_amounts/1,
         slash_not_participant/1,
         slash_wrong_nonce/1,
         slash_payload_from_another_channel/1,
         slash_payload_not_co_signed/1,
         slash_invalid_state_hash/1,
         slash_older_payload/1,
         slash_missing_channel/1
         ]).

% negative settle
-export([settle_wrong_amounts/1,
         settle_wrong_nonce/1,
         settle_missing_channel/1,
         settle_not_closing/1,
         settle_not_yet_closable/1,
         settle_not_participant/1,
         settle_delegate_not_allowed/1
        ]).

% negative deposit
-export([deposit_unknown_from/1,
         deposit_insufficent_funds/1,
         deposit_wrong_nonce/1,
         deposit_missing_channel/1,
         deposit_closing/1,
         deposit_older_round/1,
         deposit_not_participant/1,
         deposit_delegate_not_allowed/1
        ]).

% negative withdraw
-export([withdraw_unknown_from/1,
         withdraw_insufficent_funds/1,
         withdraw_wrong_nonce/1,
         withdraw_missing_channel/1,
         withdraw_closing/1,
         withdraw_older_round/1,
         withdraw_not_participant/1,
         withdraw_delegate_not_allowed/1
        ]).


% negative snapshot solo
-export([snapshot_closed_channel/1,
         snapshot_closing_channel/1,
         snapshot_missing_channel/1,
         snapshot_payload_from_another_channel/1,
         snapshot_payload_not_co_signed/1,
         snapshot_old_payload/1,
         snapshot_not_participant/1,
         snapshot_delegate_not_allowed/1
        ]).

% positive force progress
-export([fp_after_create/1,
         fp_after_deposit/1,
         fp_after_withdrawal/1,
         fp_after_fp_missing_rounds/1,
         fp_on_top_of_fp/1,
         fp_after_snapshot/1,
         fp_is_replaced_by_same_round_deposit/1,
         fp_is_replaced_by_same_round_withdrawal/1,
         fp_is_replaced_by_same_round_snapshot/1,
         % already closing
         fp_after_solo_close/1,
         fp_after_slash/1
        ]).

% negative force progress
-export([fp_closed_channel/1,
         fp_not_participant/1,
         fp_missing_channel/1,
         % co-signed payload tests
         fp_payload_from_another_channel/1,
         fp_payload_not_co_signed/1,
         fp_payload_invalid_state_hash/1,
         fp_payload_older_payload/1,
         % solo signed payload tests
         fp_solo_payload_from_another_channel/1,
         fp_solo_payload_invalid_state_hash/1,
         fp_solo_payload_wrong_round/1,
         fp_solo_payload_no_update/1,
         fp_solo_payload_multiple_updates/1,
         fp_solo_payload_not_call_update/1,
         fp_solo_payload_broken_call/1,
         % poi tests
         fp_missing_account_address/1,
         fp_missing_contract_address/1,

         fp_insufficent_tokens/1,
         fp_too_soon/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MINER_PUBKEY, <<12345:?MINER_PUB_BYTES/unit:8>>).
-define(BOGUS_CHANNEL, <<0:?MINER_PUB_BYTES/unit:8>>).
-define(ROLES, [initiator, responder]).
-define(VM_VERSION, 1).
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
       {group, create_negative},
       close_solo,
       {group, close_solo_negative},
       close_mutual,
       {group, close_mutual_negative},
       slash,
       slash_by_delegate,
       {group, slash_negative},
       deposit,
       {group, deposit_negative},
       withdraw,
       {group, withdraw_negative},
       settle,
       {group, settle_negative},
       snapshot_solo,
       {group, snapshot_solo_negative},
       {group, force_progress},
       {group, force_progress_negative}]
     },
     {create_negative, [sequence],
      [create_missing_account,
       create_insufficient_funds,
       create_wrong_nonce,
       create_insufficient_funds_reserve,
       create_exsisting
      ]},
     {close_solo_negative, [sequence],
      [close_solo_unknown_from,
       close_solo_wrong_amounts,
       close_solo_not_participant,
			 close_solo_wrong_nonce,
       close_solo_payload_from_another_channel,
       close_solo_payload_not_co_signed,
       close_solo_invalid_state_hash,
       close_solo_older_payload,
       close_solo_missing_channel,
       close_solo_already_closing,
       close_solo_delegate_not_allowed
      ]},
     {close_mutual_negative, [sequence],
      [close_mutual_wrong_amounts,
       close_mutual_wrong_nonce,
       close_mutual_missing_channel,
       close_mutual_already_closing
      ]},
     {slash_negative, [sequence],
      [slash_not_closing,
       slash_unknown_from,
       slash_wrong_amounts,
       slash_not_participant,
			 slash_wrong_nonce,
       slash_payload_from_another_channel,
       slash_payload_not_co_signed,
       slash_invalid_state_hash,
       slash_older_payload,
       slash_missing_channel
      ]},
     {settle_negative, [sequence],
      [settle_wrong_amounts,
       settle_wrong_nonce,
       settle_missing_channel,
       settle_not_closing,
       settle_not_yet_closable,
       settle_not_participant,
       settle_delegate_not_allowed
      ]},
     {deposit_negative, [sequence],
      [deposit_unknown_from,
       deposit_insufficent_funds,
       deposit_wrong_nonce,
       deposit_missing_channel,
       deposit_closing,
       deposit_older_round,
       deposit_not_participant,
       deposit_delegate_not_allowed
      ]},
     {withdraw_negative, [sequence],
      [withdraw_unknown_from,
       withdraw_insufficent_funds,
       withdraw_wrong_nonce,
       withdraw_missing_channel,
       withdraw_closing,
       withdraw_older_round,
       withdraw_not_participant,
       withdraw_delegate_not_allowed
      ]},
     {snapshot_solo_negative, [sequence],
      [snapshot_closed_channel,
       snapshot_closing_channel,
       snapshot_missing_channel,
       snapshot_payload_from_another_channel,
       snapshot_payload_not_co_signed,
       snapshot_old_payload,
       snapshot_not_participant,
       snapshot_delegate_not_allowed
      ]},
     {force_progress, [sequence],
      [fp_after_create,
       fp_after_deposit,
       fp_after_withdrawal,
       fp_after_fp_missing_rounds,
       fp_on_top_of_fp,
       fp_after_snapshot,
       fp_is_replaced_by_same_round_deposit,
       fp_is_replaced_by_same_round_withdrawal,
       fp_is_replaced_by_same_round_snapshot,
       % already closing
       fp_after_solo_close,
       fp_after_slash
      ]},
     {force_progress_negative, [sequence],
      [fp_closed_channel,
       fp_not_participant,
       fp_missing_channel,
       % co-signed payload tests
       fp_payload_from_another_channel,
       fp_payload_not_co_signed,
       fp_payload_invalid_state_hash,
       fp_payload_older_payload,
       % solo signed payload tests
       fp_solo_payload_from_another_channel,
       fp_solo_payload_invalid_state_hash,
       fp_solo_payload_wrong_round,
       fp_solo_payload_no_update,
       fp_solo_payload_multiple_updates,
       fp_solo_payload_not_call_update,
       fp_solo_payload_broken_call,
       % poi tests
       fp_missing_account_address,
       fp_missing_contract_address,

       fp_insufficent_tokens,
       fp_too_soon
      ]}
    ].

%%%===================================================================

create(Cfg) ->
    run(#{cfg => Cfg},
        [positive(fun create_channel_/2)]).

create_missing_account(_Cfg) ->
    {PubKey1, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    Trees = aesc_test_utils:trees(S),
    Height = 1,

    BadPubKey = <<42:32/unit:8>>,

    TxSpec1 = aesc_test_utils:create_tx_spec(BadPubKey, PubKey1, S),
    {ok, Tx1} = aesc_create_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    TxSpec2 = aesc_test_utils:create_tx_spec(PubKey1, BadPubKey, S),
    {ok, Tx2} = aesc_create_tx:new(TxSpec2),
    {error, account_not_found} =
        aetx:check(Tx2, Trees, Height, ?PROTOCOL_VERSION),
    ok.

create_insufficient_funds(_Cfg) ->
    {Loaded, NotLoaded, S} = create_loaded_accounts(100, 1),
    Trees = aesc_test_utils:trees(S),
    Height = 1,

    %% Test insufficient initiator funds
    TxSpecI = aesc_test_utils:create_tx_spec(
                NotLoaded, Loaded,
                #{initiator_amount => 1,
                  fee => 2}, S),
    {ok, TxI} = aesc_create_tx:new(TxSpecI),
    {error, insufficient_funds} =
        aetx:check(TxI, Trees, Height, ?PROTOCOL_VERSION),

    %% Test insufficient responder funds
    TxSpecR = aesc_test_utils:create_tx_spec(
                Loaded, NotLoaded,
                #{initiator_amount => 1,
                  fee => 2}, S),
    {ok, TxR} = aesc_create_tx:new(TxSpecR),
    {error, insufficient_funds} =
        aetx:check(TxR, Trees, Height, ?PROTOCOL_VERSION),
    ok.


create_insufficient_funds_reserve(_Cfg) ->
    {Loaded, NotLoaded, S} = create_loaded_accounts(100, 10),
    Trees = aesc_test_utils:trees(S),
    Height = 1,

    %% Test initiator funds lower than channel reserve
    TxSpecI = aesc_test_utils:create_tx_spec(
                NotLoaded, Loaded,
                #{initiator_amount => 1,
                  channel_reserve => 2,
                  fee => 1}, S),
    {ok, TxI} = aesc_create_tx:new(TxSpecI),
    {error, insufficient_initiator_amount} =
        aetx:check(TxI, Trees, Height, ?PROTOCOL_VERSION),

    %% Test responder funds lower than channel reserve
    TxSpecR = aesc_test_utils:create_tx_spec(
                Loaded, NotLoaded,
                #{responder_amount => 1,
                  channel_reserve => 2,
                  fee => 1}, S),
    {ok, TxR} = aesc_create_tx:new(TxSpecR),
    {error, insufficient_responder_amount} =
        aetx:check(TxR, Trees, Height, ?PROTOCOL_VERSION),
    ok.

create_loaded_accounts(FAmt, SAmt) ->
    {First, S1} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    {Second, S2} = aesc_test_utils:setup_new_account(S1),
    S30 = aesc_test_utils:set_account_balance(First, FAmt, S2),
    S3 = aesc_test_utils:set_account_balance(Second, SAmt, S30),
    _ = aesc_test_utils:priv_key(First, S3),
    _ = aesc_test_utils:priv_key(Second, S3),
    {First, Second, S3}.

create_wrong_nonce(_Cfg) ->
    {Initiator, Responder, S0} = create_loaded_accounts(100, 100),
		Nonce = 42,
    S = aesc_test_utils:set_account_nonce(Initiator, Nonce, S0),
    Trees = aesc_test_utils:trees(S),
    Height = 1,

    Test =
        fun(TestNonce, Err) ->
            TxSpec = aesc_test_utils:create_tx_spec(Initiator, Responder,
                                                    #{nonce => TestNonce}, S),
            {ok, Tx} = aesc_create_tx:new(TxSpec),
            {error, Err} =
                aetx:check(Tx, Trees, Height, ?PROTOCOL_VERSION)
        end,
    Test(Nonce - 1, account_nonce_too_high),
    Test(Nonce, account_nonce_too_high),
    Test(Nonce + 2, account_nonce_too_low),
    ok.

create_exsisting(Cfg) ->
    run(#{cfg => Cfg},
        [positive(fun create_channel_/2),
         % set initiator nonce back to 0 so we try creating the same channel
         fun(#{state := S0, initiator_pubkey := Initiator} = Props) ->
            S = aesc_test_utils:set_account_nonce(Initiator, 0, S0),
            Props#{state => S}
         end,
         negative(fun create_channel_/2, {error, channel_exists})
        ]).

%%%===================================================================
%%% Close solo
%%%===================================================================
close_solo(Cfg) ->
    Test =
        fun(Closer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end])
        end,
    [Test(Role) || Role <- ?ROLES],

    IStartAmt = 10,
    RStartAmt = 10,
    TestEmptyPayload =
        fun(Closer) ->
            run(#{cfg => Cfg, initiator_amount => IStartAmt, responder_amount => RStartAmt,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(payload, <<>>),
                calc_poi_by_balances(IStartAmt, RStartAmt),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end])
        end,
    [TestEmptyPayload(Role) || Role <- ?ROLES],

    Amount = 3,

    TestEmptyPayloadDeposit =
        fun(Depositor, Closer) ->
            run(#{cfg => Cfg, initiator_amount => IStartAmt, responder_amount => RStartAmt,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Depositor),
                set_prop(amount, Amount),
                calc_poi_by_balances(IStartAmt + Amount, RStartAmt),
                % calc poi so balances match the expectations
                fun(#{poi := PoI} = Props) ->
                    PoIHash = aec_trees:poi_hash(PoI),
                    Props#{state_hash => PoIHash}
                end,
                positive(fun deposit_/2),
                set_from(Closer),
                set_prop(payload, <<>>),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end])
        end,
    [TestEmptyPayloadDeposit(Depositor, Closer) || Depositor <- ?ROLES,
                                                   Closer <- ?ROLES],

    TestEmptyPayloadWithdrawal =
        fun(Withdrawer, Closer) ->
            run(#{cfg => Cfg, initiator_amount => IStartAmt, responder_amount => RStartAmt,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Withdrawer),
                set_prop(amount, Amount),
                calc_poi_by_balances(IStartAmt - Amount, RStartAmt),
                % calc poi so balances match the expectations
                fun(#{poi := PoI} = Props) ->
                    PoIHash = aec_trees:poi_hash(PoI),
                    Props#{state_hash => PoIHash}
                end,
                positive(fun withdraw_/2),
                set_from(Closer),
                set_prop(payload, <<>>),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end])
        end,
    [TestEmptyPayloadWithdrawal(Withdrawer, Closer) || Withdrawer <- ?ROLES,
                                                       Closer <- ?ROLES],
    ok.

calc_poi_by_balances(IB, RB) ->
    fun(#{initiator_pubkey := I, responder_pubkey := R} = Props) ->
        PoI = aesc_test_utils:proof_of_inclusion([{I, IB},{R, RB}]),
        Props#{poi => PoI}
    end.

calc_poi(Accounts, Contracts, Trees) ->
    AddPoI =
        fun(Key, InitPoI, Pubkeys) ->
            lists:foldl(
                fun(Pubkey, AccumPoI) ->
                    {ok, P} = aec_trees:add_poi(Key, Pubkey, Trees,
                                                AccumPoI),
                    P
                end,
                InitPoI,
                Pubkeys)
        end,
    PoI0 = AddPoI(accounts, aec_trees:new_poi(Trees), Accounts ++ Contracts),
    PoI  = AddPoI(contracts, PoI0, Contracts),
    PoI.

close_solo_unknown_from(Cfg) ->
    {MissingAccount, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    PrivKey = aesc_test_utils:priv_key(MissingAccount, S),
    run(#{cfg => Cfg},
        [positive(fun create_channel_/2),
         set_prop(from_pubkey, MissingAccount),
         set_prop(from_privkey, PrivKey),
         negative(fun close_solo_/2, {error, account_not_found})]),
    ok.

%% Test wrong amounts (different than channel balance)
close_solo_wrong_amounts(Cfg) ->
    IAmt = 35,
    RAmt = 42,
    Test =
        fun(Closer, ICloseAmt, RCloseAmt) ->
            run(#{cfg => Cfg, initiator_amount => IAmt, responder_amount => RAmt},
                [positive(fun create_channel_/2),
                 set_prop(initiator_amount, ICloseAmt),
                 set_prop(responder_amount, RCloseAmt),
                 set_from(Closer),
                 negative(fun close_solo_/2, {error, poi_amounts_change_channel_funds})])
        end,
    lists:foreach(
        fun(Closer) ->
            Test(Closer, IAmt + 1, RAmt),
            Test(Closer, IAmt, RAmt + 1)
        end,
        ?ROLES),
    ok.

%% Test not a peer can not close channel
close_solo_not_participant(Cfg) ->
    test_not_participant(Cfg, fun close_solo_/2).

close_solo_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun close_solo_/2).

close_solo_payload_from_another_channel(Cfg) ->
    test_both_payload_from_different_channel(Cfg, fun close_solo_/2).

close_solo_payload_not_co_signed(Cfg) ->
    test_payload_not_both_signed(Cfg, fun aesc_test_utils:close_solo_tx_spec/5,
                                      fun aesc_close_solo_tx:new/1).

close_solo_invalid_state_hash(Cfg) ->
    test_both_invalid_poi_hash(Cfg, fun close_solo_/2).

close_solo_older_payload(Cfg) ->
    test_both_old_round(Cfg, fun close_solo_/2).

close_solo_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun close_solo_/2).

close_solo_already_closing(Cfg) ->
    test_both_closing_channel(Cfg, fun close_solo_/2).

close_solo_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun close_solo_/2).

%%%===================================================================
%%% Close mutual
%%%===================================================================

close_mutual(Cfg) ->
    StartIAmt = 50,
    StartRAmt = 50,
    ChannelAmount = StartIAmt + StartRAmt,

    Test =
        fun(IAmt, RAmt, Fee) ->
            run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
               [positive(fun create_channel_/2),
                get_onchain_balances(before_close),
                set_prop(initiator_amount_final, IAmt),
                set_prop(responder_amount_final, RAmt),
                set_prop(fee, Fee),
                positive(fun close_mutual_/2),
                get_onchain_balances(after_close),
                % this function returns the closing amount deltas
                fun(#{before_close := #{initiator := I0, responder := R0},
                      after_close  := #{initiator := I1, responder := R1}}) ->
                    {I1 - I0, R1 - R0}
                end])
        end,
    Fee = 10,

    %% normal cases
    {45, 45} = Test(45, 45, Fee),
    {15, 75} = Test(15, ChannelAmount - 15 - Fee, Fee),

    %% fee edge cases
    %% amount - HalfFee = 0
    {0, 90} = Test(0, ChannelAmount - Fee, Fee),
    {90, 0} = Test(ChannelAmount - Fee, 0, Fee),

    %% amount - HalfFee < 0
    {1, 89} = Test(1 , ChannelAmount - Fee - 1, Fee),
    {89, 1} = Test(ChannelAmount - Fee - 1, 1, Fee),

    ok.

close_mutual_wrong_amounts(Cfg) ->
    StartIAmt = 50,
    StartRAmt = 50,

    Test =
        fun(IAmt, RAmt, Fee, Err) ->
            run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
               [positive(fun create_channel_/2),
                set_prop(initiator_amount_final, IAmt),
                set_prop(responder_amount_final, RAmt),
                set_prop(fee, Fee),
                negative(fun close_mutual_/2, {error, Err})])
        end,

    % sum too big
    Test(50, 50, 10, wrong_amounts),
    % nonce too small
    Test(50, 50, 0, too_low_fee),
    ok.

close_mutual_wrong_nonce(Cfg) ->
    InitiatorNonce = 42,
    Test =
        fun(TestNonce, Error) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                 fun(#{state := S0, initiator_pubkey := I} = Props) ->
                    S = aesc_test_utils:set_account_nonce(I, InitiatorNonce, S0),
                    Props#{state => S}
                 end,
                 set_prop(nonce, TestNonce),
                 %% prepare balances and a fee..
                 prepare_balances_for_mutual_close(),
                 negative(fun close_mutual_/2, {error, Error})])
        end,
    Test(InitiatorNonce - 1,  account_nonce_too_high),
    Test(InitiatorNonce,      account_nonce_too_high),
    Test(InitiatorNonce + 2,  account_nonce_too_low),
    ok.


close_mutual_missing_channel(Cfg) ->
    ChannelHashSize = aec_base58c:byte_size_for_type(channel),
    FakeChannelPubKey = <<42:ChannelHashSize/unit:8>>,
    run(#{cfg => Cfg},
        [positive(fun create_channel_/2),
         %% prepare balances and a fee..
         prepare_balances_for_mutual_close(),
         set_prop(channel_pubkey, FakeChannelPubKey),
         negative(fun close_mutual_/2, {error, channel_does_not_exist})]),
    ok.

close_mutual_already_closing(Cfg) ->
    Test =
        fun(Closer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                %% prepare balances and a fee..
                prepare_balances_for_mutual_close(),
                negative(fun close_mutual_/2, {error, channel_not_active})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

%%%===================================================================
%%% Slash
%%%===================================================================
slash(Cfg) ->
    Test =
        fun(Closer, Slasher, Round0, Round1) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, Round0),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_from(Slasher),
                set_prop(round, Round1),
                positive(fun slash_/2)])
        end,
    lists:foreach(
        fun(Closer) ->
            lists:foreach(
                fun(Slasher) ->
                    Test(Closer, Slasher, 1, 2),
                    Test(Closer, Slasher, 1, 5),
                    Test(Closer, Slasher, 5, 6)
                end,
                ?ROLES)
        end,
        ?ROLES),
    ok.

slash_by_delegate(Cfg) ->
    Test =
        fun(Closer, Round0, Round1) ->
            run(#{cfg => Cfg},
               [fun(Props) ->
                    {Delegate1, Delegate2, S} = create_loaded_accounts(100, 100),
                    Props#{cfg => [{state, S} | Cfg],
                           delegate_ids => [aec_id:create(account, Delegate1),
                                            aec_id:create(account, Delegate2)]}
                end,
                positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, Round0),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_prop(round, Round1),
                fun(#{delegate_ids := [D1 |_], state := S} = Props) ->
                    D1Pubkey = aec_id:specialize(D1, account),
                    D1PrivKey = aesc_test_utils:priv_key(D1Pubkey, S),
                    Props#{from_pubkey => D1Pubkey, from_privkey => D1PrivKey}
                end,
                positive(fun slash_/2)])
        end,
    lists:foreach(
        fun(Closer) ->
            Test(Closer, 1, 2),
            Test(Closer, 1, 5),
            Test(Closer, 5, 6)
        end,
        ?ROLES),
    ok.

slash_not_closing(Cfg) ->
    Test =
        fun(Slasher) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                 set_from(Slasher),
                 negative(fun slash_/2, {error, channel_not_closing})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

slash_unknown_from(Cfg) ->
    {MissingAccount, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    PrivKey = aesc_test_utils:priv_key(MissingAccount, S),
    Test =
        fun(Closer) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                 set_from(Closer),
                 positive(fun close_solo_/2),
                 set_prop(from_pubkey, MissingAccount),
                 set_prop(from_privkey, PrivKey),
                 negative(fun slash_/2, {error, account_not_found})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

%% Test wrong amounts (different than channel balance)
slash_wrong_amounts(Cfg) ->
    IAmt = 50,
    RAmt = 50,
    Test =
        fun(Closer, Slasher, ICloseAmt0, RCloseAmt0, ICloseAmt, RCloseAmt) ->
            run(#{cfg => Cfg, initiator_amount => IAmt, responder_amount => RAmt},
                [positive(fun create_channel_/2),
                 set_prop(initiator_amount, ICloseAmt0),
                 set_prop(responder_amount, RCloseAmt0),
                 set_from(Closer),
                 set_prop(round, 5),
                 positive(fun close_solo_/2),
                 set_prop(initiator_amount, ICloseAmt),
                 set_prop(responder_amount, RCloseAmt),
                 set_from(Slasher),
                 set_prop(round, 10),
                 negative(fun slash_/2, {error, poi_amounts_change_channel_funds})])
        end,
    lists:foreach(
        fun(Closer) ->
            lists:foreach(
                fun(Slasher) ->
                    % sum is more
                    Test(Closer, Slasher, 40, 60, 49, 52),
                    Test(Closer, Slasher, 40, 60, 52, 49)
                end,
                ?ROLES)
        end,
        ?ROLES),
    ok.

%% Test not a peer can not slash
slash_not_participant(Cfg) ->
    Test =
        fun(Closer) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                 set_from(Closer),
                 set_prop(round, 5),
                 positive(fun close_solo_/2),
                 fun(#{state := S0} = Props) ->
                    {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
                    S1 = aesc_test_utils:set_account_balance(NewAcc, 1000, S),
                    PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
                    Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
                 end,
                 set_prop(round, 10),
                 negative(fun slash_/2, {error, account_not_peer_or_delegate})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

slash_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun slash_/2).

slash_payload_from_another_channel(Cfg) ->
    Test =
        fun(Closer, Slasher) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2), % create a channelA
                % produce a payload for channelA
                create_payload(different_payload),
                % create another channelB and replace the old one with the
                % participansts as well
                positive(fun create_channel_/2),
                % use the payload of channelA in a snapshot_tx for channelB
                set_from(Closer),
                set_prop(round, 5),
                positive(fun close_solo_/2),
                fun(#{different_payload := Payload} = Props) ->
                    Props#{payload => Payload}
                end,
                set_from(Slasher),
                negative(fun slash_/2, {error, bad_state_channel_pubkey})])
        end,
    [Test(Closer, Slasher) || Closer <- ?ROLES,
                              Slasher <- ?ROLES],
    ok.

slash_payload_not_co_signed(Cfg) ->
    Test =
        fun(Closer, Slasher) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2), % create a channelA
                set_from(Closer),
                set_prop(round, 5),
                positive(fun close_solo_/2),
                set_prop(round, 10),
                set_from(Slasher),
                fun(#{channel_pubkey    := ChannelPubKey,
                      initiator_pubkey  := I,
                      responder_pubkey  := R,
                      initiator_privkey := IPriv,
                      responder_privkey := RPriv,
                      initiator_amount  := IAmt,
                      responder_amount  := RAmt,
                      from_pubkey       := FromPubKey,
                      state             := S,
                      height            := Height}) ->
                    lists:foreach(
                        fun(PrivKeys) ->
                            PayloadSpec = #{initiator_amount => IAmt,
                                            responder_amount => RAmt},
                            PayloadMissingS = aesc_test_utils:payload(ChannelPubKey, I, R,
                                                          PrivKeys, PayloadSpec),
                            PoI = aesc_test_utils:proof_of_inclusion([{I, IAmt},
                                                                      {R, RAmt}]),
                            TxSpecMissingS = aesc_test_utils:slash_tx_spec(ChannelPubKey, FromPubKey,
                                                    PayloadMissingS, PoI, S),
                            {ok, TxMissingS} = aesc_slash_tx:new(TxSpecMissingS),
                            Trees = aesc_test_utils:trees(S),
                            {error, signature_check_failed} =
                                aetx:check(TxMissingS, Trees, Height, ?PROTOCOL_VERSION)
                        end,
                        [[],       % not signed at all
                         [IPriv],  % signed only by initiator
                         [RPriv]]) % signed only by responder
                end])
        end,
    [Test(Closer, Slasher) || Closer <- ?ROLES,
                              Slasher <- ?ROLES],
    ok.

slash_invalid_state_hash(Cfg) ->
    StateHashSize = aec_base58c:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Closer, Slasher) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, 5),
                positive(fun close_solo_/2),
                set_from(Slasher),
                set_prop(round, 10),
                set_prop(state_hash, FakeStateHash),
                set_from(Slasher),
                negative(fun slash_/2, {error, invalid_poi_hash})])
        end,
    [Test(Closer, Slasher) || Closer <- ?ROLES,
                              Slasher <- ?ROLES],
    ok.


slash_older_payload(Cfg) ->
    Test =
        fun(Closer, Slasher) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, 5),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_from(Slasher),
                set_prop(round, 4),
                negative(fun slash_/2, {error, old_round})])
        end,
    [Test(Closer, Slasher) || Closer <- ?ROLES,
                              Slasher <- ?ROLES],
    ok.
slash_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun slash_/2).

%%%===================================================================
%%% Deposit
%%%===================================================================

deposit(Cfg) ->
    Amount = 10,
    Fee = 1,
    Test =
        fun(Depositor, RoleKey) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Depositor),
                fun(Props) ->
                    Pubkey = maps:get(RoleKey, Props),
                    (get_onchain_balance(Pubkey, depositor))(Props)
                end,
                set_prop(amount, Amount),
                set_prop(fee, Fee),
                positive(fun deposit_/2),
                fun(#{state := S, depositor := D} = Props) ->
                    [Pubkey] = maps:keys(D),
                    BalanceBefore = maps:get(Pubkey, D),

                    BalanceAfter = get_balance(Pubkey, S),

                    BalanceAfter = BalanceBefore - Amount - Fee,
                    Props
                end])
        end,
    Test(initiator, initiator_pubkey),
    Test(responder, responder_pubkey),
    ok.

deposit_unknown_from(Cfg) ->
    {MissingAccount, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    PrivKey = aesc_test_utils:priv_key(MissingAccount, S),
    run(#{cfg => Cfg},
        [positive(fun create_channel_/2),
         set_prop(from_pubkey, MissingAccount),
         set_prop(from_privkey, PrivKey),
         set_prop(amount, 10),
         negative(fun deposit_/2, {error, account_not_found})]),
    ok.

deposit_insufficent_funds(Cfg) ->
    TotalAmount = 10,
    Test =
        fun(Depositor, TestAmount, Fee, Err) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                 set_from(Depositor),
                 fun(#{state := S0, from_pubkey := FromPubKey} = Props) ->
                    S1 = aesc_test_utils:set_account_balance(FromPubKey, TotalAmount, S0),
                    Props#{state => S1}
                 end,
                 set_prop(amount, TestAmount),
                 set_prop(fee, Fee),
                 negative(fun deposit_/2, {error, Err})])
        end,
    lists:foreach(
        fun(Depositor) ->
            Test(Depositor, 12, 1, insufficient_funds),
            Test(Depositor, 10, 1, insufficient_funds),
            Test(Depositor, 10, 0, too_low_fee)
        end,
        ?ROLES),
    ok.

deposit_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun deposit_/2, #{amount => 1, fee => 1}).

deposit_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun deposit_/2, #{amount => 1, fee => 1}).

deposit_closing(Cfg) ->
    test_both_missing_channel(Cfg, fun deposit_/2, #{amount => 1, fee => 1}).

deposit_older_round(Cfg) ->
    test_both_old_round(Cfg, fun deposit_/2, #{amount => 1, fee => 1}).

deposit_not_participant(Cfg) ->
    test_not_participant(Cfg, fun deposit_/2, #{amount => 1, fee => 1}).

deposit_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun deposit_/2, #{amount => 1, fee => 1}).

%%%===================================================================
%%% Withdraw
%%%===================================================================
withdraw(Cfg) ->
    Amount = 10,
    Fee = 1,
    Test =
        fun(Depositor, RoleKey) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Depositor),
                fun(Props) ->
                    Pubkey = maps:get(RoleKey, Props),
                    (get_onchain_balance(Pubkey, withdrawer))(Props)
                end,
                set_prop(amount, Amount),
                set_prop(fee, Fee),
                positive(fun withdraw_/2),
                fun(#{state := S, withdrawer := D} = Props) ->
                    [Pubkey] = maps:keys(D),
                    BalanceBefore = maps:get(Pubkey, D),

                    BalanceAfter = get_balance(Pubkey, S),

                    BalanceAfter = BalanceBefore + Amount - Fee,
                    Props
                end])
        end,
    Test(initiator, initiator_pubkey),
    Test(responder, responder_pubkey),
    ok.

withdraw_unknown_from(Cfg) ->
    {MissingAccount, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    PrivKey = aesc_test_utils:priv_key(MissingAccount, S),
    run(#{cfg => Cfg},
        [positive(fun create_channel_/2),
         set_prop(from_pubkey, MissingAccount),
         set_prop(from_privkey, PrivKey),
         set_prop(amount, 10),
         negative(fun withdraw_/2, {error, account_not_found})]),
    ok.

withdraw_insufficent_funds(Cfg) ->
    IAmt = 5,
    RAmt = 5,
    Test =
        fun(Withdrawer, TestAmount, Fee, Err) ->
            run(#{cfg => Cfg, initiator_amount => IAmt, responder_amount => RAmt,
                  channel_reserve => 1},
                [positive(fun create_channel_/2),
                 set_from(Withdrawer),
                 set_prop(amount, TestAmount),
                 set_prop(fee, Fee),
                 negative(fun withdraw_/2, {error, Err})])
        end,
    lists:foreach(
        fun(Withdrawer) ->
            Test(Withdrawer, 11, 1, not_enough_channel_funds),
            % keep at least 2*channel_reserve in the channel
            Test(Withdrawer, 9, 1, not_enough_channel_funds)
        end,
        ?ROLES),
    ok.

withdraw_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun withdraw_/2, #{amount => 1, fee => 1}).

withdraw_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun withdraw_/2, #{amount => 1, fee => 1}).

withdraw_closing(Cfg) ->
    test_both_missing_channel(Cfg, fun withdraw_/2, #{amount => 1, fee => 1}).

withdraw_older_round(Cfg) ->
    test_both_old_round(Cfg, fun withdraw_/2, #{amount => 1, fee => 1}).

withdraw_not_participant(Cfg) ->
    test_not_participant(Cfg, fun withdraw_/2, #{amount => 1, fee => 1}).

withdraw_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun withdraw_/2, #{amount => 1, fee => 1}).

get_balances(K1, K2, S) ->
    {get_balance(K1, S), get_balance(K2, S)}.

get_balance(K, S) ->
    Acc = aesc_test_utils:get_account(K, S),
    aec_accounts:balance(Acc).

get_channel_obj_balances(Channel) ->
    IAmt = aesc_channels:initiator_amount(Channel),
    RAmt = aesc_channels:responder_amount(Channel),
    #{initiator => IAmt,
      responder => RAmt}.

%%%===================================================================
%%% Settle
%%%===================================================================
settle(Cfg) ->
    Test =
        fun(Closer, Settler) ->
            run(#{cfg => Cfg, lock_period => 10},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                positive(fun close_solo_/2),
                set_from(Settler),
                set_prop(height, 21),
                positive(fun settle_/2)
                ])
        end,
    [Test(Closer, Setler) || Closer <- ?ROLES,
                             Setler <- ?ROLES],

    TestWithSlash =
        fun(Closer, Slasher, Settler) ->
            run(#{cfg => Cfg, lock_period => 10},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                set_prop(round, 20),
                positive(fun close_solo_/2),
                set_from(Slasher),
                set_prop(height, 15),
                set_prop(round, 42),
                positive(fun slash_/2),
                set_from(Settler),
                set_prop(height, 26),
                positive(fun settle_/2)
                ])
        end,
    [TestWithSlash(Closer, Slasher, Setler) ||  Closer <- ?ROLES,
                                                Slasher <- ?ROLES,
                                                Setler <- ?ROLES],
    ok.

settle_wrong_amounts(Cfg) ->
    IAmt = 5,
    RAmt = 5,
    CloseAmtI = IAmt + 1, % initiator had gained 1 token
    CloseAmtR = RAmt - 1, % responder had spent 1 token
    Test =
        fun(Closer, Settler, IAmt1, RAmt1, Fee, Err) ->
            run(#{cfg => Cfg, lock_period => 10,
                  initiator_amount => IAmt, responder_amount => RAmt,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                set_prop(initiator_amount, CloseAmtI),
                set_prop(responder_amount, CloseAmtR),
                positive(fun close_solo_/2),
                set_from(Settler),
                set_prop(initiator_amount, IAmt1),
                set_prop(responder_amount, RAmt1),
                set_prop(fee, Fee),
                set_prop(height, 21),
                negative(fun settle_/2, {error, Err})
                ])
        end,
    % settle tx amounts must be equal to the last on-chain tx
    ActualTest =
        fun(Closer, Setler) ->
            % someone has more
            Test(Closer, Setler, CloseAmtI + 1, CloseAmtR, 1, insufficient_channel_funds),
            Test(Closer, Setler, CloseAmtI, CloseAmtR + 1, 1, insufficient_channel_funds),
            % someone has less
            Test(Closer, Setler, CloseAmtI - 1, CloseAmtR, 1, wrong_amt),
            Test(Closer, Setler, CloseAmtI, CloseAmtR - 1, 1, wrong_amt),

            % fee
            Test(Closer, Setler, CloseAmtI, CloseAmtR, 0, too_low_fee)
        end,
    [ActualTest(Closer, Setler) ||  Closer <- ?ROLES,
                                    Setler <- ?ROLES],
    ok.

settle_missing_channel(Cfg) ->
    ChannelHashSize = aec_base58c:byte_size_for_type(channel),
    FakeChannelPubKey = <<42:ChannelHashSize/unit:8>>,
    Test =
        fun(Closer, Settler) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                positive(fun close_solo_/2),
                set_from(Settler),
                set_prop(height, 21),
                set_prop(channel_pubkey, FakeChannelPubKey),
                negative(fun settle_/2, {error, channel_does_not_exist})])
        end,
    [Test(Closer, Setler) || Closer <- ?ROLES,
                             Setler <- ?ROLES],
    ok.

settle_wrong_nonce(Cfg) ->
    Nonce = 42,
    Test =
        fun(Closer, {Settler, RoleKey}, TestNonce, Err) ->
            run(#{cfg => Cfg, lock_period => 10},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                positive(fun close_solo_/2),
                fun(#{state := S0} = Props) ->
                    Pubkey = maps:get(RoleKey, Props),
                    S = aesc_test_utils:set_account_nonce(Pubkey, Nonce, S0),
                    Props#{state => S}
                 end,
                set_prop(nonce, TestNonce),
                set_from(Settler),
                set_prop(height, 21),
                negative(fun settle_/2, {error, Err})
                ])
        end,
    RolesWithKeys = [{initiator, initiator_pubkey}, {responder, responder_pubkey}],
    % settle tx amounts must be equal to the last on-chain tx
    ActualTest =
        fun(Closer, Setler) ->
            Test(Closer, Setler, Nonce - 1,  account_nonce_too_high),
            Test(Closer, Setler, Nonce    ,  account_nonce_too_high),
            Test(Closer, Setler, Nonce + 2,  account_nonce_too_low)
        end,
    [ActualTest(Closer, Setler) ||  Closer <- ?ROLES,
                                    Setler <- RolesWithKeys],
    ok.

settle_not_closing(Cfg) ->
    Test =
        fun(Settler) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Settler),
                negative(fun settle_/2, {error, channel_not_closed})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

settle_not_yet_closable(Cfg) ->
    Test =
        fun(Closer, Settler) ->
            run(#{cfg => Cfg, lock_period => 10},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                positive(fun close_solo_/2),
                set_from(Settler),
                set_prop(height, 19),
                negative(fun settle_/2, {error, channel_not_closed})
                ])
        end,
    [Test(Closer, Setler) || Closer <- ?ROLES,
                             Setler <- ?ROLES],

    TestWithSlash =
        fun(Closer, Slasher, Settler) ->
            run(#{cfg => Cfg, lock_period => 10},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                set_prop(round, 20),
                positive(fun close_solo_/2),
                set_from(Slasher),
                set_prop(height, 15),
                set_prop(round, 42),
                positive(fun slash_/2),
                set_from(Settler),
                set_prop(height, 24),
                negative(fun settle_/2, {error, channel_not_closed})
                ])
        end,
    [TestWithSlash(Closer, Slasher, Setler) ||  Closer <- ?ROLES,
                                                Slasher <- ?ROLES,
                                                Setler <- ?ROLES],
    ok.

settle_not_participant(Cfg) ->
    Test =
        fun(Closer) ->
            run(#{cfg => Cfg, lock_period => 10},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                positive(fun close_solo_/2),
                set_prop(height, 21),
                fun(#{state := S0} = Props) ->
                    {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
                    S1 = aesc_test_utils:set_account_balance(NewAcc, 1000, S),
                    PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
                    Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
                end,
                negative(fun settle_/2, {error, account_not_peer})])
        end,
    [Test(Closer) || Closer <- ?ROLES],
    ok.

settle_delegate_not_allowed(Cfg) ->
    Test =
        fun(Closer) ->
            run(#{cfg => Cfg},
              [fun(Props) ->
                    {Delegate1, Delegate2, S} = create_loaded_accounts(100, 100),
                    Props#{cfg => [{state, S} | Cfg],
                            delegate_ids => [aec_id:create(account, Delegate1),
                                             aec_id:create(account, Delegate2)]}
                end,
                positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                positive(fun close_solo_/2),
                set_prop(height, 21),
                fun(#{delegate_ids := [D1 |_], state := S} = Props) ->
                    D1Pubkey = aec_id:specialize(D1, account),
                    D1PrivKey = aesc_test_utils:priv_key(D1Pubkey, S),
                    Props#{from_pubkey => D1Pubkey, from_privkey => D1PrivKey}
                end,
                negative(fun settle_/2, {error, account_not_peer})])
        end,
    [Test(Closer) || Closer <- ?ROLES],
    ok.

%%%===================================================================
%%% Snapshot solo
%%%===================================================================

snapshot_solo(Cfg) ->
    Round = 43,
    OldRound = Round - 5,
    StateHashSize = aec_base58c:byte_size_for_type(state),
    OldStateHash = <<40:StateHashSize/unit:8>>,
    StateHash = <<43:StateHashSize/unit:8>>,
    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Snapshoter),
                set_prop(round, Round),
                set_prop(state_hash, StateHash),
                positive(fun snapshot_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    Round = aesc_channels:round(Channel),
                    StateHash = aesc_channels:state_hash(Channel),
                    Props
                end
                ])
        end,
    [Test(Role) || Role <- ?ROLES],

    TestPreAction =
        fun(PreActor, Snapshoter, Action) when is_function(Action, 2) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(PreActor),
                set_prop(round, OldRound),
                set_prop(amount, 1),
                set_prop(fee, 1),
                set_prop(state_hash, OldStateHash),
                positive(Action),
                set_from(Snapshoter),
                set_prop(round, Round),
                set_prop(state_hash, StateHash),
                positive(fun snapshot_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    Round = aesc_channels:round(Channel),
                    StateHash = aesc_channels:state_hash(Channel),
                    Props
                end
                ])
        end,
    Actions =
        [ fun deposit_/2,
          fun withdraw_/2,
          fun snapshot_solo_/2
        ],
    [TestPreAction(PreActor, Snapshoter, Action) || PreActor <- ?ROLES,
                                                    Snapshoter <- ?ROLES,
                                                    Action <- Actions],
    ok.

% no one can post a snapshot_tx to a closed channel
snapshot_closed_channel(Cfg) ->
    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(initiator),
                set_prop(fee, 1),
                prepare_balances_for_mutual_close(),
                positive(fun close_mutual_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel is closed
                    none = aesc_test_utils:lookup_channel(ChannelPubKey, S),
                    Props
                end,
                set_from(Snapshoter),
                negative(fun snapshot_solo_/2, {error, channel_does_not_exist})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

% no one can post a snapshot_tx to a closing channel, not even the one that
% initiated the close
snapshot_closing_channel(Cfg) ->
    test_both_closing_channel(Cfg, fun snapshot_solo_/2).

% snapshot_tx calls to a missing channel are rejected
snapshot_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun snapshot_solo_/2).

% snapshot_tx calls with a payload from another channel are rejected
snapshot_payload_from_another_channel(Cfg) ->
    test_both_payload_from_different_channel(Cfg, fun snapshot_solo_/2).

% no one can overwrite a state, not even the one that posted it
snapshot_payload_not_co_signed(Cfg) ->
    test_payload_not_both_signed(Cfg,
                                 fun(ChannelPubKey, FromPubKey, Payload, _PoI, S) ->
                                     aesc_test_utils:snapshot_solo_tx_spec(ChannelPubKey,
                                                                           FromPubKey,
                                                                           Payload,
                                                                           S)
                                  end,
                                  fun aesc_snapshot_solo_tx:new/1).

% snapshot_tx calls with a payload from another channel are rejected
snapshot_old_payload(Cfg) ->
    test_both_old_round(Cfg, fun snapshot_solo_/2).

snapshot_not_participant(Cfg) ->
    test_not_participant(Cfg, fun snapshot_solo_/2).

snapshot_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun snapshot_solo_/2).

%%%===================================================================
%%% Force progress
%%%===================================================================

fp_after_create(Cfg) ->
    Round = 43,
    ContractRound = 10,
    AfterCreate =
        fun(Owner, Forcer, GasPrice, GasLimit) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(gas_price, GasPrice),
                set_prop(gas_limit, GasLimit),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                force_progress_sequence(Round, Forcer)
               ])
        end,
    Test =
        fun(GasPrice, GasLimit) ->
            [AfterCreate(Owner, Forcer, GasPrice, GasLimit)
                || Owner  <- ?ROLES,
                   Forcer <- ?ROLES]
        end,
    Test(1, 100000),
    Test(2, 100000),
    Test(3, 100000),
    ok.

fp_after_deposit(Cfg) ->
    AfterDeposit =
        fun(DepositRound, FPRound, Depositor, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            Amount = 10,
            ContractCreateRound = 10,
            {IAmt1, RAmt1} =
                case Depositor of
                    initiator -> {IAmt0 + Amount, RAmt0};
                    responder -> {IAmt0, RAmt0 + Amount}
                end,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Depositor),
                set_prop(amount, Amount),
                fun(Props) ->
                    Props#{initiator_amount => IAmt1,
                           responder_amount => RAmt1}
                end,
                create_contract_poi_and_payload(FPRound - 1, 
                                                ContractCreateRound,
                                                Owner),
                set_prop(round, DepositRound),
                positive(fun deposit_/2),
                fun(Props) when DepositRound =:= FPRound - 1 ->
                      Props#{payload => <<>>};
                   (Props) -> Props
                end,
                force_progress_sequence(_Round = FPRound, Forcer)
               ])
        end,
    Test =
        fun(DepositRound, FPRound) ->
            [AfterDeposit(DepositRound, FPRound,
                          Depositor, Owner, Forcer) || Owner  <- ?ROLES,
                                                       Depositor <- ?ROLES,
                                                       Forcer <- ?ROLES]
        end,

    %% some rounds had passed since the deposit
    Test(10, 20),
    %% force progress right after a deposit
    Test(11, 12),
    ok.

fp_after_withdrawal(Cfg) ->
    AfterWithdrawal =
        fun(WithdrawalRound, FPRound, Withdrawer, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            Amount = 10,
            ContractCreateRound = 10,
            {IAmt1, RAmt1} =
                case Withdrawer of
                    initiator -> {IAmt0 - Amount, RAmt0};
                    responder -> {IAmt0, RAmt0 - Amount}
                end,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Withdrawer),
                set_prop(amount, Amount),
                fun(Props) ->
                    Props#{initiator_amount => IAmt1,
                           responder_amount => RAmt1}
                end,
                create_contract_poi_and_payload(FPRound - 1, 
                                                ContractCreateRound,
                                                Owner),
                set_prop(round, WithdrawalRound),
                positive(fun withdraw_/2),
                fun(Props) when WithdrawalRound =:= FPRound - 1 ->
                      Props#{payload => <<>>};
                   (Props) -> Props
                end,
                force_progress_sequence(_Round = FPRound, Forcer)
               ])
        end,
    Test =
        fun(WithdrawalRound, FPRound) ->
            [AfterWithdrawal(WithdrawalRound, FPRound,
                             Withdrawer, Owner, Forcer) || Owner  <- ?ROLES,
                                                           Withdrawer <- ?ROLES,
                                                           Forcer <- ?ROLES]
        end,

    %% some rounds had passed since the withdrawal
    Test(10, 20),
    %% force progress right after a withdrawal
    Test(11, 12),
    ok.

fp_on_top_of_fp(Cfg) ->
    Contract1Round = 10,
    InitHeight = 10,
    LockPeriod = 42,
    AfterFP =
        fun(Owner, Forcer1, Forcer2, FPRound1, FPRound2) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),


                create_contract_poi_and_payload(FPRound1 - 1, Contract1Round, Owner),
                set_prop(height, InitHeight),
                force_progress_sequence(FPRound1, Forcer1),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      height := Height} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    % can not force progress yet
                    false = aesc_channels:can_force_progress(Channel, Height),
                    Props
                end,
                set_prop(height, InitHeight + LockPeriod + 1),
                fun(#{trees            := Trees0,
                      state            := S,
                      channel_pubkey   := ChannelPubKey,
                      initiator_pubkey := Initiator,
                      responder_pubkey := Responder,
                      contract_id      := ContractId,
                      fake_account     := FakeAcc,
                      solo_payload     := SoloPayload} = Props) ->
                    SignedTx = aetx_sign:deserialize_from_binary(SoloPayload),
                    Tx       = aetx_sign:tx(SignedTx),
                    {channel_offchain_tx, PayloadTx} = aetx:specialize_type(Tx),
                    [Update] = aesc_offchain_tx:updates(PayloadTx),
                    FPRound1 = aesc_offchain_tx:round(PayloadTx), %assert
                    Reserve = maps:get(channel_reserve, Props, 0),
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    PoIHash = aesc_channels:state_hash(Channel),
                    Trees = aesc_offchain_update:apply_on_trees(Update,
                                                                aect_call_state_tree:prune_without_backend(Trees0),
                                                                FPRound1,
                                                                Reserve),
                    PoIHash = aec_trees:hash(Trees),
                    PoI = calc_poi([Initiator, Responder, FakeAcc],
                                  [ContractId], Trees),
                    PoIHash = aec_trees:poi_hash(PoI),
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    Props#{state_hash => PoIHash, poi => PoI,
                          payload => <<>>,
                          trees => Trees,
                          addresses => [aec_id:create(account, Initiator),
                                        aec_id:create(account, Responder),
                                        aec_id:create(account, FakeAcc),
                                        aec_id:create(contract, ContractId)
                                        ]}
                end,
                force_progress_sequence(FPRound2, Forcer2)
               ])
        end,
    Test =
        fun(FPRound1, FPRound2) ->
            [AfterFP(Owner, Forcer1, Forcer2, FPRound1, FPRound2)
                || Owner   <- ?ROLES,
                   Forcer1 <- ?ROLES,
                   Forcer2 <- ?ROLES]
        end,
    %% force progress right after the first force progress
    Test(15, 16),
    ok.

fp_after_fp_missing_rounds(Cfg) ->
    Contract1Round = 10,
    Contract2Round = 11,
    InitHeight = 10,
    LockPeriod = 42,
    AfterFP =
        fun(Owner, Forcer1, Forcer2, FPRound1, FPRound2) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),


                create_contract_poi_and_payload(FPRound1 - 1, Contract1Round, Owner),
                set_prop(height, InitHeight),
                force_progress_sequence(FPRound1, Forcer1),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      height := Height} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    % can not force progress yet
                    false = aesc_channels:can_force_progress(Channel, Height),
                    Props
                end,
                set_prop(height, InitHeight + LockPeriod + 1),
                fun(Props) when FPRound1 =/= FPRound2 - 1 ->

                    (create_contract_poi_and_payload(FPRound2 - 1,
                                                    Contract2Round,
                                                    Owner))(Props)
                end,
                force_progress_sequence(FPRound2, Forcer2)
               ])
        end,
    Test =
        fun(FPRound1, FPRound2) ->
            [AfterFP(Owner, Forcer1, Forcer2, FPRound1, FPRound2)
                || Owner   <- ?ROLES,
                   Forcer1 <- ?ROLES,
                   Forcer2 <- ?ROLES]
        end,
    %% some rounds had passed since the first force progress
    Test(10, 20),
    ok.

fp_is_replaced_by_same_round_deposit(Cfg) ->
    BogusStateHash = <<0:32/unit:8>>,
    CoSignedDepositWins =
        fun(FPRound, Depositor, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            Amount = 10,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(FPRound - 1, 
                                                ContractCreateRound,
                                                Owner),
                force_progress_sequence(_Round = FPRound, Forcer),
                fun(#{state_hash := SH} = Props) ->
                    Props#{fp_state_hash => SH}
                end,
                set_from(Depositor),
                set_prop(amount, Amount),
                set_prop(round, FPRound), % same round, co-signed shall win
                fun(Props) ->
                    Props#{state_hash => BogusStateHash}
                end,
                positive(fun deposit_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      fp_state_hash := FPStateHash} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),

                    % different hashes
                    true = BogusStateHash =/= FPStateHash,
                    % deposit had won on-chain
                    BogusStateHash = aesc_channels:state_hash(Channel),
                    Props
                end
               ])
        end,
    Test =
        fun(Round) ->
            [CoSignedDepositWins(Round, Depositor, Owner, Forcer)
                  || Owner  <- ?ROLES,
                     Depositor <- ?ROLES,
                     Forcer <- ?ROLES]
        end,

    %% same round is used for the deposit and the forced progress before it
    %% co-signed deposit wins
    Test(20),
    ok.

fp_is_replaced_by_same_round_withdrawal(Cfg) ->
    BogusStateHash = <<0:32/unit:8>>,
    CoSignedWithdrawWins =
        fun(FPRound, Withdrawer, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            Amount = 10,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(FPRound - 1, 
                                                ContractCreateRound,
                                                Owner),
                force_progress_sequence(_Round = FPRound, Forcer),
                fun(#{state_hash := SH} = Props) ->
                    Props#{fp_state_hash => SH}
                end,
                set_from(Withdrawer),
                set_prop(amount, Amount),
                set_prop(round, FPRound), % same round, co-signed shall win
                fun(Props) ->
                    Props#{state_hash => BogusStateHash}
                end,
                positive(fun withdraw_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      fp_state_hash := FPStateHash} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),

                    % different hashes
                    true = BogusStateHash =/= FPStateHash,
                    % withdraw had won on-chain
                    BogusStateHash = aesc_channels:state_hash(Channel),
                    Props
                end
               ])
        end,
    Test =
        fun(Round) ->
            [CoSignedWithdrawWins(Round, Withdrawer, Owner, Forcer)
                  || Owner  <- ?ROLES,
                     Withdrawer <- ?ROLES,
                     Forcer <- ?ROLES]
        end,

    %% same round is used for the withdrawal and the forced progress before it
    %% co-signed withdrawal wins
    Test(20),
    ok.

fp_after_snapshot(Cfg) ->
    AfterSnapshot =
        fun(SnapshotRound, FPRound, Snapshoter, Owner, Forcer) ->
            IAmt = 30,
            RAmt = 30,
            ContractCreateRound = 5,
            run(#{cfg => Cfg, initiator_amount => IAmt,
                              responder_amount => RAmt,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(SnapshotRound, 
                                                ContractCreateRound,
                                                Owner),
                set_from(Snapshoter),
                set_prop(round, SnapshotRound),
                positive(fun snapshot_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    SnapshotRound = aesc_channels:round(Channel), % assert
                    Props
                end,
                create_contract_poi_and_payload(FPRound - 1, 
                                                ContractCreateRound,
                                                Owner),
                fun(Props) when SnapshotRound =:= FPRound - 1 ->
                      Props#{payload => <<>>};
                   (Props) -> Props
                end,
                force_progress_sequence(_Round = FPRound, Forcer)
               ])
        end,
    Test =
        fun(SnapshotRound, FPRound) ->
            [AfterSnapshot(SnapshotRound, FPRound,
                           Depositor, Owner, Forcer) || Owner  <- ?ROLES,
                                                        Depositor <- ?ROLES,
                                                        Forcer <- ?ROLES]
        end,

    %% some rounds had passed since the deposit
    Test(10, 20),
    %% force progress right after a deposit
    Test(11, 12),
    ok.

fp_is_replaced_by_same_round_snapshot(Cfg) ->
    BogusStateHash = <<0:32/unit:8>>,
    CoSignedSnapshotWins =
        fun(FPRound, Snapshoter, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(FPRound - 1, 
                                                ContractCreateRound,
                                                Owner),
                force_progress_sequence(_Round = FPRound, Forcer),
                fun(#{state_hash := SH} = Props) ->
                    Props#{fp_state_hash => SH}
                end,
                set_from(Snapshoter),
                set_prop(round, FPRound),
                set_prop(state_hash, BogusStateHash),
                delete_prop(payload), % old FP payload
                positive(fun snapshot_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      fp_state_hash := FPStateHash} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),

                    % different hashes
                    true = BogusStateHash =/= FPStateHash,
                    % withdraw had won on-chain
                    BogusStateHash = aesc_channels:state_hash(Channel),
                    Props
                end
               ])
        end,
    Test =
        fun(Round) ->
            [CoSignedSnapshotWins(Round, Withdrawer, Owner, Forcer)
                  || Owner  <- ?ROLES,
                     Withdrawer <- ?ROLES,
                     Forcer <- ?ROLES]
        end,

    %% same round is used for the snapshot and the forced progress before it
    %% co-signed snapshot wins
    Test(20),
    ok.

fp_after_solo_close(Cfg) ->
    AfterClose =
        fun(CloseRound, FPRound, Closer, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Closer),
                create_contract_poi_and_payload(CloseRound, 
                                                ContractCreateRound,
                                                Owner),
                set_prop(round, CloseRound),
                positive(fun close_solo_/2),
                create_poi_by_trees(),
                set_prop(round, FPRound - 1), % for the payload
                create_payload(),
                fun(Props) when CloseRound =:= FPRound - 1 ->
                      Props#{payload => <<>>};
                   (Props) -> Props
                end,
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had NOT been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    CloseRound = aesc_channels:round(Channel),
                    false = aesc_channels:is_active(Channel),
                    ChannelAmount = aesc_channels:channel_amount(Channel),
                    % total balance not changed
                    {ChannelAmount, _} = {IAmt0 + RAmt0, ChannelAmount},
                    Props
                end,
                force_progress_sequence(_Round = FPRound, Forcer),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    FPRound = aesc_channels:round(Channel),
                    false = aesc_channels:is_active(Channel),
                    ChannelAmount = aesc_channels:channel_amount(Channel),
                    % total balance not changed
                    {ChannelAmount, _} = {IAmt0 + RAmt0, ChannelAmount},
                    Props
                end
               ])
        end,
    Test =
        fun(CloseRound, FPRound) ->
            [AfterClose(CloseRound, FPRound,
                        Closer, Owner, Forcer) || Owner  <- ?ROLES,
                                                  Closer <- ?ROLES,
                                                  Forcer <- ?ROLES]
        end,

    %% some rounds had passed since the close 
    Test(10, 20),
    %% force progress right after a close 
    Test(11, 12),
    ok.

fp_after_slash(Cfg) ->
    AfterSlash =
        fun(CloseRound, SlashRound, FPRound, Closer, Slasher, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            LockPeriod = 10,
            CloseHeight = 10,
            SlashHeight = 12,
            CallDeposit = 10,
            true = SlashHeight < CloseHeight + LockPeriod,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                % close
                set_prop(height, CloseHeight),
                set_from(Closer),
                create_contract_poi_and_payload(CloseRound, 
                                                ContractCreateRound,
                                                Owner),
                set_prop(round, CloseRound),
                positive(fun close_solo_/2),
                % slash
                set_prop(height, SlashHeight),
                set_prop(round, SlashRound),
                set_from(Slasher),
                delete_prop(state_hash),
                create_poi_by_trees(),
                create_payload(),
                positive(fun slash_/2),
                % force progress
                create_poi_by_trees(),
                set_prop(round, FPRound - 1), % for the payload
                create_payload(),
                fun(Props) when CloseRound =:= FPRound - 1 ->
                      Props#{payload => <<>>};
                   (Props) -> Props
                end,
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    SlashRound = aesc_channels:round(Channel),
                    false = aesc_channels:is_active(Channel),
                    ChannelAmount = aesc_channels:channel_amount(Channel),
                    % total balance not changed
                    {ChannelAmount, _} = {IAmt0 + RAmt0, ChannelAmount},
                    ParticipantBalances = get_channel_obj_balances(Channel),
                    Props#{amts_before_fp => ParticipantBalances}
                end,
                set_prop(call_deposit, CallDeposit),
                force_progress_sequence(_Round = FPRound, Forcer),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      amts_before_fp := #{initiator := IAmtBefore,
                                          responder := RAmtBefore}} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    FPRound = aesc_channels:round(Channel),
                    false = aesc_channels:is_active(Channel),
                    ChannelAmount = aesc_channels:channel_amount(Channel),
                    % total balance not changed
                    {ChannelAmount, _} = {IAmt0 + RAmt0, ChannelAmount},

                    #{initiator := IAmtAfter,
                      responder := RAmtAfter} = get_channel_obj_balances(Channel),
                    % Participants' amounts in the channel's state tree are updated
                    % by the contract call. This results in update of
                    % their balances in the on-chain channel object. This
                    % means that the force progress updates closing balances.
                    % Since we're using a dummy contract for tests, balance is
                    % modified only for the forcer by the deposit he makes to
                    % the contract in the call
                    case Forcer of
                        initiator ->
                            IAmtAfter = IAmtBefore - CallDeposit,
                            RAmtAfter = RAmtBefore;
                        responder ->
                            RAmtAfter = RAmtBefore - CallDeposit,
                            IAmtAfter = IAmtBefore
                    end,
                    Props
                end
               ])
        end,
    Test =
        fun(CloseRound, SlashRound, FPRound) ->
            [AfterSlash(CloseRound, SlashRound, FPRound,
                        Closer, Slasher, Owner, Forcer) || Owner  <- ?ROLES,
                                                           Closer <- ?ROLES,
                                                           Slasher <- ?ROLES,
                                                           Forcer <- ?ROLES]
        end,

    %% some rounds had passed since the close 
    Test(10, 11, 20),
    %% force progress right after a close 
    Test(11, 20, 30),
    ok.

% no one can post a force progress to a closed channel
fp_closed_channel(Cfg) ->
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(initiator),
                set_prop(fee, 1),
                prepare_balances_for_mutual_close(),
                positive(fun close_mutual_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel is closed
                    none = aesc_test_utils:lookup_channel(ChannelPubKey, S),
                    Props
                end,
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                negative_force_progress_sequence(Round, Forcer, channel_does_not_exist)])
        end,
    [Test(Owner, Forcer) || Owner <- ?ROLES,
                            Forcer<- ?ROLES],
    ok.

fp_not_participant(Cfg) ->
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                get_onchain_balances(before_force),
                set_prop(round, Round),
                set_from(Forcer),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 1),
                fun(#{state := S0} = Props) ->
                    {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
                    S1 = aesc_test_utils:set_account_balance(NewAcc, 1000, S),
                    PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
                    Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
                end,
                negative(fun force_progress_/2, {error, account_not_peer})])
        end,
    [Test(Owner, Forcer) || Owner <- ?ROLES,
                            Forcer<- ?ROLES],
    ok.

fp_missing_channel(Cfg) ->
    ChannelHashSize = aec_base58c:byte_size_for_type(channel),
    FakeChannelId = <<42:ChannelHashSize/unit:8>>,
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(channel_pubkey, FakeChannelId),
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                negative_force_progress_sequence(Round, Forcer, channel_does_not_exist)])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES],
    ok.

fp_payload_from_another_channel(Cfg) ->
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2), % create a channelA
                % produce a payload for channelA
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                fun(#{payload := Payload} = Props) ->
                    Props#{different_payload => Payload}
                end,
                % create another channelB and replace the old one with the
                % participansts as well
                positive(fun create_channel_/2),
                create_trees(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = 6,
                                         _Contract = "identity",
                                         _InitArgs = <<"()">>,
                                         _Deposit  = 2),
                % use the payload of channelA in a force progress in channelB
                fun(#{different_payload := Payload} = Props) ->
                    Props#{payload => Payload}
                end,
                negative_force_progress_sequence(Round, Forcer,
                                                 bad_state_channel_pubkey)])
        end,
    [Test(Owner, Forcer) || Owner   <- ?ROLES,
                            Forcer  <- ?ROLES],
    ok.

fp_payload_not_co_signed(Cfg) ->
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(initiator),
                set_prop(fee, 1),
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                fun(#{payload := PayloadBin} = Props) ->
                    Payload = aetx_sign:deserialize_from_binary(PayloadBin),
                    [OneSig | _] = aetx_sign:signatures(Payload),
                    Tx = aetx_sign:tx(Payload),
                    Payload1 = aetx_sign:serialize_to_binary(
                                  aetx_sign:new(Tx, [OneSig])),
                    Props#{payload => Payload1}
                end,
                negative_force_progress_sequence(Round, Forcer,
                                                 signature_check_failed)])
        end,
    [Test(Owner, Forcer) || Owner <- ?ROLES,
                            Forcer<- ?ROLES],
    ok.

fp_payload_older_payload(Cfg) ->
    StateHashSize = aec_base58c:byte_size_for_type(state),
    BogusStateHash = <<42:StateHashSize/unit:8>>,
    Round = 10,
    Test =
        fun(Snapshotter, Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(round, Round),
                set_from(Snapshotter),
                set_prop(state_hash, BogusStateHash),
                positive(fun snapshot_solo_/2),
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                negative_force_progress_sequence(Round, Forcer,
                                                 old_round)])
        end,
    [Test(Snapshotter, Owner, Forcer) ||  Owner       <- ?ROLES,
                                          Snapshotter <- ?ROLES,
                                          Forcer      <- ?ROLES],
    ok.

fp_payload_invalid_state_hash(Cfg) ->
    StateHashSize = aec_base58c:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(Round - 1, 5, Owner,
                                                #{fake_hash => FakeStateHash}),
                negative_force_progress_sequence(Round, Forcer,
                                                 invalid_poi_hash)])
        end,
    [Test(Owner, Forcer) || Owner <- ?ROLES,
                            Forcer<- ?ROLES],
    ok.

fp_solo_payload_from_another_channel(Cfg) ->
    Round = 43,
    ContractRound = 10,
    ChannelHashSize = aec_base58c:byte_size_for_type(channel),
    FakeChannelId = <<42:ChannelHashSize/unit:8>>,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                get_onchain_balances(before_force),
                set_from(Forcer),
                set_prop(round, Round),
                fun(#{channel_pubkey := ChannelPubKey} = Props) ->
                    Props#{correct_channel_pubkey => ChannelPubKey,
                           channel_pubkey => FakeChannelId} % create wrong solo payload
                end,
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                fun(#{correct_channel_pubkey := ChannelPubKey} = Props) ->
                    Props#{channel_pubkey => ChannelPubKey}
                end,
                negative(fun force_progress_/2, {error, bad_state_channel_pubkey})])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES].

fp_solo_payload_wrong_round(Cfg) ->
    ContractRound = 10,
    BrokenRounds =
        fun(Owner, Forcer, PayloadRound, SoloPayloadRound) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),

                create_contract_poi_and_payload(PayloadRound, ContractRound, Owner),
                negative_force_progress_sequence(SoloPayloadRound, Forcer,
                                                 wrong_round)])
        end,
    Test =
        fun(R1, R2) ->
            [BrokenRounds(Owner, Forcer, R1, R2) || Owner  <- ?ROLES,
                                                    Forcer <- ?ROLES]
        end,
    %% the co-signed payload and the solo-signed have the same round
    Test(10, 10),
    %% the co-signed payload has a greater round than
    %% the solo-signed one
    Test(10, 9),
    %% the co-signed payload has a smaller round than
    %% the solo-signed one
    Test(10, 12),
    ok.

fp_solo_payload_invalid_state_hash(Cfg) ->
    StateHashSize = aec_base58c:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    SnapshotRound = 13,
    SnapshotStateHash = <<1234:StateHashSize/unit:8>>,
    Round = 43,
    ContractRound = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),

                set_from(initiator),
                set_prop(round, SnapshotRound),
                set_prop(state_hash, SnapshotStateHash),
                positive(fun snapshot_solo_/2),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                get_onchain_balances(before_force),
                set_from(Forcer),
                set_prop(round, Round),
                set_prop(fake_solo_state_hash, FakeStateHash),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 1),
                different_state_hash_produced(SnapshotRound,
                                              SnapshotStateHash)])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES].

different_state_hash_produced(OldRound, OldStateHash) ->
    fun(Props0) ->
        run(Props0,
            [ %% checks pass, contract is called on-chain, gas is consumed
              %% but progress is NOT forced
              positive(fun force_progress_/2),
              fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                  % ensure channel had NOT been updated
                  Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                  OldRound = aesc_channels:round(Channel),
                  OldStateHash = aesc_channels:state_hash(Channel),
                  false = aesc_channels:is_last_state_forced(Channel),

                  % call object and gas consumption are being present in the
                  % resulting trees are tested in the positive/1 call above
                  Props
                end])
    end.

fp_solo_payload_no_update(Cfg) ->
    fp_solo_payload_broken_updates_(Cfg, [], no_update).

fp_solo_payload_multiple_updates(Cfg) ->
    AccountHashSize = aec_base58c:byte_size_for_type(account_pubkey),
    FakeAccount1 = <<42:AccountHashSize/unit:8>>,
    FakeAccount2 = <<43:AccountHashSize/unit:8>>,

    ContractSize = aec_base58c:byte_size_for_type(contract_pubkey),
    FakeContractId = <<2:ContractSize/unit:8>>,

    Update1 = aesc_offchain_update:op_call_contract(
                aec_id:create(account, FakeAccount1),
                aec_id:create(contract, FakeContractId),
                ?VM_VERSION, 1, <<>>,
                [],
                _GasPrice = 1,
                _GasLimit = 1234567890),
    Update2 = aesc_offchain_update:op_call_contract(
                aec_id:create(account, FakeAccount2),
                aec_id:create(contract, FakeContractId),
                ?VM_VERSION, 1, <<>>,
                [],
                _GasPrice = 1,
                _GasLimit = 1234567890),
    fp_solo_payload_broken_updates_(Cfg, [Update1, Update2],
                                    more_than_one_update).

fp_solo_payload_not_call_update(Cfg) ->
    AccountHashSize = aec_base58c:byte_size_for_type(account_pubkey),
    Fake1Id = aec_id:create(account, <<42:AccountHashSize/unit:8>>),
    Fake2Id = aec_id:create(account, <<43:AccountHashSize/unit:8>>),

    Transfer = aesc_offchain_update:op_transfer(Fake1Id, Fake2Id, 10),
    Deposit = aesc_offchain_update:op_deposit(Fake1Id, 10),
    Withdraw = aesc_offchain_update:op_withdraw(Fake1Id, 10),
    NewContract = aesc_offchain_update:op_new_contract(Fake1Id, 1,
                                                       <<>>, 1, <<>>),
    lists:foreach(
        fun(Update) ->
            fp_solo_payload_broken_updates_(Cfg, [Update],
                                            update_not_call)
        end,
        [Transfer,
         Deposit,
         Withdraw,
         NewContract]),
    ok.

fp_solo_payload_broken_updates_(Cfg, UpdatesList, Error) ->
    Round = 43,
    ContractRound = 10,
    StateHashSize = aec_base58c:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),

                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                get_onchain_balances(before_force),
                set_from(Forcer),
                set_prop(round, Round),
                set_prop(solo_payload_updates, UpdatesList),
                set_prop(fake_solo_state_hash, FakeStateHash),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 1),
                negative(fun force_progress_/2, {error, Error})])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES].

fp_solo_payload_broken_call(Cfg) ->
    Round = 43,
    ContractRound = 10,
    StateHashSize = aec_base58c:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Owner, Forcer, CallData) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),

                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                get_onchain_balances(before_force),
                set_from(Forcer),
                set_prop(round, Round),
                fun(#{from_pubkey := From,
                      contract_id := ContractId} = Props) ->
                    Update = aesc_offchain_update:op_call_contract(
                                aec_id:create(account, From),
                                aec_id:create(contract, ContractId),
                                ?VM_VERSION, 1,
                                CallData,
                                [],
                                _GasPrice = 1,
                                _GasLimit = 1234567890),
                    Props#{solo_payload_updates => [Update]}
                end,
                set_prop(fake_solo_state_hash, FakeStateHash),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 1),
                positive(fun force_progress_/2),
                fun(#{state := S,
                      signed_force_progress := SignedForceProgressTx,
                      solo_payload := SoloPayload} = Props) ->
                    SignedTx = aetx_sign:deserialize_from_binary(SoloPayload),
                    Tx       = aetx_sign:tx(SignedTx),
                    {channel_offchain_tx, PayloadTx} = aetx:specialize_type(Tx),
                    [Update] = aesc_offchain_tx:updates(PayloadTx),
                    Round = aesc_offchain_tx:round(PayloadTx), %assert
                    {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
                    TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                                          aetx_sign:hash(SignedForceProgressTx)),
                    CallId = aect_call:id(Caller,
                                          Round,
                                          TxHashContractPubkey),
                    Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                                    S),
                    {_, GasPrice, GasLimit} = aesc_offchain_update:extract_amounts(Update),
                    %% assert all gas was consumed
                    GasLimit = aect_call:gas_used(Call),
                    GasPrice = aect_call:gas_price(Call),
                    <<"out_of_gas">> = aect_call:return_value(Call),
                    Props
                end])
        end,
    TestWithCallData =
        fun(CallData) ->
            [Test(Owner, Forcer, CallData) || Owner  <- ?ROLES,
                                              Forcer <- ?ROLES]
        end,
    %% empty call data
    TestWithCallData(<<>>),
    % hex encoded but still wrong
    TestWithCallData(<<"0xABCD">>),
    % not hex encoded at all
    TestWithCallData(<<42:42/unit:8>>),
    ok.

fp_missing_account_address(Cfg) ->
    Round = 43,
    ContractRound = 10,
    T =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_trees(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = ContractRound,
                                          _Contract = "identity",
                                          _InitArgs = <<"()">>,
                                          _Deposit  = 2),
                fun(Props) ->
                    Skip =
                        case Forcer of % forcer is required so we remove it
                            initiator -> initiator_pubkey;
                            responder -> responder_pubkey
                        end,
                    AddressToSkip = maps:get(Skip, Props),
                    Props#{exclude_from_poi => [AddressToSkip]}
                end,
                create_poi_by_trees(),
                set_prop(round, Round - 1),
                create_payload(),
                set_from(Forcer),
                set_prop(round, Round),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                negative_force_progress_sequence(Round, Forcer,
                                                 incomplete_poi)])
        end,
    [T(Owner, Forcer) || Owner  <- ?ROLES,
                         Forcer <- ?ROLES],
    ok.

fp_missing_contract_address(Cfg) ->
    Round = 43,
    ContractRound = 10,
    T =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_trees(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = ContractRound,
                                          _Contract = "identity",
                                          _InitArgs = <<"()">>,
                                          _Deposit  = 2),
                fun(#{contract_id := ContractId} = Props) ->
                    Props#{exclude_from_poi => [ContractId]}
                end,
                create_poi_by_trees(),
                set_prop(round, Round - 1),
                create_payload(),
                set_from(Forcer),
                set_prop(round, Round),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 1),
                negative_force_progress_sequence(Round, Forcer,
                                                incomplete_poi)])
        end,
    [T(Owner, Forcer) || Owner  <- ?ROLES,
                         Forcer <- ?ROLES],
    ok.

fp_insufficent_tokens(Cfg) ->
    Round = 43,
    ContractRound = 10,
    T =
        fun(Owner, Forcer, GasPrice, GasLimit, TotalBalance) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(Forcer),
                fun(#{state := S0, from_pubkey := From} = Props) ->
                    S = aesc_test_utils:set_account_balance(From, TotalBalance, S0),
                    Props#{state => S}
                end,
                set_prop(gas_price, GasPrice),
                set_prop(gas_limit, GasLimit),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                negative_force_progress_sequence(Round, Forcer,
                                                 insufficient_funds)])
        end,
    Test =
        fun(GasPrice, GasLimit, TotalBalance) ->
            [T(Owner, Forcer, GasPrice, GasLimit, TotalBalance)
                || Owner  <- ?ROLES,
                   Forcer <- ?ROLES]
        end,
    Test(1, 1001, 1000),
    Test(2, 500,  999),
    ok.

fp_too_soon(Cfg) ->
    TooSoonTest =
        fun(FPHeight0, FPHeight1, LockPeriod, Owner, Forcer0, Forcer1) ->
            IAmt0 = 30,
            RAmt0 = 30,
            Round0 = 100,
            Round1 = 200,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                 channel_reserve => 1, lock_period => LockPeriod},
               [positive(fun create_channel_/2),
                set_prop(height, FPHeight0),
                create_contract_poi_and_payload(Round0 - 1, 
                                                ContractCreateRound,
                                                Owner),
                force_progress_sequence(_Round = Round0, Forcer0),
                set_prop(height, FPHeight1),
                create_contract_poi_and_payload(Round1 - 1, 
                                                ContractCreateRound,
                                                Owner),
                negative_force_progress_sequence(Round1, Forcer1,
                                                 force_progressed_too_soon)])
        end,
    Test =
        fun(DepositRound, FPRound, LockPeriod) ->
            [TooSoonTest(DepositRound, FPRound, LockPeriod,
                          Depositor, Owner, Forcer) || Owner  <- ?ROLES,
                                                       Depositor <- ?ROLES,
                                                       Forcer <- ?ROLES]
        end,

    % height is too low 
    Test(11, 12, 10),
    % height is one less
    Test(10, 20, 10),

    % height is one less, different lock_period
    Test(100, 200, 100),
    ok.

create_contract_poi_and_payload(Round, ContractRound, Owner) ->
    create_contract_poi_and_payload(Round, ContractRound, Owner, #{}).

create_contract_poi_and_payload(Round, ContractRound, Owner, Opts) ->
    fun(Props0) ->
        run(Props0,
          [create_trees(),
           set_from(Owner, owner, owner_privkey),
           create_contract_in_trees(_Round    = ContractRound,
                                    _Contract = "identity",
                                    _InitArgs = <<"()">>,
                                    _Deposit  = 2),
           create_poi_by_trees(),
           set_prop(round, Round),
           fun(Props) ->
               case maps:get(fake_hash, Opts, none) of
                  none -> Props;
                  SH -> Props#{state_hash => SH}
               end
           end,
           create_payload()])
    end.

create_poi_by_trees() ->
    fun(#{initiator_pubkey := Initiator,
          responder_pubkey := Responder,
          contract_id      := ContractId,
          fake_account     := FakeAcc,
          trees            := Trees} = Props) ->
        IdsToDrop = maps:get(exclude_from_poi, Props, []),
        DropSomeIds =
            fun(List) ->
                lists:foldl(
                    fun lists:delete/2,
                    List,
                    IdsToDrop)
            end,
        Accounts = DropSomeIds([Initiator, Responder, FakeAcc]),
        Contracts = DropSomeIds([ContractId]),
        PoI = calc_poi(Accounts, Contracts, Trees),
        PoIHash = aec_trees:poi_hash(PoI),
        Props#{state_hash => PoIHash, poi => PoI,
               addresses => [aec_id:create(account, Acc) || Acc <- Accounts] ++
                            [aec_id:create(contract, C) || C <- Contracts]
              }
    end.

negative_force_progress_sequence(Round, Forcer, ErrMsg) ->
    Fee = 1,
    fun(Props0) ->
        DepositAmt = maps:get(call_deposit, Props0, 1),
        run(Props0,
           [get_onchain_balances(before_force),
            set_from(Forcer),
            set_prop(round, Round),
            fun(#{contract_id := ContractId} = Props) ->
                (create_contract_call_payload(ContractId, <<"main">>,
                                              <<"42">>, DepositAmt))(Props)
            end,
            set_prop(fee, Fee),
            negative(fun force_progress_/2, {error, ErrMsg})])
      end.

force_progress_sequence(Round, Forcer) ->
    Fee = 1,
    fun(Props0) ->
        DepositAmt = maps:get(call_deposit, Props0, 1),
        run(Props0,
           [get_onchain_balances(before_force),
            set_from(Forcer),
            set_prop(round, Round),
            fun(#{contract_id := ContractId} = Props) ->
                (create_contract_call_payload(ContractId, <<"main">>,
                                              <<"42">>, DepositAmt))(Props)
            end,
            set_prop(fee, Fee),
            positive(fun force_progress_/2),
            fun(#{channel_pubkey  := ChannelPubKey, state := S,
                  solo_payload    := SoloPayload} = Props) ->
                % ensure channel had been updated
                Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                Round = aesc_channels:round(Channel),
                {channel_offchain_tx, SoloPayloadTx} = aetx:specialize_type(
                    aetx_sign:tx(aetx_sign:deserialize_from_binary(SoloPayload))),
                ExpectedStateHash = aesc_offchain_tx:state_hash(SoloPayloadTx),
                ExpectedStateHash = aesc_channels:state_hash(Channel),
                true = aesc_channels:is_last_state_forced(Channel),
                Props
            end,
            get_onchain_balances(after_force),
            fun(#{state := S,
                  signed_force_progress := SignedForceProgressTx,
                  before_force := #{initiator := I0, responder := R0},
                  after_force  := #{initiator := I1, responder := R1},
                  solo_payload := SoloPayload} = Props) ->
                SignedTx = aetx_sign:deserialize_from_binary(SoloPayload),
                Tx       = aetx_sign:tx(SignedTx),
                {channel_offchain_tx, PayloadTx} = aetx:specialize_type(Tx),
                [Update] = aesc_offchain_tx:updates(PayloadTx),
                Round = aesc_offchain_tx:round(PayloadTx), %assert
                {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
                TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                                      aetx_sign:hash(SignedForceProgressTx)),
                CallId = aect_call:id(Caller,
                                      Round,
                                      TxHashContractPubkey),
                Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                                S),
                524 = GasUsed = aect_call:gas_used(Call),
                GasPrice = aect_call:gas_price(Call),
                ConsumedGas = GasUsed * GasPrice,
                DeductedAmt = ConsumedGas + Fee,
                
                case Forcer of
                    initiator ->
                        true = I0 - DeductedAmt =:= I1 andalso R0 =:= R1;
                    responder ->
                        true = I0 =:= I1 andalso R0 - DeductedAmt =:= R1
                end,
                Props
            end])
      end.

%%%===================================================================
%%% Test utils
%%%===================================================================

%%%
%%% wrappers
%%%

positive(Fun) ->
    fun(Props) -> Fun(Props, positive) end.

negative(Fun, ErrMsg) ->
    fun(Props) -> Fun(Props, {negative, ErrMsg}) end.

set_from(Role) ->
    set_from(Role, from_pubkey, from_privkey).

set_from(Role, PubkeyKey, PrivkeyKey) ->
    fun(Props) ->
        {KeyPub, KeyPriv} =
            case Role of
                initiator -> {initiator_pubkey, initiator_privkey};
                responder -> {responder_pubkey, responder_privkey}
            end,
        PubKey = maps:get(KeyPub, Props),
        PrivKey = maps:get(KeyPriv, Props),
        Props#{PubkeyKey => PubKey, PrivkeyKey => PrivKey}
    end.

set_prop(Key, Value) ->
    fun(Props) ->
        maps:put(Key, Value, Props)
    end.

delete_prop(Key) ->
    fun(Props) ->
        maps:remove(Key, Props)
    end.

prepare_balances_for_mutual_close() ->
    fun(#{initiator_amount := IAmt, responder_amount := RAmt} = Props) ->
        Fee = maps:get(fee, Props, 1),
        Props#{initiator_amount_final => IAmt - Fee, responder_amount_final => RAmt, fee => Fee}
    end.

get_onchain_balances(Key) ->
    fun(#{state := State, initiator_pubkey := I, responder_pubkey := R} = Props ) ->
        {BI, BR} = get_balances(I, R, State),
        maps:put(Key, #{initiator => BI, responder => BR}, Props)
    end.

get_onchain_balance(Pubkey, Key) ->
    fun(#{state := State} = Props ) ->
        B = get_balance(Pubkey, State),
        OldVals = maps:get(Key, Props, #{}),
        maps:put(Key, OldVals#{Pubkey => B}, Props)
    end.

create_payload() ->
    create_payload(payload).

create_payload(Key) ->
    fun(#{channel_pubkey    := ChannelPubKey,
          initiator_amount  := IAmt,
          responder_amount  := RAmt,
          initiator_pubkey  := IPubkey,
          responder_pubkey  := RPubkey,
          initiator_privkey := IPrivkey,
          responder_privkey := RPrivkey} = Props) ->
        PayloadSpec0 = #{initiator_amount => IAmt,
                        responder_amount  => RAmt,
                        round => maps:get(round, Props, 11)},
        PayloadSpec =
            case maps:get(state_hash, Props, none) of
                none -> PayloadSpec0;
                V -> PayloadSpec0#{state_hash => V}
            end,
        Payload = aesc_test_utils:payload(ChannelPubKey, IPubkey, RPubkey,
                                        [IPrivkey, RPrivkey], PayloadSpec),
        Props#{Key => Payload}
    end.

create_contract_call_payload(ContractId, Fun, Args, Amount) ->
    create_contract_call_payload(solo_payload, ContractId, Fun, Args, Amount).

create_contract_call_payload(Key, ContractId, Fun, Args, Amount) ->
    fun(#{channel_pubkey    := ChannelPubKey,
          from_pubkey       := From,
          from_privkey      := FromPrivkey,
          round             := Round,
          trees             := Trees0} = Props) ->
        Contract = aect_test_utils:get_contract(ContractId, #{trees => Trees0}),
        Code = aect_contracts:code(Contract),
        CallData = aect_sophia:create_call(Code, Fun, Args),
        Reserve = maps:get(channel_reserve, Props, 0),
        Update = aesc_offchain_update:op_call_contract(
                    aec_id:create(account, From),
                    aec_id:create(contract, ContractId),
                    ?VM_VERSION, Amount, CallData,
                    [],
                    _GasPrice = maps:get(gas_price, Props, 1),
                    _GasLimit = maps:get(gas_limit, Props, 1234567890)),
        {StateHash, Updates} =
            case maps:get(fake_solo_state_hash, Props, none) of
                none ->
                    Trees1 = aesc_offchain_update:apply_on_trees(Update,
                                                                aect_call_state_tree:prune_without_backend(Trees0),
                                                                Round,
                                                                Reserve),
                    StateHash1 = aec_trees:hash(Trees1),
                    {StateHash1, [Update]};
                SH ->
                    Ups = maps:get(solo_payload_updates, Props, [Update]),
                    {SH, Ups}
            end,
        {ok, UnsignedP} = aesc_offchain_tx:new(#{channel_id => aec_id:create(channel, ChannelPubKey),
                                                 updates => Updates,
                                                 state_hash => StateHash,
                                                 round => Round}),
        Payload = aetx_sign:serialize_to_binary(
                    aec_test_utils:sign_tx(UnsignedP, [FromPrivkey])),
        Props#{Key => Payload}
    end.

create_trees() ->
    AccountHashSize = aec_base58c:byte_size_for_type(account_pubkey),
    FakeAccount = <<42:AccountHashSize/unit:8>>,
    FakeAmt = 3,
    fun(#{initiator_amount  := IAmt,
          responder_amount  := RAmt,
          initiator_pubkey  := IPubkey,
          responder_pubkey  := RPubkey} = Props) ->
        Accounts = [aec_accounts:new(Pubkey, Balance) ||
                {Pubkey, Balance} <- [{IPubkey, IAmt - FakeAmt},
                                      {FakeAccount, FakeAmt}, % so there is something else besides those two accounts
                                      {RPubkey, RAmt}
                                     ]],
        Trees = aec_test_utils:create_state_tree_with_accounts(Accounts, no_backend),
        Props#{trees => Trees, fake_account => FakeAccount}
    end.

create_contract_in_trees(CreationRound, ContractName, InitArg, Deposit) ->
    fun(#{trees := Trees0,
          owner := Owner} = Props) ->
        ContractString = aeso_test_utils:read_contract(ContractName),
        BinCode = aeso_compiler:from_string(ContractString, []),
        CallData = aect_sophia:create_call(BinCode, <<"init">>, InitArg),
        Update = aesc_offchain_update:op_new_contract(aec_id:create(account, Owner),
                                                      ?VM_VERSION,
                                                      BinCode,
                                                      Deposit,
                                                      CallData),
        Reserve = maps:get(channel_reserve, Props, 0),
        Trees = aesc_offchain_update:apply_on_trees(Update, Trees0, CreationRound,
                                                    Reserve),
        ContractId = aect_contracts:compute_contract_pubkey(Owner, CreationRound),
        Props#{trees => Trees, contract_id => ContractId}
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

get_state(Cfg) ->
    case proplists:get_value(state, Cfg) of
        undefined -> aesc_test_utils:new_state();
        State0    -> State0
    end.

create_(Cfg, Spec0) ->
    create_from_state(get_state(Cfg), Spec0).

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
    ChannelPubKey = aesc_channels:pubkey(PubKey1, 1, PubKey2),
    {value, Ch} = aesc_state_tree:lookup(ChannelPubKey, aec_trees:channels(Trees2)),
    PubKey1 = aesc_channels:initiator_pubkey(Ch),
    PubKey2 = aesc_channels:responder_pubkey(Ch),
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

    {PubKey1, PubKey2, ChannelPubKey, SignedTx, S3}.

%%%
%%% create and apply transactions
%%%

create_channel_(#{cfg := Cfg} = Props, _) ->
    CreateOpts = maps:filter(
                   fun(K, _V) -> lists:member(K, [state, initiator_amount,
                                                 responder_amount,
                                                 channel_reserve,
                                                 lock_period,
                                                 delegate_ids])
                   end,
                   Props),
    {PubKey1, PubKey2, ChannelPubKey, _, S0} = create_(Cfg, CreateOpts),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S0),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S0),

    Height = 2,

    %% Get channel and account funds

    IAmt = maps:get(initiator_amount, Props, 30),
    RAmt = maps:get(responder_amount, Props, 70),
    Fee = maps:get(fee, Props, 2),

    Props#{ channel_pubkey    => ChannelPubKey,
            initiator_amount  => IAmt,
            responder_amount  => RAmt,
            initiator_pubkey  => PubKey1,
            responder_pubkey  => PubKey2,
            fee               => Fee,
            height            => Height,
            state             => S0,
            initiator_privkey => PrivKey1,
            responder_privkey => PrivKey2}.

close_solo_(#{channel_pubkey    := ChannelPubKey,
              from_pubkey       := FromPubKey,
              from_privkey      := FromPrivkey,
              initiator_amount  := IAmt,
              responder_amount  := RAmt,
              initiator_pubkey  := IPubkey,
              responder_pubkey  := RPubkey,
              state             := S,
              initiator_privkey := IPrivkey,
              responder_privkey := RPrivkey} = Props, Expected) ->

    Fee = maps:get(fee, Props, 1),
    %% Create close_solo tx and apply it on state trees
    Round = maps:get(round, Props, 10),
    PayloadSpec0 = #{initiator_amount => IAmt,
                     responder_amount => RAmt,
                     round => Round},
    PayloadSpec =
        case maps:get(state_hash, Props, not_passed) of
            not_passed -> PayloadSpec0;
            Hash -> PayloadSpec0#{state_hash => Hash}
        end,
    Payload = maps:get(payload, Props,
                       aesc_test_utils:payload(ChannelPubKey, IPubkey, RPubkey,
                                               [IPrivkey, RPrivkey], PayloadSpec)),
    PoI =  maps:get(poi, Props,
                    aesc_test_utils:proof_of_inclusion([{IPubkey, IAmt},
                                                        {RPubkey, RAmt}])),
    Spec =
        case Props of
            #{nonce := Nonce} -> #{fee => Fee, nonce => Nonce};
            _ -> #{fee => Fee}
        end,
    TxSpec = aesc_test_utils:close_solo_tx_spec(ChannelPubKey, FromPubKey, Payload,
                                                PoI, Spec, S),
    {ok, Tx} = aesc_close_solo_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

slash_(#{channel_pubkey    := ChannelPubKey,
         from_pubkey       := FromPubKey,
         from_privkey      := FromPrivkey,
         initiator_amount  := IAmt,
         responder_amount  := RAmt,
         initiator_pubkey  := IPubkey,
         responder_pubkey  := RPubkey,
         fee               := Fee,
         state             := S,
         initiator_privkey := IPrivkey,
         responder_privkey := RPrivkey} = Props, Expected) ->

    %% Create slash tx and apply it on state trees
    Round = maps:get(round, Props, 10),
    PayloadSpec0 = #{initiator_amount => IAmt,
                     responder_amount => RAmt,
                     round => Round},
    PayloadSpec =
        case maps:get(state_hash, Props, not_passed) of
            not_passed -> PayloadSpec0;
            Hash -> PayloadSpec0#{state_hash => Hash}
        end,
    Payload = maps:get(payload, Props,
                        aesc_test_utils:payload(ChannelPubKey, IPubkey, RPubkey,
                                    [IPrivkey, RPrivkey], PayloadSpec)),
    PoI = maps:get(poi, Props, aesc_test_utils:proof_of_inclusion([{IPubkey,
                                                                    IAmt},
                                                                   {RPubkey,
                                                                    RAmt}])),
    Spec =
        case Props of
            #{nonce := Nonce} -> #{fee => Fee, nonce => Nonce};
            _ -> #{fee => Fee}
        end,
    TxSpec = aesc_test_utils:slash_tx_spec(ChannelPubKey, FromPubKey, Payload,
                                            PoI, Spec, S),
    {ok, Tx} = aesc_slash_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

close_mutual_(#{channel_pubkey          := ChannelPubKey,
                initiator_amount_final  := IAmt,
                responder_amount_final  := RAmt,
                initiator_pubkey        := IPubkey,
                fee                     := Fee,
                state                   := S,
                initiator_privkey       := PrivKey1,
                responder_privkey       := PrivKey2} = Props, Expected) ->
      Spec0 = #{initiator_amount_final  => IAmt,
                initiator_account       => IPubkey,
                responder_amount_final  => RAmt,
                fee    => Fee},
      Spec =
          case Props of
              #{nonce := Nonce} -> Spec0#{nonce => Nonce};
              _ -> Spec0
          end,

      TxSpec = aesc_test_utils:close_mutual_tx_spec(ChannelPubKey, Spec, S),
      {ok, Tx} = aesc_close_mutual_tx:new(TxSpec),
      SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1, PrivKey2]),
      apply_on_trees_(Props, SignedTx, S, Expected).

snapshot_solo_(#{ channel_pubkey    := ChannelPubKey,
                  from_pubkey       := FromPubKey,
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
                       aesc_test_utils:payload(ChannelPubKey, IPubkey, RPubkey,
                                      [IPrivkey, RPrivkey], PayloadSpec)),

    SnapshotTxSpec = aesc_test_utils:snapshot_solo_tx_spec(ChannelPubKey, FromPubKey,
                           Payload, #{fee => Fee},S),
    {ok, SnapshotTx} = aesc_snapshot_solo_tx:new(SnapshotTxSpec),

    SignedTx = aec_test_utils:sign_tx(SnapshotTx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

force_progress_(#{channel_pubkey    := ChannelPubKey,
                  from_pubkey       := From,
                  from_privkey      := FromPrivkey,
                  fee               := Fee,
                  state             := S,
                  payload           := Payload,
                  solo_payload      := SoloPayload,
                  addresses         := Addresses,
                  poi               := PoI,
                  initiator_privkey := _IPrivkey,
                  responder_privkey := _RPrivkey} = Props, Expected) ->

    ForceProTxSpec = aesc_test_utils:force_progress_tx_spec(ChannelPubKey, From,
                                                            Payload, SoloPayload, PoI,
                                                            Addresses,
                                                            #{fee => Fee}, S),
    {ok, ForceProTx} = aesc_force_progress_tx:new(ForceProTxSpec),

    SignedTx = aec_test_utils:sign_tx(ForceProTx, [FromPrivkey]),
    Props1 = apply_on_trees_(Props, SignedTx, S, Expected),
    Props1#{signed_force_progress => SignedTx}.


settle_(#{channel_pubkey    := ChannelPubKey,
          from_pubkey       := FromPubKey,
          from_privkey      := FromPrivkey,
          initiator_amount  := IAmt,
          responder_amount  := RAmt,
          fee               := Fee,
          state             := S} = Props, Expected) ->
    Spec0 = #{initiator_amount => IAmt,
              responder_amount => RAmt,
              ttl => 1001,
              fee    => Fee},
    Spec =
        case Props of
            #{nonce := Nonce} -> Spec0#{nonce => Nonce};
            _ -> Spec0
        end,
    TxSpec = aesc_test_utils:settle_tx_spec(ChannelPubKey, FromPubKey, Spec, S),
    {ok, Tx} = aesc_settle_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

deposit_(#{channel_pubkey    := ChannelPubKey,
           from_pubkey       := FromPubKey,
           from_privkey      := FromPrivkey,
           fee               := Fee,
           amount            := Amount,
           state             := S} = Props, Expected) ->
    Spec =
        lists:foldl(
            fun(P, AccumSpec) ->
                case maps:get(P, Props, not_found) of
                    not_found -> AccumSpec;
                    V -> maps:put(P, V, AccumSpec)
                end
            end,
            #{amount => Amount, fee => Fee},
            [round, nonce, state_hash]),
    TxSpec = aesc_test_utils:deposit_tx_spec(ChannelPubKey, FromPubKey, Spec, S),
    {ok, Tx} = aesc_deposit_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

withdraw_(#{channel_pubkey    := ChannelPubKey,
            from_pubkey       := FromPubKey,
            from_privkey      := FromPrivkey,
            fee               := Fee,
            amount            := Amount,
            state             := S} = Props, Expected) ->
    Spec =
        lists:foldl(
            fun(P, AccumSpec) ->
                case maps:get(P, Props, not_found) of
                    not_found -> AccumSpec;
                    V -> maps:put(P, V, AccumSpec)
                end
            end,
            #{amount => Amount, fee => Fee},
            [round, nonce, state_hash]),
    TxSpec = aesc_test_utils:withdraw_tx_spec(ChannelPubKey, FromPubKey,
                                              Spec, S),
    {ok, Tx} = aesc_withdraw_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

test_both_wrong_nonce(Cfg, Fun) ->
    test_both_wrong_nonce(Cfg, Fun, #{}).

test_both_wrong_nonce(Cfg, Fun, InitProps) ->
    AccountNonce = 42,
    Test =
        fun(Poster, TestNonce, Error) ->
            run(InitProps#{cfg => Cfg},
                [positive(fun create_channel_/2),
                 set_from(Poster),
                 fun(#{state := S0, from_pubkey := FromPubKey} = Props) ->
                    S = aesc_test_utils:set_account_nonce(FromPubKey, AccountNonce, S0),
                    Props#{state => S}
                 end,
                 set_prop(nonce, TestNonce),
                 negative(Fun, {error, Error})])
        end,
    lists:foreach(
        fun(Poster) ->
            Test(Poster, AccountNonce - 1,  account_nonce_too_high),
            Test(Poster, AccountNonce,      account_nonce_too_high),
            Test(Poster, AccountNonce + 2,  account_nonce_too_low)
        end,
        ?ROLES),
    ok.

test_both_payload_from_different_channel(Cfg, Fun) ->
    Test =
        fun(Poster) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2), % create a channelA
                create_payload(), % produce a payload for channelA
                % create another channelB and replace the old one with the
                % participansts as well
                positive(fun create_channel_/2),
                set_from(Poster),
                % use the payload of channelA in a snapshot_tx for channelB
                negative(Fun, {error, bad_state_channel_pubkey})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

test_both_old_round(Cfg, Fun) ->
    test_both_old_round(Cfg, Fun, #{}).

test_both_old_round(Cfg, Fun, Props) ->
    Test0 =
        fun(First, Second, R1, R2) ->
            run(Props#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(round, R1),
                set_from(First),
                set_prop(amount, 1),
                set_prop(fee, 1),
                positive(fun deposit_/2),
                set_prop(round, R2),
                set_from(Second),
                negative(Fun, {error, old_round})])
        end,
    Test =
        fun(F, S) ->
            Test0(F, S, 42, 41),
            Test0(F, S, 42, 40)
        end,
    [Test(First, Second) || First <- ?ROLES,
                            Second <- ?ROLES],
    ok.

test_payload_not_both_signed(Cfg, SpecFun, CreateTxFun) ->
    Test =
        fun(Poster) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2), % create a channelA
                set_from(Poster),
                fun(#{channel_pubkey    := ChannelPubKey,
                      initiator_pubkey  := I,
                      responder_pubkey  := R,
                      initiator_privkey := IPriv,
                      responder_privkey := RPriv,
                      initiator_amount  := IAmt,
                      responder_amount  := RAmt,
                      from_pubkey       := FromPubKey,
                      state             := S,
                      height            := Height}) ->
                    lists:foreach(
                        fun(PrivKeys) ->
                            PayloadSpec = #{initiator_amount => IAmt,
                                            responder_amount => RAmt},
                            PayloadMissingS = aesc_test_utils:payload(ChannelPubKey, I, R,
                                                          PrivKeys, PayloadSpec),
                            PoI = aesc_test_utils:proof_of_inclusion([{I, IAmt},
                                                                      {R, RAmt}]),
                            TxSpecMissingS = SpecFun(ChannelPubKey, FromPubKey,
                                                    PayloadMissingS, PoI, S),
                            {ok, TxMissingS} = CreateTxFun(TxSpecMissingS),
                            Trees = aesc_test_utils:trees(S),
                            {error, signature_check_failed} =
                                aetx:check(TxMissingS, Trees, Height, ?PROTOCOL_VERSION)
                        end,
                        [[],       % not signed at all
                         [IPriv],  % signed only by initiator
                         [RPriv]]) % signed only by responder
                end])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

test_both_missing_channel(Cfg, Fun) ->
    test_both_missing_channel(Cfg, Fun, #{}).

test_both_missing_channel(Cfg, Fun, InitProps) ->
    ChannelHashSize = aec_base58c:byte_size_for_type(channel),
    FakeChannelPubKey = <<42:ChannelHashSize/unit:8>>,
    Test =
        fun(Poster) ->
            run(InitProps#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(channel_pubkey, FakeChannelPubKey),
                set_from(Poster),
                negative(Fun, {error, channel_does_not_exist})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

test_both_closing_channel(Cfg, Fun) ->
    Test =
        fun(Closer, Poster) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                positive(fun close_solo_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_from(Poster),
                negative(Fun, {error, channel_not_active})])
        end,
    [Test(Closer, Poster) || Closer <- ?ROLES,
                             Poster <- ?ROLES],
    ok.

test_both_invalid_poi_hash(Cfg, Fun) ->
    test_both_invalid_poi_hash(Cfg, Fun, #{}).

test_both_invalid_poi_hash(Cfg, Fun, InitProps) ->
    StateHashSize = aec_base58c:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Poster) ->
            run(InitProps#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(state_hash, FakeStateHash),
                set_from(Poster),
                negative(Fun, {error, invalid_poi_hash})])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

test_delegate_not_allowed(Cfg, Fun) ->
    test_delegate_not_allowed(Cfg, Fun, #{}).

test_delegate_not_allowed(Cfg, Fun, InitProps) ->
    run(InitProps#{cfg => Cfg},
      [fun(Props) ->
            {Delegate1, Delegate2, S} = create_loaded_accounts(100, 100),
            Props#{cfg => [{state, S} | Cfg],
                    delegate_ids => [aec_id:create(account, Delegate1),
                                     aec_id:create(account, Delegate2)]}
        end,
        positive(fun create_channel_/2),
        fun(#{delegate_ids := [D1 |_], state := S} = Props) ->
            D1Pubkey = aec_id:specialize(D1, account),
            D1PrivKey = aesc_test_utils:priv_key(D1Pubkey, S),
            Props#{from_pubkey => D1Pubkey, from_privkey => D1PrivKey}
        end,
        negative(Fun, {error, account_not_peer})]),
    ok.

test_not_participant(Cfg, Fun) ->
    test_not_participant(Cfg, Fun, #{}).

test_not_participant(Cfg, Fun, InitProps) ->
    run(InitProps#{cfg => Cfg},
        [positive(fun create_channel_/2),
         fun(#{state := S0} = Props) ->
            {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
            S1 = aesc_test_utils:set_account_balance(NewAcc, 1000, S),
            PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
            Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
         end,
         negative(Fun, {error, account_not_peer})]),
    ok.
