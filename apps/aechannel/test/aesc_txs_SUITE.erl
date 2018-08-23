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

-include_lib("common_test/include/ct.hrl").
-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MINER_PUBKEY, <<12345:?MINER_PUB_BYTES/unit:8>>).
-define(BOGUS_CHANNEL, <<0:?MINER_PUB_BYTES/unit:8>>).
-define(ROLES, [initiator, responder]).
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
       {group, snapshot_solo_negative}]
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
                calc_poi(IStartAmt, RStartAmt),
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
                calc_poi(IStartAmt + Amount, RStartAmt),
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
                calc_poi(IStartAmt - Amount, RStartAmt),
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

calc_poi(IB, RB) ->
    fun(#{initiator_pubkey := I, responder_pubkey := R} = Props) ->
        PoI = aesc_test_utils:proof_of_inclusion([{I, IB},{R, RB}]),
        Props#{poi => PoI}
    end.

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
            Test(Closer, IAmt - 1, RAmt),
            Test(Closer, IAmt, RAmt + 1),
            Test(Closer, IAmt, RAmt - 1)
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
    % sum too small
    Test(10, 10, 10, wrong_amounts),
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
                    Test(Closer, Slasher, 40, 60, 52, 49),
                    % sum is less
                    Test(Closer, Slasher, 40, 60, 10, 20),
                    Test(Closer, Slasher, 40, 60, 20, 10)
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
    fun(Props) ->
        {KeyPub, KeyPriv} =
            case Role of
                initiator -> {initiator_pubkey, initiator_privkey};
                responder -> {responder_pubkey, responder_privkey}
            end,
        PubKey = maps:get(KeyPub, Props),
        PrivKey = maps:get(KeyPriv, Props),
        Props#{from_pubkey => PubKey, from_privkey => PrivKey}
    end.

set_prop(Key, Value) ->
    fun(Props) ->
        maps:put(Key, Value, Props)
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
        PayloadSpec = #{initiator_amount => IAmt,
                        responder_amount => RAmt},
        Payload = aesc_test_utils:payload(ChannelPubKey, IPubkey, RPubkey,
                                        [IPrivkey, RPrivkey], PayloadSpec),
        Props#{Key => Payload}
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
              fee               := Fee,
              state             := S,
              initiator_privkey := IPrivkey,
              responder_privkey := RPrivkey} = Props, Expected) ->

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
    PoI = aesc_test_utils:proof_of_inclusion([{IPubkey, IAmt}, {RPubkey, RAmt}]),
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
            [nonce, state_hash]),
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
            [nonce, state_hash]),
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
