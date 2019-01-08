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
         close_solo_can_not_replace_create/1,
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
         deposit_can_not_replace_create/1,
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
         withdraw_can_not_replace_create/1,
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
         snapshot_can_not_replace_create/1,
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
         % not closing, balances are NOT checked
         fp_solo_payload_overflowing_balances/1,

         fp_chain_is_replaced_by_snapnshot/1,
         fp_chain_is_replaced_by_deposit/1,
         fp_chain_is_replaced_by_withdrawal/1,
         % already closing
         fp_after_solo_close/1,
         fp_after_slash/1,
         fp_chain_is_replaced_by_slash/1,
         % fp various on-chain actions
         fp_use_onchain_oracle/1,
         fp_use_onchain_name_resolution/1,
         fp_use_onchain_enviroment/1,
         fp_use_remote_call/1,
         fp_use_onchain_contract/1
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
         fp_can_not_replace_create/1,
         % solo signed payload tests
         fp_solo_payload_invalid_state_hash/1,
         fp_solo_payload_wrong_round/1,
         fp_solo_payload_not_call_update/1,
         fp_solo_payload_broken_call/1,
         % closing, balances are checked
         fp_solo_payload_closing_overflowing_balances/1,
         % poi tests

         fp_insufficent_tokens/1,
         fp_insufficent_gas_price/1,

         % off-chain name registration not allowed
         fp_register_name/1,

         % FP resets locked_until timer
         fp_settle_too_soon/1,

         % off-chain oracle changes are not allowed
         fp_register_oracle/1,
         fp_oracle_query/1,
         fp_oracle_extend/1,
         fp_oracle_respond/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

-define(MINER_PUBKEY, <<12345:?MINER_PUB_BYTES/unit:8>>).
-define(BOGUS_CHANNEL, <<1:?MINER_PUB_BYTES/unit:8>>).
-define(ROLES, [initiator, responder]).
-define(VM_VERSION, ?AEVM_01_Sophia_01).
-define(TEST_LOG(Format, Data), ct:log(Format, Data)).
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
       close_solo_can_not_replace_create,
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
       deposit_can_not_replace_create,
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
       withdraw_can_not_replace_create,
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
       snapshot_can_not_replace_create,
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
       % not closing, balances are NOT checked
       fp_solo_payload_overflowing_balances,
       % forced chain is replaced by co-signed state
       fp_chain_is_replaced_by_snapnshot,
       fp_chain_is_replaced_by_deposit,
       fp_chain_is_replaced_by_withdrawal,
       % already closing
       fp_after_solo_close,
       fp_after_slash,
       fp_chain_is_replaced_by_slash,
       % contract referring to on-chain objects
       fp_use_onchain_oracle,
       fp_use_onchain_name_resolution,
       fp_use_onchain_enviroment,
       fp_use_remote_call
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
       fp_can_not_replace_create,
       % solo signed payload tests
       fp_solo_payload_invalid_state_hash,
       fp_solo_payload_wrong_round,
       fp_solo_payload_not_call_update,
       fp_solo_payload_broken_call,
       % closing, balances are checked
       fp_solo_payload_closing_overflowing_balances,
       % poi tests

       fp_insufficent_tokens,
       fp_insufficent_gas_price,

       fp_use_onchain_contract,
       % FP resets locked_until timer
       fp_settle_too_soon,

       fp_register_name,
       fp_register_oracle,
       fp_oracle_query,
       fp_oracle_extend,
       fp_oracle_respond
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
    Env = aetx_env:tx_env(Height),
    BadPubKey = <<42:32/unit:8>>,

    TxSpec1 = aesc_test_utils:create_tx_spec(BadPubKey, PubKey1, S),
    {ok, Tx1} = aesc_create_tx:new(TxSpec1),
    {error, account_not_found} = aetx:process(Tx1, Trees, Env),

    TxSpec2 = aesc_test_utils:create_tx_spec(PubKey1, BadPubKey, S),
    {ok, Tx2} = aesc_create_tx:new(TxSpec2),
    {error, account_not_found} = aetx:process(Tx2, Trees, Env),

    ok.

create_insufficient_funds(_Cfg) ->
    {Loaded, NotLoaded, S} = create_loaded_accounts(100, 1),
    Trees = aesc_test_utils:trees(S),
    Height = 1,
    Env = aetx_env:tx_env(Height),

    %% Test insufficient initiator funds
    TxSpecI = aesc_test_utils:create_tx_spec(
                NotLoaded, Loaded,
                #{initiator_amount => 1,
                  fee => 50000}, S),
    {ok, TxI} = aesc_create_tx:new(TxSpecI),
    {error, insufficient_funds} = aetx:process(TxI, Trees, Env),

    %% Test insufficient responder funds
    TxSpecR = aesc_test_utils:create_tx_spec(
                Loaded, NotLoaded,
                #{initiator_amount => 1,
                  fee => 50000}, S),
    {ok, TxR} = aesc_create_tx:new(TxSpecR),
    {error, insufficient_funds} = aetx:process(TxR, Trees, Env),

    ok.


create_insufficient_funds_reserve(_Cfg) ->
    {Loaded, NotLoaded, S} = create_loaded_accounts(100000, 100000),
    Trees = aesc_test_utils:trees(S),
    Height = 1,
    Env = aetx_env:tx_env(Height),

    %% Test initiator funds lower than channel reserve
    TxSpecI = aesc_test_utils:create_tx_spec(
                NotLoaded, Loaded,
                #{initiator_amount => 1,
                  channel_reserve => 2,
                  fee => 50000}, S),
    {ok, TxI} = aesc_create_tx:new(TxSpecI),
    {error, insufficient_initiator_amount} = aetx:process(TxI, Trees, Env),

    %% Test responder funds lower than channel reserve
    TxSpecR = aesc_test_utils:create_tx_spec(
                Loaded, NotLoaded,
                #{responder_amount => 1,
                  channel_reserve => 2,
                  fee => 50000}, S),
    {ok, TxR} = aesc_create_tx:new(TxSpecR),
    {error, insufficient_responder_amount} = aetx:process(TxR, Trees, Env),

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
    {Initiator, Responder, S0} = create_loaded_accounts(100000, 100000),
    Nonce = 42,
    S = aesc_test_utils:set_account_nonce(Initiator, Nonce, S0),
    Trees = aesc_test_utils:trees(S),
    Height = 1,
    Env = aetx_env:tx_env(Height),

    Test =
        fun(TestNonce, Err) ->
            TxSpec = aesc_test_utils:create_tx_spec(Initiator, Responder,
                                                    #{nonce => TestNonce}, S),
            {ok, Tx} = aesc_create_tx:new(TxSpec),
            {error, Err} = aetx:process(Tx, Trees, Env)
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
                    case aec_trees:add_poi(Key, Pubkey, Trees,
                                                AccumPoI) of
                        {ok, P} -> P;
                        {error, Err} -> error({poi_calculation, Err, Key})
                    end
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

close_solo_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun close_solo_/2).

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
    StartIAmt = 100000,
    StartRAmt = 100000,
    ChannelAmount = StartIAmt + StartRAmt,

    Test =
        fun(IAmt, RAmt, Fee, ExpectedLockedAmt) ->
            run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
               [positive(fun create_channel_/2),
                assert_locked_amount(0), % start clean
                get_onchain_balances(before_close),
                set_prop(initiator_amount_final, IAmt),
                set_prop(responder_amount_final, RAmt),
                set_prop(fee, Fee),
                positive(fun close_mutual_/2),
                get_onchain_balances(after_close),
                assert_locked_amount(ExpectedLockedAmt),
                % this function asserts the closing amount deltas
                fun(#{before_close := #{initiator := I0, responder := R0},
                      after_close  := #{initiator := I1, responder := R1}}) ->
                    ?assertEqual(IAmt, I1 - I0), % assert initator delta
                    ?assertEqual(RAmt, R1 - R0)  % assert responder delta
                end])
        end,
    Fee = 50000,

    CorrectAmt = fun(IAmt) -> ChannelAmount - Fee - IAmt end,
    %% normal cases
    Test(45, CorrectAmt(45), Fee, 0),
    Test(15, CorrectAmt(15), Fee, 0),

    %% fee edge cases
    %% amount - HalfFee = 0
    Test(0, ChannelAmount - Fee, Fee, 0),
    Test(ChannelAmount - Fee, 0, Fee, 0),

    %% amount - HalfFee < 0
    Test(1 , CorrectAmt(1), Fee, 0),
    Test(CorrectAmt(1), 1, Fee, 0),


    %% test locked amount
    LockedAmount = 10,
    Test(45, CorrectAmt(45) - LockedAmount, Fee, LockedAmount),
    Test(15, CorrectAmt(15) - LockedAmount, Fee, LockedAmount),

    ok.

close_mutual_wrong_amounts(Cfg) ->
    StartIAmt = 100000,
    StartRAmt = 100000,

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
    Test(100000, 100000, 50000, wrong_amounts),
    % nonce too small
    Test(50, 50, 0, too_low_fee),
    ok.

close_mutual_wrong_nonce(Cfg) ->
    StartIAmt = 100000,
    StartRAmt = 100000,
    InitiatorNonce = 42,

    Test =
        fun(TestNonce, Error) ->
            run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
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
    StartIAmt = 100000,
    StartRAmt = 100000,
    ChannelHashSize = aehttp_api_encoder:byte_size_for_type(channel),
    FakeChannelPubKey = <<42:ChannelHashSize/unit:8>>,

    run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
        [positive(fun create_channel_/2),
         %% prepare balances and a fee..
         prepare_balances_for_mutual_close(),
         set_prop(channel_pubkey, FakeChannelPubKey),
         negative(fun close_mutual_/2, {error, channel_does_not_exist})]),
    ok.

close_mutual_already_closing(Cfg) ->
    StartIAmt = 100000,
    StartRAmt = 100000,

    Test =
        fun(Closer) ->
            run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
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
                    Test(Closer, Slasher, 2, 3),
                    Test(Closer, Slasher, 2, 5),
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
                    {Delegate1, Delegate2, S} = create_loaded_accounts(100000, 100000),
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
            Test(Closer, 2, 3),
            Test(Closer, 3, 5),
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
                    S1 = aesc_test_utils:set_account_balance(NewAcc, 500000, S),
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
                            Env = aetx_env:tx_env(Height),
                            {error, signature_check_failed} =
                                    aetx:process(TxMissingS, Trees, Env)
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
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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
    Fee = 50000,
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
            Test(Depositor, 12, 50000, insufficient_funds),
            Test(Depositor, 10, 50000, insufficient_funds),
            Test(Depositor, 10, 0, too_low_fee)
        end,
        ?ROLES),
    ok.

deposit_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun deposit_/2, #{amount => 1, fee => 50000}).

deposit_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun deposit_/2, #{amount => 1, fee => 50000}).

deposit_closing(Cfg) ->
    test_both_missing_channel(Cfg, fun deposit_/2, #{amount => 1, fee => 50000}).

deposit_older_round(Cfg) ->
    test_both_old_round(Cfg, fun deposit_/2, #{amount => 1, fee => 50000}).

deposit_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun deposit_/2, #{amount => 1, fee => 50000}).

deposit_not_participant(Cfg) ->
    test_not_participant(Cfg, fun deposit_/2, #{amount => 1, fee => 50000}).

deposit_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun deposit_/2, #{amount => 1, fee => 50000}).

%%%===================================================================
%%% Withdraw
%%%===================================================================
withdraw(Cfg) ->
    Amount = 10,
    Fee = 50000,
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
            Test(Withdrawer, 11, 50000, not_enough_channel_funds),
            % keep at least 2*channel_reserve in the channel
            Test(Withdrawer, 9, 50000, not_enough_channel_funds)
        end,
        ?ROLES),
    ok.

withdraw_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000}).

withdraw_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000}).

withdraw_closing(Cfg) ->
    test_both_missing_channel(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000}).

withdraw_older_round(Cfg) ->
    test_both_old_round(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000}).

withdraw_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000}).

withdraw_not_participant(Cfg) ->
    test_not_participant(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000}).

withdraw_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000}).

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
    IAmt = 20,
    RAmt = 30,
    Test =
        fun(Closer, Settler, IAmt1, RAmt1, LockedAmt) ->
            run(#{cfg => Cfg, lock_period => 10,
                  initiator_amount => IAmt, responder_amount => RAmt},
               [positive(fun create_channel_/2),
                assert_locked_amount(0), % start clean
                set_from(Closer),
                set_prop(height, 10),
                set_prop(initiator_amount, IAmt1),
                set_prop(responder_amount, RAmt1),
                positive(fun close_solo_/2),
                assert_locked_amount(0), % no tokens locked in solo close
                set_from(Settler),
                set_prop(height, 21),
                positive(fun settle_/2),
                assert_locked_amount(LockedAmt)
                ])
        end,
    TestWithSlash =
        fun(Closer, Slasher, Settler, IAmt1, RAmt1, LockedAmt) ->
            run(#{cfg => Cfg, lock_period => 10,
                  initiator_amount => IAmt, responder_amount => RAmt},
               [positive(fun create_channel_/2),
                assert_locked_amount(0), % start clean
                set_from(Closer),
                set_prop(height, 10),
                set_prop(round, 20),
                positive(fun close_solo_/2),
                assert_locked_amount(0), % no tokens locked in solo close
                set_from(Slasher),
                set_prop(height, 15),
                set_prop(round, 42),
                set_prop(initiator_amount, IAmt1),
                set_prop(responder_amount, RAmt1),
                positive(fun slash_/2),
                assert_locked_amount(0), % no tokens locked in slash
                set_from(Settler),
                set_prop(height, 26),
                positive(fun settle_/2),
                assert_locked_amount(LockedAmt)
                ])
        end,
    lists:foreach(
        fun({InitCloseAmt, RespCloseAmt, ExpectedLockedAmt}) ->
            ?TEST_LOG("Initiator close amount ~p, responder close amount ~p, expected locked amount ~p",
                      [InitCloseAmt, RespCloseAmt, ExpectedLockedAmt]),
            [Test(Closer, Setler, InitCloseAmt,
                           RespCloseAmt, ExpectedLockedAmt)
                || Closer <- ?ROLES,
                   Setler <- ?ROLES],
            [TestWithSlash(Closer, Slasher, Setler, InitCloseAmt,
                           RespCloseAmt, ExpectedLockedAmt)
                || Closer <- ?ROLES,
                   Slasher <- ?ROLES,
                   Setler <- ?ROLES]
        end,
        [{10, IAmt + RAmt - 10, 0},
         {20, IAmt + RAmt - 20, 0},
         {10, IAmt + RAmt - 11, 1}]), % 1 coin locked
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
            Test(Closer, Setler, CloseAmtI + 1, CloseAmtR, 50000, insufficient_channel_funds),
            Test(Closer, Setler, CloseAmtI, CloseAmtR + 1, 50000, insufficient_channel_funds),
            % someone has less
            Test(Closer, Setler, CloseAmtI - 1, CloseAmtR, 50000, wrong_amt),
            Test(Closer, Setler, CloseAmtI, CloseAmtR - 1, 50000, wrong_amt),

            % fee
            Test(Closer, Setler, CloseAmtI, CloseAmtR, 0, too_low_fee)
        end,
    [ActualTest(Closer, Setler) ||  Closer <- ?ROLES,
                                    Setler <- ?ROLES],
    ok.

settle_missing_channel(Cfg) ->
    ChannelHashSize = aehttp_api_encoder:byte_size_for_type(channel),
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
                    S1 = aesc_test_utils:set_account_balance(NewAcc, 100000, S),
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
                    {Delegate1, Delegate2, S} = create_loaded_accounts(100000, 100000),
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
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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
                set_prop(fee, 50000),
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
    IAmt = 100000,
    RAmt = 100000,

    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg, initiator_amount => IAmt, responder_amount => RAmt},
               [positive(fun create_channel_/2),
                set_from(initiator),
                set_prop(fee, 50000),
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

snapshot_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun snapshot_solo_/2).

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
            run(#{cfg => Cfg, initiator_amount => 10000000,
                              responder_amount => 10000000,
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
                set_prop(height, InitHeight + 1),
                create_fp_trees(),
                set_prop(payload, <<>>),
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
                set_prop(height, InitHeight + 1),
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
    BogusStateHash = <<1:32/unit:8>>,
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
    BogusStateHash = <<1:32/unit:8>>,
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

                create_fp_trees(),
                set_prop(round, FPRound - 1), % for the payload
                create_payload(),
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
    BogusStateHash = <<1:32/unit:8>>,
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
            [CoSignedSnapshotWins(Round, Snapshoter, Owner, Forcer)
                  || Owner  <- ?ROLES,
                     Snapshoter <- ?ROLES,
                     Forcer <- ?ROLES]
        end,

    %% same round is used for the snapshot and the forced progress before it
    %% co-signed snapshot wins
    Test(20),
    ok.

fp_chain_is_replaced_by_snapnshot(Cfg) ->
    fp_chain_is_replaced_by_cosigned_tx(Cfg, fun() -> positive(fun snapshot_solo_/2) end).

fp_chain_is_replaced_by_deposit(Cfg) ->
    CoSignedFun =
        fun() ->
            fun(Props) ->
                run(Props,
                    [ set_prop(amount, 1),
                      positive(fun deposit_/2)])
            end
        end,
    fp_chain_is_replaced_by_cosigned_tx(Cfg, CoSignedFun).

fp_chain_is_replaced_by_withdrawal(Cfg) ->
    CoSignedFun =
        fun() ->
            fun(Props) ->
                run(Props,
                    [ set_prop(amount, 1),
                      positive(fun withdraw_/2)])
            end
        end,
    fp_chain_is_replaced_by_cosigned_tx(Cfg, CoSignedFun).

fp_chain_is_replaced_by_cosigned_tx(Cfg, PostCoSignedFun) ->
    BogusStateHash = <<1:32/unit:8>>,
    ForceProgressFromOnChain =
        fun(Round, Forcer) ->
            fun(Props) ->
                run(Props,
                    [ create_fp_trees(),
                      set_prop(payload, <<>>),
                      force_progress_sequence(Round, Forcer)])
            end
        end,
    CoSignedTxWins =
        fun(FPRound, CoSignedPoster, Owner, Forcer) ->
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
                ForceProgressFromOnChain(FPRound + 1, Forcer),
                ForceProgressFromOnChain(FPRound + 2, Forcer),
                fun(#{state_hash := SH} = Props) ->
                    Props#{fp_state_hash => SH}
                end,
                set_from(CoSignedPoster),
                set_prop(round, FPRound), % first force progressed
                set_prop(state_hash, BogusStateHash),
                delete_prop(payload), % old FP payload
                PostCoSignedFun(),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      fp_state_hash := FPStateHash} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    % different hashes
                    true = BogusStateHash =/= FPStateHash,
                    % co-signed had won on-chain
                    BogusStateHash = aesc_channels:state_hash(Channel),
                    % back to the co-signed round
                    FPRound = aesc_channels:round(Channel),
                    Props
                end
               ])
        end,
    Test =
        fun(Round) ->
            [CoSignedTxWins(Round, CoSignedPoster, Owner, Forcer)
                  || Owner  <- ?ROLES,
                     CoSignedPoster <- ?ROLES,
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
                fun(#{initiator_pubkey  := IPubkey,
                      responder_pubkey  := RPubkey,
                      trees             := Trees} = Props) ->
                    PoI = calc_poi([IPubkey, RPubkey], [], Trees),
                    Props#{poi => PoI}
                end,
                positive(fun close_solo_/2),
                delete_prop(state_hash),
                create_fp_trees(),
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
                fun(#{initiator_pubkey  := IPubkey,
                      responder_pubkey  := RPubkey,
                      trees             := Trees} = Props) ->
                    PoI = calc_poi([IPubkey, RPubkey], [], Trees),
                    Props#{poi => PoI}
                end,
                positive(fun close_solo_/2),
                % slash
                set_prop(height, SlashHeight),
                set_prop(round, SlashRound),
                set_from(Slasher),
                delete_prop(state_hash),
                create_fp_trees(),
                fun(#{initiator_pubkey  := IPubkey,
                      responder_pubkey  := RPubkey,
                      trees             := Trees} = Props) ->
                    PoI = calc_poi([IPubkey, RPubkey], [], Trees),
                    Props#{poi => PoI}
                end,
                create_payload(),
                positive(fun slash_/2),
                % force progress
                create_fp_trees(),
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

% Test that slashing with co-signed off-chain state replaces
% on-chain produced chain of unilaterally forced progress states
%
% unilaterally on-chain force progress state ROUND
% unilaterally on-chain force progress state ROUND + 1
% solo close using on-chain forced progress ROUND + 1
% force progress on-chain state ROUND + 2
% slash with co-signed off-chain state ROUND
fp_chain_is_replaced_by_slash(Cfg) ->
    ForceProgressFromOnChain =
        fun(Round, Forcer) ->
            fun(Props) ->
                run(Props,
                    [ create_fp_trees(),
                      set_prop(payload, <<>>),
                      force_progress_sequence(Round, Forcer)])
            end
        end,
    Test =
        fun(FPRound, Closer, Slasher, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            LockPeriod = 10,
            CloseHeight = 10,
            SlashHeight = 12,
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
                create_contract_poi_and_payload(FPRound + 1,
                                                ContractCreateRound,
                                                Owner),
                create_fp_trees(),
                set_prop(round, FPRound - 1), % for the payload
                create_payload(),
                force_progress_sequence(_Round = FPRound, Forcer),
                ForceProgressFromOnChain(FPRound + 1, Forcer),
                set_prop(round, FPRound + 1),
                set_prop(payload, <<>>),
                poi_participants_only(),
                positive(fun close_solo_/2),
                ForceProgressFromOnChain(FPRound + 2, Forcer),
                fun(#{state_hash := SH} = Props) ->
                    Props#{fp_state_hash => SH}
                end,
                % slash
                % produce some different state trees
                set_balances_in_trees(20, 10),
                set_prop(height, SlashHeight),
                set_prop(round, FPRound),
                set_from(Slasher),
                poi_participants_only(),
                create_payload(),
                fun(#{state_hash := SlashStateHash} = Props) ->
                    Props#{slash_state_hash => SlashStateHash}
                end,
                positive(fun slash_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S,
                      fp_state_hash := FPStateHash,
                      slash_state_hash := SlashStateHash} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),

                    % different hashes
                    true = SlashStateHash =/= FPStateHash,
                    % slash had won on-chain
                    SlashStateHash = aesc_channels:state_hash(Channel),
                    FPRound = aesc_channels:round(Channel),
                    Props
                end
               ])
        end,
    FPRound = 50,
    [Test(FPRound,
          Closer, Slasher, Owner, Forcer) || Owner   <- ?ROLES,
                                             Closer  <- ?ROLES,
                                             Slasher <- ?ROLES,
                                             Forcer  <- ?ROLES],
    ok.


fp_use_onchain_oracle(Cfg) ->
    FPRound = 20,
    LockPeriod = 10,
    FPHeight0 = 20,
    Question = <<"To be, or not to be?">>,
    OQuestion = aeso_heap:to_binary(Question, 0),
    Answer = <<"oh, yes">>,
    OResponse = aeso_heap:to_binary(Answer, 0),
    QueryFee = 50000,
    CallOnChain =
        fun(Owner, Forcer) ->
            IAmt0 = 10000000,
            RAmt0 = 10000000,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(height, FPHeight0),

                % create oracle
                register_new_oracle(aeso_heap:to_binary(string, 0),
                                    aeso_heap:to_binary(string, 0),
                                    QueryFee),

                % create off-chain contract
                create_trees_if_not_present(),
                set_from(Owner, owner, owner_privkey),
                fun(#{oracle := Oracle} = Props) ->
                    EncodedOracleId = aeu_hex:hexstring_encode(Oracle),
                    (create_contract_in_trees(_Round    = ContractCreateRound,
                                             _Contract = "channel_on_chain_contract_oracle",
                                             _InitArgs = <<"(",EncodedOracleId/binary, ", \"", Question/binary, "\")">>,
                                             _Deposit  = 2))(Props)
                end,

                % place some calls to that contract
                force_call_contract_first(Forcer, <<"place_bet">>, <<"\"Lorem\"">>,
                                          FPRound),
                force_call_contract(Forcer, <<"place_bet">>, <<"\"Ipsum\"">>),

                % try resolving a contract with wrong query id
                fun(Props) ->
                    EncodedQueryId = aeu_hex:hexstring_encode(<<1234:42/unit:8>>),
                    (force_call_contract(Forcer, <<"resolve">>,
                                         <<"(", EncodedQueryId/binary,
                                           ")">>))(Props)
                end,
                assert_last_channel_result(<<"no response">>, string),

                % oracle query and answer
                oracle_query(OQuestion, _ResponseTTL = 100),
                oracle_response(OResponse, _ResponseTTL = 100),
                fun(#{state := S, oracle := Oracle, query_id := QueryId} = Props) ->
                    OTrees = aec_trees:oracles(aesc_test_utils:trees(S)),
                    Q = aeo_state_tree:get_query(Oracle, QueryId, OTrees),
                    OResponse = aeo_query:response(Q),
                    Props
                end,

                % there is currently no bet for the correct answer, try anyway
                fun(#{query_id := QueryId} = Props) ->
                    EncodedQueryId = aeu_hex:hexstring_encode(QueryId),
                    (force_call_contract(Forcer, <<"resolve">>,
                                         <<"(", EncodedQueryId/binary,
                                           ")">>))(Props)
                end,
                assert_last_channel_result(<<"no winning bet">>, string),

                % place a winnning bet
                force_call_contract(Forcer, <<"place_bet">>, <<"\"", Answer/binary,"\"">>),
                fun(#{query_id := QueryId} = Props) ->
                    EncodedQueryId = aeu_hex:hexstring_encode(QueryId),
                    (force_call_contract(Forcer, <<"resolve">>,
                                         <<"(", EncodedQueryId/binary,
                                           ")">>))(Props)
                end,
                assert_last_channel_result(<<"ok">>, string),

                % verify that Oracle.get_question works
                fun(#{query_id := QueryId} = Props) ->
                    EncodedQueryId = aeu_hex:hexstring_encode(QueryId),
                    (force_call_contract(Forcer, <<"get_question">>,
                                         <<"(", EncodedQueryId/binary,
                                           ")">>))(Props)
                end,
                assert_last_channel_result(Question, string),

                % verify that Oracle.query_fee works
                fun(#{query_id := QueryId} = Props) ->
                    (force_call_contract(Forcer, <<"query_fee">>,
                                         <<"()">>))(Props)
                end,
                assert_last_channel_result(QueryFee, word)
               ])
        end,
    [CallOnChain(Owner, Forcer) || Owner  <- ?ROLES,
                                   Forcer <- ?ROLES],
    ok.

fp_use_onchain_name_resolution(Cfg) ->
    FPRound = 20,
    LockPeriod = 10,
    FPHeight0 = 20,
    Name = <<"lorem.test">>,
    ForceCallCheckName =
        fun(Forcer, K, Found) when is_binary(K) andalso is_boolean(Found) ->
            fun(Props) ->
              run(Props,
                  [force_call_contract(Forcer, <<"can_resolve">>,
                                    <<"(\"", Name/binary, "\",\"", K/binary, "\")">>),
                  assert_last_channel_result(Found, bool)])
            end
        end,

    CallOnChain =
        fun(Owner, Forcer) ->
            IAmt0 = 5000000,
            RAmt0 = 5000000,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(height, FPHeight0),

                % create off-chain contract
                create_trees_if_not_present(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = ContractCreateRound,
                                         _Contract = "channel_on_chain_contract_name_resolution",
                                         _InitArgs = <<"()">>,
                                         _Deposit  = 2),
                force_call_contract_first(Forcer, <<"can_resolve">>,
                                          <<"(\"", Name/binary, "\",\"oracle\")">>,
                                          FPRound),
                assert_last_channel_result(false, bool),
                register_name(Name,
                              [{<<"account_pubkey">>, aec_id:create(account, <<1:256>>)},
                               {<<"oracle">>, aec_id:create(oracle, <<2:256>>)},
                               {<<"unexpected_key">>, aec_id:create(account, <<3:256>>)}]),
                ForceCallCheckName(Forcer, <<"oracle">>, true),
                ForceCallCheckName(Forcer, <<"unexpected_key">>, true),
                ForceCallCheckName(Forcer, <<"no_such_pointer">>, false)
               ])
        end,
    [CallOnChain(Owner, Forcer) || Owner  <- ?ROLES,
                                   Forcer <- ?ROLES],
    ok.

fp_use_onchain_enviroment(Cfg) ->
    FPRound = 20,
    LockPeriod = 10,
    FPHeight0 = 20,
    ForceCall =
        fun(Forcer, Fun, RType, R) ->
            fun(Props) ->
              run(Props,
                  [force_call_contract(Forcer, Fun, <<"()">>),
                  assert_last_channel_result(R, RType)])
            end
        end,

    Height1 = 12345,
    Timestamp1 = 654321,
    BeneficiaryInt = 42,
    Beneficiary = <<BeneficiaryInt:?BENEFICIARY_PUB_BYTES/unit:8>>,

    Height2 = Height1 + LockPeriod + 1,
    Height3 = Height2 + LockPeriod + 1,
    Height4 = Height3 + LockPeriod + 1,
    CallOnChain =
        fun(Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(height, FPHeight0),

                % create off-chain contract
                create_trees_if_not_present(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = ContractCreateRound,
                                         _Contract = "channel_env",
                                         _InitArgs = <<"()">>,
                                         _Deposit  = 2),
                force_call_contract_first(Forcer, <<"block_height">>,
                                          <<"()">>,
                                          FPRound),
                fun(#{height := H} = Props) ->
                    (assert_last_channel_result(H, word))(Props)
                end,
                set_tx_env(Height1, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"coinbase">>, word, BeneficiaryInt),
                set_tx_env(Height2, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"block_height">>, word, Height2),
                set_tx_env(Height3, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"coinbase">>, word, BeneficiaryInt),
                set_tx_env(Height4, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"timestamp">>, word, Timestamp1)
               ])
        end,
    [CallOnChain(Owner, Forcer) || Owner  <- ?ROLES,
                                   Forcer <- ?ROLES],
    ok.

fp_use_remote_call(Cfg) ->
    FPRound = 20,
    LockPeriod = 10,
    FPHeight0 = 20,
    RemoteCall =
        fun(Forcer, Int) when is_integer(Int) ->
            fun(Props) ->
                Bin = integer_to_binary(Int),
                RemoteContract = maps:get(remote_contract, Props),
                Address = aeu_hex:hexstring_encode(RemoteContract),
                Args = <<"(", Address/binary, ", ", Bin/binary, ")">>,
                (force_call_contract(Forcer, <<"call">>, Args))(Props)
            end
        end,
    PushContractId =
        fun(Key) ->
            rename_prop(contract_id, Key, keep_old)
        end,
    PopContractId =
        fun(Key) ->
            rename_prop(Key, contract_id, keep_old)
        end,

    CallOnChain =
        fun(Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(height, FPHeight0),

                % create off-chain contract that is going to be used in the
                % remote call later
                create_trees_if_not_present(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = ContractCreateRound,
                                         _Contract = "identity",
                                         _InitArgs = <<"()">>,
                                         _Deposit  = 2),
                PushContractId(remote_contract),
                fun(#{contract_id := RemoteContract} = Props) ->
                    Props#{remote_contract => RemoteContract}
                end,
                % create the second contract
                create_contract_in_trees(_Round1    = ContractCreateRound + 10,
                                         _Contract2 = "remote_call",
                                         _InitArgs2 = <<"()">>,
                                         _Deposit2  = 2),
                PushContractId(second_contract),
                PopContractId(remote_contract),
                force_call_contract_first(Forcer, <<"main">>, <<"(42)">>,
                                          FPRound),
                assert_last_channel_result(42, word),% it works

                PopContractId(second_contract),
                %% contract has a hardcoded expectation for `value = 10` for
                %% the remote call
                %% this means that the contract must have at least 10 tokens
                %% in the contract's balance. This is guaranteed via the
                %% following line (granting 20 tokens to the second_contract)
                set_prop(call_deposit, 20),
                RemoteCall(Forcer, 44),
                assert_last_channel_result(44, word)% it works remote
               ])
        end,
    [CallOnChain(Owner, Forcer) || Owner  <- ?ROLES,
                                   Forcer <- ?ROLES],
    ok.


fp_use_onchain_contract(Cfg) ->
    FPRound = 20,
    LockPeriod = 10,
    FPHeight0 = 20,
    RemoteCall =
        fun(Forcer, ContractHandle) ->
            fun(Props) ->
                RemoteContract = maps:get(ContractHandle, Props),
                Address = aeu_hex:hexstring_encode(RemoteContract),
                Args = <<"(", Address/binary, ")">>,
                run(Props,
                    [ force_call_contract(Forcer, <<"increment">>, Args),
                      force_call_contract(Forcer, <<"get">>, Args)
                    ])
            end
        end,
    PushContractId =
        fun(Key) ->
            rename_prop(contract_id, Key, keep_old)
        end,
    PopContractId =
        fun(Key) ->
            rename_prop(Key, contract_id, keep_old)
        end,

    CallOnChain =
        fun(Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(height, FPHeight0),

                % create off-chain contract that is going to be used in the
                % remote call later
                create_trees_if_not_present(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = ContractCreateRound,
                                         _Contract = "counter",
                                         _InitArgs = <<"(42)">>,
                                         _Deposit  = 2),
                PushContractId(remote_contract),
                fun(#{contract_id := RemoteContract} = Props) ->
                    Props#{remote_contract => RemoteContract}
                end,
                % create the second contract
                create_contract_in_trees(_Round1    = ContractCreateRound + 10,
                                         _Contract2 = "remote_call",
                                         _InitArgs2 = <<"()">>,
                                         _Deposit2  = 2),
                PushContractId(second_contract),
                create_contract_in_onchain_trees(_OnchainContract = "counter",
                                                 _OnchainCInitArgs = <<"(42)">>,
                                                 _OnchainDeposit  = 2),
                PushContractId(onchain_contract),
                PopContractId(remote_contract),
                force_call_contract_first(Forcer, <<"tick">>, <<"()">>,
                                          FPRound),
                force_call_contract(Forcer, <<"get">>, <<"()">>),
                assert_last_channel_result(43, word),% it works

                PopContractId(second_contract),
                set_prop(call_deposit, 2),
                RemoteCall(Forcer, remote_contract),
                assert_last_channel_result(44, word), % it works remote
                fun(Props) ->
                    RemoteContract = maps:get(onchain_contract, Props),
                    Address = aeu_hex:hexstring_encode(RemoteContract),
                    Args = <<"(", Address/binary, ")">>,
                    run(Props#{check_not_all_gas_used => false},
                        %% force progress succededs but all gas is consumed
                        %% because on-chain contract is not reachable
                        [ force_call_contract(Forcer, <<"increment">>, Args)])
                end,
                fun(#{state := S,
                      signed_force_progress := SignedForceProgressTx,
                      solo_payload := #{update := Update,
                                        round  := Round}} = Props) ->
                    {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
                    TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                                          aetx_sign:hash(SignedForceProgressTx)),
                    CallId = aect_call:id(Caller,
                                          Round,
                                          TxHashContractPubkey),
                    Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                                    S),
                    GasUsed = aect_call:gas_used(Call),
                    GasLimit = maps:get(gas_limit, Props, 10000000),
                    ?assertEqual(GasUsed, GasLimit), % assert all gas
                    Props
                end])
        end,
    [CallOnChain(Owner, Forcer) || Owner  <- ?ROLES,
                                   Forcer <- ?ROLES],
    ok.


% no one can post a force progress to a closed channel
fp_closed_channel(Cfg) ->
    Round = 10,
    IStartAmt = 200000,
    RStartAmt = 200000,

    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg, initiator_amount => IStartAmt, responder_amount => RStartAmt},
               [positive(fun create_channel_/2),
                set_from(initiator),
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
                set_prop(fee, 100000),
                fun(#{state := S0} = Props) ->
                    {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
                    S1 = aesc_test_utils:set_account_balance(NewAcc, 1000000, S),
                    PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
                    Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
                end,
                negative(fun force_progress_/2, {error, account_not_peer})])
        end,
    [Test(Owner, Forcer) || Owner <- ?ROLES,
                            Forcer<- ?ROLES],
    ok.

fp_missing_channel(Cfg) ->
    ChannelHashSize = aehttp_api_encoder:byte_size_for_type(channel),
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
                delete_prop(trees),
                create_trees_if_not_present(),
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
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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

fp_can_not_replace_create(Cfg) ->
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(_Round = 1, 5, Owner),
                negative_force_progress_sequence(Round, Forcer,
                                                 old_round)])
        end,
    [Test(Owner, Forcer) ||  Owner       <- ?ROLES,
                             Forcer      <- ?ROLES],
    ok.

fp_payload_invalid_state_hash(Cfg) ->
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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
                set_prop(fee, 100000),
                different_state_hash_produced(SnapshotRound,
                                              SnapshotStateHash)])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES].

fp_solo_payload_closing_overflowing_balances(Cfg) ->
    CloseRound = 13,
    Round = 43,
    ContractRound = 10,
    CreateDeposit = 2,
    CallDeposit = 1,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(initiator),
                set_prop(round, CloseRound),
                positive(fun close_solo_/2),
                create_trees_if_not_present(),
                set_prop(call_deposit, CallDeposit),
                fun(#{channel_pubkey := ChannelPubKey,
                      initiator_pubkey := Initiator,
                      responder_pubkey := Responder,
                      trees := Trees0,
                      state := S} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    ChannelAmount = aesc_channels:channel_amount(Channel),
                    Accounts0 = aec_trees:accounts(Trees0),
                    IAcc0 = aec_accounts_trees:get(Initiator, Accounts0),
                    RAcc0 = aec_accounts_trees:get(Responder, Accounts0),
                    ToAdd = (ChannelAmount + 1) %over the channel limit
                            + CallDeposit
                            + CreateDeposit % contract created
                            - aec_accounts:balance(IAcc0)
                            - aec_accounts:balance(RAcc0),
                    {ok, IAcc} = aec_accounts:earn(IAcc0, ToAdd),
                    ?TEST_LOG("Total channel tokens: ~p\nInitator tokens: ~p,\nResponder tokens: ~p, Adding ~p tokens to initator",
                              [ChannelAmount, aec_accounts:balance(IAcc0),
                                aec_accounts:balance(RAcc0), ToAdd]),
                    Accounts = aec_accounts_trees:enter(IAcc, Accounts0),
                    Trees = aec_trees:set_accounts(Trees0, Accounts),
                    Props#{trees => Trees}
                end,
                set_prop(contract_create_deposit, CreateDeposit),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                set_prop(round, Round),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 100000),
                fun(#{channel_pubkey := ChannelPubKey,
                      state := S} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    OnChainRound = aesc_channels:round(Channel),
                    OnChainHash = aesc_channels:state_hash(Channel),
                    (different_state_hash_produced(OnChainRound,
                                                   OnChainHash))(Props)
                 end])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES].

fp_solo_payload_overflowing_balances(Cfg) ->
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
    SnapshotRound = 13,
    SnapshotStateHash = <<1234:StateHashSize/unit:8>>,
    Round = 43,
    ContractRound = 10,
    CreateDeposit = 2,
    CallDeposit = 1,
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
                create_trees_if_not_present(),
                set_prop(call_deposit, CallDeposit),
                fun(#{channel_pubkey := ChannelPubKey,
                      initiator_pubkey := Initiator,
                      responder_pubkey := Responder,
                      trees := Trees0,
                      state := S} = Props) ->
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    ChannelAmount = aesc_channels:channel_amount(Channel),
                    Accounts0 = aec_trees:accounts(Trees0),
                    IAcc0 = aec_accounts_trees:get(Initiator, Accounts0),
                    RAcc0 = aec_accounts_trees:get(Responder, Accounts0),
                    ToAdd = (ChannelAmount + 1) %over the channel limit
                            + CallDeposit
                            + CreateDeposit % contract created
                            - aec_accounts:balance(IAcc0)
                            - aec_accounts:balance(RAcc0),
                    {ok, IAcc} = aec_accounts:earn(IAcc0, ToAdd),
                    ?TEST_LOG("Total channel tokens: ~p\nInitator tokens: ~p,\nResponder tokens: ~p, Adding ~p tokens to initator",
                              [ChannelAmount, aec_accounts:balance(IAcc0),
                                aec_accounts:balance(RAcc0), ToAdd]),
                    Accounts = aec_accounts_trees:enter(IAcc, Accounts0),
                    Trees = aec_trees:set_accounts(Trees0, Accounts),
                    Props#{trees => Trees}
                end,
                set_prop(contract_create_deposit, CreateDeposit),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                force_progress_sequence(_Round = Round, Forcer)])
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

fp_solo_payload_not_call_update(Cfg) ->
    AccountHashSize = aehttp_api_encoder:byte_size_for_type(account_pubkey),
    Fake1Id = aec_id:create(account, <<42:AccountHashSize/unit:8>>),
    Fake2Id = aec_id:create(account, <<43:AccountHashSize/unit:8>>),

    Transfer = aesc_offchain_update:op_transfer(Fake1Id, Fake2Id, 10),
    Deposit = aesc_offchain_update:op_deposit(Fake1Id, 10),
    Withdraw = aesc_offchain_update:op_withdraw(Fake1Id, 10),
    NewContract = aesc_offchain_update:op_new_contract(Fake1Id, 1,
                                                       <<>>, 1, <<>>),
    lists:foreach(
        fun(Update) ->
            fp_solo_payload_broken_update_(Cfg, Update,
                                           update_not_call)
        end,
        [Transfer,
         Deposit,
         Withdraw,
         NewContract]),
    ok.

fp_solo_payload_broken_update_(Cfg, Update, Error) ->
    Round = 43,
    ContractRound = 10,
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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
                set_prop(solo_payload_update, Update),
                set_prop(fake_solo_state_hash, FakeStateHash),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 100000),
                negative(fun force_progress_/2, {error, Error})])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES].

fp_solo_payload_broken_call(Cfg) ->
    Round = 43,
    ContractRound = 10,
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Owner, Forcer, CallData, Error) ->
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
                                _GasLimit = 10000000),
                    Props#{solo_payload_update => Update}
                end,
                set_prop(fake_solo_state_hash, FakeStateHash),
                fun(#{contract_id := ContractId} = Props) ->
                    (create_contract_call_payload(ContractId, <<"main">>,
                                                  <<"42">>, 1))(Props)
                end,
                set_prop(fee, 100000),
                positive(fun force_progress_/2),
                fun(#{state := S,
                      signed_force_progress := SignedForceProgressTx,
                      solo_payload := #{update := Update,
                                        round  := Round1}} = Props) ->
                    Round1 = Round, %% assert
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
                    ?assertEqual(Error, aect_call:return_value(Call)),
                    Props
                end])
        end,
    TestWithCallData =
        fun(CallData, ErrorMsg) ->
            [Test(Owner, Forcer, CallData, ErrorMsg) || Owner  <- ?ROLES,
                                                        Forcer <- ?ROLES]
        end,
    %% empty call data
    TestWithCallData(<<>>, <<"bad_call_data">>),
    %% Too small call data
    TestWithCallData(<<"0xABCD">>, <<"bad_call_data">>),
    %% Just plain wrong call data, but that can be interpreted
    TestWithCallData(<<42:42/unit:8>>, <<"unknown_function">>),
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

fp_insufficent_gas_price(Cfg) ->
    Round = 43,
    ContractRound = 10,
    T =
        fun(Owner, Forcer, GasPrice, GasLimit) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(gas_price, GasPrice),
                set_prop(gas_limit, GasLimit),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                negative_force_progress_sequence(Round, Forcer,
                                                 too_low_gas_price)])
        end,
    Test =
        fun(GasPrice, GasLimit) ->
            [T(Owner, Forcer, GasPrice, GasLimit)
                || Owner  <- ?ROLES,
                   Forcer <- ?ROLES]
        end,
    TooLowGasPrice = 0 = aec_governance:minimum_gas_price() - 1,
    Test(TooLowGasPrice, 1001),
    ok.

fp_register_name(Cfg) ->
    Name = <<"bla.test">>,
    Salt = 42,
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash           = aeu_hex:hexstring_encode(
                        aens_hash:commitment_hash(NameAscii, Salt)),
    ?TEST_LOG("Commitment hash ~p", [aens_hash:commitment_hash(NameAscii,
                                                               Salt)]),
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
    StateHash = <<42:StateHashSize/unit:8>>,
    Round = 42,
    FPRound = Round + 10,
    SignContractAddress =
        fun(PubK, PrivK, ConId) ->
            BinToSign = <<PubK/binary, ConId/binary>>,
            SigBin = <<Word1:256, Word2:256>> =
                enacl:sign_detached(aec_governance:add_network_id(BinToSign), PrivK),
            %_Sig = aeu_hex:hexstring_encode(aeso_heap:to_binary({Word1, Word2}, 0))
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            Word11 = integer_to_binary(Word1),
            Word21 = integer_to_binary(Word2),
            <<"(", Word11/binary, ", ", Word21/binary, ")">>
        end,
    ContractName = "aens",

    % test contract on-chain
    % this validates that the contract and the fucntion are indeed callable
    % on-chain and they produce a name preclaim
    ?TEST_LOG("Create contract ~p.aes on-chain", [ContractName]),
    run(#{cfg => Cfg},
        [ % create account for being contract owner
          fun(#{} = Props) ->
              S0 = aesc_test_utils:new_state(),
              {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
              S1 = aesc_test_utils:set_account_balance(NewAcc, 10000000, S),
              PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
              ?TEST_LOG("Owner: pubkey ~p, privkey: ~p", [NewAcc, PrivKey]),
              Props#{state => S1, onchain_contract_owner_pubkey => NewAcc,
                                  onchain_contract_owner_privkey => PrivKey}
          end,
          % create contract on-chain
          fun(#{onchain_contract_owner_pubkey := PubKey,
                state := S0} = Props) ->
            {ok, BinCode} =  compile_contract(ContractName),
            {ok, CallData} = aect_sophia:encode_call_data(BinCode, <<"init">>, <<"()">>),
            Nonce = 1,
            {ok, ContractCreateTx} =
                aect_create_tx:new(
                    #{owner_id    => aec_id:create(account, PubKey),
                      nonce       => Nonce,
                      code        => BinCode,
                      vm_version  => ?VM_VERSION,
                      deposit     => 1,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => 1,
                      call_data   => CallData,
                      fee         => 1000000}),
            ?TEST_LOG("Contract create tx ~p", [ContractCreateTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 3}),
            {ok, _} = aetx:process(ContractCreateTx, OnChainTrees,
                                  TxEnv),
            {ok, OnChainTrees1} = aetx:process(ContractCreateTx,
                                                OnChainTrees,
                                                TxEnv),
            S1 = aesc_test_utils:set_trees(OnChainTrees1, S0),
            ContractId = aect_contracts:compute_contract_pubkey(PubKey, Nonce),
            ?TEST_LOG("Contract created on-chain, id ~p", [ContractId]),
            Props#{state => S1,
                    onchain_contract_id => ContractId,
                    code => BinCode}
          end,
          % call contract on-chain
          fun(#{onchain_contract_owner_pubkey := OPubKey,
                onchain_contract_owner_privkey := OPrivKey,
                onchain_contract_id := ContractId,
                code := Code,
                state := S0} = Props) ->
            Nonce = 2,
            Sig = SignContractAddress(OPubKey, OPrivKey, ContractId),
            NameOwner = aeu_hex:hexstring_encode(OPubKey),
            PreclaimArgs = <<"(", NameOwner/binary, ",",
                                  CHash/binary, ",",
                                  Sig/binary,
                              ")">>,
            ?TEST_LOG("Preclaim function arguments ~p", [PreclaimArgs]),
            {ok, CallData} = aect_sophia:encode_call_data(Code, <<"signedPreclaim">>,
                                                          PreclaimArgs),
            ?TEST_LOG("CallData ~p", [CallData]),
            true = is_binary(CallData),
            {ok, CallTx} =
                aect_call_tx:new(
                    #{caller_id   => aec_id:create(account, OPubKey),
                      nonce       => Nonce,
                      contract_id => aec_id:create(contract,
                                                    ContractId),
                      vm_version  => ?VM_VERSION,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => 1,
                      call_data   => CallData,
                      fee         => 500000}),
            ?TEST_LOG("Contract call tx ~p", [CallTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 4}),
            {ok, _} = aetx:process(CallTx, OnChainTrees,
                                  TxEnv),
            {ok, OnChainTrees1} = aetx:process(CallTx,
                                                OnChainTrees,
                                                TxEnv),
            CallId = aect_call:id(OPubKey,
                                  Nonce,
                                  ContractId),
            Calls = aec_trees:calls(OnChainTrees1),
            {value, Call} =
                aect_call_state_tree:lookup_call(ContractId, CallId,
                                                  Calls),
            ok = aect_call:return_type(Call),
            S1 = aesc_test_utils:set_trees(OnChainTrees1, S0),
            Props#{state => S1}
          end]),
    ?TEST_LOG("Name preclaimed on-chain, proceeding with off-chain tests", []),

    Test =
        fun(Owner, Forcer) ->
            ?TEST_LOG("Name preclaim off-chain, owner is ~p, forcer is ~p",
                      [Owner, Forcer]),
            ContractCreateRound = 10,
            run(#{cfg => Cfg},
                [ % test contract on-chain:
                  % create account for being contract owner
                  positive(fun create_channel_/2),
                  % store state on-chain via snapshot
                  set_from(initiator),
                  set_prop(round, 42),
                  set_prop(state_hash, StateHash),
                  positive(fun snapshot_solo_/2),
                  fun(#{state := S,
                        channel_pubkey := ChannelPubKey} = Props) ->
                      Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                      Round = aesc_channels:round(Channel),
                      StateHash = aesc_channels:state_hash(Channel),
                      Props
                  end,
                  % create contract off-chain
                  create_trees_if_not_present(),
                  set_from(Owner, owner, owner_privkey),
                  create_contract_in_trees(_Round    = ContractCreateRound,
                                          _Contract = ContractName,
                                          _InitArgs = <<"()">>,
                                          _Deposit  = 2),
                  % force progress contract on-chain
                  fun(#{contract_id := ContractId,
                        from_pubkey := Pubkey,
                        from_privkey := Privkey} = Props) ->
                      Sig = SignContractAddress(Pubkey, Privkey, ContractId),
                      Account = aeu_hex:hexstring_encode(Pubkey),
                      PreclaimArgs = <<"(", Account/binary, ",",
                                            CHash/binary, ",",
                                            Sig/binary,
                                        ")">>,
                      ?TEST_LOG("Off-chain preclaim args ~p", [PreclaimArgs]),
                      (force_call_contract_first(Forcer, <<"signedPreclaim">>,
                                            PreclaimArgs, FPRound))(Props)
                  end,
                  % ensure all gas is consumed and channel is updated
                  fun(#{state := S,
                        channel_pubkey := ChannelPubKey,
                        signed_force_progress := SignedForceProgressTx,
                        solo_payload := #{update    := Update,
                                        state_hash := ExpectedStateHash,
                                        round      := ExpectedRound}} = Props) ->
                      {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
                      TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                                            aetx_sign:hash(SignedForceProgressTx)),
                      CallId = aect_call:id(Caller,
                                            FPRound,
                                            TxHashContractPubkey),
                      Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                                      S),
                      ?TEST_LOG("Off-chain call ~p", [Call]),
                      {_, GasPrice, GasLimit} = aesc_offchain_update:extract_amounts(Update),
                      %% assert all gas was consumed
                      GasLimit = aect_call:gas_used(Call),
                      GasPrice = aect_call:gas_price(Call),
                      %% the default catch all reason for error
                      <<"not_allowed_off_chain">> = aect_call:return_value(Call),
                      error = aect_call:return_type(Call),

                      %% expected channel states
                      Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                      FPRound = aesc_channels:round(Channel),
                      FPRound = ExpectedRound,
                      ExpectedStateHash = aesc_channels:state_hash(Channel),
                      Props
                  end])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES],
    ok.

fp_settle_too_soon(Cfg) ->
    AfterSlash =
        fun(CloseRound, SlashRound, FPRound, Closer, Slasher, Owner, Forcer) ->
            IAmt0 = 30,
            RAmt0 = 30,
            LockPeriod = 10,
            CloseHeight = 10,
            SlashHeight = 12,
            FPHeight = SlashHeight + 1,
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
                poi_participants_only(),
                set_prop(round, CloseRound),
                positive(fun close_solo_/2),
                % slash
                set_prop(height, SlashHeight),
                set_prop(round, SlashRound),
                set_from(Slasher),
                poi_participants_only(),
                create_payload(),
                positive(fun slash_/2),
                % force progress
                create_fp_trees(),
                set_prop(round, FPRound - 1), % for the payload
                create_payload(),
                fun(Props) when CloseRound =:= FPRound - 1 ->
                      Props#{payload => <<>>};
                   (Props) -> Props
                end,
                set_prop(height, FPHeight),
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
                fun(#{initiator_pubkey  := I,
                      responder_pubkey  := R,
                      trees             := Trees} = Props) ->
                    Accounts = aec_trees:accounts(Trees),
                    Balance =
                        fun(Pubkey) ->
                            Acc = aec_accounts_trees:get(Pubkey, Accounts),
                            _Bal = aec_accounts:balance(Acc)
                        end,
                    IBal = Balance(I),
                    RBal = Balance(R),
                    Props#{initiator_amount => IBal,
                           responder_amount => RBal}
                end,
                % height is not enough to accept a settle tx
                negative(fun settle_/2, {error, channel_not_closed}),
                % when a proper height is reached - a settle tx is
                % accepted
                set_prop(height, FPHeight + LockPeriod + 1),
                positive(fun settle_/2)
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

%% test that a force progress transaction can NOT produce an on-chain oracle
%% query via a contract
fp_oracle_query(Cfg) ->
    FunHashInt = get_oracle_fun_hash_int(<<"createQuery">>),
    ProduceCallData =
        fun(_Pubkey, _Privkey, Oracle, _OraclePrivkey, _QueryId, _ContractId, QueryFee) ->
            <<IntOracleId:256>> = Oracle,
            RelativeTTL = {variant, 0, [10]},
            Args = {IntOracleId, <<"Very much of a question">>, QueryFee, RelativeTTL, RelativeTTL},
            ?TEST_LOG("Oracle createQuery function arguments ~p", [Args]),
            aeso_heap:to_binary({FunHashInt, Args})
        end,
    fp_oracle_action(Cfg, ProduceCallData).

%% test that a force progress transaction can NOT respond an oracle
%% query via a contract
fp_oracle_respond(Cfg) ->
    FunHashInt = get_oracle_fun_hash_int(<<"signedRespond">>),
    ProduceCallData =
        fun(_Pubkey, _Privkey, Oracle, OraclePrivkey, QueryId, ContractId, _QueryFee) ->
            ?TEST_LOG("Oracle ~p", [Oracle]),
            ?TEST_LOG("QueryId ~p", [QueryId]),
            <<IntOracleId:256>> = Oracle,
            <<IntQueryId:256>> = QueryId,
            BinToSign = <<QueryId/binary, ContractId/binary>>,
            SigBin = <<Word1:256, Word2:256>> =
                enacl:sign_detached(aec_governance:add_network_id(BinToSign),
                                    OraclePrivkey),
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            Sig = {Word1, Word2},
            Args = {IntOracleId, IntQueryId, Sig,
                    aeso_heap:to_binary(42, 0)},
            ?TEST_LOG("Oracle respond function arguments ~p", [Args]),
            aeso_heap:to_binary({FunHashInt, Args})
        end,
    fp_oracle_action(Cfg, ProduceCallData).

%% test that a force progress transaction can NOT extend an oracle
%% via a contract
fp_oracle_extend(Cfg) ->
    FunHashInt = get_oracle_fun_hash_int(<<"signedExtendOracle">>),
    ProduceCallData =
        fun(_Pubkey, _Privkey, Oracle, OraclePrivkey, _QueryId, ContractId, _QueryFee) ->
            <<IntOracleId:256>> = Oracle,
            BinToSign = <<Oracle/binary, ContractId/binary>>,
            RelativeTTL = {variant, 0, [10]},
            SigBin = <<Word1:256, Word2:256>> =
                enacl:sign_detached(aec_governance:add_network_id(BinToSign),
                                    OraclePrivkey),
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            Sig = {Word1, Word2},
            Args = {IntOracleId, Sig, RelativeTTL},
            ?TEST_LOG("Oracle respond function arguments ~p", [Args]),
            aeso_heap:to_binary({FunHashInt, Args})
        end,
    fp_oracle_action(Cfg, ProduceCallData).

fp_oracle_action(Cfg, ProduceCallData) ->
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
    StateHash = <<42:StateHashSize/unit:8>>,
    Round = 42,
    FPRound = Round + 10,
    ContractName = "oracles",
    QueryFee = 1,

    % test contract on-chain
    % this validates that the contract and the fucntion are indeed callable
    % on-chain and they produce the expected oracle action
    ?TEST_LOG("Create contract ~p.aes on-chain", [ContractName]),
    run(#{cfg => Cfg},
        [ % create account for being contract owner
          set_prop(height, 10),
          fun(#{} = Props) ->
              S0 = aesc_test_utils:new_state(),
              {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
              S1 = aesc_test_utils:set_account_balance(NewAcc, 10000000, S),
              PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
              ?TEST_LOG("Owner: pubkey ~p, privkey: ~p", [NewAcc, PrivKey]),
              Props#{state => S1, onchain_contract_owner_pubkey => NewAcc,
                                  onchain_contract_owner_privkey => PrivKey}
          end,
          % create contract on-chain
          fun(#{onchain_contract_owner_pubkey := PubKey,
                state := S0} = Props) ->
            {ok, BinCode} = compile_contract(ContractName),
            {ok, CallData} = aect_sophia:encode_call_data(BinCode, <<"init">>, <<"()">>),
            Nonce = 1,
            {ok, ContractCreateTx} =
                aect_create_tx:new(
                    #{owner_id    => aec_id:create(account, PubKey),
                      nonce       => Nonce,
                      code        => BinCode,
                      vm_version  => ?VM_VERSION,
                      deposit     => 1,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => 1,
                      call_data   => CallData,
                      fee         => 1000000}),
            ?TEST_LOG("Contract create tx ~p", [ContractCreateTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 3}),
            {ok, _} = aetx:process(ContractCreateTx, OnChainTrees,
                                  TxEnv),
            {ok, OnChainTrees1} = aetx:process(ContractCreateTx,
                                                OnChainTrees,
                                                TxEnv),
            S1 = aesc_test_utils:set_trees(OnChainTrees1, S0),
            ContractId = aect_contracts:compute_contract_pubkey(PubKey, Nonce),
            ?TEST_LOG("Contract created on-chain, id ~p", [ContractId]),
            Props#{state => S1,
                    onchain_contract_id => ContractId,
                    code => BinCode}
          end,
          % create oracle
          register_new_oracle(aeso_heap:to_binary(string, 0),
                              aeso_heap:to_binary(word, 0),
                              QueryFee),
          oracle_query(aeso_heap:to_binary(<<"Some question">>, 0), 10),
          % call contract on-chain
          fun(#{onchain_contract_owner_pubkey := OPubKey,
                onchain_contract_owner_privkey := OPrivKey,
                onchain_contract_id := ContractId,
                oracle := Oracle,
                state := S0,
                query_id := QueryID} = Props) ->
            Nonce = 2,
            OraclePrivkey = aesc_test_utils:priv_key(Oracle, S0),
            CallData = ProduceCallData(OPubKey, OPrivKey, Oracle,
                                       OraclePrivkey, QueryID, ContractId, QueryFee),
            ?TEST_LOG("CallData ~p", [CallData]),
            true = is_binary(CallData),
            {ok, CallTx} =
                aect_call_tx:new(
                    #{caller_id   => aec_id:create(account, OPubKey),
                      nonce       => Nonce,
                      contract_id => aec_id:create(contract,
                                                    ContractId),
                      vm_version  => ?VM_VERSION,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => 1,
                      call_data   => CallData,
                      fee         => 500000}),
            ?TEST_LOG("Contract call tx ~p", [CallTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 4}),
            {ok, _} = aetx:process(CallTx, OnChainTrees,
                                  TxEnv),
            {ok, OnChainTrees1} = aetx:process(CallTx,
                                                OnChainTrees,
                                                TxEnv),
            CallId = aect_call:id(OPubKey,
                                  Nonce,
                                  ContractId),
            Calls = aec_trees:calls(OnChainTrees1),
            {value, Call} =
                aect_call_state_tree:lookup_call(ContractId, CallId,
                                                  Calls),
            ok = aect_call:return_type(Call),
            S1 = aesc_test_utils:set_trees(OnChainTrees1, S0),
            Props#{state => S1}
          end]),
    ?TEST_LOG("Oracle action succeded on-chain, proceeding with off-chain tests", []),
    Test =
        fun(Owner, Forcer) ->
            ?TEST_LOG("Oracle off-chain action, owner is ~p, forcer is ~p",
                      [Owner, Forcer]),
            ContractCreateRound = 10,
            run(#{cfg => Cfg},
                [ % test contract on-chain:
                  % create account for being contract owner
                  positive(fun create_channel_/2),
                  register_new_oracle(aeso_heap:to_binary(string, 0),
                                      aeso_heap:to_binary(word, 0),
                                      QueryFee),
                  % store state on-chain via snapshot
                  set_from(initiator),
                  set_prop(round, 42),
                  set_prop(state_hash, StateHash),
                  positive(fun snapshot_solo_/2),
                  fun(#{state := S,
                        channel_pubkey := ChannelPubKey} = Props) ->
                      Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                      Round = aesc_channels:round(Channel),
                      StateHash = aesc_channels:state_hash(Channel),
                      Props
                  end,
                  % create contract off-chain
                  create_trees_if_not_present(),

                  set_from(Owner, owner, owner_privkey),
                  create_contract_in_trees(_Round    = ContractCreateRound,
                                          _Contract = ContractName,
                                          _InitArgs = <<"()">>,
                                          _Deposit  = 2),
                  oracle_query(aeso_heap:to_binary(<<"Some question">>, 0), 10),
                  % force progress contract on-chain
                  fun(#{contract_id   := ContractId,
                        oracle        := Oracle,
                        from_pubkey   := Pubkey,
                        from_privkey  := Privkey,
                        state         := S,
                        query_id      := QueryID} = Props) ->
                      OraclePrivkey = aesc_test_utils:priv_key(Oracle, S),
                      CallData = ProduceCallData(Pubkey, Privkey, Oracle,
                                                 OraclePrivkey,
                                                 QueryID,
                                                 ContractId, QueryFee),
                      (force_call_contract_first_with_calldata(Forcer,
                                            CallData, FPRound))(Props)
                  end,
                  % ensure all gas is consumed and channel is updated
                  fun(#{state := S,
                        channel_pubkey := ChannelPubKey,
                        signed_force_progress := SignedForceProgressTx,
                        solo_payload := #{update    := Update,
                                        state_hash := ExpectedStateHash,
                                        round      := ExpectedRound}} = Props) ->
                      {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
                      TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                                            aetx_sign:hash(SignedForceProgressTx)),
                      CallId = aect_call:id(Caller,
                                            FPRound,
                                            TxHashContractPubkey),
                      Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                                      S),
                      ?TEST_LOG("Off-chain call ~p", [Call]),
                      {_, GasPrice, GasLimit} = aesc_offchain_update:extract_amounts(Update),
                      %% assert all gas was consumed
                      GasLimit = aect_call:gas_used(Call),
                      GasPrice = aect_call:gas_price(Call),
                      %% the default catch all reason for error
                      <<"not_allowed_off_chain">> = aect_call:return_value(Call),
                      error = aect_call:return_type(Call),

                      %% expected channel states
                      Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                      FPRound = aesc_channels:round(Channel),
                      FPRound = ExpectedRound,
                      ExpectedStateHash = aesc_channels:state_hash(Channel),
                      Props
                  end])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES],
    ok.


get_oracle_fun_hash_int(Function) ->
    {ok, Code} = compile_contract("oracles"),
    TypeInfo = maps:get(type_info, aect_sophia:deserialize(Code)),
    {ok, <<IntFunHash:256>>} = aeso_abi:type_hash_from_function_name(
                               Function, TypeInfo),
    IntFunHash.

fp_register_oracle(Cfg) ->
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
    StateHash = <<42:StateHashSize/unit:8>>,
    Round = 42,
    FPRound = Round + 10,
    SignAddress =
        fun(Oracle, PrivK, ContractId) ->
            BinToSign = <<Oracle/binary, ContractId/binary>>,
            SigBin = <<Word1:256, Word2:256>> =
                enacl:sign_detached(aec_governance:add_network_id(BinToSign), PrivK),
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            {Word1, Word2}
        end,

    IntFunHash = get_oracle_fun_hash_int(<<"signedRegisterOracle">>),
    RegisterCallData =
        fun(OPubKey, Sig) ->
            <<IntPubKey:256>> = OPubKey,
            RelativeTTL = {variant, 0, [10]},
            RegisterArgs = {IntPubKey, Sig, 2, RelativeTTL},
            ?TEST_LOG("Oracle register function arguments ~p", [RegisterArgs]),
            aeso_heap:to_binary({IntFunHash, RegisterArgs})
        end,
    ContractName = "oracles",

    % test contract on-chain
    % this validates that the contract and the fucntion are indeed callable
    % on-chain and it registers an oracle on-chain
    ?TEST_LOG("Create contract ~p.aes on-chain", [ContractName]),
    run(#{cfg => Cfg},
        [ % create account for being contract owner
          fun(#{} = Props) ->
              S0 = aesc_test_utils:new_state(),
              {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
              S1 = aesc_test_utils:set_account_balance(NewAcc, 10000000, S),
              PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
              ?TEST_LOG("Owner: pubkey ~p, privkey: ~p", [NewAcc, PrivKey]),
              Props#{state => S1, onchain_contract_owner_pubkey => NewAcc,
                                  onchain_contract_owner_privkey => PrivKey}
          end,
          % create contract on-chain
          fun(#{onchain_contract_owner_pubkey := PubKey,
                state := S0} = Props) ->
            {ok, BinCode} = compile_contract(ContractName),
            {ok, CallData} = aect_sophia:encode_call_data(BinCode, <<"init">>, <<"()">>),
            Nonce = 1,
            {ok, ContractCreateTx} =
                aect_create_tx:new(
                    #{owner_id    => aec_id:create(account, PubKey),
                      nonce       => Nonce,
                      code        => BinCode,
                      vm_version  => ?VM_VERSION,
                      deposit     => 1,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => 1,
                      call_data   => CallData,
                      fee         => 1000000}),
            ?TEST_LOG("Contract create tx ~p", [ContractCreateTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 3}),
            {ok, _} = aetx:process(ContractCreateTx, OnChainTrees,
                                  TxEnv),
            {ok, OnChainTrees1} = aetx:process(ContractCreateTx,
                                                OnChainTrees,
                                                TxEnv),
            S1 = aesc_test_utils:set_trees(OnChainTrees1, S0),
            ContractId = aect_contracts:compute_contract_pubkey(PubKey, Nonce),
            ?TEST_LOG("Contract created on-chain, id ~p", [ContractId]),
            Props#{state => S1,
                    onchain_contract_id => ContractId,
                    code => BinCode}
          end,
          % call contract on-chain
          fun(#{onchain_contract_owner_pubkey := OPubKey,
                onchain_contract_owner_privkey := OPrivKey,
                onchain_contract_id := ContractId,
                state := S0} = Props) ->
            Nonce = 2,
            Sig = SignAddress(OPubKey, OPrivKey, ContractId),
            CallData = RegisterCallData(OPubKey, Sig),
            ?TEST_LOG("CallData ~p", [CallData]),
            true = is_binary(CallData),
            {ok, CallTx} =
                aect_call_tx:new(
                    #{caller_id   => aec_id:create(account, OPubKey),
                      nonce       => Nonce,
                      contract_id => aec_id:create(contract,
                                                    ContractId),
                      vm_version  => ?VM_VERSION,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => 1,
                      call_data   => CallData,
                      fee         => 600000}),
            ?TEST_LOG("Contract call tx ~p", [CallTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 4}),
            {ok, _} = aetx:process(CallTx, OnChainTrees,
                                  TxEnv),
            {ok, OnChainTrees1} = aetx:process(CallTx,
                                                OnChainTrees,
                                                TxEnv),
            CallId = aect_call:id(OPubKey,
                                  Nonce,
                                  ContractId),
            Calls = aec_trees:calls(OnChainTrees1),
            {value, Call} =
                aect_call_state_tree:lookup_call(ContractId, CallId,
                                                  Calls),
            ok = aect_call:return_type(Call),
            S1 = aesc_test_utils:set_trees(OnChainTrees1, S0),
            Props#{state => S1}
          end]),
    ?TEST_LOG("Oracle registered on-chain, proceeding with off-chain tests", []),
    Test =
        fun(Owner, Forcer) ->
            ?TEST_LOG("Oracle register off-chain, owner is ~p, forcer is ~p",
                      [Owner, Forcer]),
            ContractCreateRound = 10,
            run(#{cfg => Cfg},
                [ % test contract on-chain:
                  % create account for being contract owner
                  positive(fun create_channel_/2),
                  % store state on-chain via snapshot
                  set_from(initiator),
                  set_prop(round, 42),
                  set_prop(state_hash, StateHash),
                  positive(fun snapshot_solo_/2),
                  fun(#{state := S,
                        channel_pubkey := ChannelPubKey} = Props) ->
                      Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                      Round = aesc_channels:round(Channel),
                      StateHash = aesc_channels:state_hash(Channel),
                      Props
                  end,
                  % create contract off-chain
                  create_trees_if_not_present(),
                  set_from(Owner, owner, owner_privkey),
                  create_contract_in_trees(_Round    = ContractCreateRound,
                                          _Contract = ContractName,
                                          _InitArgs = <<"()">>,
                                          _Deposit  = 2),
                  % force progress contract on-chain
                  fun(#{contract_id := ContractId,
                        from_pubkey := Pubkey,
                        from_privkey := Privkey} = Props) ->
                      Sig = SignAddress(Pubkey, Privkey, ContractId),
                      Account = aeu_hex:hexstring_encode(Pubkey),
                      CallData = RegisterCallData(Pubkey, Sig),
                      (force_call_contract_first_with_calldata(Forcer,
                                            CallData, FPRound))(Props)
                  end,
                  % ensure all gas is consumed and channel is updated
                  fun(#{state := S,
                        channel_pubkey := ChannelPubKey,
                        signed_force_progress := SignedForceProgressTx,
                        solo_payload := #{update    := Update,
                                        state_hash := ExpectedStateHash,
                                        round      := ExpectedRound}} = Props) ->
                      {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
                      TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                                            aetx_sign:hash(SignedForceProgressTx)),
                      CallId = aect_call:id(Caller,
                                            FPRound,
                                            TxHashContractPubkey),
                      Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                                      S),
                      ?TEST_LOG("Off-chain call ~p", [Call]),
                      {_, GasPrice, GasLimit} = aesc_offchain_update:extract_amounts(Update),
                      %% assert all gas was consumed
                      GasLimit = aect_call:gas_used(Call),
                      GasPrice = aect_call:gas_price(Call),
                      %% the default catch all reason for error
                      <<"not_allowed_off_chain">> = aect_call:return_value(Call),
                      error = aect_call:return_type(Call),

                      %% expected channel states
                      Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                      FPRound = aesc_channels:round(Channel),
                      FPRound = ExpectedRound,
                      ExpectedStateHash = aesc_channels:state_hash(Channel),
                      Props
                  end])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES],
    ok.

create_contract_poi_and_payload(Round, ContractRound, Owner) ->
    create_contract_poi_and_payload(Round, ContractRound, Owner, #{}).

create_contract_poi_and_payload(Round, ContractRound, Owner, Opts) ->

    fun(Props0) ->
        {Contract, ContractInitProps} =
            maps:get(contract_name, Props0, {"identity", <<"()">>}),
        ContractCreateDeposit =
            maps:get(contract_create_deposit, Props0, 2),
        run(Props0,
          [create_trees_if_not_present(),
           set_from(Owner, owner, owner_privkey),
           create_contract_in_trees(_Round    = ContractRound,
                                    _Contract = Contract,
                                    _InitArgs = ContractInitProps,
                                    _Deposit  = ContractCreateDeposit),
           create_fp_trees(),
           fun(#{state_hash := PoiHash, trees := Trees} = Props) ->
               ?assertEqual(PoiHash, aec_trees:hash(Trees)),
               Props
           end,
           set_prop(round, Round),
           fun(Props) ->
               case maps:get(fake_hash, Opts, none) of
                  none -> Props;
                  SH -> Props#{state_hash => SH}
               end
           end,
           create_payload()])
    end.

create_fp_trees() ->
    fun(#{trees := Trees} = Props) ->
        Hash = aec_trees:hash(Trees),
        Props#{state_hash => Hash, offchain_trees => Trees}
    end.

set_balances_in_trees(IBal, RBal) ->
    fun(#{initiator_pubkey := Initiator,
          responder_pubkey := Responder,
          trees            := Trees} = Props) ->
        Accounts =
            lists:foldl(
                fun({Pubkey, Balance}, Accum) ->
                    Acc = aec_accounts_trees:get(Pubkey, Accum),
                    {ok, Acc1} =
                        case aec_accounts:balance(Acc) of
                            B0 when B0 > Balance ->
                                aec_accounts:spend(Acc, B0 - Balance, 0);
                            B0 when B0 =< Balance ->
                                aec_accounts:earn(Acc, Balance - B0)
                        end,
                    aec_accounts_trees:enter(Acc1, Accum)
                end,
                aec_trees:accounts(Trees),
                [{Initiator, IBal},
                 {Responder, RBal}]),
        Trees1 = aec_trees:set_accounts(Trees, Accounts),
        Props#{trees => Trees1}
    end.

negative_force_progress_sequence(Round, Forcer, ErrMsg) ->
    Fee = 300000,
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
    Fee = 300000,
    fun(Props0) ->
        DepositAmt = maps:get(call_deposit, Props0, 1),
        {FunName, FunParams} = maps:get(contract_function_call, Props0,
                                        {<<"main">>, <<"42">>}),
        run(Props0,
           [get_onchain_balances(before_force),
            fun(#{state_hash := StateHash, offchain_trees := OffChainTrees} = Props) ->
                ?assertEqual(StateHash, aec_trees:hash(OffChainTrees)),
                Props
            end,
            set_from(Forcer),
            set_prop(round, Round),
            fun(#{contract_id := ContractId} = Props) ->
                (create_contract_call_payload(ContractId, FunName,
                                              FunParams, DepositAmt))(Props)
            end,
            set_prop(fee, Fee),
            positive(fun force_progress_/2),
            fun(#{channel_pubkey  := ChannelPubKey, state := S,
                  solo_payload    := #{state_hash := ExpectedStateHash,
                                       round      := ExpectedRound}} = Props) ->
                % ensure channel had been updated
                Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                Round = aesc_channels:round(Channel),
                % assert state_hash and round had changed
                ExpectedStateHash = aesc_channels:state_hash(Channel),
                ExpectedRound = aesc_channels:round(Channel),
                true = aesc_channels:is_last_state_forced(Channel),
                Props
            end,
            get_onchain_balances(after_force),
            fun(#{state := S,
                  signed_force_progress := SignedForceProgressTx,
                  before_force := #{initiator := I0, responder := R0},
                  after_force  := #{initiator := I1, responder := R1},
                  solo_payload := #{update := Update}} = Props) ->
                {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
                TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                                      aetx_sign:hash(SignedForceProgressTx)),
                CallId = aect_call:id(Caller,
                                      Round,
                                      TxHashContractPubkey),
                Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                                S),
                GasUsed = aect_call:gas_used(Call),
                GasLimit = maps:get(gas_limit, Props, 10000000),
                case maps:get(check_not_all_gas_used, Props, true) of
                    true -> ?assert(GasUsed < GasLimit);
                    false -> pass
                end,
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

-spec rename_prop(atom(), atom(), keep_old | delete_old) -> fun((map()) -> map()).
rename_prop(Key1, Key2, KeepOld) ->
    fun(Props) ->
        Value = maps:get(Key1, Props),
        Props1 =
            case KeepOld of
                keep_old -> Props;
                delete_old -> maps:remove(Key1, Props)
            end,
        maps:put(Key2, Value, Props1)
    end.

prepare_balances_for_mutual_close() ->
    fun(#{initiator_amount := IAmt, responder_amount := RAmt} = Props) ->
        Fee = maps:get(fee, Props, 50000),
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
    fun(#{trees := Trees0} = Props) ->
        Contract = aect_test_utils:get_contract(ContractId, #{trees => Trees0}),
        Code = aect_contracts:code(Contract),
        {ok, CallData} = aect_sophia:encode_call_data(Code, Fun, Args),
        %% assert calldata is correct:
        true = is_binary(CallData),
        (create_contract_call_payload_with_calldata(Key, ContractId, CallData,
                                                   Amount))(Props)
    end.

create_contract_call_payload_with_calldata(Key, ContractId, CallData, Amount) ->
    fun(#{from_pubkey       := From,
          round             := Round,
          state             := State,
          trees             := Trees0} = Props) ->
        Reserve = maps:get(channel_reserve, Props, 0),
        OnChainTrees = aesc_test_utils:trees(State),
        Env = tx_env(Props),
        Update =
            maps:get(solo_payload_update, Props,
                aesc_offchain_update:op_call_contract(
                    aec_id:create(account, From),
                    aec_id:create(contract, ContractId),
                    ?VM_VERSION, Amount, CallData,
                    [],
                    _GasPrice = maps:get(gas_price, Props, 1),
                    _GasLimit = maps:get(gas_limit, Props, 10000000))),
        {UpdatedTrees, StateHash} =
            case maps:get(fake_solo_state_hash, Props, none) of
                none ->
                    Trees1 = aesc_offchain_update:apply_on_trees(Update,
                                                                aect_call_state_tree:prune_without_backend(Trees0),
                                                                OnChainTrees,
                                                                Env,
                                                                Round,
                                                                Reserve),
                    StateHash1 = aec_trees:hash(Trees1),
                    {Trees1, StateHash1};
                SH ->
                    {Trees0, SH}
            end,
        Props#{Key => #{state_hash => StateHash,
                        round      => Round,
                        update     => Update},
               trees => UpdatedTrees}
    end.

set_tx_env(Height, TimeStamp, Beneficiary) ->
    fun(Props0) ->
        run(Props0,
            [set_prop(height, Height),
             set_prop(timestamp, TimeStamp),
             set_prop(beneficiary, Beneficiary),
             fun(Props) ->
                 Env = tx_env(Props),
                 Props#{aetx_env => Env}
             end
            ])
    end.

tx_env(#{height := Height} = Props) ->
    Time = maps:get(timestamp, Props, aeu_time:now_in_msecs()),
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    KeyBlockHash = <<42:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
    Beneficiary = maps:get(beneficiary, Props,
                          <<24:?BENEFICIARY_PUB_BYTES/unit:8>>),
    Env = aetx_env:contract_env(Height, ConsensusVersion, Time, Beneficiary,
                                123456, KeyBlockHash),
    %% Run as transaction!
    aetx_env:set_context(Env, aetx_transaction).

create_trees_if_not_present() ->
    fun(#{trees := _} = Props) -> Props; % trees are already present
       (#{initiator_amount  := IAmt,
          responder_amount  := RAmt,
          initiator_pubkey  := IPubkey,
          responder_pubkey  := RPubkey} = Props) ->
        Accounts = [aec_accounts:new(Pubkey, Balance) ||
                {Pubkey, Balance} <- [{IPubkey, IAmt},
                                      {RPubkey, RAmt}
                                     ]],
        Trees = aec_test_utils:create_state_tree_with_accounts(Accounts, no_backend),
        Props#{trees => Trees}
    end.

create_contract_in_trees(CreationRound, ContractName, InitArg, Deposit) ->
    fun(#{trees := Trees0,
          state := State,
          owner := Owner} = Props) ->
        {ok, BinCode}  = compile_contract(ContractName),
        {ok, CallData} = aect_sophia:encode_call_data(BinCode, <<"init">>, InitArg),
        Update = aesc_offchain_update:op_new_contract(aec_id:create(account, Owner),
                                                      ?VM_VERSION,
                                                      BinCode,
                                                      Deposit,
                                                      CallData),
        Reserve = maps:get(channel_reserve, Props, 0),
        OnChainTrees = aesc_test_utils:trees(State),
        Env = tx_env(Props),
        Trees = aesc_offchain_update:apply_on_trees(Update, Trees0, OnChainTrees,
                                                    Env,
                                                    CreationRound, Reserve),
        ContractId = aect_contracts:compute_contract_pubkey(Owner, CreationRound),
        ContractIds = maps:get(contract_ids, Props, []),
        case lists:member(ContractId, ContractIds) of
            true -> error(contract_already_present); % something is wrong with the test
            false -> pass
        end,
        Props#{trees => Trees, contract_id => ContractId,
               contract_ids => [ContractId | ContractIds]}
    end.

create_contract_in_onchain_trees(ContractName, InitArg, Deposit) ->
    fun(#{state := State0,
          owner := Owner} = Props) ->
        Trees0 = aesc_test_utils:trees(State0),
        {ok, BinCode} = compile_contract(ContractName),
        {ok, CallData} = aect_sophia:encode_call_data(BinCode, <<"init">>, InitArg),
        Nonce = aesc_test_utils:next_nonce(Owner, State0),
        {ok, AetxCreateTx} =
            aect_create_tx:new(#{owner_id   => aec_id:create(account, Owner),
                                 nonce      => Nonce,
                                 code       => BinCode,
                                 vm_version => ?VM_VERSION,
                                 deposit    => Deposit,
                                 amount     => 0,
                                 gas        => 123467,
                                 gas_price  => 1,
                                 call_data  => CallData,
                                 fee        => 10}),
        {contract_create_tx, CreateTx} = aetx:specialize_type(AetxCreateTx),
        Env = tx_env(Props),
        {ok, _} = aect_create_tx:check(CreateTx, Trees0, Env),
        {ok, Trees} = aect_create_tx:process(CreateTx, Trees0, Env),
        ContractId = aect_contracts:compute_contract_pubkey(Owner, Nonce),
        State = aesc_test_utils:set_trees(Trees, State0),
        Props#{state => State, contract_id => ContractId}
    end.

run(Cfg, Funs) ->
    lists:foldl(
        fun(Fun, Props) -> Fun(Props) end,
        Cfg,
        Funs).

apply_on_trees_(#{height := Height} = Props, SignedTx, S, positive) ->
    Trees = aens_test_utils:trees(S),
    Res =
        case maps:get(aetx_env, Props, none) of
            none ->
                aesc_test_utils:apply_on_trees_without_sigs_check(
                                  [SignedTx], Trees, Height);
            AetxEnv ->
                aesc_test_utils:apply_on_trees_without_sigs_check_with_env(
                                  [SignedTx], Trees, AetxEnv)
        end,
    case Res of
        {ok, [SignedTx], Trees1} ->
            S1 = aesc_test_utils:set_trees(Trees1, S),
            Props#{state => S1};
        Err ->
            throw({case_failed, Err})
    end;
apply_on_trees_(#{height := Height} = Props, SignedTx, S, {negative, ExpectedError}) ->
    Trees = aens_test_utils:trees(S),
    Tx = aetx_sign:tx(SignedTx),
    Env = aetx_env:tx_env(Height),
    case aetx:process(Tx, Trees, Env) of
        ExpectedError -> pass;
        {ok, _} -> throw(negative_case_passed)
    end,
    Props.

get_state(Cfg) ->
    case proplists:get_value(state, Cfg) of
        undefined -> aesc_test_utils:new_state();
        State0    -> State0
    end.

assert_locked_amount(ExpectedLockedAmt) ->
    fun(#{state := S} = Props) ->
        HolderPubKey = aec_governance:locked_coins_holder_account(),
        LockedTotal =
            case aesc_test_utils:lookup_account(HolderPubKey, S) of
                none -> 0;
                {value, Account} -> aec_accounts:balance(Account)
            end,
        case LockedTotal =:= ExpectedLockedAmt of
            true -> pass;
            false -> throw({different_locked_amount, {actual, LockedTotal},
                                                     {expected, ExpectedLockedAmt}})
        end,
        Props
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
    Env      = aetx_env:tx_env(Height),
    {ok, [SignedTx], Trees1} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S3 = aesc_test_utils:set_trees(Trees1, S2),

    %% Check channel created
    Trees2 = aesc_test_utils:trees(S3),
    ChannelPubKey = aesc_channels:pubkey(PubKey1, 1, PubKey2),
    {value, Ch} = aesc_state_tree:lookup(ChannelPubKey, aec_trees:channels(Trees2)),
    PubKey1 = aesc_channels:initiator_pubkey(Ch),
    PubKey2 = aesc_channels:responder_pubkey(Ch),
    1       = aesc_channels:round(Ch),
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
    Fee = maps:get(fee, Props, 50000),

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

    Fee = maps:get(fee, Props, 50000),
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
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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
                  offchain_trees    := OffChainTrees,
                  from_pubkey       := From,
                  from_privkey      := FromPrivkey,
                  fee               := Fee,
                  state             := S,
                  payload           := Payload,
                  solo_payload      := #{update     := Update,
                                         round      := Round,
                                         state_hash := StateHash},
                  initiator_privkey := _IPrivkey,
                  responder_privkey := _RPrivkey} = Props, Expected) ->

    ForceProTxSpec = aesc_test_utils:force_progress_tx_spec(ChannelPubKey, From,
                                                            Payload,
                                                            Update, StateHash,
                                                            Round, OffChainTrees,
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
                set_prop(fee, 50000),
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

test_both_can_not_replace_create(Cfg, Fun) ->
    test_both_can_not_replace_create(Cfg, Fun, #{}).

test_both_can_not_replace_create(Cfg, Fun, Props) ->
    Test =
        fun(Poster) ->
            run(Props#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(round, 1),
                set_from(Poster),
                set_prop(amount, 1),
                set_prop(fee, 50000),
                negative(Fun, {error, old_round})])
        end,
    [Test(Poster) || Poster <- ?ROLES],
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
                            Env = aetx_env:tx_env(Height),
                            {error, signature_check_failed} =
                                aetx:process(TxMissingS, Trees, Env)
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
    ChannelHashSize = aehttp_api_encoder:byte_size_for_type(channel),
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
    StateHashSize = aehttp_api_encoder:byte_size_for_type(state),
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
            {Delegate1, Delegate2, S} = create_loaded_accounts(100000, 100000),
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
            S1 = aesc_test_utils:set_account_balance(NewAcc, 500000, S),
            PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
            Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
         end,
         negative(Fun, {error, account_not_peer})]),
    ok.

register_new_oracle(QFormat, RFormat, QueryFee) ->
    fun(Props0) ->
        run(Props0,
           [fun(#{state := S0} = Props) ->
                {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
                S1 = aesc_test_utils:set_account_balance(NewAcc, 500000, S),
                Props#{state => S1, oracle => NewAcc}
            end,
            fun(#{state := S, oracle := Oracle} = Props) ->
                RegTx = aeo_test_utils:register_tx(Oracle,
                                                   #{query_format => QFormat,
                                                     query_fee => QueryFee,
                                                     response_format => RFormat,
                                                     vm_version => ?AEVM_01_Sophia_01
                                                    },
                                                   S),
                PrivKey = aesc_test_utils:priv_key(Oracle, S),
                SignedTx = aec_test_utils:sign_tx(RegTx, [PrivKey]),
                apply_on_trees_(Props, SignedTx, S, positive)
            end
           ])
    end.

oracle_query(Question, ResponseTTL) ->
    fun(Props0) ->
        run(Props0,
           [fun(#{state := S, oracle := Oracle} = Props) ->
                QueryTx = aeo_test_utils:query_tx(Oracle,
                                                  aec_id:create(oracle, Oracle),% oracle is asking
                                                  #{query => Question,
                                                    query_fee => 50000,
                                                    response_ttl => {delta, ResponseTTL}},
                                                  S),
                {_, Tx} = aetx:specialize_type(QueryTx),
                QueryId = aeo_query_tx:query_id(Tx),
                PrivKey = aesc_test_utils:priv_key(Oracle, S),
                SignedTx = aec_test_utils:sign_tx(QueryTx, [PrivKey]),
                Props1 = apply_on_trees_(Props, SignedTx, S, positive),
                Props1#{query_id => QueryId}
            end
           ])
    end.

oracle_response(Response, ResponseTTL) ->
    fun(Props0) ->
        run(Props0,
           [fun(#{state := S, oracle := Oracle, query_id := QueryId} = Props) ->
                ResponseTx = aeo_test_utils:response_tx(Oracle, QueryId, Response,
                                                        #{response_ttl => {delta, ResponseTTL}}, S),
                PrivKey = aesc_test_utils:priv_key(Oracle, S),
                SignedTx = aec_test_utils:sign_tx(ResponseTx, [PrivKey]),
                apply_on_trees_(Props, SignedTx, S, positive)
            end
           ])
    end.

register_name(Name, Pointers0) ->
    NameSalt = rand:uniform(10000),
    fun(Props0) ->
        run(Props0,
           [% create dummy account to hold the name
            fun(#{state := S0} = Props) ->
                {NewAcc, S} = aesc_test_utils:setup_new_account(S0),
                S1 = aesc_test_utils:set_account_balance(NewAcc, 1000000, S),
                Props#{state => S1, name_owner => NewAcc}
            end,
            % preclaim
            fun(#{state := S, name_owner := NameOwner} = Props) ->
                {ok, NameAscii} = aens_utils:to_ascii(Name),
                CHash = aens_hash:commitment_hash(NameAscii, NameSalt),
                TxSpec = aens_test_utils:preclaim_tx_spec(NameOwner, CHash, S),
                {ok, Tx} = aens_preclaim_tx:new(TxSpec),
                PrivKey = aesc_test_utils:priv_key(NameOwner, S),
                SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
                apply_on_trees_(Props, SignedTx, S, positive)
            end,
            % claim
            fun(#{state := S, name_owner := NameOwner, height := Height0} = Props) ->
                PrivKey = aesc_test_utils:priv_key(NameOwner, S),
                Delta = aec_governance:name_claim_preclaim_delta(),
                TxSpec = aens_test_utils:claim_tx_spec(NameOwner, Name, NameSalt, S),
                {ok, Tx} = aens_claim_tx:new(TxSpec),
                SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
                apply_on_trees_(Props#{height := Height0 + Delta}, SignedTx, S, positive)
            end,
            % update to set pointers
            fun(#{state := S, name_owner := NameOwner} = Props) ->
                PrivKey = aesc_test_utils:priv_key(NameOwner, S),
                {ok, NameAscii} = aens_utils:to_ascii(Name),
                NHash = aens_hash:name_hash(NameAscii),
                Pointers =
                    lists:map(
                        fun({PointerName, Value}) ->
                            aens_pointer:new(PointerName, Value)
                        end,
                        Pointers0),
                NameTTL  = 40000,
                TxSpec = aens_test_utils:update_tx_spec(
                           NameOwner, NHash, #{pointers => Pointers, name_ttl => NameTTL}, S),
                {ok, Tx} = aens_update_tx:new(TxSpec),
                SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
                apply_on_trees_(Props, SignedTx, S, positive)
            end
           ])
    end.

%% provide payload
force_call_contract_first(Forcer, Fun, Args, Round) ->
    fun(Props0) ->
        run(Props0,
           [set_prop(round, Round - 1),
            set_from(Forcer),
            create_fp_trees(),
            create_payload(),
            set_prop(round, Round),
            fun(#{contract_id := ContractId} = Props) ->
                (create_contract_call_payload(ContractId, Fun,
                                              Args, 1))(Props)
            end,
            set_prop(fee, 1000000),
            positive(fun force_progress_/2)
            ])
    end.

force_call_contract_first_with_calldata(Forcer, CallData, Round) ->
    fun(Props0) ->
        run(Props0,
           [set_prop(round, Round - 1),
            set_from(Forcer),
            create_fp_trees(),
            create_payload(),
            set_prop(round, Round),
            fun(#{contract_id := ContractId} = Props) ->
                (create_contract_call_payload_with_calldata(
                   solo_payload,
                   ContractId, CallData, 1))(Props)
            end,
            set_prop(fee, 600000),
            positive(fun force_progress_/2)
            ])
    end.

%% build on top of on-chain payload
force_call_contract(Forcer, Fun, Args) ->
    fun(Props0) ->
        #{channel_pubkey := ChannelPubKey, state := S} = Props0,
        Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
        Round = aesc_channels:round(Channel),
        (force_call_contract(Forcer, Fun, Args, Round + 1))(Props0)
    end.

force_call_contract(Forcer, Fun, Args, Round) ->
    fun(Props0) ->
        run(Props0,
            [set_prop(contract_function_call, {Fun, Args}),
            create_fp_trees(),
            set_prop(fee, 500000),
            set_prop(payload, <<>>),
            force_progress_sequence(Round, Forcer)])
    end.


assert_last_channel_result(Result, Type) ->
    fun(#{state := S,
          signed_force_progress := SignedForceProgressTx,
          solo_payload := #{update := Update,
                            round  := Round}} = Props) ->
        {_ContractId, Caller} = aesc_offchain_update:extract_call(Update),
        TxHashContractPubkey = aesc_utils:tx_hash_to_contract_pubkey(
                              aetx_sign:hash(SignedForceProgressTx)),
        CallId = aect_call:id(Caller,
                              Round,
                              TxHashContractPubkey),
        Call = aect_test_utils:get_call(TxHashContractPubkey, CallId,
                                        S),
        EncRValue = aect_call:return_value(Call),
        {ok, Result} = aeso_heap:from_binary(Type, EncRValue),
        Props
    end.

% Create poi just for participants; used by slash and close solo
poi_participants_only() ->
    fun(#{initiator_pubkey  := IPubkey,
          responder_pubkey  := RPubkey,
          trees             := Trees} = Props) ->
          PoI = calc_poi([IPubkey, RPubkey], [], Trees),
          PoIHash = aec_trees:poi_hash(PoI),
          Props#{poi => PoI, state_hash => PoIHash}
   end.

compile_contract(ContractName) ->
    aect_test_utils:compile_contract(
      filename:join(["contracts", 
                     filename:basename(ContractName, ".aes") ++ ".aes"])).
