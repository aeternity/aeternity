%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    CT test suite for AE State Channels on-chain transactions
%%% @end
%%%=============================================================================

-module(aesc_txs_SUITE).

-export([
          all/0
        , groups/0
        , suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% test case exports
-export([create/1,
         close_solo/1,
         close_mutual/1,
         slash/1,
         slash_after_lock_timer/1,
         slash_by_delegate/1,
         deposit/1,
         withdraw/1,
         settle/1,
         snapshot_solo/1,
         set_delegates/1]).

%% negative create
-export([create_missing_account/1,
         create_same_account/1,
         create_insufficient_funds/1,
         create_wrong_nonce/1,
         create_insufficient_funds_reserve/1,
         create_exsisting/1
         ]).

%% negative close solo
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

-export([set_delegates_unknown_from/1,
         set_delegates_missing_channel/1,
         set_delegates_not_participant/1,
         set_delegates_already_closing/1,
         set_delegates_payload_from_another_channel/1,
         set_delegates_payload_not_co_signed/1,
         set_delegates_old_payload/1,
         set_delegates_can_not_replace_create/1,
         set_delegates_state_hash_mismatch/1,
         set_delegates_round_mismatch/1
        ]).

%% negative close mutual
%% close mutual does not have a `from` - it is always implicitly the initiator
%% thus we can not test the tx being posted from another account (not
%% participant or a delegate). If it is signed by non-participant - the
%% signature test will fail
-export([close_mutual_wrong_amounts/1,
         close_mutual_wrong_nonce/1,
         close_mutual_missing_channel/1
        ]).

%% negative slash
-export([slash_not_closing/1,
         slash_unknown_from/1,
         slash_wrong_amounts/1,
         slash_not_participant/1,
         slash_wrong_nonce/1,
         slash_payload_from_another_channel/1,
         slash_payload_not_co_signed/1,
         slash_invalid_state_hash/1,
         slash_older_payload/1,
         slash_no_payload/1,
         slash_missing_channel/1
         ]).

%% negative settle
-export([settle_wrong_amounts/1,
         settle_wrong_nonce/1,
         settle_missing_channel/1,
         settle_not_closing/1,
         settle_not_yet_closable/1,
         settle_not_participant/1,
         settle_delegate_not_allowed/1
        ]).

%% negative deposit
-export([deposit_unknown_from/1,
         deposit_insufficent_funds/1,
         deposit_wrong_nonce/1,
         deposit_missing_channel/1,
         deposit_closing/1,
         deposit_closed/1,
         deposit_older_round/1,
         deposit_can_not_replace_create/1,
         deposit_not_participant/1,
         deposit_delegate_not_allowed/1
        ]).

%% negative withdraw
-export([withdraw_unknown_from/1,
         withdraw_insufficent_funds/1,
         withdraw_wrong_nonce/1,
         withdraw_missing_channel/1,
         withdraw_closing/1,
         withdraw_closed/1,
         withdraw_older_round/1,
         withdraw_can_not_replace_create/1,
         withdraw_not_participant/1,
         withdraw_delegate_not_allowed/1
        ]).


%% negative snapshot solo
-export([snapshot_closed_channel/1,
         snapshot_closing_channel/1,
         snapshot_missing_channel/1,
         snapshot_payload_from_another_channel/1,
         snapshot_payload_not_co_signed/1,
         snapshot_old_payload/1,
         snapshot_can_not_replace_create/1,
         snapshot_not_participant/1,
         snapshot_delegate_sometimes_allowed/1
        ]).

%% positive force progress
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

%% negative force progress
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

%% more complex scenarios
-export([ fp_close_solo_slash_with_same_round/1
        , fp_fp_close_solo_with_same_round/1
        , fp_from_delegate_after_iris_not_closing/1
        , fp_from_delegate_after_iris/1
        , fp_wrong_delegate_after_iris/1
        ]).

%% fork related tests
-export([ fp_sophia_versions/1,
          close_mutual_already_closing/1,
          reject_old_offchain_tx_vsn/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include("../../aecore/include/blocks.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(MINER_PUBKEY, <<12345:?MINER_PUB_BYTES/unit:8>>).
-define(BOGUS_CHANNEL, <<1:?MINER_PUB_BYTES/unit:8>>).
-define(ROLES, [initiator, responder]).
-define(TEST_LOG(Format, Data), ct:log(Format, Data)).
-define(MINERVA_FORK_HEIGHT, 1000).
-define(FORTUNA_FORK_HEIGHT, 10000).
-define(LIMA_FORK_HEIGHT,    100000).

-define(IF_AEVM(AEVM, FATE),
    case ?IS_AEVM_SOPHIA(aect_test_utils:vm_version()) of
        true  -> AEVM;
        false -> FATE
    end).

-define(assertMatchVM(AEVM, FATE, Res),
    case ?IS_AEVM_SOPHIA(aect_test_utils:vm_version()) of
        true  -> ?assertMatch(AEVM, Res);
        false -> ?assertMatch(FATE, Res)
    end).

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
       {group, close_solo_with_payload_negative},
       {group, close_solo_without_payload_negative},
       close_mutual,
       {group, close_mutual_negative},
       slash,
       slash_after_lock_timer,
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
       {group, aevm},
       {group, fate},
       {group, fork_awareness},
       {group, set_delegates}
      ]
     },

     {aevm, [], [{group, force_progress}, {group, force_progress_negative}, {group, complex_sequences}]},
     {fate, [], [{group, force_progress}, {group, force_progress_negative}, {group, complex_sequences}]},

     {create_negative, [sequence],
      [create_missing_account,
       create_same_account,
       create_insufficient_funds,
       create_wrong_nonce,
       create_insufficient_funds_reserve,
       create_exsisting
      ]},
     {close_solo_with_payload_negative, [sequence],
      close_solo_negative_seq()},
     {close_solo_without_payload_negative, [sequence],
      close_solo_negative_seq()},
     {close_mutual_negative, [sequence],
      [close_mutual_wrong_amounts,
       close_mutual_wrong_nonce,
       close_mutual_missing_channel
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
       slash_no_payload,
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
       deposit_closed,
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
       withdraw_closed,
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
       snapshot_delegate_sometimes_allowed
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
     {force_progress_negative, [sequence], [{group, fp_with_payload},
                                            {group, fp_empty_payload}]},
     {fp_with_payload, [sequence],  force_progress_payload_negative_seq() ++
                                    force_progress_negative_seq()},
     {fp_empty_payload, [sequence], force_progress_negative_seq()},
     {fork_awareness, [sequence],
      [ fp_sophia_versions,
        close_mutual_already_closing,
        reject_old_offchain_tx_vsn
      ]},
     {complex_sequences, [sequence],
      [ fp_close_solo_slash_with_same_round
      , fp_fp_close_solo_with_same_round
      ]},
     {set_delegates, [sequence],
      [set_delegates,
       {group, set_delegates_negative}
      ]},
     {set_delegates_negative, [sequence],
      [set_delegates_unknown_from,
       set_delegates_missing_channel,
       set_delegates_not_participant,
       set_delegates_already_closing,
       set_delegates_payload_from_another_channel,
       set_delegates_payload_not_co_signed,
       set_delegates_old_payload,
       set_delegates_can_not_replace_create,
       set_delegates_state_hash_mismatch,
       set_delegates_round_mismatch
      ]}
    ].

close_solo_negative_seq() ->
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
    ].

force_progress_payload_negative_seq() ->
    [ fp_payload_from_another_channel,
      fp_payload_not_co_signed,
      fp_payload_invalid_state_hash,
      fp_payload_older_payload,
      % solo signed payload tests
      fp_solo_payload_invalid_state_hash,
      fp_solo_payload_wrong_round,
      fp_solo_payload_not_call_update,
      fp_solo_payload_broken_call,
      % closing, balances are checked
      fp_solo_payload_closing_overflowing_balances,
      fp_can_not_replace_create

    ].

force_progress_negative_seq() ->
    [ fp_closed_channel,
      fp_not_participant,
      fp_missing_channel,
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
      fp_oracle_respond,

      fp_from_delegate_after_iris,
      fp_from_delegate_after_iris_not_closing,
      fp_wrong_delegate_after_iris
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(fork_awareness, Config) ->
    case aect_test_utils:latest_protocol_version() of
        P when P >= ?IRIS_PROTOCOL_VSN -> {skip, not_updated_for_iris_yet};
        _P ->
            meck:expect(aec_hard_forks, protocol_effective_at_height,
                fun(V) when V < ?MINERVA_FORK_HEIGHT -> ?ROMA_PROTOCOL_VSN;
                   (V) when V < ?FORTUNA_FORK_HEIGHT -> ?MINERVA_PROTOCOL_VSN;
                   (V) when V < ?LIMA_FORK_HEIGHT    -> ?FORTUNA_PROTOCOL_VSN;
                   (_)                               -> ?LIMA_PROTOCOL_VSN
                end),
            aect_test_utils:init_per_group(aevm, Config)
    end;
init_per_group(VM, Config) when VM == aevm; VM == fate ->
    aect_test_utils:init_per_group(VM, Config);
init_per_group(close_solo_with_payload_negative, Config) ->
    init_per_group_([{close_solo_use_payload, true} | Config]);
init_per_group(close_solo_without_payload_negative, Config) ->
    init_per_group_([{close_solo_use_payload, false} | Config]);
init_per_group(fp_with_payload, Config) ->
    init_per_group_([{force_progress_use_payload, true} | Config]);
init_per_group(fp_empty_payload, Config) ->
    init_per_group_([{force_progress_use_payload, false} | Config]);
init_per_group(set_delegates, Config) ->
    case aect_test_utils:latest_protocol_version() of
        P when P < ?IRIS_PROTOCOL_VSN -> {skip, no_set_delegates_before_iris};
        _P ->
            Config
    end;
init_per_group(_Group, Config) ->
    init_per_group_(Config).

init_per_group_(Config) ->
    %% Disable name auction for these groups
    application:set_env(aecore, name_claim_bid_timeout, 0),
    Config.

end_per_group(fork_awareness, _Config) ->
    meck:unload(aec_hard_forks),
    ok;
end_per_group(_Group, _Config) ->
    application:unset_env(aecore, name_claim_bid_timeout),
    ok.

init_per_testcase(_, Config) ->
    case proplists:is_defined(sophia_version, Config) of
        true  -> aect_test_utils:setup_testcase(Config);
        false -> put('$protocol_version', aect_test_utils:latest_protocol_version())
    end,
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

protocol_version() ->
    case get('$protocol_version') of Vsn when is_integer(Vsn) -> Vsn end.


%%%===================================================================

create(Cfg) ->
    run(#{cfg => Cfg},
        [positive(fun create_channel_/2)]).

create_missing_account(_Cfg) ->
    {PubKey, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    PrivKey = aesc_test_utils:priv_key(PubKey, S),

    {BadPubKey, BadPrivKey} = aesc_test_utils:new_key_pair(), % not present in state
    Trees = aesc_test_utils:trees(S),
    Height = 100,
    Env0 = aetx_env:tx_env(Height),

    DefSpec = no_delegates_spec(Height),
    TxSpec1 = aesc_test_utils:create_tx_spec(BadPubKey, PubKey, DefSpec, S),
    {ok, Tx1} = aesc_create_tx:new(TxSpec1),
    SignedTx1 = aec_test_utils:sign_tx(Tx1, [PrivKey, BadPrivKey]),
    Env1 = aetx_env:set_signed_tx(Env0, {value, SignedTx1}),
    {error, account_not_found} = aetx:process(Tx1, Trees, Env1),

    TxSpec2 = aesc_test_utils:create_tx_spec(PubKey, BadPubKey, DefSpec, S),
    {ok, Tx2} = aesc_create_tx:new(TxSpec2),
    SignedTx2 = aec_test_utils:sign_tx(Tx2, [PrivKey, BadPrivKey]),
    Env2 = aetx_env:set_signed_tx(Env0, {value, SignedTx2}),
    {error, account_not_found} = aetx:process(Tx2, Trees, Env2),

    ok.

create_same_account(_Cfg) ->
    {PubKey, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    PrivKey = aesc_test_utils:priv_key(PubKey, S),
    Trees = aesc_test_utils:trees(S),
    Height = 100,
    Env0 = aetx_env:tx_env(Height),

    %% Test channel with oneself is not allowed
    TxSpecI = aesc_test_utils:create_tx_spec(
                PubKey, PubKey,
                no_delegates_spec(Height), S),
    {ok, TxI} = aesc_create_tx:new(TxSpecI),
    SignedTx = aec_test_utils:sign_tx(TxI, [PrivKey]),
    Env = aetx_env:set_signed_tx(Env0, {value, SignedTx}),
    {error, initiator_is_responder} = aetx:process(TxI, Trees, Env),
    ok.

create_insufficient_funds(_Cfg) ->
    {Loaded, NotLoaded, S} = create_loaded_accounts(60000 * aec_test_utils:min_gas_price(),
                                                    1),
    LoadedPrivKey = aesc_test_utils:priv_key(Loaded, S),
    NotLoadedPrivKey = aesc_test_utils:priv_key(NotLoaded, S),
    Trees = aesc_test_utils:trees(S),
    Height = 100,
    Env0 = aetx_env:tx_env(Height),

    %% Test insufficient initiator funds
    TxSpecI = aesc_test_utils:create_tx_spec(
                NotLoaded, Loaded,
                maps:merge(no_delegates_spec(Height),
                          #{ initiator_amount => 2
                           , fee => 50000 * aec_test_utils:min_gas_price()}),
                S),
    {ok, TxI} = aesc_create_tx:new(TxSpecI),
    SignedTxI = aec_test_utils:sign_tx(TxI, [LoadedPrivKey, NotLoadedPrivKey]),
    EnvI = aetx_env:set_signed_tx(Env0, {value, SignedTxI}),
    {error, insufficient_funds} = aetx:process(TxI, Trees, EnvI),

    %% Test insufficient responder funds
    TxSpecR = aesc_test_utils:create_tx_spec(
                Loaded, NotLoaded,
                maps:merge(no_delegates_spec(Height),
                           #{ responder_amount => 2
                            , fee => 50000 * aec_test_utils:min_gas_price()}),
                S),
    {ok, TxR} = aesc_create_tx:new(TxSpecR),
    SignedTxR = aec_test_utils:sign_tx(TxR, [LoadedPrivKey, NotLoadedPrivKey]),
    EnvR = aetx_env:set_signed_tx(Env0, {value, SignedTxR}),
    {error, insufficient_funds} = aetx:process(TxR, Trees, EnvR),

    ok.


create_insufficient_funds_reserve(_Cfg) ->
    {Loaded1, Loaded2, S} = create_loaded_accounts(100000 * aec_test_utils:min_gas_price(),
                                                   100000 * aec_test_utils:min_gas_price()),
    Loaded1PrivKey = aesc_test_utils:priv_key(Loaded1, S),
    Loaded2PrivKey = aesc_test_utils:priv_key(Loaded2, S),
    Trees = aesc_test_utils:trees(S),
    Height = 100,
    Env0 = aetx_env:tx_env(Height),

    %% Test initiator funds lower than channel reserve
    TxSpecI = aesc_test_utils:create_tx_spec(
                Loaded1, Loaded2,
                maps:merge(no_delegates_spec(Height),
                           #{ initiator_amount => 1
                            , channel_reserve => 2}),
                S),
    {ok, TxI} = aesc_create_tx:new(TxSpecI),
    SignedTxI = aec_test_utils:sign_tx(TxI, [Loaded1PrivKey, Loaded2PrivKey]),
    EnvI = aetx_env:set_signed_tx(Env0, {value, SignedTxI}),
    {error, insufficient_initiator_amount} = aetx:process(TxI, Trees, EnvI),

    %% Test responder funds lower than channel reserve
    TxSpecR = aesc_test_utils:create_tx_spec(
                Loaded1, Loaded2,
                maps:merge(no_delegates_spec(Height),
                           #{ responder_amount => 1
                            , channel_reserve => 2}),
                S),
    {ok, TxR} = aesc_create_tx:new(TxSpecR),
    SignedTxR = aec_test_utils:sign_tx(TxR, [Loaded1PrivKey, Loaded2PrivKey]),
    EnvR = aetx_env:set_signed_tx(Env0, {value, SignedTxR}),
    {error, insufficient_responder_amount} = aetx:process(TxR, Trees, EnvR),

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
    {Initiator, Responder, S0} = create_loaded_accounts(100000 * aec_test_utils:min_gas_price(),
                                                        100000 * aec_test_utils:min_gas_price()),
    InitiatorPrivKey = aesc_test_utils:priv_key(Initiator, S0),
    ResponderPrivKey = aesc_test_utils:priv_key(Responder, S0),
    Nonce = 42,
    S = aesc_test_utils:set_account_nonce(Initiator, Nonce, S0),
    Trees = aesc_test_utils:trees(S),
    Height = 100,
    Env0 = aetx_env:tx_env(Height),

    Test =
        fun(TestNonce, Err) ->
            TxSpec = aesc_test_utils:create_tx_spec(Initiator, Responder,
                maps:merge(no_delegates_spec(Height),
                           #{nonce => TestNonce}), S),
            {ok, Tx} = aesc_create_tx:new(TxSpec),
            SignedTx = aec_test_utils:sign_tx(Tx, [InitiatorPrivKey,
                                                   ResponderPrivKey]),
            Env = aetx_env:set_signed_tx(Env0, {value, SignedTx}),
            {error, Err} = aetx:process(Tx, Trees, Env)
        end,
    Test(Nonce - 1, tx_nonce_already_used_for_account),
    Test(Nonce, tx_nonce_already_used_for_account),
    Test(Nonce + 2, tx_nonce_too_high_for_account),
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
                positive(fun close_solo_with_payload/2),
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
                calc_poi_by_balances(IStartAmt, RStartAmt),
                positive(fun close_solo_without_payload/2),
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
                positive(fun close_solo_without_payload/2),
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
                positive(fun close_solo_without_payload/2),
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
         negative(fun close_solo_without_payload/2,
                  {error, account_not_found})]),
    ok.

%% Test wrong amounts (different than channel balance)
close_solo_wrong_amounts(Cfg) ->
    IAmt = 35 * aec_test_utils:min_gas_price(),
    RAmt = 42 * aec_test_utils:min_gas_price(),
    Test =
        fun(Closer, ICloseAmt, RCloseAmt) ->
            run(#{cfg => Cfg, initiator_amount => IAmt, responder_amount => RAmt},
                [ positive(fun create_channel_/2),
                  set_prop(initiator_amount, ICloseAmt),
                  set_prop(responder_amount, RCloseAmt),
                  set_from(Closer),
                  negative_close_solo_with_optional_payload(Cfg,
                      {error, poi_amounts_change_channel_funds}, % there is payload
                      {error, invalid_poi_hash_in_channel} %  no payload
                      )])
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
    test_not_participant(Cfg, close_solo_with_optional_payload(Cfg)).

close_solo_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, close_solo_with_optional_payload(Cfg)).

close_solo_payload_from_another_channel(Cfg) ->
    test_both_payload_from_different_channel(Cfg, fun close_solo_with_payload/2).

close_solo_payload_not_co_signed(Cfg) ->
    test_payload_not_both_signed(Cfg, fun aesc_test_utils:close_solo_tx_spec/5,
                                      fun aesc_close_solo_tx:new/1).

close_solo_invalid_state_hash(Cfg) ->
    test_both_invalid_poi_hash(Cfg, fun close_solo_with_payload/2).

close_solo_older_payload(Cfg) ->
    test_both_old_round(Cfg, fun close_solo_with_payload/2, #{}, old_round).

close_solo_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun close_solo_with_payload/2).

close_solo_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, close_solo_with_optional_payload(Cfg)).

close_solo_already_closing(Cfg) ->
    test_both_closing_channel(Cfg, close_solo_with_optional_payload(Cfg)).

close_solo_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, close_solo_with_optional_payload(Cfg)).

%%%===================================================================
%%% Close mutual
%%%===================================================================

close_mutual(Cfg) ->
    StartIAmt = 100000 * aec_test_utils:min_gas_price(),
    StartRAmt = 100000 * aec_test_utils:min_gas_price(),
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
    Fee = 50000 * aec_test_utils:min_gas_price(),

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
    StartIAmt = 100000 * aec_test_utils:min_gas_price(),
    StartRAmt = 100000 * aec_test_utils:min_gas_price(),

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
    Test(100000 * aec_test_utils:min_gas_price(),
         100000 * aec_test_utils:min_gas_price(),
         50000 * aec_test_utils:min_gas_price(),
         wrong_amounts),
    % fee too small
    Test(50 * aec_test_utils:min_gas_price(),
         50 * aec_test_utils:min_gas_price(), 0, too_low_fee),
    ok.

close_mutual_wrong_nonce(Cfg) ->
    StartIAmt = 100000 * aec_test_utils:min_gas_price(),
    StartRAmt = 100000 * aec_test_utils:min_gas_price(),
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
    Test(InitiatorNonce - 1,  tx_nonce_already_used_for_account),
    Test(InitiatorNonce,      tx_nonce_already_used_for_account),
    Test(InitiatorNonce + 2,  tx_nonce_too_high_for_account),
    ok.


close_mutual_missing_channel(Cfg) ->
    StartIAmt = 100000 * aec_test_utils:min_gas_price(),
    StartRAmt = 100000 * aec_test_utils:min_gas_price(),
    ChannelHashSize = aeser_api_encoder:byte_size_for_type(channel),
    FakeChannelPubKey = <<42:ChannelHashSize/unit:8>>,

    run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
        [positive(fun create_channel_/2),
         %% prepare balances and a fee..
         prepare_balances_for_mutual_close(),
         set_prop(channel_pubkey, FakeChannelPubKey),
         negative(fun close_mutual_/2, {error, channel_does_not_exist})]),
    ok.

close_mutual_already_closing(Cfg) ->
    StartIAmt = 100000 * aec_test_utils:min_gas_price(),
    StartRAmt = 100000 * aec_test_utils:min_gas_price(),
    FortunaHeight = 12345,
    LimaHeight = 123456,

    true = LimaHeight > FortunaHeight,
    true = FortunaHeight >= ?FORTUNA_FORK_HEIGHT,
    true = LimaHeight >= ?LIMA_FORK_HEIGHT,

    Test =
        fun(Closer) ->
            run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, FortunaHeight),
                positive(fun close_solo_with_payload/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                %% prepare balances and a fee..
                prepare_balances_for_mutual_close(),
                % Before Lima fork this should fail
                negative(fun close_mutual_/2, {error, channel_not_active}),
                % After Lima fork this should succeed
                set_prop(height, LimaHeight),
                positive(fun close_mutual_/2)])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

reject_old_offchain_tx_vsn(Cfg) ->
    StartIAmt = 100000 * aec_test_utils:min_gas_price(),
    StartRAmt = 100000 * aec_test_utils:min_gas_price(),

    RomaHeight = 123,
    FortunaHeight = 12345,
    LimaHeight = 123456,

    % ensure assumptions regarding heights
    true = RomaHeight < ?MINERVA_FORK_HEIGHT,
    true = LimaHeight > FortunaHeight,
    true = FortunaHeight >= ?FORTUNA_FORK_HEIGHT,
    true = LimaHeight >= ?LIMA_FORK_HEIGHT,

    Test =
        fun(Closer) ->
            run(#{cfg => Cfg, initiator_amount => StartIAmt, responder_amount => StartRAmt},
               [positive(fun create_channel_/2),
                set_from(Closer),
                %% produce an off-chain transaction with the old structure
                set_prop(height, RomaHeight),
                create_payload(),
                set_prop(height, LimaHeight),
                %% it fails after Lima
                negative(fun close_solo_with_payload/2,
                         {error, invalid_at_protocol})
                ])
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
                positive(fun close_solo_with_payload/2),
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

slash_after_lock_timer(Cfg) ->
    LockPeriod = 10,
    CloseHeight = 42,
    Test =
        fun(Closer, Slasher, SlashHeight) ->
            run(#{cfg => Cfg, lock_period => 10},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, 10),
                set_prop(height, CloseHeight),
                positive(fun close_solo_with_payload/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_from(Slasher),
                set_prop(round, 20),
                set_prop(height, SlashHeight),
                positive(fun slash_/2)])
        end,
    lists:foreach(
        fun(Closer) ->
            lists:foreach(
                fun(Slasher) ->
                    Test(Closer, Slasher, LockPeriod + CloseHeight),
                    Test(Closer, Slasher, LockPeriod + CloseHeight + 1),
                    Test(Closer, Slasher, LockPeriod + CloseHeight + 10)
                end,
                ?ROLES)
        end,
        ?ROLES),
    ok.

slash_by_delegate(Cfg) ->
    Height = 100,
    Test =
        fun(Closer, Round0, Round1) ->
            run(#{cfg => Cfg},
               [fun(Props) ->
                    {Delegate1, Delegate2, S} = create_loaded_accounts(100000 * aec_test_utils:min_gas_price(),
                                                                       100000 * aec_test_utils:min_gas_price()),
                    maps:merge(Props#{cfg => [{state, S} | Cfg]},
                               delegates_spec([aeser_id:create(account, Delegate1)],
                                              [aeser_id:create(account, Delegate2)],
                                              Height))
                end,
                set_prop(height, Height),
                positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, Round0),
                positive(fun close_solo_with_payload/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_prop(round, Round1),
                fun(#{delegate_ids := Ds, state := S} = Props) ->
                    D1 = pick_random_delegate(Ds),
                    D1Pubkey = aeser_id:specialize(D1, account),
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
                 positive(fun close_solo_with_payload/2),
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
                 positive(fun close_solo_with_payload/2),
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
                 positive(fun close_solo_with_payload/2),
                 fun(#{state := S0} = Props) ->
                    {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
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
                positive(fun close_solo_with_payload/2),
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
                positive(fun close_solo_with_payload/2),
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
                      height            := Height} = Props) ->
                    lists:foreach(
                        fun(PrivKeys) ->
                            PayloadSpec = create_payload_spec(Props),
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
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Closer, Slasher) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, 5),
                positive(fun close_solo_with_payload/2),
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
        fun(Closer, Slasher, CloseRound, SlashRound, Error) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, CloseRound),
                positive(fun close_solo_with_payload/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_from(Slasher),
                set_prop(round, SlashRound),
                negative(fun slash_/2, {error, Error})])
        end,
    [Test(Closer, Slasher, CloseRound, SlashRound, Error)
        || Closer <- ?ROLES,
           Slasher <- ?ROLES,
           {CloseRound, SlashRound, Error} <- [{5, 4, old_round},
                                               {5, 5, same_round}]],
    ok.

slash_no_payload(Cfg) ->
    CloseRound = 5,
    Test =
        fun(Closer, Slasher) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(round, CloseRound),
                positive(fun close_solo_with_payload/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % make sure the channel is not active any more
                    ClosedCh = aesc_test_utils:get_channel(ChannelPubKey, S),
                    false = aesc_channels:is_active(ClosedCh),
                    Props
                end,
                set_from(Slasher),
                set_prop(round, CloseRound),
                set_prop(payload, <<>>),
                negative(fun slash_/2, {error, slash_must_have_payload})])
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
    Fee = 50000 * aec_test_utils:min_gas_price(),
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
            Test(Depositor, 12, 50000 * aec_test_utils:min_gas_price(), insufficient_funds),
            Test(Depositor, 10, 50000 * aec_test_utils:min_gas_price(), insufficient_funds),
            Test(Depositor, 10, 0, too_low_fee)
        end,
        ?ROLES),
    ok.

deposit_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun deposit_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

deposit_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun deposit_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

deposit_closing(Cfg) ->
    test_both_closing_channel(Cfg, fun deposit_/2, #{amount => 1}).

deposit_closed(Cfg) ->
    test_both_already_closed(Cfg, fun deposit_/2, #{amount => 1}).

deposit_older_round(Cfg) ->
    test_both_old_round(Cfg, fun deposit_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}, old_round).

deposit_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun deposit_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

deposit_not_participant(Cfg) ->
    test_not_participant(Cfg, fun deposit_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

deposit_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun deposit_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

%%%===================================================================
%%% Withdraw
%%%===================================================================
withdraw(Cfg) ->
    Amount = 10,
    Fee = 50000 * aec_test_utils:min_gas_price(),
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
            Test(Withdrawer, 11, 50000 * aec_test_utils:min_gas_price(), not_enough_channel_funds),
            % keep at least 2*channel_reserve in the channel
            Test(Withdrawer, 9, 50000 * aec_test_utils:min_gas_price(), not_enough_channel_funds)
        end,
        ?ROLES),
    ok.

withdraw_wrong_nonce(Cfg) ->
    test_both_wrong_nonce(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

withdraw_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

withdraw_closing(Cfg) ->
    test_both_closing_channel(Cfg, fun withdraw_/2, #{amount => 1}).

withdraw_closed(Cfg) ->
    test_both_already_closed(Cfg, fun withdraw_/2, #{amount => 1}).

withdraw_older_round(Cfg) ->
    test_both_old_round(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}, old_round).

withdraw_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

withdraw_not_participant(Cfg) ->
    test_not_participant(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

withdraw_delegate_not_allowed(Cfg) ->
    test_delegate_not_allowed(Cfg, fun withdraw_/2, #{amount => 1, fee => 50000 * aec_test_utils:min_gas_price()}).

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
                positive(fun close_solo_with_payload/2),
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
                positive(fun close_solo_with_payload/2),
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
                positive(fun close_solo_with_payload/2),
                set_from(Settler),
                set_prop(initiator_amount, IAmt1),
                set_prop(responder_amount, RAmt1),
                set_prop(fee, Fee),
                set_prop(height, 21),
                negative(fun settle_/2, {error, Err})
                ])
        end,
    % settle tx amounts must be equal to the last on-chain tx
    CorrectFee = 50000 * aec_test_utils:min_gas_price(),
    ActualTest =
        fun(Closer, Setler) ->
            % someone has more
            Test(Closer, Setler, CloseAmtI + 1, CloseAmtR, CorrectFee, insufficient_channel_funds),
            Test(Closer, Setler, CloseAmtI, CloseAmtR + 1, CorrectFee, insufficient_channel_funds),
            % someone has less
            Test(Closer, Setler, CloseAmtI - 1, CloseAmtR, CorrectFee, wrong_amt),
            Test(Closer, Setler, CloseAmtI, CloseAmtR - 1, CorrectFee, wrong_amt),

            % fee
            Test(Closer, Setler, CloseAmtI, CloseAmtR, 0, too_low_fee)
        end,
    [ActualTest(Closer, Setler) ||  Closer <- ?ROLES,
                                    Setler <- ?ROLES],
    ok.

settle_missing_channel(Cfg) ->
    ChannelHashSize = aeser_api_encoder:byte_size_for_type(channel),
    FakeChannelPubKey = <<42:ChannelHashSize/unit:8>>,
    Test =
        fun(Closer, Settler) ->
            run(#{cfg => Cfg},
                [positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, 10),
                positive(fun close_solo_with_payload/2),
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
                positive(fun close_solo_with_payload/2),
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
            Test(Closer, Setler, Nonce - 1,  tx_nonce_already_used_for_account),
            Test(Closer, Setler, Nonce    ,  tx_nonce_already_used_for_account),
            Test(Closer, Setler, Nonce + 2,  tx_nonce_too_high_for_account)
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
                positive(fun close_solo_with_payload/2),
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
                positive(fun close_solo_with_payload/2),
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
                positive(fun close_solo_with_payload/2),
                set_prop(height, 21),
                fun(#{state := S0} = Props) ->
                    {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
                    PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
                    Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
                end,
                negative(fun settle_/2, {error, account_not_peer})])
        end,
    [Test(Closer) || Closer <- ?ROLES],
    ok.

settle_delegate_not_allowed(Cfg) ->
    Height = 100,
    Test =
        fun(Closer) ->
            run(#{cfg => Cfg},
              [fun(Props) ->
                    {Delegate1, Delegate2, S} = create_loaded_accounts(100000 * aec_test_utils:min_gas_price(),
                                                                       100000 * aec_test_utils:min_gas_price()),
                    #{delegate_ids := DelegateIds} =
                        delegates_spec([aeser_id:create(account, Delegate1)], %% initiator delegates
                                      [aeser_id:create(account, Delegate2)], %% responder delegates
                                      Height),
                    Props#{ cfg => [{state, S} | Cfg]
                          , delegate_ids => DelegateIds}
                end,
                positive(fun create_channel_/2),
                set_from(Closer),
                set_prop(height, Height - 11),
                positive(fun close_solo_with_payload/2),
                set_prop(height, Height),
                fun(#{delegate_ids := Ds, state := S} = Props) ->
                    D1 = pick_random_delegate(Ds), 
                    D1Pubkey = aeser_id:specialize(D1, account),
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
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
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
                set_prop(fee, 50000 * aec_test_utils:min_gas_price()),
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

%% no one can post a snapshot_tx to a closed channel
snapshot_closed_channel(Cfg) ->
    IAmt = 100000 * aec_test_utils:min_gas_price(),
    RAmt = 100000 * aec_test_utils:min_gas_price(),

    Test =
        fun(Snapshoter) ->
            run(#{cfg => Cfg, initiator_amount => IAmt, responder_amount => RAmt},
               [positive(fun create_channel_/2),
                set_from(initiator),
                set_prop(fee, 50000 * aec_test_utils:min_gas_price()),
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

%% no one can post a snapshot_tx to a closing channel, not even the one that
%% initiated the close
snapshot_closing_channel(Cfg) ->
    test_both_closing_channel(Cfg, fun snapshot_solo_/2).

%% snapshot_tx calls to a missing channel are rejected
snapshot_missing_channel(Cfg) ->
    test_both_missing_channel(Cfg, fun snapshot_solo_/2).

%% snapshot_tx calls with a payload from another channel are rejected
snapshot_payload_from_another_channel(Cfg) ->
    test_both_payload_from_different_channel(Cfg, fun snapshot_solo_/2).

%% no one can overwrite a state, not even the one that posted it
snapshot_payload_not_co_signed(Cfg) ->
    test_payload_not_both_signed(Cfg,
                                 fun(ChannelPubKey, FromPubKey, Payload, _PoI, S) ->
                                     aesc_test_utils:snapshot_solo_tx_spec(ChannelPubKey,
                                                                           FromPubKey,
                                                                           Payload,
                                                                           S)
                                  end,
                                  fun aesc_snapshot_solo_tx:new/1).

%% snapshot_tx calls with a payload from another channel are rejected
snapshot_old_payload(Cfg) ->
    test_both_old_round(Cfg, fun snapshot_solo_/2, #{}, old_round).

snapshot_can_not_replace_create(Cfg) ->
    test_both_can_not_replace_create(Cfg, fun snapshot_solo_/2).

snapshot_not_participant(Cfg) ->
    Height = 100,
    PreIris = aec_hard_forks:protocol_effective_at_height(Height) < ?IRIS_PROTOCOL_VSN,
    ErrorMsg =
        case PreIris of
            true -> account_not_peer;
            false -> account_not_peer_or_delegate
        end,
    test_not_participant(Cfg, fun snapshot_solo_/2, #{height => Height},
                         ErrorMsg).

snapshot_delegate_sometimes_allowed(Cfg) ->
    Height = 100,
    PreIris = aec_hard_forks:protocol_effective_at_height(Height) < ?IRIS_PROTOCOL_VSN,
    case PreIris of
        true ->
            test_delegate_not_allowed(Cfg, fun snapshot_solo_/2, #{height => Height});
        false ->
            test_delegate_allowed(Cfg, fun snapshot_solo_/2, #{height => Height})
    end,
    ok.

%%%===================================================================
%%% Force progress
%%%===================================================================

fp_after_create(Cfg) ->
    Round = 43,
    ContractRound = 10,
    AfterCreate =
        fun(Owner, Forcer, GasPrice, GasLimit) ->
            run(#{cfg => Cfg, initiator_amount => 10000000 * aec_test_utils:min_gas_price(),
                              responder_amount => 10000000 * aec_test_utils:min_gas_price(),
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
    Test(1 * aec_test_utils:min_gas_price(), 100000 * aec_test_utils:min_gas_price()),
    Test(2 * aec_test_utils:min_gas_price(), 100000 * aec_test_utils:min_gas_price()),
    Test(3 * aec_test_utils:min_gas_price(), 100000 * aec_test_utils:min_gas_price()),
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
    InitHeight = 100,
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
                positive(fun close_solo_with_payload/2),
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
            CloseHeight = 100,
            SlashHeight = 102,
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
                positive(fun close_solo_with_payload/2),
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

%% Test that slashing with co-signed off-chain state replaces
%% on-chain produced chain of unilaterally forced progress states
%%
%% unilaterally on-chain force progress state ROUND
%% unilaterally on-chain force progress state ROUND + 1
%% solo close using on-chain forced progress ROUND + 1
%% force progress on-chain state ROUND + 2
%% slash with co-signed off-chain state ROUND
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
            CloseHeight = 100,
            SlashHeight = 102,
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
                poi_participants_only(),
                positive(fun close_solo_without_payload/2),
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
    FPHeight0 = 100,
    Question = <<"To be, or not to be?">>,
    OQuestion = sophia_value(Question),
    Answer = <<"oh, yes">>,
    OResponse = sophia_value(Answer),
    QueryFee = 50000,
    CallOnChain =
        fun(Owner, Forcer) ->
            IAmt0 = 10000000 * aec_test_utils:min_gas_price(),
            RAmt0 = 10000000 * aec_test_utils:min_gas_price(),
            ContractCreateRound = 10,
            run(#{cfg => Cfg, initiator_amount => IAmt0,
                              responder_amount => RAmt0,
                  lock_period => LockPeriod,
                  channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(height, FPHeight0),

                % create oracle
                register_new_oracle(sophia_typerep(string), sophia_typerep(string), QueryFee),

                % create off-chain contract
                create_trees_if_not_present(),
                set_from(Owner, owner, owner_privkey),
                fun(#{oracle := Oracle} = Props) ->
                    EncodedOracleId = address_encode(oracle_pubkey, Oracle),
                    (create_contract_in_trees(_Round    = ContractCreateRound,
                                             _Contract = "channel_on_chain_contract_oracle",
                                             _InitArgs = [EncodedOracleId, quote(Question)],
                                             _Deposit  = 2))(Props)
                end,

                % place some calls to that contract
                force_call_contract_first(Forcer, <<"place_bet">>, [<<"\"Lorem\"">>],
                                          FPRound),
                force_call_contract(Forcer, <<"place_bet">>, [<<"\"Ipsum\"">>]),

                % try resolving a contract with wrong query id
                fun(Props) ->
                    EncodedQueryId = address_encode(oracle_query_id, <<1234:32/unit:8>>),
                    (force_call_contract(Forcer, <<"resolve">>, [EncodedQueryId]))(Props)
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

                fun(#{query_id := QueryId} = Props) ->
                    EncodedQueryId = address_encode(oracle_query_id, QueryId),
                    (force_call_contract(Forcer, <<"get_question">>, [EncodedQueryId]))(Props)
                end,
                assert_last_channel_result(Question, string),

                % there is currently no bet for the correct answer, try anyway
                fun(#{query_id := QueryId} = Props) ->
                    EncodedQueryId = address_encode(oracle_query_id, QueryId),
                    (force_call_contract(Forcer, <<"resolve">>, [EncodedQueryId]))(Props)
                end,
                assert_last_channel_result(<<"no winning bet">>, string),

                % place a winnning bet
                force_call_contract(Forcer, <<"place_bet">>, [quote(Answer)]),
                fun(#{query_id := QueryId} = Props) ->
                    EncodedQueryId = address_encode(oracle_query_id, QueryId),
                    (force_call_contract(Forcer, <<"resolve">>, [EncodedQueryId]))(Props)
                end,
                assert_last_channel_result(<<"ok">>, string),

                % verify that Oracle.get_question works
                fun(#{query_id := QueryId} = Props) ->
                    EncodedQueryId = address_encode(oracle_query_id, QueryId),
                    (force_call_contract(Forcer, <<"get_question">>, [EncodedQueryId]))(Props)
                end,
                assert_last_channel_result(Question, string),

                % verify that Oracle.query_fee works
                fun(#{query_id := _QueryId} = Props) ->
                    (force_call_contract(Forcer, <<"query_fee">>, []))(Props)
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
    FPHeight0 = 100,
    Name = aens_test_utils:fullname(<<"lorem">>),
    ForceCallCheckName =
        fun(Forcer, K, Found) when is_binary(K) andalso is_boolean(Found) ->
            fun(Props) ->
              run(Props,
                  [force_call_contract(Forcer, <<"can_resolve">>, [quote(Name), quote(K)]),
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
                                         _InitArgs = [],
                                         _Deposit  = 2),
                force_call_contract_first(Forcer, <<"can_resolve">>, [quote(Name), quote(<<"oracle">>)], FPRound),
                assert_last_channel_result(false, bool),
                register_name(Name,
                              [{<<"account_pubkey">>, aeser_id:create(account, <<1:256>>)},
                               {<<"oracle">>, aeser_id:create(oracle, <<2:256>>)},
                               {<<"unexpected_key">>, aeser_id:create(account, <<3:256>>)}]),
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
    FPHeight0 = 100,
    ForceCall =
        fun(Forcer, Fun, RType, R) ->
            fun(Props) ->
              run(Props,
                  [force_call_contract(Forcer, Fun, []),
                  assert_last_channel_result(R, RType)])
            end
        end,

    Height1 = 12345,
    Timestamp1 = 654321,
    BeneficiaryInt = 42,
    Beneficiary = <<BeneficiaryInt:?BENEFICIARY_PUB_BYTES/unit:8>>,
    EncAddress =
        fun(<<AddressInt:32/unit:8>> = Address) ->
            ?IF_AEVM(AddressInt, aeb_fate_data:make_address(Address))
        end,
    ExpBeneficiary = EncAddress(Beneficiary),

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
                                         _InitArgs = [],
                                         _Deposit  = 2),
                force_call_contract_first(Forcer, <<"block_height">>, [], FPRound),
                fun(#{height := H} = Props) ->
                    (assert_last_channel_result(H, word))(Props)
                end,
                set_tx_env(Height1, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"coinbase">>, word, ExpBeneficiary),
                set_tx_env(Height2, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"block_height">>, word, Height2),
                set_tx_env(Height3, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"coinbase">>, word, ExpBeneficiary),
                set_tx_env(Height4, Timestamp1, Beneficiary),
                ForceCall(Forcer, <<"timestamp">>, word, Timestamp1),
                set_from(Forcer),
                fun(#{ owner       := OwnerPubkey
                     , from_pubkey := ForcerPubkey} = Props) ->
                    ExpectedCaller  = EncAddress(ForcerPubkey),
                    ExpectedCreator = EncAddress(OwnerPubkey),
                    run(Props,
                        [ ForceCall(Forcer, <<"caller">>, word, ExpectedCaller)
                        , ForceCall(Forcer, <<"origin">>, word, ExpectedCaller)
                        , fun(#{height := H} = Props1) ->
                              case aec_hard_forks:protocol_effective_at_height(H) of
                                  PreF when PreF < ?FORTUNA_PROTOCOL_VSN -> %% no creator
                                      Props1;
                                  _PostFortuna ->
                                      (ForceCall(Forcer, <<"creator">>, word,
                                                 ExpectedCreator))(Props1)
                              end
                          end
                        ])
                end
               ])
        end,
    [CallOnChain(Owner, Forcer) || Owner  <- ?ROLES,
                                   Forcer <- ?ROLES],
    ok.

fp_use_remote_call(Cfg) ->
    FPRound = 20,
    LockPeriod = 10,
    FPHeight0 = 100,
    RemoteCall =
        fun(Forcer, Int) when is_integer(Int) ->
            fun(Props) ->
                Bin = integer_to_binary(Int),
                RemoteContract = maps:get(remote_contract, Props),
                Address = address_encode(contract_pubkey, RemoteContract),
                Args = [Address, Bin],
                (force_call_contract(Forcer, <<"call">>, Args))(Props)
            end
        end,

    Save = fun(Key1, Key2) -> rename_prop(Key1, Key2, keep_old) end,
    Load = fun(Key1, Key2) -> Save(Key2, Key1) end,
    SaveContract = fun(Key) ->
                       [Save(contract_id, Key), Save(contract_file, {Key, file})]
                   end,
    LoadContract = fun(Key) ->
                       [Load(contract_id, Key), Load(contract_file, {Key, file})]
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
                                         _Contract = identity,
                                         _InitArgs = [],
                                         _Deposit  = 2),
                SaveContract(remote_contract),
                fun(#{contract_id := RemoteContract} = Props) ->
                    Props#{remote_contract => RemoteContract}
                end,
                % create the second contract
                create_contract_in_trees(_Round1    = ContractCreateRound + 10,
                                         _Contract2 = remote_call,
                                         _InitArgs2 = [],
                                         _Deposit2  = 2),
                SaveContract(second_contract),
                LoadContract(remote_contract),
                force_call_contract_first(Forcer, <<"main_">>, [<<"42">>],
                                          FPRound),
                assert_last_channel_result(42, word),% it works

                LoadContract(second_contract),
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
    Height =  100,
    RemoteCall =
        fun(Forcer, ContractHandle) ->
            fun(Props) ->
                RemoteContract = maps:get(ContractHandle, Props),
                Address = address_encode(contract_pubkey, RemoteContract),
                Args = [Address],
                run(Props,
                    [ force_call_contract(Forcer, <<"increment">>, Args),
                      force_call_contract(Forcer, <<"get">>, Args)
                    ])
            end
        end,
    NameKey =
        fun(Key) ->
            list_to_atom(atom_to_list(Key) ++ "_name")
        end,
    PushContractId =
        fun(Key) ->
            fun(P) ->
                Key2 = NameKey(Key),
                run(P,
                    [rename_prop(contract_id, Key, keep_old),
                     rename_prop(contract_file, Key2, keep_old)])
            end
        end,
    PopContractId =
        fun(Key) ->
            fun(P) ->
                Key2 = NameKey(Key),
                run(P,
                    [rename_prop(Key, contract_id, keep_old),
                     rename_prop(Key2, contract_file, keep_old)])
            end
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
                set_prop(height, Height),

                % create off-chain contract that is going to be used in the
                % remote call later
                create_trees_if_not_present(),
                set_from(Owner, owner, owner_privkey),
                create_contract_in_trees(_Round    = ContractCreateRound,
                                         _Contract = counter,
                                         _InitArgs = [<<"42">>],
                                         _Deposit  = 2),
                PushContractId(remote_contract),
                fun(#{contract_id := RemoteContract} = Props) ->
                    Props#{remote_contract => RemoteContract}
                end,
                % create the second contract
                create_contract_in_trees(_Round1    = ContractCreateRound + 10,
                                         _Contract2 = remote_call,
                                         _InitArgs2 = [],
                                         _Deposit2  = 2),
                PushContractId(second_contract),
                create_contract_in_onchain_trees(_OnchainContract = counter,
                                                 _OnchainCInitArgs = [<<"42">>],
                                                 _OnchainDeposit  = 2),
                PushContractId(onchain_contract),
                PopContractId(remote_contract),
                force_call_contract_first(Forcer, <<"tick">>, [],
                                          FPRound),
                force_call_contract(Forcer, <<"get">>, []),
                assert_last_channel_result(43, word),% it works

                PopContractId(second_contract),
                set_prop(call_deposit, 2),
                RemoteCall(Forcer, remote_contract),
                assert_last_channel_result(44, word), % it works remote
                fun(Props) ->
                    RemoteContract = maps:get(onchain_contract, Props),
                    Address = address_encode(contract_pubkey, RemoteContract),
                    Args = [Address],
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


%% no one can post a force progress to a closed channel
fp_closed_channel(Cfg) ->
    Round = 10,
    IStartAmt = 200000 * aec_test_utils:min_gas_price(),
    RStartAmt = 200000 * aec_test_utils:min_gas_price(),

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
                negative_force_progress_sequence(Cfg, Round, Forcer, channel_does_not_exist)])
        end,
    [Test(Owner, Forcer) || Owner <- ?ROLES,
                            Forcer<- ?ROLES],
    ok.

fp_not_participant(Cfg) ->
    Round = 10,
    Height = 100,
    PreIris = aec_hard_forks:protocol_effective_at_height(Height) < ?IRIS_PROTOCOL_VSN,
    NonEmptyPayload = proplists:get_value(force_progress_use_payload, Cfg, true),
    Err =
        case PreIris of
            true -> account_not_peer;
            false when NonEmptyPayload =:= true -> account_not_peer_or_delegate;
            false when NonEmptyPayload =:= false -> account_not_peer
        end,
    Test =
        fun(Owner) ->
            run(#{cfg => Cfg, height => Height},
               [positive(fun create_channel_/2),
                fun(#{state := S0} = Props) ->
                    {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
                     PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
                    Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
                end,
                %% workaround for creating a new account in the off-chain
                %% trees so it can be used for calling the contract later on
                fun(#{ initiator_amount  := IAmt
                     , responder_amount  := RAmt
                     , initiator_pubkey  := IPubkey
                     , responder_pubkey  := RPubkey
                     , from_pubkey       := NotParticipant} = Props) ->
                    Accounts = [aec_accounts:new(Pubkey, Balance) ||
                            {Pubkey, Balance} <- [{IPubkey, IAmt - 10},
                                                  {RPubkey, RAmt},
                                                  {NotParticipant, 10}
                                                ]],
                    Trees = aec_test_utils:create_state_tree_with_accounts(Accounts, no_backend),
                    Props#{trees => Trees}
                end,
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                get_onchain_balances(before_force),
                set_prop(round, Round),
                fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                    (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                  [<<"42">>], 1))(Props)
                end,
                set_prop(fee, 100000 * aec_test_utils:min_gas_price()),
                fun(Props) ->
                    case NonEmptyPayload of
                        true ->
                            Props;
                        false ->
                            run(Props,
                                [ rename_prop(from_pubkey, np_pubkey, keep_old),
                                  rename_prop(from_privkey, np_privkey, keep_old),
                                  set_from(initiator),
                                  positive(fun snapshot_solo_/2),
                                  set_prop(payload, <<>>),
                                  set_prop(round, Round + 1),
                                  rename_prop(np_pubkey, from_pubkey, keep_old),
                                  rename_prop(np_privkey, from_privkey, keep_old)
                                ])
                    end
                end,
                negative(fun force_progress_/2, {error, Err})])
        end,
    [Test(Owner) || Owner <- ?ROLES],
    ok.

fp_missing_channel(Cfg) ->
    ChannelHashSize = aeser_api_encoder:byte_size_for_type(channel),
    FakeChannelId = <<42:ChannelHashSize/unit:8>>,
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(channel_pubkey, FakeChannelId),
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                negative_force_progress_sequence(Cfg, Round, Forcer, channel_does_not_exist)])
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
                                         _Contract = identity,
                                         _InitArgs = [],
                                         _Deposit  = 2),
                % use the payload of channelA in a force progress in channelB
                fun(#{different_payload := Payload} = Props) ->
                    Props#{payload => Payload}
                end,
                set_from(initiator),
                negative_force_progress_sequence(Cfg, Round, Forcer,
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
                create_contract_poi_and_payload(Round - 1, 5, Owner),
                fun(#{payload := PayloadBin} = Props) ->
                    Payload = aetx_sign:deserialize_from_binary(PayloadBin),
                    [OneSig | _] = aetx_sign:signatures(Payload),
                    Tx = aetx_sign:tx(Payload),
                    Payload1 = aetx_sign:serialize_to_binary(
                                  aetx_sign:new(Tx, [OneSig])),
                    Props#{payload => Payload1}
                end,
                negative_force_progress_sequence(Cfg, Round, Forcer,
                                                 signature_check_failed)])
        end,
    [Test(Owner, Forcer) || Owner <- ?ROLES,
                            Forcer<- ?ROLES],
    ok.

fp_payload_older_payload(Cfg) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
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
                negative_force_progress_sequence(Cfg, Round, Forcer,
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
                negative_force_progress_sequence(Cfg, Round, Forcer,
                                                 same_round)])
        end,
    [Test(Owner, Forcer) ||  Owner       <- ?ROLES,
                             Forcer      <- ?ROLES],
    ok.

fp_payload_invalid_state_hash(Cfg) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Round = 10,
    Test =
        fun(Owner, Forcer) ->
            run(#{cfg => Cfg},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(Round - 1, 5, Owner,
                                                #{fake_hash => FakeStateHash}),
                negative_force_progress_sequence(Cfg, Round, Forcer,
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
                negative_force_progress_sequence(Cfg, SoloPayloadRound, Forcer,
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
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
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
                fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                    (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                  [<<"42">>], 1))(Props)
                end,
                set_prop(fee, 100000 * aec_test_utils:min_gas_price()),
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
        fun(Owner, _Forcer) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_from(initiator),
                set_prop(round, CloseRound),
                positive(fun close_solo_with_payload/2),
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
                fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                    (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                  [<<"42">>], 1))(Props)
                end,
                set_prop(fee, 100000 * aec_test_utils:min_gas_price()),
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
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
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
    AccountHashSize = aeser_api_encoder:byte_size_for_type(account_pubkey),
    Fake1Id = aeser_id:create(account, <<42:AccountHashSize/unit:8>>),
    Fake2Id = aeser_id:create(account, <<43:AccountHashSize/unit:8>>),

    Transfer = aesc_offchain_update:op_transfer(Fake1Id, Fake2Id, 10),
    Deposit = aesc_offchain_update:op_deposit(Fake1Id, 10),
    Withdraw = aesc_offchain_update:op_withdraw(Fake1Id, 10),
    NewContract = aesc_offchain_update:op_new_contract(Fake1Id, 1, 1,
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
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
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
                fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                    (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                  [<<"42">>], 1))(Props)
                end,
                set_prop(fee, 100000 * aec_test_utils:min_gas_price()),
                negative(fun force_progress_/2, {error, Error})])
        end,
    [Test(Owner, Forcer) || Owner  <- ?ROLES,
                            Forcer <- ?ROLES].

fp_solo_payload_broken_call(Cfg) ->
    Round = 43,
    ContractRound = 10,
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    FakeStateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Owner, Forcer, CallData, AEVMError, FATEError) ->
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
                                aeser_id:create(account, From),
                                aeser_id:create(contract, ContractId),
                                aect_test_utils:abi_version(), 1,
                                CallData,
                                [],
                                _GasPrice = aec_test_utils:min_gas_price(),
                                _GasLimit = 10000000),
                    Props#{solo_payload_update => Update}
                end,
                set_prop(fake_solo_state_hash, FakeStateHash),
                fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                    (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                  [<<"42">>], 1))(Props)
                end,
                set_prop(fee, 100000 * aec_test_utils:min_gas_price()),
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
                    Ret = aect_call:return_value(Call),
                    ?assertMatchVM(AEVMError, FATEError, Ret),
                    Props
                end])
        end,
    TestWithCallData =
        fun(CallData, ErrorAEVM, ErrorFATE) ->
            [Test(Owner, Forcer, CallData, ErrorAEVM, ErrorFATE)
             || Owner  <- ?ROLES,
                Forcer <- ?ROLES]
        end,
    %% empty call data
    TestWithCallData(<<>>, <<"bad_call_data">>, <<"bad_call_data">>),
    %% Too small call data
    TestWithCallData(<<"0xABCD">>, <<"bad_call_data">>, <<"bad_call_data">>),
    %% Just plain wrong call data, but that can be interpreted by the aevm
    TestWithCallData(<<42:42/unit:8>>, <<"unknown_function">>, <<"bad_call_data">>),
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
                set_prop(gas_price, GasPrice),
                set_prop(gas_limit, GasLimit),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                set_from(Forcer),
                maybe_snapshot(Cfg, Round - 1),
                fun(#{state := S0, from_pubkey := From} = Props) ->
                    S = aesc_test_utils:set_account_balance(From, TotalBalance, S0),
                    Props#{state => S}
                end,
                negative_force_progress_sequence(Cfg, Round, Forcer,
                                                 insufficient_funds)])
        end,
    Test =
        fun(GasPrice, GasLimit, TotalBalance) ->
            [T(Owner, Forcer, GasPrice, GasLimit, TotalBalance)
                || Owner  <- ?ROLES,
                   Forcer <- ?ROLES]
        end,
    Test(aec_test_utils:min_gas_price(), 1001, 1000),
    Test(2 * aec_test_utils:min_gas_price(), 500,  999),
    ok.

fp_insufficent_gas_price(Cfg) ->
    Round = 43,
    ContractRound = 10,
    Height = 1234,
    T =
        fun(Owner, Forcer, GasPrice, GasLimit) ->
            run(#{cfg => Cfg, initiator_amount => 30,
                              responder_amount => 30,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(gas_price, GasPrice),
                set_prop(gas_limit, GasLimit),
                set_prop(height, Height),
                create_contract_poi_and_payload(Round - 1, ContractRound, Owner),
                negative_force_progress_sequence(Cfg, Round, Forcer,
                                                 too_low_gas_price)])
        end,
    Test =
        fun(GasPrice, GasLimit) ->
            [T(Owner, Forcer, GasPrice, GasLimit)
                || Owner  <- ?ROLES,
                   Forcer <- ?ROLES]
        end,
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    TooLowGasPrice = aec_governance:minimum_gas_price(Protocol) - 1,
    Test(TooLowGasPrice, 1001),
    ok.

fp_register_name(Cfg) ->
    %% protocol version in config is latest version w.r.t. 'make' target
    Name = aens_test_utils:fullname(<<"bla">>),
    Salt = 42,
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash           = address_encode(hash, aens_hash:commitment_hash(NameAscii, Salt)),
    ?TEST_LOG("Commitment hash ~p", [aens_hash:commitment_hash(NameAscii,
                                                               Salt)]),
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    StateHash = <<42:StateHashSize/unit:8>>,
    Round = 42,
    FPRound = Round + 10,
    SignContractAddress =
        fun(PubK, PrivK, ConId) ->
            BinToSign = <<PubK/binary, ConId/binary>>,
            SigBin = enacl:sign_detached(aec_governance:add_network_id(BinToSign), PrivK),
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            address_encode(hash, SigBin)
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
              {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
              PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
              ?TEST_LOG("Owner: pubkey ~p, privkey: ~p", [NewAcc, PrivKey]),
              Props#{state => S1, onchain_contract_owner_pubkey => NewAcc,
                                  onchain_contract_owner_privkey => PrivKey}
          end,
          % create contract on-chain
          fun(#{onchain_contract_owner_pubkey := PubKey,
                state := S0} = Props) ->
            {ok, BinCode} =  compile_contract(ContractName),
            {ok, CallData} = encode_call_data(ContractName, <<"init">>, []),
            Nonce = 1,
            {ok, ContractCreateTx} =
                aect_create_tx:new(
                    #{owner_id    => aeser_id:create(account, PubKey),
                      nonce       => Nonce,
                      code        => BinCode,
                      vm_version  => aect_test_utils:vm_version(),
                      abi_version => aect_test_utils:abi_version(),
                      deposit     => 1,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => aec_test_utils:min_gas_price(),
                      call_data   => CallData,
                      fee         => 1000000 * aec_test_utils:min_gas_price()}),
            ?TEST_LOG("Contract create tx ~p", [ContractCreateTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 3}),
            {ok, OnChainTrees1, _} = aetx:process(ContractCreateTx,
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
                code := _Code,
                state := S0} = Props) ->
            Nonce = 2,
            Sig = SignContractAddress(OPubKey, OPrivKey, ContractId),
            NameOwner = address_encode(account_pubkey, OPubKey),
            PreclaimArgs = [NameOwner, CHash, Sig],
            ?TEST_LOG("Preclaim function arguments ~p", [PreclaimArgs]),
            {ok, CallData} = encode_call_data(ContractName, <<"signedPreclaim">>,
                                              PreclaimArgs),
            ?TEST_LOG("CallData ~p", [CallData]),
            true = is_binary(CallData),
            {ok, CallTx} =
                aect_call_tx:new(
                    #{caller_id   => aeser_id:create(account, OPubKey),
                      nonce       => Nonce,
                      contract_id => aeser_id:create(contract,
                                                    ContractId),
                      abi_version => aect_test_utils:abi_version(),
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => aec_test_utils:min_gas_price(),
                      call_data   => CallData,
                      fee         => 500000 * aec_test_utils:min_gas_price()}),
            ?TEST_LOG("Contract call tx ~p", [CallTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 4}),
            {ok, OnChainTrees1, _} = aetx:process(CallTx,
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
                                          _InitArgs = [],
                                          _Deposit  = 2),
                  % force progress contract on-chain
                  fun(#{contract_id := ContractId,
                        from_pubkey := Pubkey,
                        from_privkey := Privkey} = Props) ->
                      Sig = SignContractAddress(Pubkey, Privkey, ContractId),
                      Account = address_encode(account_pubkey, Pubkey),
                      PreclaimArgs = [Account, CHash, Sig],
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
                      ErrorRes = aect_call:return_value(Call),
                      MatchRes = re:run(ErrorRes, <<"not.allowed.off.chain">>),
                      ?assertMatch({ErrorRes, {match, _}}, {ErrorRes, MatchRes}),
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
            CloseHeight = 100,
            SlashHeight = 102,
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
                positive(fun close_solo_with_payload/2),
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
    ProduceCallData =
        fun(_Pubkey, _Privkey, Oracle, _OraclePrivkey, _QueryId, _ContractId, QueryFee) ->
            Args = [binary_to_list(aeser_api_encoder:encode(oracle_pubkey, Oracle)),
                    "\"Very much of a question\"", integer_to_list(QueryFee),
                    "RelativeTTL(10)", "RelativeTTL(10)"],
            ?TEST_LOG("Oracle createQuery function arguments ~p", [Args]),
            encode_call_data(oracles, "createQuery", Args)
        end,
    fp_oracle_action(Cfg, ProduceCallData).

%% test that a force progress transaction can NOT respond an oracle
%% query via a contract
fp_oracle_respond(Cfg) ->
    ProduceCallData =
        fun(_Pubkey, _Privkey, Oracle, OraclePrivkey, QueryId, ContractId, _QueryFee) ->
            ?TEST_LOG("Oracle ~p", [Oracle]),
            ?TEST_LOG("QueryId ~p", [QueryId]),
            BinToSign = <<QueryId/binary, ContractId/binary>>,
            SigBin = enacl:sign_detached(aec_governance:add_network_id(BinToSign), OraclePrivkey),
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            Args = [binary_to_list(aeser_api_encoder:encode(oracle_pubkey, Oracle)),
                    binary_to_list(aeser_api_encoder:encode(oracle_query_id, QueryId)),
                    encode_sig(SigBin), "42"],
            ?TEST_LOG("Oracle respond function arguments ~p", [Args]),
            encode_call_data(oracles, "signedRespond", Args)
        end,
    fp_oracle_action(Cfg, ProduceCallData).

%% test that a force progress transaction can NOT extend an oracle
%% via a contract
fp_oracle_extend(Cfg) ->
    ProduceCallData =
        fun(_Pubkey, _Privkey, Oracle, OraclePrivkey, _QueryId, ContractId, _QueryFee) ->
            BinToSign = <<Oracle/binary, ContractId/binary>>,
            SigBin = enacl:sign_detached(aec_governance:add_network_id(BinToSign), OraclePrivkey),
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            Args = [binary_to_list(aeser_api_encoder:encode(oracle_pubkey, Oracle)),
                    encode_sig(SigBin), "RelativeTTL(10)"],
            ?TEST_LOG("Oracle respond function arguments ~p", [Args]),
            encode_call_data(oracles, "signedExtendOracle", Args)
        end,
    fp_oracle_action(Cfg, ProduceCallData).

fp_oracle_action(Cfg, ProduceCallData) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
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
              {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
              PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
              ?TEST_LOG("Owner: pubkey ~p, privkey: ~p", [NewAcc, PrivKey]),
              Props#{state => S1, onchain_contract_owner_pubkey => NewAcc,
                                  onchain_contract_owner_privkey => PrivKey}
          end,
          % create contract on-chain
          fun(#{onchain_contract_owner_pubkey := PubKey,
                state := S0} = Props) ->
            {ok, BinCode} = compile_contract(ContractName),
            {ok, CallData} = encode_call_data(ContractName, <<"init">>, []),
            Nonce = 1,
            {ok, ContractCreateTx} =
                aect_create_tx:new(
                    #{owner_id    => aeser_id:create(account, PubKey),
                      nonce       => Nonce,
                      code        => BinCode,
                      vm_version  => aect_test_utils:vm_version(),
                      abi_version => aect_test_utils:abi_version(),
                      deposit     => 1,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => aec_test_utils:min_gas_price(),
                      call_data   => CallData,
                      fee         => 1000000 * aec_test_utils:min_gas_price()}),
            ?TEST_LOG("Contract create tx ~p", [ContractCreateTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 3}),
            {ok, OnChainTrees1,_} = aetx:process(ContractCreateTx,
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
          register_new_oracle(sophia_typerep(string), sophia_typerep(integer), QueryFee),
          oracle_query(sophia_value(<<"Some question">>), 10),
          % call contract on-chain
          fun(#{onchain_contract_owner_pubkey := OPubKey,
                onchain_contract_owner_privkey := OPrivKey,
                onchain_contract_id := ContractId,
                oracle := Oracle,
                state := S0,
                query_id := QueryID} = Props) ->
            Nonce = 2,
            OraclePrivkey = aesc_test_utils:priv_key(Oracle, S0),
            {ok, CallData} = ProduceCallData(OPubKey, OPrivKey, Oracle,
                                             OraclePrivkey, QueryID, ContractId, QueryFee),
            ?TEST_LOG("CallData ~p", [CallData]),
            true = is_binary(CallData),
            {ok, CallTx} =
                aect_call_tx:new(
                    #{caller_id   => aeser_id:create(account, OPubKey),
                      nonce       => Nonce,
                      contract_id => aeser_id:create(contract,
                                                    ContractId),
                      abi_version => aect_test_utils:abi_version(),
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => aec_test_utils:min_gas_price(),
                      call_data   => CallData,
                      fee         => 500000 * aec_test_utils:min_gas_price()}),
            ?TEST_LOG("Contract call tx ~p", [CallTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 4}),
            {ok, OnChainTrees1,_} = aetx:process(CallTx,
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
                  register_new_oracle(sophia_typerep(string), sophia_typerep(integer), QueryFee),
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
                                          _InitArgs = [],
                                          _Deposit  = 2),
                  oracle_query(sophia_value(<<"Some question">>), 10),
                  % force progress contract on-chain
                  fun(#{contract_id   := ContractId,
                        oracle        := Oracle,
                        from_pubkey   := Pubkey,
                        from_privkey  := Privkey,
                        state         := S,
                        query_id      := QueryID} = Props) ->
                      OraclePrivkey = aesc_test_utils:priv_key(Oracle, S),
                      {ok, CallData} = ProduceCallData(Pubkey, Privkey, Oracle, OraclePrivkey,
                                                       QueryID, ContractId, QueryFee),
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
                      ErrorRes = aect_call:return_value(Call),
                      MatchRes = re:run(ErrorRes, <<"not.allowed.off.chain">>),
                      ?assertMatch({ErrorRes, {match, _}}, {ErrorRes, MatchRes}),
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

fp_register_oracle(Cfg) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    StateHash = <<42:StateHashSize/unit:8>>,
    Round = 42,
    FPRound = Round + 10,
    SignAddress =
        fun(Oracle, PrivK, ContractId) ->
            BinToSign = <<Oracle/binary, ContractId/binary>>,
            SigBin = enacl:sign_detached(aec_governance:add_network_id(BinToSign), PrivK),
            ?TEST_LOG("Signature binary ~p", [SigBin]),
            encode_sig(SigBin)
        end,

    RegisterCallData =
        fun(OPubKey, Sig) ->
            encode_call_data(oracles, "signedRegisterOracle",
                             [binary_to_list(aeser_api_encoder:encode(account_pubkey, OPubKey)),
                              Sig, "2", "RelativeTTL(10)"])
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
              {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
              PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
              ?TEST_LOG("Owner: pubkey ~p, privkey: ~p", [NewAcc, PrivKey]),
              Props#{state => S1, onchain_contract_owner_pubkey => NewAcc,
                                  onchain_contract_owner_privkey => PrivKey}
          end,
          % create contract on-chain
          fun(#{onchain_contract_owner_pubkey := PubKey,
                state := S0} = Props) ->
            {ok, BinCode} = compile_contract(ContractName),
            {ok, CallData} = encode_call_data(ContractName, <<"init">>, []),
            Nonce = 1,
            {ok, ContractCreateTx} =
                aect_create_tx:new(
                    #{owner_id    => aeser_id:create(account, PubKey),
                      nonce       => Nonce,
                      code        => BinCode,
                      vm_version  => aect_test_utils:vm_version(),
                      abi_version => aect_test_utils:abi_version(),
                      deposit     => 1,
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => aec_test_utils:min_gas_price(),
                      call_data   => CallData,
                      fee         => 1000000 * aec_test_utils:min_gas_price()}),
            ?TEST_LOG("Contract create tx ~p", [ContractCreateTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 3}),
            {ok, OnChainTrees1,_} = aetx:process(ContractCreateTx,
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
            {ok, CallData} = RegisterCallData(OPubKey, Sig),
            ?TEST_LOG("CallData ~p", [CallData]),
            true = is_binary(CallData),
            {ok, CallTx} =
                aect_call_tx:new(
                    #{caller_id   => aeser_id:create(account, OPubKey),
                      nonce       => Nonce,
                      contract_id => aeser_id:create(contract,
                                                    ContractId),
                      abi_version => aect_test_utils:abi_version(),
                      amount      => 1,
                      gas         => 123456,
                      gas_price   => aec_test_utils:min_gas_price(),
                      call_data   => CallData,
                      fee         => 600000 * aec_test_utils:min_gas_price()}),
            ?TEST_LOG("Contract call tx ~p", [CallTx]),
            OnChainTrees = aesc_test_utils:trees(S0),
            TxEnv = tx_env(#{height => 4}),
            {ok, OnChainTrees1,_} = aetx:process(CallTx,
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
                                          _InitArgs = [],
                                          _Deposit  = 2),
                  % force progress contract on-chain
                  fun(#{contract_id := ContractId,
                        from_pubkey := Pubkey,
                        from_privkey := Privkey} = Props) ->
                      Sig = SignAddress(Pubkey, Privkey, ContractId),
                      {ok, CallData} = RegisterCallData(Pubkey, Sig),
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
                      ErrorRes = aect_call:return_value(Call),
                      MatchRes = re:run(ErrorRes, <<"not.allowed.off.chain">>),
                      ?assertMatch({ErrorRes, {match, _}}, {ErrorRes, MatchRes}),
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
            maps:get(contract_name, Props0, {identity, []}),
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

maybe_snapshot(Cfg, Round) ->
    fun(Props) ->
        case proplists:get_value(force_progress_use_payload, Cfg, true) of
            true ->
                Props;
            false ->
                run(Props,
                    [ set_prop(round, Round),
                      fun(#{channel_pubkey := ChannelPubKey, state := S} = Props1) ->
                          % make sure the channel is not active any more
                          Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                          IsChannelActive = aesc_channels:is_active(Channel),
                          ?TEST_LOG("The channel is active: ~p", [IsChannelActive]),
                          case IsChannelActive of
                              true -> (positive(fun snapshot_solo_/2))(Props1);
                              false -> (positive(fun slash_/2))(Props1)
                          end
                      end])
        end
    end.

negative_force_progress_sequence(Cfg, Round, Forcer, {negative, ErrMsg}) ->
    negative_force_progress_sequence(Cfg, Round, Forcer, ErrMsg);
negative_force_progress_sequence(Cfg, Round, Forcer, ErrMsg) ->
    Fee = 500000 * aec_test_utils:min_gas_price(),
    SetIfNotPresent =
        fun(Key, DefaultValue) ->
            fun(Props) ->
                case maps:is_key(Key, Props) of
                    true -> Props;
                    false -> maps:put(Key, DefaultValue, Props)
                end
            end
        end,
    fun(Props0) ->
        DepositAmt = maps:get(call_deposit, Props0, 1),
        run(Props0,
           [get_onchain_balances(before_force),
            set_from(Forcer),
            set_prop(round, Round),
            fun(#{ contract_id := ContractId
                  , contract_file := CName} = Props) ->
                (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                              [<<"42">>], DepositAmt))(Props)
            end,
            fun(Props) ->
                case proplists:get_value(force_progress_use_payload, Cfg, true) of
                    true ->
                        Props;
                    false ->
                        (set_prop(payload, <<>>))(Props)
                end
            end,
            SetIfNotPresent(fee, Fee),
            SetIfNotPresent(height, 42),
            negative(fun force_progress_/2, {error, ErrMsg}),
            fun(#{signed_force_progress := SignedForceProgressTx}) -> % revert changes, espl in off-chain trees
                Props0#{signed_force_progress => SignedForceProgressTx}
            end])
      end.

force_progress_sequence(Round, Forcer) ->
    Fee = 300000 * aec_test_utils:min_gas_price(),
    fun(Props0) ->
        DepositAmt = maps:get(call_deposit, Props0, 1),
        {FunName, FunParams} = maps:get(contract_function_call, Props0,
                                        {<<"main_">>, [<<"42">>]}),
        run(Props0,
           [get_onchain_balances(before_force),
            fun(#{state_hash := StateHash, offchain_trees := OffChainTrees} = Props) ->
                ?assertEqual(StateHash, aec_trees:hash(OffChainTrees)),
                Props
            end,
            set_from(Forcer),
            set_prop(round, Round),
            fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                (create_contract_call_payload(ContractId, CName, FunName,
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

set_from(do_not_change_from) ->
    fun(Props) -> Props end;
set_from(Role) when Role =:= initiator; Role =:= responder ->
    set_from(Role, from_pubkey, from_privkey).

set_from(Role, PubkeyKey, PrivkeyKey) when Role =:= initiator; Role =:= responder ->
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
        Fee = maps:get(fee, Props, 50000 * aec_test_utils:min_gas_price()),
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

reuse_or_create_payload(Key, Props) ->
    case maps:get(Key, Props, not_specified) of
        not_specified ->
            CreateFun = create_payload(Key),
            Props1 = CreateFun(Props),
            maps:get(Key, Props1);
        Payload -> Payload
    end.

create_payload(Key) ->
    fun(#{channel_pubkey    := ChannelPubKey,
          initiator_pubkey  := IPubkey,
          responder_pubkey  := RPubkey,
          initiator_privkey := IPrivkey,
          responder_privkey := RPrivkey} = Props) ->

        PayloadSpec = create_payload_spec(Props),
        Payload = aesc_test_utils:payload(ChannelPubKey, IPubkey, RPubkey,
                                        [IPrivkey, RPrivkey], PayloadSpec),
        Props#{Key => Payload}
    end.

create_payload_spec(#{initiator_amount  := IAmt,
                      responder_amount  := RAmt} = Props) ->
        PayloadSpec0 = #{initiator_amount => IAmt,
                        responder_amount  => RAmt,
                        round => maps:get(round, Props, 11)},
        Protocol =
            case maps:get(height, Props, use_no_updates_vsn) of
                use_no_updates_vsn ->
                    aect_test_utils:latest_protocol_version();
                ChainHeight ->
                    aec_hard_forks:protocol_effective_at_height(ChainHeight)
            end,
        PayloadSpec01 =
            case Protocol of
                _P when _P >= ?FORTUNA_PROTOCOL_VSN -> %% no updates
                    PayloadSpec0;
                _ ->
                    PayloadSpec0#{updates => []} %% this is some updates
            end,
        lists:foldl(
            fun({PropsKey, OffChainKey}, Accum) ->
                case maps:get(PropsKey, Props, none) of
                    none -> Accum;
                    V -> maps:put(OffChainKey, V, Accum)
                end
            end,
            PayloadSpec01,
            [{state_hash, state_hash}]).

create_contract_call_payload(ContractId, ContractName, Fun, Args, Amount) ->
    create_contract_call_payload(solo_payload, ContractId, ContractName, Fun, Args, Amount).

create_contract_call_payload(Key, ContractId, ContractName, Fun, Args, Amount) ->
    fun(Props) ->
        {ok, CallData} = encode_call_data(ContractName, Fun, Args),
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
        OffChainUpdateFrom = maps:get(offchain_update_from, Props, From),
        Reserve = maps:get(channel_reserve, Props, 0),
        OnChainTrees = aesc_test_utils:trees(State),
        Env = tx_env(Props),
        Update =
            maps:get(solo_payload_update, Props,
                aesc_offchain_update:op_call_contract(
                    aeser_id:create(account, OffChainUpdateFrom),
                    aeser_id:create(contract, ContractId),
                    aect_test_utils:abi_version(), Amount, CallData,
                    [],
                    _GasPrice = maps:get(gas_price, Props, aec_test_utils:min_gas_price()),
                    _GasLimit = maps:get(gas_limit, Props, 10000000))),
        {UpdatedTrees, StateHash} =
            case maps:get(fake_solo_state_hash, Props, none) of
                none ->
                    Trees1 = aesc_offchain_update:apply_on_trees(Update,
                                                                 aect_call_state_tree:prune_without_backend(Trees0),
                                                                 OnChainTrees, Env, Round, Reserve),
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
    fun(#{owner := Owner} = Props) ->
        {ok, BinCode}  =
            case maps:get(compiler_fun, Props, not_set) of
                not_set -> compile_contract(ContractName);
                Compiler when is_function(Compiler)-> Compiler(ContractName)
            end,
        {ok, CallData} = encode_call_data(ContractName, <<"init">>, InitArg),
        VmVersion  = maps:get(vm_version, Props, aect_test_utils:vm_version()),
        ABIVersion = maps:get(abi_version, Props, aect_test_utils:abi_version()),
        Update = aesc_offchain_update:op_new_contract(aeser_id:create(account, Owner),
                                                      VmVersion, ABIVersion, BinCode,
                                                      Deposit, CallData),
        Props1 = apply_offchain_update(Props, CreationRound, Update),
        ContractId = aect_contracts:compute_contract_pubkey(Owner, CreationRound),
        ContractIds = maps:get(contract_ids, Props, []),
        case lists:member(ContractId, ContractIds) of
            true -> error(contract_already_present); % something is wrong with the test
            false -> pass
        end,
        Props1#{contract_id => ContractId, contract_file => ContractName,
                contract_ids => [ContractId | ContractIds]}
    end.

apply_offchain_update(Props, Round, Update) ->
    #{trees := Trees0,
      state := State} = Props,
    OnChainTrees = aesc_test_utils:trees(State),
    Env = tx_env(Props),
    Reserve = maps:get(channel_reserve, Props, 0),
    Trees = aesc_offchain_update:apply_on_trees(Update, Trees0, OnChainTrees,
                                                Env, Round, Reserve),
    Props#{trees => Trees}.


run(Cfg, Funs) ->
    lists:foldl(
        fun(Fun, Props) -> Fun(Props) end,
        Cfg,
        lists:flatten(Funs)).

apply_on_trees_(#{height := Height} = Props, SignedTx, S, positive) ->
    Trees = aesc_test_utils:trees(S),
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
            ?TEST_LOG("Transaction failed to be applied ~p", [SignedTx]),
            throw({case_failed, Err})
    end;
apply_on_trees_(#{height := Height} = Props, SignedTx, S, {negative, ExpectedError}) ->
    Trees = aesc_test_utils:trees(S),
    case aesc_test_utils:apply_on_trees_without_sigs_check([SignedTx], Trees,
                                                            Height) of
        ExpectedError ->
            ct:log("Transaction failed with: ~p", [ExpectedError]),
            pass;
        {error, Unexpected} -> throw({unexpected_error, Unexpected});
        {ok, _, _} -> throw(negative_case_passed)
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
    Height = 100,
    TxSpec = aesc_test_utils:create_tx_spec(PubKey1, PubKey2, DefaultSpec, S2),
    {ok, Tx} = aesc_create_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey1, PrivKey2]),
    Env      = aetx_env:tx_env(Height),
    {ok, [SignedTx], Trees1, _Events} =
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
    CreateOpts0 = maps:filter(
                   fun(K, _V) -> lists:member(K, [state, initiator_amount,
                                                 responder_amount,
                                                 channel_reserve,
                                                 lock_period])
                   end,
                   Props),
    IAmt = maps:get(initiator_amount, Props, 30 * aec_test_utils:min_gas_price()),
    RAmt = maps:get(responder_amount, Props, 70 * aec_test_utils:min_gas_price()),
    Height = 100,
    #{delegate_ids := NoDelegates} = no_delegates_spec(Height),
    Delegates = maps:get(delegate_ids, Props, NoDelegates),
    CreateOpts = CreateOpts0#{initiator_amount => IAmt,
                              responder_amount => RAmt,
                              delegate_ids => Delegates}, % ensure amounts and delegates
    {PubKey1, PubKey2, ChannelPubKey, _, S0} = create_(Cfg, CreateOpts),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S0),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S0),


    %% Get channel and account funds

    IAmt = maps:get(initiator_amount, Props, 30 * aec_test_utils:min_gas_price()),
    RAmt = maps:get(responder_amount, Props, 70 * aec_test_utils:min_gas_price()),
    Fee = maps:get(fee, Props, 50000 * aec_test_utils:min_gas_price()),

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

close_solo_with_optional_payload(Cfg) ->
    case proplists:get_value(close_solo_use_payload, Cfg, true) of
        true  -> fun close_solo_with_payload/2;
        false -> fun close_solo_without_payload/2
    end.

negative_close_solo_with_optional_payload(Cfg, ErrorWithPayload,
                                          ErrorEmptyPayload) ->

    Negative =
        fun(Fun, Error) -> fun(Props) -> (negative(Fun, Error))(Props) end end,
    case proplists:get_value(close_solo_use_payload, Cfg, true) of
        true  -> Negative(fun close_solo_with_payload/2, ErrorWithPayload);
        false -> Negative(fun close_solo_without_payload/2, ErrorEmptyPayload)
    end.

close_solo_with_payload(Props, Expected) ->
    close_solo_(Props, Expected).

close_solo_without_payload(Props, Expected) ->
    run(Props,
        [ set_prop(payload, <<>>),
          fun(Props1) -> close_solo_(Props1, Expected) end
        ]).

close_solo_(#{channel_pubkey    := ChannelPubKey,
              from_pubkey       := FromPubKey,
              from_privkey      := FromPrivkey,
              initiator_amount  := IAmt,
              responder_amount  := RAmt,
              initiator_pubkey  := IPubkey,
              responder_pubkey  := RPubkey,
              state             := S} = Props, Expected) ->

    Fee = maps:get(fee, Props, 50000 * aec_test_utils:min_gas_price()),
    %% Create close_solo tx and apply it on state trees
    PoI =  maps:get(poi, Props,
                    aesc_test_utils:proof_of_inclusion([{IPubkey, IAmt},
                                                        {RPubkey, RAmt}])),
    StateHash =
        case maps:get(state_hash, Props, not_passed) of
            not_passed -> aec_trees:poi_hash(PoI);
            Hash -> Hash
        end,
    Payload = reuse_or_create_payload(payload, Props#{state_hash => StateHash}),
    ct:log("Close is based on payload: ~p", [aesc_utils:deserialize_payload(Payload)]),
    Spec =
        case Props of
            #{nonce := Nonce} -> #{fee => Fee, nonce => Nonce};
            _ -> #{fee => Fee}
        end,
    TxSpec = aesc_test_utils:close_solo_tx_spec(ChannelPubKey, FromPubKey, Payload,
                                                PoI, Spec, S),
    {ok, Tx} = aesc_close_solo_tx:new(TxSpec),
    ?TEST_LOG("Close solo transaction ~p", [Tx]),
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
         state             := S} = Props, Expected) ->

    %% Create slash tx and apply it on state trees
    PoI = maps:get(poi, Props, aesc_test_utils:proof_of_inclusion([{IPubkey,
                                                                    IAmt},
                                                                   {RPubkey,
                                                                    RAmt}])),
    StateHash =
        case maps:get(state_hash, Props, not_passed) of
            not_passed -> aec_trees:poi_hash(PoI);
            Hash -> Hash
        end,
    Payload = reuse_or_create_payload(payload, Props#{state_hash => StateHash}),
    Spec =
        case Props of
            #{nonce := Nonce} -> #{fee => Fee, nonce => Nonce};
            _ -> #{fee => Fee}
        end,
    TxSpec = aesc_test_utils:slash_tx_spec(ChannelPubKey, FromPubKey, Payload,
                                            PoI, Spec, S),
    {ok, Tx} = aesc_slash_tx:new(TxSpec),
    ?TEST_LOG("Slash transaction ~p", [Tx]),
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
                  fee               := Fee,
                  state             := S} = Props, Expected) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    StateHash = maps:get(state_hash, Props, <<42:StateHashSize/unit:8>>),
    Payload = reuse_or_create_payload(payload, Props#{state_hash => StateHash}),

    SnapshotTxSpec = aesc_test_utils:snapshot_solo_tx_spec(ChannelPubKey, FromPubKey,
                           Payload, #{fee => Fee},S),
    {ok, SnapshotTx} = aesc_snapshot_solo_tx:new(SnapshotTxSpec),

    SignedTx = aec_test_utils:sign_tx(SnapshotTx, [FromPrivkey]),
    apply_on_trees_(Props, SignedTx, S, Expected).

set_delegates_(#{ channel_pubkey          := ChannelPubKey,
                  from_pubkey             := FromPubKey,
                  from_privkey            := FromPrivkey,
                  initiator_delegate_ids  := IDelegates,
                  responder_delegate_ids  := RDelegates,
                  round                   := Round,
                  fee                     := Fee,
                  state                   := S} = Props, Expected) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    StateHash = maps:get(state_hash, Props, <<42:StateHashSize/unit:8>>),
    Round = maps:get(round, Props, 43),
    PayloadStateHash = maps:get(payload_state_hash, Props, StateHash),
    PayloadRound = maps:get(payload_round, Props, Round),
    Payload = reuse_or_create_payload(payload, Props#{state_hash => PayloadStateHash,
                                                      round => PayloadRound}),

    SetDelegatesTxSpec =
        aesc_test_utils:set_delegates_tx_spec(ChannelPubKey, FromPubKey,
                                              IDelegates, RDelegates,
                                              StateHash, Round, Payload, #{fee => Fee}, S),
    {ok, SetDelegatesTx} = aesc_set_delegates_tx:new(SetDelegatesTxSpec),

    SignedTx = aec_test_utils:sign_tx(SetDelegatesTx, [FromPrivkey]),
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

    Spec0 = #{fee => Fee},
    ForceProTxSpec = aesc_test_utils:force_progress_tx_spec(ChannelPubKey, From,
                                                            Payload,
                                                            Update, StateHash,
                                                            Round, OffChainTrees,
                                                            Spec0, S),
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
            Test(Poster, AccountNonce - 1,  tx_nonce_already_used_for_account),
            Test(Poster, AccountNonce,      tx_nonce_already_used_for_account),
            Test(Poster, AccountNonce + 2,  tx_nonce_too_high_for_account)
        end,
        ?ROLES),
    ok.

test_both_payload_from_different_channel(Cfg, Fun) ->
    test_both_payload_from_different_channel(Cfg, Fun, #{}).

test_both_payload_from_different_channel(Cfg, Fun, Opts) ->
    Test =
        fun(Poster) ->
            run(Opts#{cfg => Cfg},
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


test_both_old_round(Cfg, Fun, Props, Reason) ->
    Test0 =
        fun(First, Second, R1, R2) ->
            run(Props#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_prop(round, R1),
                set_from(First),
                set_prop(amount, 1),
                set_prop(fee, 50000 * aec_test_utils:min_gas_price()),
                positive(fun deposit_/2),
                set_prop(round, R2),
                set_from(Second),
                negative(Fun, {error, Reason})])
        end,
    Test =
        fun(F, S) ->
            Test0(F, S, 42, 41),
            Test0(F, S, 42, 40)
        end,
    [Test(First, Second) || First <- ?ROLES,
                            Second <- ?ROLES],
    ok.

test_both_already_closed(Cfg, Fun, InitProps0) ->
    IAmt = 100000 * aec_test_utils:min_gas_price(),
    RAmt = 100000 * aec_test_utils:min_gas_price(),

    InitProps = InitProps0#{ initiator_amount => IAmt
                           , responder_amount => RAmt
                           , fee => 50000 * aec_test_utils:min_gas_price()},
    Test =
        fun(Poster) ->
            run(InitProps#{cfg => Cfg},
               [positive(fun create_channel_/2),
                prepare_balances_for_mutual_close(),
                positive(fun close_mutual_/2),
                set_from(Poster),
                negative(Fun, {error, channel_does_not_exist})
               ])
        end,
    [Test(Role) || Role <- ?ROLES],
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
                set_prop(fee, 50000 * aec_test_utils:min_gas_price()),
                negative(Fun, {error, same_round})])
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
                      height            := Height} = Props) ->
                    lists:foreach(
                        fun(PrivKeys) ->
                            PayloadSpec = create_payload_spec(Props),
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
    ChannelHashSize = aeser_api_encoder:byte_size_for_type(channel),
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
    test_both_closing_channel(Cfg, Fun, #{}).

test_both_closing_channel(Cfg, Fun, InitProps) ->
    Test =
        fun(Closer, Poster) ->
            run(InitProps#{cfg => Cfg},
               [positive(fun create_channel_/2),
                set_from(Closer),
                positive(close_solo_with_optional_payload(Cfg)),
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
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
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
    test_delegate_not_allowed(Cfg, Fun, InitProps, account_not_peer).

test_delegate_not_allowed(Cfg, Fun, InitProps, Err) ->
    Height = 100,
    run(InitProps#{ cfg => Cfg
                  , height => Height},
      [fun(Props) ->
            {Delegate1, Delegate2, S} = create_loaded_accounts(1000000000 * aec_test_utils:min_gas_price(),
                                                               1000000000 * aec_test_utils:min_gas_price()),
            #{delegate_ids := DelegateIds} =
                delegates_spec([aeser_id:create(account, Delegate1)], %% initiator delegates
                               [aeser_id:create(account, Delegate2)], %% responder delegates
                               Height),
            ?TEST_LOG("DelegateIds ~p", [DelegateIds]),
            Props#{ cfg => [{state, S} | Cfg]
                  , delegate_ids => DelegateIds}
        end,
        positive(fun create_channel_/2),
        fun(#{delegate_ids := Ds, state := S} = Props) ->
            D1 =
                case maps:get(use_delegate, Props, random) of
                    random -> pick_random_delegate(Ds);
                    Role -> pick_delegate_for_role(Role, Ds)
                end,
            D1Pubkey = aeser_id:specialize(D1, account),
            ?TEST_LOG("Use delegate: ~p", [D1Pubkey]),
            D1PrivKey = aesc_test_utils:priv_key(D1Pubkey, S),
            Props#{from_pubkey => D1Pubkey, from_privkey => D1PrivKey}
        end,
        negative(Fun, {error, Err})]),
    ok.

test_delegate_allowed(Cfg, Fun) ->
    test_delegate_allowed(Cfg, Fun, #{}).

test_delegate_allowed(Cfg, Fun, InitProps) ->
    Height = 100,
    run(InitProps#{ cfg => Cfg
                  , height => Height},
      [fun(Props) ->
            {Delegate1, Delegate2, S} = create_loaded_accounts(100000000 * aec_test_utils:min_gas_price(),
                                                               100000000 * aec_test_utils:min_gas_price()),
            #{delegate_ids := DelegateIds} =
                delegates_spec([aeser_id:create(account, Delegate1)], %% initiator delegates
                               [aeser_id:create(account, Delegate2)], %% responder delegates
                               Height),
            ?TEST_LOG("DelegateIds ~p", [DelegateIds]),
            Props#{ cfg => [{state, S} | Cfg]
                  , delegate_ids => DelegateIds}
        end,
        positive(fun create_channel_/2),
        fun(#{delegate_ids := Ds, state := S} = Props) ->
            D1 =
                case maps:get(use_delegate, Props, random) of
                    random -> pick_random_delegate(Ds);
                    Role -> pick_delegate_for_role(Role, Ds)
                end,
            D1Pubkey = aeser_id:specialize(D1, account),
            ?TEST_LOG("Chosen delegate is ~p", [D1Pubkey]),
            D1PrivKey = aesc_test_utils:priv_key(D1Pubkey, S),
            Props#{from_pubkey => D1Pubkey, from_privkey => D1PrivKey}
        end,
        positive(Fun)]),
    ok.

test_not_participant(Cfg, Fun) ->
    test_not_participant(Cfg, Fun, #{}).

test_not_participant(Cfg, Fun, InitProps) ->
    test_not_participant(Cfg, Fun, InitProps, account_not_peer).

test_not_participant(Cfg, Fun, InitProps, Error) ->
    run(InitProps#{cfg => Cfg},
        [positive(fun create_channel_/2),
         fun(#{state := S0} = Props) ->
            {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
            PrivKey = aesc_test_utils:priv_key(NewAcc, S1),
            Props#{state => S1, from_pubkey => NewAcc, from_privkey => PrivKey}
         end,
         negative(Fun, {error, Error})]),
    ok.

register_new_oracle(QFormat, RFormat, QueryFee) ->
    fun(Props0) ->
        run(Props0,
           [fun(#{state := S0} = Props) ->
                {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
                Props#{state => S1, oracle => NewAcc}
            end,
            fun(#{state := S, oracle := Oracle} = Props) ->
                RegTx = aeo_test_utils:register_tx(Oracle,
                                                   #{query_format => QFormat,
                                                     query_fee => QueryFee,
                                                     response_format => RFormat,
                                                     abi_version => aect_test_utils:abi_version()
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
                                                  aeser_id:create(oracle, Oracle),% oracle is asking
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
                {NewAcc, S1} = aesc_test_utils:setup_new_account(S0),
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
                Protocol = aec_hard_forks:protocol_effective_at_height(Height0),
                NameFee =
                        case Protocol >= ?LIMA_PROTOCOL_VSN of
                            true -> aec_governance:name_claim_fee(Name, Protocol);
                            false -> prelima
                        end,
                TxSpec = aens_test_utils:claim_tx_spec(NameOwner, Name, NameSalt, NameFee, S),
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
            fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                (create_contract_call_payload(ContractId, CName, Fun,
                                              Args, 1))(Props)
            end,
            set_prop(fee, 1000000 * aec_test_utils:min_gas_price()),
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
            set_prop(fee, 600000 * aec_test_utils:min_gas_price()),
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
            set_prop(fee, 500000 * aec_test_utils:min_gas_price()),
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
        ?assertMatch({ok, Result}, ?IF_AEVM(aeb_heap:from_binary(Type, EncRValue),
                                            {ok, aeb_fate_encoding:deserialize(EncRValue)})),
        Props
    end.

%% Create poi just for participants; used by slash and close solo
poi_participants_only() ->
    fun(#{initiator_pubkey  := IPubkey,
          responder_pubkey  := RPubkey,
          trees             := Trees} = Props) ->
          PoI = calc_poi([IPubkey, RPubkey], [], Trees),
          PoIHash = aec_trees:poi_hash(PoI),
          Props#{poi => PoI, state_hash => PoIHash}
   end.

compile_contract(ContractName) ->
    aect_test_utils:compile_contract(aect_test_utils:sophia_version(), ContractName).

compile_contract_vsn(Name, Vsn) ->
    case aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name) of
        {ok, SerCode} ->
            CodeMap = #{ type_info := TIs } = aect_sophia:deserialize(SerCode),
            case maps:get(contract_vsn, CodeMap) of
                Vsn -> {ok, SerCode};
                _   ->
                    TIs1 = [ patch_type_info(TI, Vsn) || TI <- TIs ],
                    Compiler = maps:get(compiler_version, CodeMap, <<"1.4.0">>),
                    CodeMap1 = CodeMap#{ type_info := TIs1, compiler_version => Compiler },
                    {ok, aect_sophia:serialize(CodeMap1, Vsn)}
            end;
        Err -> Err
    end.

patch_type_info({H, F, As, R}, 3) -> {H, F, true, As, R};
patch_type_info({H, F, _, As, R}, N) when N < 3 -> {H, F, As, R};
patch_type_info(TI, _) -> TI.


%% test that a force progress transaction can NOT produce an on-chain
%% contract with a code with the wrong Sophia serialization
fp_sophia_versions(Cfg) ->
    RomaHeight = 123,
    MinervaHeight = 1234,
    FortunaHeight = 12345,
    LimaHeight = 123456,

    SophiaVsn1 = 1,
    SophiaVsn2 = 2,
    SophiaVsn3 = 3,
    % ensure assumptions regarding heights
    true = RomaHeight < ?MINERVA_FORK_HEIGHT,
    true = aect_sophia:is_legal_serialization_at_protocol(SophiaVsn1, ?ROMA_PROTOCOL_VSN),
    false = aect_sophia:is_legal_serialization_at_protocol(SophiaVsn2, ?ROMA_PROTOCOL_VSN),
    false = aect_sophia:is_legal_serialization_at_protocol(SophiaVsn3, ?ROMA_PROTOCOL_VSN),
    true = MinervaHeight >= ?MINERVA_FORK_HEIGHT,
    true = MinervaHeight < ?FORTUNA_FORK_HEIGHT,
    true = aect_sophia:is_legal_serialization_at_protocol(SophiaVsn2, ?MINERVA_PROTOCOL_VSN),
    false = aect_sophia:is_legal_serialization_at_protocol(SophiaVsn3, ?MINERVA_PROTOCOL_VSN),
    true = FortunaHeight >= ?FORTUNA_FORK_HEIGHT,
    true = FortunaHeight < ?LIMA_FORK_HEIGHT,
    true = aect_sophia:is_legal_serialization_at_protocol(SophiaVsn2, ?FORTUNA_PROTOCOL_VSN),
    false = aect_sophia:is_legal_serialization_at_protocol(SophiaVsn3, ?FORTUNA_PROTOCOL_VSN),
    true = LimaHeight >= ?LIMA_FORK_HEIGHT,

    TestAtHeight =
        fun(Height, Res) ->
            fun(Props0) ->
                ct:log("Testing at height ~p, expecting ~p", [Height, Res]),
                Protocol = aec_hard_forks:protocol_effective_at_height(Height),
                run(Props0,
                    [ set_prop(height, Height),
                      set_prop(fee, 500000 * aec_governance:minimum_gas_price(Protocol)),
                      set_prop(gas_price, aec_governance:minimum_gas_price(Protocol)),
                      %% recompute the update with the new gas price
                      fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                          (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                        [<<"42">>], 1))(Props)
                      end,
                      fun(Props) ->
                          Exec =
                              case Res of
                                  {error, Err} ->
                                      negative(fun force_progress_/2, {error, Err});
                                  ok ->
                                      positive(fun force_progress_/2)
                              end,
                          Exec(Props),
                          Props0 %% so we don't have round issues
                      end
                    ])
            end
        end,
    FP =
        fun(Forcer, Vm, CodeSVsn, RomaRes, MinervaRes, FortunaRes, LimaRes) ->
            Round = 42,
            run(#{cfg => Cfg, initiator_amount => 10000000,
                              responder_amount => 10000000,
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                set_prop(vm_version, Vm),
                set_prop(compiler_fun,
                         fun(ContractName) ->
                            compile_contract_vsn(ContractName,
                                                 CodeSVsn)
                          end),
                create_contract_poi_and_payload(Round - 1,
                                                _ContractCreateRound = 10,
                                                _ContractOwner = Forcer),
                set_from(Forcer),
                set_prop(height, RomaHeight),
                set_prop(round, Round),
                fun(#{contract_id := ContractId, contract_file := CName} = Props) ->
                    (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                  [<<"42">>], 1))(Props)
                end,
                fun(#{contract_id := ContractId,
                      trees := Trees0} = Props) ->
                    Contract = aect_test_utils:get_contract(ContractId, #{trees => Trees0}),
                    {code, Code} = aect_contracts:code(Contract),
                    Deserialized = aect_sophia:deserialize(Code),
                    %% ensure contract serialization version
                    CodeSVsn = maps:get(contract_vsn, Deserialized),
                    Props
                end,
                TestAtHeight(RomaHeight, RomaRes),
                TestAtHeight(MinervaHeight, MinervaRes),
                TestAtHeight(FortunaHeight, FortunaRes),
                %% recreate the payload to be the new vsn
                set_prop(height, LimaHeight),
                set_prop(round, Round - 1),
                delete_prop(payload), % new off-chain vsn with correct round
                create_payload(),
                set_prop(round, Round),
                TestAtHeight(LimaHeight, LimaRes)
               ])
        end,
    %% return values
    OK = ok,
    ErrIllegal = {error, illegal_contract_compiler_version},
    ErrUnknown = {error, unknown_vm_version},
    %% actual checks
    ForkChecks =
        [
         %% AEVM 1
         {?VM_AEVM_SOPHIA_1, SophiaVsn1,
            OK,         %% Roma
            OK,         %% Minerva
            OK,         %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_1, SophiaVsn2,
            ErrIllegal, %% Roma
            OK,         %% Minerva
            OK,         %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_1, SophiaVsn3,
            ErrIllegal, %% Roma
            ErrIllegal, %% Minerva
            ErrIllegal, %% Fortuna
            OK          %% Lima
         },
         %% AEVM 2
         {?VM_AEVM_SOPHIA_2, SophiaVsn1,
            ErrUnknown, %% Roma
            OK,         %% Minerva
            OK,         %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_2, SophiaVsn2,
            ErrUnknown, %% Roma
            OK,         %% Minerva
            OK,         %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_2, SophiaVsn3,
            ErrUnknown, %% Roma
            ErrIllegal, %% Minerva
            ErrIllegal, %% Fortuna
            OK          %% Lima
         },
         %% AEVM 3
         {?VM_AEVM_SOPHIA_3, SophiaVsn1,
            ErrUnknown, %% Roma
            ErrUnknown, %% Minerva
            OK,         %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_3, SophiaVsn2,
            ErrUnknown, %% Roma
            ErrUnknown, %% Minerva
            OK,         %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_3, SophiaVsn3,
            ErrUnknown, %% Roma
            ErrUnknown, %% Minerva
            ErrIllegal, %% Fortuna
            OK          %% Lima
         },
         %% AEVM 4
         {?VM_AEVM_SOPHIA_4, SophiaVsn1,
            ErrUnknown, %% Roma
            ErrUnknown, %% Minerva
            ErrUnknown, %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_4, SophiaVsn2,
            ErrUnknown, %% Roma
            ErrUnknown, %% Minerva
            ErrUnknown, %% Fortuna
            ErrIllegal  %% Lima
         },
         {?VM_AEVM_SOPHIA_4, SophiaVsn3,
            ErrUnknown, %% Roma
            ErrUnknown, %% Minerva
            ErrUnknown, %% Fortuna
            OK          %% Lima
         }
        ],

    [FP(Forcer, Vm, CodeSVsn, RRes, MRes, FRes, LRes)
        || Forcer <- ?ROLES,
           {Vm, CodeSVsn, RRes, MRes, FRes, LRes} <- ForkChecks],
    ok.

encode_call_data(ContractName, Function, Arguments) ->
    {ok, Contract} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), ContractName),
    ct:pal("ENCODE:\n----\n~s\n----\nWhat: ~p",
          [Contract, {Function, Arguments}]),
    aect_test_utils:encode_call_data(aect_test_utils:sophia_version(), Contract, Function, Arguments).

address_encode(Type, Binary) ->
    case protocol_version() of
        Vsn when Vsn < ?MINERVA_PROTOCOL_VSN; Type == hash ->
            <<_:16, HexStr/binary>> = aeu_hex:hexstring_encode(Binary),
            <<"#", HexStr/binary>>;
        _ ->
            aeser_api_encoder:encode(Type, Binary)
    end.

quote(Bin) -> <<$", Bin/binary, $">>.

create_contract_in_onchain_trees(ContractName, InitArg, Deposit) ->
    fun(#{state := State0,
          owner := Owner} = Props) ->
        Trees0 = aesc_test_utils:trees(State0),
        {ok, BinCode} = compile_contract(ContractName),
        {ok, CallData} = encode_call_data(ContractName, <<"init">>, InitArg),
        Nonce = aesc_test_utils:next_nonce(Owner, State0),
        {ok, AetxCreateTx} =
            aect_create_tx:new(#{owner_id    => aeser_id:create(account, Owner),
                                 nonce       => Nonce,
                                 code        => BinCode,
                                 vm_version  => aect_test_utils:vm_version(),
                                 abi_version => aect_test_utils:abi_version(),
                                 deposit     => Deposit,
                                 amount      => 0,
                                 gas         => 123467,
                                 gas_price   => aec_test_utils:min_gas_price(),
                                 call_data   => CallData,
                                 fee         => 10 * aec_test_utils:min_gas_price()}),
        {contract_create_tx, CreateTx} = aetx:specialize_type(AetxCreateTx),
        Env = tx_env(Props),
        {ok, _} = aect_create_tx:check(CreateTx, Trees0, Env),
        {ok, Trees, _} = aect_create_tx:process(CreateTx, Trees0, Env),
        ContractId = aect_contracts:compute_contract_pubkey(Owner, Nonce),
        State = aesc_test_utils:set_trees(Trees, State0),
        Props#{state => State, contract_file => ContractName, contract_id => ContractId}
    end.

sophia_typerep(Type) ->
    case aect_test_utils:backend() of
        aevm -> aeb_heap:to_binary(aevm_type(Type), 0);
        fate -> iolist_to_binary(aeb_fate_encoding:serialize_type(Type))
    end.

sophia_value(Value) ->
    case aect_test_utils:backend() of
        aevm -> aeb_heap:to_binary(Value, 0);
        fate -> aeb_fate_encoding:serialize(Value)
    end.

aevm_type(integer) -> word;
aevm_type(Type) -> Type.

encode_sig(Sig) ->
    <<"0x", Hex/binary>> = aeu_hex:hexstring_encode(Sig),
    binary_to_list(<<"#", Hex/binary>>).

fp_close_solo_slash_with_same_round(Cfg) ->
    FPRound = 43,
    CloseRound = 50,
    ContractRound = 10,
    Test =
        fun(Closer, Slasher, CloseHeight, IsPositiveTest) ->
            run(#{cfg => Cfg, initiator_amount => 10000000 * aec_test_utils:min_gas_price(),
                              responder_amount => 10000000 * aec_test_utils:min_gas_price(),
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(FPRound - 1, ContractRound,
                                                initiator),
                force_progress_sequence(FPRound, initiator),
                delete_prop(payload), % old FP payload
                delete_prop(state_hash),
                set_from(Closer),
                set_prop(round, CloseRound),
                set_prop(height, CloseHeight),
                positive(fun close_solo_with_payload/2),
                set_from(Slasher),
                set_prop(round, CloseRound), %% same round
                fun(Props) ->
                    Fun =
                        case IsPositiveTest of
                            true ->
                                positive(fun slash_/2);
                            false ->
                                negative(fun slash_/2, {error, same_round})
                        end,
                    Fun(Props)
                end
               ])
        end,
    SwitchHeight = 168300,
    [Test(Closer, Slasher, CloseHeight, IsPossitiveTest)
        || Closer  <- ?ROLES,
           Slasher <- ?ROLES,
           {CloseHeight, IsPossitiveTest} <- [{SwitchHeight - 1, true},
                                              {SwitchHeight    , false},
                                              {SwitchHeight + 1, false}]],
    ok.

fp_from_delegate_after_iris_not_closing(Cfg) ->
    Height = 100,
    PreIris = aec_hard_forks:protocol_effective_at_height(Height) < ?IRIS_PROTOCOL_VSN,
    NonEmptyPayload = proplists:get_value(force_progress_use_payload, Cfg, true),
    FPRound = 30,
    ContractRound = 2,
    Test =
        fun(WhosDelegate) ->
            PrepareFP =
                fun(Props) ->
                    run(Props,
                        [ rename_prop(from_pubkey, delegate_pubkey, delete_old),
                          rename_prop(from_privkey, delegate_privkey, delete_old),
                          set_from(WhosDelegate),
                          fun(#{ initiator_pubkey := Initiator
                              , responder_pubkey := Responder
                              , from_pubkey      := From 
                              , delegate_pubkey  := Delegate} = Props1) ->
                              ?TEST_LOG("Initiator: ~p,\nresponder: ~p,\ndelegate: ~p",
                                        [Initiator, Responder, Delegate]),
                              ?TEST_LOG("From: ~p", [From]),
                              Props1#{offchain_update_from => From}
                          end,
                          create_contract_poi_and_payload(FPRound - 1, ContractRound, WhosDelegate),
                          rename_prop(delegate_pubkey, from_pubkey, keep_old),
                          rename_prop(delegate_privkey, from_privkey, keep_old)
                        ])
                  end,
            Err =
                case PreIris of
                    true when NonEmptyPayload -> account_not_peer;
                    true -> not_caller;
                    false -> channel_not_closing
                end,
            test_delegate_not_allowed(Cfg,
                fun(Props, {negative, {error, Err1}}) ->
                    {Err, Err} = {Err, Err1},
                    run(Props,
                        [PrepareFP,
                        negative_force_progress_sequence(Cfg, FPRound,
                                                          _Forcer = do_not_change_from,
                                                          Err)
                        ])
                end,
                #{ height => Height }, Err)
        end,
    Test(initiator),
    Test(responder),
    ok.

fp_from_delegate_after_iris(Cfg) ->
    Height = 100,
    PreIris = aec_hard_forks:protocol_effective_at_height(Height) < ?IRIS_PROTOCOL_VSN,
    NonEmptyPayload = proplists:get_value(force_progress_use_payload, Cfg, true),
    FPRound = 30,
    ContractRound = 2,
    Test =
        fun(WhosDelegate, WhoCloses) ->
            PrepareFP =
                fun(Props) ->
                    run(Props,
                        [ rename_prop(from_pubkey, delegate_pubkey, delete_old),
                          rename_prop(from_privkey, delegate_privkey, delete_old),
                          set_prop(round, FPRound - 2),
                          set_from(WhoCloses),
                          positive(fun close_solo_with_payload/2),
                          set_from(WhosDelegate),
                          fun(#{ initiator_pubkey := Initiator
                              , responder_pubkey := Responder
                              , from_pubkey      := From 
                              , delegate_pubkey  := Delegate} = Props1) ->
                              ?TEST_LOG("Initiator: ~p,\nresponder: ~p,\ndelegate: ~p",
                                        [Initiator, Responder, Delegate]),
                              ?TEST_LOG("From: ~p", [From]),
                              Props1#{offchain_update_from => From}
                          end,
                          create_contract_poi_and_payload(FPRound - 1, ContractRound, WhosDelegate),
                          rename_prop(delegate_pubkey, from_pubkey, keep_old),
                          rename_prop(delegate_privkey, from_privkey, keep_old)
                        ])
                  end,
            case PreIris of
                true ->
                    Err =
                        case NonEmptyPayload of
                            true -> account_not_peer;
                            false -> not_caller
                        end,
                    test_delegate_not_allowed(Cfg,
                        fun(Props, {negative, {error, Err1}}) ->
                            {Err, Err} = {Err, Err1},
                            run(Props,
                                [PrepareFP,
                                negative_force_progress_sequence(Cfg, FPRound,
                                                                  _Forcer = do_not_change_from,
                                                                  Err)
                                ])
                        end,
                        #{ height => Height }, Err);
                false ->
                    test_delegate_allowed(Cfg,
                        fun(Props, positive) ->
                            run(Props,
                                [PrepareFP,
                                set_prop(round, FPRound),
                                set_prop(fee, 100000 * aec_test_utils:min_gas_price()),
                                  fun(#{contract_id := ContractId, contract_file := CName} = Props1) ->
                                      (create_contract_call_payload(ContractId, CName, <<"main_">>,
                                                                    [<<"42">>], 1))(Props1)
                                  end,
                                positive(fun force_progress_/2)
                                ])
                        end,
                        #{ height => Height, use_delegate => WhosDelegate })
            end
        end,
    [Test(WhosDelegate, WhoCloses)
        || WhosDelegate  <- ?ROLES,
           WhoCloses     <- ?ROLES],
    ok.

fp_wrong_delegate_after_iris(Cfg) ->
    Height = 100,
    PreIris = aec_hard_forks:protocol_effective_at_height(Height) < ?IRIS_PROTOCOL_VSN,
    NonEmptyPayload = proplists:get_value(force_progress_use_payload, Cfg, true),
    FPRound = 30,
    ContractRound = 2,
    Test =
        fun(WhosDelegate, WhoCloses) ->
            CallFrom =
                case WhosDelegate of
                    initiator -> responder;
                    responder -> initiator
                end,
            PrepareFP =
                fun(Props) ->
                    run(Props,
                        [ rename_prop(from_pubkey, delegate_pubkey, delete_old),
                          rename_prop(from_privkey, delegate_privkey, delete_old),
                          set_from(WhoCloses),
                          positive(fun close_solo_with_payload/2),
                          set_from(CallFrom),
                          fun(#{ initiator_pubkey := Initiator
                               , responder_pubkey := Responder
                               , from_pubkey      := From 
                               , delegate_pubkey  := Delegate} = Props1) ->
                              ?TEST_LOG("Initiator: ~p,\nresponder: ~p,\ndelegate: ~p",
                                        [Initiator, Responder, Delegate]),
                              ?TEST_LOG("From: ~p", [From]),
                              Props1#{offchain_update_from => From}
                          end,
                          set_prop(round, FPRound - 1),
                          create_contract_poi_and_payload(FPRound - 1, ContractRound, WhosDelegate),
                          set_prop(fee, 100000 * aec_test_utils:min_gas_price()),
                          fun(Props1) ->
                              case NonEmptyPayload of
                                  true ->
                                      Props1;
                                  false ->
                                      run(Props1,
                                          [ set_from(initiator),
                                            set_prop(round, FPRound-1),
                                            delete_prop(state_hash),
                                            delete_prop(payload),
                                            positive(fun slash_/2),
                                            set_prop(payload, <<>>)
                                          ])
                              end
                          end,
                          rename_prop(delegate_pubkey, from_pubkey, keep_old),
                          rename_prop(delegate_privkey, from_privkey, keep_old)
                        ])
                  end,
            Err = 
                case PreIris of
                    true  when NonEmptyPayload -> account_not_peer;
                    true  -> not_caller;
                    false -> not_caller_or_delegate
                end,
            test_delegate_not_allowed(Cfg,
                fun(Props, {negative, {error, Err1}}) ->
                    {Err, Err} = {Err, Err1},
                    run(Props,
                        [PrepareFP,
                        negative_force_progress_sequence(Cfg, FPRound,
                                                          _Forcer = do_not_change_from,
                                                          Err)
                        ])
                end,
                #{ height => Height, use_delegate => WhosDelegate }, Err)
        end,
    [Test(WhosDelegate, WhoCloses)
        || WhosDelegate  <- ?ROLES,
           WhoCloses     <- ?ROLES],
    ok.

fp_fp_close_solo_with_same_round(Cfg) ->
    FirstFPRound = 3,
    SecondFPRound = 14,
    CloseRound = 13,
    ContractRound = 2,
    CleanAfterFP =
        fun(Props) ->
            run(Props,
                [delete_prop(payload), % old FP payload
                 delete_prop(state_hash)])
        end,
    Test =
        fun(Closer, CloseHeight, IsPositiveTest) ->
            run(#{cfg => Cfg, initiator_amount => 10000000 * aec_test_utils:min_gas_price(),
                              responder_amount => 10000000 * aec_test_utils:min_gas_price(),
                 channel_reserve => 1},
               [positive(fun create_channel_/2),
                create_contract_poi_and_payload(FirstFPRound - 1, ContractRound,
                                                initiator),
                force_progress_sequence(FirstFPRound, initiator),
                CleanAfterFP,
                set_prop(round, SecondFPRound - 1), % for the payload
                create_fp_trees(),
                create_payload(),
                set_prop(height, CloseHeight),
                force_progress_sequence(SecondFPRound, initiator),
                CleanAfterFP,
                set_from(Closer),
                set_prop(round, CloseRound),
                fun(Props) ->
                    Fun =
                        case IsPositiveTest of
                            true ->
                                positive(fun close_solo_with_payload/2);
                            false ->
                                negative(fun close_solo_with_payload/2, {error, same_round})
                        end,
                    Fun(Props)
                end
               ])
        end,
    SwitchHeight = 168300,
    [Test(Closer, CloseHeight, IsPossitiveTest)
        || Closer  <- ?ROLES,
           {CloseHeight, IsPossitiveTest} <- [{SwitchHeight - 1, true},
                                              {SwitchHeight    , false},
                                              {SwitchHeight + 1, false}]],
    ok.

no_delegates_spec(Height) ->
    delegates_spec([], [], Height).

delegates_spec(IIds, RIds, Height) ->
    case aec_hard_forks:protocol_effective_at_height(Height) < ?IRIS_PROTOCOL_VSN of
        true -> #{delegate_ids => IIds ++ RIds};
        false -> #{delegate_ids => {IIds, RIds}}
    end.

pick_delegate_for_role(Role, DelegateIds) ->
    case DelegateIds of
        [CommonDelegate | _] -> CommonDelegate;
        {[ID | _] , _} when Role =:= initiator -> ID;
        {_, [RD | _]} when Role =:= responder -> RD
    end.

pick_random_delegate(DelegateIds) ->
    Ds = case DelegateIds of
            L when is_list(L) -> L;
            {L1, L2} -> L1 ++ L2
          end,
    %% pick a random delegate. If this fails inermittently, it could
    %% be a symptom of an actual bug
    lists:nth(rand:uniform(length(Ds)), Ds).

set_delegates(Cfg) ->
    Height = 100,
    Round = 43,
    OldRound = Round - 5,
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    OldStateHash = <<40:StateHashSize/unit:8>>,
    StateHash = <<43:StateHashSize/unit:8>>,
    TestWithPayload =
        fun(Snapshoter, Setter, IDelegates, RDelegates) ->
            ?TEST_LOG("Test with payload,~nSnapshoter is ~p,~nSetter is ~p,~nIDelegates are ~p,~nRDelegates are ~p",
                      [Snapshoter, Setter, IDelegates, RDelegates]),
            run(#{cfg => Cfg
                 , initiator_delegate_ids => IDelegates
                 , responder_delegate_ids => RDelegates
                 , height => Height},
               [positive(fun create_channel_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure no delegates
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    [] = aesc_channels:delegate_ids(Channel, any),
                    [] = aesc_channels:delegate_ids(Channel, initiator),
                    [] = aesc_channels:delegate_ids(Channel, responder),
                    Props
                end,
                set_from(Snapshoter),
                set_prop(round, OldRound),
                set_prop(state_hash, OldStateHash),
                positive(fun snapshot_solo_/2),
                set_from(Setter),
                set_prop(round, Round),
                set_prop(state_hash, StateHash),
                positive(fun set_delegates_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    Round = aesc_channels:round(Channel),
                    StateHash = aesc_channels:state_hash(Channel),
                    % ensure updated delegates
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    IDelegates = aesc_channels:delegate_ids(Channel, initiator),
                    RDelegates = aesc_channels:delegate_ids(Channel, responder),
                    AllDelegates = IDelegates ++ RDelegates,
                    AllDelegates = aesc_channels:delegate_ids(Channel, any),
                    Props
                end
                ])
        end,
    [FakeId1, FakeId2, FakeId3, FakeId4] = fake_account_ids(4),
    Combinations1 = [[],%% no ids
                     [FakeId1], %% one id
                     [FakeId1, FakeId2]], %% two ids
    Combinations2 = [[], %% no ids
                     [FakeId3], %% one id
                     [FakeId3, FakeId4]], %% two ids
    [TestWithPayload(Role1, Role2, IDels, RDels) || Role1 <- ?ROLES,
                                                    Role2 <- ?ROLES,
                                                    IDels <- Combinations1,
                                                    RDels <- Combinations2],
    TestWithEmptyPayload =
        fun(Snapshoter, Setter, IDelegates, RDelegates) ->
            ?TEST_LOG("Test with payload,~nSnapshoter is ~p,~nSetter is ~p,~nIDelegates are ~p,~nRDelegates are ~p",
                      [Snapshoter, Setter, IDelegates, RDelegates]),
            run(#{cfg => Cfg
                 , initiator_delegate_ids => IDelegates
                 , responder_delegate_ids => RDelegates
                 , height => Height},
               [positive(fun create_channel_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure no delegates
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    [] = aesc_channels:delegate_ids(Channel, any),
                    [] = aesc_channels:delegate_ids(Channel, initiator),
                    [] = aesc_channels:delegate_ids(Channel, responder),
                    Props
                end,
                set_from(Snapshoter),
                set_prop(round, Round),
                set_prop(state_hash, StateHash),
                positive(fun snapshot_solo_/2),
                set_from(Setter),
                set_prop(payload, <<>>),
                positive(fun set_delegates_/2),
                fun(#{channel_pubkey := ChannelPubKey, state := S} = Props) ->
                    % ensure channel had been updated
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    Round = aesc_channels:round(Channel),
                    StateHash = aesc_channels:state_hash(Channel),
                    % ensure updated delegates
                    Channel = aesc_test_utils:get_channel(ChannelPubKey, S),
                    IDelegates = aesc_channels:delegate_ids(Channel, initiator),
                    RDelegates = aesc_channels:delegate_ids(Channel, responder),
                    AllDelegates = IDelegates ++ RDelegates,
                    AllDelegates = aesc_channels:delegate_ids(Channel, any),
                    Props
                end
                ])
        end,
    [TestWithEmptyPayload(Role1, Role2, IDels, RDels) || Role1 <- ?ROLES,
                                                         Role2 <- ?ROLES,
                                                         IDels <- Combinations1,
                                                         RDels <- Combinations2],
    ok.

set_delegates_unknown_from(Config) ->
    {MissingAccount, S} = aesc_test_utils:setup_new_account(aesc_test_utils:new_state()),
    PrivKey = aesc_test_utils:priv_key(MissingAccount, S),
    Round = 42,
    Height = 100,
    [FakeId1, FakeId2] = fake_account_ids(2),
    TestWithPayload =
        fun() ->
            run(#{cfg => Config, height => Height},
                [positive(fun create_channel_/2),
                 set_prop(from_pubkey, MissingAccount),
                 set_prop(from_privkey, PrivKey),
                 set_prop(round, Round),
                 set_prop(initiator_delegate_ids, [FakeId1]),
                 set_prop(responder_delegate_ids, [FakeId2]),
                 negative(fun set_delegates_/2, {error, account_not_found})])
        end,
    TestWithPayload(),

    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    
    TestWithoutPayload =
        fun(Snapshoter) ->
            run(#{cfg => Config, height => Height},
                [positive(fun create_channel_/2),
                 set_from(Snapshoter),
                 set_prop(round, Round - 5),
                 set_prop(state_hash, <<42:StateHashSize/unit:8>>),
                 positive(fun snapshot_solo_/2),
                 set_prop(from_pubkey, MissingAccount),
                 set_prop(from_privkey, PrivKey),
                 set_prop(round, Round),
                 set_prop(initiator_delegate_ids, [FakeId1]),
                 set_prop(responder_delegate_ids, [FakeId2]),
                 negative(fun set_delegates_/2, {error, account_not_found})])
        end,
    [TestWithoutPayload(Role) || Role <- ?ROLES],
    ok.

fake_account_ids(Cnt) ->
    fake_account_ids(Cnt, []).

fake_account_ids(Cnt, Accum) when Cnt < 1 -> Accum;
fake_account_ids(Cnt, Accum) ->
    UniqueNumber = 10000 - Cnt,
    AccountHashSize = aeser_api_encoder:byte_size_for_type(account_pubkey),
    Id = aeser_id:create(account, <<UniqueNumber:AccountHashSize/unit:8>>),
    fake_account_ids(Cnt - 1, [Id | Accum]).

set_delegates_missing_channel(Config) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    Opts = #{initiator_delegate_ids => [],
             responder_delegate_ids => [],
             round => 42,
             state_hash => <<42:StateHashSize/unit:8>>,
             fee => 50000 * aec_test_utils:min_gas_price(),
             height => 100},
    test_both_missing_channel(Config, fun set_delegates_/2, Opts).

set_delegates_not_participant(Config) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    Opts = #{initiator_delegate_ids => [],
             responder_delegate_ids => [],
             round => 42,
             state_hash => <<42:StateHashSize/unit:8>>,
             fee => 50000 * aec_test_utils:min_gas_price(),
             height => 100},
    test_not_participant(Config, fun set_delegates_/2, Opts).

set_delegates_already_closing(Config) ->
    Opts = #{initiator_delegate_ids => [],
             responder_delegate_ids => [],
             round => 42,
             %% do not set state_hash as it will mess up the close solo's
             %% state
             fee => 50000 * aec_test_utils:min_gas_price(),
             height => 100},
    test_both_closing_channel(Config, fun set_delegates_/2, Opts).

set_delegates_payload_from_another_channel(Config) ->
    Opts = #{initiator_delegate_ids => [],
             responder_delegate_ids => [],
             round => 42,
             fee => 50000 * aec_test_utils:min_gas_price(),
             height => 100},
    test_both_payload_from_different_channel(Config, fun set_delegates_/2, Opts).

set_delegates_payload_not_co_signed(Config) ->
    test_payload_not_both_signed(
        Config,
        fun(ChannelPubKey, FromPubKey, PayloadBin, _PoI, S) ->
            Payload = aetx_sign:deserialize_from_binary(PayloadBin),
            {channel_offchain_tx, OffChainTx} = aetx:specialize_type(aetx_sign:tx(Payload)),
            StateHash = aesc_offchain_tx:state_hash(OffChainTx),
            Round = aesc_offchain_tx:round(OffChainTx),
            aesc_test_utils:set_delegates_tx_spec(ChannelPubKey,
                                                  FromPubKey,
                                                  [],
                                                  [],
                                                  StateHash,
                                                  Round,
                                                  PayloadBin,
                                                  S)
        end,
        fun aesc_set_delegates_tx:new/1).

set_delegates_old_payload(Config) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    Opts = #{initiator_delegate_ids => [],
             responder_delegate_ids => [],
             round => 42,
             state_hash => <<42:StateHashSize/unit:8>>,
             fee => 50000 * aec_test_utils:min_gas_price(),
             height => 100},
    test_both_old_round(Config, fun set_delegates_/2, Opts, old_round).

set_delegates_can_not_replace_create(Config) ->
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    Opts = #{initiator_delegate_ids => [],
             responder_delegate_ids => [],
             round => 42,
             state_hash => <<42:StateHashSize/unit:8>>,
             fee => 50000 * aec_test_utils:min_gas_price(),
             height => 100},
    test_both_can_not_replace_create(Config, fun set_delegates_/2, Opts).

set_delegates_state_hash_mismatch(Config) ->
    Height = 100,
    Round = 43,
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    StateHash = <<42:StateHashSize/unit:8>>,
    PayloadStateHash = <<43:StateHashSize/unit:8>>,
    Test =
        fun(Setter) ->
            ?TEST_LOG("Test Setter is ~p", [Setter]),
            run(#{cfg => Config
                , initiator_delegate_ids => []
                , responder_delegate_ids => [] 
                , height => Height},
               [positive(fun create_channel_/2),
                set_from(Setter),
                set_prop(round, Round),
                set_prop(state_hash, StateHash),
                set_prop(payload_state_hash, PayloadStateHash),
                negative(fun set_delegates_/2, {error, unexpected_state_hash})
                ])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

set_delegates_round_mismatch(Config) ->
    Height = 100,
    Round = 43,
    PayloadRound = 44,
    StateHashSize = aeser_api_encoder:byte_size_for_type(state),
    StateHash = <<42:StateHashSize/unit:8>>,
    Test =
        fun(Setter) ->
            ?TEST_LOG("Test Setter is ~p", [Setter]),
            run(#{cfg => Config
                , initiator_delegate_ids => []
                , responder_delegate_ids => [] 
                , height => Height},
               [positive(fun create_channel_/2),
                set_from(Setter),
                set_prop(round, Round),
                set_prop(payload_round, PayloadRound),
                set_prop(state_hash, StateHash),
                negative(fun set_delegates_/2, {error, unexpected_round})
                ])
        end,
    [Test(Role) || Role <- ?ROLES],
    ok.

