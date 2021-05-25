-module(aehttp_sc_SUITE).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export(
   [sc_ws_timeout_open/1,
    sc_ws_attach_initiator/1,
    sc_ws_attach_responder/1,
    sc_ws_broken_open_params/1,
    sc_ws_min_depth_not_reached_timeout/1,
    sc_ws_min_depth_is_modifiable/1,
    sc_ws_basic_open_close/1,
    sc_ws_basic_open_close_server/1,
    sc_ws_failed_update/1,
    sc_ws_generic_messages/1,
    sc_ws_update_with_meta/1,
    sc_ws_update_transfer/1,
    sc_ws_conflict_two_offchain_updates/1,
    sc_ws_conflict_deposit_and_offchain_update/1,
    sc_ws_conflict_two_deposits/1,
    sc_ws_conflict_withdrawal_and_offchain_update/1,
    sc_ws_conflict_withdrawal_and_deposit/1,
    sc_ws_conflict_two_withdrawals/1,
    sc_ws_conflict_on_new_offchain/1,
    sc_ws_update_abort/1,
    sc_ws_snapshot_solo/1,
    sc_ws_close_mutual/1,
    sc_ws_close_solo/1,
    sc_ws_slash/1,
    sc_ws_force_progress_based_on_offchain_state/1,
    sc_ws_force_progress_based_on_onchain_state/1,
    sc_ws_failed_force_progress_balances/1,
    sc_ws_leave_reestablish/1,
    sc_ws_leave_reestablish_wrong_fsm_id/1,
    sc_ws_leave_reestablish_responder_stays/1,
    sc_ws_leave_reconnect/1,
    sc_ws_leave_responder_does_not_timeout/1,
    sc_ws_ping_pong/1,
    sc_ws_opening_ping_pong/1,
    sc_ws_deposit/1,
    sc_ws_withdraw/1,
    sc_ws_get_history/1,
    sc_ws_basic_contracts/1,
    sc_ws_oracle_contract/1,
    sc_ws_nameservice_contract/1,
    sc_ws_environment_contract/1,
    sc_ws_remote_call_contract/1,
    sc_ws_remote_call_contract_refering_onchain_data/1,
    sc_ws_wrong_call_data/1,
    sc_ws_wrong_abi/1,
    sc_ws_reconnect_early/1,
    sc_ws_basic_client_reconnect_i/1,
    sc_ws_basic_client_reconnect_r/1,
    sc_ws_basic_client_reconnect_i_w_reestablish/1,
    sc_ws_pinned_update/1,
    sc_ws_pinned_deposit/1,
    sc_ws_pinned_withdraw/1,
    sc_ws_pinned_error_update/1,
    sc_ws_pinned_error_deposit/1,
    sc_ws_pinned_error_withdraw/1,
    sc_ws_pinned_contract/1,
    sc_ws_optional_params_create/1,
    sc_ws_optional_params_fail_create/1,
    sc_ws_optional_params_deposit/1,
    sc_ws_optional_params_fail_deposit/1,
    sc_ws_optional_params_fail_withdrawal/1,
    sc_ws_optional_params_withdrawal/1,
    sc_ws_optional_params_snapshot/1,
    sc_ws_optional_params_fail_snapshot/1,
    sc_ws_optional_params_close_mutual/1,
    sc_ws_optional_params_fail_close_mutual/1,
    sc_ws_optional_params_close_solo/1,
    sc_ws_optional_params_fail_close_solo/1,
    sc_ws_optional_params_slash/1,
    sc_ws_optional_params_fail_slash/1,
    sc_ws_optional_params_settle/1,
    sc_ws_optional_params_fail_settle/1,
    sc_ws_optional_params_fail_force_progress/1,
    sc_ws_abort_offchain_update/1,
    sc_ws_abort_deposit/1,
    sc_ws_abort_withdraw/1,
    sc_ws_abort_snapshot_solo/1,
    sc_ws_abort_shutdown/1,
    sc_ws_abort_slash/1,
    sc_ws_abort_settle/1,
    sc_ws_can_not_abort_while_open/1
   ]).
-export([sc_ws_open_/4,
         sc_ws_close_/1,
         sc_ws_close_mutual_/2,
         sc_ws_get_both_balances/4,
         sc_ws_get_balance/3,
         start_node/1,
         stop_node/1,
         sign_post_mine/2,
         sign_and_post_tx/1,
         produce_update_volley_funs/2,
         with_registered_events/3,
         create_contract_/6,
         call_a_contract_/9,
         wait_for_channel_event/3,
         wait_for_channel_event/4,
         reset_participants/2]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(WS, aehttp_ws_test_utils).
-define(DEFAULT_MIN_DEPTH_FACTOR, 10).
-define(DEFAULT_MIN_DEPTH, 3).
-define(MAX_MINED_BLOCKS, 20).
-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).
-define(BOGUS_FSM_ID, <<"Invalid">>).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).

-define(SLOGAN, slogan(?FUNCTION_NAME, ?LINE)).
-define(SLOGAN(Param), slogan(?FUNCTION_NAME, ?LINE, Param)).

-define(ROLES, [initiator, responder]).

-define(SIMPLE_AUTH_GA_SECRET, "42").

-define(ALICE, {
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>}).

-define(BOB, {
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>}).

-define(CAROL, {
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>}).

-define(CHECK_INFO(Timeout), check_info(?LINE, Timeout)).
-define(PEEK_MSGQ, peek_msgq(?LINE)).

-define(ARBITRARY_BIG_FEE, 123456789876543).

all() -> [{group, plain}, {group, assume_min_depth}, {group, aevm}, {group, fate}].

groups() ->
    [{plain, [],
      [ sc_ws_broken_open_params,
        sc_ws_timeout_open,
        sc_ws_min_depth_not_reached_timeout,
        sc_ws_min_depth_is_modifiable,
        sc_ws_basic_open_close,
        sc_ws_basic_open_close_server,
        sc_ws_opening_ping_pong,
        sc_ws_snapshot_solo,
        sc_ws_update_with_meta,
        %% both can start close mutual
        sc_ws_close_mutual,
        %% both can solo-close
        sc_ws_close_solo,
        %% fsm informs of slash potential
        sc_ws_slash,
        {group, reconnects},
        {group, force_progress},
        {group, with_open_channel},
        {group, with_meta},
        {group, client_reconnect},
        {group, optional_fee},
        {group, optional_gas_price},
        {group, optional_fee_higher_than_gas_price},
        {group, optional_fee_lower_than_gas_price},
        {group, optional_nonce},
        {group, abort_updates},
        {group, only_one_signs},
        {group, both_sign}
      ]},

     {assume_min_depth, [],
      [ sc_ws_basic_open_close ]},

     {with_meta, [],
      [ sc_ws_update_transfer,
        sc_ws_deposit,
        sc_ws_withdraw,
        sc_ws_remote_call_contract ]},

     {with_open_channel, [sequence],
      [ sc_ws_ping_pong,
        sc_ws_get_history,
        %% both can deposit
        sc_ws_deposit,
        %% both can withdraw
        sc_ws_withdraw,
        %% ensure port is reusable
        sc_ws_failed_update,
        sc_ws_generic_messages,
        sc_ws_update_abort
      ]},

     {aevm, [], [{group, sc_contracts}, {group, sc_ga}]},
     {fate, [], [{group, sc_contracts}, {group, sc_ga}]},

     {sc_contracts, [sequence],
      [ %% both can refer on-chain objects - oracle
        sc_ws_basic_contracts,
        sc_ws_oracle_contract,
        %% both can refer on-chain objects - name service
        sc_ws_nameservice_contract,
        %% both can refer on-chain objects - chain environment
        sc_ws_environment_contract,
        %% both can call a remote contract
        sc_ws_remote_call_contract,
        %% both can call a remote contract refering on-chain data
        sc_ws_remote_call_contract_refering_onchain_data,
        sc_ws_wrong_call_data,
        sc_ws_wrong_abi
      ]},
     {only_one_signs, [sequence],
      [ sc_ws_conflict_on_new_offchain,
        {group, conflicts}
      ]
     },
     {both_sign, [sequence],
      [ {group, conflicts}
      ]
     },

     {conflicts, [sequence], conflicts_sequence()},
     {sc_ga, [sequence],
      [ {group, ga_initiator},
        {group, ga_responder},
        {group, ga_both}
      ]},

     {ga_initiator, [sequence],
      [ sc_ws_attach_initiator,
        sc_ws_basic_open_close
      ]},

     {ga_responder, [sequence],
      [ sc_ws_attach_responder,
        sc_ws_basic_open_close
      ]},

     {ga_both, [sequence],
      [ sc_ws_attach_initiator,
        sc_ws_attach_responder,
        sc_ws_basic_open_close
      ]},

     {client_reconnect, [sequence],
      [ sc_ws_basic_client_reconnect_i,
        sc_ws_basic_client_reconnect_r,
        sc_ws_basic_client_reconnect_i_w_reestablish
      ]},

     {pinned_env, [sequence],
      [
        sc_ws_pinned_update,
        sc_ws_pinned_deposit,
        sc_ws_pinned_withdraw,
        sc_ws_pinned_error_update,
        sc_ws_pinned_error_deposit,
        sc_ws_pinned_error_withdraw,
        sc_ws_pinned_contract
      ]},

     {optional_fee, [sequence], optional_onchain_params_sequence()},
     {optional_gas_price, [sequence], optional_onchain_params_sequence()},
     {optional_fee_higher_than_gas_price, [sequence], optional_onchain_params_sequence()},
     {optional_fee_lower_than_gas_price, [sequence], optional_onchain_params_sequence()},
     {optional_nonce, [sequence], [sc_ws_optional_params_fail_force_progress
                                   | optional_onchain_fail_params_sequence()]},

     {abort_updates, [sequence],
      [ sc_ws_abort_offchain_update
      , sc_ws_abort_deposit
      , sc_ws_abort_withdraw
      , sc_ws_abort_snapshot_solo
      , sc_ws_abort_shutdown
      , sc_ws_abort_slash
      , sc_ws_abort_settle
      , sc_ws_can_not_abort_while_open
      ]},
     {force_progress, [sequence],
      [ sc_ws_force_progress_based_on_offchain_state
      , sc_ws_force_progress_based_on_onchain_state
      , sc_ws_failed_force_progress_balances
      ]},

      %% possible to leave and reestablish channel
     {reconnects, [sequence],
      [ sc_ws_leave_reestablish
      , sc_ws_leave_reestablish_wrong_fsm_id
      , sc_ws_leave_reestablish_responder_stays
      , sc_ws_leave_reconnect
      , sc_ws_reconnect_early
      , sc_ws_leave_responder_does_not_timeout
      ]}
    ].

conflicts_sequence() ->
    [ 
      sc_ws_conflict_two_offchain_updates,
      sc_ws_conflict_deposit_and_offchain_update,
      sc_ws_conflict_two_deposits,
      sc_ws_conflict_withdrawal_and_offchain_update,
      sc_ws_conflict_withdrawal_and_deposit,
      sc_ws_conflict_two_withdrawals
    ].

optional_onchain_params_sequence() ->
    [
      sc_ws_optional_params_create,
      sc_ws_optional_params_deposit,
      sc_ws_optional_params_withdrawal,
      sc_ws_optional_params_snapshot,
      %sc_ws_optional_params_close_mutual,
      sc_ws_optional_params_close_solo,
      sc_ws_optional_params_slash,
      sc_ws_optional_params_settle
    ].

optional_onchain_fail_params_sequence() ->
    [
      sc_ws_optional_params_fail_create,
      sc_ws_optional_params_fail_deposit,
      sc_ws_optional_params_fail_withdrawal,
      sc_ws_optional_params_fail_snapshot,
      sc_ws_optional_params_fail_close_mutual,
      sc_ws_optional_params_fail_close_solo,
      sc_ws_optional_params_fail_slash,
      sc_ws_optional_params_fail_settle
    ].

suite() -> [].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => false,
                     <<"hard_forks">> => Forks},
               <<"mining">> =>
                   #{<<"micro_block_cycle">> => 1,
                     %% disable name claim auction
                     <<"name_claim_bid_timeout">> => 0}},
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [ {instant_mining, true}
                                                                 , {symlink_name, "latest.http_sc"}
                                                                 , {test_module, ?MODULE}] ++ Config),
    ct:log("Config1 = ~p", [Config1]),
    make_channel_docs_symlink(Config1),
    start_node([ {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
               , {started_apps, StartedApps}
               , {ws_port, 12340}] ++ Config1).

make_channel_docs_symlink(Config) ->
    DocsDir = filename:join(?config(priv_dir, Config), "channel_docs"),
    BaseSymlink = ?config(symlink_name, Config),
    LinkName = filename:join([?config(top_dir, Config), "_build/test/logs", BaseSymlink, "channel_docs"]),
    ok = aecore_suite_utils:symlink(DocsDir, LinkName).

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    stop_node(Config).

init_per_group(VM, Config) when VM == aevm; VM == fate ->
    aect_test_utils:init_per_group(VM, inc_group_port(VM, Config));
init_per_group(sc_ga, Config) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        Vsn when Vsn >= ?FORTUNA_PROTOCOL_VSN -> Config
    end;
init_per_group(with_meta, Config) ->
    sc_ws_open_([{include_meta, true} | Config]);
init_per_group(only_one_signs, Config) ->
    Config1 = [{who_signs_update, one} | Config], % this is being ran in the with_open_channel context
    sc_ws_open_(Config1);
init_per_group(both_sign, Config) ->
    Config1 = [{who_signs_update, both} | Config], % this is being ran in the with_open_channel context
    sc_ws_open_(Config1);
init_per_group(with_open_channel, Config) ->
    sc_ws_open_(Config, #{log_keep => 25});
init_per_group(Grp, Config) when Grp == ga_both; Grp == ga_initiator; Grp == ga_responder ->
    reset_participants(Grp, Config);
init_per_group(sc_contracts, Config) ->
    sc_ws_open_(reset_participants(sc_contracts, Config));
init_per_group(plain, Config) ->
    reset_participants(plain, Config);
init_per_group(assume_min_depth, Config) ->
    [{assume_min_depth, true} | reset_participants(plain, Config)];
init_per_group(client_reconnect, Config) ->
    reset_participants(client_reconnect, Config);
init_per_group(pinned_env, Config) ->
    aect_test_utils:init_per_group(aevm, reset_participants(pinned_env, Config));
init_per_group(optional_fee, Config) ->
    Fee = ?ARBITRARY_BIG_FEE,
    Opts = #{fee => Fee},
    CheckFun =
        fun(SignedTx) ->
            Tx = aetx_sign:innermost_tx(SignedTx),
            ActualFee = aetx:fee(Tx),
            case ActualFee =:= Fee of
                true -> ok;
                false -> error({wrong_fee, ActualFee, Fee})
            end
        end,
    [{fee_computation, {Opts, CheckFun}} |Config];
init_per_group(optional_gas_price, Config) ->
    GasPrice = aec_test_utils:min_gas_price() + 1234,
    Opts = #{gas_price => GasPrice},
    CheckFun =
        fun(SignedTx) ->
            Tx = aetx_sign:innermost_tx(SignedTx),
            Version = aect_test_utils:latest_protocol_version(),
            Height = current_height(),
            ActualGasPrice = aetx:min_gas_price(Tx, Height, Version),
            case ActualGasPrice =:= GasPrice of
                true -> ok;
                false -> error({wrong_gas_price, ActualGasPrice, GasPrice})
            end
        end,
    [{fee_computation, {Opts, CheckFun}} |Config];
init_per_group(optional_fee_higher_than_gas_price, Config) ->
    GasPrice = aec_test_utils:min_gas_price(),
    Fee = ?ARBITRARY_BIG_FEE * 2,
    Opts = #{ gas_price => GasPrice
            , fee => Fee},
    CheckFun =
        fun(SignedTx) ->
            Tx = aetx_sign:innermost_tx(SignedTx),
            ActualFee = aetx:fee(Tx),
            case ActualFee =:= Fee of
                true -> ok;
                false -> error({wrong_fee, ActualFee, Fee})
            end
        end,
    [{fee_computation, {Opts, CheckFun}} |Config];
init_per_group(optional_fee_lower_than_gas_price, Config) ->
    GasPrice = aec_test_utils:min_gas_price() + 1234,
    Opts = #{ gas_price => GasPrice
            , fee => 1}, % some small fee so it is being ignored in favour of gas price based computed fee
    CheckFun =
        fun(SignedTx) ->
            Tx = aetx_sign:innermost_tx(SignedTx),
            Version = aect_test_utils:latest_protocol_version(),
            Height = current_height(),
            ActualGasPrice = aetx:min_gas_price(Tx, Height, Version),
            case ActualGasPrice =:= GasPrice of
                true -> ok;
                false -> error({wrong_gas_price, ActualGasPrice, GasPrice})
            end
        end,
    [{fee_computation, {Opts, CheckFun}} |Config];
init_per_group(optional_nonce, Config) ->
    Nonce = 1234567,
    Opts = #{nonce => Nonce},
    CheckFun =
        fun(SignedTx) ->
            Tx = aetx_sign:innermost_tx(SignedTx),
            ActualNonce = aetx:nonce(Tx),
            case ActualNonce =:= Nonce of
                true -> ok;
                false -> error({wrong_nonce, ActualNonce, Nonce})
            end
        end,
    [{fee_computation, {Opts, CheckFun}} |Config];
init_per_group(_Grp, Config) ->
    Config.

end_per_group(sc_contracts, Config) ->
    sc_ws_close_(Config);
end_per_group(with_meta, Config) ->
    sc_ws_close_(Config);
end_per_group(Group, Config) when Group =:= only_one_signs;
                                  Group =:= both_sign ->
    sc_ws_close_(Config);
end_per_group(with_open_channel, Config) ->
    sc_ws_close_(Config);
end_per_group(_Grp, _Config) ->
    ok.

-define(DOC_LOG, [ sc_ws_ping_pong, sc_ws_deposit, sc_ws_withdraw,
                   sc_ws_get_history,
                   sc_ws_failed_update, sc_ws_generic_messages,
                   sc_ws_conflict_two_offchain_updates,
                   sc_ws_conflict_deposit_and_offchain_update,
                   sc_ws_conflict_two_deposits,
                   sc_ws_conflict_withdrawal_and_offchain_update,
                   sc_ws_conflict_withdrawal_and_deposit,
                   sc_ws_conflict_two_withdrawals,
                   sc_ws_conflict_on_new_offchain,
                   sc_ws_basic_contracts, sc_ws_oracle_contract, sc_ws_nameservice_contract,
                   sc_ws_environment_contract, sc_ws_remote_call_contract,
                   sc_ws_remote_call_contract_refering_onchain_data,
                   sc_ws_wrong_call_data, sc_ws_wrong_abi ]).

init_per_testcase(Case, Config) ->
    case proplists:is_defined(sophia_version, Config) of
        true  -> aect_test_utils:setup_testcase(Config);
        false -> ok
    end,
    case lists:member(Case, ?DOC_LOG) of
        true ->
            LogFile = docs_log_file(Config),
            #{initiator := IConnPid,
              responder := RConnPid} = proplists:get_value(channel_clients, Config),
            ?WS:set_log_file(IConnPid, LogFile),
            ?WS:set_log_file(RConnPid, LogFile);
        false ->
            ok
    end,
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    ?CHECK_INFO(20),
    [{tc_start, os:timestamp()} | Config].

end_per_testcase(_Case, Config) ->
    case ?CHECK_INFO(20) of
        [] -> ok;
        [_|_] = L ->
            ct:comment("~p unhandled messages", [length(L)])
    end,
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p",
           [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
             || {_,N} <- ?config(nodes, Config)]]),
    ok.

start_node(Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),

    {ok, 404, _} = get_balance_at_top(),
    aecore_suite_utils:mine_key_blocks(Node, 10),

    Config1 = lists:keystore(sc_ws_protocol, 1, Config, {sc_ws_protocol, <<"json-rpc">>}),

    [{node, Node} | Config1].

reset_participants(Grp, Config) ->
    Node = ?config(node, Config),

    StartAmt = 70000000000 * aec_test_utils:min_gas_price(),
        %% case aect_test_utils:latest_protocol_version() >= ?LIMA_PROTOCOL_VSN of
        %%     false -> 50000000000 * aec_test_utils:min_gas_price();
        %%     true  ->
        %%         100 * 50000000000 * aec_test_utils:min_gas_price()
        %% end,

    Initiator = {IPub, IPriv} = aecore_suite_utils:generate_key_pair(),
    Responder = {RPub, RPriv} = aecore_suite_utils:generate_key_pair(),

    ITx = initialize_account(StartAmt, Initiator, false),
    RTx = initialize_account(StartAmt, Responder, false),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ITx, RTx], ?MAX_MINED_BLOCKS),

    Participants = #{initiator => #{pub_key => IPub,
                                    priv_key => IPriv,
                                    start_amt => StartAmt},
                     responder => #{pub_key => RPub,
                                    priv_key => RPriv,
                                    start_amt => StartAmt}},
    %% New participants need to bump the port to not risk colliding.
    Config1 = [{participants, Participants} | proplists:delete(participants, Config)],
    inc_group_port(Grp, Config1).

inc_group_port(Grp, Config) ->
    %% Note that we are dealing with a group hierarchy that will add different
    %% offsets for each level.
    Offset = erlang:phash2(Grp, 999) + 1,
    [{ws_port, ?config(ws_port, Config) + Offset}|Config].


stop_node(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config).

%% ============================================================
%% Test cases
%% ============================================================
get_key_blocks_current_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current", []).

get_accounts_by_pubkey_sut(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

get_transactions_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ http_uri:encode(Hash), []).

post_transactions_sut(Tx) ->
    Host = external_address(),
    http_request(Host, post, "transactions", #{tx => Tx}).

get_names_entry_by_name_sut(Name) ->
    Host = external_address(),
    http_request(Host, get, "names/" ++ Name, []).

get_oracles_by_pubkey_sut(Pubkey) ->
    Host = external_address(),
    http_request(Host, get, "oracles/" ++ http_uri:encode(Pubkey), []).

get_balance_at_top() ->
    EncodedPubKey = get_pubkey(),
    get_accounts_by_pubkey_sut(EncodedPubKey).

post_tx(TxHash, Tx) ->
    {ok, 200, Resp} = post_transactions_sut(Tx),
    ?assertEqual(TxHash, maps:get(<<"tx_hash">>, Resp)),
    Fun = fun() -> tx_in_mempool(TxHash) end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    ok.

%% ============================================================
%% Websocket tests
%% ============================================================

%%
%% Channels
%%
assert_balance(Pubkey, ExpectedBalance) ->
    assert_balance(Pubkey, ExpectedBalance, equal).

assert_balance_at_most(Pubkey, ExpectedBalance) ->
    assert_balance(Pubkey, ExpectedBalance, equal_or_less).

assert_balance_at_least(Pubkey, MaxExpectedBalance) ->
    assert_balance(Pubkey, MaxExpectedBalance, equal_or_greater).

assert_balance(Pubkey, ExpectedBalance, Action) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := ActualBalance}} =
        get_accounts_by_pubkey_sut(Address),
    Res =
        case Action of
            equal -> ExpectedBalance =:= ActualBalance;
            equal_or_greater -> ActualBalance >= ExpectedBalance;
            equal_or_less -> ActualBalance =< ExpectedBalance
        end,
    {true, _, _, _} = {Res, ActualBalance, Action, ExpectedBalance}.

assert_trees_balance(Trees, Pubkey, ExpectedBalance) ->
    AccTrees = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(Pubkey, AccTrees),
    ExpectedBalance = aec_accounts:balance(Account).

channel_sign_tx(Pubkey, ConnPid, Privkey, Method, Config) ->
    {ok, _, Tag, #{<<"signed_tx">> := EncSignedTx,
                   <<"updates">>   := Updates}} = wait_for_channel_event(ConnPid, sign, Config),
    channel_sign_tx_(EncSignedTx, Updates, Tag, Method, Pubkey, ConnPid, Privkey,
                     Config).

channel_sign_tx_(EncSignedTx0, Updates, Tag, Method, Pubkey, ConnPid, Privkey, Config) ->
    Method = <<"channels.", (bin(Tag))/binary>>,
    {ok, SignedTxBin} = aeser_api_encoder:safe_decode(transaction, EncSignedTx0),
    SignedTx0 = aetx_sign:deserialize_from_binary(SignedTxBin),
    Tx = aetx_sign:innermost_tx(SignedTx0),
    SignedTx =
        case account_type(Pubkey) of
            basic -> aec_test_utils:co_sign_tx(SignedTx0, Privkey);
            generalized ->
                MetaTx = simple_auth_meta(Pubkey, ?SIMPLE_AUTH_GA_SECRET,
                                          "1", % make this incremental
                                          Tx,
                                          SignedTx0),
                MetaTx
        end,
    EncSignedTx =
        aeser_api_encoder:encode(transaction,
                                 aetx_sign:serialize_to_binary(SignedTx)),
    ws_call_async_method(ConnPid, Method, #{signed_tx => EncSignedTx}, Config),
    #{tx => Tx, signed_tx => SignedTx, updates => Updates}.

channel_abort_sign_tx(ConnPid, Code, Method, Config) ->
    {ok, _, Tag, #{ <<"signed_tx">> := _EncSignedTx0
                  , <<"updates">>   := _Updates }} = wait_for_channel_event(ConnPid, sign, Config),
    Method = <<"channels.", (bin(Tag))/binary>>,
    ws_call_async_method(ConnPid, Method, #{error => Code}, Config),
    ok.

sc_ws_open_(Config) ->
    sc_ws_open_(Config, #{}).

sc_ws_open_(Config, Opts) ->
    sc_ws_open_(Config, Opts, ?DEFAULT_MIN_DEPTH).


sc_ws_open_(Config, ChannelOpts0, MinBlocksToMine) ->
     sc_ws_open_(Config, ChannelOpts0, MinBlocksToMine, default_dir).

sc_ws_open_(Config, ChannelOpts0, MinBlocksToMine, LogDir) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),
    OptionallyPingPong = optionally_ping_pong(Config),

    {ok, 200, #{<<"balance">> := IStartAmt}} =
                 get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := RStartAmt}} =
                 get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, RPubkey)),
    IAmt = maps:get(initiator_amount, ChannelOpts0,
                    70000 * aec_test_utils:min_gas_price()),
    RAmt = maps:get(responder_amount, ChannelOpts0,
                    40000 * aec_test_utils:min_gas_price()),

    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    TestEvents = sc_ws_open_events(),
    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt, ChannelOpts0, Config),
    {IChanOpts, RChanOpts} = special_channel_opts(ChannelOpts),
    ct:log("IChanOpts = ~p~n"
           "RChanOpts = ~p~n", [IChanOpts, RChanOpts]),
    %% We need to register for some events as soon as possible - otherwise a race may occur where
    %% some fsm messages are missed
    {ok, IConnPid, IFsmId} = channel_ws_start(initiator,
                                              maps:put(host, <<"localhost">>, IChanOpts),
                                              Config, TestEvents, LogDir),

    ct:log("initiator spawned", []),
    OptionallyPingPong(IConnPid),

    {ok, RConnPid, RFsmId} = channel_ws_start(responder, RChanOpts, Config,
                                              TestEvents, LogDir),
    ct:log("responder spawned", []),
    OptionallyPingPong(RConnPid),

    channel_send_conn_open_infos(RConnPid, IConnPid, Config),

    SignedCrTx = channel_create(Config, IConnPid, RConnPid),
    CrTxHash = aetx_sign:hash(SignedCrTx),
    CrTx = aetx_sign:innermost_tx(SignedCrTx),
    {ok, ChPubkey} = aesc_utils:channel_pubkey(SignedCrTx),
    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    ChId = aeser_api_encoder:encode(channel, ChPubkey),
    IPubkey = aesc_create_tx:initiator_pubkey(Tx),
    RPubkey = aesc_create_tx:responder_pubkey(Tx),

    %% We stuff the channel id into the Clients map out of convenience
    ChannelClients = #{ channel_id => ChId
                      , initiator  => IConnPid
                      , initiator_fsm_id => IFsmId
                      , responder  => RConnPid
                      , responder_fsm_id => RFsmId},
    Config1 = [{channel_clients, ChannelClients},
               {channel_start_amounts, #{ initiator => IStartAmt
                                        , responder => RStartAmt }},
               {create_tx, SignedCrTx},
               {create_tx_hash, CrTxHash},
               {min_blocks_to_mine, MinBlocksToMine},
               {channel_options, ChannelOpts} | Config],
    %%
    case proplists:get_value(mine_create_tx, Config, true) of
        true ->
            finish_sc_ws_open(Config1, MinBlocksToMine, false);
        false ->
            ok = ?WS:unregister_test_for_channel_events(IConnPid, TestEvents),
            ok = ?WS:unregister_test_for_channel_events(RConnPid, TestEvents)
    end,
    Config1.

optionally_ping_pong(Config) ->
    TestPings = proplists:get_value(ping_pong, Config, false),
    fun(ConnPid) when TestPings =:= true -> ping_pong(ConnPid, Config);
       (_) -> pass
    end.

optionally_ping_pong(IConnPid, RConnPid, Config) ->
    TestPings = proplists:get_value(ping_pong, Config, false),
    fun() when TestPings =:= true ->
            ping_pong(IConnPid, Config),
            ping_pong(RConnPid, Config);
       () -> pass
    end.

sc_ws_open_events() ->
    [info, get, sign, on_chain_tx, update].

finish_sc_ws_open(Config, MinBlocksToMine) ->
    finish_sc_ws_open(Config, MinBlocksToMine, true).

finish_sc_ws_open(Config, MinBlocksToMine, Register) ->
    ct:log("finish_sc_ws_open", []),
    #{ channel_id := ChId
     , initiator  := IConnPid
     , responder  := RConnPid } = proplists:get_value(channel_clients, Config),
    #{ initiator_amount := IAmt
     , responder_amount := RAmt } = ChannelOpts = proplists:get_value(channel_options, Config),
    SignedCrTx = proplists:get_value(create_tx, Config),
    %%
    TestEvents = sc_ws_open_events(),
    case Register of
        true ->
            ok = ?WS:register_test_for_channel_events(IConnPid, TestEvents),
            ok = ?WS:register_test_for_channel_events(RConnPid, TestEvents);
        false ->
            ok
    end,
    %%
    ok = maybe_mine_create_tx(SignedCrTx, Config, IConnPid, RConnPid),
    CrTx = aetx_sign:innermost_tx(SignedCrTx),
    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    ChannelCreateFee = aesc_create_tx:fee(Tx),
    IPubkey = aesc_create_tx:initiator_pubkey(Tx),
    RPubkey = aesc_create_tx:responder_pubkey(Tx),
    #{ initiator := IStartAmt
     , responder := RStartAmt } = proplists:get_value(channel_start_amounts, Config),
    %%
    make_two_gen_messages_volleys(IConnPid, IPubkey, RConnPid,
                                  RPubkey, Config),

    {ok, {IBal, RBal}} = sc_ws_get_both_balances(IConnPid,
                                                 IPubkey,
                                                 RPubkey,
                                                 Config),
    %% assert off-chain balances
    PushAmt = maps:get(push_amount, ChannelOpts),
    IBal = IAmt - PushAmt,
    RBal = RAmt + PushAmt,

    %% ensure new balances
    assert_balance_at_most(IPubkey, IStartAmt - IAmt - ChannelCreateFee),
    assert_balance_at_most(RPubkey, RStartAmt - RAmt),

    %% mine min depth
    case proplists:get_bool(assume_min_depth, Config) of
        true ->
            tell_participants_to_assume_min_depth(
              [IConnPid, RConnPid], SignedCrTx);
        false ->
            aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                               MinBlocksToMine)
    end,

    {ok, ChId} = channel_send_locking_infos(IConnPid, RConnPid, Config),

    channel_send_chan_open_infos(RConnPid, IConnPid, Config),
    %%
    ok = ?WS:unregister_test_for_channel_events(IConnPid, TestEvents),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, TestEvents),
    %%
    ok.

maybe_collect_deferred_min_depth_msg(Config) ->
    case proplists:get_bool(assume_min_depth, Config) of
        true ->
            collect_deferred_min_depth_msg(Config);
        false ->
            ok
    end.

collect_deferred_min_depth_msg(Config) ->
    {_, MinBlocksToMine} = lists:keyfind(min_blocks_to_mine, 1, Config),
    #{ initiator := IConnPid
     , responder := RConnPid } = proplists:get_value(channel_clients, Config),
    {_, CrTxHash} = lists:keyfind(create_tx_hash, 1, Config),
    EncHash = aeser_api_encoder:encode(tx_hash, CrTxHash),
    with_registered_events(
      [info], [IConnPid, RConnPid],
      fun() ->
              aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                                 MinBlocksToMine),
              {ok, E} = wait_for_channel_event_full(IConnPid, info, Config),
              ct:log("Deferred min-depth Msg E = ~p", [E]),
              #{data := #{ <<"event">> := <<"minimum_depth_achieved">>
                         , <<"tx_hash">> := EncHash }} = E,
              {ok, E} = wait_for_channel_event_full(RConnPid, info, Config),
              ok
      end).

make_two_gen_messages_volleys(IConnPid, IPubkey, RConnPid,
                                  RPubkey, Config) ->
    make_msg_round(IConnPid, IPubkey, RConnPid, RPubkey,
                   <<"Hello">>, Config),
    make_msg_round(RConnPid, RPubkey, IConnPid, IPubkey,
                   <<"Hello back">>, Config),
    ok.

make_msg_round(SenderPid, SenderPubkey, ReceiverPid, ReceiverPubkey, Msg, Config) ->
    ok = ?WS:register_test_for_channel_events(SenderPid, [message]),
    ok = ?WS:register_test_for_channel_events(ReceiverPid, [message]),
    SenderEncodedK = aeser_api_encoder:encode(account_pubkey, SenderPubkey),
    ReceiverEncodedK = aeser_api_encoder:encode(account_pubkey, ReceiverPubkey),
    ws_send_tagged(SenderPid, <<"channels.message">>,
            #{<<"to">> => ReceiverEncodedK,
              <<"info">> => Msg}, Config),
    {ok, _, #{<<"message">> := #{ <<"from">> := SenderEncodedK
                                , <<"to">> := ReceiverEncodedK
                                , <<"info">> := Msg }}}
        = wait_for_channel_event(ReceiverPid, message, Config),
    ok = ?WS:unregister_test_for_channel_events(SenderPid, [message]),
    ok = ?WS:unregister_test_for_channel_events(ReceiverPid, [message]),
    ok.

channel_send_conn_open_infos(RConnPid, IConnPid, Config) ->
    {ok, _, #{<<"event">> := <<"channel_open">>}} = wait_for_channel_event(RConnPid, info, Config),
    {ok, _, #{<<"event">> := <<"channel_accept">>}} = wait_for_channel_event(IConnPid, info, Config),
    ok.

channel_send_locking_infos(IConnPid, RConnPid, Config) ->
    {ok, #{channel_id := ChId,
           data := #{<<"event">> := IE1}}}
        = wait_for_channel_event_full(IConnPid, info, Config),
    {ok, #{channel_id := ChId,
           data := #{<<"event">> := RE1}}}
        = wait_for_channel_event_full(RConnPid, info, Config),
    {ok, #{channel_id := ChId,
           data := #{<<"event">> := IE2}}}
        = wait_for_channel_event_full(IConnPid, info, Config),
    {ok, #{channel_id := ChId,
           data := #{<<"event">> := RE2}}}
        = wait_for_channel_event_full(RConnPid, info, Config),

    MF = fun(<<"own_funding_locked">>, <<"funding_locked">>) -> ok end,
    MF(IE1, IE2),
    MF(RE1, RE2),
    {ok, ChId}.

channel_send_chan_open_infos(RConnPid, IConnPid, Config) ->
    {ok, _, #{<<"event">> := <<"open">>}} = wait_for_channel_event(IConnPid, info, Config),
    {ok, _, #{<<"event">> := <<"open">>}} = wait_for_channel_event(RConnPid, info, Config),

    %% Assert we receive an update
    {ok, _, #{<<"state">> := InitialState}} = wait_for_channel_event(IConnPid, update, Config),
    {ok, _, #{<<"state">> := InitialState}} = wait_for_channel_event(RConnPid, update, Config),

    %% Assert we received a channel_create_tx
    {ok, InitialStateTxBin} = aeser_api_encoder:safe_decode(transaction, InitialState), InitialStateTx = aetx_sign:deserialize_from_binary(InitialStateTxBin),
    Tx = aetx_sign:innermost_tx(InitialStateTx),
    {aesc_create_tx, _} = aetx:specialize_callback(Tx),

    ok.

channel_participants_balances(IPubkey, RPubkey) ->
    {ok, 200, #{<<"balance">> := BalI}} =
        get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := BalR}} =
        get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, RPubkey)),
    {BalI, BalR}.

channel_create(Config, IConnPid, RConnPid) ->
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants, Config),
    OptionallyPingPong = optionally_ping_pong(IConnPid, RConnPid, Config),
    %% initiator gets to sign a create_tx
    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),
    OptionallyPingPong(),

    #{ tx := CrTx
     , updates := Updates } = channel_sign_tx(IPubkey, IConnPid, IPrivkey, <<"channels.initiator_sign">>, Config),
    {ok, _, #{<<"event">> := <<"funding_created">>}} = wait_for_channel_event(RConnPid, info, Config),
    %% responder gets to sign a create_tx
    OptionallyPingPong(),
    #{ tx := CrTx
     , updates := Updates } = channel_sign_tx(RPubkey, RConnPid, RPrivkey, <<"channels.responder_sign">>, Config),
    {ok, _, #{<<"event">> := <<"funding_signed">>}} = wait_for_channel_event(IConnPid, info, Config),
    OptionallyPingPong(),

    %% both of them receive the same co-signed channel_create_tx
    {ok, _, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, _, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),

    OptionallyPingPong(),
    {ok, SSignedCrTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedCrTx),
    SignedCrTx = aetx_sign:deserialize_from_binary(SSignedCrTx),
    %% same transaction
    CrTx = aetx_sign:innermost_tx(SignedCrTx),

    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    IPubkey = aesc_create_tx:initiator_pubkey(Tx),
    RPubkey = aesc_create_tx:responder_pubkey(Tx),
    ct:log("ChID (channel_create()) = ~p", [aeser_api_encoder:encode(
                                              channel, aesc_create_tx:channel_pubkey(Tx))]),

    %% ensure the tx is in the mempool
    ok = wait_for_signed_transaction_in_pool(SignedCrTx),

    %% balances hadn't changed yet
    assert_balance(IPubkey, IStartAmt),
    assert_balance(RPubkey, RStartAmt),

    case proplists:get_value(mine_create_tx, Config, true) of
        true ->
            ct:log("Will mine create_tx", []),
            ok = channel_create_mined(SignedCrTx, Config, IConnPid, RConnPid);
        false ->
            ct:log("Will not mine create_tx at this time", []),
            ok
    end,
    SignedCrTx.

maybe_mine_create_tx(SignedCrTx, Config, IConnPid, RConnPid) ->
    case proplists:get_value(mine_create_tx, Config, true) of
        false ->
            %% This means we didn't mine it before - do it now!
            channel_create_mined(SignedCrTx, Config, IConnPid, RConnPid);
        true ->
            ok
    end.

tell_participants_to_assume_min_depth(ConnPids, SignedTx) ->
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    ct:log("Will tell ~p to assume min_depth for ~p", [ConnPids, TxHash]),
    [ ok = assume_min_depth(Pid, TxHash) || Pid <- ConnPids],
    ok.

assume_min_depth(Pid, TxHash) ->
    case ?WS:json_rpc_call(
            Pid, #{ <<"method">> => <<"channels.assume_minimum_depth">>
                  , <<"params">> => #{ <<"tx_hash">> => TxHash } }) of
        <<"ok">> -> ok;
        Other    -> {error, Other}
    end.

channel_create_mined(SignedCrTx, Config, IConnPid, RConnPid) ->
    % mine the create_tx
    ok = wait_for_signed_transaction_in_block(SignedCrTx),

    {ok, _, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, _, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),
    (optionally_ping_pong(IConnPid, RConnPid, Config))(),
    ok.

sc_ws_update_(Config) ->
    Participants = proplists:get_value(participants, Config),
    Conns = proplists:get_value(channel_clients, Config),
    lists:foldl(
        fun(Sender, Round) ->
            channel_update(Conns, Sender, Participants, 1, Round, Config),
            Round + 1
        end,
        2, % we start from round 2
        [initiator,
         responder,
         responder,
         initiator,
         responder]),
    ok.

sc_ws_update_basic_round_(Round0, Config) ->
    Participants = proplists:get_value(participants, Config),
    Conns = proplists:get_value(channel_clients, Config),
    lists:foldl(
      fun(Role, Round) ->
              channel_update(Conns, Role, Participants, 1,
                             Round, _TestErrors = false, Config),
              Round + 1
      end, Round0, [initiator, responder]).

channel_conflict(#{initiator := IConnPid, responder :=RConnPid},
                 StarterRole,
                 #{initiator := #{pub_key := IPubkey,
                                  priv_key := IPrivkey},
                   responder := #{pub_key := RPubkey,
                                  priv_key := RPrivkey}},
                Config,
                TriggerFun) ->
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey, RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey, IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, error, conflict]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, error, conflict]),

    SignUpdate =
        fun TrySignUpdate(ConnPid, Privkey, EncSignedTx0, Method) ->
                {ok, SignedBinTx} =
                    aeser_api_encoder:safe_decode(transaction, EncSignedTx0),
                STx = aetx_sign:deserialize_from_binary(SignedBinTx),
                SignedTx = aec_test_utils:co_sign_tx(STx, Privkey),
                EncSignedTx = aeser_api_encoder:encode(transaction,
                                              aetx_sign:serialize_to_binary(SignedTx)),
                ws_send_tagged(ConnPid, Method,
                                #{signed_tx => EncSignedTx}, Config)
        end,
    TriggerFun(StarterPid, StarterPubkey, StarterPrivkey,
               AcknowledgerPid, AcknowledgerPubkey, AcknowledgerPrivkey,
               SignUpdate),
    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, error, conflict]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, error, conflict]),

    ok.

channel_update(Conns, Starter, Participants, Amount, Round, Config) ->
    channel_update(Conns, Starter, Participants, Amount, Round,
                   _TestErrors = true, Config).

channel_update(Conns, Starter, Participants, Amount, Round,
               TestErrors, Config) ->
    channel_update(Conns, Starter, Participants, Amount, Round,
                   TestErrors, Config, #{}).

channel_update(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount, _Round, TestErrors, Config, UpdateOpts0) ->
    true = undefined =/= process_info(IConnPid),
    true = undefined =/= process_info(RConnPid),
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey,
                                    RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey,
                                    IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, update,
                                                         get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, update,
                                                         error]),

    %% sender initiates an update
    GetBothBalances = fun(ConnPid) ->
                              sc_ws_get_both_balances(
                                ConnPid, StarterPubkey, AcknowledgerPubkey, Config)
                      end,
    {ok, {Ba0, Bb0} = Bal0} = GetBothBalances(IConnPid),
    ct:log("Balances before: ~p", [Bal0]),
    UpdateOpts1 =
        UpdateOpts0#{ from => aeser_api_encoder:encode(account_pubkey, StarterPubkey)
                    , to => aeser_api_encoder:encode(account_pubkey, AcknowledgerPubkey)
                    , amount => Amount },
    UpdateOpts = maybe_include_meta(UpdateOpts1, Config),
    if TestErrors ->
            ws_send_tagged(StarterPid, <<"channels.update.new">>,
                           UpdateOpts#{amount => <<"1">>}, Config),
            {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
                wait_for_channel_event(StarterPid, error, Config),
            ws_send_tagged(StarterPid, <<"channels.update.new">>,
                           UpdateOpts#{from => <<"ABCDEF">>}, Config),
            {ok, _, #{<<"reason">> := <<"broken_encoding: accounts">>}} =
                wait_for_channel_event(StarterPid, error, Config),
            ws_send_tagged(StarterPid, <<"channels.update.new">>,
                           UpdateOpts#{to => <<"ABCDEF">>}, Config),
            {ok, _, #{<<"reason">> := <<"broken_encoding: accounts">>}} =
                wait_for_channel_event(StarterPid, error, Config),
            ws_send_tagged(StarterPid, <<"channels.update.new">>,
                           UpdateOpts#{meta => 17}, Config),
            {ok, _, #{<<"reason">> := <<"Invalid meta object">>}} =
                wait_for_channel_event(StarterPid, error, Config);
       true -> ok
    end,
    ws_send_tagged(StarterPid, <<"channels.update.new">>,
                   UpdateOpts, Config),

    %% starter signs the new state
    #{tx := UnsignedStateTx,
      updates := UpdatesS} = channel_sign_tx(StarterPubkey, StarterPid, StarterPrivkey, <<"channels.update">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    %% verify contents
    {channel_offchain_tx, _OffchainTx} = aetx:specialize_type(UnsignedStateTx),
    IncludeMeta = include_meta(Config),
    ok = verify_updates(UpdatesS, StarterPubkey, AcknowledgerPubkey, Amount, IncludeMeta),

    %% acknowledger signs the new state
    {ok, _, #{<<"event">> := <<"update">>}} = wait_for_channel_event(AcknowledgerPid, info, Config),
    #{tx := UnsignedStateTx,
      updates := UpdatesR} = channel_sign_tx(AcknowledgerPubkey, AcknowledgerPid, AcknowledgerPrivkey, <<"channels.update_ack">>, Config),
    {UpdatesR, UpdatesS} = {UpdatesS, UpdatesR},   % verify that they are identical

    {ok, _, #{<<"state">> := NewState}} = wait_for_channel_event(IConnPid, update, Config),
    {ok, _, #{<<"state">> := NewState}} = wait_for_channel_event(RConnPid, update, Config),
    {ok, SignedStateTxBin} = aeser_api_encoder:safe_decode(transaction, NewState),
    SignedStateTx = aetx_sign:deserialize_from_binary(SignedStateTxBin),

    %% validate it is co-signed
    {ok, Trees} = rpc(aec_chain, get_top_state, []),
    ok = rpc(aetx_sign, verify, [SignedStateTx, Trees, 1]), % RPC because of DB
    {UnsignedStateTx, _} = % same transaction that was signed
        {aetx_sign:innermost_tx(SignedStateTx), UnsignedStateTx},

    {ok, {Ba1, Bb1} = Bal1} = GetBothBalances(IConnPid),
    ct:log("Balances after: ~p", [Bal1]),
    Ba1 = Ba0 - Amount,
    Bb1 = Bb0 + Amount,

    {ok, #{ trees := StateTrees
          , signed_tx := StateSignedStateTx }} = sc_ws_get_state(IConnPid, Config),
    SignedStateTx = StateSignedStateTx,
    assert_trees_balance(StateTrees, StarterPubkey, Ba1),
    assert_trees_balance(StateTrees, AcknowledgerPubkey, Bb1),

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, info, update,
                                                           get, error]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, info, update,
                                                           error]),

    ok.

verify_updates(Updates, StarterPubkey, AcknowledgerPubkey, Amount, false) ->
    [Update] = Updates,
    Expected = aesc_offchain_update:op_transfer(
                 aeser_id:create(account, StarterPubkey),
                 aeser_id:create(account, AcknowledgerPubkey), Amount),
    ExpectedForClient = aesc_offchain_update:for_client(Expected),
    {ExpectedForClient, _} = {Update, ExpectedForClient},
    ok;
verify_updates(Updates, StarterPubkey, AcknowledgerPubkey, Amount, true) ->
    [Update, Meta] = Updates,
    ok = validate_meta(Meta),
    ExpUpd = aesc_offchain_update:op_transfer(
               aeser_id:create(account, StarterPubkey),
               aeser_id:create(account, AcknowledgerPubkey), Amount),
    ExpUpdForClient = aesc_offchain_update:for_client(ExpUpd),
    {ExpUpdForClient, _} = {Update, ExpUpdForClient},
    ok.

channel_update_fail(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey},
                 responder := #{pub_key := RPubkey}},
               Amount, Config) ->
    {StarterPid, _AcknowledgerPid, StarterPubkey, AcknowledgerPubkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, RPubkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, IPubkey}
        end,
    ok = ?WS:register_test_for_channel_event(StarterPid, error),

    %% sender initiates an update
    ws_send_tagged(StarterPid, <<"channels.update.new">>,
                   #{from => aeser_api_encoder:encode(account_pubkey, StarterPubkey),
                     to => aeser_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                     amount => Amount}, Config),

    {ok, _, _Payload}= Res = wait_for_channel_event(StarterPid, error, Config),


    ok = ?WS:unregister_test_for_channel_event(StarterPid, error),
    Res.

sc_ws_close_(ConfigList) ->
    %% We cannot simply close the ConnPid if we want to also bring down the fsm
    %% since the fsm may be configured to allow the client to disconnect/reconnect.
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),
    Close = fun(Pid) ->
                    %% send a `channels.system` stop message. It does not
                    %% return a result. We rather wait for the channel FSM to
                    %% die and to close the WebSocket connection.
                    %% The actual WebSocket closing is the preferred solution,
                    %% opposed to waiting a `died` WebSocket message because
                    %% even if the `died` message is received, the WebSocket
                    %% could still be running for a few milliseconds
                    try ?WS:json_rpc_call(
                           Pid, #{ <<"method">> => <<"channels.system">>
                                 , <<"params">> => #{<<"action">> => <<"stop">>}})
                    catch
                        %% when the WebSocket process dies, it emmits a
                        %% {connpid_died, Reason} message
                        error:{connpid_died, Reason} when Reason == {error, closed}
                                                        ; Reason == {error, einval}
                                                        ; Reason == noproc
                                                        ; Reason == normal ->
                            ok
                    end
            end,
    Close(RConnPid),
    Close(IConnPid),
    ok.

sc_ws_get_balance(ConnPid, PubKey, Config) ->
    Account = aeser_api_encoder:encode(account_pubkey, PubKey),
    {ok, Res} = query_balances(ConnPid, [Account], Config),
    [#{<<"account">> := Account, <<"balance">> := B}] = Res,
    {ok, B}.

sc_ws_get_both_balances(ConnPid, PubKeyI, PubKeyR, Config) ->
    AccountI = aeser_api_encoder:encode(account_pubkey, PubKeyI),
    AccountR = aeser_api_encoder:encode(account_pubkey, PubKeyR),
    {ok, Res} = query_balances(ConnPid, [AccountI, AccountR], Config),
    [#{<<"account">> := AccountI, <<"balance">> := BI},
     #{<<"account">> := AccountR, <<"balance">> := BR}] = Res,
    {ok, {BI, BR}}.

query_balances(ConnPid, Accounts, Config) ->
    query_balances_(ConnPid, Accounts, sc_ws_protocol(Config)).

query_balances_(ConnPid, Accounts, <<"json-rpc">>) ->
    {ok, ?WS:json_rpc_call(
            ConnPid, #{ <<"method">> => <<"channels.get.balances">>
                      , <<"params">> => #{<<"accounts">> => Accounts} })}.

request_slash(ConnPid, Params) ->
    {ok, ?WS:json_rpc_call(
            ConnPid, #{ <<"method">> => <<"channels.slash">>
                      , <<"params">> => Params })}.

sc_ws_get_state(ConnPid, Config) ->
    {ok, Res} = query_state(ConnPid, Config),
    #{ <<"trees">> := EncodedTrees
     , <<"calls">> := EncodedCalls
     , <<"signed_tx">> := EncodedSignedTx
     , <<"half_signed_tx">> := EncodedHalfSignedTx
     } = Res,
    {ok, STrees} = aeser_api_encoder:safe_decode(state_trees, EncodedTrees),
    Trees = aec_trees:deserialize_from_binary_without_backend(STrees),
    {ok, SCalls} = aeser_api_encoder:safe_decode(call_state_tree, EncodedCalls),
    Calls = aect_call_state_tree:from_binary_without_backend(SCalls),
    {ok, #{ trees => Trees
          , calls => Calls
          , signed_tx => decode_signed_tx(EncodedSignedTx)
          , half_signed_tx => decode_signed_tx(EncodedHalfSignedTx)}}.

decode_signed_tx(<<>>) ->
    no_tx;
decode_signed_tx(EncodedSignedTx) ->
    {ok, SSignedTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedTx),
    aetx_sign:deserialize_from_binary(SSignedTx).

query_state(ConnPid, Config) ->
    query_state_(ConnPid, sc_ws_protocol(Config)).

query_state_(ConnPid, <<"json-rpc">>) ->
    {ok, ?WS:json_rpc_call(
        ConnPid, #{ <<"method">> => <<"channels.get.offchain_state">>
                  , <<"params">> => #{} })}.

sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, Pubkey, Config) ->
    ContractState = sc_ws_get_contract(ConnPid1, Pubkey, Config),
    ContractState = sc_ws_get_contract(ConnPid2, Pubkey, Config),
    ContractState.

sc_ws_get_contract(ConnPid, Pubkey, Config) ->
    PubkeyE = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    {ok, #{<<"contract">> := ContractE,
           <<"contract_state">> := ContractStateE }} = query_contract(ConnPid, PubkeyE, Config),
    #{ <<"id">>          := IdE
    , <<"owner_id">>     := OwnerIdE
    , <<"vm_version">>   := VMVersion
    , <<"abi_version">>  := ABIVersion
    , <<"active">>       := Active
    , <<"referrer_ids">> := ReferrerIdsE
    , <<"deposit">>      := Deposit
    } = ContractE,
    {ok, Id} = aeser_api_encoder:safe_decode({id_hash, [contract_pubkey]}, IdE),
    {ok, OwnerId} = aeser_api_encoder:safe_decode({id_hash, [account_pubkey]}, OwnerIdE),
    ReferrerIds = [ RId || {ok, RId} <- [aeser_api_encoder:safe_decode({id_hash, [contract_pubkey]}, RId) || RId <- ReferrerIdsE]],
    ContractState = maps:from_list([
        {Key, Val} || {{ok, Key}, {ok, Val}}
        <- [ {aeser_api_encoder:safe_decode(contract_store_key, Key),
              aeser_api_encoder:safe_decode(contract_store_value, Val)} || {Key, Val} <- maps:to_list(ContractStateE) ]
    ]),
    #{ id => Id
     , owner_id => OwnerId
     , vm_version => VMVersion
     , abi_version => ABIVersion
     , active => Active
     , referrer_ids => ReferrerIds
     , deposit => Deposit
     , state => ContractState
    }.

query_contract(ConnPid, Pubkey, Config) ->
    query_contract_(ConnPid, Pubkey, sc_ws_protocol(Config)).

query_contract_(ConnPid, Pubkey, <<"json-rpc">>) ->
    {ok, ?WS:json_rpc_call(
        ConnPid, #{ <<"method">> => <<"channels.get.contract">>
                  , <<"params">> => #{<<"pubkey">> => Pubkey}})}.

sc_ws_close_mutual_(Config0) ->
    lists:foreach(
        fun(WhoCloses) ->
            Config = sc_ws_open_(Config0,
                                 #{initiator_amount => ?ARBITRARY_BIG_FEE * 2,
                                   responder_amount => ?ARBITRARY_BIG_FEE * 2}),
            sc_ws_close_mutual_(Config, WhoCloses)
        end,
        [initiator, responder]).

sc_ws_close_mutual_(Config, Closer) ->
    sc_ws_close_mutual_(Config, Closer, #{}).

sc_ws_close_mutual_(Config, Closer, Params) when Closer =:= initiator orelse
                                                 Closer =:= responder ->
    ct:log("ConfigList = ~p", [Config]),
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants,
                                                                 Config),
    {IStartB, RStartB} = channel_participants_balances(IPubkey, RPubkey),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, error, info, on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, error, info, on_chain_tx]),
    MoreTokens = IStartB + RStartB + 100,
    ws_send_tagged( IConnPid, <<"channels.shutdown">>, Params#{fee => MoreTokens}
                  , Config),
    {ok, _, #{<<"reason">> := <<"insufficient_balance">>}} =
        wait_for_channel_event(IConnPid, error, Config),


    CloseMutual =
        fun(CloserPubkey, CloserConn, CloserPrivkey, OtherPubkey, OtherConn, OtherPrivkey) ->
                ws_send_tagged(CloserConn, <<"channels.shutdown">>, Params, Config),

                #{tx := ShTx,
                  updates := Updates} = channel_sign_tx(CloserPubkey, CloserConn, CloserPrivkey, <<"channels.shutdown_sign">>, Config),
                #{tx := ShTx,
                  updates := Updates} = channel_sign_tx(OtherPubkey, OtherConn, OtherPrivkey, <<"channels.shutdown_sign_ack">>, Config),
            ShTx
        end,
    ShutdownTx =
        case Closer of
            initiator -> CloseMutual(IPubkey, IConnPid, IPrivkey, RPubkey, RConnPid, RPrivkey);
            responder -> CloseMutual(RPubkey, RConnPid, RPrivkey, IPubkey, IConnPid, IPrivkey)
        end,

    {ok, _, #{<<"tx">> := EncodedSignedMutualTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, _, #{<<"tx">> := EncodedSignedMutualTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),
    ?PEEK_MSGQ,
    {ok, SSignedMutualTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedMutualTx),
    SignedMutualTx = aetx_sign:deserialize_from_binary(SSignedMutualTx),
    %% same transaction
    ShutdownTx = aetx_sign:tx(SignedMutualTx),
    ?PEEK_MSGQ,
    {channel_close_mutual_tx, MutualTx} = aetx:specialize_type(ShutdownTx),
    IExpectedEvents = [<<"closing">>, {optional, <<"shutdown">>}],
    RExpectedEvents = [<<"closing">>, {optional, <<"shutdown">>}],
    wait_for_expected_events(IConnPid, Config, IExpectedEvents),
    wait_for_expected_events(RConnPid, Config, RExpectedEvents),

    ok = wait_for_signed_transaction_in_pool(SignedMutualTx),

    assert_balance(IPubkey, IStartB),
    assert_balance(RPubkey, RStartB),

    ok = wait_for_signed_transaction_in_block(SignedMutualTx),
    {ok, _, #{ <<"tx">> := EncodedSignedMutualTx
             , <<"info">> := <<"channel_closed">> }} =
        wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, _, #{ <<"tx">> := EncodedSignedMutualTx
             , <<"info">> := <<"channel_closed">> }} =
        wait_for_channel_event(RConnPid, on_chain_tx, Config),

    IChange = aesc_close_mutual_tx:initiator_amount_final(MutualTx),
    RChange = aesc_close_mutual_tx:responder_amount_final(MutualTx),

    assert_balance(IPubkey, IStartB + IChange),
    assert_balance(RPubkey, RStartB + RChange),

    % ensure tx is not hanging in mempool
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),
    {ok, SignedMutualTx}.

sc_ws_close_solo_(Config, Closer, Opts) when Closer =:= initiator;
                                             Closer =:= responder ->
    ?PEEK_MSGQ,
    ct:log("ConfigList = ~p", [Config]),
    #{initiator := #{pub_key  := IPubKey,
                     priv_key := IPrivKey},
      responder := #{pub_key  := RPubKey,
                     priv_key := RPrivKey}} = proplists:get_value(participants,
                                                                      Config),
    {_IStartB, _RStartB} = channel_participants_balances(IPubKey, RPubKey),
    #{initiator := IConnPid,
      responder := RConnPid} = Conns
        = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [info]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [info]),

    CloseSolo =
        fun(CloserConn, CloserPrivKey) ->
                ws_send_tagged(CloserConn, <<"channels.close_solo">>,
                               Opts, Config),
                {ok, CSTx} = sign_tx(CloserConn, <<"close_solo_sign">>,
                                     <<"channels.close_solo_sign">>,
                                     CloserPrivKey, Config),
                CSTx
        end,
    CloseSoloTx = case Closer of
                      initiator -> CloseSolo(IConnPid, IPrivKey);
                      responder -> CloseSolo(RConnPid, RPrivKey)
                  end,
    ?PEEK_MSGQ,
    {ok, [ #{<<"tx">> := EncodedSignedSoloTx}
         , #{<<"tx">> := EncodedSignedSoloTx} ]} =
        wait_for_onchain_tx_events(
          Conns, #{},
          fun() ->
                  ok = wait_for_signed_transaction_in_block(CloseSoloTx)
          end, Config),
    ?PEEK_MSGQ,
    {ok, SSignedSoloTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedSoloTx),
    SignedSoloTx = aetx_sign:deserialize_from_binary(SSignedSoloTx),
    SoloTx = aetx_sign:tx(SignedSoloTx),
    {channel_close_solo_tx, _TxI} = aetx:specialize_type(SoloTx),

    ct:log("Close_solo tx verified", []),

    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),
    ?PEEK_MSGQ,
    ok = ?WS:unregister_test_for_channel_events(IConnPid, [info]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [info]),
    {ok, SignedSoloTx}.

settle_(Config, Closer) ->
    settle_(Config, Closer, #{}).

settle_(Config, Closer, Params) when Closer =:= initiator;
                                     Closer =:= responder ->
    #{initiator := #{priv_key := IPrivKey},
      responder := #{priv_key := RPrivKey}} =
          proplists:get_value(participants, Config),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Config),
    {ConnPid, PrivKey} = case Closer of
                             initiator -> {RConnPid, RPrivKey};
                             responder -> {IConnPid, IPrivKey}
                         end,
    ws_send_tagged(ConnPid, <<"channels.settle">>, Params, Config),

    {ok, SettleTx} = sign_tx(ConnPid, <<"settle_sign">>,
                             <<"channels.settle_sign">>, PrivKey, Config),

    ok = wait_for_signed_transaction_in_block(SettleTx),
    ?PEEK_MSGQ,
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 15),
    ?CHECK_INFO(20),
    {ok, SettleTx}.

sc_ws_leave_(Config) ->
    ResponderLeaves = proplists:get_value(responder_leaves, Config, true),
    #{ initiator := IConnPid
     , initiator_fsm_id := IFsmId
     , responder := RConnPid
     , responder_fsm_id := RFsmId } = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [leave, info]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [leave, info]),
    ok = ?WS:register_test_for_events(IConnPid, websocket, [closed]),
    ok = ?WS:register_test_for_events(RConnPid, websocket, [closed]),
    %%
    ok = ws_send_tagged(IConnPid, <<"channels.leave">>, #{}, Config),
    %%
    {ok, #{id := IDi, state := StI}} = wait_for_channel_leave_msg(IConnPid, Config),
    {ok, #{id := IDr, state := StR}} = wait_for_channel_leave_msg(RConnPid, Config),
    {IDi, IDr} = {IDr, IDi},
    {StI, StR} = {StR, StI},
    {ok, _, #{<<"event">> := <<"died">>}} = wait_for_channel_event(IConnPid, info, Config),
    if ResponderLeaves ->
            {ok, _, #{<<"event">> := <<"died">>}} = wait_for_channel_event(RConnPid, info, Config);
       true ->
            {ok, _, #{<<"event">> := <<"peer_disconnected">>}}
                = wait_for_channel_event(RConnPid, info, Config)
    end,
    ok = ?WS:wait_for_event(IConnPid, websocket, closed),
    if ResponderLeaves ->
            ok = ?WS:wait_for_event(RConnPid, websocket, closed);
       true ->
            %% RConnPid remains. Need to unregister events
            ok = ?WS:unregister_test_for_channel_events(RConnPid, [leave, info]),
            ok = ?WS:unregister_test_for_events(RConnPid, websocket, [closed]),
            ok
    end,
    %%
    Options = proplists:get_value(channel_options, Config),
    Port = maps:get(port, Options),
    RPort = if ResponderLeaves ->
                    Port+1;     % new port
               true ->
                    Port        % same port as before
            end,
    #{ existing_channel_id => IDi
     , existing_fsm_id => [{initiator, IFsmId}, {responder, RFsmId}]
     , offchain_tx => StI
     , port => RPort
     , protocol => maps:get(protocol, Options)}.

sc_ws_reestablish_(ReestablishOptions, Config) ->
    ct:log("ReestablishOptions = ~p~n"
           "Config = ~p", [ReestablishOptions, Config]),
    ResponderLeaves = proplists:get_value(responder_leaves, Config, true),
    {RrConnPid, RFsmId} = if ResponderLeaves ->
                                {ok, RrCP, FsmId} = channel_ws_start(responder, ReestablishOptions, Config),
                                {RrCP, FsmId};
                              true ->
                                %% responder still running
                                #{ responder := RCP
                                 , responder_fsm_id := FsmId} = proplists:get_value(channel_clients, Config),
                                {RCP, FsmId}
                          end,
    ReestablishOptions1 = maps:put(host, <<"localhost">>, ReestablishOptions),
    {ok, IrConnPid, IFsmId} = channel_ws_start(initiator, ReestablishOptions1, Config),
    Config1 = lists:keystore(channel_clients, 1, Config,
                            {channel_clients, #{ initiator => IrConnPid
                                               , initiator_fsm_id => IFsmId
                                               , responder => RrConnPid
                                               , responder_fsm_id => RFsmId}}),
    ok = await_reestablish_reports(Config1),
    Config1.

await_reestablish_reports(Config) ->
    ResponderLeaves = proplists:get_value(responder_leaves, Config, true),
    #{initiator := IConnPid, responder := RConnPid}
        = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(RConnPid, [info, update]),
    ok = ?WS:register_test_for_channel_events(IConnPid, [info, update]),
    {ok, ChId, IMsg} = wait_for_channel_event(IConnPid, info, Config),
    {ok, ChId, RMsg} = wait_for_channel_event(RConnPid, info, Config),
    {ChId, true} = {ChId, ChId =/= null},
    #{<<"event">> := <<"channel_reestablished">>} = IMsg,
    #{<<"event">> := <<"channel_reestablished">>} = RMsg,
    {ok, _, #{<<"event">> := <<"open">>}} =
        wait_for_channel_event(IConnPid, info, Config),
    {ok, _, #{<<"event">> := <<"open">>}} =
        wait_for_channel_event(RConnPid, info, Config),
    {ok, _, #{<<"state">> := NewState}} = wait_for_channel_event(IConnPid, update, Config),
    if ResponderLeaves ->
            {ok, _, #{<<"state">> := NewState}} = wait_for_channel_event(RConnPid, update, Config);
       true ->
            %% Responder doesn't issue a new update report, since its state didn't change
            ok
    end,
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [info, update]),
    ok = ?WS:unregister_test_for_channel_events(IConnPid, [info, update]),
    ?CHECK_INFO(20),
    ok.

sc_ws_deposit_(Config, Origin, XOpts) when Origin =:= initiator
                                    orelse Origin =:= responder ->
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    {SStartB, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                              update, error]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                           update, error]),
    make_two_gen_messages_volleys(SenderConnPid, SenderPubkey, AckConnPid,
                                  AckPubkey, Config),
    ws_send_tagged(SenderConnPid, <<"channels.deposit">>, #{amount => <<"2">>}, Config),
    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    make_two_gen_messages_volleys(SenderConnPid, SenderPubkey, AckConnPid,
                                  AckPubkey, Config),
    ws_send_tagged(SenderConnPid, <<"channels.deposit">>, XOpts#{amount => 2}, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(SenderPubkey, SenderConnPid, SenderPrivkey, <<"channels.deposit_tx">>, Config),
    {ok, _, #{<<"event">> := <<"deposit_created">>}} = wait_for_channel_event(AckConnPid, info, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(AckPubkey, AckConnPid, AckPrivkey, <<"channels.deposit_ack">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, _, #{<<"tx">> := EncodedSignedDepositTx} = E1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    make_two_gen_messages_volleys(SenderConnPid, SenderPubkey, AckConnPid,
                                  AckPubkey, Config),
    ok = match_info(E1, #{<<"info">> => <<"deposit_signed">>}),
    {ok, _, #{<<"tx">> := EncodedSignedDepositTx} = E2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(E2, #{<<"info">> => <<"deposit_created">>}),

    {ok, SSignedDepositTx} = aeser_api_encoder:safe_decode(transaction,
                                                     EncodedSignedDepositTx),
    SignedDepositTx = aetx_sign:deserialize_from_binary(SSignedDepositTx),
    ok = wait_for_signed_transaction_in_block(SignedDepositTx),

    {ok, _, #{<<"tx">> := EncodedSignedWTx} = Cc1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    ok = match_info(Cc1, #{<<"info">> => <<"channel_changed">>}),
    {ok, _, #{<<"tx">> := EncodedSignedWTx} = Cc2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(Cc2, #{<<"info">> => <<"channel_changed">>}),

    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    % assert sender balance has changed
    Fee = aetx:fee(aetx_sign:tx(SignedDepositTx)),
    {SStartB1, _, _} = {SStartB - 2 - Fee, SStartB1, SStartB},

    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                   ?DEFAULT_MIN_DEPTH),
    {ok, _, #{<<"event">> := <<"own_deposit_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, _, #{<<"event">> := <<"own_deposit_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    {ok, _, #{<<"event">> := <<"deposit_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, _, #{<<"event">> := <<"deposit_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),
    {ok, _, #{<<"state">> := _NewState}} = wait_for_channel_event(SenderConnPid, update, Config),
    {ok, _, #{<<"state">> := _NewState}} = wait_for_channel_event(AckConnPid, update, Config),
    ok = ?WS:unregister_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                                update, error]),
    ok = ?WS:unregister_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                             update, error]),
    {ok, SignedDepositTx}.

sc_ws_withdraw_(Config, Origin, XOpts) when Origin =:= initiator
                                     orelse Origin =:= responder ->
    ct:log("withdraw test, Origin == ~p", [Origin]),
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    {SStartB, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                              update, error]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                           update, error]),
    make_two_gen_messages_volleys(SenderConnPid, SenderPubkey, AckConnPid,
                                  AckPubkey, Config),
    ws_send_tagged(SenderConnPid, <<"channels.withdraw">>, #{amount => <<"2">>}, Config),
    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    make_two_gen_messages_volleys(SenderConnPid, SenderPubkey, AckConnPid,
                                  AckPubkey, Config),
    ws_send_tagged(SenderConnPid, <<"channels.withdraw">>, XOpts#{amount => 2}, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(SenderPubkey, SenderConnPid, SenderPrivkey, <<"channels.withdraw_tx">>, Config),
    {ok, _, #{<<"event">> := <<"withdraw_created">>}} = wait_for_channel_event(AckConnPid, info, Config),
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(AckPubkey, AckConnPid, AckPrivkey, <<"channels.withdraw_ack">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, _, #{<<"tx">> := EncodedSignedWTx} = E1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    make_two_gen_messages_volleys(SenderConnPid, SenderPubkey, AckConnPid,
                                  AckPubkey, Config),
    ok = match_info(E1, #{<<"info">> => <<"withdraw_signed">>}),
    {ok, _, #{<<"tx">> := EncodedSignedWTx} = E2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(E2, #{<<"info">> => <<"withdraw_created">>}),

    ct:log("Got on_chain_tx from both"),

    {ok, SSignedWTx} = aeser_api_encoder:safe_decode(transaction, EncodedSignedWTx),
    SignedWTx = aetx_sign:deserialize_from_binary(SSignedWTx),
    ok = wait_for_signed_transaction_in_block(SignedWTx),

    ct:log("Found signed transaction in block"),

    {ok, _, #{<<"tx">> := EncodedSignedWTx} = Cc1} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    ok = match_info(Cc1, #{<<"info">> => <<"channel_changed">>}),
    {ok, _, #{<<"tx">> := EncodedSignedWTx} = Cc2} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),
    ok = match_info(Cc2, #{<<"info">> => <<"channel_changed">>}),

    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    % assert sender balance has changed
    Fee = aetx:fee(aetx_sign:tx(SignedWTx)),
    {SStartB1, _, _} = {SStartB + 2 - Fee, SStartB1, SStartB},

    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                   ?DEFAULT_MIN_DEPTH, #{strictly_follow_top => true}),
    {ok, _, #{<<"event">> := <<"own_withdraw_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, _, #{<<"event">> := <<"own_withdraw_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    ct:log("own_withdraw_locked from both"),

    {ok, _, #{<<"event">> := <<"withdraw_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, _, #{<<"event">> := <<"withdraw_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    ct:log("withdraw_locked from both"),
    {ok, _, #{<<"state">> := _NewState}} = wait_for_channel_event(SenderConnPid, update, Config),
    {ok, _, #{<<"state">> := _NewState}} = wait_for_channel_event(AckConnPid, update, Config),

    ok = ?WS:unregister_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx,
                                                                update, error]),
    ok = ?WS:unregister_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx,
                                                             update, error]),
    ct:log("sequence successful", []),
    {ok, SignedWTx}.

sc_ws_basic_contracts(Config) ->
    lists:foreach(
        fun({Owner, TestName}) ->
            sc_ws_contract_(Config, TestName, Owner)
        end,
        [{Owner, Test} || Owner <- [initiator, responder],
                          Test  <- [identity, counter, spend_test]]),
    ok.

sc_ws_oracle_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_oracle_contract_/9, Config, [])
        || Role <- [initiator, responder],
           ContractSource <- [offchain]], % Oracle contract requires an oracle to be created so we skip onchain check
    ok.

sc_ws_nameservice_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_nameservice_contract_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [offchain]],
    ok.

sc_ws_environment_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_environment_contract_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [offchain]],
    ok.

sc_ws_remote_call_contract(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_remote_call_contract_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [offchain]],
    ok.

sc_ws_remote_call_contract_refering_onchain_data(Config) ->
    [sc_ws_contract_generic_(Role, ContractSource, fun sc_ws_remote_call_contract_refering_onchain_data_/9, Config,
                            [])
        || Role <- [initiator, responder], ContractSource <- [offchain]],
    ok.

random_unused_name() ->
    random_unused_name(_Attempts = 10).

random_unused_name(Attempts) when Attempts < 1->
    {error, exhausted};
random_unused_name(Attempts) ->
    Size = 10,
    RandStr = base58:binary_to_base58(crypto:strong_rand_bytes(Size)),
    Name = aens_test_utils:fullname(list_to_binary(RandStr)),
    case get_names_entry_by_name_sut(Name) of
        {ok, 404, _Error} -> Name; % name not used yet
        _ -> random_unused_name(Attempts - 1)
    end.

sc_ws_contract_generic_(Origin, ContractSource, Fun, Config, Opts) ->
    %% get the infrastructure for users going
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    EventTags = [sign, info, get, error, update],
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    %% helper lambda for update
    GetVolley =
        fun(Actor) ->
            case Actor =:= Origin of
                true ->
                    {fun() -> update_volley_(SenderPubkey, SenderConnPid, SenderPrivkey,
                                             AckPubkey, AckConnPid, AckPrivkey, Config) end,
                     SenderConnPid, SenderPubkey};
                false ->
                    {fun() -> update_volley_(AckPubkey, AckConnPid, AckPrivkey,
                                             SenderPubkey, SenderConnPid, SenderPrivkey, Config) end,
                     AckConnPid, AckPubkey}
            end
        end,

    GetPubkeys =
        fun(Role) ->
            case Origin =:= Role of
                true  -> {SenderPubkey, AckPubkey};
                false -> {AckPubkey, SenderPubkey}
            end
        end,

    CreateContract =
        case ContractSource of
            offchain ->
                fun(Owner, EncodedCode, EncodedInitData, Deposit) ->
                    {CreateVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
                    NewContractOpts0 =
                        #{vm_version  => aect_test_utils:vm_version(),
                          abi_version => aect_test_utils:abi_version(),
                          deposit     => Deposit,
                          code        => EncodedCode,
                          call_data   => EncodedInitData},
                    NewContractOpts = maybe_include_meta(NewContractOpts0, Config),
                    % incorrect calls
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts#{deposit => <<"1">>}, Config),
                    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),

                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts#{vm_version => <<"1">>}, Config),
                    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),

                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts#{abi_version => <<"1">>}, Config),
                    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
                        wait_for_channel_event(OwnerConnPid, error, Config),
                    % correct call
                    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                        NewContractOpts, Config),

                    #{tx := UnsignedStateTx, updates := _Updates} = CreateVolley(),
                    contract_id_from_create_update(OwnerPubKey, UnsignedStateTx)
                end
        end,

    Actors = [{R, GetPubkeys(R)} || R <- [initiator, responder]],
    WrappedFun =
        fun(Owner, OwnerPubkey, OtherPubkey) ->
            with_registered_events(EventTags, [SenderConnPid, AckConnPid],
                fun() ->            
                    Fun(Owner, GetVolley, CreateContract, SenderConnPid,
                        AckConnPid, OwnerPubkey, OtherPubkey, Opts, Config)
                end)
        end,

    [WrappedFun(Owner, OwnerPubkey, OtherPubkey)
        || {Owner, {OwnerPubkey, OtherPubkey}} <- Actors],
    ok.

sc_ws_oracle_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                       OwnerPubkey, OtherPubkey, _Opts, Config) ->
    %% Register an oracle. It will be used in an off-chain contract
    %% Oracle ask itself a question and answers it
    {OraclePubkey, OraclePrivkey} = ?CAROL,
    ok = initialize_account(2000000 * aec_test_utils:min_gas_price(), ?CAROL),

    SophiaStringType =
        case aect_test_utils:backend() of
            aevm -> aeb_heap:to_binary(string, 0);
            fate -> iolist_to_binary(aeb_fate_encoding:serialize_type(string))
        end,
    SophiaString =
        fun(S0) ->
            S = list_to_binary(tl(lists:droplast(S0))),
            case aect_test_utils:backend() of
                aevm -> aeb_heap:to_binary(S, 0);
                fate -> aeb_fate_encoding:serialize(S)
            end
        end,
    QueryFee = 3,
    OracleTTL = 20,
    QueryTTL = 5,
    ResponseTTL = 5,
    Question = "\"Fill me in with something reasonable\"",
    register_oracle(OraclePubkey, OraclePrivkey,
                    #{query_format    => SophiaStringType,
                      response_format => SophiaStringType,
                      query_fee       => QueryFee,
                      query_ttl       => QueryTTL,
                      abi_version     => aect_test_utils:abi_version(),
                      oracle_ttl      => {delta, OracleTTL}
                     }),
    OracleQuerySequence =
        fun(Q0, R0) ->
            Q = SophiaString(Q0),
            R = SophiaString(R0),
            QueryId = query_oracle(OraclePubkey, OraclePrivkey, %oracle asks oracle
                                   OraclePubkey,
                                   #{query        => Q,
                                     query_ttl    => {delta, QueryTTL - 1},
                                     response_ttl => {delta, ResponseTTL}}),
            respond_oracle(OraclePubkey, OraclePrivkey, QueryId,
                           R, #{response_ttl => {delta, ResponseTTL}}),
            QueryId
        end,

    EncodedCode = contract_byte_code("channel_on_chain_contract_oracle"),
    {ok, EncodedInitData} = encode_call_data(channel_on_chain_contract_oracle,
                              "init", [aeser_api_encoder:encode(oracle_pubkey, OraclePubkey), Question]),

    ContractPubKey = CreateContract(Owner, EncodedCode, EncodedInitData, 10),

    ContractId = aeser_id:create(contract, ContractPubKey),
    OwnerId = aeser_id:create(account, OwnerPubkey),
    ExpectedABIVersion = aect_test_utils:abi_version(),
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),

    CallContract =
        fun(Who, Fun, Args, _ReturnType, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            TopHash = aecore_suite_utils:get_key_hash_by_delta(dev1, 0),
            Hash = aeser_api_encoder:encode(key_block_hash, TopHash),
            R0 = dry_call_a_contract(Fun, Args, ContractPubKey,
                                     channel_on_chain_contract_oracle, UpdaterConnPid,
                                     0, Config, #{block_hash => Hash}),
            #{tx := Tx, updates := Updates} =
                call_a_contract(Fun, Args, ContractPubKey, channel_on_chain_contract_oracle,
                                UpdaterConnPid, UpdateVolley,
                                0, Config, #{block_hash => Hash}),
            R =
                ws_get_decoded_result(ConnPid1, ConnPid2, channel_on_chain_contract_oracle,
                                      Fun, Updates, Tx, Config),
            {R, R} = {R0, R},
            {R, R} = {Result, R}

        end,
    [CallContract(Who, "place_bet", [Bet],
                  <<"string">>, <<"ok">>)
        || {Who, Bet} <- [{initiator, "\"I win\""},
                          {responder, "\"no, I win\""}]],
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     , state := _State2
     } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),
    %% initiator places a bet and then nobody can overwrite it
    ParkedAnswer = "\"I claim this\"",
    CallContract(initiator, "place_bet", [ParkedAnswer],
                  <<"string">>, <<"ok">>),
    [CallContract(Who, "place_bet", [ParkedAnswer],
                  <<"string">>, <<"bet_already_taken">>)
        || Who <- [initiator, responder]],

    %% place some oracle query id with a different question
    ErrQueryId = OracleQuerySequence("\"other question\"", "\"some answer\""),
    [CallContract(Who, "resolve", [aeser_api_encoder:encode(oracle_query_id, ErrQueryId)],
                  <<"string">>, <<"different question">>)
        || Who <- [initiator, responder]],
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     , state := _State3
     } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),
    Answer = "\"other reasonable thingy\"",
    CorrectQueryId = OracleQuerySequence(Question, Answer),
    EncodedQueryId = aeser_api_encoder:encode(oracle_query_id, CorrectQueryId),

    {ok, {OwnerBal0, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    CallContract(Owner, "resolve", [EncodedQueryId],
                 <<"string">>, <<"no winning bet">>),
    sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),
    % no changes in balances
    {ok, {OwnerBal0, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),

    % owner posts the correct
    CallContract(Owner, "place_bet", [Answer], <<"string">>, <<"ok">>),
    #{ active := true
     , abi_version := ExpectedABIVersion
     , deposit := 10
     , id := ContractId
     , owner_id := OwnerId
     , referrer_ids := []
     , state := _State4
    } = sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),

    {ok, {OwnerBal, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                          OwnerPubkey,
                                                          OtherPubkey,
                                                          Config),
    {ok, ContractBalance} = sc_ws_get_balance(ConnPid1, ContractPubKey, Config),
    CallContract(Owner, "resolve", [EncodedQueryId], <<"string">>, <<"ok">>),
    sc_ws_get_contract_assert_equal(ConnPid1, ConnPid2, ContractPubKey, Config),

    % contract balance is 0 now
    {ok, 0} = sc_ws_get_balance(ConnPid1, ContractPubKey, Config),
    {ok, 0} = sc_ws_get_balance(ConnPid2, ContractPubKey, Config),

    % contract owner balance is updated
    {ok, {OwnerBal1, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    {ok, {OwnerBal1, OtherBal0}} = sc_ws_get_both_balances(ConnPid2,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    {OwnerBal1, _} = {OwnerBal + ContractBalance, OwnerBal1},
    ok.

sc_ws_nameservice_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                            _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    Name = random_unused_name(),
    %% Register an oracle. It will be used in an off-chain contract
    %% Oracle ask itself a question and answers it
    {NamePubkey, NamePrivkey} = ?CAROL,
    ok = initialize_account(4000000000000 * aec_test_utils:min_gas_price(), ?CAROL),

    EncodedCode = contract_byte_code("channel_on_chain_contract_name_resolution"),
    {ok, EncodedInitData} = encode_call_data(channel_on_chain_contract_name_resolution,
                              "init", []),

    ContractPubKey = CreateContract(Owner, EncodedCode, EncodedInitData, 10),

    ContractCanNameResolve =
        fun(Who, Name0, Key0, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name0),
            QKey = AddQuotes(Key0),
            Args = [QName, QKey],
            FunctionName = "can_resolve",
            TopHash = aecore_suite_utils:get_key_hash_by_delta(dev1, 0),
            Hash = aeser_api_encoder:encode(key_block_hash, TopHash),
            R0 = dry_call_a_contract(FunctionName, Args, ContractPubKey,
                                     channel_on_chain_contract_name_resolution,
                                     UpdaterConnPid,
                                     0, Config, #{block_hash => Hash}),
            #{tx := Tx, updates := Updates} =
                call_a_contract(FunctionName, Args, ContractPubKey,
                                channel_on_chain_contract_name_resolution,
                                UpdaterConnPid, UpdateVolley,
                                0, Config, #{block_hash => Hash}),
            R = ws_get_decoded_result(ConnPid1, ConnPid2,
                                      channel_on_chain_contract_name_resolution,
                                      FunctionName, Updates, Tx, Config),
            {R, R} = {R0, R},
            {R, R} = {Result, R}

        end,
    Test =
        fun(Name1, Key, Result) ->
            [ContractCanNameResolve(Who, Name1, Key, Result)
                || Who <- [initiator, responder]]
        end,

    Test(Name, <<"oracle">>, false),
    Protocol = aect_test_utils:latest_protocol_version(),
    NameFee =
        case Protocol >= ?LIMA_PROTOCOL_VSN of
            true -> aec_governance:name_claim_fee(Name, Protocol);
            false -> prelima
        end,

    register_name(NamePubkey, NamePrivkey, Name, NameFee,
                  [{<<"account_pubkey">>, aeser_id:create(account, <<1:256>>)},
                   {<<"oracle">>, aeser_id:create(oracle, <<2:256>>)},
                   {<<"unexpected_key">>, aeser_id:create(account, <<3:256>>)}]),
    Test(Name, <<"account_pubkey">>, true),
    Test(Name, <<"oracle">>, true),
    Test(Name, <<"unexpected_key">>, true),
    Test(Name, <<"missing_key">>, false),
    ok.

sc_ws_environment_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                            _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    EncodedCode = contract_byte_code("channel_env"),
    {ok, EncodedInitData} = encode_call_data(channel_env, "init", []),

    ContractPubKey = CreateContract(Owner, EncodedCode, EncodedInitData, 10),

    ContractCall =
        fun(Who, Fun, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            Args = [],
            TopHash = aecore_suite_utils:get_key_hash_by_delta(dev1, 0),
            Hash = aeser_api_encoder:encode(key_block_hash, TopHash),
            R0 = dry_call_a_contract(Fun, Args, ContractPubKey,
                                     channel_env, UpdaterConnPid,
                                     0, Config, #{block_hash => Hash}),
            #{tx := Tx, updates := Updates} =
                call_a_contract(Fun, Args, ContractPubKey, channel_env,
                                UpdaterConnPid, UpdateVolley,
                                0, Config, #{block_hash => Hash}),
            R = ws_get_decoded_result(ConnPid1, ConnPid2, channel_env,
                                      Fun, Updates, Tx, Config),
            {R, R} = {R0, R},
            case is_function(Result) of
                true -> true = Result(Who, R);
                false ->
                    {R, R} = {Result, R}
            end
        end,
    Test =
        fun(Fun, Result) ->
            [ContractCall(Who, Fun, Result)
                || Who <- [initiator, responder]]
        end,
    {ok, 200, Block} = get_key_blocks_current_sut(),
    #{<<"height">> := BlockHeight,
      <<"beneficiary">> := EncBeneficiary,
      <<"time">> := Time
     } = Block,

    Test(<<"block_height">>, BlockHeight),
    Test(<<"coinbase">>, EncBeneficiary),
    Test(<<"timestamp">>, fun(_, T) -> T > Time end),
    CheckCaller =
        fun(Who, Result) ->
            Participants = proplists:get_value(participants, Config),
            #{pub_key := Pubkey} = maps:get(Who, Participants),
            EncPubkey = aeser_api_encoder:encode(account_pubkey, Pubkey),
            {Result, Result, Who} = {Result, EncPubkey, Who},
            true
        end,
    Test(<<"caller">>, CheckCaller),
    Test(<<"origin">>, CheckCaller),
    case aect_test_utils:latest_protocol_version() of
        PostFortuna when PostFortuna >= ?FORTUNA_PROTOCOL_VSN ->
            Test(<<"creator">>, fun(_, Res) -> CheckCaller(Owner, Res) end);
        _PreFortuna -> pass
    end,
    ok.

sc_ws_remote_call_contract_(Owner, GetVolley, CreateContract, ConnPid1, ConnPid2,
                            _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% create identity contract off-chain
    CreateIdentityContract =
        fun(Name) ->
            EncodedCode = contract_byte_code(Name),
            {ok, EncodedInitData} = encode_call_data(Name, "init", []),
            CreateContract(Owner, EncodedCode, EncodedInitData, 10)
        end,
    IdentityCPubKey   = CreateIdentityContract(identity),
    RemoteCallCPubKey = CreateIdentityContract(remote_call),

    ContractCall =
        fun(Who, ContractPubKey, Contract, Fun, Args, Result, Amount) ->
                {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
                R0 = dry_call_a_contract(Fun, Args, ContractPubKey, Contract,
                                         UpdaterConnPid, Amount, Config),
                #{tx := Tx, updates := Updates} =
                    call_a_contract(Fun, Args, ContractPubKey, Contract,
                                    UpdaterConnPid, UpdateVolley, Amount, Config),
                R = ws_get_decoded_result(ConnPid1, ConnPid2, Contract,
                                          Fun, Updates, Tx, Config),
                {R, R} = {R0, R},
                {R, R} = {Result, R}
        end,
    CallIdentity =
        fun(Who, Val) ->
            ValB = integer_to_list(Val),
            ContractCall(Who, IdentityCPubKey, identity, <<"main_">>,
                         [ValB], Val, _Amount = 0)
        end,
    EncIdPubkey = aeser_api_encoder:encode(contract_pubkey, IdentityCPubKey),
    CallRemoteContract =
        fun(Who, Val) ->
            ValB = integer_to_list(Val),
            ContractCall(Who, RemoteCallCPubKey, remote_call, <<"call">>,
                         [EncIdPubkey, ValB], Val,
                         % beacuse of hardcoded value=10 in the
                         % remote_call.aes -> amount in the call must be > 10
                         _Amount = 20)
        end,
    Test =
        fun(Fun, Val) ->
            [Fun(Who, Val)
                || Who <- [initiator, responder]]
        end,

    % actual tests
    Test(CallIdentity, 10),
    Test(CallIdentity, 11),
    Test(CallRemoteContract, 42),
    Test(CallRemoteContract, 43),
    Test(CallIdentity, 12),
    ok.

sc_ws_remote_call_contract_refering_onchain_data_(Owner, GetVolley, CreateContract,
                                                  ConnPid1, ConnPid2, _OwnerPubkey,
                                                  _OtherPubkey, _Opts, Config) ->
    %% create identity contract off-chain
    CreateIdentityContract =
        fun(Name) ->
            EncodedCode = contract_byte_code(Name),
            {ok, EncodedInitData} = encode_call_data(Name, "init", []),
            CreateContract(Owner, EncodedCode, EncodedInitData, 10)
          end,
    ResolverCPubKey   = CreateIdentityContract(channel_on_chain_contract_name_resolution),
    RemoteCallCPubKey = CreateIdentityContract(channel_remote_on_chain_contract_name_resolution),

    ContractCall =
        fun(Who, ContractPubKey, Contract, Fun, Args, Result, Amount) ->
                {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
                TopHash = aecore_suite_utils:get_key_hash_by_delta(dev1, 0),
                Hash = aeser_api_encoder:encode(key_block_hash, TopHash),
                R0 = dry_call_a_contract(Fun, Args, ContractPubKey, Contract,
                                         UpdaterConnPid, Amount, Config,
                                         #{block_hash => Hash}),
                #{tx := Tx, updates := Updates} =
                    call_a_contract(Fun, Args, ContractPubKey, Contract,
                                    UpdaterConnPid, UpdateVolley, Amount,
                                    Config, #{block_hash => Hash}),
                R = ws_get_decoded_result(ConnPid1, ConnPid2, Contract, Fun,
                                          Updates, Tx, Config),
                {R, R} = {R0, R},
                {R, R} = {Result, R}
        end,
    CallResolve =
        fun(Who, Name, Key, IsResolvable) ->
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name),
            QKey = AddQuotes(Key),
            Args = [QName, QKey],
            ContractCall(Who, ResolverCPubKey, channel_on_chain_contract_name_resolution,
                         <<"can_resolve">>, Args, IsResolvable, _Amount = 0)
        end,
    EncResPubkey = aeser_api_encoder:encode(contract_pubkey, ResolverCPubKey),
    CallRemoteContract =
        fun(Who, Name, Key, IsResolvable) ->
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name),
            QKey = AddQuotes(Key),
            Args = [EncResPubkey, QName, QKey],
            ContractCall(Who, RemoteCallCPubKey,
                         channel_remote_on_chain_contract_name_resolution,
                         <<"remote_resolve">>, Args, IsResolvable,
                         % beacuse of hardcoded value=10 in the
                         % remote_call.aes -> amount in the call must be > 10
                         _Amount = 20)
        end,
    Test =
        fun(Fun, N, K, Res) ->
            [Fun(Who, N, K, Res)
                || Who <- [initiator, responder]]
        end,

    % actual tests
    % we have two contracts: c
    % * channel_on_chain_contract_name_resolution.aes that has
    %     `can_resolve(Name, Key)` function. It resolves on-chain names
    % * channel_remote_on_chain_contract_name_res.aes that has
    %     `remote_resolve(Contract, Name, Key)` function that makes a remote
    %     call to the first contract and uses it to resolve the name on-chain
    % both functions shall return the same result
    Name = random_unused_name(),

    % name is not present on-chain, both contracts shall return false:
    Test(CallResolve, Name, <<"account_pubkey">>, false),
    Test(CallRemoteContract, Name, <<"account_pubkey">>, false),

    % registering the name on-chain
    {NamePubkey, NamePrivkey} = ?CAROL,
    ok = initialize_account(4000000000000 * aec_test_utils:min_gas_price(), ?CAROL),
    Protocol = aect_test_utils:latest_protocol_version(),
    NameFee =
        case  Protocol >= ?LIMA_PROTOCOL_VSN of
            true -> aec_governance:name_claim_fee(Name, Protocol);
            false -> prelima
        end,
    register_name(NamePubkey, NamePrivkey, Name, NameFee,
                  [{<<"account_pubkey">>, aeser_id:create(account, <<1:256>>)}]),

    % now the name is on-chain, both must return true:
    Test(CallResolve, Name, <<"account_pubkey">>, true),
    case aect_test_utils:backend() of
        aevm ->
            Test(CallRemoteContract, Name, <<"account_pubkey">>, false); % BUG
        fate ->
            Test(CallRemoteContract, Name, <<"account_pubkey">>, true)
    end,
    ok.

register_oracle(OraclePubkey, OraclePrivkey, Opts) ->
    case rpc(aec_chain, get_oracle, [OraclePubkey]) of
        {error, not_found} -> pass;
        {ok, Oracle} ->
            Height = current_height(),
            TTL = aeo_oracles:ttl(Oracle), % absolute TTL
            ExpBlocksCnt = TTL - Height,
            ct:log("Already an oracle, mining ~p blocks so it expires",
                   [ExpBlocksCnt]),
            Node = aecore_suite_utils:node_name(?NODE),
            aecore_suite_utils:mine_key_blocks(Node, ExpBlocksCnt + 1)
    end,
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:register_tx(OraclePubkey, Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, OraclePrivkey),
    OracleId = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {ok, 200, _Resp} = get_oracles_by_pubkey_sut(OracleId),
    ok.

query_oracle(FromPubkey, FromPrivkey, OraclePubkey, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [FromPubkey]),
    Tx = aeo_test_utils:query_tx(FromPubkey, aeser_id:create(oracle, OraclePubkey),
                                 Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, FromPrivkey),
    {aeo_query_tx, QueryTx} = aetx:specialize_callback(Tx),
    aeo_query_tx:query_id(QueryTx).

respond_oracle(OraclePubkey, OraclePrivkey, QueryId, Response, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:response_tx(OraclePubkey, QueryId,
                                    Response, Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, OraclePrivkey),
    ok.

sign_post_mine(Tx, Privkey) ->
    SignedTx = aec_test_utils:sign_tx(Tx, Privkey),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedSignedTx =
        aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
    ok = post_tx(TxHash, EncodedSerializedSignedTx),
    ok = wait_for_tx_hash_on_chain(TxHash).

register_name(Owner, OwnerPrivKey, Name, NameFee, Pointers) ->
    Salt = rand:uniform(10000),
    preclaim_name(Owner, OwnerPrivKey, Name, Salt),
    claim_name(Owner, OwnerPrivKey, Name, Salt, NameFee),
    update_pointers(Owner, OwnerPrivKey, Name, Pointers),
    ok.

preclaim_name(Owner, OwnerPrivKey, Name, Salt) ->
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    CHash = aens_hash:commitment_hash(NameAscii, Salt),
    TxSpec = aens_test_utils:preclaim_tx_spec(Owner, CHash, #{nonce => Nonce}, #{}),
    {ok, Tx} = aens_preclaim_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

claim_name(Owner, OwnerPrivKey, Name, Salt, NameFee) ->
    Delta = aec_governance:name_claim_preclaim_delta(),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:mine_key_blocks(Node, Delta),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    TxSpec = aens_test_utils:claim_tx_spec(Owner, Name, Salt, NameFee, #{nonce => Nonce}, #{}),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

update_pointers(Owner, OwnerPrivKey, Name, Pointers0) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
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
                Owner, NHash, #{pointers => Pointers,
                                name_ttl => NameTTL,
                                nonce => Nonce}, #{}),
    {ok, Tx} = aens_update_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

initialize_account(Amount, KeyPair) ->
    initialize_account(Amount, KeyPair, true).

initialize_account(Amount, {Pubkey, _Privkey}, Check) ->
    Fee = ?SPEND_FEE,
    Node = aecore_suite_utils:node_name(?NODE),
    MaxMined = ?MAX_MINED_BLOCKS + (Amount div aec_governance:block_mine_reward(1)),
    ct:pal("Mining ~p blocks at most for ~p tokens", [MaxMined, Amount]),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Amount, Fee),
    TxHash = sign_and_post_tx(SpendTx),
    if Check ->
        aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [TxHash], MaxMined),
        assert_balance_at_least(Pubkey, Amount),
        ok;
       true ->
        TxHash
    end.

update_volley_(FirstPubkey, FirstConnPid, FirstPrivkey, SecondPubkey, SecondConnPid, SecondPrivkey, Config) ->
    #{tx := UnsignedStateTx,
      updates := Updates} = channel_sign_tx(FirstPubkey, FirstConnPid, FirstPrivkey, <<"channels.update">>, Config),

    % acknowledger signs update_ack
    {ok, _, #{<<"event">> := <<"update">>}} = wait_for_channel_event(SecondConnPid, info, Config),
    #{ tx := UnsignedStateTx
     , updates := Updates} = Res = channel_sign_tx(SecondPubkey,
                                                   SecondConnPid,
                                                   SecondPrivkey,
                                                   <<"channels.update_ack">>,
                                                   Config),
    {ok, _, #{<<"state">> := _State}} = wait_for_channel_event(FirstConnPid, update, Config),
    {ok, _,  #{<<"state">> := _State}} = wait_for_channel_event(SecondConnPid, update, Config),
    Res.

produce_update_volley_funs(Sender, Config) ->
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Sender of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),

    %% helper lambda for update
    UpdateVolley =
        fun() ->
            update_volley_(SenderPubkey, SenderConnPid, SenderPrivkey,
                           AckPubkey, AckConnPid, AckPrivkey, Config)
        end,
    UpdateVolleyReverse =
        fun() ->
            update_volley_(AckPubkey, AckConnPid, AckPrivkey,
                           SenderPubkey, SenderConnPid, SenderPrivkey, Config)
        end,
    {UpdateVolley, UpdateVolleyReverse}.

sc_ws_contract_(Config, TestName, Owner) ->
    Participants = proplists:get_value(participants, Config),
    Clients = proplists:get_value(channel_clients, Config),
    {SenderRole, AckRole} =
        case Owner of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),

    %% helper lambda for update
    {UpdateVolley, UpdateVolleyReverse} = produce_update_volley_funs(Owner,
                                                                     Config),
    with_registered_events(
      [sign, info, get, error, update], [SenderConnPid, AckConnPid],
      fun() ->
          % trigger new contract
          {UnsignedStateTx, _Updates, _Code} = create_contract_(TestName, SenderConnPid, UpdateVolley, Config),

          ContractPubKey = contract_id_from_create_update(SenderPubkey,
                                                          UnsignedStateTx),

          %% helper lambdas for pruning and call not found
          PruneCalls =
              fun(ConnPid) ->
                  ok = ?WS:register_test_for_channel_events(ConnPid, [calls_pruned]),
                  ws_send_tagged(ConnPid, <<"channels.clean_contract_calls">>, #{}, Config),
                  {ok, _, _} = wait_for_channel_event(ConnPid, calls_pruned, Config),
                  ok = ?WS:unregister_test_for_channel_events(ConnPid, [calls_pruned])
              end,
          CallMissingCall =
              fun(Tx, [U], ConnPid) ->
                  ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                                ws_get_call_params(U, Tx), Config),
                  {ok, _, #{<<"reason">> := <<"call_not_found">>}} = wait_for_channel_event(ConnPid, error, Config),
                  ok
              end,

          % trigger call contract
          % owner can call a contract
          {Fun, SomeUnsignedStateTx, Updates} =
              contract_calls_(TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                              AckConnPid, SenderPubkey, AckPubkey, Config),
          _ = ws_get_decoded_result(SenderConnPid, AckConnPid, TestName, Fun,
                                    Updates, SomeUnsignedStateTx, Config),
          ok = PruneCalls(SenderConnPid),
          ok = CallMissingCall(SomeUnsignedStateTx, Updates, SenderConnPid),
          % state is still usable

          % acknowledger can call a contract
          contract_calls_(TestName, ContractPubKey, AckConnPid, UpdateVolleyReverse,
                          SenderConnPid, AckPubkey, SenderPubkey, Config),

          GetPoI =
              fun(ConnPid) ->
                      Scope = #{contracts   => [aeser_api_encoder:encode(contract_pubkey, ContractPubKey)],
                                accounts    => [aeser_api_encoder:encode(account_pubkey, SenderPubkey),
                                                aeser_api_encoder:encode(account_pubkey, AckPubkey)]
                              },
                      {ok, #{poi := P}} = sc_ws_get_poi_(ConnPid, Scope, Config),
                      P
              end,

          GetMissingPoI =
              fun(ConnPid, Accs, Cts) ->
                  ws_send_tagged(ConnPid, <<"channels.get.poi">>,
                                  #{contracts   => [aeser_api_encoder:encode(contract_pubkey, C) || C <- Cts],
                                    accounts    => [aeser_api_encoder:encode(account_pubkey, Acc) || Acc <- Accs]
                                  }, Config),

                          {ok, _, #{<<"reason">> := R}} = wait_for_channel_event(ConnPid, error, Config),
                          R
                      end,

          EncodedPoI = GetPoI(SenderConnPid),
          EncodedPoI = GetPoI(AckConnPid),

          NegativePoiTests =
              fun(ConnPid) ->
                  <<"broken_encoding: accounts">> = GetMissingPoI(ConnPid, [<<123456789>>], []),
                  <<"broken_encoding: contracts">> = GetMissingPoI(ConnPid, [], [<<123456789>>]),
                  <<"broken_encoding: accounts, contracts">> = GetMissingPoI(ConnPid, [<<123456789>>], [<<123456789>>]),
                  AccountByteSize = aeser_api_encoder:byte_size_for_type(account_pubkey),
                  FakeAccountId = <<42:AccountByteSize/unit:8>>,
                  <<"not_found">> = GetMissingPoI(ConnPid, [FakeAccountId], []),
                  ContractByteSize = aeser_api_encoder:byte_size_for_type(contract_pubkey),
                  FakeContractId = <<42:ContractByteSize/unit:8>>,
                  <<"not_found">> = GetMissingPoI(ConnPid, [], [FakeContractId])
              end,

          NegativePoiTests(SenderConnPid),
          NegativePoiTests(AckConnPid),

          {ok, PoIBin} = aeser_api_encoder:safe_decode(poi, EncodedPoI),
          PoI = aec_trees:deserialize_poi(PoIBin),
          {ok, _SenderAcc} = aec_trees:lookup_poi(accounts, SenderPubkey, PoI),
          {ok, _AckAcc} = aec_trees:lookup_poi(accounts, AckPubkey, PoI),
          {ok, _ContractAcc} = aec_trees:lookup_poi(accounts, ContractPubKey, PoI),
          {ok, _ContractObj} = aec_trees:lookup_poi(contracts, ContractPubKey, PoI)
      end),
    ok.

sc_ws_get_poi_(ConnPid, Scope, Config) ->
    with_registered_events(
      [get], [ConnPid],
      fun() ->
              ws_send_tagged(ConnPid, <<"channels.get.poi">>, Scope, Config),
              {ok, <<"poi">>, #{ channel_id := ChId
                               , data := #{<<"poi">> := Poi}}} =
                  wait_for_channel_event_full(ConnPid, get, Config),
              {ok, #{ channel_id => ChId
                    , poi => Poi}}
      end).

contract_id_from_create_update(Owner, OffchainTx) ->
    {CB, Tx} = aetx:specialize_callback(OffchainTx),
    Round = CB:round(Tx),
    aect_contracts:compute_contract_pubkey(Owner, Round).


create_contract_(TestName, SenderConnPid, UpdateVolley, Config) ->
    InitArgument = contract_create_init_arg(TestName),
    create_contract_(TestName, InitArgument, SenderConnPid, UpdateVolley, Config).

create_contract_(TestName, InitArgument, SenderConnPid, UpdateVolley, Config) ->
    create_contract_(TestName, InitArgument, SenderConnPid, UpdateVolley, Config,
                10).

create_contract_(TestName, InitArgument, SenderConnPid, UpdateVolley, Config,
                Deposit) ->
    EncodedCode = contract_byte_code(TestName),
    {ok, EncodedInitData} = encode_call_data(TestName, "init", InitArgument),

    ws_send_tagged(SenderConnPid, <<"channels.update.new_contract">>,
                   #{vm_version  => aect_test_utils:vm_version(),
                     abi_version => aect_test_utils:abi_version(),
                     deposit     => Deposit,
                     code        => EncodedCode,
                     call_data   => EncodedInitData}, Config),
    #{tx := UnsignedStateTx, updates := Updates} = UpdateVolley(),
    {UnsignedStateTx, Updates, contract_code(TestName)}.

contract_calls_(identity = TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                AckConnPid, _ , _, Config) ->
    FunctionName = "main_",
    Args = ["42"],
    ExpectedResult = 42,
    #{tx := UnsignedStateTx, updates := Updates} =
        call_a_contract(FunctionName, Args, ContractPubKey, TestName, SenderConnPid,
                    UpdateVolley, Config),
    ExpectedResult = dry_call_a_contract(FunctionName, Args, ContractPubKey,
                                         TestName, SenderConnPid, Config),
    DecodedCallResult = ws_get_decoded_result(SenderConnPid, AckConnPid,
                                              TestName, FunctionName,
                                              Updates, UnsignedStateTx, Config),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    {FunctionName, UnsignedStateTx, Updates};
contract_calls_(counter = TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                AckConnPid, _ , _, Config) ->
    #{tx := UnsignedStateTx0, updates := Updates0} =
        call_a_contract("get", [], ContractPubKey, TestName, SenderConnPid,
                        UpdateVolley, Config),
    GetDecodedResult =
        fun(Tx, U) ->
            ws_get_decoded_result(SenderConnPid, AckConnPid, TestName,
                                  "get", U, Tx, Config)
        end,

    ExpectedInitResult = dry_call_a_contract("get", [], ContractPubKey,
                                             TestName, SenderConnPid, Config),
    InitResult = GetDecodedResult(UnsignedStateTx0, Updates0),
    {InitResult, InitResult} = {ExpectedInitResult, InitResult},
    call_a_contract("tick", [], ContractPubKey, TestName,
                    SenderConnPid, UpdateVolley, Config),

    #{tx := UnsignedStateTx1, updates := Updates1} =
        call_a_contract("get", [], ContractPubKey, TestName,
                        SenderConnPid, UpdateVolley, Config),

    #{tx := UnsignedStateTx2, updates := Updates2} =
        call_a_contract("get", [], ContractPubKey, TestName,
                        SenderConnPid, UpdateVolley, Config),

    ExpectedResult = InitResult + 1,
    DecodedCallResult = GetDecodedResult(UnsignedStateTx1, Updates1),
    DecodedCallResult = GetDecodedResult(UnsignedStateTx2, Updates2),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    {"get", UnsignedStateTx0, Updates0};
contract_calls_(spend_test = TestName, ContractPubKey, SenderConnPid, UpdateVolley,
                AckConnPid, SenderPubkey, AckPubkey, Config) ->
    GetBalance =
        fun(Args) ->
            FunName =
                case Args of
                    [] -> <<"get_balance">>;
                    _ -> <<"get_balance_of">>
                end,
                #{tx := UsStateTx, updates := Updates} =
                call_a_contract(FunName, Args, ContractPubKey, TestName,
                                SenderConnPid, UpdateVolley, Config),
            ws_get_decoded_result(SenderConnPid, AckConnPid, TestName, FunName,
                                  Updates, UsStateTx, Config)
        end,
    ContractBalance0 = GetBalance([]),

    SenderB0 = GetBalance(format_args(SenderPubkey)),
    AckB0 = GetBalance(format_args(AckPubkey)),

    SpendFun =
        fun(To, Amt) ->
            SpendArgs = format_args([To, Amt]),
            #{tx := _SpendStateTx, updates := _Updates2} =
                call_a_contract("spend", SpendArgs, ContractPubKey, TestName,
                                SenderConnPid, UpdateVolley, Config)
        end,

    SpendAmt = 3,
    SpendFun(SenderPubkey, SpendAmt),
    ContractBalance = GetBalance([]),
    {ContractBalance, _} = {ContractBalance0 - SpendAmt, ContractBalance0},
    SenderB = GetBalance(format_args(SenderPubkey)),
    AckB0 = GetBalance(format_args(AckPubkey)),
    SenderB = SenderB0 + SpendAmt,

    SpendAmt2 = 2,
    #{tx := UnsignedStateTx, updates := Updates} = SpendFun(AckPubkey, SpendAmt2),
    ContractBalance1 = GetBalance([]),
    {ContractBalance1, _} = {ContractBalance - SpendAmt2, ContractBalance1},
    SenderB = GetBalance(format_args(SenderPubkey)),
    AckB = GetBalance(format_args(AckPubkey)),
    AckB = AckB0 + SpendAmt2,
    {"spend", UnsignedStateTx, Updates}.

call_a_contract(Function, Argument, ContractPubKey, Contract,
                SenderConnPid, UpdateVolley, Config) ->
    call_a_contract(Function, Argument, ContractPubKey, Contract,
                    SenderConnPid, UpdateVolley, 0, Config).

call_a_contract(Function, Argument, ContractPubKey, Contract, SenderConnPid,
                UpdateVolley, Amount, Config) ->
    call_a_contract(Function, Argument, ContractPubKey, Contract, SenderConnPid,
                UpdateVolley, Amount, Config, #{}).

call_a_contract(Function, Argument, ContractPubKey, Contract, SenderConnPid,
                UpdateVolley, Amount, Config, XOpts) ->
    {ok, EncodedMainData} = encode_call_data(Contract, Function, Argument),
    CallOpts0 =
        XOpts#{ contract_id => aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
              , abi_version => aect_test_utils:abi_version()
              , amount      => Amount
              , call_data   => EncodedMainData },

    CallOpts = maybe_include_meta(CallOpts0, Config),
    % invalid call
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
                   CallOpts#{amount => <<"1">>}, Config),
    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
                   CallOpts#{abi_version => <<"1">>}, Config),
    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
        CallOpts#{call_data => <<"ABCDEFG">>}, Config),
    {ok, _, #{<<"reason">> := <<"broken_encoding: bytearray">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
        CallOpts#{contract_id => <<"ABCDEFG">>}, Config),
    {ok, _, #{<<"reason">> := <<"broken_encoding: contracts">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    % correct call
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
                   CallOpts, Config),
    #{tx := _UnsignedStateTx, updates := _Updates} = UpdateVolley().

call_a_contract_(Function, Argument, ContractPubKey, Contract, SenderConnPid,
                UpdateVolley, Amount, Config, XOpts) ->
    {ok, EncodedMainData} = encode_call_data(Contract, Function, Argument),
    CallOpts0 =
        XOpts#{ contract_id => aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
              , abi_version => aect_test_utils:abi_version()
              , amount      => Amount
              , call_data   => EncodedMainData },
    CallOpts = maybe_include_meta(CallOpts0, Config),
    ws_send_tagged(SenderConnPid, <<"channels.update.call_contract">>,
                   CallOpts, Config),
    #{tx := _UnsignedStateTx, updates := _Updates} = UpdateVolley().

dry_call_a_contract(Function, Argument, CPubKey, Contract, SenderConnPid, Config) ->
    dry_call_a_contract(Function, Argument, CPubKey, Contract, SenderConnPid, 0, Config).

dry_call_a_contract(Function, Argument, ContractPubKey, Contract, SenderConnPid, Amount, Config) ->
    dry_call_a_contract(Function, Argument, ContractPubKey, Contract,
                        SenderConnPid, Amount, Config, #{}).

dry_call_a_contract(Function, Argument, ContractPubKey, Contract, SenderConnPid,
                    Amount, Config, XOpts) ->
    {ok, EncodedMainData} = encode_call_data(Contract, Function, Argument),
    ok = ?WS:register_test_for_channel_event(SenderConnPid, dry_run),
    CallOpts0 =
        XOpts#{ contract_id => aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
              , abi_version => aect_test_utils:abi_version()
              , amount      => Amount
              , call_data   => EncodedMainData},
    CallOpts = maybe_include_meta(CallOpts0, Config),
    % invalid call
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{amount => <<"1">>}, Config),
    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{abi_version => <<"1">>}, Config),
    {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{call_data => <<"ABCDEFG">>}, Config),
    {ok, _, #{<<"reason">> := <<"broken_encoding: bytearray">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    ws_send_tagged(SenderConnPid, <<"channels.dry_run.call_contract">>,
                   CallOpts#{contract_id => <<"ABCDEFG">>}, Config),
    {ok, _, #{<<"reason">> := <<"broken_encoding: contracts">>}} =
        wait_for_channel_event(SenderConnPid, error, Config),
    {ok, _, <<"call_contract">>, CallRes} = wait_for_channel_event(SenderConnPid, dry_run, Config),
    ok = ?WS:unregister_test_for_channel_event(SenderConnPid, dry_run),
    #{<<"caller_id">>         := _CallerId,
      <<"caller_nonce">>      := CallRound,
      <<"contract_id">>       := _ContractId,
      <<"gas_price">>         := _,
      <<"gas_used">>          := _,
      <<"height">>            := CallRound,
      <<"return_type">>       := <<"ok">>,
      <<"return_value">>      := ReturnValue} = CallRes,
    decode_call_result(Contract, Function, ok, ReturnValue).

encode_call_data(Name, Fun, Args) when is_atom(Name) ->
    encode_call_data(contract_code(Name), Fun, Args);
encode_call_data(Src, Fun, Args) ->
    {ok, CallData} = aect_test_utils:encode_call_data(aect_test_utils:sophia_version(), Src, Fun, Args),
    {ok, aeser_api_encoder:encode(contract_bytearray, CallData)}.

contract_code(ContractName) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), ContractName),
    BinSrc.

contract_byte_code(ContractName) ->
    {ok, BinCode} = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), ContractName),
    aeser_api_encoder:encode(contract_bytearray, BinCode).

get_contract_bytecode(ContractName) ->
    {ok, contract_byte_code(ContractName)}.

contract_create_init_arg(identity) ->
    [];
contract_create_init_arg(counter) ->
    ["21"];
contract_create_init_arg(spend_test) ->
    [].

wait_for_signed_transaction_in_pool(SignedTx) ->
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    WaitForTx =
        fun Try(0) -> no_transaction;
            Try(Attempts) ->
                case tx_in_mempool(TxHash) of
                    true  -> ok;
                    false ->
                        timer:sleep(10),
                        Try(Attempts - 1)
                end
            end,
    ok = WaitForTx(30). % 30 attempts * 10ms

wait_for_signed_transaction_in_block(SignedTx) ->
    Node = aecore_suite_utils:node_name(?NODE),
    TxHash = aeser_api_encoder:encode(tx_hash, Hash = aetx_sign:hash(SignedTx)),
    case rpc:call(Node, aec_chain, find_tx_location, [Hash]) of
        BHash when is_binary(BHash) ->
            case rpc:call(Node, aec_chain, hash_is_in_main_chain, [BHash]) of
                true ->
                    ct:log("Tx is already on chain (BHash = ~p)", [BHash]),
                    ok;
                false ->
                    ct:log("Tx found in a block off-chain (BHash = ~p)", [BHash]),
                    wait_for_tx_hash_on_chain(TxHash)
            end;
        _Other ->
            ct:log("Current Tx location: ~p", [_Other]),
            wait_for_tx_hash_on_chain(TxHash)
    end.

wait_for_tx_hash_on_chain(TxHash) ->
    Node = aecore_suite_utils:node_name(?NODE),
    case rpc:call(Node, aec_chain, find_tx_location, [TxHash]) of
        BlockHash when is_binary(BlockHash) ->
            ct:log("TxHash is already on chain (~p)", [TxHash]),
            ok;
        _ ->
            Rate = aecore_suite_utils:expected_mine_rate(),
            Opts = #{strictly_follow_top => true},
            case aecore_suite_utils:mine_blocks_until_txs_on_chain(
                   aecore_suite_utils:node_name(?NODE), [TxHash], Rate, ?MAX_MINED_BLOCKS, Opts) of
                {ok, _Blocks} -> ok;
                {error, _Reason} -> did_not_mine
            end
    end.

sc_ws_timeout_open(Config) ->
    with_trace(fun sc_ws_timeout_open_/1, Config, "sc_ws_timeout_open").

sc_ws_timeout_open_(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    IAmt = 8,
    RAmt = 4,

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt,
                                  #{timeout_accept => 500}, Config),
    {ok, IConnPid, _FsmId} = channel_ws_start(initiator, maps:put(host, <<"localhost">>, ChannelOpts), Config, [info]),
    ok = wait_for_channel_event(<<"timeout">>, IConnPid, info, Config),
    ok = wait_for_channel_event(<<"died">>, IConnPid, info, Config),
    ok = ?WS:unregister_test_for_channel_event(IConnPid, info),
    ok.

sc_ws_attach_initiator(Config) ->
    #{initiator := #{pub_key  := Pubkey,
                     priv_key := Privkey}} = proplists:get_value(participants, Config),
    attach({Pubkey, Privkey}, "simple_auth", "authorize", [?SIMPLE_AUTH_GA_SECRET]),
    ok.

sc_ws_attach_responder(Config) ->
    #{responder := #{pub_key  := Pubkey,
                     priv_key := Privkey}} = proplists:get_value(participants, Config),
    attach({Pubkey, Privkey}, "simple_auth", "authorize", [?SIMPLE_AUTH_GA_SECRET]),
    ok.

sc_ws_min_depth_not_reached_timeout(Config) ->
    with_trace(fun sc_ws_min_depth_not_reached_timeout_/1, Config,
               "sc_ws_min_depth_not_reached_timeout").

sc_ws_min_depth_not_reached_timeout_(Config) ->
    S = ?SLOGAN,
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    IAmt = 70000 * aec_test_utils:min_gas_price(),
    RAmt = 40000 * aec_test_utils:min_gas_price(),
    %% Set the `timeout_funding_lock` to something short enough that the test case completes in
    %% reasonable time, but not so short that the fsms time out before the `channel_create` has
    %% time to collect the `on_chain_tx` notifications for the `create_tx`. Also, not so long
    %% that the 12 second default timeout in `wait_for_channel_event/4` triggers.
    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt,
                                  #{ timeout_funding_lock => 3000
                                   , minimum_depth => 1
                                   , slogan => S }, Config),
    TestEvents = [info, get, sign, on_chain_tx],
    {ok, IConnPid, _IFsmId} = channel_ws_start(initiator,
                                      maps:put(host, <<"localhost">>, ChannelOpts), Config, TestEvents),

    {ok, RConnPid, _RFsmId} = channel_ws_start(responder, ChannelOpts, Config, TestEvents),

    channel_send_conn_open_infos(RConnPid, IConnPid, Config),

    channel_create(Config, IConnPid, RConnPid),

    % We've set the minimum_depth factor very low at channel opening, so the
    % transaction shouldn't be confirmed at this point.
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), ?DEFAULT_MIN_DEPTH),

    ConnDied =
        fun(Pid) ->
            %% there is a race condition between participant's FSMs: if
            %% responder timeouts before the initiator, it could close the
            %% noise connection, dragging the initator FSM with it, resulting
            %% in a timeout message not being sent
            try
                ok = wait_for_channel_event(<<"timeout">>, Pid, info, Config),
                ok = wait_for_channel_event(<<"died">>, Pid, info, Config)
            catch error:{connection_died, _} -> ok
            end
        end,

    ConnDied(IConnPid),
    ConnDied(RConnPid),
    ok.

sc_ws_reconnect_early(Config) ->
    S = ?SLOGAN,
    Role = initiator,
    MinBlocksToMine = 2,
    #{initiator := #{pub_key := _IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Config),

    Config1 = sc_ws_open_([{mine_create_tx, false}|Config], #{ slogan => S
                                                             , minimum_depth => 0},
                          MinBlocksToMine),

    ct:log("Channel set up, but tx not mined", []),

    #{ channel_id       := ChId
     , initiator_fsm_id := IFsmId
     , responder_fsm_id := RFsmId
     , initiator        := IConnPid } = proplists:get_value(channel_clients, Config1),

    ?WS:stop(IConnPid),
    ct:log("IConnPid killed", []),
    timer:sleep(100),
    ct:log("Reconnecting client ..." , []),
    Options = proplists:get_value(channel_options, Config1),
    %% TODO: Port shouldn't matter when reconnecting
    Port = maps:get(port, Options),
    ReestablishOpts = #{ existing_channel_id => ChId
                       , existing_fsm_id => [{initiator, IFsmId}, {responder, RFsmId}]
                       , port => Port
                       , protocol => maps:get(protocol, Options)},
    Config2 = reconnect_client_(ReestablishOpts, Role, [{scenario, reconnect} | Config1]),
    %%
    ok = finish_sc_ws_open(Config2, MinBlocksToMine),
    sc_ws_close_mutual_(Config2, Role),
    ok.


sc_ws_min_depth_is_modifiable(Config0) ->
    Config = sc_ws_open_(Config0, #{minimum_depth => 0}, 2),
    ok = sc_ws_update_(Config),
    ok = sc_ws_close_(Config).

sc_ws_basic_open_close(Config0) ->
    Config = sc_ws_open_(Config0),
    ok = sc_ws_update_(Config),
    ok = maybe_collect_deferred_min_depth_msg(Config),
    ok = sc_ws_close_(Config).

sc_ws_basic_open_close_server(Config0) ->
    Config = sc_ws_open_([server_mode|Config0]),
    ok = sc_ws_update_(Config),
    ok = sc_ws_close_(Config).

sc_ws_opening_ping_pong(Config0) ->
    Config = sc_ws_open_([{ping_pong, true} | Config0]),
    ok = sc_ws_close_(Config).

sc_ws_snapshot_solo(Config0) ->
    Config = sc_ws_open_(Config0),
    %% snapshots can only be taken when the latest state is
    %% an offchain_tx
    Ps = proplists:get_value(participants, Config),
    Cs = proplists:get_value(channel_clients, Config),
    Round0 = 2,
    ct:log("*** Initiator tries snapshot before there's an offchain_tx"
           " - should fail ***", []),
    {error, OffchainExpected}
        = perform_snapshot_solo(initiator, Round0,
                                Ps, Cs, Config),
    {json_rpc_error,
     #{ <<"message">> := <<"Action not allowed">>
      , <<"data">> := [#{ <<"code">> := 1012
                        , <<"message">> := <<"Offchain tx expected">> }] }}
        = OffchainExpected,
    assert_no_registered_events(?LINE, Config),
    ct:log("*** Perform updates to create an offchain_tx"
           " - should succeed ***", []),
    Round1 = sc_ws_update_basic_round_(Round0, Config),
    ct:log("*** Initiator tries snapshot - should succeed ***", []),
    {ok, Round2, _} = perform_snapshot_solo(initiator, Round1,
                                            Ps, Cs, Config),
    ct:log("*** Responder tries snapshot (no interleaved updates)"
           " - should succeed ***", []),
    {ok, no_update, _} = perform_snapshot_solo(responder, no_update,
                                               Ps, Cs, Config),
    ct:log("*** Responder tries another snapshot"
           " - should fail (already on chain) ***", []),
    {error, AlreadyOnchain}
        = perform_snapshot_solo(responder, Round2,
                                Ps, Cs, Config),
    {json_rpc_error, #{ <<"message">> := <<"Rejected">>
                      , <<"data">> := [#{ <<"code">> := 1013
                                        , <<"message">> :=
                                              <<"Tx already on-chain">> }] }}
        = AlreadyOnchain,
    ct:log("*** Perform another round of updates"
           " - should succeed ***", []),
    Round3 = sc_ws_update_basic_round_(Round2 + 1, Config),
    ct:log("*** Responder tries another snapshot"
           " - should succeed ***", []),
    {ok, _Round4, _} = perform_snapshot_solo(responder, Round3,
                                             Ps, Cs, Config),
    ct:log("*** Closing ***", []),
    ok = sc_ws_close_(Config).

assert_no_registered_events(L, Config) ->
    #{ initiator := ConnPidI, responder := ConnPidR }
        = proplists:get_value(channel_clients, Config),
    {L, ok} = {L, ?WS:get_registered_events(ConnPidI)},
    {L, ok} = {L, ?WS:get_registered_events(ConnPidR)},
    ok.

perform_snapshot_solo(Role, Round, Participants, Conns, Config) ->
    perform_snapshot_solo(Role, Round, Participants, Conns, Config, #{}).

perform_snapshot_solo(Role, Config, Params) ->
    Participants = proplists:get_value(participants, Config),
    Conns = proplists:get_value(channel_clients, Config),
    perform_snapshot_solo(Role, no_update, Participants, Conns, Config, Params).

perform_snapshot_solo(Role, Round, Participants, Conns, Config, Params) ->
    #{ priv_key := Privkey } = maps:get(Role, Participants),
    ConnPid = maps:get(Role, Conns),
    try {ok, SignedTx}
             = request_and_sign(
                 ConnPid, <<"snapshot_solo_tx">>, <<"channels.snapshot_solo_sign">>,
                 Privkey, Config,
                 fun() ->
                         case ?WS:json_rpc_call(
                                 ConnPid, #{ <<"method">> => <<"channels.snapshot_solo">>
                                           , <<"params">> => Params }) of
                             <<"ok">> -> ok;
                             Other    -> {error, Other}
                         end
                 end),
         TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
         Round1 = case Round of
                      no_update ->
                          Round;
                      _ when is_integer(Round) ->
                          ct:log("*** Verify that updates can be performed"
                                 " while waiting for snapshot confirmation ***", []),
                          sc_ws_update_basic_round_(Round+1, Config)
                  end,
         wait_for_onchain_tx_events(
           Conns, #{ <<"type">> => <<"channel_snapshot_solo_tx">> },
           fun() ->
                   wait_for_tx_hash_on_chain(TxHash)
           end, Config),
         MinBlocksToMine = ?DEFAULT_MIN_DEPTH,
         wait_for_info_msg(
           ConnPid, #{ <<"event">> => <<"min_depth_achieved">>
                     , <<"type">>  => <<"channel_snapshot_solo_tx">> },
           fun() ->
                   aecore_suite_utils:mine_key_blocks(
                     aecore_suite_utils:node_name(?NODE),
                     MinBlocksToMine +1)
           end, Config),
         {ok, Round1, SignedTx}
    catch
        error:Other ->
            ct:log("Got Other = ~p", [Other]),
            {error, Other}
    end.

wait_for_onchain_tx_events(#{ initiator := PidI
                            , responder := PidR }, Pat, F, Config) ->
    wait_for_onchain_tx_events([PidI, PidR], Pat, F, Config);
wait_for_onchain_tx_events(Pids, Pat, F, Config) when is_list(Pids)
                                                    , is_map(Pat)
                                                    , is_function(F, 0) ->
    with_registered_events(
      [on_chain_tx], Pids,
      fun() ->
              F(),
              Msgs =
                  lists:map(
                    fun(Pid1) ->
                            {ok, _, M1} = wait_for_channel_event_match(
                                           Pid1, on_chain_tx, Pat, Config),
                            M1
                    end, Pids),
              {ok, Msgs}
      end).

wait_for_info_msg(Pid, Pat, F, Config) ->
    wait_for_info_msgs([Pid], Pat, F, Config).

wait_for_info_msgs(#{ initiator := PidI
                    , responder := PidR }, Pat, F, Config) ->
    wait_for_info_msgs([PidI, PidR], Pat, F, Config);
wait_for_info_msgs(Pids, Pat, F, Config) when is_list(Pids)
                                            , is_map(Pat)
                                            , is_function(F, 0) ->
    with_registered_events(
      [info], Pids,
      fun() ->
              F(),
              Msgs =
                  lists:map(
                    fun(Pid1) ->
                            {ok, _, Msg} = wait_for_channel_event_match(Pid1, info, Pat, Config),
                            Msg
                    end, Pids),
              {ok, Msgs}
      end).

sc_ws_update_with_meta(Config0) ->
    Config = sc_ws_open_(Config0),
    ok = sc_ws_update_([{include_meta, true}|Config]),
    ok = sc_ws_close_(Config).

sc_ws_update_transfer(Config) ->
    ok = sc_ws_update_(Config).

sc_ws_basic_client_reconnect_i(Config) ->
    sc_ws_basic_client_reconnect_(initiator, Config).

sc_ws_basic_client_reconnect_r(Config) ->
    sc_ws_basic_client_reconnect_(responder, Config).

sc_ws_basic_client_reconnect_i_w_reestablish(Config) ->
    sc_ws_basic_client_reconnect_(initiator, [{reconnect_method, reestablish}|Config]).

sc_ws_basic_client_reconnect_(Role, Config0) ->
    Config = sc_ws_open_(Config0),
    Participants = proplists:get_value(participants, Config),
    #{ channel_id := ChId
     , initiator_fsm_id := IFsmId
     , responder_fsm_id := RFsmId
     , Role := ConnPid } = Clients = ?config(channel_clients, Config),
    unlink(ConnPid),
    ?WS:stop(ConnPid),
    ct:log("ConnPid killed", []),
    timer:sleep(100),
    OtherRole = other_role(Role),
    {error, conflict}
        = update_with_client_disconnected(
            OtherRole, OtherRole, Clients#{Role => undefined},
            Participants, Config),
    {ok, SignedTx} = update_with_client_disconnected(
        both, OtherRole, Clients#{Role => undefined},
        Participants, Config),
    ct:log("Reconnecting client ..." , []),
    Options = proplists:get_value(channel_options, Config),
    %% TODO: Port shouldn't matter when reconnecting
    Port = maps:get(port, Options),
    ReestablishOpts = #{ existing_channel_id => ChId
                       , existing_fsm_id => [{initiator, IFsmId}, {responder, RFsmId}]
                       , offchain_tx => SignedTx
                       , port => Port
                       , protocol => maps:get(protocol, Options)},
    sc_ws_fsm_id_errors([Role], ReestablishOpts, Config),
    Config1 = reconnect_client_(ReestablishOpts, Role, Config),
    sc_ws_close_mutual_(Config1, Role).

update_with_client_disconnected(SignAs, Role, Clients, Participants, Config) ->
    OtherRole = other_role(Role),
    ConnPid = maps:get(Role, Clients),
    #{ pub_key  := FromPubkey
     , priv_key := FromPrivkey } = maps:get(Role, Participants),
    #{ pub_key  := ToPubkey
     , priv_key := ToPrivkey } = maps:get(OtherRole, Participants),
    Privkeys = case SignAs of
                   both ->
                       [FromPrivkey, ToPrivkey];
                   Role ->
                       [FromPrivkey]
               end,
    ok = ?WS:register_test_for_channel_events(ConnPid, [sign, update, conflict]),

    SignUpdate =
        fun (Pid, Pkeys) ->
            case wait_for_channel_event(Pid, sign, Config) of
                {ok, _, <<"update">>, #{<<"signed_tx">> := EncSignedTx0}} ->
                    {ok, SignedBinTx} =
                        aeser_api_encoder:safe_decode(transaction, EncSignedTx0),
                    STx = aetx_sign:deserialize_from_binary(SignedBinTx),
                    SignedTx = lists:foldl(
                                 fun(PK, STx1) ->
                                         aec_test_utils:co_sign_tx(
                                           STx1, PK)
                                 end, STx, Pkeys),
                    EncSignedTx = aeser_api_encoder:encode(
                                    transaction,
                                    aetx_sign:serialize_to_binary(SignedTx)),
                    ws_send_tagged(Pid, <<"channels.update">>,
                                   #{signed_tx => EncSignedTx}, Config)
            end
        end,
    %% sender initiates an update
    ws_send_tagged(ConnPid, <<"channels.update.new">>,
                   #{from => aeser_api_encoder:encode(account_pubkey, FromPubkey),
                     to => aeser_api_encoder:encode(account_pubkey, ToPubkey),
                     amount => 1}, Config),

    %% starter signs the new state
    SignUpdate(ConnPid, Privkeys),

    Res = case SignAs of
              both ->
                  {ok, _, #{ <<"state">> := NewState }}
                      = wait_for_channel_event(ConnPid, update, Config),
                  {ok, NewState};
              Role ->
                  {ok, _, Conflict} = wait_for_channel_event(ConnPid, conflict, Config),
                  ct:log("Conflict: ~p", [Conflict]),
                  {error, conflict}
          end,

    ok = ?WS:unregister_test_for_channel_events(ConnPid, [sign, update, conflict]),

    Res.

other_role(responder) -> initiator;
other_role(initiator) -> responder.

reconnect_client_(ReestablishOpts, Role, Config) ->
    ct:log("Trying to reconnect via reestablish", []),
    ct:log("Config = ~p", [Config]),
    ?CHECK_INFO(20),
    ct:log("ReestablishOpts = ~p", [ReestablishOpts]),
    ReestablishOpts1 = case Role of
                           initiator ->
                               maps:put(host, <<"localhost">>, ReestablishOpts);
                           responder ->
                               ReestablishOpts
                       end,
    {ok, ConnPid, FsmId} = channel_ws_start(Role, ReestablishOpts1, Config),
    ct:log("New ConnPid = ~p", [ConnPid]),
    ct:log("Check if reestablish resulted in a reconnect", []),
    OldClients = ?config(channel_clients, Config),
    ct:log("OldClients = ~p", [OldClients]),
    {OldFsmId, NewClients} = case Role of
                                 initiator ->
                                     { maps:get(initiator_fsm_id, OldClients)
                                     , #{initiator => ConnPid, initiator_fsm_id => FsmId}};
                                 responder ->
                                     { maps:get(responder_fsm_id, OldClients)
                                     , #{responder => ConnPid, responder_fsm_id => FsmId}}
                             end,
    ct:log("Old fsm id: ~p", [OldFsmId]),
    ct:log("New fsm id: ~p", [FsmId]),
    Scenario = proplists:get_value(reconnect_scenario, Config, reconnect),
    case Scenario of
        reconnect ->
            ct:log("Checking for reconnect", []),
            %% The FsmId should not change
            {FsmId, OldFsmId} = {OldFsmId, FsmId};
        reestablish ->
            ct:log("Checking for reestablish", []),
            %% A new Fsm was spawned
            true = (FsmId =/= OldFsmId)
    end,
    Config1 = lists:keystore(channel_clients, 1, Config,
                            {channel_clients, maps:merge(OldClients, NewClients)}),
    case Scenario of
        reconnect ->
            ok;
        reestablish ->
            await_reestablish_reports(Config1)
    end,
    Config1.

sc_ws_failed_update(Config) ->
    ChannelClients = proplists:get_value(channel_clients, Config),
    Participants = proplists:get_value(participants, Config),
    lists:foreach(
        fun(Sender) ->
            LogPid = maps:get(Sender, ChannelClients),
            ?WS:log(LogPid, info, "Failing update, insufficient balance"),
            {ok, _, #{ <<"reason">> := <<"insufficient_balance">>
                     , <<"request">> := _Request0 }} =
                channel_update_fail(ChannelClients, Sender, Participants,
                                    10000000 * aec_test_utils:min_gas_price(), % try sending too much
                                    Config),
            ?WS:log(LogPid, info, "Failing update, negative amount"),
            {ok, _, #{ <<"reason">> := <<"negative_amount">>
                     , <<"request">> := _Request1 }} =
                channel_update_fail(ChannelClients, Sender, Participants, -1, Config),
            ?WS:log(LogPid, info, "Failing update, invalid pubkeys"),
            {ok, _, #{ <<"reason">> := <<"invalid_pubkeys">>
                     , <<"request">> := _Request2 }} =
                channel_update_fail(ChannelClients, Sender,
                                    #{initiator => #{pub_key => <<42:32/unit:8>>},
                                      responder => #{pub_key => <<43:32/unit:8>>}},
                                    1, Config),
            ok
        end,
        [initiator, responder]),
    ok.

sc_ws_generic_messages(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder := RConnPid}
        = proplists:get_value(channel_clients, Config),
    lists:foreach(
        fun({Sender, Msg}) ->
            {SenderPubkey, ReceiverPubkey, SenderPid, ReceiverPid} =
                case Sender of
                    initiator ->
                        {IPubkey, RPubkey, IConnPid, RConnPid};
                    responder ->
                        {RPubkey, IPubkey, RConnPid, IConnPid}
                end,
                SenderEncodedK = aeser_api_encoder:encode(account_pubkey, SenderPubkey),
                ReceiverEncodedK = aeser_api_encoder:encode(account_pubkey, ReceiverPubkey),
                ok = ?WS:register_test_for_channel_event(ReceiverPid, message),

                ws_send_tagged(SenderPid, <<"channels.message">>,
                        #{<<"to">> => ReceiverEncodedK,
                          <<"info">> => Msg}, Config),

                {ok, _, #{<<"message">> := #{ <<"from">> := SenderEncodedK
                                            , <<"to">> := ReceiverEncodedK
                                            , <<"info">> := Msg} }}
                    = wait_for_channel_event(ReceiverPid, message, Config),
                ok = ?WS:unregister_test_for_channel_event(ReceiverPid, message)
        end,
      [ {initiator, <<"hejsan">>}                   %% initiator can send
      , {responder, <<"svejsan">>}                  %% responder can send
      , {initiator, <<"first message in a row">>}   %% initiator can send two messages in a row
      , {initiator, <<"second message in a row">>}
      , {responder, <<"some message">>}             %% responder can send two messages in a row
      , {responder, <<"other message">>}
      ]),
    ok.

sc_ws_conflict_two_offchain_updates(Config) ->
    sc_ws_conflict_(
        fun(Alice, Bob) -> update_params(Alice, Bob, _Amt1 = 1) end,
        fun(Alice, Bob) -> update_params(Alice, Bob, _Amt2 = 2) end,
        Config).

sc_ws_conflict_deposit_and_offchain_update(Config) ->
    sc_ws_conflict_(
        fun(_Alice, _Bob) -> deposit_params(_Amt1 = 1) end,
        fun(Alice, Bob) -> update_params(Alice, Bob, _Amt2 = 2) end,
        Config),
    sc_ws_conflict_(
        fun(Alice, Bob) -> update_params(Alice, Bob, _Amt2 = 2) end,
        fun(_Alice, _Bob) -> deposit_params(_Amt1 = 1) end,
        Config).

sc_ws_conflict_two_deposits(Config) ->
    sc_ws_conflict_(
        fun(_Alice, _Bob) -> deposit_params(_Amt1 = 1) end,
        fun(_Alice, _Bob) -> deposit_params(_Amt2 = 2) end,
        Config).

sc_ws_conflict_withdrawal_and_offchain_update(Config) ->
    sc_ws_conflict_(
        fun(_Alice, _Bob) -> withdraw_params(_Amt1 = 1) end,
        fun(Alice, Bob) -> update_params(Alice, Bob, _Amt2 = 2) end,
        Config),
    sc_ws_conflict_(
        fun(Alice, Bob) -> update_params(Alice, Bob, _Amt2 = 2) end,
        fun(_Alice, _Bob) -> withdraw_params(_Amt1 = 1) end,
        Config).

sc_ws_conflict_withdrawal_and_deposit(Config) ->
    sc_ws_conflict_(
        fun(_Alice, _Bob) -> withdraw_params(_Amt1 = 1) end,
        fun(_Alice, _Bob) -> deposit_params(_Amt2 = 2) end,
        Config),
    sc_ws_conflict_(
        fun(_Alice, _Bob) -> deposit_params(_Amt2 = 2) end,
        fun(_Alice, _Bob) -> withdraw_params(_Amt1 = 1) end,
        Config).

sc_ws_conflict_two_withdrawals(Config) ->
    sc_ws_conflict_(
        fun(_Alice, _Bob) -> withdraw_params(_Amt1 = 1) end,
        fun(_Alice, _Bob) -> withdraw_params(_Amt2 = 1) end,
        Config).

sc_ws_conflict_on_new_offchain(Config) ->
    sc_ws_conflict_new_tx_(
        fun(_Alice, _Bob) -> withdraw_params(_Amt1 = 1) end,
        fun(Alice, Bob) -> update_params(Alice, Bob, _Amt2 = 2) end,
        Config).

sc_ws_conflict_new_tx_(StarterAction, AckAction, Config) ->
    Participants = proplists:get_value(participants, Config),
    ChannelClients = proplists:get_value(channel_clients, Config),

    Trigger =
        fun(StarterPid, StarterPubkey, StarterPrivkey,
            AcknowledgerPid, AcknowledgerPubkey, _AcknowledgerPrivkey,
            SignUpdate) ->
                %% starter initiates an update
                {StarterMethod, StarterParams, StarterEvent} =
                    StarterAction(StarterPubkey, AcknowledgerPubkey),
                ws_send_tagged(StarterPid, StarterMethod, StarterParams, Config),
                {ok, _, StarterEvent, #{<<"signed_tx">> := StarterTx}} =
                    wait_for_channel_event(StarterPid, sign, Config),
                SignUpdate(StarterPid, StarterPrivkey, StarterTx,
                           tag_to_method(StarterEvent)),

                wait_for_channel_event(AcknowledgerPid, sign, Config),
                %% acknowledger initiates an update too
                {AckMethod, AckParams, _AckEvent} =
                    AckAction(StarterPubkey, AcknowledgerPubkey),
                ws_send_tagged(AcknowledgerPid, AckMethod, AckParams, Config),
                {ok, _, #{ <<"error_code">> := 2
                        , <<"error_msg">> := <<"conflict">> }} = wait_for_channel_event(StarterPid, conflict, Config),
                {ok, _, #{ <<"error_code">> := 2
                        , <<"error_msg">> := <<"conflict">> }} = wait_for_channel_event(AcknowledgerPid, conflict, Config),
                ok
        end,
    lists:foreach(
        fun(FirstSender) ->
                channel_conflict(ChannelClients, FirstSender, Participants,
                                 Config, Trigger)
        end,
        [initiator,
         responder]),
    ok.
    

update_params(ParticipantA, ParticipantB, Amount) ->
    {<<"channels.update.new">>,
     #{ from => aeser_api_encoder:encode(account_pubkey, ParticipantA),
        to => aeser_api_encoder:encode(account_pubkey, ParticipantB),
        amount => Amount},
     <<"update">>}.

deposit_params(Amount) ->
    {<<"channels.deposit">>,
     #{amount => Amount},
     <<"deposit_tx">>}.

withdraw_params(Amount) ->
    {<<"channels.withdraw">>,
     #{amount => Amount},
     <<"withdraw_tx">>}.

snapshot_params() ->
    {<<"channels.snapshot_solo">>,
     #{},
     <<"snapshot_solo_tx">>}.

sc_ws_conflict_(StarterAction, AckAction, Config) ->
    Participants = proplists:get_value(participants, Config),
    ChannelClients = proplists:get_value(channel_clients, Config),
    Trigger =
        fun(StarterPid, StarterPubkey, StarterPrivkey,
            AcknowledgerPid, AcknowledgerPubkey, AcknowledgerPrivkey,
            SignUpdate) ->
                {StarterMethod, StarterParams, StarterEvent} =
                    StarterAction(StarterPubkey, AcknowledgerPubkey),

                {AckMethod, AckParams, AckEvent} =
                    AckAction(StarterPubkey, AcknowledgerPubkey),
                ws_send_tagged(StarterPid, StarterMethod, StarterParams, Config),
                ws_send_tagged(AcknowledgerPid, AckMethod, AckParams, Config),
                sign_or_wait(StarterPid, AcknowledgerPid,
                             StarterPrivkey, AcknowledgerPrivkey,
                             StarterEvent, AckEvent,
                             SignUpdate,
                             Config),
                {ok, _, #{ <<"error_code">> := 2
                        , <<"error_msg">> := <<"conflict">> }} = wait_for_channel_event(StarterPid, conflict, Config),
                {ok, _, #{ <<"error_code">> := 2
                        , <<"error_msg">> := <<"conflict">> }} = wait_for_channel_event(AcknowledgerPid, conflict, Config),
                ok
        end,
    lists:foreach(
        fun(FirstSender) ->
                channel_conflict(ChannelClients, FirstSender, Participants,
                                 Config, Trigger)
        end,
        [initiator,
         responder]),
    ok.

tag_to_method(<<"update">>) -> <<"channels.update">>;
tag_to_method(<<"deposit_tx">>) -> <<"channels.deposit_tx">>;
tag_to_method(<<"withdraw_tx">>) -> <<"channels.withdraw_tx">>;
tag_to_method(<<"snapshot_solo_tx">>) -> <<"channels.snapshot_solo_sign">>.

sign_or_wait(ConnPidA, ConnPidB, PrivkeyA, PrivkeyB,
             EventA, EventB, SignUpdate, Config) ->
    WhoSigns = ?config(who_signs_update, Config),
    WaitForCorrectEvent =
        fun Fun(ConnPid, Event) ->
            {ok, _, ReceivedEvent, #{<<"signed_tx">> := Tx}} =
                wait_for_channel_event(ConnPid, sign, Config),
            case ReceivedEvent =:= Event of
                true -> Tx;
                false ->
                    ct:log("While waiting for a ~p, discarding received sign event ~p",
                           [Event, ReceivedEvent]),
                    Fun(ConnPid, Event)
            end
        end,
    TxA = WaitForCorrectEvent(ConnPidA, EventA),

    TxB = WaitForCorrectEvent(ConnPidB, EventB),

    case WhoSigns of
        noone ->
            pass;
        one ->
            SignUpdate(ConnPidA, PrivkeyA, TxA, tag_to_method(EventA));
        both ->
            SignUpdate(ConnPidA, PrivkeyA, TxA, tag_to_method(EventA)),
            SignUpdate(ConnPidB, PrivkeyB, TxB, tag_to_method(EventB))
    end,
    ok.

sc_ws_update_abort(Config) ->
    #{ initiator := #{pub_key := IPubkey}
     , responder := #{pub_key := RPubkey} } = proplists:get_value(participants, Config),
    #{ initiator := IConnPid
     , responder := RConnPid } = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, update, conflict]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, update, conflict]),
    Balances = sc_ws_get_both_balances(IConnPid, IPubkey, RPubkey, Config),
    {ok, {Bi0, Br0}} = Balances,
    UpdateOpts = #{ from   => aeser_api_encoder:encode(account_pubkey, IPubkey)
                  , to     => aeser_api_encoder:encode(account_pubkey, RPubkey)
                  , amount => 1 },
    ws_send_tagged(IConnPid, <<"channels.update.new">>, UpdateOpts, Config),
    channel_abort_sign_tx(IConnPid, 147, <<"channels.update">>, Config),
    {ok, _, #{<<"event">> := <<"aborted_update">>}} =
        wait_for_channel_event(IConnPid, info, Config),
    %%
    {ok, {Bi0, Br0}} = sc_ws_get_both_balances(IConnPid, IPubkey, RPubkey, Config),
    %%
    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, info, update, conflict]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, info, update, conflict]),
    ok.

sc_ws_close_mutual(Config) ->
    with_trace(fun sc_ws_close_mutual_/1, Config, "sc_ws_close_mutual").

sc_ws_close_solo(Config) ->
    with_trace(fun sc_ws_close_solo_/1, Config, "sc_ws_close_solo").

sc_ws_close_solo_(Config0) ->
    lists:foreach(
      fun(WhoCloses) ->
              S = ?SLOGAN(WhoCloses),
              Config = sc_ws_open_(Config0, #{slogan => S}),
              sc_ws_close_solo_(Config, WhoCloses, #{}),
              settle_(Config, WhoCloses)
      end, [initiator, responder]).

sc_ws_slash(Config) ->
    with_trace(fun sc_ws_slash_/1, Config, "sc_ws_slash").

sc_ws_slash_(Config0) ->
    lists:foreach(
      fun({WhoCloses, WhoSlashes, WhoSettles}) ->
              S = ?SLOGAN([WhoCloses, ",", WhoSlashes]),
              Config = sc_ws_open_(Config0, #{slogan => S}),
              ct:log("Channel opened, Slogan = ~p", [S]),
              sc_ws_slash_(Config, WhoCloses, WhoSlashes, #{}),
              settle_(Config, WhoSettles)
      end, [{A,B, C} || A <- [initiator],
                        B <- [initiator, responder],
                        C <- [initiator]]).

sc_ws_slash_(Config, WhoCloses, WhoSlashes, Params) ->
    ct:log("WhoCloses  = ~p~n"
           "WhoSlashes = ~p~n", [WhoCloses, WhoSlashes]),
    true = lists:member(WhoCloses, ?ROLES),
    true = lists:member(WhoSlashes, ?ROLES),
    ct:log("ConfigList = ~p", [Config]),
    #{initiator := #{pub_key  := IPubKey},
      responder := #{pub_key  := RPubKey}} = Participants =
        proplists:get_value(participants, Config),
    #{initiator := IConnPid} = Conns =
        proplists:get_value(channel_clients, Config),
    %%
    %% Fetch ChId and POI for initial state
    PoiScope = #{accounts => [aeser_api_encoder:encode(account_pubkey, Acc)
                              || Acc <- [IPubKey, RPubKey]]},
    {ok, #{ channel_id := ChIdEnc
          , poi := PoiEnc}} = sc_ws_get_poi_(IConnPid, PoiScope, Config),
    {channel, ChId} = aeser_api_encoder:decode(ChIdEnc),
    {poi, PoiSer} = aeser_api_encoder:decode(PoiEnc),
    Poi = aec_trees:deserialize_poi(PoiSer),
    %% create a new offchain state

    channel_update(Conns, initiator, Participants, 1, 2,
                   _TestErrors = false, Config),
    wait_for_info_msgs(
      Conns, #{ <<"event">> => <<"closing">> },
      fun() ->
              %%
              %% Both sides detect slash potential
              %%
              wait_for_onchain_tx_events(
                Conns, #{ <<"info">> => <<"can_slash">>
                        , <<"type">> => <<"channel_offchain_tx">> },
                fun() ->
                        sc_ws_cheating_close_solo_(Config, ChId, Poi, WhoCloses)
                end, Config)
      end, Config),
    %%
    %% WhoSlashes initiates a slash
    %%
    SlasherPid = maps:get(WhoSlashes, Conns),
    {ok, SignedSlashTx} = request_and_sign_slash_tx(
                            SlasherPid, WhoSlashes, Config,
                           fun() ->
                                   {ok, <<"ok">>} = request_slash(SlasherPid, Params),
                                   ok
                           end),
    ct:log("SignedSlashTx = ~p", [SignedSlashTx]),
    SlashTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSlashTx)),
    ct:log("SlashTxHash = ~p", [SlashTxHash]),
    wait_for_onchain_tx_events(
      Conns, #{ <<"info">> => <<"solo_closing">>
              , <<"type">> => <<"channel_slash_tx">> },
      fun() ->
              ok = wait_for_tx_hash_on_chain(SlashTxHash)
      end, Config),
    {ok, SignedSlashTx}.

request_and_sign_slash_tx(ConnPid, Who, Config, ReqF) ->
    Participants = proplists:get_value(participants, Config),
    #{priv_key := PrivKey} = maps:get(Who, Participants),
    request_and_sign(ConnPid, <<"slash_tx">>, <<"channels.slash_sign">>, PrivKey, Config, ReqF).

%% This wrapper is used to ensure that event registration for signing is
%% done before e.g. performing an rpc which triggers the signing.
request_and_sign(ConnPid, Tag, ReplyMethod, PrivKey, Config, ReqF) ->
    with_registered_events(
      [sign], [ConnPid],
      fun() ->
              case ReqF() of
                  ok ->
                      sign_tx_(ConnPid, Tag, ReplyMethod, PrivKey, Config);
                  {error, _} = Error ->
                      Error
              end
      end).

sign_tx(ConnPid, Tag, ReplyMethod, Privkey, Config) ->
    with_registered_events(
      [sign], [ConnPid],
      fun() ->
              sign_tx_(ConnPid, Tag, ReplyMethod, Privkey, Config)
      end).

sign_tx_(ConnPid, Tag, ReplyMethod, Privkey, Config) ->
    {ok, _, Tag, #{<<"signed_tx">> := EncSTx}} =
        wait_for_channel_event(ConnPid, sign, Config),
    {ok, BinSTx} = aeser_api_encoder:safe_decode(transaction, EncSTx),
    STx = aetx_sign:deserialize_from_binary(BinSTx),
    SignedTx = aec_test_utils:co_sign_tx(STx, Privkey),
    EncSignedTx = aeser_api_encoder:encode(
                    transaction,
                    aetx_sign:serialize_to_binary(SignedTx)),
    ws_send_tagged(ConnPid, ReplyMethod, #{signed_tx => EncSignedTx}, Config),
    {ok, SignedTx}.

register_channel_events(Events, Pids) ->
    [ok = ?WS:register_test_for_channel_events(Pid, Events)
     || Pid <- Pids],
    ok.

unregister_channel_events(Events, Pids) ->
    [ok = ?WS:unregister_test_for_channel_events(Pid, Events)
     || Pid <- Pids],
    ok.

with_registered_events(Events, Pids, F) ->
    ok = register_channel_events(Events, Pids),
    try
        F()
    after
        ok = unregister_channel_events(Events, Pids)
    end.

%% avoid_double_reg(Events, Pids, F) ->
%%     ok = unregister_channel_events(Events, Pids),
%%     Res = F(),
%%     ok = register_channel_events(Events, Pids),
%%     Res.

sc_ws_cheating_close_solo_(Config, ChId, Poi, WhoCloses) ->
    %% Create a close_solo based on the create_tx
    Participants = proplists:get_value(participants, Config),
    Keys = maps:get(WhoCloses, Participants),
    #{pub_key := PubKey, priv_key := PrivKey} = Keys,
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [PubKey]),
    TTL = current_height() + 10,
    Fee = 30000 * aec_test_utils:min_gas_price(),
    ct:log("Will try to create close_solo_tx~n"
           "ChId = ~p~n"
           "PubKey = ~p~n", [ChId, PubKey]),
    TxOpts = #{ channel_id => aeser_id:create(channel, ChId)
              , from_id    => aeser_id:create(account, PubKey)
              , payload    => <<>>
              , poi        => Poi
              , ttl        => TTL
              , fee        => Fee
              , nonce      => Nonce
              },
    ct:log("close_solo_tx: ~p", [TxOpts]),
    {ok, Tx} = aesc_close_solo_tx:new(TxOpts),
    SignedTx = aec_test_utils:sign_tx(Tx, [PrivKey]),
    ok = rpc(dev1, aec_tx_pool, push, [SignedTx]),
    wait_for_signed_transaction_in_block(SignedTx),
    ok.

sc_ws_leave_reestablish(Config0) ->
    Config = sc_ws_open_([{slogan, ?SLOGAN}|Config0]),
    ReestablishOptions = sc_ws_leave_(Config),
    Config1 = sc_ws_reestablish_(ReestablishOptions, Config),
    ok = sc_ws_update_(Config1),
    ok = sc_ws_close_(Config1).

sc_ws_leave_reestablish_wrong_fsm_id(Config0) ->
    Config = sc_ws_open_([{slogan, ?SLOGAN}|Config0]),
    ReestablishOptions = sc_ws_leave_(Config),

    sc_ws_fsm_id_errors([initiator, responder], ReestablishOptions, Config),
    Config1 = sc_ws_reestablish_(ReestablishOptions, Config),
    ok = sc_ws_update_(Config1),
    ok = sc_ws_close_(Config1).

sc_ws_leave_reestablish_responder_stays(Config0) ->
    Config = sc_ws_open_([{slogan, ?SLOGAN}|Config0], #{responder_opts => #{keep_running => true}}),
    Config1 = [{responder_leaves, false}|Config],
    ReestablishOptions = sc_ws_leave_(Config1),
    Config2 = sc_ws_reestablish_(ReestablishOptions, Config1),
    ok = sc_ws_update_(Config2),
    ok = sc_ws_close_(Config2).

sc_ws_leave_reconnect(Config0) ->
    ct:log("opening channel", []),
    ct:log("Config0 = ~p", [Config0]),
    Config = sc_ws_open_([{slogan, ?SLOGAN}|Config0], #{responder_opts => #{keep_running => true}}),
    ct:log("channel opened", []),
    ct:log("Config = ~p", [Config]),
    ct:log("*** Leaving channel ***", []),
    Config1 = [{responder_leaves, false}|Config],
    ct:log("Config1 = ~p", [Config1]),
    ReestablishOptions = sc_ws_leave_(Config1),
    ct:log("ReestablishOpts = ~p", [ReestablishOptions]),
    ct:log("*** Testing invalid reconneciton requests ***", []),
    sc_ws_fsm_id_errors([initiator], ReestablishOptions, Config),
    ct:log("*** Reconnecting ... ***", []),
    Config2 = reconnect_client_(ReestablishOptions, initiator,
                                [{reconnect_scenario, reestablish} | Config1]),
    ct:log("*** Verifying that channel is operational ***", []),
    ok = sc_ws_update_(Config2),
    ct:log("*** Closing ... ***", []),
    ok = sc_ws_close_(Config2).


sc_ws_ping_pong(Config) ->
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, Config),
    ping_pong(IConnPid, Config),
    ping_pong(RConnPid, Config),
    ok.

ping_pong(ConnPid, Config) ->
    ok = ?WS:register_test_for_channel_events(ConnPid, [system]),
    ok = ws_send_tagged(ConnPid, <<"channels.system">>,
                        #{<<"action">> => <<"ping">>}, Config),
    {ok, _, <<"pong">>, #{}} =
        wait_for_channel_event(ConnPid, system, Config),
    ok = ?WS:unregister_test_for_channel_events(ConnPid, [system]),
    ok.

sc_ws_get_history(Config) ->
    #{initiator := Pid} =
        proplists:get_value(channel_clients, Config),
    Res0 = call_fetch_rpc(Pid, #{}),
    ct:log("Res0 = ~p", [Res0]),
    [_,_,_,_|_] = Res0,
    %%
    Res = call_fetch_rpc(Pid, #{n => 1000}),     %% more than what's in the log
    ct:log("Res = ~p", [Res]),
    [_,_,_,_|_] = Res,
    %%
    Res1 = call_fetch_rpc(Pid, #{n => 3}),
    ct:log("Res1 = ~p", [Res1]),
    [_,_,_] = Res1,
    %%
    Res2 = call_fetch_rpc(Pid, #{n => 1, type => [rpt], tag => [update]}),
    ct:log("Res2 = ~p", [Res2]),
    [#{<<"type">> := <<"report">>}] = Res2,
    %%
    Res3 = call_fetch_rpc(Pid, #{n => 1, type => [rcv]}),
    ct:log("Res3 = ~p", [Res3]),
    [#{<<"type">> := <<"receive">>}] = Res3,
    %%
    Res4 = call_fetch_rpc(Pid, #{n => 1, type => [snd]}),
    ct:log("Res4 = ~p", [Res4]),
    [#{<<"type">> := <<"send">>}] = Res4,
    ok.

call_fetch_rpc(ConnPid, Params) ->
    ?WS:json_rpc_call(
       ConnPid, #{ <<"method">> => <<"channels.history.fetch">>
                 , <<"params">> => Params }).
    

sc_ws_fsm_id_errors(Roles, ReestablishOptions, Config) ->
    TestError =
        fun(Opts, Error) ->
            [sc_ws_test_broken_params(Role, Config, Opts, Error, Config) || Role <- Roles]
        end,

    %% Missing fsm id
    TestError(maps:remove(existing_fsm_id, ReestablishOptions), <<"Missing field: existing_fsm_id">>),

    %% Encoding
    TestError(ReestablishOptions#{existing_fsm_id => ?BOGUS_FSM_ID}, <<"Invalid fsm id">>),
    <<InvalidLenFsmId:8/bytes, _/binary>> = aesc_fsm_id:retrieve(aesc_fsm_id:new()),
    InvalidLenFsmIdEnc = aeser_api_encoder:encode(bytearray, InvalidLenFsmId),
    TestError(ReestablishOptions#{existing_fsm_id => InvalidLenFsmIdEnc}, <<"Invalid fsm id">>),

    %% Invalid value but valid format
    ValidFormatFsmId = aesc_fsm_id:retrieve_for_client(aesc_fsm_id:new()),
    TestError(ReestablishOptions#{existing_fsm_id => ValidFormatFsmId}, <<"Invalid fsm id">>).

sc_ws_deposit(Config) ->
    lists:foreach(
        fun(Depositor) ->
            sc_ws_deposit_(Config, Depositor, maybe_include_meta(#{}, Config)),
            ok
        end,
        [initiator, responder]).

sc_ws_withdraw(Config) ->
    lists:foreach(
        fun(Depositor) ->
            sc_ws_withdraw_(Config, Depositor, maybe_include_meta(#{}, Config)),
            ok
        end,
        [initiator, responder]).

%% channel_options(IPubkey, RPubkey, IAmt, RAmt) ->
%%     channel_options(IPubkey, RPubkey, IAmt, RAmt, #{}).

channel_options(IPubkey, RPubkey, IAmt, RAmt, Other, Config) ->
    lists:foldl(
        fun(MaybeSet, Accum) ->
            MaybeSet(Accum, Config)
        end,
        maps:merge(#{ port => ?config(ws_port, Config)
                    , initiator_id => aeser_api_encoder:encode(account_pubkey, IPubkey)
                    , responder_id => aeser_api_encoder:encode(account_pubkey, RPubkey)
                    , lock_period => 10
                    , push_amount => 1
                    , initiator_amount => IAmt
                    , responder_amount => RAmt
                    , channel_reserve => 2
                    , keep_running => false
                    , protocol => sc_ws_protocol(Config)
                    }, Other),
        [fun maybe_add_fee/2,
         fun maybe_add_gas_price/2,
         fun maybe_add_nonce/2,
         fun maybe_add_slogan/2
        ]).


maybe_add_slogan(Map, Config) ->
    maybe_add_param(slogan, Map, Config).

maybe_add_fee(Map, Config) ->
    maybe_add_param(fee, Map, Config).

maybe_add_gas_price(Map, Config) ->
    maybe_add_param(gas_price, Map, Config).

maybe_add_nonce(Map, Config) ->
    maybe_add_param(nonce, Map, Config).

maybe_add_param(ParamName, Map, Config) ->
    case maps:find(ParamName, Map) of
        {ok, _Value} ->
            Map;
        error ->
            case proplists:get_value(ParamName, Config) of
                undefined ->
                    Map;
                Value ->
                    maps:put(ParamName, Value, Map)
            end
    end.

special_channel_opts(Opts) ->
    Opts1 = maps:without([initiator_opts, responder_opts], Opts),
    {maps:merge(Opts1, maps:get(initiator_opts, Opts, #{})),
     maps:merge(Opts1, maps:get(responder_opts, Opts, #{}))}.

maybe_include_meta(Opts, Config) ->
    case proplists:get_bool(include_meta, Config) of
        true ->
            Opts#{ meta => [meta_str()] };
        false ->
            Opts
    end.

meta_str() ->
    <<"meta 1">>.

validate_meta(Meta) ->
    ExpMeta = aesc_offchain_update:op_meta(meta_str()),
    ExpMetaForClient = aesc_offchain_update:for_client(ExpMeta),
    {ExpMetaForClient, _} = {Meta, ExpMetaForClient},
    ok.

include_meta(Config) ->
    proplists:get_bool(include_meta, Config).

current_height() ->
    case rpc(aec_chain, top_header, []) of
        undefined -> 1;
        Header -> aec_headers:height(Header) + 1
    end.

format_args(Xs) when is_list(Xs) -> lists:map(fun format_arg/1, Xs);
format_args(X)                   -> [format_arg(X)].

format_arg(B) when is_binary(B)  -> aeser_api_encoder:encode(account_pubkey, B);
format_arg(I) when is_integer(I) -> integer_to_list(I).

%% ============================================================
%% HTTP Requests
%% ============================================================

get_pending_transactions() ->
    Host = internal_address(),
    http_request(Host, get, "debug/transactions/pending", []).

post_spend_tx(RecipientId, Amount, Fee) ->
    {_, Sender} = aecore_suite_utils:sign_keys(?NODE),
    SenderId = aeser_api_encoder:encode(account_pubkey, Sender),
    post_spend_tx(SenderId, RecipientId, Amount, Fee, <<"foo">>).

post_spend_tx(SenderId, RecipientId, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/spend",
                 #{sender_id => SenderId,
                   recipient_id => RecipientId,
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

get_pubkey() ->
    {_, Pubkey} = aecore_suite_utils:sign_keys(?NODE),
    aeser_api_encoder:encode(account_pubkey, Pubkey).

%% ============================================================
%% private functions
%% ============================================================
channel_ws_host_and_port() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"websocket">>, <<"channel">>, <<"port">>],
                aehttp, [channel, websocket, port], 8045]),
    {"localhost", Port}.

channel_ws_start(Role, Opts, Config) ->
    channel_ws_start(Role, Opts, Config, []).

-spec channel_ws_start(initiator | responder, map(), list(), list(atom())) ->
    {ok, pid(), binary()} | {error, term()}.
channel_ws_start(Role, Opts, Config, Events) ->
    LogFile = docs_log_file(Config),
    channel_ws_start(Role, Opts, Config, Events, LogFile).

-spec channel_ws_start(initiator | responder, map(), default_dir | list(), list(atom()),
                       string()) ->
    {ok, pid(), binary()} | {error, term()}.
channel_ws_start(Role, Opts, Config, Events, default_dir) ->
    LogFile = docs_log_file(Config),
    channel_ws_start(Role, Opts, Config, Events, LogFile);
channel_ws_start(Role, Opts, Config, Events, LogFile) when is_list(LogFile) ->
    EventsSet = sets:from_list(Events),
    ReqEventsSet = sets:from_list([info, closed, error]),
    RegisterEvents = sets:to_list(sets:union(EventsSet, ReqEventsSet)),
    UnregisterEvents = sets:to_list(sets:subtract(ReqEventsSet, EventsSet)),

    Opts1 = Opts#{ {int,logfile} => LogFile },
    Opts2 = maybe_add_fsm_id(Role, Opts1),
    {Host, Port} = channel_ws_host_and_port(),
    case ?WS:start_channel(Host, Port, Role, RegisterEvents, Opts2) of
        {ok, Pid} ->
                case wait_for_next_channel_event(Pid, Config) of
                    {ok, info, #{<<"event">> := <<"fsm_up">>, <<"fsm_id">> := FsmId}} ->
                        ok = ?WS:unregister_test_for_channel_events(Pid, UnregisterEvents),
                        {ok, Pid, FsmId};
                    {ok, error, #{<<"reason">> := Error}} ->
                        try ok = ?WS:wait_for_event(Pid, websocket, closed)
                        catch error:{connection_died, _Reason} -> ok
                        end,
                        {error, Error}
                end;
        Rest ->
            Rest
    end.

maybe_add_fsm_id(Role, #{existing_fsm_id := Props} = Opts) when is_list(Props) ->
    Opts#{existing_fsm_id => proplists:get_value(Role, Props)};
maybe_add_fsm_id(_, Opts) ->
    Opts.

docs_log_file(Config) ->
    %% TCLogBase = atom_to_list(?config(tc_name, Config)),
    TCLogBase = log_basename(Config),
    {aehttp_sc_SUITE, TestName} = proplists:get_value(current, ct:get_status()),
    MsgLogFile = filename:join([?config(priv_dir, Config), TCLogBase,
                               atom_to_list(TestName)++ ".md"]),
    ct:log("MsgLogFile = ~p", [MsgLogFile]),
    MsgLogFile.

log_basename(Config) ->
    Protocol = binary_to_list(?config(sc_ws_protocol, Config)),
    GOpts = ?config(tc_group_properties, Config),
    GName = ?config(name, GOpts),
    SubDir =
        case GName of %% an assertive and explicit check. If a new group is added - this will crash
            with_open_channel ->
                filename:join(Protocol, "continuous");
            only_one_signs ->
                filename:join(Protocol, "only_one_signs");
            both_sign ->
                filename:join(Protocol, "both_sign");
            conflicts ->
                ParentGroup = ?config(who_signs_update, Config),
                ParentDir =
                    case ParentGroup of
                        one   -> "only_one_signs";
                        both  -> "both_sign"
                    end,
                filename:join([Protocol, ParentDir, "conflicts"]);
            with_meta ->
                filename:join(Protocol, "with_meta");
            client_reconnect ->
                filename:join(Protocol, "reconnect");
            sc_contracts ->
                filename:join(Protocol, "contracts");
            ga_initiator ->
                filename:join([Protocol, "generalized_accounts", "initiator"]);
            ga_responder ->
                filename:join([Protocol, "generalized_accounts", "responder"]);
            ga_both ->
                filename:join([Protocol, "generalized_accounts", "both"]);
            pinned_env ->
                filename:join([Protocol, "pinned_env"]);
            optional_fee ->
                filename:join([Protocol, "changeable_fee"]);
            optional_gas_price ->
                filename:join([Protocol, "changeable_gas_price"]);
            optional_fee_higher_than_gas_price ->
                filename:join([Protocol, "changeable_fee_higher_than_gas_price"]);
            optional_fee_lower_than_gas_price ->
                filename:join([Protocol, "changeable_fee_lower_than_gas_price"]);
            abort_updates ->
                filename:join([Protocol, "abort_updates"]);
            optional_nonce ->
                filename:join([Protocol, "changeable_nonce"]);
            force_progress ->
                filename:join([Protocol, "force_progress"]);
            reconnects ->
                filename:join([Protocol, "reconnects"]);
            assume_min_depth ->
                filename:join([Protocol, "assume_min_depth"]);
            plain -> Protocol
        end,
    filename:join("channel_docs", SubDir).

sign_and_post_tx(EncodedUnsignedTx) ->
    sign_and_post_tx(EncodedUnsignedTx, on_node).

sign_and_post_tx(EncodedUnsignedTx, PrivKey) ->
    %% Check that we get the correct hash
    {ok, 200, #{<<"tx_hash">> := TxHash}} = sign_and_post_tx_(EncodedUnsignedTx, PrivKey),
    %% Check tx is in mempool.
    Fun = fun() ->
                  tx_in_mempool(TxHash)
          end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    TxHash.

sign_and_post_tx_(EncodedUnsignedTx, PrivKey) ->
    {ok, SerializedUnsignedTx} = aeser_api_encoder:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} =
        case PrivKey =:= on_node of
            true  -> aecore_suite_utils:sign_on_node(?NODE, UnsignedTx);
            false -> {ok, aec_test_utils:sign_tx(UnsignedTx, PrivKey)}
        end,
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    post_transactions_sut(aeser_api_encoder:encode(transaction, SerializedTx)).

tx_in_mempool(TxHash) ->
    case get_transactions_by_hash_sut(TxHash) of
        {ok, 200, #{<<"block_hash">> := <<"none">>}} -> true;
        {ok, 200, #{<<"block_hash">> := Other}} ->
            ct:log("Tx not in mempool, but in chain: ~p", [Other]),
            false;
        {ok, 404, _} -> false
    end.

ws_get_decoded_result(ConnPid1, ConnPid2, Contract, Function, Updates, UnsignedTx, Config) ->
    case proplists:get_bool(include_meta, Config) of
        true ->
            [Update, Meta] = Updates,
            ok = validate_meta(Meta),
            ws_get_decoded_result_(ConnPid1, ConnPid2, Contract, Function,
                                   [Update], UnsignedTx, Config);
        false ->
            [_] = Updates,
            ws_get_decoded_result_(ConnPid1, ConnPid2, Contract, Function,
                                   Updates, UnsignedTx, Config)
    end.

ws_get_decoded_result_(ConnPid1, ConnPid2, Contract, Function, [Update], UnsignedTx, Config) ->
    %% helper lambda for decoded result
    GetCallResult =
        fun(ConnPid) ->
            GetParams = ws_get_call_params(Update, UnsignedTx),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                           GetParams#{round => <<"2">>}, Config),
            {ok, _, #{<<"reason">> := <<"not_a_number">>}} =
                wait_for_channel_event(ConnPid, error, Config),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                GetParams#{caller_id => <<"ABCEDFG">>}, Config),
            {ok, _, #{<<"reason">> := <<"broken_encoding: accounts">>}} =
                wait_for_channel_event(ConnPid, error, Config),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                GetParams#{contract_id => <<"ABCDEFG">>}, Config),
            {ok, _, #{<<"reason">> := <<"broken_encoding: contracts">>}} =
                wait_for_channel_event(ConnPid, error, Config),
            ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                           GetParams, Config),
            {ok, _, <<"contract_call">>, Res} = wait_for_channel_event(ConnPid, get, Config),
            Res
        end,
    CallRes = GetCallResult(ConnPid1),
    CallRes = GetCallResult(ConnPid2),
    #{<<"caller_id">>         := _CallerId,
      <<"caller_nonce">>      := CallRound,
      <<"contract_id">>       := _ContractId,
      <<"gas_price">>         := _,
      <<"gas_used">>          := _,
      <<"height">>            := CallRound,
      <<"return_type">>       := <<"ok">>,
      <<"return_value">>      := ReturnValue} = CallRes,
    decode_call_result(Contract, Function, ok, ReturnValue).

decode_call_result(ContractName, Fun, ResType, ResValue) ->
    {ok, BinCode} = aect_test_utils:read_contract(?SOPHIA_IRIS_FATE, ContractName),
    aect_test_utils:decode_call_result(binary_to_list(BinCode), Fun, ResType, ResValue).


ws_get_call_params(Update, UnsignedTx) ->
    {CB1, Tx1} = aetx:specialize_callback(UnsignedTx),
    CallRound = CB1:round(Tx1),
    CallerPubKey = extract_caller(Update),
    ContractPubKey = extract_contract_pubkey(Update),
    CallerId = aeser_api_encoder:encode(account_pubkey, CallerPubKey),
    ContractId = aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
    #{contract_id => ContractId,
      caller_id   => CallerId,
      round       => CallRound}.

%% wait_for_channel_msg(ConnPid, Action, Config) ->
%%     wait_for_channel_msg_(ConnPid, Action, sc_ws_protocol(Config)).

%% wait_for_channel_msg_(ConnPid, Action, <<"json-rpc">>) ->
%%     wait_for_channel_event_(ConnPid, Action, <<"json-rpc">>).

wait_for_next_channel_event(ConnPid, Config) ->
    case wait_for_next_channel_event_(ConnPid, sc_ws_protocol(Config)) of
        {ok, Action, #{error := Error}} ->
            {ok, Action, Error};
        {ok, Action, #{data := Data}} ->
            {ok, Action, Data};
        {ok, Action, Tag, #{data := Data}} ->
            {ok, Action, Tag, Data}
    end.

wait_for_channel_event(ConnPid, Action, Config) ->
    wait_for_channel_event_match(ConnPid, Action, #{}, Config).

wait_for_channel_event_match(ConnPid, Action, Pat, Config) ->
    case wait_for_channel_event_(ConnPid, Action, sc_ws_protocol(Config)) of
        {ok, #{channel_id := ChId, error := Error}} ->
            {ok, ChId, Error};
        {ok, #{channel_id := ChId, data := Data}} ->
            match_msg(Pat, Data),
            {ok, ChId, Data};
        {ok, Tag, #{channel_id := ChId, data := Data}} ->
            match_msg(Pat, Data),
            {ok, ChId, Tag, Data}
    end.

match_msg(Pat, Msg) ->
    maps:fold(
      fun(Key, Val, ok) when is_map(Val) ->
              match_msg(Val, maps:get(Key, Msg));
         (Key, Val, ok) ->
              Val = maps:get(Key, Msg),
              ok
      end, ok, Pat).

wait_for_channel_event_full(ConnPid, Action, Config) ->
    wait_for_channel_event_(ConnPid, Action, sc_ws_protocol(Config)).

wait_for_channel_event_(ConnPid, error, <<"json-rpc">>) ->
    parse_channel_event(error, <<"json-rpc">>, ?WS:wait_for_channel_msg(ConnPid, error));
wait_for_channel_event_(ConnPid, Action, <<"json-rpc">>) ->
    parse_channel_event(Action, <<"json-rpc">>, ?WS:wait_for_channel_msg(ConnPid, Action)).

wait_for_next_channel_event_(ConnPid, <<"json-rpc">>) ->
    {Action, Res} =
        case ?WS:wait_for_any_channel_msg(ConnPid) of   % whole msg
            {ok, Action1, Data1} ->
                {Action1, parse_channel_event(Action1, <<"json-rpc">>, {ok, Data1})};
            {ok, Action1, Tag1, Data1} ->
                {Action1, parse_channel_event(Action1, <<"json-rpc">>, {ok, Tag1, Data1})}
        end,
    case Res of
        {ok, Data} ->
            {ok, Action, Data};
        {ok, Tag, Data} ->
            {ok, Action, Tag, Data}
    end.

parse_channel_event(error, <<"json-rpc">>, {ok, #{ <<"jsonrpc">> := <<"2.0">>
                                                 , <<"channel_id">> := ChId
                                                 , <<"id">>      := null
                                                 , <<"error">>   := E } }) ->
    {ok, #{ channel_id => ChId, error => lift_reason(E)}};
parse_channel_event(Action, <<"json-rpc">>, Msg) ->
    Method = method_pfx(Action),
    Sz = byte_size(Method),
    case {Msg, Method} of   % whole msg
        {{ok, #{ <<"jsonrpc">> := <<"2.0">>
               , <<"method">>  := <<Method:Sz/binary, _/binary>>
               , <<"params">>  := #{<<"channel_id">> := ChId} = Params }}, _} ->
            Data = maps:get(<<"data">>, Params, no_data),
            {ok, #{channel_id => ChId, data => Data}};
        {{ok, Tag, #{ <<"jsonrpc">> := <<"2.0">>
                    , <<"method">>  := <<Method:Sz/binary, _/binary>>
                    , <<"params">>  := #{<<"channel_id">> := ChId} = Params }}, _} ->
            Data = maps:get(<<"data">>, Params, no_data),
            {ok, Tag, #{channel_id => ChId, data => Data}}
    end.

wait_for_channel_event(Event, ConnPid, Type, Config) ->
    wait_for_channel_event_(Event, ConnPid, Type, sc_ws_protocol(Config)).

wait_for_channel_event_(Event, ConnPid, Action, <<"json-rpc">>) ->
    {ok, #{ <<"data">> := #{ <<"event">> := Event } }} =
        wait_for_json_rpc_action_event(ConnPid, Action, Event),
    ok.

match_info(Info, Match) ->
    maps:fold(fun(K,V,Acc) ->
                      case maps:find(K, Info) of
                          {ok, V} ->
                              Acc;
                          {ok, V1} when is_map(V), is_map(V1) ->
                              match_info(V, V1);
                          {ok, Other} ->
                              error({info_mismatch, {K, [V, Other]}});
                          error ->
                              error({no_such_key, K})
                      end
              end, ok, Match).


wait_for_channel_leave_msg(ConnPid, Config) ->
    wait_for_channel_leave_msg_(ConnPid, sc_ws_protocol(Config)).

wait_for_channel_leave_msg_(ConnPid, <<"json-rpc">>) ->
    {ok, #{ <<"channel_id">> := ChId,
            <<"data">> := #{ <<"state">> := St } }} =
        wait_for_json_rpc_action(ConnPid, leave),
    {ok, #{id => ChId, state => St}}.

wait_for_json_rpc_action_event(ConnPid, Action, Event) ->
    wait_for_json_rpc_action_(Action,
                              fun() ->
                                  ?WS:wait_for_channel_msg_event(ConnPid,
                                                                 Action,
                                                                 Event)
                              end).

wait_for_json_rpc_action(ConnPid, Action) ->
    wait_for_json_rpc_action_(Action,
                              fun() ->
                                  ?WS:wait_for_channel_msg(ConnPid, Action)
                              end).

wait_for_json_rpc_action_(Action, WaitFun) ->
    Method0 = method_pfx(Action),
    Sz = byte_size(Method0),
    {ok, #{ <<"jsonrpc">> := <<"2.0">>
          , <<"method">>  := <<Method0:Sz/binary, _/binary>>
          , <<"params">>  := #{<<"channel_id">> := _} = Params }} = WaitFun(),
    {ok, Params}.

lift_reason(#{ <<"message">> := <<"Rejected">>
             , <<"data">>    := Data } = E) ->
    Codes = lists:sort([Code || #{<<"code">> := Code} <- Data]),
    E#{ <<"reason">> => data_code_to_reason(Codes) };
lift_reason(#{ <<"code">> := Code } = E) ->
    E#{ <<"reason">> => data_code_to_reason([Code]) };
lift_reason(#{ <<"reason">> := _ } = E) ->
    E.

data_code_to_reason([100 ])      -> <<"not_found">>;
data_code_to_reason([107 ])      -> <<"conflict">>;
data_code_to_reason([1001])      -> <<"insufficient_balance">>;
data_code_to_reason([1002])      -> <<"negative_amount">>;
data_code_to_reason([1003])      -> <<"invalid_pubkeys">>;
data_code_to_reason([1004])      -> <<"call_not_found">>;
data_code_to_reason([1005])      -> <<"broken_encoding: accounts">>;
data_code_to_reason([1006])      -> <<"broken_encoding: contracts">>;
data_code_to_reason([1007])      -> <<"contract_init_failed">>;
data_code_to_reason([1008])      -> <<"not_a_number">>;
data_code_to_reason([1005,1006]) -> <<"broken_encoding: accounts, contracts">>;
data_code_to_reason([1009])      -> <<"broken_encoding: bytearray">>;
data_code_to_reason([1010])      -> <<"broken_encoding: transaction">>;
data_code_to_reason([Code])      -> sc_ws_api_jsonrpc:error_data_msg(Code).

method_pfx(Action) ->
    <<"channels.", (bin(Action))/binary>>.

ws_send_tagged(ConnPid, Method, Payload, Config) ->
    ws_send_tagged_(ConnPid, Method, Payload, sc_ws_protocol(Config)).

ws_call_async_method(ConnPid, Method, Payload, Config) ->
    ws_call_async_method(ConnPid, Method, Payload, sc_ws_protocol(Config), Config).

ws_call_async_method(ConnPid, Method, Payload, <<"json-rpc">>, _Config) ->
    <<"ok">> = ?WS:json_rpc_call(
                  ConnPid, #{ <<"method">> => Method
                            , <<"params">> => Payload }),
    ok.


ws_send_tagged_(ConnPid, Method, Payload, <<"json-rpc">>) ->
    ?WS:json_rpc_notify(
       ConnPid,
       #{ <<"method">> => Method
        , <<"params">> => Payload }).

bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
bin(B) when is_binary(B) ->
    B.

sc_ws_protocol(Config) ->
    {_, Protocol} = lists:keyfind(sc_ws_protocol, 1, Config),
    Protocol.

sc_ws_wrong_call_data(Config) ->
    [sc_ws_contract_generic_(Role, offchain, fun sc_ws_broken_init_code_/9, Config, [])
        || Role <- [initiator, responder]],
    [sc_ws_contract_generic_(Role, offchain, fun sc_ws_broken_call_code_/9, Config, [])
        || Role <- [initiator, responder]],
    ok.

sc_ws_wrong_abi(Config) ->
    [sc_ws_contract_generic_(Role, offchain, fun sc_ws_wrong_abi_/9, Config, [])
        || Role <- [initiator, responder]],
    ok.

sc_ws_broken_init_code_(Owner, GetVolley, _CreateContract, _ConnPid1, _ConnPid2,
                   _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% Example broken init code will be calling not the init function
    {ok, EncodedCode} = get_contract_bytecode(identity),
    %% call main instead of init
    {ok, EncodedInitData} = encode_call_data(identity, "main_", ["1"]),
    {_CreateVolley, OwnerConnPid, _OwnerPubKey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                   #{vm_version  => aect_test_utils:vm_version(),
                     abi_version => aect_test_utils:abi_version(),
                     deposit     => 10,
                     code        => EncodedCode,
                     call_data   => EncodedInitData}, Config),

    {ok, _, ErrPayload} = wait_for_channel_event(OwnerConnPid, error, Config),
    #{<<"reason">> := <<"contract_init_failed">>} = lift_reason(ErrPayload),
    ok.

sc_ws_broken_call_code_(Owner, GetVolley, _CreateContract, _ConnPid1, _ConnPid2,
                   _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% Example broken call code data will be calling a function from another
    %% contract
    {ok, EncodedCode} = get_contract_bytecode(identity),
    {ok, EncodedInitData} = encode_call_data(identity, "init", []),

    {SignVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                   #{vm_version  => aect_test_utils:vm_version(),
                     abi_version => aect_test_utils:abi_version(),
                     deposit     => 10,
                     code        => EncodedCode,
                     call_data   => EncodedInitData}, Config),
    #{tx := UnsignedCreateTx, updates := _Updates} = SignVolley(),
    % contract is succesfully created
    ContractPubKey = contract_id_from_create_update(OwnerPubKey,
                                                    UnsignedCreateTx),

    % have some other contract with some other function
    {ok, EncodedCalcCallData} = encode_call_data(safe_math, "add", ["1", "2"]),
    % call the existing contract with the other contract's call data
    ws_send_tagged(OwnerConnPid, <<"channels.update.call_contract">>,
                   #{contract_id => aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
                     abi_version => aect_test_utils:abi_version(),
                     amount      => 1,
                     call_data   => EncodedCalcCallData}, Config),
    % contract call succeeds executing
    #{tx := UnsignedCallTx, updates := [Update]} = SignVolley(),
    ws_send_tagged(OwnerConnPid, <<"channels.get.contract_call">>,
                   ws_get_call_params(Update, UnsignedCallTx), Config),
    % contract call is present in FSM
    {ok, _, <<"contract_call">>, Res} = wait_for_channel_event(OwnerConnPid, get, Config),
    % call result is still an error
    #{<<"return_type">> := <<"error">>} = Res,
    ok.

sc_ws_wrong_abi_(Owner, GetVolley, _CreateContract, _ConnPid1, _ConnPid2,
                   _OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    {ok, EncodedCode} = get_contract_bytecode(safe_math),
    {ok, EncodedInitData} = encode_call_data(safe_math, "init", []),

    {SignVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                   #{vm_version  => aect_test_utils:vm_version(),
                     abi_version => aect_test_utils:abi_version(),
                     deposit     => 10,
                     code        => EncodedCode,
                     call_data   => EncodedInitData}, Config),
    #{tx := UnsignedCreateTx, updates := _Updates} = SignVolley(),
    % contract is succesfully created
    ContractPubKey = contract_id_from_create_update(OwnerPubKey,
                                                    UnsignedCreateTx),
    {ok, EncodedCalcCallData} = encode_call_data(safe_math, "add", ["1", "2"]),
    % call the existing contract with wrong ABI
    ws_send_tagged(OwnerConnPid, <<"channels.update.call_contract">>,
                   #{contract_id => aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
                     abi_version => aect_test_utils:abi_version() + 1, %% wrong ABI
                     amount      => 1,
                     call_data   => EncodedCalcCallData}, Config),
    {ok, _, #{<<"reason">> := <<"ABI version mismatch">>}} =
        wait_for_channel_event(OwnerConnPid, error, Config),
    ok.

extract_caller(#{<<"op">> := <<"OffChainNewContract">>,
                 <<"owner">> := EncOwnerId}) ->
    {ok, Owner} = aeser_api_encoder:safe_decode(account_pubkey, EncOwnerId),
    Owner;
extract_caller(#{<<"op">> := <<"OffChainCallContract">>,
                 <<"caller_id">> := EncCallerId}) ->
    {ok, Caller} = aeser_api_encoder:safe_decode(account_pubkey, EncCallerId),
    Caller.

extract_contract_pubkey(#{<<"op">> := <<"OffChainCallContract">>,
                          <<"contract_id">> := EncContractId}) ->
    {ok, ContractPubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncContractId),
    ContractPubKey.

with_trace(F, Config, File) ->
    with_trace(F, Config, File, on_error).

with_trace(F, Config, File, When) ->
    ct:log("with_trace ...", []),
    TTBRes = aesc_ttb:on_nodes([node()|get_nodes()], File),
    ct:log("Trace set up: ~p", [TTBRes]),
    try F(Config)
    catch E:R:Stack ->
        case E of
            error ->
                ct:pal("Error ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                erlang:error(R);
            exit ->
                ct:pal("Exit ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                exit(R);
            throw ->
                ct:pal("Caught throw:~p", [R]),
                throw(R)
        end
    end,
    case When of
        on_error ->
            ct:log("Discarding trace", []),
            aesc_ttb:stop_nofetch();
        always ->
            ttb_stop()
    end,
    ok.

ttb_stop() ->
    Dir = aesc_ttb:stop(),
    Out = filename:join(filename:dirname(Dir),
                        filename:basename(Dir) ++ ".txt"),
    case aesc_ttb:format(Dir, Out, #{limit => 30000}) of
        {error, Reason} ->
            ct:pal("TTB formatting error: ~p", [Reason]);
        _ ->
            ok
    end,
    ct:pal("Formatted trace log in ~s~n", [Out]).

get_nodes() ->
    [aecore_suite_utils:node_name(?NODE)].

attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args) ->
   attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args, #{}).


attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args, Opts) ->
   case aega_test_utils:get_contract(Contract) of
       {ok, #{src := Src, bytecode := C, map := #{type_info := TI}}} ->
           attach_({Owner, OwnerPrivkey}, Src, C, TI, AuthFun, Args, Opts);
       _ ->
           error(bad_contract)
   end.

attach_({Owner, OwnerPrivkey}, Src, ByteCode, TypeInfo, AuthFun, Args, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    Calldata = aega_test_utils:make_calldata(Src, "init", Args),
    FunName = list_to_binary(AuthFun),
    {ok, AuthFunHash} =
        case aect_test_utils:backend() of
            aevm -> aeb_aevm_abi:type_hash_from_function_name(FunName, TypeInfo);
            fate ->{ok, <<(aeb_fate_code:symbol_identifier(FunName)):4/binary, 0:(28*8)>>}
        end,
    Options1 = maps:merge(#{nonce => Nonce, code => ByteCode,
                            auth_fun => AuthFunHash, call_data => Calldata},
                            Opts),
    AttachTx = aega_test_utils:ga_attach_tx(Owner, Options1),

    SignedTx = aec_test_utils:sign_tx(AttachTx, [OwnerPrivkey]),
    ok = rpc(aec_tx_pool, push, [SignedTx]),
    wait_for_signed_transaction_in_block(SignedTx),
    ok.

slogan(F, L) ->
    S = iolist_to_binary([?MODULE_STRING, ":", atom_to_binary(F, latin1),
                          "/", integer_to_binary(L)]),
    ct:log("slogan: ~p", [S]),
    S.

slogan(F, L, X) ->
    S = iolist_to_binary([?MODULE_STRING, ":", atom_to_binary(F, latin1),
                          "/", integer_to_binary(L), "[", to_binary(X), "]"]),
    ct:log("slogan: ~p", [S]),
    S.

to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
to_binary([_|_] = L) ->
    iolist_to_binary([to_binary(X) || X <- L]).

simple_auth_meta(Owner, Secret, GANonce, InnerMostTx, InnerTx) ->
    AuthTx =
        case aetx:specialize_type(InnerMostTx) of
            {channel_offchain_tx, _} -> InnerMostTx;
            {_, _} -> InnerTx
        end,
    AuthData = simple_auth(Secret, GANonce, AuthTx),
    meta(Owner, AuthData, InnerTx).

simple_auth(Secret, Nonce, _Tx) ->
    aega_test_utils:make_calldata("simple_auth", "authorize", [Secret, Nonce]).

meta(Owner, AuthData, InnerTx) ->
    aecore_suite_utils:meta_tx(Owner, #{}, AuthData, InnerTx).

account_type(Pubkey) ->
    {value, Account} = rpc(aec_chain, get_account, [Pubkey]),
    aec_accounts:type(Account).

sc_ws_test_broken_params(Role, Config, Opts, Error, Config) ->
    {error, Error} = channel_ws_start(Role,
                                      maps:put(host, <<"localhost">>,
                                               Opts), [{slogan, ?SLOGAN}|Config]).

sc_ws_broken_open_params(Config) ->
    #{ initiator := #{pub_key := IPubkey}
     , responder := #{pub_key := RPubkey} } = proplists:get_value(participants, Config),

    IAmt = 8,
    RAmt = 4,

    BogusPubkey = <<42:32/unit:8>>,

    Roles = [initiator, responder],
    Test =
        fun(Opts, Error) ->
            [sc_ws_test_broken_params(Role, Config, Opts, Error, Config) || Role <- Roles]
        end,

    TestData = [ { "test initiator pubkey missing"
                 , BogusPubkey, RPubkey, IAmt, RAmt, #{}, Config, <<"Participant not found">> }
               , { "test responder pubkey missing"
                 , IPubkey, BogusPubkey, IAmt, RAmt, #{}, Config, <<"Participant not found">> }
               , { "test initiator having a negative amount"
                 , IPubkey, RPubkey, -1, RAmt, #{}, Config, <<"Value too low">> }
               , { "test initiator having a negative amount"
                 , IPubkey, RPubkey, IAmt, -1, #{}, Config, <<"Value too low">> }
               , { "test both having a negative amount"
                 , IPubkey, RPubkey, -1, -1, #{}, Config, <<"Value too low">> }
               , { "test channel_reserve having a negative amount"
                 , IPubkey, RPubkey, IAmt, RAmt, #{channel_reserve => -1}, Config, <<"Value too low">> }
               , { "test push_amount having a negative amount"
                 , IPubkey, RPubkey, IAmt, RAmt, #{push_amount => -1}, Config, <<"Value too low">> }
               , { "test lock_period having a negative value"
                 , IPubkey, RPubkey, IAmt, RAmt, #{lock_period => -1}, Config, <<"Value too low">> }
               ],

    lists:map(
      fun({Name, IKey1, RKey1, IAmt1, RAmt1, Opts1, Config1, Error}) ->
              ct:log(Name, []),
              ChannelOpts = channel_options(IKey1, RKey1, IAmt1, RAmt1, Opts1, Config1),
              Test(ChannelOpts, Error)
      end, TestData),

    ok.

sc_ws_pinned_update(Cfg) ->
    NOT = 10,
    NNT = 1,
    Pick = 1,
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                       NOT + 1),
    Config = sc_ws_open_(Cfg, #{ bh_delta_not_older_than => NOT
                               , bh_delta_not_newer_than => NNT
                               , bh_delta_pick => Pick}),
    Participants = proplists:get_value(participants, Config),
    Conns = proplists:get_value(channel_clients, Config),
    lists:foldl(
        fun(Sender, Round) ->
            HashNOT = aecore_suite_utils:get_key_hash_by_delta(dev1, NOT),
            Hash1 = aeser_api_encoder:encode(key_block_hash, HashNOT),
            channel_update(Conns, Sender, Participants, 1, Round,
                           false, Cfg, #{block_hash => Hash1}),
            Round + 1
        end,
        2, % we start from round 2
        [initiator,
         responder]),
    ok = sc_ws_close_(Config).

sc_ws_pinned_deposit(Cfg) ->
    NOT = 10,
    NNT = 1,
    Pick = 1,
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                       NOT + 1),
    Config = sc_ws_open_(Cfg, #{ bh_delta_not_older_than => NOT
                               , bh_delta_not_newer_than => NNT
                               , bh_delta_pick => Pick}),
    lists:foldl(
        fun(Depositor, Round) ->
            HashNOT = aecore_suite_utils:get_key_hash_by_delta(dev1, NOT),
            Hash1 = aeser_api_encoder:encode(key_block_hash, HashNOT),
            sc_ws_deposit_(Config, Depositor,
                           #{block_hash => Hash1}),
            Round + 1
        end,
        2, % we start from round 2
        [initiator,
         responder]),
    ok = sc_ws_close_(Config).

sc_ws_pinned_withdraw(Cfg) ->
    NOT = 10,
    NNT = 1,
    Pick = 1,
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                       NOT + 1),
    Config = sc_ws_open_(Cfg, #{ bh_delta_not_older_than => NOT
                               , bh_delta_not_newer_than => NNT
                               , bh_delta_pick => Pick}),
    lists:foldl(
        fun(Depositor, Round) ->
            HashNOT = aecore_suite_utils:get_key_hash_by_delta(dev1, NOT),
            Hash1 = aeser_api_encoder:encode(key_block_hash, HashNOT),
            sc_ws_withdraw_(Config, Depositor,
                            #{block_hash => Hash1}),
            Round + 1
        end,
        2, % we start from round 2
        [initiator,
         responder]),
    ok = sc_ws_close_(Config).

sc_ws_pinned_error_update(Cfg) ->
    sc_ws_pinned_error_(
        <<"channels.update.new">>,
        fun(SenderPubkey, AckPubkey) ->                
            #{from => aeser_api_encoder:encode(account_pubkey, SenderPubkey),
              to => aeser_api_encoder:encode(account_pubkey, AckPubkey),
              amount => 1}
        end,
        <<"channels.update">>, Cfg).

sc_ws_pinned_error_deposit(Cfg) ->
    sc_ws_pinned_error_(
        <<"channels.deposit">>,
        fun(_SenderPubkey, _AckPubkey) ->                
            #{amount => 1}
        end,
        <<"channels.deposit_tx">>, Cfg).

sc_ws_pinned_error_withdraw(Cfg) ->
    sc_ws_pinned_error_(
        <<"channels.withdraw">>,
        fun(_SenderPubkey, _AckPubkey) ->                
            #{amount => 1}
        end,
        <<"channels.withdraw_tx">>, Cfg).

sc_ws_pinned_error_(Tag, XOptsFun, ResponseTag, Cfg) ->
    NOT = 10,
    NNT = 2,
    Pick = 1,
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                       NOT + 1),
    Config = sc_ws_open_(Cfg, #{ bh_delta_not_older_than => NOT
                               , bh_delta_not_newer_than => NNT
                               , bh_delta_pick => Pick}),
    Participants = proplists:get_value(participants, Config),
    #{initiator := IConnPid,
      responder := RConnPid} = Conns = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, conflict]),
    lists:foldl(
        fun(SenderRole, Round) ->
            {SenderRole, AckRole} =
                case SenderRole of
                    initiator -> {initiator, responder};
                    responder -> {responder, initiator}
                end,
            #{pub_key := SenderPubkey,
              priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
            #{pub_key := AckPubkey} = maps:get(AckRole, Participants),
            Test =
                fun(Delta) ->
                    HashBin = aecore_suite_utils:get_key_hash_by_delta(dev1, Delta),
                    Hash = aeser_api_encoder:encode(key_block_hash, HashBin),
                    SenderConnPid = maps:get(SenderRole, Conns),
                    AckConnPid = maps:get(AckRole, Conns),
                    XOpts = XOptsFun(SenderPubkey, AckPubkey),
                    ws_send_tagged(SenderConnPid, Tag,
                                   XOpts#{block_hash => Hash}, Cfg),
                    channel_sign_tx(SenderPubkey, SenderConnPid, SenderPrivkey,
                                    ResponseTag, Cfg),
                    {ok, _, _} = wait_for_channel_event(SenderConnPid, conflict, Cfg),
                    {ok, _, _} = wait_for_channel_event(AckConnPid, conflict, Cfg)
                end,
            Test(NNT - 1), %% too nww
            Test(NOT + 1), %% too old
            Round %% not bumped
        end,
        2, % we start from round 2
        [initiator,
         responder]),
    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, conflict]),

    ok = sc_ws_close_(Config).

sc_ws_pinned_contract(Cfg) ->
    NOT = 10,
    NNT = 2,
    Pick = 1,
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                       NOT + 1),
    Config = sc_ws_open_(Cfg, #{ bh_delta_not_older_than => NOT
                               , bh_delta_not_newer_than => NNT
                               , bh_delta_pick => Pick}),
    #{initiator := #{pub_key  := IPubkey,
                     priv_key := IPrivkey},
      responder := #{pub_key  := RPubkey,
                     priv_key := RPrivkey}} =
        proplists:get_value(participants, Config),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, get, error]),
    GetVolley =
        fun(Actor) ->
            case Actor =:= initiator of
                true ->
                    {fun() -> update_volley_(IPubkey, IConnPid, IPrivkey,
                                             RPubkey, RConnPid, RPrivkey, Cfg) end,
                     IConnPid, IPubkey};
                false ->
                    {fun() -> update_volley_(RPubkey, RConnPid, RPrivkey,
                                             IPubkey, IConnPid, IPrivkey, Cfg) end,
                     RConnPid, RPubkey}
            end
        end,
    CreateContract =
        fun(Owner) ->
            EncodedCode = contract_byte_code("channel_env"),
            {ok, EncodedInitData} = encode_call_data(channel_env, "init", []),
            {CreateVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
            NewContractOpts0 =
                #{vm_version  => aect_test_utils:vm_version(),
                  abi_version => aect_test_utils:abi_version(),
                  deposit     => 1,
                  code        => EncodedCode,
                  call_data   => EncodedInitData},
            NewContractOpts = maybe_include_meta(NewContractOpts0, Cfg),
            % correct call
            ws_send_tagged(OwnerConnPid, <<"channels.update.new_contract">>,
                NewContractOpts, Cfg),

            #{tx := UnsignedStateTx, updates := _Updates} = CreateVolley(),
            contract_id_from_create_update(OwnerPubKey, UnsignedStateTx)
        end,

    PinnedContractCall =
        fun(Who, Delta) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            Fun = <<"block_height">>,
            ContractPubKey = CreateContract(Who),
            HashBin = aecore_suite_utils:get_key_hash_by_delta(dev1, Delta),
            Hash = aeser_api_encoder:encode(key_block_hash, HashBin),
            Args = [],
            R0 = dry_call_a_contract(Fun, Args, ContractPubKey,
                                     channel_env, UpdaterConnPid, 0, Cfg,
                                     #{block_hash => Hash}),
            #{tx := Tx, updates := Updates} =
                call_a_contract(Fun, Args, ContractPubKey, channel_env,
                                UpdaterConnPid, UpdateVolley, 0, Cfg,
                                #{block_hash => Hash}),
            R = ws_get_decoded_result(IConnPid, RConnPid, channel_env,
                                      Fun, Updates, Tx, Cfg),
            {R, R} = {R0, R}, %% dry run is the same
            {ok, 200, #{<<"height">> := TopHeight}} = get_key_blocks_current_sut(),
            ExpectedRes = TopHeight - Delta,
            {R, R} = {ExpectedRes, R}
        end,
    [PinnedContractCall(Who, Delta)
        || Who   <- [initiator, responder],
           Delta <- [NNT, NOT, NNT + 1]],
    ok = sc_ws_close_(Config).

check_info(L, Timeout) ->
    receive
        Msg ->
            ct:log("[~p] UNEXPECTED: ~p", [L, Msg]),
            [Msg|check_info(L, Timeout)]
    after Timeout ->
            []
    end.

peek_msgq(L) ->
    case process_info(self(), messages) of
        {_, []} ->
            ok;
        {_, Msgs} ->
            ct:log("[~p] PEEK = ~p", [L, Msgs])
    end.

wait_for_expected_events(Pid, Config, ExpectedEvents) ->
    {ok, _, #{<<"event">> := Event}} = wait_for_channel_event(Pid, info, Config),
    ct:log("Received ~p for ~p", [Event, Pid]),
    ExpectedEvents1 = ExpectedEvents -- [Event, {optional, Event}],
    Mandatory = [E || E <- ExpectedEvents1, is_binary(E)],
    case Mandatory of
        [] ->
            ok;
        _ ->
            wait_for_expected_events(Pid, Config, ExpectedEvents1)
    end.


signed_channel_tx_round(SignedTx) ->
    AeTx = aetx_sign:innermost_tx(SignedTx),
    {Mod, Tx} = aetx:specialize_callback(AeTx),
    Mod:round(Tx).

sc_ws_optional_params_create(Cfg0) ->
    {Opts, CheckFun} = ?config(fee_computation, Cfg0),
    Cfg1 = maps:to_list(maps:with([nonce, fee, gas_price], Opts)) ++ Cfg0,
    Cfg = sc_ws_open_(Cfg1),

    SignedCrTx = ?config(create_tx, Cfg),

    CheckFun(SignedCrTx),
    sc_ws_close_mutual_(Cfg, initiator),
    ok.


sc_ws_optional_params_fail_create(Cfg0) ->
    {ChannelOpts0, CheckFun} = ?config(fee_computation, Cfg0),
    Config = maps:to_list(maps:with([nonce, fee, gas_price], ChannelOpts0)) ++ Cfg0,
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),
    IAmt = maps:get(initiator_amount, ChannelOpts0,
                    70000 * aec_test_utils:min_gas_price()),
    RAmt = maps:get(responder_amount, ChannelOpts0,
                    40000 * aec_test_utils:min_gas_price()),

    TestEvents = [info, get, sign, on_chain_tx, update],
    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt, ChannelOpts0, Config),
    {IChanOpts, RChanOpts} = special_channel_opts(ChannelOpts),
    ct:log("IChanOpts = ~p~n"
           "RChanOpts = ~p~n", [IChanOpts, RChanOpts]),
    %% We need to register for some events as soon as possible - otherwise a race may occur where
    %% some fsm messages are missed
    {ok, IConnPid, _IFsmId} =
        channel_ws_start(initiator, maps:put(host, <<"localhost">>, IChanOpts),
                         Config, TestEvents),
    ct:log("initiator spawned", []),

    {ok, RConnPid, _RFsmId} = channel_ws_start(responder, RChanOpts, Config, TestEvents),
    ct:log("responder spawned", []),

    channel_send_conn_open_infos(RConnPid, IConnPid, Config),

    {ok, _, _Tag, #{<<"signed_tx">> := EncSignedTx,
                    <<"updates">>   := _Updates}} =
        wait_for_channel_event(IConnPid, sign, Config),
    SignedCrTx = aetx_sign:deserialize_from_binary(
                   ok(aeser_api_encoder:safe_decode(transaction, EncSignedTx))),
    CheckFun(SignedCrTx),
    ok.

sc_ws_optional_params_fail_deposit(Cfg) ->
    sc_ws_optional_params_fail(Cfg, <<"channels.deposit">>, #{amount => 2}).

sc_ws_optional_params_fail_withdrawal(Cfg) ->
    sc_ws_optional_params_fail(Cfg, <<"channels.withdraw">>, #{amount => 2}).

sc_ws_optional_params_fail_snapshot(Cfg) ->
    PreAction =
        fun(ConnPid, Cfg1) ->
            {ok, #{ signed_tx := StateSignedStateTx }} =
                sc_ws_get_state(ConnPid, Cfg1),
            LatestRound = signed_channel_tx_round(StateSignedStateTx),
            _NextRound = sc_ws_update_basic_round_(LatestRound + 1, Cfg1),
            ok
        end,
    PostAction =
        fun(Cfg1) ->
            sc_ws_close_mutual_(Cfg1, initiator)
        end,
    sc_ws_optional_params_fail(Cfg, <<"channels.snapshot_solo">>, #{},
                               <<"channels.snapshot_solo_sign">>,
                               PreAction, PostAction).

sc_ws_optional_params_fail_close_mutual(Cfg) ->
    sc_ws_optional_params_fail(Cfg, <<"channels.shutdown">>, #{}).

sc_ws_optional_params_fail_close_solo(Cfg) ->
    sc_ws_optional_params_fail(Cfg, <<"channels.close_solo">>, #{}).

sc_ws_optional_params_fail_slash(Cfg0) ->
    PreAction =
        %% prepare the channel for slashing
        fun(ConnPid, Cfg) ->
            #{initiator := #{pub_key := IPubkey, priv_key := IPrivkey},
              responder := #{pub_key := RPubkey, priv_key := _RPrivkey}}
                = proplists:get_value(participants, Cfg),
            {CloserPubkey, CloserPrivkey} = {IPubkey, IPrivkey},

            Round0 = 2,
            %% make an update so we are not at channel_create_tx
            Round1 = sc_ws_update_basic_round_(Round0, Cfg),
            %% make a close solo based on this update but don't post it yet
            {ok, #{ trees := Round2StateTrees
                  , signed_tx := SignedRound2StateTx }} = sc_ws_get_state(ConnPid, Cfg),
            {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [CloserPubkey]),
            CloseSoloRound2Args =
                maps:merge(
                    make_close_solo_args(SignedRound2StateTx, Round2StateTrees,
                                        [IPubkey, RPubkey]),
                    #{ from_id => aeser_id:create(account, CloserPubkey)
                    , nonce   => Nonce}),
            {ok, Round2CloseTx} = aesc_close_solo_tx:new(CloseSoloRound2Args),
            %% sign this tx
            SignedTx = aec_test_utils:sign_tx(Round2CloseTx, CloserPrivkey),
            SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
            EncRound2CloseTx = aeser_api_encoder:encode(transaction, SerializedTx),
            Round2CloseTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),

            %% make a new off-chain update
            _Round2 = sc_ws_update_basic_round_(Round1, Cfg),

            %% at this point the Round2CloseTx is not based on the latest state so it
            %% is malicious to post it on-chain

            %% post the malicious tx and wait it to be included
            %% Ensure both fsms get notified
            post_transactions_sut(EncRound2CloseTx),
            Conns = proplists:get_value(channel_clients, Cfg),
            wait_for_info_msgs(
              Conns, #{ <<"event">> => <<"closing">> },
              fun() ->
                      wait_for_onchain_tx_events(
                        Conns, #{ <<"info">> => <<"can_slash">>
                                , <<"type">> => <<"channel_offchain_tx">> },
                        fun() ->
                            ok = wait_for_tx_hash_on_chain(Round2CloseTxHash),
                            aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                ?DEFAULT_MIN_DEPTH)
                        end, Cfg)
              end, Cfg),
            ok
        end,
    PostAction =
        fun(Cfg1) ->
            CleanCfg = proplists:delete(fee_computation, Cfg1),
            #{initiator := IConnPid} = Conns
                = proplists:get_value(channel_clients, CleanCfg),
            {ok, SignedSlashTx} = request_and_sign_slash_tx(
                                    IConnPid, initiator, CleanCfg,
                                  fun() ->
                                          {ok, <<"ok">>} = request_slash(IConnPid, #{}),
                                          ok
                                  end),
            ct:log("SignedSlashTx = ~p", [SignedSlashTx]),
            SlashTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSlashTx)),
            ct:log("SlashTxHash = ~p", [SlashTxHash]),
            wait_for_onchain_tx_events(
              Conns, #{ <<"info">> => <<"solo_closing">>
                      , <<"type">> => <<"channel_slash_tx">> },
              fun() ->
                      ok = wait_for_tx_hash_on_chain(SlashTxHash)
              end, CleanCfg),
            aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                           ?DEFAULT_MIN_DEPTH),
            settle_(CleanCfg, initiator)
        end,
    sc_ws_optional_params_fail(Cfg0, <<"channels.slash">>, #{},
                               compute, PreAction, PostAction).

sc_ws_optional_params_fail_settle(Cfg0) ->
    PreAction =
        %% prepare the channel for slashing
        fun(_ConnPid, Cfg) ->
            Round0 = 2,
            %% make an update so we are not at channel_create_tx
            _Round1 = sc_ws_update_basic_round_(Round0, Cfg),
            {ok, _SignedTx} = sc_ws_close_solo_(Cfg, initiator, #{}),
            aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                           ?DEFAULT_MIN_DEPTH),

            ok
        end,
    PostAction =
        fun(_Cfg) ->
            ok
        end,
    sc_ws_optional_params_fail(Cfg0, <<"channels.settle">>, #{},
                               compute, PreAction, PostAction).

sc_ws_optional_params_fail_force_progress(Cfg0) ->
    ContractName = counter,
    Function = "tick",
    Arguments = [],
    PreAction =
        %% prepare the channel for slashing
        fun(_ConnPid, Cfg) ->
            %% create and call some contracts
            sc_ws_contract_(Cfg, ContractName, initiator),
            aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE),
                                           ?DEFAULT_MIN_DEPTH),

            ok
        end,
    PostAction =
        fun(_Cfg) ->
            ok
        end,
    MakeFPParams =
        fun(Config) ->
            [ContractPubkey] = contract_ids(Config),
            {ok, EncodedData} = encode_call_data(ContractName, Function, Arguments),
            GasPrice = aec_test_utils:min_gas_price() + rand:uniform(10000),
            #{ contract_id => aeser_api_encoder:encode(contract_pubkey, ContractPubkey)
            , abi_version => aect_test_utils:abi_version()
            , amount      => 10
            , call_data   => EncodedData
            , gas_price   => GasPrice}
        end,
    sc_ws_optional_params_fail(Cfg0, <<"channels.force_progress">>,
                               MakeFPParams,
                               compute, PreAction, PostAction).

sc_ws_optional_params_fail(Cfg0, Action, ActionParams) ->
    sc_ws_optional_params_fail(Cfg0, Action, ActionParams, compute).

sc_ws_optional_params_fail(Cfg0, Action, ActionParams, AbortMethod) ->
    PreAction =
        fun(_, _) ->
            ok
        end,
    PostAction =
        fun(Cfg1) ->
            sc_ws_close_mutual_(Cfg1, initiator)
        end,
    sc_ws_optional_params_fail(Cfg0, Action, ActionParams, AbortMethod,
                               PreAction, PostAction).

sc_ws_optional_params_fail(Cfg0, Action, ActionParams0, AbortMethod,
                           PreAction, PostAction) ->
    {Opts, CheckFun} = ?config(fee_computation, Cfg0),
    Test =
        fun(Who) ->
            Cfg1 = sc_ws_open_(Cfg0),
            Cfg = maps:to_list(maps:with([nonce, fee, gas_price], Opts)) ++ Cfg1,
            Clients = proplists:get_value(channel_clients, Cfg),
            SenderConnPid = maps:get(Who, Clients),
            PreAction(SenderConnPid, Cfg1),
            ActionParams =
                case is_function(ActionParams0) of
                    true -> ActionParams0(Cfg1);
                    false -> ActionParams0
                end,
            with_registered_events([sign], [SenderConnPid],
                fun() ->
                    ws_send_tagged(SenderConnPid, Action,
                                   maps:merge(Opts, ActionParams), Cfg),
                    {ok, _, Tag, #{<<"signed_tx">> := EncSignedTx,
                                   <<"updates">>   := _Updates}} = wait_for_channel_event(SenderConnPid, sign, Cfg),
                    SignedTx = aetx_sign:deserialize_from_binary(
                                  ok(aeser_api_encoder:safe_decode(transaction, EncSignedTx))),
                    %% check the expected transaction
                    CheckFun(SignedTx),

                    %% cancel the action
                    Method =
                        case AbortMethod of
                            compute -> <<"channels.", (bin(Tag))/binary>>;
                            M -> M
                        end,
                    ws_send_tagged(SenderConnPid, Method, #{<<"error">> => 42}, Cfg)
                end),
                PostAction(Cfg)
            end,

    Test(initiator),
    Test(responder),

    ok.

sc_ws_optional_params_deposit(Cfg0) ->
    MakeDeposit = fun sc_ws_deposit_/3,
    sc_ws_optional_params_(Cfg0, MakeDeposit).

sc_ws_optional_params_withdrawal(Cfg0) ->
    MakeDeposit =
        fun(Cfg, Who, Opts) ->
            sc_ws_withdraw_(Cfg, Who, Opts)
        end,
    sc_ws_optional_params_(Cfg0, MakeDeposit).

sc_ws_optional_params_snapshot(Cfg0) ->
    MakeDeposit =
        fun(Cfg, Who, Opts) ->
            #{ initiator := IConnPid } = Cs =
                proplists:get_value(channel_clients, Cfg),
            Ps = proplists:get_value(participants, Cfg),
            {ok, #{ signed_tx := StateSignedStateTx }} =
                sc_ws_get_state(IConnPid, Cfg),
            LatestRound = signed_channel_tx_round(StateSignedStateTx),
            channel_update(Cs, Who, Ps, 1, LatestRound + 1,
                          _TestErrors = false, Cfg),
            {ok, _, SignedSnapshotTx} =
                perform_snapshot_solo(Who, LatestRound + 1,
                                      Ps, Cs, Cfg, Opts),
            {ok, SignedSnapshotTx}
        end,
    sc_ws_optional_params_(Cfg0, MakeDeposit).

sc_ws_optional_params_close_mutual(Cfg0) ->
    {Opts, CheckFun} = ?config(fee_computation, Cfg0),
    %% start the channel with enough funds to be able to pay the big close
    %% mutual fee
    Cfg = sc_ws_open_(Cfg0, #{initiator_amount => ?ARBITRARY_BIG_FEE * 2,
                              responder_amount => ?ARBITRARY_BIG_FEE * 2}),

    {ok, SignedMutualTx} = sc_ws_close_mutual_(Cfg, initiator, Opts),
    CheckFun(SignedMutualTx),
    ok.

sc_ws_optional_params_(Cfg0, MakeOnChainTxFun) ->
    {Opts, CheckFun} = ?config(fee_computation, Cfg0),
    Cfg = sc_ws_open_(Cfg0),

    Test =
        fun(Who) ->
            {ok, SignedTx} = MakeOnChainTxFun(Cfg, Who, Opts),
            CheckFun(SignedTx)
        end,

    Test(initiator),
    Test(responder),

    sc_ws_close_mutual_(Cfg, initiator),
    ok.

sc_ws_optional_params_close_solo(Cfg0) ->
    {Opts, CheckFun} = ?config(fee_computation, Cfg0),
    Test =
        fun(Who) ->
            Cfg = sc_ws_open_(Cfg0),
            {ok, SignedTx} = sc_ws_close_solo_(Cfg, Who, Opts),
            CheckFun(SignedTx),
            settle_(Cfg, initiator)
        end,
    Test(initiator),
    Test(responder),
    ok.

sc_ws_optional_params_slash(Cfg0) ->
    {Opts, CheckFun} = ?config(fee_computation, Cfg0),
    Test =
        fun(Who) ->
            Cfg = sc_ws_open_(Cfg0),
            {ok, SignedTx} = sc_ws_slash_(Cfg, _WhoCloses = initiator,
                                          Who, Opts),
            CheckFun(SignedTx),
            settle_(Cfg, initiator)
        end,
    Test(initiator),
    Test(responder),
    ok.

sc_ws_optional_params_settle(Cfg0) ->
    {Opts, CheckFun} = ?config(fee_computation, Cfg0),
    Test =
        fun(Who) ->
            Cfg = sc_ws_open_(Cfg0),
            sc_ws_close_solo_(Cfg, initiator, #{}),
            {ok, SignedTx} = settle_(Cfg, Who, Opts),
            CheckFun(SignedTx),
            ok
        end,
    Test(initiator),
    Test(responder),
    ok.

sc_ws_abort_offchain_update(Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Cfg),
    UpdateOpts =
        #{ from => aeser_api_encoder:encode(account_pubkey, IPubkey)
         , to => aeser_api_encoder:encode(account_pubkey, RPubkey)
         , amount => 1 },
    InitUpdate =
        fun(WhoPid) ->
            with_registered_events([sign], [WhoPid],
                fun() ->
                    ws_send_tagged(WhoPid, <<"channels.update.new">>, UpdateOpts, Cfg),
                    {ok, _, <<"update">> = Tag, Data} =
                        wait_for_channel_event(WhoPid, sign, Cfg),
                    {Tag, Tag, Tag, Data}
                end)
        end,
    abort_update(InitUpdate, Cfg, can_abort_ack),
    sc_ws_close_(Cfg),
    ok.

sc_ws_abort_deposit(Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    InitUpdate =
        fun(WhoPid) ->
            with_registered_events([sign], [WhoPid],
                fun() ->
                    ws_send_tagged(WhoPid, <<"channels.deposit">>, #{amount => 2}, Cfg),
                    {ok, _, <<"deposit_tx">> = Tag, Data} =
                        wait_for_channel_event(WhoPid, sign, Cfg),
                    {Tag, Tag, <<"deposit_created">>, Data}
                end)
        end,
    abort_update(InitUpdate, Cfg, can_abort_ack),
    sc_ws_close_(Cfg),
    ok.

sc_ws_abort_withdraw(Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    InitUpdate =
        fun(WhoPid) ->
            with_registered_events([sign], [WhoPid],
                fun() ->
                    ws_send_tagged(WhoPid, <<"channels.withdraw">>, #{amount => 2}, Cfg),
                    {ok, _, <<"withdraw_tx">> = Tag, Data} =
                        wait_for_channel_event(WhoPid, sign, Cfg),
                    {Tag, Tag, <<"withdraw_created">>, Data}
                end)
        end,
    abort_update(InitUpdate, Cfg, can_abort_ack),
    sc_ws_close_(Cfg),
    ok.

sc_ws_abort_snapshot_solo(Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    Round0 = 2,
    %% Since we have the channel_create_tx on-chain, the channel has a
    %% round of 1 on-chain already. In order for the client to be able to
    %% perform a snapshot, one must procvide an channel_offchain_tx
    %% first: produxe the off-chain update:
    _Round1 = sc_ws_update_basic_round_(Round0, Cfg),
    InitUpdate =
        fun(WhoPid) ->
            with_registered_events([sign], [WhoPid],
                fun() ->
                    ws_send_tagged(WhoPid, <<"channels.snapshot_solo">>, #{}, Cfg),
                    {ok, _, <<"snapshot_solo_tx">> = Tag, Data} =
                        wait_for_channel_event(WhoPid, sign, Cfg),
                    {Tag, <<"snapshot_solo_sign">>, <<"snapshot_solo_sign">>, Data}
                end)
        end,
    %% now test the snapshot
    abort_update(InitUpdate, Cfg, abort_own),
    sc_ws_close_(Cfg),
    ok.

sc_ws_abort_shutdown(Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    InitUpdate =
        fun(WhoPid) ->
            with_registered_events([sign], [WhoPid],
                fun() ->
                    ws_send_tagged(WhoPid, <<"channels.shutdown">>, #{}, Cfg),
                    {ok, _, <<"shutdown_sign">> = Tag, Data} =
                        wait_for_channel_event(WhoPid, sign, Cfg),
                    {Tag, Tag, <<"shutdown">>, Data}
                end)
        end,
    abort_update(InitUpdate, Cfg, can_abort_ack),
    sc_ws_close_(Cfg),
    ok.

sc_ws_abort_slash(Cfg) ->
    Roles = [initiator, responder],
    [sc_ws_abort_slash_(WhoCloses, WhoRejects, Cfg)
        || WhoCloses  <- Roles,
           WhoRejects <- Roles],
    ok.

sc_ws_abort_slash_(WhoCloses, WhoRejects, Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    #{initiator := IConnPid, responder := RConnPid}
        = proplists:get_value(channel_clients, Cfg),
    #{initiator := #{pub_key := IPubkey, priv_key := IPrivkey},
      responder := #{pub_key := RPubkey, priv_key := RPrivkey}}
        = proplists:get_value(participants, Cfg),
    Round0 = 2,
    %% make an update so we are not at channel_create_tx
    Round1 = sc_ws_update_basic_round_(Round0, Cfg),
    %% make a close solo based on this update but don't post it yet
    {ok, #{ trees := Round2StateTrees
          , signed_tx := SignedRound2StateTx }} = sc_ws_get_state(IConnPid, Cfg),
    {CloserPubkey, CloserPrivkey} =
        case WhoCloses of
            initiator -> {IPubkey, IPrivkey};
            responder -> {RPubkey, RPrivkey}
        end,
    WhoPid =
        case WhoRejects of
            initiator -> IConnPid;
            responder -> RConnPid
        end,
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [CloserPubkey]),
    CloseSoloRound2Args =
        maps:merge(
            make_close_solo_args(SignedRound2StateTx, Round2StateTrees,
                                 [IPubkey, RPubkey]),
            #{ from_id => aeser_id:create(account, CloserPubkey)
             , nonce   => Nonce}),
    {ok, Round2CloseTx} = aesc_close_solo_tx:new(CloseSoloRound2Args),
    %% sign this tx
    SignedTx = aec_test_utils:sign_tx(Round2CloseTx, CloserPrivkey),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    EncRound2CloseTx = aeser_api_encoder:encode(transaction, SerializedTx),
    Round2CloseTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),

    %% make a new off-chain update
    _Round2 = sc_ws_update_basic_round_(Round1, Cfg),

    %% at this point the Round2CloseTx is not based on the latest state so it
    %% is malicious to post it on-chain
    with_registered_events([sign, info], [WhoPid],
        fun() ->
            %% post the malicious tx and wait it to be included
            post_transactions_sut(EncRound2CloseTx),
            ok = wait_for_tx_hash_on_chain(Round2CloseTxHash),
            {ok, _, #{<<"event">> := <<"closing">>}} =
                wait_for_channel_event(WhoPid, info, Cfg),
            ws_send_tagged(WhoPid, <<"channels.slash">>, #{}, Cfg),
            {ok, _, <<"slash_tx">>, _Data} =
                wait_for_channel_event(WhoPid, sign, Cfg)
        end),
    %% now test the snapshot
    sc_ws_abort_update_(WhoPid, <<"slash_sign">>, Cfg),
    sc_ws_close_(Cfg),
    ok.

sc_ws_abort_settle(Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    sc_ws_close_solo_(Cfg, initiator, #{}),
    InitUpdate =
        fun(WhoPid) ->
            with_registered_events([sign], [WhoPid],
                fun() ->
                    ws_send_tagged(WhoPid, <<"channels.settle">>, #{}, Cfg),
                    {ok, _, <<"settle_sign">> = Tag, Data} =
                        wait_for_channel_event(WhoPid, sign, Cfg),
                    {ok, _, #{<<"event">> := <<"closing">>}} =
                        wait_for_channel_event(WhoPid, info, Cfg),
                    {Tag, Tag, <<"shutdown">>, Data}
                end)
        end,
    abort_update(InitUpdate, Cfg, abort_own),
    sc_ws_close_(Cfg),
    ok.

abort_update(InitUpdate, Cfg, TestType) when TestType =:= abort_own;
                                             TestType =:= can_abort_ack ->
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, Cfg),
    #{initiator := #{ pub_key  := IPubkey
                    , priv_key := IPrivkey}} = proplists:get_value(participants, Cfg),
    {_, AbortTag, _, _} = InitUpdate(IConnPid),
    sc_ws_abort_update_(IConnPid, AbortTag, Cfg),

    {_, AbortTag, _, _} = InitUpdate(RConnPid),
    sc_ws_abort_update_(RConnPid, AbortTag, Cfg),

    case TestType of
        abort_own ->
            ok;
        can_abort_ack ->
            {Tag, _, Event,
            #{ <<"signed_tx">> := EncSignedTx
              , <<"updates">>   := Updates}} = InitUpdate(IConnPid),
            ok = ?WS:register_test_for_channel_events(RConnPid, [info, sign, error]),
            ok = ?WS:register_test_for_channel_events(IConnPid, [error]),
            Method = <<"channels.", (bin(Tag))/binary>>,
            channel_sign_tx_(EncSignedTx, Updates, Tag, Method, IPubkey, IConnPid, IPrivkey,
                             Cfg),

            %% once signed initiator can not abort it anymore
            ws_send_tagged(IConnPid, Method, #{<<"error">> => 42}, Cfg),
            {ok, _, #{<<"reason">> := <<"Not allowed at current channel state">>}} =
                wait_for_channel_event(IConnPid, error, Cfg),
            ok = ?WS:unregister_test_for_channel_events(IConnPid, [error]),

            %% on the other hand, responder can abort it
            {ok, _, #{<<"event">> := Event}} = wait_for_channel_event(RConnPid, info, Cfg),
            {ok, _, AckTag, _Data} = wait_for_channel_event(RConnPid, sign, Cfg),
            ok = ?WS:unregister_test_for_channel_events(RConnPid, [info, sign]),

            with_registered_events([conflict], [IConnPid],
                fun() ->
                    sc_ws_abort_update_(RConnPid, AckTag, Cfg),
                    {ok, _, #{ <<"error_code">> := 42
                             , <<"error_msg">>  := <<"unknown">> }} =
                        wait_for_channel_event(IConnPid, conflict, Cfg)
                end)
    end,
    ok.

sc_ws_abort_update_(WhoPid, Tag, Cfg) ->
    ok = ?WS:register_test_for_channel_events(WhoPid, [info]),
    ws_send_tagged(WhoPid, <<"channels.", Tag/binary>>,
                   #{<<"error">> => 42}, Cfg),
    {ok, _, #{<<"event">> := <<"aborted_update">>}} =
        wait_for_channel_event(WhoPid, info, Cfg),
    ok = ?WS:unregister_test_for_channel_events(WhoPid, [info]).


make_close_solo_args(SignedOffChainTx, OffchainTrees, Pubkeys) ->
    {channel_offchain_tx, OffChainTx}
        = aetx:specialize_type(aetx_sign:innermost_tx(SignedOffChainTx)),
    ChannelId = aesc_offchain_tx:channel_id(OffChainTx),
    Payload = aetx_sign:serialize_to_binary(SignedOffChainTx),
    {ok, PoI} =
        lists:foldl(
            fun(Pubkey, {ok, PoIAccum}) -> aec_trees:add_poi(accounts,
                                                            Pubkey, OffchainTrees,
                                                            PoIAccum)
            end,
            {ok, aec_trees:new_poi(OffchainTrees)},
            Pubkeys),
    #{ channel_id => ChannelId
     , payload    => Payload
     , poi        => PoI
     , fee        => 40000 * aec_test_utils:min_gas_price()}.

sc_ws_can_not_abort_while_open(Cfg0) ->
    Cfg = sc_ws_open_(Cfg0),
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, Cfg),
    lists:foreach(
        fun(WhoPid) ->
            ok = ?WS:register_test_for_channel_events(WhoPid, [error]),
            ws_send_tagged(WhoPid, <<"channels.update">>, #{<<"error">> => 42}, Cfg),
            {ok, _, #{<<"reason">> := <<"Not allowed at current channel state">>}} =
                wait_for_channel_event(WhoPid, error, Cfg),
            ok = ?WS:unregister_test_for_channel_events(WhoPid, [error]),
            ok
        end,
        [IConnPid, RConnPid]),
    sc_ws_close_(Cfg),
    ok.

ok({ok, Term}) -> Term.

sc_ws_force_progress_based_on_offchain_state(Config0) ->
    Test =
        fun(ContractOwner, Forcer) ->
            Config = sc_ws_open_(Config0),
            ContractName = counter,
            %% create and call some contracts
            sc_ws_contract_(Config, ContractName, ContractOwner),
            [ContractPubkey] = contract_ids(Config),
            {ok, _SignedTx} = sc_ws_force_progress_(Forcer, ContractPubkey,
                              ContractName, "tick", [], _Amount = 10, #{}, Config),
            ct:log("*** Closing ***", []),
            ok = sc_ws_close_(Config)
        end,
    [Test(ContractOwner, Forcer) || ContractOwner <- ?ROLES
                                  , Forcer <- ?ROLES],
    ok.

sc_ws_force_progress_based_on_onchain_state(Config0) ->
    Test =
        fun(ContractOwner, Snapshotter, Forcer) ->
            Config = sc_ws_open_(Config0),
            ContractName = counter,
            %% create and call some contracts
            sc_ws_contract_(Config, ContractName, ContractOwner),
            perform_snapshot_solo(Snapshotter, Config, #{}),
            [ContractPubkey] = contract_ids(Config),
            {ok, _SignedTx} = sc_ws_force_progress_(Forcer, ContractPubkey,
                              ContractName, "tick", [], _Amount = 10, #{}, Config),
            ct:log("*** Closing ***", []),
            ok = sc_ws_close_(Config)
        end,
    [Test(ContractOwner, Snapshotter, Forcer) || ContractOwner <- ?ROLES
                                               , Snapshotter <- ?ROLES
                                               , Forcer <- ?ROLES],
    ok.

sc_ws_failed_force_progress_balances(Config0) ->
    Test =
        fun(ContractOwner, Snapshotter, Forcer) ->
            Config = sc_ws_open_(Config0),
            Participants = proplists:get_value(participants, Config),
            #{pub_key := ForcerPubkey} = maps:get(Forcer, Participants),
            #{Forcer := ConnPid} = proplists:get_value(channel_clients, Config),
            ContractName = counter,
            ContractName2 = chain,
            %% create and call some contracts
            sc_ws_contract_(Config, ContractName, ContractOwner),
            perform_snapshot_solo(Snapshotter, Config, #{}),
            [ContractPubkey] = contract_ids(Config),
            Amount = 10,
            {ok, ContractBalance0} = sc_ws_get_balance(ConnPid, ContractPubkey, Config),
            {ok, ForcerBalance0} = sc_ws_get_balance(ConnPid, ForcerPubkey, Config),
            %% force progress with some unexpected calldata: this is a
            %% correctly structured calldata but not for this contract
            %% there is no "miner" function and this can be known only after
            %% a contract execution. The FSM does not protect from this
            {ok, _SignedTx} = sc_ws_force_progress_(Forcer, ContractPubkey,
                              ContractName2, "miner", [], Amount, #{},
                              Config, <<"error">>),
            {ok, ContractBalance} = sc_ws_get_balance(ConnPid, ContractPubkey, Config),
            {ok, ForcerBalance} = sc_ws_get_balance(ConnPid, ForcerPubkey, Config),
            case aect_test_utils:latest_protocol_version() of
                PostIrisVsn when PostIrisVsn >= ?IRIS_PROTOCOL_VSN ->
                    ?assertEqual(ContractBalance, ContractBalance0),
                    ?assertEqual(ForcerBalance, ForcerBalance0);
                _PreIrisVsn ->
                    ?assertEqual(ContractBalance - Amount, ContractBalance0),
                    ?assertEqual(ForcerBalance + Amount, ForcerBalance0)
            end
        end,
    [Test(ContractOwner, Snapshotter, Forcer) || ContractOwner <- ?ROLES
                                               , Snapshotter <- ?ROLES
                                               , Forcer <- ?ROLES],
    ok.


contract_ids(Config) ->
    #{initiator := IConnPid} = proplists:get_value(channel_clients, Config),
    {ok, #{ trees := StateTrees }} = sc_ws_get_state(IConnPid, Config),
    ContractsTree = aec_trees:contracts(StateTrees),
    [ Key || {Key = <<_:256>>, _} <- aect_state_tree:to_list(ContractsTree)].

sc_ws_force_progress_(Origin, ContractPubkey,
                      ContractName, Function, Arguments, Amount, XOpts,
                      Config) ->
    sc_ws_force_progress_(Origin, ContractPubkey,
                          ContractName, Function, Arguments, Amount, XOpts,
                          Config, <<"ok">>).

sc_ws_force_progress_(Origin, ContractPubkey,
                      ContractName, Function, Arguments, Amount, XOpts,
                      Config, ReturnType)
    when Origin =:= initiator orelse Origin =:= responder ->
    ct:log("*** Forcing progress ***", []),
    {ok, EncodedData} = encode_call_data(ContractName, Function, Arguments),
    sc_ws_force_progress_(Origin, ContractPubkey,
                          EncodedData, Amount, XOpts, Config, ReturnType).

sc_ws_force_progress_(Origin, ContractPubkey,
                      EncodedData, Amount, XOpts, Config, ReturnType) ->
    Participants = proplists:get_value(participants, Config),
    #{ channel_id := ChannelId
     , initiator  := IConnPid
     , responder  := RConnPid } = Clients
        = proplists:get_value(channel_clients, Config),
    #{pub_key := _SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(Origin, Participants),
    ConnPid = maps:get(Origin, Clients),
    ok = ?WS:register_test_for_channel_events(IConnPid, [on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [on_chain_tx]),

    {ok, Channel0} = get_channel(ChannelId),
    Round0 = aesc_channels:round(Channel0),
    StateHash0 = aesc_channels:state_hash(Channel0),

    %% force progress
    %% using random gas_price to check no hardcoded one
    GasPrice = aec_test_utils:min_gas_price() + rand:uniform(10000),
    FPOpts =
        XOpts#{ contract_id => aeser_api_encoder:encode(contract_pubkey, ContractPubkey)
              , abi_version => aect_test_utils:abi_version()
              , amount      => Amount
              , call_data   => EncodedData
              , gas_price   => GasPrice},

    {ok, SignedTx}
        = request_and_sign(ConnPid,
            <<"force_progress_tx">>, <<"channels.force_progress_sign">>,
            SenderPrivkey, Config,
            fun() ->
                    case ?WS:json_rpc_call(
                            ConnPid, #{ <<"method">> => <<"channels.force_progress">>
                                      , <<"params">> => FPOpts }) of
                        <<"ok">> -> ok;
                        Other    -> {error, Other}
                    end
            end),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    wait_for_tx_hash_on_chain(TxHash),
    {ok, 200, #{ <<"call_info">> :=
                     #{<<"caller_id">> := _,
                       <<"caller_nonce">> := _,
                       <<"contract_id">> := _,
                       <<"gas_price">> := _,
                       <<"gas_used">> := _,
                       <<"height">> := _,
                       <<"log">> := _,
                       <<"return_type">> := ReturnType,
                       <<"return_value">> := _}}} =
        aehttp_integration_SUITE:get_transaction_info_by_hash_sut(binary_to_list(TxHash)),

    %% ensure channel object changed
    {ok, Channel1} = get_channel(ChannelId),
    Round1 = aesc_channels:round(Channel1),
    StateHash1 = aesc_channels:state_hash(Channel1),
  
    ct:log("Force progress asserts:~nOld round ~p, New round ~p",
           [Round0, Round1]),
    ?assertNotEqual(StateHash0, StateHash1),
    ?assertEqual(true, Round0 < Round1),

    %% both clients are informed for the Forced progress on-chain
    {ok, _, #{ <<"tx">> := _EncodedSignedWTx
             , <<"info">> := <<"consumed_forced_progress">>
             , <<"type">> := <<"channel_force_progress_tx">>}}
        = wait_for_channel_event(IConnPid, on_chain_tx, Config),

    {ok, _, #{ <<"tx">> := _EncodedSignedWTx
             , <<"info">> := <<"consumed_forced_progress">>
             , <<"type">> := <<"channel_force_progress_tx">>}}
        = wait_for_channel_event(RConnPid, on_chain_tx, Config),

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [on_chain_tx]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [on_chain_tx]),

    {ok, SignedTx}.

get_channel(ChannelId) ->
    {ok, ChannelPubkey} =
        aeser_api_encoder:safe_decode(channel, ChannelId),
    case rpc(aec_chain, get_channel, [ChannelPubkey]) of
        {ok, _Channel} = OK -> OK;
        {error, _} = Err -> Err
    end.

sc_ws_leave_responder_does_not_timeout(Config0) ->
    ct:log("opening channel", []),
    ct:log("Config0 = ~p", [Config0]),
    IdleTimeout = 1000,
    Config = sc_ws_open_([{slogan, ?SLOGAN}|Config0],
                         #{ responder_opts => #{keep_running => true}
                          , timeout_idle => IdleTimeout}),
    #{ responder_fsm_id := RFsmId
     , responder        := RConnPid } = proplists:get_value(channel_clients, Config),
    ct:log("channel opened", []),
    ct:log("Config = ~p", [Config]),
    ct:log("*** Leaving channel ***", []),

    Config1 = [{responder_leaves, false}|Config],
    ct:log("Config1 = ~p", [Config1]),
    ReestablishOptions = sc_ws_leave_(Config1),

    timer:sleep(IdleTimeout + 100),
    ping_pong(RConnPid, Config),
    Config2 = reconnect_client_(ReestablishOptions, initiator,
                                [{reconnect_scenario, reestablish} | Config1]),
    ct:log("*** Verifying that channel is operational ***", []),
    ok = sc_ws_update_(Config2),
    ct:log("*** Closing ... ***", []),
    ok = sc_ws_close_(Config2).

