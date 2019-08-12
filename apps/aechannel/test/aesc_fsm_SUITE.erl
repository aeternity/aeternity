%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Test utils for State Channels
%%% @end
%%%=============================================================================

-module(aesc_fsm_SUITE).

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
-export([
          create_channel/1
        , multiple_responder_keys_per_port/1
        , channel_insufficent_tokens/1
        , inband_msgs/1
        , upd_transfer/1
        , update_with_conflict/1
        , update_with_soft_reject/1
        , deposit_with_conflict/1
        , deposit_with_soft_reject/1
        , withdraw_with_conflict/1
        , withdraw_with_soft_reject/1
        , upd_dep_with_conflict/1
        , upd_wdraw_with_conflict/1
        , dep_wdraw_with_conflict/1
        , deposit/1
        , withdraw/1
        , channel_detects_close_solo/1
        , leave_reestablish/1
        , leave_reestablish_close/1
        , change_config_get_history/1
        , multiple_channels/1
        , many_chs_msg_loop/1
        , check_incorrect_create/1
        , check_incorrect_deposit/1
        , check_incorrect_withdrawal/1
        , check_incorrect_update/1
        , check_incorrect_mutual_close/1
        , check_mutual_close_with_wrong_amounts/1
        , check_mutual_close_after_close_solo/1
        , attach_initiator/1
        , attach_responder/1
        , initiator_spend/1
        , responder_spend/1
        , client_reconnect_initiator/1
        , client_reconnect_responder/1
        ]).

%% exports for aehttp_integration_SUITE
-export([
          create_channel_on_port/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(TIMEOUT, 10000).
-define(LONG_TIMEOUT, 60000).
-define(PORT, 9325).

-define(BOGUS_PUBKEY, <<12345:32/unit:8>>).
-define(BOGUS_PRIVKEY, <<12345:64/unit:8>>).

-define(LOG(E), ct:log("LINE ~p <== ~p", [?LINE, E])).

-define(MINIMUM_DEPTH, 3).

-define(SLOGAN, {slogan, {?FUNCTION_NAME, ?LINE}}).
-define(SLOGAN(I), {slogan, {?FUNCTION_NAME, ?LINE, I}}).

-define(MAX_MINED_BLOCKS, 20).

-define(I_SECP256K1_PRIV, <<61,194,116,40,192,100,75,189,11,148,242,211,52,100,55,
                            188,165,162,142,65,19,181,89,25,9,228,120,175,152,249,
                            83,234>>).

-define(I_SECP256K1_PUB,  <<80,118,213,110,10,210,229,6,53,195,79,174,91,179,150,
                            239,241,145,147,201,2,48,7,221,141,221,250,110,105,29,
                            90,39,75,16,175,218,92,200,158,78,172,106,64,159,90,180,
                            147,171,9,70,251,21,150,252,90,63,233,252,123,104,198,
                            103,79,255>>).

-define(I_OWNER, aega_test_utils:to_hex_lit(64, ?I_SECP256K1_PUB)).

-define(R_SECP256K1_PRIV, <<0,169,18,170,156,1,39,254,83,178,199,29,237,192,219,236,
                            123,56,91,160,110,87,188,70,11,12,162,137,254,88,178,86,
                            87>>).

-define(R_SECP256K1_PUB,  <<15,249,155,45,180,160,214,151,219,55,30,123,106,151,
                            231,215,225,242,77,213,174,10,209,196,149,19,17,235,69,
                            28,110,249,37,177,164,60,163,135,0,139,210,80,69,176,55,
                            81,255,196,182,224,32,193,177,173,153,101,72,244,218,
                            138,84,57,120,255>>).

-define(R_OWNER, aega_test_utils:to_hex_lit(64, ?R_SECP256K1_PUB)).

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [ {group, transactions}
                             , {group, errors}
                             , {group, client_reconnect}
                             , {group, throughput}
                             , {group, signatures}
                             , {group, channel_ids}
                             , {group, round_too_low}
                             , {group, round_too_high}
                             , {group, generalized_accounts}
                             ]},
     {transactions, [sequence],
      [
        create_channel
      , multiple_responder_keys_per_port
      , channel_insufficent_tokens
      , inband_msgs
      , upd_transfer
      , update_with_conflict
      , update_with_soft_reject
      , deposit_with_conflict
      , deposit_with_soft_reject
      , withdraw_with_conflict
      , withdraw_with_soft_reject
      , upd_dep_with_conflict
      , upd_wdraw_with_conflict
      , dep_wdraw_with_conflict
      , deposit
      , withdraw
      , channel_detects_close_solo
      , leave_reestablish
      , leave_reestablish_close
      , change_config_get_history
      ]},
     {throughput, [sequence],
      [
        multiple_channels
      , many_chs_msg_loop
      ]},
     {errors, [sequence],
      [
        check_mutual_close_with_wrong_amounts
      , check_mutual_close_after_close_solo
      ]},
     {signatures, [sequence], [check_incorrect_create | update_sequence()]
                               ++ [check_incorrect_mutual_close]},
     {channel_ids, [sequence],
      % tests all mutually signed transactions with the exception of
      % channel_create_tx as its channel_id is not subject to the transaction
      % as it is computed. Unilateral transactions are not tested.
      update_sequence() ++ [check_incorrect_mutual_close]},
     {round_too_low, [sequence], update_sequence()},
     {round_too_high, [sequence], update_sequence()},
     {state_hash, [sequence], [check_incorrect_create | update_sequence()]},
     {generalized_accounts, [sequence],
      [ {group, initiator_is_ga}
      , {group, responder_is_ga}
      , {group, both_are_ga}
      ]},
     {initiator_is_ga, [sequence], ga_sequence()},
     {responder_is_ga, [sequence], ga_sequence()},
     {both_are_ga, [sequence], ga_sequence()},
     {client_reconnect, [sequence],
      [ client_reconnect_initiator
      , client_reconnect_responder ]}
    ].

ga_sequence() ->
    [ {group, transactions}
    , {group, errors}
    , {group, signatures}
    , {group, channel_ids}
    ].

update_sequence() ->
    [ check_incorrect_deposit
    , check_incorrect_withdrawal
    , check_incorrect_update].

suite() ->
    [].

init_per_suite(Config) ->
    {ok, StartedApps} = application:ensure_all_started(gproc),
    {_PrivKey, PubKey} = aecore_suite_utils:sign_keys(dev1),
    TableOwner = new_config_table(),
    Miner = aeser_api_encoder:encode(account_pubkey, PubKey),
    DefCfg = #{<<"chain">> => #{<<"persist">> => false},
               <<"mining">> => #{<<"micro_block_cycle">> => 1,
                                 <<"beneficiary">> => Miner,
                                 <<"beneficiary_reward_delay">> => 2}},
    Config1 = aecore_suite_utils:init_per_suite([dev1], DefCfg, [{symlink_name, "latest.aesc_fsm"}, {test_module, ?MODULE}] ++ Config),
    [ {nodes, [aecore_suite_utils:node_tuple(N)
               || N <- [dev1]]}
    , {table_owner, TableOwner}
    , {started_apps, StartedApps} | Config1].

end_per_suite(Config) ->
    StartedApps = proplists:get_value(started_apps, Config),
    [ok = application:stop(A) || A <- lists:reverse(StartedApps)],
    TableOwner = ?config(table_owner, Config),
    TableOwner ! die,
    ok.

init_per_group(signatures, Config0) ->
    Config = init_per_group_(Config0),
    [{wrong_create, fun wrong_sig_create/2},
     {wrong_action, fun wrong_sig_action/4},
     {wrong_action_detailed, fun wrong_sig_action/5}
     | Config];
init_per_group(channel_ids, Config0) ->
    Config = init_per_group_(Config0),
    [{wrong_create, fun(_, _) -> error(no_invalid_id_on_create) end},
     {wrong_action, fun wrong_id_action/4},
     {wrong_action_detailed, fun wrong_id_action/5}
     | Config];
init_per_group(round_too_low, Config0) ->
    ModRoundTooLow = fun(Round) -> Round end, % keep previous round
    Config = init_per_group_(Config0),
    [{wrong_create, fun(_, _) -> error(no_invalid_round_on_create) end},
     {wrong_action, fun(C, P, M, F) -> wrong_round_action(C, P, M, F,
                                                          ModRoundTooLow) end},
     {wrong_action_detailed, fun(C, P, M, F, D) -> wrong_round_action(C, P, M, F,
                                                          D, ModRoundTooLow) end}
     | Config];
init_per_group(round_too_high, Config0) ->
    ModRoundTooHigh = fun(Round) -> Round + 2 end, % skip one
    Config = init_per_group_(Config0),
    [{wrong_create, fun(_, _) -> error(no_invalid_round_on_create) end},
     {wrong_action, fun(C, P, M, F) -> wrong_round_action(C, P, M, F,
                                                          ModRoundTooHigh) end},
     {wrong_action_detailed, fun(C, P, M, F, D) -> wrong_round_action(C, P, M, F,
                                                          D, ModRoundTooHigh) end}
     | Config];
init_per_group(state_hash, Config0) ->
    Config = init_per_group_(Config0),
    [{wrong_create, fun wrong_hash_create/2},
     {wrong_action, fun wrong_hash_action/4},
     {wrong_action_detailed, fun wrong_hash_action/5}
     | Config];
init_per_group(Group, Config) when Group =:= initiator_is_ga;
                                   Group =:= responder_is_ga;
                                   Group =:= both_are_ga ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        _ ->
            Config1 = init_per_group_(Config),
            Params =
                fun(Owner, PrivKey) ->
                    #{init_params => [Owner],
                      prep_fun =>
                          fun(TxHash, N) ->
                              btc_auth(TxHash, integer_to_list(N), PrivKey)
                          end}
                end,
            Config2 =
                [{ga, #{contract    => "bitcoin_auth",
                        auth_fun    => "authorize",
                        auth_params => #{initiator =>
                                            Params(?I_OWNER,
                                                   ?I_SECP256K1_PRIV),
                                        responder =>
                                            Params(?R_OWNER,
                                                   ?R_SECP256K1_PRIV)}}},
                 {ga_group, true},
                 {bench_rounds, 100} %% a lower amount than the default 1000
                    | Config1],
            case Group of
                initiator_is_ga ->
                    attach_initiator(Config2),
                    initiator_spend(Config2);
                responder_is_ga ->
                    attach_responder(Config2),
                    responder_spend(Config2);
                both_are_ga ->
                    attach_initiator(Config2),
                    initiator_spend(Config2),
                    attach_responder(Config2),
                    responder_spend(Config2)
            end,
            Config2
    end;
init_per_group(generalized_accounts, Config) ->
    Config;
init_per_group(_Group, Config) ->
    init_per_group_(Config).

init_per_group_(Config) ->
    case proplists:get_value(ga_group, Config, false) of
        true -> Config;
        false ->
            aecore_suite_utils:start_node(dev1, Config),
            aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
            ct:log("dev1 connected", []),
            try begin
                    Initiator = prep_initiator(dev1),
                    Responder = prep_responder(Initiator, dev1),
                    Responder2 = prep_responder(Initiator, dev1),
                    [{initiator, Initiator},
                    {responder, Responder},
                    {responder2, Responder2},
                    {port, ?PORT},
                    {initiator_amount, 10000000 * aec_test_utils:min_gas_price()},
                    {responder_amount, 10000000 * aec_test_utils:min_gas_price()}
                    | Config]
                end
            catch
                error:Reason ->
                    Trace = erlang:get_stacktrace(),
                    catch stop_node(dev1, Config),
                    error(Reason, Trace)
            end
    end.

end_per_group(Group, Config) ->
    case proplists:get_value(ga_group, Config, false) of
        true ->
            case lists:member(Group, [initiator_is_ga,
                                      responder_is_ga, both_are_ga]) of
                true ->
                    _Config1 = stop_node(dev1, Config);
                false -> pass
            end;
        false ->
            _Config1 = stop_node(dev1, Config)
    end,
    ok.

init_per_testcase(_, Config) ->
    Config1 = load_idx(Config),
    [{debug, true} | Config1].

end_per_testcase(T, _Config) when T==multiple_channels;
                                  T==many_chs_msg_loop ->
    Node = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    bump_idx(),
    ok;
end_per_testcase(_Case, _Config) ->
    bump_idx(),
    ok.


stop_node(N, Config) ->
    aecore_suite_utils:stop_node(N, Config),
    Config.


%%%===================================================================
%%% Test state
%%%===================================================================

create_channel(Cfg) ->
    with_trace(fun t_create_channel_/1, Cfg, "create_channel").

multiple_responder_keys_per_port(Cfg) ->
    Slogan = ?SLOGAN,
    %% Debug = get_debug(Cfg),
    Debug = true,
    {_, Responder2} = lists:keyfind(responder2, 1, Cfg),
    ct:log("Responder2 = ~p", [Responder2]),
    Cfg2 = lists:keyreplace(responder, 1, Cfg, {responder, Responder2}),
    Me = self(),
    Initiator = maps:get(pub, ?config(initiator, Cfg)),
    InitiatorAccountType = account_type(Initiator),
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Initiator]),
    CreateMultiChannel =
        fun(N, CustomCfg) ->
            ChannelCfg0 =
                [ {port, ?PORT},
                  {ack_to, Me},
                  {minimum_depth, 0},
                  {slogan, {Slogan, N}} | CustomCfg],
            ChannelCfg =
                case InitiatorAccountType of
                    basic -> % give away nonces in advance to avoid race conditions
                        [{nonce, Nonce + N - 1} | ChannelCfg0];
                    generalized -> % nonce is 0
                        ChannelCfg0
                end,
            C = create_multi_channel(ChannelCfg, #{mine_blocks => {ask, Me},
                                                               debug => Debug}, false),
            [{C, Tx}] = collect_acks_w_payload([C], mine_blocks, 1),
            TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)),
            mine_blocks_until_txs_on_chain(dev1, [TxHash]),
            C
        end,
    C1 = CreateMultiChannel(1, Cfg),
    C2 = CreateMultiChannel(2, Cfg2),
    Cs = [C1, C2],
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    Cs = collect_acks(Cs, channel_ack, 2),
    ct:log("channel pids collected: ~p", [Cs]),
    %% At this point, we know the pairing worked
    [begin
         MRef = erlang:monitor(process, P),
         ct:log("P (~p) info: ~p", [P, process_info(P)]),
         P ! die,
         receive {'DOWN', MRef, _, _, _} -> ok
         after 5000 ->
                 ct:log("timed out: ~p", [process_info(self(), messages)]),
                 erlang:error({channel_not_dying, P})
         end
     end || P <- Cs],
    ok.

t_create_channel_(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI, channel_id := ChannelId} = I
     , r := #{} = R} = create_channel_([?SLOGAN|Cfg]),

    {ok, _} = rpc(dev1, aec_chain, get_channel, [ChannelId]),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    _ = await_signing_request(shutdown, I, ?TIMEOUT, Debug, Cfg),
    _ = await_signing_request(shutdown_ack, R, ?TIMEOUT, Debug, Cfg),
    SignedTx = await_on_chain_report(I, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    verify_close_mutual_tx(SignedTx, ChannelId),
    check_info(500),
    ok.

channel_insufficent_tokens(Cfg) ->
    Debug = get_debug(Cfg),
    Port = 9325,
    Test =
        fun(IAmt, RAmt, ChannelReserve, PushAmount, Error) ->
            {_, _, Spec} =
                channel_spec([{initiator_amount, IAmt},
                              {responder_amount, RAmt},
                              ?SLOGAN|Cfg],
                              ChannelReserve, PushAmount),
            {error, Error} =
                rpc(dev1, aesc_fsm, respond, [Port, Spec], Debug),
            {error, Error} =
                rpc(dev1, aesc_fsm, initiate, ["localhost", Port, Spec], Debug)
        end,
    Test(10, 10, 5, 6, insufficient_initiator_amount),
    Test(10, 1, 5, 3, insufficient_responder_amount),
    Test(1, 1, 5, 3, insufficient_amounts),
    ok.

inband_msgs(Cfg) ->
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := PubR }} = create_channel_(
                                           [{push_amount, 1234},
                                             ?SLOGAN|Cfg]),
    ok = rpc(dev1, aesc_fsm, inband_msg, [FsmI, PubR, <<"i2r hello">>]),
    {ok,_} =receive_from_fsm(
              message, R,
              fun(#{info := #{info := <<"i2r hello">>}}) -> ok end, 1000, true),

    shutdown_(I, R, Cfg),
    check_info(500).

upd_transfer(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI, channel_id := ChannelId} = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {I0, R0} = do_update(PubI, PubR, 2, I, R, true, Cfg),
    {BalI1, BalR1} = get_both_balances(FsmI, PubI, PubR),
    BalI1 = BalI - 2,
    BalR1 = BalR + 2,
    {I1, R1} = update_bench(I0, R0, Cfg),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    {_I2, _} = await_signing_request(shutdown, I1, Cfg),
    {_R2, _} = await_signing_request(shutdown_ack, R1, Cfg),
    SignedTx = await_on_chain_report(I, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    verify_close_mutual_tx(SignedTx, ChannelId),
    check_info(500),
    ok.

update_with_conflict(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, PubI, PubR, 1]),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmR, PubR, PubI, 2]),
    {_I1, _} = await_signing_request(update, I, Debug, Cfg),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(update, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

update_with_soft_reject(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{}            = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, PubI, PubR, 1]),
    {I1, _} = await_signing_request(update, I, Debug, Cfg),
    Reject = fun(_Tag, #{fsm := Fsm1} = Rx, SignedTx, _Updates) ->
                     {error, conflict} = rpc(dev1, aesc_fsm, upd_transfer,
                                             [Fsm1, PubR, PubI, 1]),
                     {Rx, SignedTx}
             end,
    {R1, _} = await_signing_request(update_ack, R, Reject, ?TIMEOUT, Debug, Cfg),
    {ok, _} = receive_from_fsm(conflict, I1, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R1, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

deposit_with_conflict(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => 1}]),
    rpc(dev1, aesc_fsm, upd_deposit, [FsmR, #{amount => 2}]),
    {_I1, _} = await_signing_request(deposit_tx, I, Debug, Cfg),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(deposit_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

deposit_with_soft_reject(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{}            = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => 1}]),
    {I1, _} = await_signing_request(deposit_tx, I, Debug, Cfg),
    Reject = fun(_Tag, #{fsm := Fsm1} = Rx, SignedTx, _Updates) ->
                     {error, conflict} = rpc(dev1, aesc_fsm, upd_transfer,
                                             [Fsm1, PubR, PubI, 1]),
                     {Rx, SignedTx}
             end,
    {R1, _} = await_signing_request(deposit_created, R, Reject, ?TIMEOUT, Debug, Cfg),
    {ok,_} = receive_from_fsm(conflict, I1, any_msg(), ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(conflict, R1, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

withdraw_with_conflict(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, #{amount => 1}]),
    rpc(dev1, aesc_fsm, upd_withdraw, [FsmR, #{amount => 2}]),
    {_I1, _} = await_signing_request(withdraw_tx, I, Debug, Cfg),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(withdraw_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

withdraw_with_soft_reject(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{}            = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, #{amount => 1}]),
    {I1, _} = await_signing_request(withdraw_tx, I, Debug, Cfg),
    Reject = fun(_Tag, #{fsm := Fsm1} = Rx, SignedTx, _Updates) ->
                     {error, conflict} = rpc(dev1, aesc_fsm, upd_transfer,
                                             [Fsm1, PubR, PubI, 1]),
                     {Rx, SignedTx}
             end,
    {R1, _} = await_signing_request(withdraw_created, R, Reject, ?TIMEOUT, Debug, Cfg),
    {ok,_} = receive_from_fsm(conflict, I1, any_msg(), ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(conflict, R1, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

upd_dep_with_conflict(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, PubI, PubR, 1]),
    rpc(dev1, aesc_fsm, upd_deposit, [FsmR, #{amount => 2}]),
    {_I1, _} = await_signing_request(update, I, Debug, Cfg),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(deposit_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

upd_wdraw_with_conflict(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, PubI, PubR, 1]),
    rpc(dev1, aesc_fsm, upd_withdraw, [FsmR, #{amount => 2}]),
    {_I1, _} = await_signing_request(update, I, Debug, Cfg),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(withdraw_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

dep_wdraw_with_conflict(Cfg) ->
    Debug = true,
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => 1}]),
    rpc(dev1, aesc_fsm, upd_withdraw, [FsmR, #{amount => 2}]),
    {_I1, _} = await_signing_request(deposit_tx, I, Debug, Cfg),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(withdraw_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

signing_req() ->
    fun(#{type := sign}) -> true;
       (_) -> false
    end.

any_msg() ->
    fun(_) -> true end.

update_bench(I, R, C) ->
    Rounds = proplists:get_value(bench_rounds, C, 1000),
    {Time, I1, R1} = do_n(Rounds, fun update_volley/3, I, R, C),
    Fmt = "Time (1*2*" ++ integer_to_list(Rounds) ++ "): ~.1f s; ~.1f mspt; ~.1f tps",
    Args = [Time/1000, Time/2000, 2000*Rounds/Time],
    ct:log(Fmt, Args),
    ct:comment(Fmt, Args),
    {I1, R1}.

do_n(N, F, I, R, C) ->
    TS = erlang:system_time(millisecond),
    {I1, R1} = do_n_(N, F, I, R, C),
    {erlang:system_time(millisecond) - TS, I1, R1}.

do_n_(0, _, I, R, _C) ->
    {I, R};
do_n_(N, F, I, R, C) when N > 0 ->
    {I1, R1} = F(I, R, C),
    do_n_(N-1, F, I1, R1, C).

     
update_volley(#{pub := PubI} = I, #{pub := PubR} = R, Cfg) ->
    {I1, R1} = do_update(PubR, PubI, 1, I, R, false, Cfg),
    do_update(PubI, PubR, 1, I1, R1, false, Cfg).

do_update(From, To, Amount, #{fsm := FsmI} = I, R, Debug, Cfg) ->
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, From, To, Amount], Debug),
    {I1, _} = await_signing_request(update, I, Debug, Cfg),
    {R1, _} = await_signing_request(update_ack, R, Debug, Cfg),
    check_info(if_debug(Debug, 100, 0), Debug),
    {I1, R1}.

msg_volley(#{fsm := FsmI, pub := PubI} = I, #{fsm := FsmR, pub := PubR} = R, _) ->
    rpc(dev1, aesc_fsm, inband_msg, [FsmI, PubR, <<"ping">>], false),
    {ok,_} = receive_from_fsm(
               message, R,
               fun(#{info := #{info := <<"ping">>}}) -> ok end, 1000, false),
    rpc(dev1, aesc_fsm, inband_msg, [FsmR, PubI, <<"pong">>]),
    {ok,_} = receive_from_fsm(
               message, I,
               fun(#{info := #{info := <<"pong">>}}) -> ok end, 1000, false),
    {I, R}.


deposit(Cfg) ->
    Debug = get_debug(Cfg),
    Deposit = 10,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ct:log("I = ~p", [I]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, Round0 = 1} = check_fsm_state(FsmI),
    check_info(0),
    {ok, I1, R1} = deposit_(I, R, Deposit, Round0, Debug, Cfg),
    check_info(500),
    shutdown_(I1, R1, Cfg),
    ok.

deposit_(#{fsm := FsmI} = I, R, Deposit, Debug, Cfg) ->
    {_IAmt0, _RAmt0, _, Round0} = check_fsm_state(FsmI),
    deposit_(I, R, Deposit, Round0, Debug, Cfg).

deposit_(#{fsm := FsmI} = I, R, Deposit, Round1, Debug, Cfg) ->
    ok = rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => Deposit}]),
    {#{channel_id := ChannelId} = I1, _} = await_signing_request(deposit_tx, I, Cfg),
    {R1, _} = await_signing_request(deposit_created, R, Cfg),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {IAmt0, RAmt0, _, _Round1} = check_fsm_state(FsmI),
    Round2 = Round1 + 1,
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    {ok, Channel} = rpc(dev1, aec_chain, get_channel, [ChannelId]),
    {aesc_deposit_tx, DepositTx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    ChannelRound = aesc_channels:round(Channel),
    DepositRound = aesc_deposit_tx:round(DepositTx),
    ct:log("Channel on-chain round ~p, expected round ~p", [ChannelRound,
                                                            DepositRound]),
    {ChannelRound, ChannelRound} = {ChannelRound, DepositRound},
    {IAmt, RAmt0, StateHash, Round2} = check_fsm_state(FsmI),
    {IAmt, _} = {IAmt0 + Deposit, IAmt}, %% assert correct amounts
    Round2 = aesc_deposit_tx:round(DepositTx), %% assert correct round
    StateHash = aesc_deposit_tx:state_hash(DepositTx), %% assert correct state hash
    ct:log("I1 = ~p", [I1]),
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I1,
    Expected = {IAmt2, RAmt2},
    {Expected, Expected} = {{IAmt0 + Deposit, RAmt0}, Expected},
    SignedTx = await_on_chain_report(I1, #{info => channel_changed}, ?TIMEOUT), % same tx
    SignedTx = await_on_chain_report(R1, #{info => channel_changed}, ?TIMEOUT), % same tx
    {ok, I1, R1}.

withdraw(Cfg) ->
    Debug = get_debug(Cfg),
    Withdrawal = 2,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ct:log("I = ~p", [I]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, _Round0 = 1} = check_fsm_state(FsmI),
    check_info(0),
    ok = rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, #{amount => Withdrawal}]),
    {I1, _} = await_signing_request(withdraw_tx, I, Cfg),
    {R1, _} = await_signing_request(withdraw_created, R, Cfg),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {IAmt0, RAmt0, _, _Round0 = 1} = check_fsm_state(FsmI),
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    {IAmt, RAmt0, StateHash, Round2 = 2} = check_fsm_state(FsmI),
    {IAmt, _} = {IAmt0 - Withdrawal, IAmt}, %% assert correct amounts
    {channel_withdraw_tx, WithdrawalTx} =
        aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    Round2 = aesc_withdraw_tx:round(WithdrawalTx), %% assert correct round
    StateHash = aesc_withdraw_tx:state_hash(WithdrawalTx), %% assert correct state hash
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I1,
    Expected = {IAmt2, RAmt2},
    {Expected, Expected} = {{IAmt0 - Withdrawal, RAmt0}, Expected},
    SignedTx = await_on_chain_report(I1, #{info => channel_changed}, ?TIMEOUT), % same tx
    SignedTx = await_on_chain_report(R1, #{info => channel_changed}, ?TIMEOUT), % same tx
    check_info(500),
    shutdown_(I, R, Cfg),
    ok.

channel_detects_close_solo(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := I, r := R, spec := Spec } = create_channel_([?SLOGAN|Cfg]),
    {ok, Tx} = close_solo_tx(I, <<>>),
    {SignedCloseSoloTx, I1} = sign_tx(I, Tx, Cfg),
    ok = rpc(dev1, aec_tx_pool, push, [SignedCloseSoloTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedCloseSoloTx)),
    mine_blocks_until_txs_on_chain(dev1, [TxHash]),
    LockPeriod = maps:get(lock_period, Spec),
    TTL = current_height(dev1) + LockPeriod,
    ct:log("Expected TTL = ~p", [TTL]),
    SignedTx = await_on_chain_report(I1, #{info => solo_closing}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, #{info => solo_closing}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I1, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun closing/1, ?TIMEOUT, Debug),
    check_info(500),
    settle_(LockPeriod, maps:get(minimum_depth, Spec), I1, R, Debug, Cfg),
    check_info(500),
    ok.

close_solo_tx(#{ fsm        := Fsm
               , channel_id := ChannelId }, Payload) ->
    {ok, #{ round      := Round
          , initiator  := IPubKey
          , responder  := RPubKey
          , round      := Round }} = St = rpc(dev1, aesc_fsm, get_state, [Fsm]),
    ct:log("St = ~p", [St]),
    {ok, PoI} =  rpc(dev1, aesc_fsm, get_poi, [Fsm, [{account, IPubKey},
                                                     {account, RPubKey}]]),
    Nonce =
        case account_type(IPubKey) of
            basic ->
                {ok, N} = rpc(dev1, aec_next_nonce, pick_for_account, [IPubKey]),
                N;
            generalized ->
                0
        end,
    TTL = current_height(dev1) + 100,
    TxSpec = #{ channel_id => aeser_id:create(channel, ChannelId)
              , from_id    => aeser_id:create(account, IPubKey)
              , payload    => Payload
              , poi        => PoI
              , ttl        => TTL
              , fee        => 30000 * aec_test_utils:min_gas_price()
              , nonce      => Nonce },
    {ok, _Tx} = aesc_close_solo_tx:new(TxSpec).

leave_reestablish(Cfg) ->
    with_trace(fun t_leave_reestablish_/1, Cfg, "leave_reestablish").

t_leave_reestablish_(Cfg) ->
    #{i := I, r := R} = leave_reestablish_([?SLOGAN|Cfg]),
    shutdown_(I, R, Cfg),
    ok.

leave_reestablish_(Cfg) ->
    Debug = get_debug(Cfg),
    {I0, R0, Spec0} = channel_spec(Cfg),
    #{ i := #{} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} =
        create_channel_from_spec(I0, R0, Spec0, ?PORT, Debug, Cfg),
    ct:log("I = ~p", [I]),
    ChId = maps:get(channel_id, I),
    LeaveAndReestablish =
        fun(Idx, #{i := ILocal, r := RLocal}) ->
            ct:log("starting attempt ~p", [Idx]),
            Cache1 = cache_status(ChId),
            [_,_] = in_ram(Cache1),
            false = on_disk(Cache1),
            #{fsm := FsmI} = ILocal,
            ok = rpc(dev1, aesc_fsm, leave, [FsmI]),
            {ok,Li} = await_leave(ILocal, ?TIMEOUT, Debug),
            {ok,Lr} = await_leave(RLocal, ?TIMEOUT, Debug),
            SignedTx = maps:get(info, Li),
            SignedTx = maps:get(info, Lr),
            {ok,_} = receive_from_fsm(info, ILocal, fun died_normal/1, ?TIMEOUT, Debug),
            {ok,_} = receive_from_fsm(info, RLocal, fun died_normal/1, ?TIMEOUT, Debug),
            retry(3, 100,
                  fun() ->
                          Cache2 = cache_status(ChId),
                          [] = in_ram(Cache2),
                          true = on_disk(Cache2)
                  end),
            %%
            %% reestablish
            %%
            ChId = maps:get(channel_id, RLocal),
            mine_key_blocks(dev1, 6), % min depth at 4, so more than 4
            ct:log("reestablishing ...", []),
            Res = reestablish(ChId, ILocal, RLocal, SignedTx,
                                           Spec0, ?PORT, Debug),
            ct:log("ending attempt ~p", [Idx]),
            Res
        end,
    lists:foldl(LeaveAndReestablish,
                #{i => I, r => R},
                lists:seq(1, 10)). % 10 iterations

leave_reestablish_close(Cfg) ->
    Debug = get_debug(Cfg),
    #{i := I, r := R, spec := Spec} = leave_reestablish_([?SLOGAN|Cfg]),
    #{initiator := PubI, responder := PubR} = Spec,
    {I1, R1} = do_update(PubI, PubR, 1, I, R, Debug, Cfg),
    ChId = maps:get(channel_id, I1),
    Cache1 = cache_status(ChId),
    [_,_] = in_ram(Cache1),
    false = on_disk(Cache1),
    shutdown_(I1, R1, Cfg),
    retry(3, 100,
          fun() ->
                  Cache2 = cache_status(ChId),
                  []   = in_ram(Cache2),
                  true = on_disk(Cache2)
          end),
    mine_blocks(dev1, 5),
    Cache3 = cache_status(ChId),
    []    = in_ram(Cache3),
    false = on_disk(Cache3).


change_config_get_history(Cfg) ->
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R } = create_channel_([?SLOGAN|Cfg]),
    Log = rpc(dev1, aesc_fsm, get_history, [FsmI]),
    ok = check_history(Log),
    ok = rpc(dev1, aesc_fsm, change_config, [FsmI, log_keep, 17]),
    Status = rpc(dev1, sys, get_status, [FsmI]),
    check_w_param(17,Status),
    {error, invalid_config} =
        rpc(dev1, aesc_fsm, change_config, [FsmI, log_keep, -1]),
    {error, invalid_config} =
        rpc(dev1, aesc_fsm, change_config, [FsmI, invalid, config]),
    shutdown_(I, R, Cfg),
    check_info(500).

check_history(Log) ->
    %% Expected events for initiator so far, in reverse cronological order
    Expected = [{rcv, funding_locked},
                {snd, funding_locked},
                {rcv, channel_changed},
                {rcv, funding_signed},
                {snd, funding_created},
                {rcv, signed},
                {req, sign},
                {rcv, channel_accept},
                {snd, channel_open}],
    ok = check_log(Expected, Log).

check_w_param(N, Status) ->
    match_tuple(fun({w,_,Keep,_,_}) ->
                        N = Keep,  %% crash if no match
                        ok;
                   (_) -> error
               end, Status).

match_tuple(F, Term) when is_tuple(Term) ->
    case F(Term) of
        ok ->
            ok;
        error ->
            match_tuple(F, tuple_to_list(Term))
    end;
match_tuple(F, [H|T]) ->
    case match_tuple(F, H) of
        ok    -> ok;
        error -> match_tuple(F, T)
    end;
match_tuple(_, _) -> error.

get_both_balances(Fsm, PubI, PubR) ->
    {ok, [{PubI, BalI},
          {PubR, BalR}]} = rpc(dev1, aesc_fsm, get_balances, [Fsm, [PubI, PubR]]),
    {BalI, BalR}.

check_log([{Op, Type}|T], [{Op, Type, _, _}|T1]) ->
    check_log(T, T1);
check_log([H|_], [H1|_]) ->
    ct:log("ERROR: Expected ~p in log; got ~p", [H, H1]),
    error(log_inconsistent);
check_log([_|_], []) ->
    %% the log is a sliding window; events may be flushed at the tail
    ok;
check_log([], _) ->
    ok.

died_normal(#{info := {died,normal}}) -> ok.

died_subverted(#{info := {died,_}}) -> ok.

closing(#{info := closing} = Msg) ->
    ct:log("matches #{info := closing} - ~p", [Msg]),
    ok.

multiple_channels(Cfg) ->
    multiple_channels_t(10, 9360, {transfer, 100}, ?SLOGAN, Cfg).

many_chs_msg_loop(Cfg) ->
    multiple_channels_t(10, 9400, {msgs, 100}, ?SLOGAN, Cfg).

multiple_channels_t(NumCs, FromPort, Msg, {slogan, Slogan}, Cfg) ->
    Debug = get_debug(Cfg),
    ct:log("spawning ~p channels", [NumCs]),
    Initiator = maps:get(pub, ?config(initiator, Cfg)),
    ct:log("Initiator: ~p", [Initiator]),
    Me = self(),
    Node = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, NumCs),
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Initiator]),
    Cs = [create_multi_channel([{port, FromPort},
                                {ack_to, Me},
                                {nonce, Nonce + N - 1},
                                {minimum_depth, 0},
                                {slogan, {Slogan,N}} | Cfg], #{mine_blocks => {ask, Me},
                                                 debug => Debug})
          || N <- lists:seq(1, NumCs)],
    ct:log("channels spawned", []),
    CsAcks = collect_acks_w_payload(Cs, mine_blocks, NumCs),
    ct:log("mining requests collected", []),

    Cs  = [C  || {C, _} <- CsAcks],
    Txs = [Tx || {_, Tx} <- CsAcks],

    TxHashes =
        lists:map(
            fun(Tx) ->
                aeser_api_encoder:encode(tx_hash, aetx_sign:hash(Tx))
            end,
            Txs),
    mine_blocks_until_txs_on_chain(dev1, TxHashes),
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    Cs = collect_acks(Cs, channel_ack, NumCs),
    ct:log("channel pids collected: ~p", [Cs]),
    [P ! Msg || P <- Cs],
    T0 = erlang:system_time(millisecond),
    Cs = collect_acks(Cs, loop_ack, NumCs),
    T1 = erlang:system_time(millisecond),
    Time = T1 - T0,
    Transfers = NumCs*2*100,
    Fmt = "Time (~w*2*100) ~.1f s: ~.1f mspt; ~.1f tps",
    Args = [NumCs, Time/1000, Time/Transfers, (Transfers*1000)/Time],
    ct:log(Fmt, Args),
    ct:comment(Fmt, Args),
    [P ! die || P <- Cs],
    ok.

check_incorrect_create(Cfg) ->
    {I, R, Spec} = channel_spec(Cfg),
    config(Cfg),
    Port = proplists:get_value(port, Cfg, ?PORT),
    CreateFun = proplists:get_value(wrong_create, Cfg),
    CreateData = {I, R, Spec, Port, get_debug(Cfg)},
    CreateFun(CreateData, initiator),
    CreateFun(CreateData, responder),
    ok.

wrong_sig_create(Params, Malicious) ->
    wrong_create_(Params, Malicious,
                  fun(SignedTx, _Priv) ->
                      Tx = aetx_sign:tx(SignedTx),
                      _WrongSignedTx = aec_test_utils:sign_tx(Tx, [?BOGUS_PRIVKEY])
                  end,
                  bad_signature).

wrong_hash_create(Params, Malicious) ->
    wrong_create_(Params, Malicious,
                  fun(SignedTx, Priv) ->
                      Tx0 = aetx_sign:tx(SignedTx),
                      Tx = update_tx(Tx0, set_state_hash,
                                      [?BOGUS_PUBKEY]),
                      _SignedTx = aec_test_utils:sign_tx(Tx, [Priv])
                  end,
                  bad_state_hash).

wrong_hash_action(ChannelStuff, Poster, Malicious,
                 FsmStuff) ->
    wrong_hash_action(ChannelStuff, Poster, Malicious,
                  FsmStuff,
                  fun(Who, DebugL) ->
                      {ok, _} = receive_from_fsm(conflict, Who, any_msg(), ?TIMEOUT, DebugL)
                  end).

wrong_hash_action(ChannelStuff, Poster, Malicious, FsmStuff,
                  DetectConflictFun) ->
    wrong_action_modified_tx(ChannelStuff, Poster, Malicious,
                  FsmStuff,
                  DetectConflictFun,
                  fun(Tx0, _Fsm) ->
                      _Tx = update_tx(Tx0, set_state_hash,
                                      [?BOGUS_PUBKEY])
                  end,
                  bad_state_hash).

check_incorrect_deposit(Cfg) ->
    config(Cfg),
    Debug = true,
    Fun = proplists:get_value(wrong_action, Cfg),
    #{ i := I
     , r := R
     , spec := Spec} = create_channel_([?SLOGAN|Cfg]),
    Port = proplists:get_value(port, Cfg, ?PORT),
    Data = {I, R, Spec, Port, Debug},
    Roles = [initiator, responder],
    Deposit =
        fun(Depositor, Malicious) ->
            Fun(Data, Depositor, Malicious,
                {upd_deposit, [#{amount => 1}], deposit_tx, deposit_created})
        end,
    [Deposit(Depositor, Malicious) || Depositor <- Roles,
                                      Malicious <- Roles],
    shutdown_(I, R, Cfg),
    ok.

check_incorrect_withdrawal(Cfg) ->
    config(Cfg),
    Debug = true,
    Fun = proplists:get_value(wrong_action, Cfg),
    #{ i := I
     , r := R
     , spec := Spec} = create_channel_([?SLOGAN|Cfg]),
    Port = proplists:get_value(port, Cfg, ?PORT),
    Data = {I, R, Spec, Port, Debug},
    Roles = [initiator, responder],
    Deposit =
        fun(Depositor, Malicious) ->
            Fun(Data, Depositor, Malicious,
                {upd_withdraw, [#{amount => 1}], withdraw_tx, withdraw_created})
        end,
    [Deposit(Depositor, Malicious) || Depositor <- Roles,
                                      Malicious <- Roles],
    shutdown_(I, R, Cfg),
    ok.

check_incorrect_update(Cfg) ->
    config(Cfg),
    Fun = proplists:get_value(wrong_action, Cfg),
    Test =
        fun(Depositor, Malicious) ->
            Cfg1 = load_idx(Cfg),
            Debug = true,
            #{ i := #{pub := IPub, fsm := FsmI} = I
            , r := #{pub := RPub, fsm := FsmR} = R
            , spec := Spec} = create_channel_([?SLOGAN|Cfg1]),
            Port = proplists:get_value(port, Cfg, ?PORT),
            Data = {I, R, Spec, Port, Debug},
            Deposit =
                fun() ->
                    Fun(Data, Depositor, Malicious,
                        {upd_transfer, [IPub, RPub, 1], update, update_ack})
                end,
            Deposit(),
            AliveFsm =
                case Depositor of
                    initiator -> FsmI;
                    responder -> FsmR
                end,
            ok = gen_statem:stop(AliveFsm),
            timer:sleep(1000), % so all sockets are free
            bump_idx(),
            ok
          end,
    Roles = [initiator, responder],
    [Test(Depositor, Malicious) || Depositor <- Roles,
                                   Malicious <- Roles],
    ok.

check_incorrect_mutual_close(Cfg) ->
    config(Cfg),
    Fun = proplists:get_value(wrong_action_detailed, Cfg),
    Test =
        fun(Depositor, Malicious) ->
            Cfg1 = load_idx(Cfg),
            Debug = true,
            #{ i := I
             , r := R
             , spec := Spec} = create_channel_([?SLOGAN|Cfg1]),
            Port = proplists:get_value(port, Cfg, ?PORT),
            Data = {I, R, Spec, Port, Debug},
            Fun(Data, Depositor, Malicious,
                {shutdown, [], shutdown,
                 shutdown_ack},
                fun(#{fsm := FsmPid}, _Debug) ->
                    timer:sleep(1000),
                    true = rpc(dev1, erlang, process_info, [FsmPid]) =:= undefined
                end),
            bump_idx(),
            ok
          end,
    Roles = [initiator, responder],
    [Test(Depositor, Malicious) || Depositor <- Roles,
                                   Malicious <- Roles],
    ok.

check_mutual_close_with_wrong_amounts(Cfg) ->
    Debug = get_debug(Cfg),
    {Si, Sr, Spec} = channel_spec([{initiator_amount, 5001},
                                   {responder_amount, 5001}, ?SLOGAN | Cfg],
                                  5000, 0),
    Port = proplists:get_value(port, Cfg, 9325),
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R } =
        create_channel_from_spec(Si, Sr, Spec, Port, Debug, Cfg),
    %% We don't have enough funds to cover the closing fee
    {error, insufficient_funds} = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    timer:sleep(1000),
    %% Fsms should be unaffected
    true = (rpc(dev1, erlang, process_info, [FsmI]) =/= undefined),
    true = (rpc(dev1, erlang, process_info, [FsmR]) =/= undefined),
    %% Deposit funds to cover the closing fee. Then it should work
    {ok, I1, R1} = deposit_(I, R, 30000 * aec_test_utils:min_gas_price(),
                              Debug, Cfg),
    shutdown_(I1, R1, Cfg),
    ok.

check_mutual_close_after_close_solo(Cfg) ->
    Debug = get_debug(Cfg),
    {Si, Sr, Spec} = channel_spec([?SLOGAN | Cfg],
                                  5000, 0),
    Spec1 = Spec#{
        timeouts => #{
            idle => 20000,
            sign => 1000,
            accept => 1000
        }
    },
    Port = proplists:get_value(port, Cfg, 9325),
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R } =
        create_channel_from_spec(Si, Sr, Spec1, Port, Debug, Cfg),
    %% One of the parties solo closes the channel
    ok = rpc(dev1, aesc_fsm, close_solo, [FsmI]),
    {_, SignedCloseSoloTx} = await_signing_request(close_solo_tx, I, Cfg),
    wait_for_signed_transaction_in_block(dev1, SignedCloseSoloTx),
    %% Before the lima fork the FSM should make sure that we
    %% cannot shutdown the channel after it was closed but before the TTL runs out
    case aect_test_utils:latest_protocol_version() < ?LIMA_PROTOCOL_VSN of
        true ->
            {error, unknown_request} = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
            {error, unknown_request} = rpc(dev1, aesc_fsm, shutdown, [FsmR]),

            %% Check that a malicious shutdown request does not kill the FSM
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmI, false]),
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
            {_, _} = await_signing_request(shutdown, I, Cfg),

            %% TODO: Check if we receive an ?UpdateErr message
            channel_closing = fsm_state(FsmR, Debug),
            timer:sleep(1100), %% For now just wait for a timeout
            channel_closing = fsm_state(FsmI, Debug);
        false ->
            % Test that timeouts do not kill the FSM
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmR]),
            timer:sleep(1100),
            channel_closing = fsm_state(FsmI, Debug),
            channel_closing = fsm_state(FsmR, Debug),

            % Test that after sending the SHUTDOWN message and timing out we
            % are still alive
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
            {_, _} = await_signing_request(shutdown, I, Cfg),
            timer:sleep(1100),
            channel_closing = fsm_state(FsmI, Debug),
            channel_closing = fsm_state(FsmR, Debug),

            % Test that closing works
            check_info(500),
            shutdown_(I, R, Cfg)
    end,
    ok.

fsm_state(Pid, Debug) ->
    {State, _Data} = rpc(dev1, sys, get_state, [Pid, 1000], _RpcDebug = false),
    log(Debug, "fsm_state(~p) -> ~p", [Pid, State]),
    State.

wrong_sig_action(ChannelStuff, Poster, Malicious,
                 FsmStuff) ->
    wrong_sig_action(ChannelStuff, Poster, Malicious,
                  FsmStuff,
                  fun(Who, DebugL) ->
                      {ok, _} = receive_from_fsm(conflict, Who, any_msg(), ?TIMEOUT, DebugL)
                  end).

wrong_sig_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                 {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                  DetectConflictFun) ->
    Cfg = config(),
    wrong_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                 {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                  DetectConflictFun,
                  fun(Action, Signer, Debug1) ->
                      {_, _WrongSignedTx} =
                          await_signing_request_basic(Action,
                                                      Signer#{priv => ?BOGUS_PRIVKEY},
                                                      Debug1, Cfg)
                  end,
                  bad_signature).

wrong_id_action(ChannelStuff, Poster, Malicious,
                 FsmStuff) ->
    wrong_id_action(ChannelStuff, Poster, Malicious,
                  FsmStuff,
                  fun(Who, DebugL) ->
                      {ok, _} = receive_from_fsm(conflict, Who, any_msg(), ?TIMEOUT, DebugL)
                  end).

wrong_id_action(ChannelStuff, Poster, Malicious,
                FsmStuff, DetectConflictFun) ->
    wrong_action_modified_tx(ChannelStuff, Poster, Malicious,
                  FsmStuff,
                  DetectConflictFun,
                  fun(Tx0, _Fsm) ->
                      _Tx = update_tx(Tx0, set_channel_id,
                                      [aeser_id:create(channel, ?BOGUS_PUBKEY)])
                  end,
                  different_channel_id).

wrong_round_action(ChannelStuff, Poster, Malicious,
                 FsmStuff, ModifyRound) ->
    wrong_round_action(ChannelStuff, Poster, Malicious,
                  FsmStuff,
                  fun(Who, DebugL) ->
                      {ok, _} = receive_from_fsm(conflict, Who, any_msg(), ?TIMEOUT, DebugL)
                  end, ModifyRound).

wrong_round_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                   {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                    DetectConflictFun, ModifyRoundFun) ->
    wrong_action_modified_tx({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                 {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                  DetectConflictFun,
                  fun(Tx0, Fsm) ->
                      {ok, Round} = aesc_fsm:get_round(Fsm),
                      _Tx = update_tx(Tx0, set_round, [ModifyRoundFun(Round)])
                  end,
                  wrong_round).

wrong_create_({I, R, #{initiator_amount := IAmt0, responder_amount := RAmt0,
              push_amount := PushAmount} = Spec, Port, Debug},
              Malicious, SignTxFun, ErrResponse) ->
    Cfg = config(),
    Action = fun sign_signing_request/4,
    TryCheating =
        fun(Tag, #{priv := Priv} = Signer, Debug1) ->
            receive {aesc_fsm, Fsm, #{type := sign, tag := Tag,
                                      info := #{signed_tx := SignedTx0, updates := Updates}} = Msg} ->
                    log(Debug1, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
                    SignedTx = SignTxFun(SignedTx0, Priv),
                    Action(Tag, Signer, SignedTx, Updates)
            after ?TIMEOUT ->
                    error(timeout)
            end
        end,
    IAmt = IAmt0 - PushAmount,
    RAmt = RAmt0 + PushAmount,
    {ok, FsmR} = rpc(dev1, aesc_fsm, respond, [Port, Spec], Debug),
    {ok, FsmI} = rpc(dev1, aesc_fsm, initiate, ["localhost", Port, Spec], Debug),

    log(Debug, "FSMs, I = ~p, R = ~p", [FsmI, FsmR]),

    I1 = I#{fsm => FsmI, initiator_amount => IAmt, responder_amount => RAmt},
    R1 = R#{fsm => FsmR, initiator_amount => IAmt, responder_amount => RAmt},

    {ok, _} = receive_from_fsm(info, R1, channel_open, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(info, I1, channel_accept, ?TIMEOUT, Debug),

    case Malicious of
        initiator ->
            % default behavor - FSM guards you from sending a bad msg
            {_, WrongTx} = TryCheating(create_tx, I1, Debug),
            {ok, _} = receive_from_fsm(error, I1, ErrResponse, ?TIMEOUT, Debug),

            % turn default behavior off, the initator deliberatly had sent
            % invalid tx, the responder must reject it
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmI, false], Debug),

            % resend the same wrong tx, this time no check from initiator's
            % side
            aesc_fsm:signing_response(FsmI, create_tx, WrongTx),
            {ok,_} = receive_from_fsm(info, R1, ErrResponse, ?TIMEOUT, Debug),
            {ok,_} = receive_from_fsm(info, R1, fun(#{info := {died, normal}}) -> ok end,
                                      ?TIMEOUT, Debug),
            {ok,_} = receive_from_fsm(info, I1, fun died_subverted/1, ?TIMEOUT, Debug);
        responder ->
            {_I2, _} = await_signing_request(create_tx, I1, Debug, Cfg),
            receive_from_fsm(info, R1, funding_created, ?TIMEOUT, Debug),
            % default behavor - FSM guards you from sending a bad tx
            {_, WrongTx} = TryCheating(funding_created, R1, Debug),
            %% since this is a different tx now, the correct signature of the
            %% incoming transaction will be invalid according to the incorrect
            %% new tx
            {ok, _} = receive_from_fsm(error, R1, bad_signature, ?TIMEOUT, Debug),

            % turn default behavior off, the responder deliberatly had sent
            % invalid tx, the initiator must reject it
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmR, false], Debug),

            % resend the same wrong tx, this time no check from responder's
            % side; the transaction has bad signature as it had changed
            aesc_fsm:signing_response(FsmR, funding_created, WrongTx),
            {ok,_} = receive_from_fsm(info, I1, bad_signature, ?TIMEOUT, Debug),
            {ok,_} = receive_from_fsm(info, I1, fun(#{info := {died, normal}}) -> ok end,
                                      ?TIMEOUT, Debug),
            {ok,_} = receive_from_fsm(info, R1, fun died_subverted/1, ?TIMEOUT, Debug)
    end,
    ok.

wrong_action_modified_tx({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                   {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                    DetectConflictFun, ModifyTxFun, ErrorMsg) ->
    Cfg = config(),
    wrong_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                 {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                  DetectConflictFun,
                  fun(Tag, Signer, Debug1) ->
                      Action = fun sign_signing_request/4,
                      receive {aesc_fsm, Fsm, #{type := sign, tag := Tag,
                                                info := #{signed_tx := SignedTx0,
                                                          updates := Updates}} = Msg} ->
                              log(Debug1, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
                              Tx0 = aetx_sign:innermost_tx(SignedTx0),
                              Tx = ModifyTxFun(Tx0, Fsm),
                              log(Debug1, "modified ~p", [Tx]),
                              %% Note: this invalidates the authentication method of
                              %% the initial signer
                              SignedTx1 = aetx_sign:set_tx(SignedTx0, Tx),
                              {SignedTx, _} = co_sign_tx(Signer, SignedTx1, Cfg),
                              Action(Tag, Signer, SignedTx, Updates)
                      after ?TIMEOUT ->
                              error(timeout)
                      end
                  end,
                  ErrorMsg).

wrong_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
              {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
               DetectConflictFun, MaliciousSign, ErrMsg) ->
    Cfg = config(),
    ct:log("Testing with Poster ~p, Malicious ~p",
          [Poster, Malicious]),
    #{fsm := FsmI} = I,
    #{fsm := FsmR} = R,
    {D, A, FsmD, FsmA} =
        case Poster of
            initiator -> {I, R, FsmI, FsmR};
            responder -> {R, I, FsmR, FsmI}
        end,
    ok = rpc(dev1, aesc_fsm, FsmFun, [FsmD | FsmFunArg]),
    case Poster =:= Malicious of
        true ->
            % default behavor - FSM guards you from sending a bad event
            {_, WrongSignedTx} = MaliciousSign(FsmNewAction, D, Debug),
            {ok, _} = receive_from_fsm(error, D, ErrMsg, ?TIMEOUT, Debug),

            % turn default behavior off, the poster deliberatly had sent
            % invalid tx, the acknowledger must reject it
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmD, false], Debug),

            % resend the same wrong tx, this time no check from poster's
            % side
            aesc_fsm:signing_response(FsmD, FsmNewAction, WrongSignedTx),

            DetectConflictFun(D, Debug),
            % make sure setting back defaults if process is still there
            rpc(dev1, aesc_fsm, strict_checks, [FsmD, true], Debug);
        false ->
            {_, _} = await_signing_request(FsmNewAction, D, Debug, Cfg),
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmA, false], Debug),
            {_, _} = MaliciousSign(FsmCreatedAction, A, Debug),
            DetectConflictFun(A, Debug),
            rpc(dev1, aesc_fsm, strict_checks, [FsmA, true], Debug)
    end,
    check_info(500),
    ok.

shutdown_(#{fsm := FsmI, channel_id := ChannelId} = I, R, Cfg) ->
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    {I1, _} = await_signing_request(shutdown, I, Cfg),
    {R1, _} = await_signing_request(shutdown_ack, R, Cfg),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx),
    verify_close_mutual_tx(SignedTx, ChannelId),
    ok.

settle_(TTL, MinDepth, #{fsm := FsmI, channel_id := ChannelId} = I, R, Debug,
       Cfg) ->
    ok = rpc(dev1, aesc_fsm, settle, [FsmI]),
    {_, SignedTx} = await_signing_request(settle_tx, I, Cfg),
    ct:log("settle_tx signed", []),
    {ok, MinedKeyBlocks} = mine_blocks_until_txs_on_chain(
                             dev1,
                             [aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))]),
    KeyBlocksMissingForTTL = (TTL + 1) - length(MinedKeyBlocks),
    KeyBlocksMissingForMinDepth =
        if
            KeyBlocksMissingForTTL > 0 ->
                mine_key_blocks(dev1, KeyBlocksMissingForTTL),
                MinDepth;
            KeyBlocksMissingForTTL =< 0 ->
                MinDepth + KeyBlocksMissingForTTL
        end,
    SignedTx = await_on_chain_report(I, #{info => channel_closed}, ?TIMEOUT), % same tx
    ct:log("I received On-chain report: ~p", [SignedTx]),
    SignedTx = await_on_chain_report(R, #{info => channel_closed}, ?TIMEOUT), % same tx
    ct:log("R received On-chain report: ~p", [SignedTx]),
    verify_settle_tx(SignedTx, ChannelId),
    ct:log("settle_tx verified", []),
    if
        KeyBlocksMissingForMinDepth > 0 ->
            mine_key_blocks(dev1, KeyBlocksMissingForMinDepth);
        KeyBlocksMissingForMinDepth =< 0 ->
            ok
    end,
    {ok, _} = receive_from_fsm(info, I, closed_confirmed, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(info, R, closed_confirmed, ?TIMEOUT, Debug),
    ct:log("closed_confirmed received from both", []),
    {ok,_} = receive_from_fsm(info, I, fun died_normal/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun died_normal/1, ?TIMEOUT, Debug),
    ct:log("died_normal detected from both", []),
    ok.

client_reconnect_initiator(Cfg) ->
    with_trace(fun(Cfg1) -> client_reconnect_(initiator, Cfg1) end, Cfg, "client_reconnect_i").

client_reconnect_responder(Cfg) ->
    client_reconnect_(responder, Cfg).

client_reconnect_(Role, Cfg) ->
    Debug = get_debug(Cfg),

    #{ i := I, r := R } = Ch = create_channel_([?SLOGAN | Cfg]),
    MapKey = case Role of
                 initiator -> i;
                 responder -> r
             end,
    #{ fsm := Fsm, proxy := Proxy } = RoleI = maps:get(MapKey, Ch),
    ct:log("Ch = ~p", [Ch]),
    {error, _} = Err = try_reconnect(Fsm, Role, RoleI, Debug),
    log(Debug, "Reconnecting before disconnecting failed: ~p", [Err]),
    unlink(Proxy),
    exit(Proxy, kill),
    timer:sleep(100),  % give the above exit time to propagate
    ok = things_that_should_fail_if_no_client(Role, I, R, Debug, Cfg),
    Res = reconnect(Fsm, Role, RoleI, Debug),
    ct:log("Reconnect req -> ~p", [Res]),
    %% run tests here
    shutdown_(I, R, Cfg).

things_that_should_fail_if_no_client(ClientRole, I, R, Debug, Cfg) ->
    update_req_conflict(ClientRole, I, R, Debug, Cfg).

update_req_conflict(Role, I, R, Debug, Cfg) ->
    %% Note that the A and B represent I and R, possibly switched, to
    %% indicate who should initiate the update request. Here, A will be
    %% the side that currently has no client.
    {#{ pub := PubA, fsm := FsmA } = A,
     #{ pub := PubB, fsm := FsmB } = B}
        = roles_for_update(Role, I, R),
    %% cannot request an update from the side with client disconnected
    {error, client_disconnected}
        = rpc(dev1, aesc_fsm, upd_transfer, [FsmA, PubA, PubB, 1]),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmB, PubB, PubA, 2]),
    {B1, _} = await_signing_request(update, B, Debug, Cfg),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(conflict, B1, any_msg(), ?TIMEOUT, Debug),
    %% Try now with a co-signed update request
    %% This should work
    rpc(dev1, aesc_fsm, upd_transfer, [FsmB, PubB, PubA, 3]),
    {B3, _} = await_signing_request_multisig(update, B, [A], Debug, Cfg),
    {ok, _} = receive_from_fsm(update, B3, any_msg(), ?TIMEOUT, Debug),
    ok.

roles_for_update(initiator, I, R) -> {I, R};
roles_for_update(responder, I, R) -> {R, I}.

reconnect(Fsm, Role, #{} = R, Debug) ->
    Me = self(),
    NewProxy = spawn(fun() ->
                             erlang:monitor(process, Me),
                             ok = try_reconnect(Fsm, Role, R, Debug),
                             log(Debug, "Reconnect successful; Fsm = ~p", [Fsm]),
                             Me ! {self(), reconnected},
                             fsm_relay(R, Me, Debug)
                     end),
    receive
        {NewProxy, reconnected} -> ok
    after 1000 ->
            error(timeout)
    end,
    R#{ proxy => NewProxy }.

try_reconnect(Fsm, Role, R, Debug) ->
    try_reconnect(Fsm, 1, Role, R, Debug).

try_reconnect(Fsm, Round, Role, #{ channel_id := ChId
                                 , fsm  := Fsm
                                 , pub  := Pub
                                 , priv := Priv}, Debug) ->
    ChIdId = aeser_id:create(channel, ChId),
    PubId = aeser_id:create(account, Pub),
    {ok, Tx} = aesc_client_reconnect_tx:new(#{ channel_id => ChIdId
                                             , round      => Round
                                             , role       => Role
                                             , pub_key    => PubId }),
    log(Debug, "Reconnect Tx = ~p", [Tx]),
    SignedTx = aec_test_utils:sign_tx(Tx, Priv),
    rpc(dev1, aesc_fsm, reconnect_client, [Fsm, self(), SignedTx]).


%% Retry N times, T ms apart, if F() raises an exception.
%% Used in places where there could be a race.
%%
retry(N, T, F) ->
    retry(N, T, F, undefined, []).

retry(0, _, _, E, T) ->
    error(E, T);
retry(N, T, F, _, _) when N > 0 ->
    try F()
    catch
        error:E ->
            timer:sleep(T),
            retry(N-1, T, F, E, erlang:get_stacktrace())
    end.


cache_status(ChId) ->
    rpc(dev1, aesc_state_cache, cache_status, [ChId]).

in_ram(St)  ->  proplists:get_value(in_ram, St).
on_disk(St) ->  proplists:get_value(on_disk, St).


collect_acks([Pid | Pids], Tag, N) ->
    ct:log("collect_acks, Tag = ~p, N = ~p", [Tag, N]),
    Timeout = 60000 + (N div 10)*5000,  % wild guess
    receive
        {Pid, Tag} ->
            ct:log("Ack from ~p (~p)", [Pid, Tag]),
            [Pid | collect_acks(Pids, Tag, N)]
    after Timeout ->
            error(timeout)
    end;
collect_acks([], _Tag, _) ->
    [].

collect_acks_w_payload([Pid | Pids], Tag, N) ->
    ct:log("collect_acks_w_payload, Tag = ~p, N = ~p", [Tag, N]),
    Timeout = 30000 + (N div 10)*5000,  % wild guess
    receive
        {Pid, Tag, Payload} ->
            ct:log("Ack from ~p (~p)", [Pid, Tag]),
            [{Pid, Payload} | collect_acks_w_payload(Pids, Tag, N)]
    after Timeout ->
            error(timeout)
    end;
collect_acks_w_payload([], _Tag, _) ->
    [].


create_multi_channel(Cfg, Debug) ->
    create_multi_channel(Cfg, Debug, true).

create_multi_channel(Cfg, Debug, UseAny) ->
    spawn_link(fun() ->
                       create_multi_channel_(Cfg, Debug, UseAny)
               end).

create_multi_channel_(Cfg0, Debug, UseAny) when is_boolean(UseAny) ->
    Cfg = if UseAny -> [ use_any | Cfg0 ];
             true   -> Cfg0
          end,
    #{i := I, r := R} = create_channel_(Cfg, Debug),
    Parent = ?config(ack_to, Cfg),
    Parent ! {self(), channel_ack},
    ch_loop(I, R, Parent, Cfg).

ch_loop(I, R, Parent, Cfg) ->
    receive
        {transfer, N} ->
            {_, I1, R1} = do_n(N, fun update_volley/3, I, R, Cfg),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg);
        {msgs, N} ->
            {_, I1, R1} = do_n(N, fun msg_volley/3, I, R, Cfg),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg);
        die ->
            ct:log("~p got die request", [self()]),
            #{ proxy := ProxyI } = I,
            #{ proxy := ProxyR } = R,
            ProxyI ! {self(), die},
            ProxyR ! {self(), die},
            exit(normal);
        Other ->
            ct:log("Got Other = ~p, I = ~p~nR = ~p", [Other, I, R]),
            ch_loop(I, R, Parent, Cfg)
    end.

create_channel_on_port(Port) ->
    Node = dev1,
    I = prep_initiator(Node),
    R = prep_responder(I, Node),
    Cfg = [{port, Port}, {initiator, I}, {responder, R},
           {initiator_amount, 500000}, {responder_amount, 500000}, ?SLOGAN],
    create_channel_(Cfg, get_debug(Cfg)).

create_channel_(Cfg) ->
    create_channel_(Cfg, get_debug(Cfg)).

create_channel_(Cfg, Debug) ->
    {I, R, Spec} = channel_spec(Cfg),
    log(Debug, "channel_spec: ~p", [{I, R, Spec}]),
    Port = proplists:get_value(port, Cfg, 9325),
    create_channel_from_spec(I, R, Spec, Port, proplists:get_bool(use_any, Cfg),
                             Debug, Cfg).

channel_spec(Cfg) ->
    PushAmount = proplists:get_value(push_amount, Cfg, 200000),
    ChannelReserve = proplists:get_value(channel_reserve, Cfg, 300000),
    channel_spec(Cfg, ChannelReserve, PushAmount).

channel_spec(Cfg, ChannelReserve, PushAmount) ->
    I = ?config(initiator, Cfg),
    R = ?config(responder, Cfg),

    %% dynamic key negotiation
    Proto = <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>,

    IAmt = ?config(initiator_amount, Cfg),
    RAmt = ?config(responder_amount, Cfg),
    Spec = #{initiator        => maps:get(pub, I),
             responder        => maps:get(pub, R),
             initiator_amount => IAmt,
             responder_amount => RAmt,
             push_amount      => PushAmount,
             lock_period      => 10,
             channel_reserve  => ChannelReserve,
             minimum_depth    => config(minimum_depth, Cfg, ?MINIMUM_DEPTH),
             client           => self(),
             noise            => [{noise, Proto}],
             timeouts         => #{idle => 20000},
             slogan           => slogan(Cfg),
             report           => #{debug => true} },
    Spec1 = case ?config(nonce, Cfg) of
                undefined -> Spec;
                Nonce     -> Spec#{nonce => Nonce}
            end,
    {I, R, Spec1}.

config(K, Cfg, Def) ->
    case ?config(K, Cfg) of
        undefined -> Def;
        Other     -> Other
    end.

slogan(Cfg) ->
    ?config(slogan, Cfg).

create_channel_from_spec(I, R, Spec, Port, Debug, Cfg) ->
    create_channel_from_spec(I, R, Spec, Port, false, Debug, Cfg).

create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg) ->
    RProxy = spawn_responder(Port, Spec, R, UseAny, Debug),
    IProxy = spawn_initiator(Port, Spec, I, Debug),
    log("RProxy = ~p, IProxy = ~p", [RProxy, IProxy]),
    #{ i := #{ fsm := FsmI } = I1
     , r := #{ fsm := FsmR } = R1 } = Info
        = match_responder_and_initiator(RProxy, Debug),
    log(Debug, "channel paired: ~p", [Info]),

    log(Debug, "FSMs, I = ~p, R = ~p", [FsmI, FsmR]),

    {I2, R2} = try await_create_tx_i(I1, R1, Debug, Cfg)
               catch
                   error:Err ->
                       log("Caught Err = ~p~nMessages = ~p",
                           [Err, element(2, process_info(self(), messages))]),
                       error(Err, erlang:get_stacktrace())
               end,
    log(Debug, "mining blocks on dev1 for minimum depth", []),
    CurrentHeight = current_height(dev1),
    SignedTx = await_on_chain_report(I2, ?TIMEOUT),
    ct:log("SignedTx = ~p", [SignedTx]),
    SignedTx = await_on_chain_report(R2, ?TIMEOUT), % same tx
    ok = wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    mine_blocks(dev1, ?MINIMUM_DEPTH, opt_add_tx_to_debug(SignedTx, Debug)),
    %% in case of multiple channels starting in parallel - the mining above
    %% has no effect (the blocks are mined in another process)
    %% The following line makes sure this process is blocked until the proper
    %% height is reached
    aecore_suite_utils:wait_for_height(aecore_suite_utils:node_name(dev1),
                                       CurrentHeight + ?MINIMUM_DEPTH),
    %% we've seen 10-15 second block times in CI, so wait a while longer

    % check the channel is present on-chain
    {ok, ChannelId} = aesc_utils:channel_pubkey(SignedTx),
    {ok, _} = rpc(dev1, aec_chain, get_channel, [ChannelId]),

    {ok, _} = receive_from_fsm(info, R2, own_funding_locked, ?LONG_TIMEOUT, Debug),
    %% shouldn't be necessary to use ?LONG_TIMEOUT again
    {ok, _} = receive_from_fsm(info, I2, own_funding_locked, ?TIMEOUT, Debug),
    I3 = await_funding_locked(I2, ?TIMEOUT, Debug),
    R3 = await_funding_locked(R2, ?TIMEOUT, Debug),
    I4 = await_update(I3, ?TIMEOUT, Debug),
    R4 = await_update(R3, ?TIMEOUT, Debug),
    I5 = await_open_report(I4, ?TIMEOUT, Debug),
    R5 = await_open_report(R4, ?TIMEOUT, Debug),
    check_info(500, Debug),
    #{i => I5, r => R5, spec => Spec}.

spawn_responder(Port, Spec, R, UseAny, Debug) ->
    Me = self(),
    spawn_link(fun() ->
                       log("responder spawned: ~p", [Spec]),
                       Spec1 = maybe_use_any(UseAny, Spec#{ client => self() }),
                       {ok, Fsm} = rpc(dev1, aesc_fsm, respond, [Port, Spec1], Debug),
                       responder_instance_(Fsm, Spec1, R, Me, Debug)
               end).

maybe_use_any(true, Spec) ->
    Spec#{initiator => any};
maybe_use_any(false, Spec) ->
    Spec.

spawn_initiator(Port, Spec, I, Debug) ->
    Me = self(),
    spawn_link(fun() ->
                       log("initiator spawned: ~p", [Spec]),
                       Spec1 = Spec#{ client => self() },
                       {ok, Fsm} = rpc(dev1, aesc_fsm, initiate,
                                       ["localhost", Port, Spec1], Debug),
                       initiator_instance_(Fsm, Spec1, I, Me, Debug)
               end).

match_responder_and_initiator(RProxy, Debug) ->
    receive
        {channel_up, RProxy, Info} ->
            log(Debug, "Matched initiator/responder pair: ~p", [Info]),
            Info
    after ?TIMEOUT ->
            log(Debug, "Timed out waiting for matched pair", []),
            error(timeout)
    end.

responder_instance_(Fsm, Spec, R0, Parent, Debug) ->
    R = fsm_map(Fsm, Spec, R0),
    {ok, ChOpen} = receive_from_fsm(info, R, channel_open, ?TIMEOUT, Debug),
    ct:log("Got ChOpen: ~p~nSpec = ~p", [ChOpen, Spec]),
    #{data := #{temporary_channel_id := TmpChanId}} = ChOpen,
    R1 = R#{ proxy => self(), parent => Parent },
    gproc:reg({n,l,{?MODULE,TmpChanId,responder}}, #{ r => R1 }),
    {_IPid, #{ i := I1
            , channel_accept := ChAccept }}
        = gproc:await({n,l,{?MODULE,TmpChanId,initiator}}, ?TIMEOUT),
    Parent ! {channel_up, self(), #{ i => I1#{parent => Parent}
                                   , r => R1
                                   , channel_accept => ChAccept
                                   , channel_open   => ChOpen }},
    fsm_relay(R, Parent, Debug).

initiator_instance_(Fsm, Spec, I0, Parent, Debug) ->
    I = fsm_map(Fsm, Spec, I0),
    {ok, ChAccept} = receive_from_fsm(info, I, channel_accept, ?TIMEOUT, Debug),
    ct:log("Got ChAccept: ~p~nSpec = ~p", [ChAccept, Spec]),
    #{data := #{temporary_channel_id := TmpChanId}} = ChAccept,
    I1 = I#{ proxy => self() },
    gproc:reg({n,l,{?MODULE,TmpChanId,initiator}}, #{ i => I1
                                                    , channel_accept => ChAccept}),
    {_RPid, #{ r := #{parent := NewParent}}}
        = gproc:await({n,l,{?MODULE,TmpChanId,responder}}, ?TIMEOUT),
    unlink(Parent),
    link(NewParent),
    fsm_relay(I1#{parent => NewParent}, NewParent, Debug).

fsm_relay(Map, Parent, Debug) ->
    log(Debug, "fsm_relay(~p, ~p, Debug)", [Map, Parent]),
    fsm_relay_(Map, Parent, Debug).

fsm_relay_(#{ fsm := Fsm } = Map, Parent, Debug) ->
    receive
        {aesc_fsm, Fsm, _} = Msg ->
            log(Debug, "Relaying(~p) ~p", [Parent, Msg]),
            Parent ! Msg;
        {Parent, die} ->
            ct:log("Got 'die' from parent", []),
            aesc_fsm:stop(Fsm),
            ct:log("relay stopping (die)", []),
            exit(normal);
        Other ->
            log(Debug, "Relay got Other: ~p", [Other])
    end,
    fsm_relay_(Map, Parent, Debug).

fsm_map(Fsm, #{ initiator_amount := IAmt
              , responder_amount := RAmt
              , push_amount      := PushAmt
              , slogan           := Slogan }, Map) ->
    Map#{ fsm    => Fsm
        , slogan => Slogan
        , initiator_amount => IAmt - PushAmt
        , responder_amount => RAmt + PushAmt }.

reestablish(ChId, I0, R0, SignedTx, Spec0, Port, Debug) ->
    {IAmt, RAmt} = tx_amounts(SignedTx),
    I = set_amounts(IAmt, RAmt, I0),
    R = set_amounts(IAmt, RAmt, R0),
    Spec = Spec0#{existing_channel_id => ChId, offchain_tx => SignedTx,
                  initiator_amount => IAmt, responder_amount => RAmt},
    {ok, FsmR} = rpc(dev1, aesc_fsm, respond,
                     [Port, Spec]),
    {ok, FsmI} = rpc(dev1, aesc_fsm, initiate,
                     ["localhost", Port, Spec], Debug),
    I1 = await_open_report(I#{fsm => FsmI}, ?TIMEOUT, Debug),
    R1 = await_open_report(R#{fsm => FsmR}, ?TIMEOUT, Debug),
    check_info(500),
    #{i => I1, r => R1, spec => Spec}.

tx_amounts(SignedTx) ->
    {Mod, TxI} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    {Mod:initiator_amount(TxI),
     Mod:responder_amount(TxI)}.

update_tx(Tx0, F, Args) ->
    {Mod, TxI} = aetx:specialize_callback(Tx0),
    TxI1 = apply(Mod, F, [TxI|Args]),
    aetx:new(Mod, TxI1).

set_amounts(IAmt, RAmt, Map) ->
    Map#{initiator_amount => IAmt, responder_amount => RAmt}.

verify_close_mutual_tx(SignedTx, ChannelId) ->
    {aesc_close_mutual_tx, Tx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    {_, ChInfo} = aesc_close_mutual_tx:serialize(Tx),
    true = lists:member(ChannelId,
        [ aeser_id:specialize(ChId, channel)||
             {channel_id, ChId} <- ChInfo
        ]).

verify_settle_tx(SignedTx, ChannelId) ->
    {channel_settle_tx, Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    {_, ChInfo} = aesc_settle_tx:serialize(Tx),
    true = lists:member(ChannelId,
        [ aeser_id:specialize(ChId, channel) ||
            {channel_id, ChId} <- ChInfo
        ]).

await_create_tx_i(I, R, Debug, Cfg) ->
    {I1, _} = await_signing_request(create_tx, I, Debug, Cfg),
    await_funding_created_p(I1, R, Debug, Cfg).

await_funding_created_p(I, R, Debug, Cfg) ->
    receive_from_fsm(info, R, funding_created, ?TIMEOUT, Debug),
    {R1, _} = await_signing_request(funding_created, R, Debug, Cfg),
    await_funding_signed_i(I, R1, Debug).

await_funding_signed_i(I, R, Debug) ->
    receive_info(I, funding_signed, Debug),
    {I, R}.

await_funding_locked(#{role := Role} = R, Timeout, Debug) ->
    {ok, Msg} = receive_from_fsm(info, R, funding_locked, Timeout, Debug),
    log(Debug, "~p got funding_locked: ~p", [Role, Msg]),
    R#{channel_id => maps:get(channel_id, Msg)}.

await_update(#{channel_id := ChId} = R, Timeout, Debug) ->
    {ok, Msg} = receive_from_fsm(
                  update, R,
                  fun(#{ channel_id := ChId1
                         , info := SignedTx }) ->
                          true =
                              ChId1 == ChId
                              andalso
                              element(1, SignedTx) == signed_tx
                  end, Timeout, Debug),
    #{info := SignedTx} = Msg,
    R#{signed_tx => SignedTx}.

await_signing_request(Tag, R, Cfg) ->
    await_signing_request(Tag, R, ?TIMEOUT, true, Cfg).

await_signing_request(Tag, R, Debug, Cfg) ->
    await_signing_request(Tag, R, ?TIMEOUT, Debug, Cfg).

await_signing_request_multisig(Tag, R, Sigs, Debug, Cfg) ->
    Action = fun sign_signing_request/4,
    await_signing_request(Tag, R, Sigs, Action, ?TIMEOUT, Debug, Cfg, according_account).

await_signing_request_basic(Tag, Signer, Debug, Cfg) ->
    Action = fun sign_signing_request/4,
    await_signing_request(Tag, Signer,
                           Action, ?TIMEOUT, Debug, Cfg, basic).

await_signing_request(Tag, R, Timeout, Debug, Cfg) ->
    Action = fun sign_signing_request/4,
    await_signing_request(Tag, R, Action, Timeout, Debug, Cfg).

await_signing_request(Tag, Signer, Action, Timeout, Debug, Cfg) ->
    await_signing_request(Tag, Signer, Action, Timeout, Debug, Cfg, according_account).

await_signing_request(Tag, Signer, Action, Timeout, Debug, Cfg, SignatureType) ->
    await_signing_request(Tag, Signer, [], Action, Timeout, Debug, Cfg, SignatureType).

await_signing_request(Tag, #{fsm := Fsm, pub := Pub} = Signer, OtherSigs,
                      Action, Timeout, Debug, Cfg, SignatureType) ->
    log("await_signing_request, Fsm = ~p (Pub = ~p, Other = ~p)",
        [Fsm, Pub, [P || #{pub := P} <- OtherSigs]]),
    receive {aesc_fsm, Fsm, #{type := sign, tag := Tag,
                              info := #{signed_tx := SignedTx0,
                                        updates   := Updates}} = Msg} ->
            log(Debug, "await_signing(~p, ~p, ~p, ~p) <- ~p",
                [Tag, Pub, [P || #{pub := P} <- OtherSigs], Fsm, Msg]),
            {[Signer1|_], SignedTx} =
                lists:mapfoldl(
                  fun(S, STx) ->
                          {STx1, S1} =
                              case SignatureType of
                                  according_account -> co_sign_tx(S, STx, Cfg);
                                  basic ->
                                      #{priv := Priv} = S,
                                      {aec_test_utils:co_sign_tx(STx, Priv), S}
                              end,
                          {S1, STx1}
                  end, SignedTx0, [Signer|OtherSigs]),
            log(Debug,"SIGNED TX ~p", [SignedTx]),
            Action(Tag, Signer1, SignedTx, Updates)
    after Timeout ->
            error(timeout)
    end.

sign_tx(Signer, Tx, Cfg) ->
    co_sign_tx(Signer, aetx_sign:new(Tx, []), Cfg).

co_sign_tx(Signer, SignedTx, Cfg) ->
    #{role := Role, pub := Pubkey, priv := Priv} = Signer,
    case account_type(Pubkey) of
        basic ->
            {aec_test_utils:co_sign_tx(SignedTx, Priv), Signer};
        generalized ->
            #{auth_idx := N} = Signer,
            #{auth_params := Auths} = ?config(ga, Cfg),
            AuthOpts = maps:get(Role, Auths),
            {meta(Pubkey, AuthOpts, N + 1, SignedTx), Signer#{auth_idx => N + 1}}
    end.


sign_signing_request(Tag, #{fsm := Fsm} = R, SignedTx, Updates) ->
    aesc_fsm:signing_response(Fsm, Tag, SignedTx),
    {check_amounts(R, SignedTx, Updates), SignedTx}.

await_on_chain_report(R, Timeout) ->
    await_on_chain_report(R, #{}, Timeout).

await_on_chain_report(#{fsm := Fsm}, Match, Timeout) ->
    ct:log("~p awaiting on-chain from ~p", [self(), Fsm]),
    receive
        {aesc_fsm, Fsm, #{info := {died, _}}} = Died ->
            ct:log("Fsm died while waiting for on-chain report:~n"
                   "~p", [Died]),
            error(fsm_died);
        {aesc_fsm, Fsm, #{type := report, tag := on_chain_tx,
                          info := #{tx := SignedTx} = I}} = M ->
            ct:log("OnChainRpt = ~p", [M]),
            ok = match_info(I, Match),
            SignedTx
    after Timeout ->
              error(timeout)
    end.

match_info(Info, Match) ->
    maps:fold(fun(K,V,Acc) ->
                      case maps:find(K, Info) of
                          {ok, V} ->
                              Acc;
                          {ok, Other} ->
                              error({info_mismatch, {K, [V, Other]}});
                          error ->
                              error({no_such_key, K})
                      end
              end, ok, Match).

await_open_report(#{fsm := Fsm} = R, Timeout, _Debug) ->
    receive {aesc_fsm, Fsm, #{type := report, tag := info, info := open} = Msg} ->
                {ok, ChannelId} = maps:find(channel_id, Msg),
                R#{channel_id => ChannelId}
    after Timeout ->
              error(timeout)
    end.

await_leave(#{channel_id := ChId0} = R, Timeout, Debug) ->
    receive_from_fsm(
      leave, R,
      fun(#{channel_id := ChId, info := SignedTx})
            when ChId == ChId0, element(1, SignedTx) == signed_tx ->
              ok
      end, Timeout, Debug).

receive_info(R, Msg, Debug) ->
    {ok, _} = receive_from_fsm(info, R, Msg, ?LONG_TIMEOUT, Debug).

receive_from_fsm(Tag, R, Info, Timeout, Debug) ->
    {ok, _} = receive_from_fsm(Tag, R, Info, Timeout, Debug, false).

receive_from_fsm(Tag, #{role := Role, fsm := Fsm} = R, Info, Timeout, Debug, _Cont)
  when is_atom(Info) ->
    log(Debug, "receive_from_fsm_(~p, ~p, ~p, ~p, ~p, _Cont)",
        [Tag, R, Info, Timeout, Debug]),
    receive
        {aesc_fsm, Fsm, #{type := _Type, tag := Tag, info := Info} = Msg} ->
            log(Debug, "~p: received ~p:~p", [Role, Tag, Msg]),
            {ok, Msg}
    after Timeout ->
            flush(),
            error(timeout)
    end;
receive_from_fsm(Tag, R, Msg, Timeout, Debug, Cont) ->
    TRef = erlang:start_timer(Timeout, self(), receive_from_fsm),
    receive_from_fsm_(Tag, R, Msg, TRef, Debug, Cont).

receive_from_fsm_(Tag, #{role := Role, fsm := Fsm} = R, Msg, TRef, Debug, Cont) ->
    log(Debug, "receive_from_fsm_(~p, ~p, ~p, ~p, ~p, ~p)",
        [Tag, R, Msg, TRef, Debug, Cont]),
    receive
        {aesc_fsm, Fsm, #{type := Type, tag := Tag} = Msg1} ->
            log(Debug, "~p: received ~p:~p/~p", [Role, Type, Tag, Msg1]),
            try match_msgs(Msg, Msg1, Cont)
            catch
                throw:continue ->
                    ct:log("Failed match: ~p", [Msg1]),
                    receive_from_fsm_(Tag, R, Msg, TRef, Debug, Cont)
            after
                erlang:cancel_timer(TRef)
            end;
        {timeout, TRef, receive_from_fsm} ->
            flush(),
            error(timeout)
    end.

flush() ->
    receive M ->
            ct:log("<== ~p", [M]),
            flush()
    after 0 ->
            ok
    end.

match_msgs(F, Msg, Cont) when is_function(F, 1) ->
    try F(Msg), {ok, Msg}
    catch
        error:_ ->
            if Cont ->
                    throw(continue);
               true ->
                    {module, Mod} = erlang:fun_info(F, module),
                    {name, Name} = erlang:fun_info(F, name),
                    ct:log("Message doesn't match fun: ~p / ~w:~w/1",
                           [Msg, Mod, Name]),
                    error({message_mismatch, [Msg]})
            end
    end;
match_msgs(M, #{info := M} = Msg, _) ->
    {ok, Msg};
match_msgs(M, M, _) ->
    {ok, M};
match_msgs(_, _, true) ->
    throw(continue);
match_msgs(A, B, false) ->
    ct:log("Messages don't match: ~p / ~p", [A, B]),
    erlang:error({message_mismatch, [A, B]}).

check_info(Timeout) -> check_info(Timeout, true).

check_info(Timeout, Debug) ->
    receive
        Msg when element(1, Msg) == aesc_fsm ->
            log(Debug, "UNEXPECTED: ~p", [Msg]),
            [Msg|check_info(Timeout, Debug)]
    after Timeout ->
            log(Debug, "unconsumed msgs: ~p",
                [element(2,process_info(self(),messages))]),
            []
    end.

mine_blocks(Node, N) -> mine_blocks(Node, N, true).

mine_blocks(_Node, _N, #{mine_blocks := {ask, Pid},
                         signed_tx   := SignedTx}) when is_pid(Pid) ->
    Pid ! {self(), mine_blocks, SignedTx},
    ok;
mine_blocks(_, _, #{mine_blocks := false}) ->
    ok;
mine_blocks(Node, N, _) ->
    mine_key_blocks(Node, N).

opt_add_tx_to_debug(SignedTx, #{mine_blocks := {ask, _}} = Debug) ->
    Debug#{signed_tx => SignedTx};
opt_add_tx_to_debug(_, Debug) ->
    Debug.

prep_initiator(Node) ->
    {PrivKey, PubKey} = aecore_suite_utils:sign_keys(Node),
    ct:log("initiator Pubkey = ~p", [PubKey]),
    mine_key_blocks(Node, 30),
    ct:log("initiator: 30 blocks mined on ~p", [Node]),
    {ok, Balance} = rpc(Node, aehttp_logic, get_account_balance, [PubKey]),
    #{role => initiator,
      priv => PrivKey,
      pub  => PubKey,
      balance => Balance,
      auth_idx => 1}.

prep_responder(#{pub := IPub, balance := IBal} = _Initiator, Node) ->
    NodeName = aecore_suite_utils:node_name(Node),
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    Amount = IBal div 2,
    {ok, SignedTx} = aecore_suite_utils:spend(NodeName, IPub, Pub, Amount,
                                              20000 * aec_test_utils:min_gas_price()),
    ok = wait_for_signed_transaction_in_block(Node, SignedTx),
    {ok, Amount} = rpc(Node, aehttp_logic, get_account_balance, [Pub]),
    #{role => responder,
      priv => Priv,
      pub  => Pub,
      balance => Amount,
      auth_idx => 1}.

rpc(Node, Mod, Fun, Args) -> rpc(Node, Mod, Fun, Args, true).

rpc(Node, Mod, Fun, Args, Debug) ->
    Res = rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000),
    log(Debug, "rpc(~p,~p,~p,~p) -> ~p", [Node, Mod, Fun, Args, Res]),
    Res.

check_amounts(R, SignedTx, Updates) ->
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
        {aesc_offchain_tx, _TxI} ->
            apply_updates(Updates, R);
        {aesc_deposit_tx, TxD} ->
            Deposit = aesc_deposit_tx:amount(TxD),
            From = aesc_deposit_tx:origin(TxD),
            #{initiator_amount  := IAmt
            , responder_amount  := RAmt
            , pub               := MyKey
            , role              := Role} = R,
            {IAmt1, RAmt1} =
                case {From, Role} of
                    {MyKey, initiator} -> {IAmt + Deposit, RAmt};
                    {MyKey, responder} -> {IAmt, RAmt + Deposit};
                    {_OtherKey, initiator} -> {IAmt, RAmt + Deposit};
                    {_OtherKey, responder} -> {IAmt + Deposit, RAmt}
                end,
            R#{ initiator_amount => IAmt1
                , responder_amount => RAmt1 };
        {aesc_withdraw_tx, TxD} ->
            Withdrawal = aesc_withdraw_tx:amount(TxD),
            From = aesc_withdraw_tx:origin(TxD),
            #{initiator_amount  := IAmt
            , responder_amount  := RAmt
            , pub               := MyKey
            , role              := Role} = R,
            {IAmt1, RAmt1} =
                case {From, Role} of
                    {MyKey, initiator} -> {IAmt - Withdrawal, RAmt};
                    {MyKey, responder} -> {IAmt, RAmt - Withdrawal};
                    {_OtherKey, initiator} -> {IAmt, RAmt - Withdrawal};
                    {_OtherKey, responder} -> {IAmt - Withdrawal, RAmt}
                end,
            R#{ initiator_amount => IAmt1
                , responder_amount => RAmt1 };
        _ ->
            R
    end.

current_height(Node) ->
    case rpc(Node, aec_chain, top_header, []) of
        undefined -> 0;
        Header -> aec_headers:height(Header)
    end.

log(Fmt, Args) ->
    log(true, Fmt, Args).

log(true, Fmt, Args) ->
    ct:log("~p: " ++ Fmt, [self()|Args]);
log(#{debug := true}, Fmt, Args) ->
    ct:log("~p: " ++ Fmt, [self()|Args]);
log(_, _, _) ->
    ok.

if_debug(true, X, _) -> X;
if_debug(#{debug := true}, X, _) -> X;
if_debug(_, _, Y) -> Y.

apply_updates([], R) ->
    R;
apply_updates([{transfer, FromId, ToId, Amount} | T], R) -> %TODO:FIX THIS
    From = aeser_id:specialize(FromId, account),
    To = aeser_id:specialize(ToId, account),
    #{ initiator_amount := IAmt0
     , responder_amount := RAmt0 } = R,
    {IAmt, RAmt} =
        case {role(From, R), role(To, R)} of
            {initiator, responder} ->
                {IAmt0 - Amount, RAmt0 + Amount};
            {responder, initiator} ->
                {IAmt0 + Amount, RAmt0 - Amount}
        end,
    apply_updates(T, R#{ initiator_amount => IAmt
                       , responder_amount => RAmt }).

role(Pubkey, #{pub               := MyKey
             , role              := Role}) ->
    case {Pubkey, Role} of
        {MyKey, initiator}     -> initiator;
        {MyKey, responder}     -> responder;
        {_OtherKey, initiator} -> responder;
        {_OtherKey, responder} -> initiator
    end.

wait_for_signed_transaction_in_block(Node, SignedTx) ->
    wait_for_signed_transaction_in_block(Node, SignedTx, true).

wait_for_signed_transaction_in_block(_, _, #{mine_blocks := {ask,_}}) ->
    ok;
wait_for_signed_transaction_in_block(Node, SignedTx, Debug) ->
    ct:log("waiting for tx ~p", [SignedTx]),
    TxHash = aetx_sign:hash(SignedTx),
    case rpc(dev1, aec_chain, find_tx_location, [TxHash], Debug) of
        BH when is_binary(BH) -> ok;
        NotConfirmed when NotConfirmed =:= mempool;
                          NotConfirmed =:= not_found ->
            EncTxHash = aeser_api_encoder:encode(tx_hash, TxHash),
            case mine_blocks_until_txs_on_chain(Node, [EncTxHash]) of
                {ok, _Blocks} -> ok;
                {error, _Reason} -> did_not_mine
            end
    end.

check_fsm_state(Fsm) ->
    {ok, #{initiator := Initiator,
           responder := Responder,
           init_amt  := IAmt,
           resp_amt  := RAmt,
           state_hash:= StateHash,
           round     := Round}} = aesc_fsm:get_state(Fsm),
    Trees =
        aec_test_utils:create_state_tree_with_accounts(
            [aec_accounts:new(Pubkey, Balance) ||
                {Pubkey, Balance} <- [{Initiator, IAmt}, {Responder, RAmt}]],
            no_backend),
    Hash = aec_trees:hash(Trees),
    StateHash = Hash, %% assert same root hash
    {IAmt, RAmt, StateHash, Round}.

get_debug(Config) ->
    proplists:get_bool(debug, Config).

mine_blocks_until_txs_on_chain(Node, TxHashes) ->
    aecore_suite_utils:mine_blocks_until_txs_on_chain(
      aecore_suite_utils:node_name(Node),
      TxHashes,
      aecore_suite_utils:expected_mine_rate(),
      ?MAX_MINED_BLOCKS,
      #{strictly_follow_top => true}).

mine_key_blocks(Node, KeyBlocks) ->
    aecore_suite_utils:mine_key_blocks(
      aecore_suite_utils:node_name(Node),
      KeyBlocks,
      #{strictly_follow_top => true}).

with_trace(F, Config, File) ->
    with_trace(F, Config, File, on_error).

with_trace(F, Config, File, When) ->
    ct:log("with_trace ...", []),
    TTBRes = aesc_ttb:on_nodes([node()|get_nodes()], File),
    ct:log("Trace set up: ~p", [TTBRes]),
    try F(Config)
    catch
	error:R ->
	    Stack = erlang:get_stacktrace(),
	    ct:pal("Error ~p~nStack = ~p", [R, Stack]),
	    ttb_stop(),
	    erlang:error(R);
	exit:R ->
	    Stack = erlang:get_stacktrace(),
	    ct:pal("Exit ~p~nStack = ~p", [R, Stack]),
	    ttb_stop(),
	    exit(R);
        throw:Res ->
            ct:pal("Caught throw:~p", [Res]),
            throw(Res)
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
    case aesc_ttb:format(Dir, Out, #{limit => 50000}) of
        {error, Reason} ->
            ct:pal("TTB formatting error: ~p", [Reason]);
        _ ->
            ok
    end,
    ct:pal("Formatted trace log in ~s~n", [Out]).

get_nodes() ->
    [aecore_suite_utils:node_name(dev1)].

attach_initiator(Cfg) ->
    #{ pub := IPub, priv := IPrivKey } = proplists:get_value(initiator, Cfg),
    #{contract    := GAContract,
      auth_fun    := GAAuthFun,
      auth_params := #{initiator := #{init_params := GAParams}}} =
        proplists:get_value(ga, Cfg),
    attach({IPub, IPrivKey}, GAContract, GAAuthFun, GAParams),
    ok.

attach_responder(Cfg) ->
    #{ pub := RPub, priv := RPrivKey } = ?config(responder, Cfg),
    #{contract    := GAContract,
      auth_fun    := GAAuthFun,
      auth_params := #{responder := #{init_params := GAParams}}} = ?config(ga, Cfg),
    attach({RPub, RPrivKey}, GAContract, GAAuthFun, GAParams),
    ok.

attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args) ->
    attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args, #{}).

attach({Owner, OwnerPrivkey}, Contract, AuthFun, Args, Opts) ->
    case get_contract(Contract) of
        {ok, #{src := Src, bytecode := C, map := #{type_info := TI}}} ->
            attach_({Owner, OwnerPrivkey}, Src, C, TI, AuthFun, Args, Opts);
        _ ->
            error(bad_contract)
    end.

attach_({Owner, OwnerPrivkey}, Src, ByteCode, TypeInfo, AuthFun, Args, Opts) ->
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Owner]),
    Calldata = aega_test_utils:make_calldata(Src, "init", Args),
    {ok, AuthFunHash} = aeb_aevm_abi:type_hash_from_function_name(list_to_binary(AuthFun),
                                                                  TypeInfo),
    Options1 = maps:merge(#{nonce => Nonce, code => ByteCode,
                            auth_fun => AuthFunHash, call_data => Calldata},
                          Opts),
    AttachTx = aega_test_utils:ga_attach_tx(Owner, Options1),

    SignedTx = aec_test_utils:sign_tx(AttachTx, [OwnerPrivkey]),
    ok = rpc(dev1, aec_tx_pool, push, [SignedTx]),
    wait_for_signed_transaction_in_block(dev1, SignedTx),
    ok.

initiator_spend(Cfg) ->
    ga_spend(initiator, responder, 1234, Cfg),
    %ga_spend(initiator, responder, 42, Cfg),
    ok.

responder_spend(Cfg) ->
    ga_spend(responder, initiator, 333, Cfg),
    ok.

ga_spend(From, To, Amt, Cfg) ->
    #{pub := SendPub, auth_idx := _N} = ?config(From, Cfg),
    #{pub := ReceivePub} = ?config(To, Cfg),
    #{auth_params := BothAuths} = ?config(ga, Cfg),
    Auth = maps:get(From, BothAuths),
    SpendProps = #{sender_id    => aeser_id:create(account, SendPub),
                   recipient_id => aeser_id:create(account, ReceivePub),
                   amount       => Amt,
                   fee          => 20000 * aec_test_utils:min_gas_price(),
                   nonce        => 0,
                   payload      => <<>>},
    {ok, SpendAetx} = aec_spend_tx:new(SpendProps),
    SignedTx = meta(SendPub, Auth, 1, aetx_sign:new(SpendAetx, [])),
    ok = rpc(dev1, aec_tx_pool, push, [SignedTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(
        aecore_suite_utils:node_name(dev1), [TxHash], ?MAX_MINED_BLOCKS),
    ok.

get_contract(Name) ->
    aega_test_utils:get_contract(?VM_AEVM_SOPHIA_3, Name).

account_type(Pubkey) ->
    {value, Account} = rpc(dev1, aec_chain, get_account, [Pubkey]),
    aec_accounts:type(Account).

btc_auth(TxHash, Nonce, PrivKey) ->
    Val = <<32:256, TxHash/binary, (list_to_integer(Nonce)):256>>,
    Sig0 = crypto:sign(ecdsa, sha256, {digest, aec_hash:hash(tx, Val)},
                       [PrivKey, secp256k1]),
    Sig  = aega_test_utils:to_hex_lit(64, aeu_crypto:ecdsa_from_der_sig(Sig0)),
    aega_test_utils:make_calldata("bitcoin_auth", "authorize", [Nonce, Sig]).

meta(Owner, AuthOpts, _N, SignedTx) ->
    {Nonce, Aetx} =
        case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
            {aesc_offchain_tx, OffChainTx} ->
                ChannelPubKey = aesc_offchain_tx:channel_pubkey(OffChainTx),
                {get_btc_offchain_nonce(ChannelPubKey, Owner),
                 %% off-chain tx are authenticating the innermost tx
                 aetx_sign:innermost_tx(SignedTx)};
            _ -> {get_btc_auth_nonce(Owner),
                  %% on-chain txs are authenticating the inner tx
                  aetx_sign:tx(SignedTx)}
        end,
    TxBin    = aec_governance:add_network_id(aetx:serialize_to_binary(Aetx)),
    %% authenticate the inner tx, that could be a transaction instance or yet
    %% another meta
    AuthData = make_authdata(AuthOpts, Nonce, aec_hash:hash(tx, TxBin)),
    %% produce the new layer of meta authenticating the inner tx and not the
    %% innermost one, but include the inner tx
    aecore_suite_utils:meta_tx(Owner, AuthOpts, AuthData, SignedTx).

make_authdata(#{ prep_fun := F }, N, TxHash) ->
    F(TxHash, N).

new_config_table() ->
    spawn(fun() ->
              ets:new(fsm_suite, [set, named_table, public]),
              ets:insert(fsm_suite, {initiator, 1}),
              ets:insert(fsm_suite, {responder, 1}),
              receive
                  die -> ok
              end
          end).

load_idx(Cfg) ->
    Cfg1 = load_last_idx(initiator, Cfg),
    load_last_idx(responder, Cfg1).

load_last_idx(Role, Cfg) ->
    OldValue =
        case ets:lookup(fsm_suite, Role) of
            [{Role, IDx}] -> IDx;
            [] -> 1
        end,
    Part = proplists:get_value(Role, Cfg),
    [{Role, Part#{auth_idx => OldValue}} | Cfg].

bump_idx() ->
    bump_last_idx(responder, 10000),
    bump_last_idx(initiator, 10000).

bump_last_idx(Role, Bump) ->
    ets:update_counter(fsm_suite, Role, Bump).

config() -> get(config).
config(Cfg) -> put(config, Cfg).

get_btc_auth_nonce(PubKey) ->
    {value, Account} = rpc(dev1, aec_chain, get_account, [PubKey]),
    ContractPubkey = aeser_id:specialize(aec_accounts:ga_contract(Account),
                                         contract),
    {ok, Contract} = rpc(dev1, aec_chain, get_contract, [ContractPubkey]),
    extract_nonce_from_btc_auth_store(aect_contracts:state(Contract)).

get_btc_offchain_nonce(ChannelPubKey, Owner) ->
    case rpc(dev1, aec_chain, get_channel, [ChannelPubKey]) of
        {ok, Channel} ->
            StoreKey = aesc_channels:auth_store_key(aeser_id:create(account, Owner),
                                                    Channel),
            {ok, Trees} = rpc(dev1, aec_chain, get_top_state, []),
            ContractTree = aec_trees:contracts(Trees),
            {ok, Store} = rpc(dev1, aect_state_tree, read_contract_store, [StoreKey,
                                                                          ContractTree]),
            extract_nonce_from_btc_auth_store(Store);
        {error, not_found} -> 42 %% negative tests
    end.


%% this function inspects the bitcoin_auth contract's store and extracts the
%% nonce out of it. It heavily relies on the state of the contract being
%% { nonce : int, owner : bytes(64) }
extract_nonce_from_btc_auth_store(Store) ->
    #{<<0>> := Encoded0} = rpc(dev1, aect_contracts_store, contents, [Store]),
    {ok, {Nonce, _}} = aeb_heap:from_binary({tuple, [word, {tuple, [word, word]}]},
                                Encoded0),
    Nonce.


