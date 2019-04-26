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
        ]).

%% exports for aehttp_integration_SUITE
-export([
          create_channel_on_port/1
        ]).

-include_lib("common_test/include/ct.hrl").

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

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [ {group, transactions}
                             , {group, errors}
                             , {group, throughput}
                             , {group, signatures}
                             , {group, channel_ids}
                             , {group, round_too_low}
                             , {group, round_too_high}
                             ]},
     {transactions, [sequence],
      [
        create_channel
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
     {state_hash, [sequence], [check_incorrect_create | update_sequence()]}
    ].

update_sequence() ->
    [ check_incorrect_deposit
    , check_incorrect_withdrawal
    , check_incorrect_update].

suite() ->
    [].

init_per_suite(Config) ->
    DefCfg = #{<<"chain">> => #{<<"persist">> => false},
               <<"mining">> => #{<<"micro_block_cycle">> => 1}},
    Config1 = aecore_suite_utils:init_per_suite([dev1], DefCfg, [{symlink_name, "latest.aesc_fsm"}, {test_module, ?MODULE}] ++ Config),
    [{nodes, [aecore_suite_utils:node_tuple(N)
              || N <- [dev1]]} | Config1].

end_per_suite(_Config) ->
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
init_per_group(_Group, Config) ->
    init_per_group_(Config).

init_per_group_(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ct:log("dev1 connected", []),
    try begin
            Initiator = prep_initiator(dev1),
            Responder = prep_responder(Initiator, dev1),
            [{initiator, Initiator},
             {responder, Responder},
             {port, ?PORT},
             {initiator_amount, 1000000 * aec_test_utils:min_gas_price()},
             {responder_amount, 1000000 * aec_test_utils:min_gas_price()}
             | Config]
        end
    catch
        error:Reason ->
            Trace = erlang:get_stacktrace(),
            catch stop_node(dev1, Config),
            error(Reason, Trace)
    end.

end_per_group(_Group, Config) ->
    _Config1 = stop_node(dev1, Config),
    ok.

init_per_testcase(leave_reestablish, Config) ->
    [{debug, true} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(T, _Config) when T==multiple_channels;
                                  T==many_chs_msg_loop ->
    Node = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    ok;
end_per_testcase(_Case, _Config) ->
    ok.


stop_node(N, Config) ->
    aecore_suite_utils:stop_node(N, Config),
    Config.


%%%===================================================================
%%% Test state
%%%===================================================================

create_channel(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI, channel_id := ChannelId} = I
     , r := #{} = R} = create_channel_([?SLOGAN|Cfg]),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    _ = await_signing_request(shutdown, I, ?TIMEOUT, Debug),
    _ = await_signing_request(shutdown_ack, R, ?TIMEOUT, Debug),
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
                                           [?SLOGAN|Cfg]),
    ok = rpc(dev1, aesc_fsm, inband_msg, [FsmI, PubR, <<"i2r hello">>]),
    {ok,_} =receive_from_fsm(
              message, R,
              fun(#{info := #{info := <<"i2r hello">>}}) -> ok end, 1000, true),

    shutdown_(I, R),
    check_info(500).

upd_transfer(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI, channel_id := ChannelId} = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {I0, R0} = do_update(PubI, PubR, 2, I, R, true),
    {BalI1, BalR1} = get_both_balances(FsmI, PubI, PubR),
    BalI1 = BalI - 2,
    BalR1 = BalR + 2,
    {I1, R1} = update_bench(I0, R0),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    {_I2, _} = await_signing_request(shutdown, I1),
    {_R2, _} = await_signing_request(shutdown_ack, R1),
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
    {_I1, _} = await_signing_request(update, I, Debug),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(update, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {I1, _} = await_signing_request(update, I, Debug),
    Reject = fun(_Tag, #{fsm := Fsm1} = Rx, SignedTx, _Updates) ->
                     {error, conflict} = rpc(dev1, aesc_fsm, upd_transfer,
                                             [Fsm1, PubR, PubI, 1]),
                     {Rx, SignedTx}
             end,
    {R1, _} = await_signing_request(update_ack, R, Reject, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I1, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R1, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {_I1, _} = await_signing_request(deposit_tx, I, Debug),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(deposit_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {I1, _} = await_signing_request(deposit_tx, I, Debug),
    Reject = fun(_Tag, #{fsm := Fsm1} = Rx, SignedTx, _Updates) ->
                     {error, conflict} = rpc(dev1, aesc_fsm, upd_transfer,
                                             [Fsm1, PubR, PubI, 1]),
                     {Rx, SignedTx}
             end,
    {R1, _} = await_signing_request(deposit_created, R, Reject, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(conflict, I1, any_msg(), ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(conflict, R1, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {_I1, _} = await_signing_request(withdraw_tx, I, Debug),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(withdraw_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {I1, _} = await_signing_request(withdraw_tx, I, Debug),
    Reject = fun(_Tag, #{fsm := Fsm1} = Rx, SignedTx, _Updates) ->
                     {error, conflict} = rpc(dev1, aesc_fsm, upd_transfer,
                                             [Fsm1, PubR, PubI, 1]),
                     {Rx, SignedTx}
             end,
    {R1, _} = await_signing_request(withdraw_created, R, Reject, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(conflict, I1, any_msg(), ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(conflict, R1, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {_I1, _} = await_signing_request(update, I, Debug),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(deposit_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {_I1, _} = await_signing_request(update, I, Debug),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(withdraw_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
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
    {_I1, _} = await_signing_request(deposit_tx, I, Debug),
    %% FsmR should now detect a conflict
    {ok, _} = receive_from_fsm(withdraw_tx, R, signing_req(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    check_info(500),
    shutdown_(I, R),
    ok.

signing_req() ->
    fun(#{type := sign}) -> true;
       (_) -> false
    end.

any_msg() ->
    fun(_) -> true end.

%% update_with_conflict(PKi, PKr,
%%                      #fsm{fsm := FsmI} = I,
%%                      #fsm{fsm := FsmR} = R, Debug) ->
%%     #{ i := #{fsm := FsmI, channel_id := ChannelId} = I
%%      , r := #{} = R
%%      , spec := #{ initiator := PubI
%%                 , responder := PubR }} = create_channel_(
%%                                            [{port,9326},?SLOGAN|Cfg]),
%%     {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
%%     rpc(dev1, aesc_fsm, upd_transfer, [FsmI, PKi, PKr, 1], Debug),
%%     foo.

update_bench(I, R) ->
    {Time, I1, R1} = do_n(1000, fun update_volley/2, I, R),
    Fmt = "Time (1*2*1000): ~.1f s; ~.1f mspt; ~.1f tps",
    Args = [Time/1000, Time/2000, 2000*1000/Time],
    ct:log(Fmt, Args),
    ct:comment(Fmt, Args),
    {I1, R1}.

do_n(N, F, I, R) ->
    TS = erlang:system_time(millisecond),
    {I1, R1} = do_n_(N, F, I, R),
    {erlang:system_time(millisecond) - TS, I1, R1}.

do_n_(0, _, I, R) ->
    {I, R};
do_n_(N, F, I, R) when N > 0 ->
    {I1, R1} = F(I, R),
    do_n_(N-1, F, I1, R1).

     
update_volley(#{pub := PubI} = I, #{pub := PubR} = R) ->
    {I1, R1} = do_update(PubR, PubI, 1, I, R, false),
    do_update(PubI, PubR, 1, I1, R1, false).

do_update(From, To, Amount, #{fsm := FsmI} = I, R, Debug) ->
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, From, To, Amount], Debug),
    {I1, _} = await_signing_request(update, I, Debug),
    {R1, _} = await_signing_request(update_ack, R, Debug),
    check_info(if_debug(Debug, 100, 0), Debug),
    {I1, R1}.

msg_volley(#{fsm := FsmI, pub := PubI} = I, #{fsm := FsmR, pub := PubR} = R) ->
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
    {ok, I1, R1} = deposit_(I, R, Deposit, Round0, Debug),
    check_info(500),
    shutdown_(I1, R1),
    ok.

deposit_(#{fsm := FsmI} = I, R, Deposit, Debug) ->
    {_IAmt0, _RAmt0, _, Round0} = check_fsm_state(FsmI),
    deposit_(I, R, Deposit, Round0, Debug).

deposit_(#{fsm := FsmI} = I, R, Deposit, Round1, Debug) ->
    ok = rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => Deposit}]),
    {I1, _} = await_signing_request(deposit_tx, I),
    {R1, _} = await_signing_request(deposit_created, R),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {IAmt0, RAmt0, _, _Round1} = check_fsm_state(FsmI),
    Round2 = Round1 + 1,
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    {IAmt, RAmt0, StateHash, Round2} = check_fsm_state(FsmI),
    {IAmt, _} = {IAmt0 + Deposit, IAmt}, %% assert correct amounts
    {channel_deposit_tx, DepositTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
    Round2 = aesc_deposit_tx:round(DepositTx), %% assert correct round
    StateHash = aesc_deposit_tx:state_hash(DepositTx), %% assert correct state hash
    ct:log("I1 = ~p", [I1]),
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I1,
    Expected = {IAmt2, RAmt2},
    {Expected, Expected} = {{IAmt0 + Deposit, RAmt0}, Expected},
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
    {I1, _} = await_signing_request(withdraw_tx, I),
    {_R1, _} = await_signing_request(withdraw_created, R),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {IAmt0, RAmt0, _, _Round0 = 1} = check_fsm_state(FsmI),
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    {IAmt, RAmt0, StateHash, Round2 = 2} = check_fsm_state(FsmI),
    {IAmt, _} = {IAmt0 - Withdrawal, IAmt}, %% assert correct amounts
    {channel_withdraw_tx, WithdrawalTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
    Round2 = aesc_withdraw_tx:round(WithdrawalTx), %% assert correct round
    StateHash = aesc_withdraw_tx:state_hash(WithdrawalTx), %% assert correct state hash
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I1,
    Expected = {IAmt2, RAmt2},
    {Expected, Expected} = {{IAmt0 - Withdrawal, RAmt0}, Expected},
    check_info(500),
    shutdown_(I, R),
    ok.

channel_detects_close_solo(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := I, r := R, spec := Spec } = create_channel_([?SLOGAN|Cfg]),
    {ok, Tx} = close_solo_tx(I, <<>>),
    #{ priv := IPrivKey } = ?config(initiator, Cfg),
    SignedCloseSoloTx = aec_test_utils:sign_tx(Tx, [IPrivKey]),
    ok = rpc(dev1, aec_tx_pool, push, [SignedCloseSoloTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedCloseSoloTx)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(
        aecore_suite_utils:node_name(dev1), [TxHash], ?MAX_MINED_BLOCKS),
    LockPeriod = maps:get(lock_period, Spec),
    TTL = current_height(dev1) + LockPeriod,
    ct:log("Expected TTL = ~p", [TTL]),
    SignedTx = await_on_chain_report(I, #{info => solo_closing}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, #{info => solo_closing}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun closing/1, ?TIMEOUT, Debug),
    check_info(500),
    settle_(LockPeriod, maps:get(minimum_depth, Spec), I, R, Debug),
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
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [IPubKey]),
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
    #{i := I, r := R} = leave_reestablish_([?SLOGAN|Cfg]),
    shutdown_(I, R),
    ok.

leave_reestablish_(Cfg) ->
    Debug = get_debug(Cfg),
    {I0, R0, Spec0} = channel_spec(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} =
        create_channel_from_spec(I0, R0, Spec0, ?PORT, Debug),
    ct:log("I = ~p", [I]),
    ChId = maps:get(channel_id, I),
    Cache1 = cache_status(ChId),
    [_,_] = in_ram(Cache1),
    false = on_disk(Cache1),
    ok = rpc(dev1, aesc_fsm, leave, [FsmI]),
    {ok,Li} = await_leave(I, ?TIMEOUT, Debug),
    {ok,Lr} = await_leave(R, ?TIMEOUT, Debug),
    SignedTx = maps:get(info, Li),
    SignedTx = maps:get(info, Lr),
    {ok,_} = receive_from_fsm(info, I, fun died_normal/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun died_normal/1, ?TIMEOUT, Debug),
    retry(3, 100,
          fun() ->
                  Cache2 = cache_status(ChId),
                  [] = in_ram(Cache2),
                  true = on_disk(Cache2)
          end),
    %%
    %% reestablish
    %%
    ChId = maps:get(channel_id, R),
    check_info(500),
    ct:log("reestablishing ...", []),
    reestablish(ChId, I0, R0, SignedTx, Spec0, ?PORT, Debug).

leave_reestablish_close(Cfg) ->
    Debug = get_debug(Cfg),
    #{i := I, r := R, spec := Spec} = leave_reestablish_([?SLOGAN|Cfg]),
    #{initiator := PubI, responder := PubR} = Spec,
    {I1, R1} = do_update(PubI, PubR, 1, I, R, Debug),
    ChId = maps:get(channel_id, I1),
    Cache1 = cache_status(ChId),
    [_,_] = in_ram(Cache1),
    false = on_disk(Cache1),
    shutdown_(I1, R1),
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
    shutdown_(I, R),
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

%% died_subverted(#{info := {died,channel_closing_on_chain}}) -> ok.
died_subverted(#{info := {died,_}}) -> ok.

%% solo_closing(#{info := solo_closing}) -> ok.

closing(#{info := closing} = Msg) ->
    ct:log("matches #{info := closing} - ~p", [Msg]),
    ok.

multiple_channels(Cfg) ->
    multiple_channels_t(10, 9360, {transfer, 100}, ?SLOGAN, Cfg).

many_chs_msg_loop(Cfg) ->
    multiple_channels_t(10, 9400, {msgs, 100}, ?SLOGAN, Cfg).

multiple_channels_t(NumCs, FromPort, Msg, Slogan, Cfg) ->
    ct:log("spawning ~p channels", [NumCs]),
    Initiator = maps:get(pub, ?config(initiator, Cfg)),
    ct:log("Initiator: ~p", [Initiator]),
    Me = self(),
    Node = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, NumCs),
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Initiator]),
    Cs = [create_multi_channel([{port, FromPort + N},
                                {ack_to, Me},
                                {nonce, Nonce + N - 1},
                                {minimum_depth, 0},
                                Slogan | Cfg], #{mine_blocks => {ask, Me},
                                                 debug => false})
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
    aecore_suite_utils:mine_blocks_until_txs_on_chain(
        aecore_suite_utils:node_name(dev1), TxHashes, ?MAX_MINED_BLOCKS),
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
    Port = proplists:get_value(port, Cfg, ?PORT),
    CreateFun = proplists:get_value(wrong_create, Cfg),
    CreateData = {I, R, Spec, Port, get_debug(Cfg)},
    CreateFun(CreateData, initiator),
    CreateFun(CreateData, responder),
    ok.

wrong_sig_create(Params, Malicious) ->
    wrong_create_(Params, Malicious,
                  fun(Tx, _Priv) ->
                      _SignedTx = aec_test_utils:sign_tx(Tx, [?BOGUS_PRIVKEY])
                  end,
                  bad_signature).

wrong_hash_create(Params, Malicious) ->
    wrong_create_(Params, Malicious,
                  fun(Tx0, Priv) ->
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
    shutdown_(I, R),
    ok.

check_incorrect_withdrawal(Cfg) ->
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
    shutdown_(I, R),
    ok.

check_incorrect_update(Cfg) ->
    Fun = proplists:get_value(wrong_action, Cfg),
    Test =
        fun(Depositor, Malicious) ->
            Debug = true,
            #{ i := #{pub := IPub, fsm := FsmI} = I
            , r := #{pub := RPub, fsm := FsmR} = R
            , spec := Spec} = create_channel_([?SLOGAN|Cfg]),
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
            ok
          end,
    Roles = [initiator, responder],
    [Test(Depositor, Malicious) || Depositor <- Roles,
                                   Malicious <- Roles],
    ok.

check_incorrect_mutual_close(Cfg) ->
    Fun = proplists:get_value(wrong_action_detailed, Cfg),
    Test =
        fun(Depositor, Malicious) ->
            Debug = true,
            #{ i := I
             , r := R
             , spec := Spec} = create_channel_([?SLOGAN|Cfg]),
            Port = proplists:get_value(port, Cfg, ?PORT),
            Data = {I, R, Spec, Port, Debug},
            Fun(Data, Depositor, Malicious,
                {shutdown, [], shutdown,
                 shutdown_ack},
                fun(#{fsm := FsmPid}, _Debug) ->
                    timer:sleep(1000),
                    true = rpc(dev1, erlang, process_info, [FsmPid]) =:= undefined
                end),
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
        create_channel_from_spec(Si, Sr, Spec, Port, Debug),
    %% We don't have enough funds to cover the closing fee
    {error, insufficient_funds} = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    timer:sleep(1000),
    %% Fsms should be unaffected
    true = (rpc(dev1, erlang, process_info, [FsmI]) =/= undefined),
    true = (rpc(dev1, erlang, process_info, [FsmR]) =/= undefined),
    %% Deposit funds to cover the closing fee. Then it should work
    {ok, _I1, _R1} = deposit_(I, R, 20000 * aec_test_utils:min_gas_price(), Debug),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    ok.

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
    wrong_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                 {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                  DetectConflictFun,
                  fun(Action, Signer, Debug1) ->
                      {_, _WrongSignedTx} =
                          await_signing_request(Action, Signer#{priv =>
                                                                ?BOGUS_PRIVKEY},
                                                Debug1)
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
    Action = fun sign_signing_request/4,
    TryCheating =
        fun(Tag, #{priv := Priv} = Signer, Debug1) ->
            receive {aesc_fsm, Fsm, #{type := sign, tag := Tag,
                                      info := #{tx := Tx, updates := Updates}} = Msg} ->
                    log(Debug1, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
                    SignedTx = SignTxFun(Tx, Priv),
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
            {_I2, _} = await_signing_request(create_tx, I1, Debug),
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
    wrong_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
                 {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
                  DetectConflictFun,
                  fun(Tag, #{priv := Priv} = Signer, Debug1) ->
                      Action = fun sign_signing_request/4,
                      receive {aesc_fsm, Fsm, #{type := sign, tag := Tag,
                                                info := #{tx := Tx0,
                                                          updates := Updates}} = Msg} ->
                              log(Debug1, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
                              Tx = ModifyTxFun(Tx0, Fsm),
                              log(Debug1, "modified ~p", [Tx]),
                              SignedTx = aec_test_utils:sign_tx(Tx, [Priv]),
                              Action(Tag, Signer, SignedTx, Updates)
                      after ?TIMEOUT ->
                              error(timeout)
                      end
                  end,
                  ErrorMsg).

wrong_action({I, R, _Spec, _Port, Debug}, Poster, Malicious,
              {FsmFun, FsmFunArg, FsmNewAction, FsmCreatedAction},
               DetectConflictFun, MaliciousSign, ErrMsg) ->
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
            {_, _} = await_signing_request(FsmNewAction, D, Debug),
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmA, false], Debug),
            {_, _} = MaliciousSign(FsmCreatedAction, A, Debug),
            DetectConflictFun(A, Debug),
            rpc(dev1, aesc_fsm, strict_checks, [FsmA, true], Debug)
    end,
    check_info(500),
    ok.

shutdown_(#{fsm := FsmI, channel_id := ChannelId} = I, R) ->
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    _ = await_signing_request(shutdown, I),
    _ = await_signing_request(shutdown_ack, R),
    SignedTx = await_on_chain_report(I, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx),
    verify_close_mutual_tx(SignedTx, ChannelId),
    ok.

settle_(TTL, MinDepth, #{fsm := FsmI, channel_id := ChannelId} = I, R, Debug) ->
    ok = rpc(dev1, aesc_fsm, settle, [FsmI]),
    {_, SignedTx} = await_signing_request(settle_tx, I),
    ct:log("settle_tx signed", []),
    {ok, MinedKeyBlocks} = aecore_suite_utils:mine_blocks_until_txs_on_chain(
                             aecore_suite_utils:node_name(dev1),
                             [aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))],
                             ?MAX_MINED_BLOCKS),
    KeyBlocksMissingForTTL = (TTL + 1) - length(MinedKeyBlocks),
    KeyBlocksMissingForMinDepth =
        if
            KeyBlocksMissingForTTL > 0 ->
                aecore_suite_utils:mine_key_blocks(
                  aecore_suite_utils:node_name(dev1), KeyBlocksMissingForTTL),
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
            aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(dev1), KeyBlocksMissingForMinDepth);
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
    Timeout = 60000 + (N div 10)*5000,  % wild guess
    receive
        {Pid, Tag} ->
            [Pid | collect_acks(Pids, Tag, N)]
    after Timeout ->
            error(timeout)
    end;
collect_acks([], _Tag, _) ->
    [].

collect_acks_w_payload([Pid | Pids], Tag, N) ->
    Timeout = 30000 + (N div 10)*5000,  % wild guess
    receive
        {Pid, Tag, Payload} ->
            [{Pid, Payload} | collect_acks_w_payload(Pids, Tag, N)]
    after Timeout ->
            error(timeout)
    end;
collect_acks_w_payload([], _Tag, _) ->
    [].


create_multi_channel(Cfg, Debug) ->
    spawn_link(fun() ->
                       create_multi_channel_(Cfg, Debug)
               end).

create_multi_channel_(Cfg, Debug) ->
    #{i := I, r := R} = create_channel_(Cfg, Debug),
    Parent = ?config(ack_to, Cfg),
    Parent ! {self(), channel_ack},
    ch_loop(I, R, Parent).

ch_loop(I, R, Parent) ->
    receive
        {transfer, N} ->
            {_, I1, R1} = do_n(N, fun update_volley/2, I, R),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent);
        {msgs, N} ->
            {_, I1, R1} = do_n(N, fun msg_volley/2, I, R),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent);
        die -> ok
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
    Port = proplists:get_value(port, Cfg, 9325),
    create_channel_from_spec(I, R, Spec, Port, Debug).

channel_spec(Cfg) ->
    channel_spec(Cfg, 300000, 200000).

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

create_channel_from_spec(
  I, R, #{initiator_amount := IAmt0, responder_amount := RAmt0,
          push_amount := PushAmount} = Spec, Port, Debug) ->
    IAmt = IAmt0 - PushAmount,
    RAmt = RAmt0 + PushAmount,
    {ok, FsmR} = rpc(dev1, aesc_fsm, respond, [Port, Spec], Debug),
    {ok, FsmI} = rpc(dev1, aesc_fsm, initiate, ["localhost", Port, Spec], Debug),

    log(Debug, "FSMs, I = ~p, R = ~p", [FsmI, FsmR]),

    I1 = I#{fsm => FsmI, initiator_amount => IAmt, responder_amount => RAmt},
    R1 = R#{fsm => FsmR, initiator_amount => IAmt, responder_amount => RAmt},

    {ok, _} = receive_from_fsm(info, R1, channel_open, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(info, I1, channel_accept, ?TIMEOUT, Debug),

    {I2, R2} = try await_create_tx_i(I1, R1, Debug)
               catch
                   error:Err ->
                       ct:log("Caught Err = ~p~nMessages = ~p",
                              [Err, element(2, process_info(self(), messages))]),
                       error(Err, erlang:get_stacktrace())
               end,
    log(Debug, "mining blocks on dev1 for minimum depth", []),
    CurrentHeight = current_height(dev1),
    SignedTx = await_on_chain_report(I2, ?TIMEOUT),
    ct:log("SignedTx = ~p", [SignedTx]),
    SignedTx = await_on_chain_report(R2, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    mine_blocks(dev1, ?MINIMUM_DEPTH, opt_add_tx_to_debug(SignedTx, Debug)),
    %% in case of multiple channels starting in parallel - the mining above
    %% has no effect (the blocks are mined in another process)
    %% The following line makes sure this process is blocked until the proper
    %% height is reached
    aecore_suite_utils:wait_for_height(aecore_suite_utils:node_name(dev1),
                                       CurrentHeight + ?MINIMUM_DEPTH),
    %% we've seen 10-15 second block times in CI, so wait a while longer
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
    {Mod, TxI} = aetx:specialize_callback(aetx_sign:tx(SignedTx)),
    {Mod:initiator_amount(TxI),
     Mod:responder_amount(TxI)}.

update_tx(Tx0, F, Args) ->
    {Mod, TxI} = aetx:specialize_callback(Tx0),
    TxI1 = apply(Mod, F, [TxI|Args]),
    aetx:new(Mod, TxI1).

set_amounts(IAmt, RAmt, Map) ->
    Map#{initiator_amount => IAmt, responder_amount => RAmt}.

verify_close_mutual_tx(SignedTx, ChannelId) ->
    {channel_close_mutual_tx, Tx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
    {_, ChInfo} = aesc_close_mutual_tx:serialize(Tx),
    true = lists:member(ChannelId,
        [ aeser_id:specialize(ChId, channel)||
             {channel_id, ChId} <- ChInfo
        ]).

verify_settle_tx(SignedTx, ChannelId) ->
    {channel_settle_tx, Tx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
    {_, ChInfo} = aesc_settle_tx:serialize(Tx),
    true = lists:member(ChannelId,
        [ aeser_id:specialize(ChId, channel) ||
            {channel_id, ChId} <- ChInfo
        ]).

await_create_tx_i(I, R, Debug) ->
    {I1, _} = await_signing_request(create_tx, I, Debug),
    await_funding_created_p(I1, R, Debug).

await_funding_created_p(I, R, Debug) ->
    receive_from_fsm(info, R, funding_created, ?TIMEOUT, Debug),
    {R1, _} = await_signing_request(funding_created, R, Debug),
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

await_signing_request(Tag, R) ->
    await_signing_request(Tag, R, ?TIMEOUT, true).

await_signing_request(Tag, R, Debug) ->
    await_signing_request(Tag, R, ?TIMEOUT, Debug).

await_signing_request(Tag, R, Timeout, Debug) ->
    Action = fun sign_signing_request/4,
    await_signing_request(Tag, R, Action, Timeout, Debug).

await_signing_request(Tag, #{fsm := Fsm, priv := Priv} = R,
                      Action, Timeout, Debug) ->
    receive {aesc_fsm, Fsm, #{type := sign, tag := Tag,
                              info := #{tx := Tx, updates := Updates}} = Msg} ->
            log(Debug, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
            SignedTx = aec_test_utils:sign_tx(Tx, [Priv]),
            Action(Tag, R, SignedTx, Updates)
    after Timeout ->
            error(timeout)
    end.

sign_signing_request(Tag, #{fsm := Fsm} = R, SignedTx, Updates) ->
    aesc_fsm:signing_response(Fsm, Tag, SignedTx),
    {check_amounts(R, SignedTx, Updates), SignedTx}.

await_on_chain_report(R, Timeout) ->
    await_on_chain_report(R, #{}, Timeout).

await_on_chain_report(#{fsm := Fsm}, Match, Timeout) ->
    receive
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
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(Node), N,
                                       #{strictly_follow_top => true}).

opt_add_tx_to_debug(SignedTx, #{mine_blocks := {ask, _}} = Debug) ->
    Debug#{signed_tx => SignedTx};
opt_add_tx_to_debug(_, Debug) ->
    Debug.

prep_initiator(Node) ->
    {PrivKey, PubKey} = aecore_suite_utils:sign_keys(Node),
    ct:log("initiator Pubkey = ~p", [PubKey]),
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(Node), 30,
                                      #{strictly_follow_top => true}),
    ct:log("initiator: 30 blocks mined on ~p", [Node]),
    {ok, Balance} = rpc(Node, aehttp_logic, get_account_balance, [PubKey]),
    #{role => initiator,
      priv => PrivKey,
      pub  => PubKey,
      balance => Balance}.

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
      balance => Amount}.

rpc(Node, Mod, Fun, Args) -> rpc(Node, Mod, Fun, Args, true).

rpc(Node, Mod, Fun, Args, Debug) ->
    Res = rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000),
    log(Debug, "rpc(~p,~p,~p,~p) -> ~p", [Node, Mod, Fun, Args, Res]),
    Res.

check_amounts(R, SignedTx, Updates) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of
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


log(true, Fmt, Args) ->
    ct:log(Fmt, Args);
log(#{debug := true}, Fmt, Args) ->
    ct:log(Fmt, Args);
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
wait_for_signed_transaction_in_block(Node, SignedTx, _Debug) ->
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    NodeName = aecore_suite_utils:node_name(Node),
    case aecore_suite_utils:mine_blocks_until_txs_on_chain(NodeName, [TxHash], ?MAX_MINED_BLOCKS) of
        {ok, _Blocks} -> ok;
        {error, _Reason} -> did_not_mine
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

