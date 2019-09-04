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
        , channel_invalid_state_password/1
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
        , update_with_signing_abort/1
        , deposit_with_signing_abort/1
        , withdraw_with_signing_abort/1
        , shutdown_with_signing_abort/1
        , deposit/1
        , withdraw/1
        , withdraw_high_amount_static_confirmation_time/1
        , withdraw_high_amount_short_confirmation_time/1
        , withdraw_low_amount_long_confirmation_time/1
        , withdraw_low_amount_long_confirmation_time_negative_test/1
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
        , check_fsm_crash_reestablish/1
        , check_invalid_reestablish_password/1
        , check_password_is_changeable/1
        , attach_initiator/1
        , attach_responder/1
        , initiator_spend/1
        , responder_spend/1
        , client_reconnect_initiator/1
        , client_reconnect_responder/1
        , request_unknown_bh/1
        , request_too_new_bh/1
        , request_too_old_bh/1
        , positive_bh/1
        ]).

%% exports for aehttp_integration_SUITE
-export([ create_channel_on_port/1
        ]).

-export([with_trace/3]).  % mostly to avoid warning if not used

-include_lib("common_test/include/ct.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").

-define(TIMEOUT, 10000).
-define(LONG_TIMEOUT, 60000).
-define(PORT, 9325).
-define(CHANNEL_CREATION_RETRIES, 3).

-define(BOGUS_PUBKEY, <<12345:32/unit:8>>).
-define(BOGUS_PRIVKEY, <<12345:64/unit:8>>).
-define(BOGUS_BLOCKHASH, <<42:32/unit:8>>).

-define(PEEK_MSGQ, peek_message_queue(?LINE, Debug)).

-define(MINIMUM_DEPTH, 10).
-define(MINIMUM_DEPTH_FACTOR, 0).

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
                             , {group, pinned_env}
                             , {group, generalized_accounts}
                             , {group, no_kdf_mock}
                             ]},
     {transactions, [sequence],
      [
        create_channel
      , multiple_responder_keys_per_port
      , channel_insufficent_tokens
      , channel_invalid_state_password
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
      , withdraw_high_amount_static_confirmation_time
      , withdraw_high_amount_short_confirmation_time
      , withdraw_low_amount_long_confirmation_time
      , withdraw_low_amount_long_confirmation_time_negative_test
      , channel_detects_close_solo
      , leave_reestablish
      , leave_reestablish_close
      , check_password_is_changeable
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
      , check_fsm_crash_reestablish
      , check_invalid_reestablish_password
      ]},
     {signing_abort, [sequence],
      [
        update_with_signing_abort
      , deposit_with_signing_abort
      , withdraw_with_signing_abort
      , shutdown_with_signing_abort
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
      , client_reconnect_responder
      ]},
     {pinned_env, [sequence],
      [ request_unknown_bh
      , request_too_new_bh
      , request_too_old_bh
      , positive_bh
      ]},
     {no_kdf_mock, [sequence], [create_channel]}
    ].

ga_sequence() ->
    [ {group, transactions}
    , {group, errors}
    , {group, signatures}
    , {group, channel_ids}
    , {group, pinned_env}
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
                 {bench_rounds, 1} %% a lower amount than the default 10
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
init_per_group(no_kdf_mock, Config) ->
    Cfg = init_per_group_(Config),
    aecore_suite_utils:end_mock(aecore_suite_utils:node_name(dev1), sc_cache_kdf),
    Cfg;
init_per_group(_Group, Config) ->
    init_per_group_(Config).

init_per_group_(Config) ->
    case proplists:get_value(ga_group, Config, false) of
        true -> Config;
        false ->
            aecore_suite_utils:start_node(dev1, Config),
            aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1), [block_pow, sc_cache_kdf]),
            log("dev1 connected", []),
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
            ?_catch_(error, Reason, Trace)
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
    [{debug, false} | Config1].

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
    %% with_trace(fun t_create_channel_/1, Cfg, "create_channel").
    t_create_channel_(Cfg).

multiple_responder_keys_per_port(Cfg) ->
    Slogan = ?SLOGAN,
    Debug = get_debug(Cfg),
    {_, Responder2} = lists:keyfind(responder2, 1, Cfg),
    log(Debug, "Responder2 = ~p", [Responder2]),
    Cfg2 = lists:keyreplace(responder, 1, Cfg, {responder, Responder2}),
    Me = self(),
    Initiator = maps:get(pub, ?config(initiator, Cfg)),
    InitiatorAccountType = account_type(Initiator),
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Initiator]),
    MinerHelper = spawn_miner_helper(),
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
            C = create_multi_channel(ChannelCfg, #{ mine_blocks => {ask, MinerHelper},
                                                  , debug => Debug }, false),
            %% [{C, Tx}] = collect_acks_w_payload([C], mine_blocks, 1),
            %% TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)),
            %% mine_blocks_until_txs_on_chain(dev1, [TxHash]),
            C
        end,
    %% We create the channels one at a time, waiting for acknowledgement that
    %% the whole channel_create() completed. This is to ensure that the GAs
    %% are initialized in order (they depend on the nonces).
    C1 = CreateMultiChannel(1, Cfg),
    collect_acks([C1], channel_ack, 1),
    C2 = CreateMultiChannel(2, Cfg2),
    collect_acks([C2], channel_ack, 2),
    Cs = [C1, C2],
    %% mine_blocks(dev1, ?MINIMUM_DEPTH),
    %% Cs = collect_acks(Cs, channel_ack, 2),
    log(Debug, "channel pids collected: ~p", [Cs]),
    %% At this point, we know the pairing worked
    [begin
         MRef = erlang:monitor(process, P),
         log(Debug, "P (~p) info: ~p", [P, process_info(P)]),
         P ! die,
         receive {'DOWN', MRef, _, _, _} -> ok
         after 5000 ->
                 log(Debug, "timed out: ~p", [process_info(self(), messages)]),
                 erlang:error({channel_not_dying, P})
         end
     end || P <- Cs],
    ok = stop_miner_helper(MinerHelper),
    ok.

-record(miner, { parent
               , mining = false
               , height
               , requests = [] }).

stop_miner_helper(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {self(), done},
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    after ?TIMEOUT ->
            erlang:error(timeout)
    end.

spawn_miner_helper() ->
    Parent = self(),
    spawn(fun() ->
                  MiningRate = aecore_suite_utils:expected_mine_rate(),
                  ok = rpc(
                         dev1, application, set_env, [aecore, expected_mine_rate, MiningRate],
                         5000),
                  SubRes =aecore_suite_utils:subscribe(aecore_suite_utils:node_name(dev1), top_changed),
                  ct:pal("SubRes = ~p", [SubRes]),
                  Height = current_height(dev1),
                  miner_loop(#miner{parent = Parent, height = Height})
          end).

miner_loop(#miner{parent = Parent} = St) ->
    ct:pal("miner_loop(~p)", [St]),
    receive
        {Parent, done} ->
            ct:pal("miner helper done", []),
            if St#miner.mining ->
                    stop_mining();
               true ->
                    ok
            end,
            exit(normal);
        {From, mine_until_txs_on_chain, Txs} ->
            case txs_not_already_on_chain(Txs) of
                [] ->
                    ct:pal("Txs already on-chain: ~p", [Txs]),
                    From ! {self(), txs_on_chain};
                [_|_] = TxsLeft ->
                    St1 = add_mining_req({From, {txs, TxsLeft}}, St),
                    miner_loop(St1)
            end;
        {gproc_ps_event, top_changed, #{ info := #{ height := NewH } }} ->
            ct:pal("top_changed; new height = ~p", [NewH]),
            St1 = check_if_reqs_fulfilled(NewH, St),
            miner_loop(St1);
        {From, mine_blocks, N, #{current_height := CurH}} ->
            WantedHeight = CurH + N,
            St1 = if St#miner.height >= WantedHeight ->
                          From ! {self(), blocks_mined},
                          St;
                     true ->
                          add_mining_req({From, {height, WantedHeight}}, St)
                    end,
            miner_loop(St1);
        Other ->
            ct:pal("miner loop got Other = ~p", [Other]),
            miner_loop(St)
    end.

start_mining() ->
    ct:pal("start mining", []),
    ok = rpc(dev1, aec_conductor, start_mining, [#{strictly_follow_top => true}], 5000).

stop_mining() ->
    ct:pal("stop mining", []),
    rpc(dev1, aec_conductor, stop_mining, [], 5000).


add_mining_req(Req, #miner{requests = Reqs, mining = Mining} = St) ->
    Mining1 = if (Reqs == []) andalso (not Mining) ->
                      start_mining(),
                      true;
                 true ->
                      Mining
              end,
    St#miner{requests = [Req | Reqs], mining = Mining1}.

check_if_reqs_fulfilled(NewH, #miner{requests = Reqs, mining = Mining} = St) ->
    Reqs1 = lists:flatmap(
              fun({From, {height, H}} = Req) ->
                      if H =< NewH ->
                              From ! {self(), blocks_mined},
                              [];
                         true ->
                              [Req]
                      end;
                 ({From, {txs, Txs}}) ->
                      case txs_not_already_on_chain(Txs) of
                          [] ->
                              From ! {self(), txs_on_chain},
                              [];
                          TxsLeft ->
                              [{From, {txs, TxsLeft}}]
                      end
              end, Reqs),
    Mining1 = if (Reqs1 == []) andalso Mining ->
                      stop_mining(),
                      false;
                 true ->
                      Mining
              end,
    St#miner{requests = Reqs1, mining = Mining1}.

txs_not_already_on_chain(Txs) ->
    lists:filter(
      fun(TxHash) ->
              not is_binary(rpc(dev1, aec_chain, find_tx_location, [TxHash]))
      end, Txs).

t_create_channel_(Cfg) ->
    #{ i := #{channel_id := ChannelId} = I
     , r := #{} = R} = create_channel_([?SLOGAN|Cfg]),

    {ok, _} = rpc(dev1, aec_chain, get_channel, [ChannelId]),
    shutdown_(I, R, Cfg),
    check_info(20).

channel_insufficent_tokens(Cfg) ->
    Debug = get_debug(Cfg),
    Test =
        fun(IAmt, RAmt, ChannelReserve, PushAmount, Error) ->
            Params =
                channel_spec([{initiator_amount, IAmt},
                              {responder_amount, RAmt},
                              ?SLOGAN|Cfg],
                              ChannelReserve, PushAmount),
            channel_create_invalid_spec(Params, Error, Debug)
        end,
    Test(10, 10, 5, 6, insufficient_initiator_amount),
    Test(10, 1, 5, 3, insufficient_responder_amount),
    Test(1, 1, 5, 3, insufficient_amounts),
    ok.

channel_invalid_state_password(Cfg) ->
    Debug = get_debug(Cfg),
    Test =
        fun(Password, Error) ->
            Params = channel_spec([{initiator_password, Password},
                                         {responder_password, Password},
                                         ?SLOGAN|Cfg]),
            channel_create_invalid_spec(Params, Error, Debug)
        end,
    Test("A", invalid_password),
    Test("12345", invalid_password),
    case aect_test_utils:latest_protocol_version() >= ?LIMA_PROTOCOL_VSN of
        true ->
            Test(ignore, password_required_since_lima);
        _ ->
            ok
    end.

channel_create_invalid_spec({I, R, Spec}, Error, Debug) ->
    Port = 9325,
    SpecR = move_password_to_spec(R, Spec),
    {error, Error} =
        rpc(dev1, aesc_fsm, respond, [Port, SpecR], Debug),
    SpecI = move_password_to_spec(I, Spec),
    {error, Error} =
        rpc(dev1, aesc_fsm, initiate, ["localhost", Port, SpecI], Debug).

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
              fun(#{info := #{info := <<"i2r hello">>}}) -> ok end, 1000),

    shutdown_(I, R, Cfg),
    check_info(20).

upd_transfer(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI, channel_id := ChannelId} = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                           [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {I0, R0} = do_update(PubI, PubR, 2, I, R, Debug, Cfg),
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
    check_info(20),
    ok.

update_with_conflict(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

update_with_soft_reject(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

deposit_with_conflict(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

deposit_with_soft_reject(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

withdraw_with_conflict(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

withdraw_with_soft_reject(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

upd_dep_with_conflict(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

upd_wdraw_with_conflict(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

dep_wdraw_with_conflict(Cfg) ->
    Debug = get_debug(Cfg),
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
    check_info(20),
    shutdown_(I, R, Cfg),
    ok.

update_with_signing_abort(Cfg) ->
    op_with_signing_abort(
      upd_transfer,
      fun(Fsm, Amt, #{spec := #{initiator := PubI, responder := PubR}}) ->
              [Fsm, PubI, PubR, Amt]
      end, update, update, update_ack, Cfg).

deposit_with_signing_abort(Cfg) ->
    op_with_signing_abort(upd_deposit, fun(Fsm, Amt, _) ->
                                               [Fsm, #{amount => Amt}]
                                       end, deposit_tx, deposit_created, deposit_created, Cfg).

withdraw_with_signing_abort(Cfg) ->
    op_with_signing_abort(upd_withdraw, fun(Fsm, Amt, _) ->
                                                [Fsm, #{amount => Amt}]
                                        end, withdraw_tx, withdraw_created, withdraw_created, Cfg).

shutdown_with_signing_abort(Cfg) ->
    op_with_signing_abort(shutdown, fun(Fsm, _, _) ->
                                            [Fsm]
                                    end, shutdown, shutdown, shutdown_ack, Cfg).

op_with_signing_abort(Op, Args, SignTag1, RptTag2, SignTag2, Cfg) ->
    Debug = get_debug(Cfg),
    ErrorCode = 147,
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = Spec = create_channel_(
                                                  [?SLOGAN|Cfg]),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    %%
    %% Initiator client aborts
    %%
    rpc(dev1, aesc_fsm, Op, Args(FsmI, 1, Spec)),
    abort_signing_request(SignTag1, I, ErrorCode, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I, #{info => #{error_code => ErrorCode,
                                                        round => Round0}},
                               ?TIMEOUT, Debug),
    wait_for_open(FsmI, Debug),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    [] = check_info(0, Debug),
    %%
    %% Responder client aborts
    %%
    ErrorCode2 = 148,
    rpc(dev1, aesc_fsm, Op, Args(FsmI, 2, Spec)),
    {_, _} = await_signing_request(SignTag1, I, Debug, Cfg),
    {ok, _} = receive_from_fsm(info, R, RptTag2, ?TIMEOUT, Debug),
    abort_signing_request(SignTag2, R, ErrorCode2, ?TIMEOUT, Debug),
    Pat = #{info => #{error_code => ErrorCode2,
                      round => Round0}},
    {ok, _} = receive_from_fsm(conflict, I, Pat, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R, Pat, ?TIMEOUT, Debug),
    wait_for_open(FsmI, Debug),
    wait_for_open(FsmR, Debug),
    ?PEEK_MSGQ,
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    ?PEEK_MSGQ,
    shutdown_(I, R, Cfg),
    ok.

signing_req() ->
    fun(#{type := sign}) -> true;
       (_) -> false
    end.

any_msg() ->
    fun(_) -> true end.

update_bench(I, R, C0) ->
    set_proxy_debug(false, I),
    set_proxy_debug(false, R),
    C = set_debug(false, C0),
    Rounds = proplists:get_value(bench_rounds, C, 1000),
    log("=== Starting benchmark ===", []),
    {Time, I1, R1} = do_n(Rounds, fun update_volley/3,
                          cache_account_type(I),
                          cache_account_type(R), C),
    Fmt = "Time (1*2*" ++ integer_to_list(Rounds) ++ "): ~.1f s; ~.1f mspt; ~.1f tps",
    Args = [Time/1000, Time/(2*Rounds), 2000*Rounds/Time],
    log(Fmt, Args),
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
    Opts = case aect_test_utils:latest_protocol_version() < ?LIMA_PROTOCOL_VSN of
               false -> #{meta => [<<"meta1">>, <<"meta2">>]};
               true  -> #{}
           end,
    case rpc(dev1, aesc_fsm, upd_transfer, [FsmI, From, To, Amount, Opts], Debug) of
        {error, _} = Error ->
            throw(Error);
        ok ->
            {I1, _} = await_signing_request(update, I, Debug, Cfg),
            {R1, _} = await_signing_request(update_ack, R, Debug, Cfg),
            %% TODO: Refactor remaining check_info calls - this one needed to be refactored together
            %% TODO: with state cache encryption because the tests failed randomly because of it
            await_update_incoming_report(R1, ?TIMEOUT, Debug),
            I2 = await_update_report(I1, ?TIMEOUT, Debug),
            R2 = await_update_report(R1, ?TIMEOUT, Debug),
            {I2, R2}
    end.

msg_volley(#{fsm := FsmI, pub := PubI} = I, #{fsm := FsmR, pub := PubR} = R, _) ->
    rpc(dev1, aesc_fsm, inband_msg, [FsmI, PubR, <<"ping">>], false),
    %% A 1 second timeout might not be enough if there is a lot of CPU intensive
    %% background processes in the OS, bump to 4 seconds to avoid random failures
    {ok,_} = receive_from_fsm(
               message, R,
               fun(#{info := #{info := <<"ping">>}}) -> ok end, 1000),
    rpc(dev1, aesc_fsm, inband_msg, [FsmR, PubI, <<"pong">>]),
    {ok,_} = receive_from_fsm(
               message, I,
               fun(#{info := #{info := <<"pong">>}}) -> ok end, 1000),
    {I, R}.

deposit(Cfg) ->
    Debug = get_debug(Cfg),
    Deposit = 10,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    log(Debug, "I = ~p", [I]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, Round0 = 1} = check_fsm_state(FsmI),
    check_info(0),
    {ok, I1, R1} = deposit_(I, R, Deposit, Round0, Debug, Cfg),
    check_info(20),
    shutdown_(I1, R1, Cfg),
    ok.

deposit_(#{fsm := FsmI} = I, R, Deposit, Debug, Cfg) ->
    {_IAmt0, _RAmt0, _, Round0} = check_fsm_state(FsmI),
    deposit_(I, R, Deposit, Round0, Debug, Cfg).

deposit_(#{fsm := FsmI} = I, R, Deposit, Round1, Debug, Cfg) ->
    deposit_(#{fsm := FsmI} = I, R, Deposit, #{}, Round1, Debug, Cfg).

deposit_(#{fsm := FsmI} = I, R, Deposit, Opts, Round1, Debug, Cfg) ->
    ok = rpc(dev1, aesc_fsm, upd_deposit, [FsmI, Opts#{amount => Deposit}]),
    {#{channel_id := ChannelId} = I1, _} = await_signing_request(deposit_tx, I, Cfg),
    receive_from_fsm(info, R, deposit_created, ?TIMEOUT, Debug),
    {R1, _} = await_signing_request(deposit_created, R, Cfg),
    ?PEEK_MSGQ,
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, ?TIMEOUT), % same tx
    ?PEEK_MSGQ,
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    SignedTx = await_on_chain_report(I1, #{info => channel_changed}, ?TIMEOUT), % same tx
    SignedTx = await_on_chain_report(R1, #{info => channel_changed}, ?TIMEOUT), % same tx
    {IAmt0, RAmt0, _, _Round1} = check_fsm_state(FsmI),
    Round2 = Round1 + 1,
    ?PEEK_MSGQ,
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    await_own_deposit_locked(I, ?TIMEOUT, Debug),
    await_own_deposit_locked(R, ?TIMEOUT, Debug),
    await_deposit_locked(I, ?TIMEOUT, Debug),
    await_deposit_locked(R, ?TIMEOUT, Debug),
    {ok, Channel} = rpc(dev1, aec_chain, get_channel, [ChannelId]),
    {aesc_deposit_tx, DepositTx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    ChannelRound = aesc_channels:round(Channel),
    DepositRound = aesc_deposit_tx:round(DepositTx),
    log(Debug, "Channel on-chain round ~p, expected round ~p", [ChannelRound,
                                                            DepositRound]),
    {ChannelRound, ChannelRound} = {ChannelRound, DepositRound},
    {IAmt, RAmt0, StateHash, Round2} = check_fsm_state(FsmI),
    {IAmt, _} = {IAmt0 + Deposit, IAmt}, %% assert correct amounts
    Round2 = aesc_deposit_tx:round(DepositTx), %% assert correct round
    StateHash = aesc_deposit_tx:state_hash(DepositTx), %% assert correct state hash
    log(Debug, "I1 = ~p", [I1]),
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I1,
    Expected = {IAmt2, RAmt2},
    {Expected, Expected} = {{IAmt0 + Deposit, RAmt0}, Expected},
    I2 = await_update_report(I1, ?TIMEOUT, Debug),
    R2 = await_update_report(R1, ?TIMEOUT, Debug),
    #{signed_tx := SignedTx} = I2, %% Same Tx
    #{signed_tx := SignedTx} = R2, %% Same Tx
    {ok, I2, R2}.

withdraw(Cfg) ->
    % Use default values for minimum depth calculation
    Cfg1 = [?SLOGAN | Cfg],
    Amount = 2,
    MinDepth = 3,
    MinDepthChannel = 5,
    Round = 1,
    {ok, _I, _R} = withdraw_full_cycle_(Amount, #{}, MinDepth, MinDepthChannel, Round, Cfg1),
    ok.

withdraw_high_amount_static_confirmation_time(Cfg) ->
    Cfg1 = [ ?SLOGAN
           % Factor of 0 sets min depths to 1 for all amounts
           , {minimum_depth, 0}
           ] ++ Cfg,
    Amount = 300000,
    MinDepth = 1,
    MinDepthChannel = 1,
    Round = 1,
    {ok, _I, _R} = withdraw_full_cycle_(Amount, #{}, MinDepth, MinDepthChannel, Round, Cfg1),
    ok.

withdraw_high_amount_short_confirmation_time(Cfg) ->
    Cfg1 = [ ?SLOGAN
           % High amount and high factor should lead to single block required
           , {minimum_depth, 20}
           ] ++ Cfg,
    Amount = 300000,
    MinDepth = 1,
    MinDepthChannel = 1,
    Round = 1,
    {ok, _I, _R} = withdraw_full_cycle_(Amount, #{}, MinDepth, MinDepthChannel, Round, Cfg1),
    ok.

withdraw_low_amount_long_confirmation_time(Cfg) ->
    Cfg1 = [ ?SLOGAN
           % Low amount and low factor should lead to longer confirmation time
           % increased confirmation times
           , {minimum_depth, 4}
           ] ++ Cfg,
    Amount = 1,
    MinDepth = 5,
    MinDepthChannel = 10,
    Round = 1,
    {ok, _I, _R} = withdraw_full_cycle_(Amount, #{}, MinDepth, MinDepthChannel, Round, Cfg1),
    ok.

withdraw_low_amount_long_confirmation_time_negative_test(Cfg) ->
    Cfg1 = [ ?SLOGAN
           % Low amount and high factor should lead to single block required
           % increased confirmation times
           , {minimum_depth, 20}
           ] ++ Cfg,
    Amount = 1,
    MinDepth = 1,
    MinDepthChannel = 1,
    Round = 1,
    {ok, _I, _R} = withdraw_full_cycle_(Amount, #{}, MinDepth, MinDepthChannel, Round, Cfg1),
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
    log(Debug, "Expected TTL = ~p", [TTL]),
    SignedTx = await_on_chain_report(I1, #{info => solo_closing}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, #{info => solo_closing}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I1, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun closing/1, ?TIMEOUT, Debug),
    check_info(20),
    settle_(LockPeriod, maps:get(minimum_depth, Spec), I1, R, Debug, Cfg),
    check_info(20),
    ok.

close_solo_tx(#{ fsm        := Fsm
               , channel_id := ChannelId }, Payload) ->
    {ok, #{ round      := Round
          , initiator  := IPubKey
          , responder  := RPubKey
          , round      := Round }} = St = rpc(dev1, aesc_fsm, get_state, [Fsm]),
    log("St = ~p", [St]),
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
    %% with_trace(fun t_leave_reestablish_/1, Cfg, "leave_reestablish").
    t_leave_reestablish_(Cfg).

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
    log(Debug, "I = ~p", [I]),
    ChId = maps:get(channel_id, I),
    LeaveAndReestablish =
        fun(Idx, #{i := ILocal, r := RLocal}) ->
            log(Debug, "starting attempt ~p", [Idx]),
            Cache1 = cache_status(ChId),
            [_, _] = in_ram(Cache1),
            [] = on_disk(Cache1),
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
                          [_, _] = on_disk(Cache2)
                  end),
            %%
            %% reestablish
            %%
            ChId = maps:get(channel_id, RLocal),
            mine_key_blocks(dev1, 6), % min depth at 4, so more than 4
            log(Debug, "reestablishing ...", []),
            Res = reestablish(ILocal, RLocal, SignedTx, Spec0, ?PORT, Debug),
            log(Debug, "ending attempt ~p", [Idx]),
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
    [_, _] = in_ram(Cache1),
    [] = on_disk(Cache1),
    shutdown_(I1, R1, Cfg),
    retry(3, 100,
          fun() ->
                  Cache2 = cache_status(ChId),
                  [] = in_ram(Cache2),
                  [_, _] = on_disk(Cache2)
          end),
    mine_blocks(dev1, 5),
    Cache3 = cache_status(ChId),
    [] = in_ram(Cache3),
    [] = on_disk(Cache3).


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
    check_info(20).

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
    log("ERROR: Expected ~p in log; got ~p", [H, H1]),
    error(log_inconsistent);
check_log([_|_], []) ->
    %% the log is a sliding window; events may be flushed at the tail
    ok;
check_log([], _) ->
    ok.

died_normal(#{info := {died,normal}}) -> ok.

died_subverted(#{info := {died,_}}) -> ok.

closing(#{info := closing} = Msg) ->
    log("matches #{info := closing} - ~p", [Msg]),
    ok.

multiple_channels(Cfg) ->
    multiple_channels_t(10, 9360, {transfer, 100}, ?SLOGAN, Cfg).

many_chs_msg_loop(Cfg) ->
    multiple_channels_t(10, 9400, {msgs, 100}, ?SLOGAN, Cfg).

multiple_channels_t(NumCs, FromPort, Msg, {slogan, Slogan}, Cfg) ->
    Debug = get_debug(Cfg),
    log(Debug, "spawning ~p channels", [NumCs]),
    Initiator = maps:get(pub, ?config(initiator, Cfg)),
    log(Debug, "Initiator: ~p", [Initiator]),
    Me = self(),
    Node = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, NumCs),
    MinerHelper = spawn_miner_helper(),
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Initiator]),
    Cs = [create_multi_channel([{port, FromPort},
                                {ack_to, Me},
                                {nonce, Nonce + N - 1},
                                {minimum_depth, 0},
                                {slogan, {Slogan,N}} | Cfg], #{mine_blocks => {ask, MinerHelper},
                                                               debug => Debug})
          || N <- lists:seq(1, NumCs)],
    log(Debug, "channels spawned", []),
    Cs = collect_acks(Cs, channel_ack, NumCs),
    log(Debug, "channel pids collected: ~p", [Cs]),
    [P ! Msg || P <- Cs],
    T0 = erlang:system_time(millisecond),
    Cs = collect_acks(Cs, loop_ack, NumCs),
    T1 = erlang:system_time(millisecond),
    Time = T1 - T0,
    Transfers = NumCs*2*100,
    Fmt = "Time (~w*2*100) ~.1f s: ~.1f mspt; ~.1f tps",
    Args = [NumCs, Time/1000, Time/Transfers, (Transfers*1000)/Time],
    log(Debug, Fmt, Args),
    ct:comment(Fmt, Args),
    [P ! die || P <- Cs],
    ok = stop_miner_helper(MinerHelper),
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
    Debug = get_debug(Cfg),
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
    Debug = get_debug(Cfg),
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
    InitialPort = proplists:get_value(port, Cfg, ?PORT),
    Test =
        fun({Depositor, Malicious}, CurPort) ->
            Cfg1 = load_idx(Cfg),
            Debug = get_debug(Cfg),
            #{ i := #{pub := IPub, fsm := FsmI} = I
            , r := #{pub := RPub, fsm := FsmR} = R
            , spec := Spec} = create_channel_([{port, CurPort}, ?SLOGAN|Cfg1]),
            Data = {I, R, Spec, CurPort, Debug},
            Deposit =
                fun() ->
                    Fun(Data, Depositor, Malicious,
                        {upd_transfer, [IPub, RPub, 1], update, update_ack})
                end,
            Deposit(),
            {AliveFsm, OtherFsm} =
                case Depositor of
                    initiator -> {FsmI, FsmR};
                    responder -> {FsmR, FsmI}
                end,
            ok = gen_statem:stop(AliveFsm),
            receive_dying_declaration(AliveFsm, OtherFsm, Debug),
            bump_idx(),
            CurPort
          end,
    Roles = [initiator, responder],
    Combinations = [{Depositor, Malicious} || Depositor <- Roles,
                                              Malicious <- Roles],
    lists:foldl(Test, InitialPort, Combinations),
    ok.

receive_dying_declaration(Fsm, OtherFsm, Debug) ->
    TRef = erlang:send_after(?TIMEOUT, self(), {dying_declaration, ?LINE}),
    receive_dying_(Fsm, TRef, false, false, Debug),
    receive
        {aesc_fsm, OtherFsm, #{ tag := info
                              , type := report
                              , info := peer_disconnected }} ->
            ok
    after ?TIMEOUT ->
            error(timeout)
    end.

receive_dying_(_, TRef, true, true, _) ->
    erlang:cancel_timer(TRef),
    ok;
receive_dying_(Fsm, TRef, Died, Log, Debug) ->
    receive
        {aesc_fsm, Fsm, #{info := {died, _}} = Msg} ->
            log(Debug, "from ~p: ~p", [Fsm, Msg]),
            receive_dying_(Fsm, TRef, true, Log, Debug);
        {aesc_fsm, Fsm, #{info := {log, _}} = Msg} ->
            log(Debug, "from ~p: ~p", [Fsm, Msg]),
            receive_dying_(Fsm, TRef, Died, true, Debug);
        {timeout, TRef, Msg} ->
            error({timeout, Msg})
    end.

check_incorrect_mutual_close(Cfg) ->
    config(Cfg),
    Fun = proplists:get_value(wrong_action_detailed, Cfg),
    Test =
        fun(Depositor, Malicious) ->
            Cfg1 = load_idx(Cfg),
            Debug = get_debug(Cfg),
            [] = check_info(0, Debug), %% ensure no hanging messages
            #{ i := I
             , r := R
             , spec := Spec} = create_channel_([?SLOGAN|Cfg1]),
            Port = proplists:get_value(port, Cfg, ?PORT),
            Data = {I, R, Spec, Port, Debug},
            Fun(Data, Depositor, Malicious,
                {shutdown, [], shutdown,
                 shutdown_ack},
                fun(#{fsm := FsmPid}, Debug1) ->
                    log(Debug1, "checking state of ~p (Depositor=~p, Malicious=~p, FsmI = ~p, FsmR = ~p)",
                        [FsmPid, Depositor, Malicious, maps:get(fsm,I), maps:get(fsm,R)]),
                        case Depositor =:= Malicious of
                            true ->
                                mutual_closing = fsm_state(FsmPid, Debug);
                            false ->
                                mutual_closed = fsm_state(FsmPid, Debug)
                        end,
                    {ok, _} = receive_from_fsm(conflict, I, any_msg(), ?TIMEOUT, Debug),
                    {ok, _} = receive_from_fsm(conflict, R, any_msg(), ?TIMEOUT, Debug)
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
    timer:sleep(50),
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
    SignTimeout = 2000,
    Spec1 = Spec#{
        timeouts => #{
            idle => 20000,
            sign => SignTimeout,
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
            timer:sleep(SignTimeout + 100), %% For now just wait for a timeout
            channel_closing = fsm_state(FsmI, Debug);
        false ->
            % Test that timeouts do not kill the FSM
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmR]),
            timer:sleep(SignTimeout + 100),
            channel_closing = fsm_state(FsmI, Debug),
            channel_closing = fsm_state(FsmR, Debug),

            % Test that after sending the SHUTDOWN message and timing out we
            % are still alive
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
            {_, _} = await_signing_request(shutdown, I, Cfg),
            {ok, _} = receive_from_fsm(info, R, shutdown, ?TIMEOUT, Debug),
            timer:sleep(SignTimeout + 100),
            channel_closing = fsm_state(FsmI, Debug),
            channel_closing = fsm_state(FsmR, Debug),

            % Test that closing works
            check_info(20),
            shutdown_(I, R, [{already_closing, true}|Cfg])
    end,
    ok.

check_fsm_crash_reestablish(Cfg) ->
    Debug = get_debug(Cfg),
    {I0, R0, Spec0} = channel_spec(Cfg),
    #{ i := I, r := R } = create_channel_from_spec(I0, R0, Spec0, ?PORT, Debug, Cfg),
    ct:log("I = ~p", [I]),
    ct:log("R = ~p", [R]),
    {_, I1, R1} = do_n(4, fun update_volley/3, I, R, Cfg),
    Actions = [
        fun fsm_crash_action_during_transfer/3,
        fun fsm_crash_action_during_withdraw/3,
        fun fsm_crash_action_during_deposit/3,
        fun fsm_crash_action_during_shutdown/3
    ],
    lists:foldl(fun (Action, {IA, RA}) ->
        {IA1, RA1} = fsm_crash_reestablish(IA, RA, Spec0, Cfg, Action),
        {_, IA2, RA2} = do_n(4, fun update_volley/3, IA1, RA1, Cfg),
        {IA2, RA2}
    end, {I1, R1}, Actions).

fsm_crash_reestablish(#{channel_id := ChId, fsm := FsmI} = I, #{fsm := FsmR} = R, Spec, Cfg, Action) ->
    Debug = get_debug(Cfg),
    {ok, State} = rpc(dev1, aesc_fsm, get_offchain_state, [FsmI]),
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    ct:log("Performing action before crashing the FSM"),
    ok = Action(I, R, Cfg),
    ct:log("Simulating random crash"),
    erlang:exit(FsmI, test_fsm_random_crash),
    erlang:exit(FsmR, test_fsm_random_crash),
    timer:sleep(20), %% Give some time for the cache to persist the state

    Cache = cache_status(ChId),
    [] = in_ram(Cache),
    [_, _] = on_disk(Cache),
    check_info(20),
    ct:log("reestablishing ...", []),
    #{i := I1, r := R1} = reestablish(I, R, SignedTx, Spec, ?PORT, Debug),
    {I1, R1}.

fsm_crash_action_during_transfer( #{fsm := FsmI, pub := PubI} = I
                                , #{pub := PubR} = R
                                , Cfg) ->
    Debug = get_debug(Cfg),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, PubI, PubR, 1], Debug),
    {_, _} = await_signing_request(update, I, Debug, Cfg),
    {ok, _} = receive_from_fsm(update_ack, R, signing_req(), ?TIMEOUT, Debug),
    ok.

fsm_crash_action_during_withdraw( #{fsm := FsmI} = I
                                , R
                                , Cfg) ->
    Debug = get_debug(Cfg),
    rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, #{amount => 1}], Debug),
    {_, _} = await_signing_request(withdraw_tx, I, Debug, Cfg),
    {ok, _} = receive_from_fsm(withdraw_created, R, signing_req(), ?TIMEOUT, Debug),
    ok.

fsm_crash_action_during_deposit( #{fsm := FsmI} = I
                               , R
                               , Cfg) ->
    Debug = get_debug(Cfg),
    rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => 1}], Debug),
    {_, _} = await_signing_request(deposit_tx, I, Debug, Cfg),
    {ok, _} = receive_from_fsm(deposit_created, R, signing_req(), ?TIMEOUT, Debug),
    ok.

fsm_crash_action_during_shutdown( #{fsm := FsmI} = I
                                , R
                                , Cfg) ->
    Debug = get_debug(Cfg),
    rpc(dev1, aesc_fsm, shutdown, [FsmI], Debug),
    {_, _} = await_signing_request(shutdown, I, Debug, Cfg),
    {ok, _} = receive_from_fsm(shutdown_ack, R, signing_req(), ?TIMEOUT, Debug),
    ok.

check_invalid_reestablish_password(Cfg) ->
    Debug = get_debug(Cfg),
    {I0, R0, Spec0} = channel_spec(Cfg),
    #{ i := I
     , r := R } = create_channel_from_spec(I0, R0, Spec0, ?PORT, Debug, Cfg),
    ct:log("I = ~p", [I]),
    ct:log("R = ~p", [R]),
    {_, I1, R1} = do_n(4, fun update_volley/3, I, R, Cfg),
    {ok, SignedTx} = leave_channel(I1, R1, Debug),

    %% Reestablish with wrong passwords
    #{state_password := PwI} = I1,
    #{state_password := PwR} = R1,
    reestablish_wrong_password( I1#{state_password => PwI ++ "_bogus"}
                              , R1#{state_password => PwR ++ "_bogus"}
                              , SignedTx, Spec0, Debug),

    %% Check that after an invalid password has been provided we can still reestablish the channel
    _ = reestablish(I1, R1, SignedTx, Spec0, ?PORT, Debug),
    ok.

check_password_is_changeable(Cfg) ->
    Debug = get_debug(Cfg),
    {I0, R0, Spec0} = channel_spec(Cfg),
    #{ i := I
     , r := R } = create_channel_from_spec(I0, R0, Spec0, ?PORT, Debug, Cfg),
    ct:log("I = ~p", [I]),
    ct:log("R = ~p", [R]),
    {_, I1, R1} = do_n(4, fun update_volley/3, I, R, Cfg),

    I2 = change_password(I1),
    R2 = change_password(R1),
    {ok, SignedTx} = leave_channel(I2, R2, Debug),

    %% Reestablish with old password should fail
    reestablish_wrong_password(I1, R1, SignedTx, Spec0, Debug),
    %% Reestablish with new password should succeed
    _ = reestablish(I2, R2, SignedTx, Spec0, ?PORT, Debug),

    ok.

change_password(#{fsm := Fsm, state_password := StatePassword} = P) ->
    StatePassword1 = StatePassword ++ "_changed",
    ok = rpc(dev1, aesc_fsm, change_state_password, [Fsm, StatePassword1]),
    P#{state_password => StatePassword1}.

leave_channel(#{fsm := FsmI} = I, R, Debug) ->
    ok = rpc(dev1, aesc_fsm, leave, [FsmI]),
    {ok,Li} = await_leave(I, ?TIMEOUT, Debug),
    {ok,Lr} = await_leave(R, ?TIMEOUT, Debug),
    SignedTx = maps:get(info, Li),
    SignedTx = maps:get(info, Lr),
    {ok,_} = receive_from_fsm(info, I, fun died_normal/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun died_normal/1, ?TIMEOUT, Debug),
    {ok, SignedTx}.

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
    {ok, FsmR} = rpc(dev1, aesc_fsm, respond, [Port, move_password_to_spec(R, Spec)], Debug),
    {ok, FsmI} = rpc(dev1, aesc_fsm, initiate, ["localhost", Port, move_password_to_spec(I, Spec)], Debug),

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
            {ok,_} = receive_from_fsm(info, I1, fun(#{info := peer_disconnected}) -> ok end,
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
            {ok,_} = receive_from_fsm(info, R1, fun(#{info := peer_disconnected}) -> ok end,
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
    log(Debug, "Testing with Poster ~p, Malicious ~p",
          [Poster, Malicious]),
    #{fsm := FsmI} = I,
    #{fsm := FsmR} = R,
    {D, A, FsmD, FsmA} =
        case Poster of
            initiator -> {I, R, FsmI, FsmR};
            responder -> {R, I, FsmR, FsmI}
        end,
    Post = fun() -> ok = rpc(dev1, aesc_fsm, FsmFun, [FsmD | FsmFunArg]) end,
    case Poster =:= Malicious of
        true ->
            Post(),
            % default behavor - FSM guards you from sending a bad event
            {_, WrongSignedTx} = MaliciousSign(FsmNewAction, D, Debug),
            {ok, _} = receive_from_fsm(error, D, ErrMsg, ?TIMEOUT, Debug),
            wait_for_open(FsmD, Debug),

            % turn default behavior off, the poster deliberatly had sent
            % invalid tx, the acknowledger must reject it
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmD, false], Debug),

            % resend the same wrong tx, this time no check from poster's
            % side
            Post(),
            aesc_fsm:signing_response(FsmD, FsmNewAction, WrongSignedTx),

            DetectConflictFun(D, Debug),
            % make sure setting back defaults if process is still there
            rpc(dev1, aesc_fsm, strict_checks, [FsmD, true], Debug);
        false ->
            Post(),
            {_, _} = await_signing_request(FsmNewAction, D, Debug, Cfg),
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmA, false], Debug),
            {_, _} = MaliciousSign(FsmCreatedAction, A, Debug),
            DetectConflictFun(A, Debug),
            rpc(dev1, aesc_fsm, strict_checks, [FsmA, true], Debug)
    end,
    check_info(20),
    ok.

wait_for_open(FsmPid, Debug) ->
    wait_for_fsm_state(open, FsmPid, 5, Debug).

wait_for_fsm_state(_St, _FsmPid, 0, _Debug) ->
    error(timeout);
wait_for_fsm_state(St, FsmPid, Retries, Debug) when Retries > 0 ->
    case fsm_state(FsmPid, Debug) of
        St ->
            ok;
        badrpc ->
            error(fsm_not_running);
        Other ->
            log(Debug, "Fsm state (~p) is ~p - retrying for ~p", [FsmPid, Other, St]),
            timer:sleep(50),
            wait_for_fsm_state(St, FsmPid, Retries-1, Debug)
    end.

shutdown_(#{fsm := FsmI, channel_id := ChannelId} = I, R, Cfg) ->
    Debug = get_debug(Cfg),
    AlreadyClosing = proplists:get_value(already_closing, Cfg, false),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    {I1, _} = await_signing_request(shutdown, I, Cfg),
    {ok, _} = receive_from_fsm(info, R, shutdown, ?TIMEOUT, Debug),
    {R1, _} = await_signing_request(shutdown_ack, R, Cfg),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx),
    verify_close_mutual_tx(SignedTx, ChannelId),
    if AlreadyClosing ->
            ok;
       true ->
            {ok,_} = receive_from_fsm(info, I1, fun closing/1, ?TIMEOUT, Debug),
            {ok,_} = receive_from_fsm(info, R, fun closing/1, ?TIMEOUT, Debug),
            ct:pal("closing", [])
    end,
    SignedTx = await_on_chain_report(I1, #{info => channel_closed}, ?TIMEOUT), % same tx
    SignedTx = await_on_chain_report(R1, #{info => channel_closed}, ?TIMEOUT), % same tx
    ct:pal("=== Mining for minimum depth, close_mutual ===", []),
    mine_blocks(dev1, ?MINIMUM_DEPTH,
                opt_add_to_debug(#{ signed_tx => SignedTx
                                  , current_height => current_height(dev1) }, Debug)),
    ct:pal("=== Mining stopped ===", []),
    {ok,_} = receive_from_fsm(info, I1, #{info => closed_confirmed}, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R1, #{info => closed_confirmed}, ?TIMEOUT, Debug),
    ct:pal("close confirmed", []),
    {ok,_} = receive_from_fsm(info, I1, #{info => {died,normal}}, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R1, #{info => {died,normal}}, ?TIMEOUT, Debug),
    ct:pal("died normal", []),
    ok.

settle_(TTL, MinDepth, #{fsm := FsmI, channel_id := ChannelId} = I, R, Debug,
       Cfg) ->
    ok = rpc(dev1, aesc_fsm, settle, [FsmI]),
    {_, SignedTx} = await_signing_request(settle_tx, I, Cfg),
    log(Debug, "settle_tx signed", []),
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
    log(Debug, "I received On-chain report: ~p", [SignedTx]),
    SignedTx = await_on_chain_report(R, #{info => channel_closed}, ?TIMEOUT), % same tx
    log(Debug, "R received On-chain report: ~p", [SignedTx]),
    verify_settle_tx(SignedTx, ChannelId),
    log(Debug, "settle_tx verified", []),
    if
        KeyBlocksMissingForMinDepth > 0 ->
            mine_key_blocks(dev1, KeyBlocksMissingForMinDepth);
        KeyBlocksMissingForMinDepth =< 0 ->
            ok
    end,
    {ok, _} = receive_from_fsm(info, I, closed_confirmed, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(info, R, closed_confirmed, ?TIMEOUT, Debug),
    log(Debug, "closed_confirmed received from both", []),
    {ok,_} = receive_from_fsm(info, I, fun died_normal/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun died_normal/1, ?TIMEOUT, Debug),
    log(Debug, "died_normal detected from both", []),
    ok.

client_reconnect_initiator(Cfg) ->
    %% with_trace(fun(Cfg1) -> client_reconnect_(initiator, Cfg1) end, Cfg, "client_reconnect_i").
    client_reconnect_(initiator, Cfg).

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
    log(Debug, "Ch = ~p", [Ch]),
    {error, _} = Err = try_reconnect(Fsm, Role, RoleI, Debug),
    log(Debug, "Reconnecting before disconnecting failed: ~p", [Err]),
    unlink(Proxy),
    exit(Proxy, kill),
    timer:sleep(50),  % give the above exit time to propagate
    ok = things_that_should_fail_if_no_client(Role, I, R, Debug, Cfg),
    Res = reconnect(Fsm, Role, RoleI, Debug),
    log(Debug, "Reconnect req -> ~p", [Res]),
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
        ?_catch_(error, E, ST)
        timer:sleep(T),
        retry(N-1, T, F, E, ST)
    end.


cache_status(ChId) ->
    rpc(dev1, aesc_state_cache, cache_status, [ChId]).

in_ram(St)  ->  proplists:get_value(in_ram, St).
on_disk(St) ->  proplists:get_value(on_disk, St).


collect_acks([Pid | Pids], Tag, N) ->
    log("collect_acks, Tag = ~p, N = ~p", [Tag, N]),
    Timeout = 60000 + (N div 10)*5000,  % wild guess
    receive
        {Pid, Tag} ->
            log("Ack from ~p (~p)", [Pid, Tag]),
            [Pid | collect_acks(Pids, Tag, N)]
    after Timeout ->
              error(timeout)
    end;
collect_acks([], _Tag, _) ->
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
    set_proxy_debug(false, I),
    set_proxy_debug(false, R),
    I1 = cache_account_type(I),
    R1 = cache_account_type(R),
    Parent ! {self(), channel_ack},
    ch_loop(I1, R1, Parent, set_debug(false, Cfg)).

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
            log("~p got die request", [self()]),
            #{ proxy := ProxyI } = I,
            #{ proxy := ProxyR } = R,
            ProxyI ! {self(), die},
            ProxyR ! {self(), die},
            exit(normal);
        Other ->
            log(get_debug(Cfg), "Got Other = ~p, I = ~p~nR = ~p", [Other, I, R]),
            ch_loop(I, R, Parent, Cfg)
    end.

create_channel_on_port(Port) ->
    Node = dev1,
    I = prep_initiator(Node),
    R = prep_responder(I, Node),
    Cfg = [{port, Port}, {initiator, I}, {responder, R},
           {initiator_amount, 500000}, {responder_amount, 500000}, ?SLOGAN],
    create_channel_(Cfg, get_debug(Cfg)).

create_channel_from_spec(I, R, Spec, Port, Debug, Cfg) ->
    create_channel_from_spec(I, R, Spec, Port, false, Debug, Cfg).

create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg) ->
    create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg, ?MINIMUM_DEPTH).

create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg, MinDepth) ->
    %% TODO: Somehow there is a CI only race condition which rarely occurs in
    %% round_too_high.check_incorrect_* and round_too_low.check_incorrect_* tests
    %% For now just wrap this operation in a retry loop and come back to it later
    {FsmI, FsmR, I1, R1} = retry(?CHANNEL_CREATION_RETRIES, 100,
        fun() ->
            RProxy = spawn_responder(Port, Spec, R, UseAny, Debug),
            IProxy = spawn_initiator(Port, Spec, I, Debug),
            log("RProxy = ~p, IProxy = ~p", [RProxy, IProxy]),
            #{ i := #{ fsm := WFsmI } = WI1
             , r := #{ fsm := WFsmR } = WR1 } = Info
                = match_responder_and_initiator(RProxy, Debug),
            log(Debug, "channel paired: ~p", [Info]),
            {WFsmI, WFsmR, WI1, WR1}
        end),
    log(Debug, "FSMs, I = ~p, R = ~p", [FsmI, FsmR]),
    {I2, R2} = try await_create_tx_i(I1, R1, Debug, Cfg)
                   ?_catch_(error, Err, ST)
                   log("Caught Err = ~p~nMessages = ~p",
                       [Err, element(2, process_info(self(), messages))]),
                   error(Err, ST)
               end,
    log(Debug, "mining blocks on dev1 for minimum depth", []),
    SignedTx = await_on_chain_report(R2, #{info => funding_created}, ?TIMEOUT),
    ct:log("=== SignedTx = ~p", [SignedTx]),
    SignedTx = await_on_chain_report(I2, #{info => funding_signed}, ?TIMEOUT), % same tx
    {ok, _} = wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    ct:log("=== Signed tx in block: ~p", [SignedTx]),
    SignedTx = await_channel_changed_report(I2, ?TIMEOUT),
    SignedTx = await_channel_changed_report(R2, ?TIMEOUT),
    CurrentHeight = current_height(dev1),
    mine_blocks(dev1, MinDepth, opt_add_to_debug(#{ signed_tx => SignedTx
                                                  , current_height => CurrentHeight }, Debug)),
    %% in case of multiple channels starting in parallel - the mining above
    %% has no effect (the blocks are mined in another process)
    %% The following line makes sure this process is blocked until the proper
    %% height is reached
    aecore_suite_utils:wait_for_height(aecore_suite_utils:node_name(dev1),
                                       CurrentHeight + MinDepth),
    log(Debug, "=== Min-depth height achieved", []),
    %% we've seen 10-15 second block times in CI, so wait a while longer

    await_own_funding_locked(I2, ?TIMEOUT, Debug),
    await_own_funding_locked(R2, ?TIMEOUT, Debug),

    % check the channel is present on-chain
    {ok, ChannelId} = aesc_utils:channel_pubkey(SignedTx),
    {ok, _} = rpc(dev1, aec_chain, get_channel, [ChannelId]),

    I3 = await_funding_locked(I2, ?TIMEOUT, Debug),
    R3 = await_funding_locked(R2, ?TIMEOUT, Debug),
    log(Debug, "=== Funding locked ===", []),
    %%
    I4 = await_update_report(I3, ?TIMEOUT, Debug),
    R4 = await_update_report(R3, ?TIMEOUT, Debug),
    log(Debug, "=== Update reports received ===", []),
    I5 = await_open_report(I4, ?TIMEOUT, Debug),
    R5 = await_open_report(R4, ?TIMEOUT, Debug),
    SignedTx = await_on_chain_report(I5, #{info => channel_changed}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R5, #{info => channel_changed}, ?TIMEOUT),
    log(Debug, "=== Open reports received ===", []),
    log(Debug, "=== Message Q: ~p", [element(2,process_info(self(), messages))]),
    [] = check_info(0, Debug),
    #{i => I5, r => R5, spec => Spec}.

spawn_responder(Port, Spec, R, UseAny, Debug) ->
    Me = self(),
    spawn(fun() ->
                       log("responder spawned: ~p", [Spec]),
                       Spec1 = maybe_use_any(UseAny, Spec#{ client => self() }),
                       Spec2 = move_password_to_spec(R, Spec1),
                       {ok, Fsm} = rpc(dev1, aesc_fsm, respond, [Port, Spec2], Debug),
                       responder_instance_(Fsm, Spec2, R, Me, Debug)
               end).

maybe_use_any(true, Spec) ->
    Spec#{initiator => any};
maybe_use_any(false, Spec) ->
    Spec.

spawn_initiator(Port, Spec, I, Debug) ->
    Me = self(),
    spawn(fun() ->
                       log("initiator spawned: ~p", [Spec]),
                       Spec1 = Spec#{ client => self() },
                       Spec2 = move_password_to_spec(I, Spec1),
                       {ok, Fsm} = rpc(dev1, aesc_fsm, initiate,
                                       ["localhost", Port, Spec2], Debug),
                       initiator_instance_(Fsm, Spec2, I, Me, Debug)
               end).

move_password_to_spec(#{state_password := StatePassword}, Spec) ->
    Spec#{state_password => StatePassword};
move_password_to_spec(_, Spec) ->
    Spec.

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
    log(Debug, "Got ChOpen: ~p~nSpec = ~p", [ChOpen, Spec]),
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
    log(Debug, "Got ChAccept: ~p~nSpec = ~p", [ChAccept, Spec]),
    #{data := #{temporary_channel_id := TmpChanId}} = ChAccept,
    I1 = I#{ proxy => self() },
    gproc:reg({n,l,{?MODULE,TmpChanId,initiator}}, #{ i => I1
                                                      , channel_accept => ChAccept}),
    {_RPid, #{ r := #{parent := NewParent}}}
    = gproc:await({n,l,{?MODULE,TmpChanId,responder}}, ?TIMEOUT),
    unlink(Parent),
    link(NewParent),
    fsm_relay(I1#{parent => NewParent}, NewParent, Debug).

set_proxy_debug(Bool, #{proxy := P}) when is_boolean(Bool) ->
    P ! {self(), debug, Bool},
    receive
        {P, debug_ack, Prev} ->
            Prev
    after ?TIMEOUT ->
              error(timeout)
    end.

-record(relay_st, {parent, debug}).

fsm_relay(Map, Parent, Debug) ->
    log(Debug, "fsm_relay(~p, ~p, Debug)", [Map, Parent]),
    fsm_relay_(Map, #relay_st{ parent = Parent
                               , debug  = Debug }).

fsm_relay_(#{ fsm := Fsm } = Map, #relay_st{ parent = Parent
                                             , debug  = Debug } = St) ->
    St1 = receive
              {aesc_fsm, Fsm, _} = Msg ->
                  log(Debug, "Relaying(~p) ~p", [Parent, Msg]),
                  Parent ! Msg,
                  St;
              {Parent, debug, NewDebug} when is_boolean(NewDebug) ->
                  log(NewDebug, "Applying new debug mode: ~p", [NewDebug]),
                  Parent ! {self(), debug_ack, Debug},
                  St#relay_st{ debug = NewDebug };
              {Parent, die} ->
                  log(Debug, "Got 'die' from parent", []),
                  aesc_fsm:stop(Fsm),
                  log(Debug, "relay stopping (die)", []),
                  exit(normal);
              Other ->
                  log(Debug, "Relay got Other: ~p", [Other]),
                  St
          end,
    fsm_relay_(Map, St1).

fsm_map(Fsm, #{ initiator_amount := IAmt
                , responder_amount := RAmt
                , push_amount      := PushAmt
                , slogan           := Slogan }, Map) ->
    Map#{ fsm    => Fsm
          , slogan => Slogan
          , initiator_amount => IAmt - PushAmt
          , responder_amount => RAmt + PushAmt }.

reestablish( #{channel_id := ChId, initiator_amount := IAmt, responder_amount := RAmt} = I
           , #{channel_id := ChId, initiator_amount := IAmt, responder_amount := RAmt} = R
           , SignedTx, Spec0, Port, Debug) ->
    Spec = Spec0#{existing_channel_id => ChId, offchain_tx => SignedTx,
                  initiator_amount => IAmt, responder_amount => RAmt},
    {ok, FsmR} = rpc(dev1, aesc_fsm, respond,
                     [Port, move_password_to_spec(R, Spec)]),
    {ok, FsmI} = rpc(dev1, aesc_fsm, initiate,
                     ["localhost", Port, move_password_to_spec(I, Spec)], Debug),
    I1 = await_open_report(I#{fsm => FsmI}, ?TIMEOUT, Debug),
    R1 = await_open_report(R#{fsm => FsmR}, ?TIMEOUT, Debug),
    check_info(20),
    #{i => I1, r => R1, spec => Spec}.

reestablish_wrong_password( #{channel_id := ChId, initiator_amount := IAmt, responder_amount := RAmt} = I
                          , #{channel_id := ChId, initiator_amount := IAmt, responder_amount := RAmt} = R
                          , SignedTx, Spec, Debug) ->
    Spec1 = Spec#{existing_channel_id => ChId, offchain_tx => SignedTx,
                  initiator_amount => IAmt, responder_amount => RAmt},
    {error, invalid_password} = rpc(dev1, aesc_fsm, respond,
                     [?PORT, move_password_to_spec(R, Spec1)]),
    {error, invalid_password} = rpc(dev1, aesc_fsm, initiate,
                     ["localhost", ?PORT, move_password_to_spec(I, Spec1)], Debug).

update_tx(Tx0, F, Args) ->
    {Mod, TxI} = aetx:specialize_callback(Tx0),
    TxI1 = apply(Mod, F, [TxI|Args]),
    aetx:new(Mod, TxI1).

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

await_funding_locked(R, Timeout, Debug) ->
    await_locked_rpt(funding_locked, R, Timeout, Debug).

await_deposit_locked(R, Timeout, Debug) ->
    await_locked_rpt(deposit_locked, R, Timeout, Debug).

await_locked_rpt(Type, #{role := Role} = R, Timeout, Debug) when Type==funding_locked;
                                                                 Type==deposit_locked;
                                                                 Type==withdraw_locked ->
    {ok, Msg} = receive_from_fsm(info, R, Type, Timeout, Debug),
    log(Debug, "~p got ~p: ~p", [Role, Type, Msg]),
    R#{channel_id => maps:get(channel_id, Msg)}.

await_update_report(#{channel_id := ChId} = R, Timeout, Debug) ->
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
    await_signing_request(Tag, R, ?TIMEOUT, false, Cfg).

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
    log(Debug, "await_signing_request, Fsm = ~p (Pub = ~p, Other = ~p)",
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

abort_signing_request(Tag, #{fsm := Fsm}, Code, Timeout, Debug) ->
    log(Debug, "waiting to abort signing req, Fsm = ~p, (Code = ~p)",
        [Fsm, Code]),
    receive {aesc_fsm, Fsm, #{type := sign, tag := Tag,
                              info := #{signed_tx := SignedTx,
                                        updates   := _}} = Msg} ->
                log(Debug, "abort_signing(~p, ~p, ~p) <- ~p", [Tag, Code, Fsm, Msg]),
                aesc_fsm:signing_response(Fsm, Tag, {error, Code}),
                {ok, SignedTx}
    after Timeout ->
              error(timeout)
    end.

sign_tx(Signer, Tx, Cfg) ->
    co_sign_tx(Signer, aetx_sign:new(Tx, []), Cfg).

co_sign_tx(Signer, SignedTx, Cfg) ->
    #{role := Role, pub := Pubkey, priv := Priv} = Signer,
    case account_type(Signer) of
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
    log("~p awaiting on-chain from ~p", [self(), Fsm]),
    receive
        {aesc_fsm, Fsm, #{info := {died, _}}} = Died ->
            log("Fsm died while waiting for on-chain report:~n"
                   "~p", [Died]),
            error(fsm_died);
        {aesc_fsm, Fsm, #{type := report, tag := on_chain_tx,
                          info := #{tx := SignedTx} = I}} = M ->
            log("OnChainRpt = ~p", [M]),
            ok = match_info(I, Match),
            SignedTx
    after Timeout ->
              error(timeout)
    end.

await_channel_changed_report(#{fsm := Fsm}, Timeout) ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                           , tag := on_chain_tx
                           , info := #{ info := channel_changed
                                        , tx := SignedTx } }} ->
            SignedTx
    after Timeout ->
              error(timeout)
    end.

await_own_funding_locked(Role, Timeout, Debug) ->
    await_own_locked_rpt(own_funding_locked, Role, Timeout, Debug).

await_own_deposit_locked(Role, Timeout, Debug) ->
    await_own_locked_rpt(own_deposit_locked, Role, Timeout, Debug).

await_own_locked_rpt(Type, #{role := Role, fsm := Fsm}, Timeout, Debug)
  when Type==own_funding_locked;
       Type==own_deposit_locked;
       Type==own_withdraw_locked ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                           , tag  := info
                           , info := Type } = Msg} ->
            log(Debug, "~p got ~p: ~p", [Role, Type, Msg]),
            ok
    after Timeout ->
              error(timeout)
    end.

match_info(Info, Match) ->
    maps:fold(fun(K,V,Acc) ->
                      case maps:find(K, Info) of
                          {ok, V1} when is_map(V), is_map(V1) ->
                              match_info(V1, V);
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

await_update_incoming_report(#{fsm := Fsm, channel_id := ChannelId}, Timeout, _Debug) ->
    receive {aesc_fsm, Fsm, #{type := report, tag := info, info := update} = Msg} ->
            {ok, ChannelId} = maps:find(channel_id, Msg)
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

receive_from_fsm(Tag, R, Info, Timeout) ->
    receive_from_fsm(Tag, R, Info, Timeout, false).

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
                    log(Debug, "Failed match: ~p", [Msg1]),
                    receive_from_fsm_(Tag, R, Msg, TRef, Debug, Cont)
            after
                erlang:cancel_timer(TRef)
            end;
        {timeout, TRef, receive_from_fsm} ->
            flush(),
            error(timeout)
    end.

flush() ->
    receive
        M ->
            log("<== ~p", [M]),
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
                    log("Message doesn't match fun: ~p / ~w:~w/1",
                        [Msg, Mod, Name]),
                    error({message_mismatch, [Msg]})
            end
    end;
match_msgs(M, #{info := M} = Msg, _) ->
    {ok, Msg};
match_msgs(M, M, _) ->
    {ok, M};
match_msgs(Pat, M, Cont) when is_map(M), is_map(Pat) ->
    try  match_info(M, Pat),
         {ok, M}
    catch
        error:_ when Cont ->
            throw(continue)
    end;
match_msgs(_, _, true) ->
    throw(continue);
match_msgs(A, B, false) ->
    log("Messages don't match: ~p / ~p", [A, B]),
    erlang:error({message_mismatch, [A, B]}).

check_info(Timeout) -> check_info(Timeout, false).

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

mine_blocks(_Node, N, #{mine_blocks := {ask, Pid}} = Opt) when is_pid(Pid) ->
    Pid ! {self(), mine_blocks, N, maps:without([mine_blocks], Opt)},
    receive
        {Pid, blocks_mined} -> ok
    after ?TIMEOUT ->
            error(timeout)
    end;
mine_blocks(_, _, #{mine_blocks := false}) ->
    ok;
mine_blocks(Node, N, _) ->
    mine_key_blocks(Node, N).

opt_add_to_debug(Map, #{mine_blocks := {ask, _}} = Debug) ->
    maps:merge(Debug, Map);
opt_add_to_debug(_, Debug) ->
    Debug.

prep_initiator(Node) ->
    {PrivKey, PubKey} = aecore_suite_utils:sign_keys(Node),
    log("initiator Pubkey = ~p", [PubKey]),
    mine_key_blocks(Node, 30),
    log("initiator: 30 blocks mined on ~p", [Node]),
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
    {ok, _} = wait_for_signed_transaction_in_block(Node, SignedTx),
    {ok, Amount} = rpc(Node, aehttp_logic, get_account_balance, [Pub]),
    #{role => responder,
      priv => Priv,
      pub  => Pub,
      balance => Amount,
      auth_idx => 1}.

rpc(Node, Mod, Fun, Args) -> rpc(Node, Mod, Fun, Args, false).

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

if_debug(true, X, _) -> X;
if_debug(#{debug := true}, X, _) -> X;
if_debug(_, _, Y) -> Y.

apply_updates([], R) ->
    R;
apply_updates([{meta, _} | T], R) ->
    apply_updates(T, R);
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
    wait_for_signed_transaction_in_block(Node, SignedTx, false).

wait_for_signed_transaction_in_block(_, SignedTx, #{mine_blocks := {ask,Pid}}) ->
    TxHash = aetx_sign:hash(SignedTx),
    Pid ! {self(), mine_until_txs_on_chain, [TxHash]},
    receive
        {Pid, txs_on_chain} ->
            ok
    after ?TIMEOUT ->
            error(timeout)
    end;
wait_for_signed_transaction_in_block(Node, SignedTx, Debug) ->
    log(Debug, "waiting for tx ~p", [SignedTx]),
    TxHash = aetx_sign:hash(SignedTx),
    case rpc(dev1, aec_chain, find_tx_location, [TxHash], Debug) of
        BH when is_binary(BH) ->
            log(Debug, "Found tx in block", []),
            ok;
        NotConfirmed when NotConfirmed =:= mempool;
                          NotConfirmed =:= not_found ->
            EncTxHash = aeser_api_encoder:encode(tx_hash, TxHash),
            case mine_blocks_until_txs_on_chain(Node, [EncTxHash]) of
                {ok, Blocks} ->
                    log(Debug, "Tx on-chain after mining ~p blocks", [length(Blocks)]),
                    {ok, Blocks};
                {error, _Reason} ->
                    log("Error: ~p - did not mine", [_Reason]),
                    did_not_mine
            end
    end.

check_fsm_state(Fsm) ->
    {ok, #{ initiator  := Initiator
          , responder  := Responder
          , init_amt   := IAmt
          , resp_amt   := RAmt
          , state_hash := StateHash
          , round      := Round }} = aesc_fsm:get_state(Fsm),
    Trees =
        aec_test_utils:create_state_tree_with_accounts(
            [aec_accounts:new(Pubkey, Balance) ||
                {Pubkey, Balance} <- [{Initiator, IAmt}, {Responder, RAmt}]],
            no_backend),
    Hash = aec_trees:hash(Trees),
    StateHash = Hash, %% assert same root hash
    {IAmt, RAmt, StateHash, Round}.

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
    log("with_trace ...", []),
    TTBRes = aesc_ttb:on_nodes([node()|get_nodes()], File),
    log("Trace set up: ~p", [TTBRes]),
    try F(Config)
    ?_catch_(Error, Reason, Stack)
        case {Error, Reason} of
            {error, R} ->
                ct:pal("Error ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                error(R);
            {exit, R} ->
                ct:pal("Exit ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                exit(R);
            {throw, Res} ->
                ct:pal("Caught throw:~p", [Res]),
                throw(Res)
        end
    end,
    case When of
        on_error ->
            log("Discarding trace", []),
            aesc_ttb:stop_nofetch();
        always ->
            ttb_stop()
    end,
    ok.

ttb_stop() ->
    Dir = aesc_ttb:stop(),
    Out = filename:join(filename:dirname(Dir), filename:basename(Dir) ++ ".txt"),
    case aesc_ttb:format(Dir, Out, #{limit => 10000}) of
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
    aega_test_utils:get_contract(aect_test_utils:latest_sophia_version(), Name).

cache_account_type(R) ->
    Type = account_type(R),
    R#{account_type => Type}.

account_type(#{ account_type := Type }) ->
    Type;
account_type(#{ pub := Pubkey }) ->
    account_type_(Pubkey);
account_type(Pubkey) when is_binary(Pubkey) ->
    account_type_(Pubkey).

account_type_(Pubkey) ->
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

get_btc_auth_nonce(PubKey) ->
    {value, Account} = rpc(dev1, aec_chain, get_account, [PubKey]),
    ContractPubkey = aeser_id:specialize(aec_accounts:ga_contract(Account),
                                         contract),
    {ok, Contract} = rpc(dev1, aec_chain, get_contract, [ContractPubkey]),
    extract_nonce_from_btc_auth_store(aect_contracts:state(Contract)).

get_btc_offchain_nonce(ChannelPubKey, Owner) ->
    %% Disable debug logs here as in case of contract cache misses these debug
    %% logs clutter the test logs significantly
    case rpc(dev1, aec_chain, get_channel, [ChannelPubKey], false) of
        {ok, Channel} ->
            StoreKey = aesc_channels:auth_store_key(aeser_id:create(account, Owner),
                                                    Channel),
            {ok, Trees} = rpc(dev1, aec_chain, get_top_state, [], false),
            ContractTree = aec_trees:contracts(Trees),
            {ok, Store} = rpc(dev1, aect_state_tree, read_contract_store, [StoreKey,
                                                                          ContractTree], false),
            extract_nonce_from_btc_auth_store(Store);
        {error, not_found} -> 42 %% negative tests
    end.

request_unknown_bh(Cfg) ->
    NOT = 10,
    NNT = 0,
    #{ i := I
     , r := R
     , spec := Spec} = create_channel_([{block_hash_delta, #{ not_older_than => NOT
                                                            , not_newer_than => NNT
                                                            , pick           => 1}},
                                        ?SLOGAN|Cfg]),
    #{ initiator := PubI
     , responder := PubR } = Spec,
    TryUnknown =
        fun(#{fsm := Fsm}, Function, Args0) ->
            Args =
                case is_list(Args0) of
                    true -> [Fsm] ++ Args0 ++ [#{block_hash => ?BOGUS_BLOCKHASH}]; %% transfer
                    false -> [Fsm, Args0#{block_hash => ?BOGUS_BLOCKHASH}]
                end,
            {error, unknown_block_hash} = rpc(dev1, aesc_fsm, Function, Args)
        end,
    Funs = [ {upd_deposit, #{amount => 1}}
           , {upd_withdraw, #{amount => 1}}
           , {upd_transfer, [PubI, PubR, 1]}
           ],
    [TryUnknown(Who, Fun, Args) || Who <- [I, R],
                                   {Fun, Args} <- Funs],
    shutdown_(I, R, Cfg),
    ok.

request_too_new_bh(Cfg) ->
    NOT = 10,
    NNT = 1, % top not allowed
    Debug = get_debug(Cfg),
    #{ i := I
     , r := R
     , spec := Spec} = create_channel_([{block_hash_delta, #{ not_older_than => NOT
                                                            , not_newer_than => NNT
                                                            , pick           => 1}},
                                        ?SLOGAN|Cfg]),
    #{ initiator := PubI
     , responder := PubR } = Spec,
    TopHash = aecore_suite_utils:get_key_hash_by_delta(dev1, NNT - 1), %% actually uses Top Hash
    TryTooNew =
        fun({#{fsm := Fsm} = Participant, OtherP}, Function, Args0, TxType) ->
            Args =
                case is_list(Args0) of
                    true -> [Fsm] ++ Args0 ++ [#{block_hash => TopHash}]; %% transfer
                    false -> [Fsm, Args0#{block_hash => TopHash}]
                end,
            ok = rpc(dev1, aesc_fsm, Function, Args),
            {Participant1, _} = await_signing_request(TxType, Participant, Cfg),
            %% rejected by other party
            {ok, _} = receive_from_fsm(conflict, OtherP, any_msg(),
                                       ?TIMEOUT, Debug),
            {ok, _} = receive_from_fsm(conflict, Participant1, any_msg(),
                                       ?TIMEOUT, Debug),
            Participant1
        end,
    Funs = [ {upd_deposit, #{amount => 1}, deposit_tx}
           , {upd_withdraw, #{amount => 1}, withdraw_tx}
           , {upd_transfer, [PubI, PubR, 1], update}
           ],
    [TryTooNew(Who, Fun, Args, TxType) || Who <- [{I, R}, {R, I}],
                                          {Fun, Args, TxType} <- Funs],
    shutdown_(I, R, Cfg),
    ok.

request_too_old_bh(Cfg) ->
    NOT = 10, % top not allowed
    NNT = 1,
    Debug = get_debug(Cfg),
    #{ i := I
     , r := R
     , spec := Spec} = create_channel_([{block_hash_delta, #{ not_older_than => NOT
                                                            , not_newer_than => NNT
                                                            , pick           => 1}},
                                        ?SLOGAN|Cfg]),
    mine_key_blocks(dev1, NOT + 2), % do not rely on min depth
    #{ initiator := PubI
     , responder := PubR } = Spec,
    OldHash = aecore_suite_utils:get_key_hash_by_delta(dev1, NOT + 1),
    TryTooOld =
        fun({#{fsm := Fsm} = Participant, OtherP}, Function, Args0, TxType) ->
            Args =
                case is_list(Args0) of
                    true -> [Fsm] ++ Args0 ++ [#{block_hash => OldHash}]; %% transfer
                    false -> [Fsm, Args0#{block_hash => OldHash}]
                end,
            ok = rpc(dev1, aesc_fsm, Function, Args),
            {Participant1, _} = await_signing_request(TxType, Participant, Cfg),
            %% rejected by other party
            {ok, _} = receive_from_fsm(conflict, OtherP, any_msg(),
                                       ?TIMEOUT, Debug),
            {ok, _} = receive_from_fsm(conflict, Participant1, any_msg(),
                                       ?TIMEOUT, Debug),
            Participant1
        end,
    Funs = [ {upd_deposit, #{amount => 1}, deposit_tx}
           , {upd_withdraw, #{amount => 1}, withdraw_tx}
           , {upd_transfer, [PubI, PubR, 1], update}
           ],
    [TryTooOld(Who, Fun, Args, TxType) || Who <- [{I, R}, {R, I}],
                                          {Fun, Args, TxType} <- Funs],
    shutdown_(I, R, Cfg),
    ok.


positive_bh(Cfg) ->
    NOT = 10,
    NNT = 1,
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := R
     , spec := _Spec} = create_channel_([{block_hash_delta, #{ not_older_than => NOT
                                                             , not_newer_than => NNT
                                                             , pick           => 1}},
                                        ?SLOGAN|Cfg]),
    mine_key_blocks(dev1, NOT + 2), % do not rely on min depth
    TestByDelta =
        fun(Delta, {_I, _R} = Participants) ->
            lists:foldl(
                fun(Fun, {I0, R0}) ->
                    {_IAmt0, _RAmt0, _, Round} = check_fsm_state(FsmI),
                    BlockHash = aecore_suite_utils:get_key_hash_by_delta(dev1,
                                                                        Delta),
                    {ok, I1, R1} = Fun(I0, R0, #{block_hash => BlockHash}, Round),
                    {I1, R1}
                end,
                {_I, _R} = Participants,
                [ fun(Il, Rl, Opts, Round) ->
                      deposit_(Il, Rl, 1, Opts, Round, Debug, Cfg)
                  end,
                  fun(Il, Rl, Opts, Round) ->
                      withdraw_(Il, Rl, 1, Opts, 1, Round, Debug, Cfg)
                  end
                ])
        end,
    {IFinal, RFinal} =
        lists:foldl(
            TestByDelta,
            {I, R},
            [ NOT,      %% border condition
              NNT,      %% border condition
              NOT - 1,  %% in the range
              NNT + 1]),%% in the range
    shutdown_(IFinal, RFinal, Cfg),
    ok.

%% ==================================================================
%% Shared test cases

withdraw_full_cycle_(Amount, Opts, MinDepth, MinDepthChannel, Round, Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := FsmI
             , initiator_amount := IAmt0
             , responder_amount := RAmt0 } = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(Cfg, MinDepthChannel, Debug),
    ct:log("I = ~p", [I]),
    {ok, _, _} = withdraw_(I, R, Amount, Opts, MinDepth, Round, Debug, Cfg),
    shutdown_(I, R, Cfg),
    ok.

withdraw_(#{fsm := FsmI} = I, R, Amount, Opts, MinDepth, Round0, Debug, Cfg) ->
   #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, Round0} = check_fsm_state(FsmI),
    ct:log("Round0 = ~p, IAmt0 = ~p, RAmt0 = ~p", [Round0, IAmt0, RAmt0]),
    check_info(0),
    ok = rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, Opts#{amount => Amount}]),
    {I1, _} = await_signing_request(withdraw_tx, I, Cfg),
    {R1, _} = await_signing_request(withdraw_created, R, Cfg),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {IAmt0, RAmt0, _, Round0} = check_fsm_state(FsmI),
    mine_blocks(dev1, MinDepth),
    {IAmt1, RAmt0, StateHash, Round1} = check_fsm_state(FsmI),
    Round1 = Round0 + 1, %% assert round
    {IAmt1, _} = {IAmt0 - Amount, IAmt1}, %% assert correct amounts
    {channel_withdraw_tx, WithdrawalTx} =
        aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    Round1 = aesc_withdraw_tx:round(WithdrawalTx), %% assert correct round
    StateHash = aesc_withdraw_tx:state_hash(WithdrawalTx), %% assert correct state hash
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I1,
    Expected = {IAmt2, RAmt2},
    {Expected, Expected} = {{IAmt0 - Amount, RAmt0}, Expected},
    SignedTx2 = await_on_chain_report(I1, #{info => channel_changed}, ?TIMEOUT), % same tx
    SignedTx2 = await_on_chain_report(R1, #{info => channel_changed}, ?TIMEOUT), % same tx
    check_info(20),
    {ok, I1, R1}.

%% ==================================================================
%% Internal functions

create_channel_(Cfg) ->
    create_channel_(Cfg, get_debug(Cfg)).

create_channel_(Cfg, Debug) ->
    create_channel_(Cfg, ?MINIMUM_DEPTH, #{}, get_debug(Cfg)).

create_channel_(Cfg, XOpts, Debug) when is_map(XOpts) ->
    create_channel_(Cfg, ?MINIMUM_DEPTH, XOpts, Debug);

create_channel_(Cfg, Debug) ->
    create_channel_(Cfg, ?MINIMUM_DEPTH, Debug).

create_channel_(Cfg, MinDepth, Debug) ->
    create_channel_(Cfg, MinDepth, #{}, Debug).

create_channel_(Cfg, MinDepth, XOpts, Debug) ->
    {I, R, Spec} = channel_spec(Cfg, XOpts),
    log(Debug, "channel_spec: ~p", [{I, R, Spec}]),
    Port = proplists:get_value(port, Cfg, 9325),
    UseAny = proplists:get_bool(use_any, Cfg),
    create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg, MinDepth).

channel_spec(Cfg) ->
    channel_spec(Cfg, #{}).

channel_spec(Cfg, XOpts) ->
    PushAmount = proplists:get_value(push_amount, Cfg, 200000),
    ChannelReserve = proplists:get_value(channel_reserve, Cfg, 300000),
    channel_spec(Cfg, ChannelReserve, PushAmount, XOpts).

channel_spec(Cfg, ChannelReserve, PushAmount) ->
    channel_spec(Cfg, ChannelReserve, PushAmount, #{}).

channel_spec(Cfg, ChannelReserve, PushAmount, XOpts) ->
    I = ?config(initiator, Cfg),
    R = ?config(responder, Cfg),

    %% dynamic key negotiation
    Proto = <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>,

    IAmt = ?config(initiator_amount, Cfg),
    RAmt = ?config(responder_amount, Cfg),
    Spec0 = #{ initiator            => maps:get(pub, I)
             , responder            => maps:get(pub, R)
             , initiator_amount     => IAmt
             , responder_amount     => RAmt
             , push_amount          => PushAmount
             , lock_period          => 10
             , channel_reserve      => ChannelReserve
             , minimum_depth        => config(minimum_depth, Cfg, ?MINIMUM_DEPTH)
             , minimum_depth_factor => config(minimum_depth_factor, Cfg, ?MINIMUM_DEPTH_FACTOR)
             , client               => self()
             , noise                => [{noise, Proto}]
             , timeouts             => #{idle => 20000}
             , slogan               => slogan(Cfg)
             , report               => #{debug => true}
             },
    Spec1 = maps:merge(Spec0, XOpts),
    Spec2 =
        lists:foldl(
          fun(K, AccumSpec) ->
                  case ?config(K, Cfg) of
                      undefined -> AccumSpec;
                      Val       -> maps:put(K, Val, AccumSpec)
                  end
          end,
          Spec,
          [nonce, block_hash_delta]),

    I1 = prime_password(I, initiator_password, Cfg),
    R1 = prime_password(R, responder_password, Cfg),

    {I1, R1, Spec2}.

log(Fmt, Args) ->
    Debug = case config() of
                undefined ->
                    false;
                Cfg ->
                    config(debug, Cfg, false)
            end,
    log(Debug, Fmt, Args).

log(true, Fmt, Args) ->
    ct:log("~p: " ++ Fmt, [self()|Args]);
log(#{debug := true}, Fmt, Args) ->
    ct:log("~p: " ++ Fmt, [self()|Args]);
log(_, _, _) ->
    ok.

config() -> get(config).

config(Cfg) -> put(config, Cfg).

config(K, Cfg, Def) ->
    case ?config(K, Cfg) of
        undefined -> Def;
        Other     -> Other
    end.

get_debug(Config) ->
    proplists:get_bool(debug, Config).

set_debug(Bool, Config) when is_boolean(Bool) ->
    lists:keystore(debug, 1, Config -- [debug], {debug, Bool}).

%% @doc This function inspects the bitcoin_auth contract's store and extracts the
%% nonce out of it. It heavily relies on the state of the contract being
%% { nonce : int, owner : bytes(64) }
extract_nonce_from_btc_auth_store(Store) ->
    #{<<0>> := Encoded0} = rpc(dev1, aect_contracts_store, contents, [Store]),
    {ok, {Nonce, _}} = aeb_heap:from_binary({tuple, [word, {tuple, [word, word]}]},
                                Encoded0),
    Nonce.

peek_message_queue(L, Debug) ->
    {messages, Msgs} = process_info(self(), messages),
    case Msgs of
        [] -> ok;
        _ ->
            log(Debug,
                "==================================================~n"
                "~p: message Q: ~p~n"
               "==================================================~n", [L, Msgs])
    end,
    ok.

slogan(Cfg) ->
    ?config(slogan, Cfg).

prime_password(#{} = P, Key, Cfg) when Key =:= initiator_password; Key =:= responder_password ->
    case ?config(Key, Cfg) of
        undefined ->
            P#{state_password => generate_password()};
        ignore ->
            ct:log("Ignoring password"),
            maps:remove(state_password, P);
        Password ->
            ct:log("Using predefined password"),
            P#{state_password => Password}
     end.

-spec generate_password() -> string().
generate_password() ->
    binary:bin_to_list(crypto:strong_rand_bytes(6)).
