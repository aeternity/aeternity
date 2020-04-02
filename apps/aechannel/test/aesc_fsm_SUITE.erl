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
        , deposit_with_failed_onchain/1
        , withdraw_with_conflict/1
        , withdraw_with_soft_reject/1
        , withdraw_with_failed_onchain/1
        , upd_dep_with_conflict/1
        , upd_wdraw_with_conflict/1
        , dep_wdraw_with_conflict/1
        , update_with_signing_abort/1
        , deposit_with_signing_abort/1
        , withdraw_with_signing_abort/1
        , shutdown_with_signing_abort/1
        , deposit/1
        , slash/1
        , withdraw/1
        , withdraw_high_amount_static_confirmation_time/1
        , withdraw_high_amount_short_confirmation_time/1
        , withdraw_low_amount_long_confirmation_time/1
        , withdraw_low_amount_long_confirmation_time_negative_test/1
        , channel_detects_close_solo_and_settles/1
        , close_mutual_with_failed_onchain/1
        , close_solo_with_failed_onchain/1
        , snapshot_with_failed_onchain/1
        , slash_with_failed_onchain/1
        , settle_with_failed_onchain/1
        , leave_reestablish_responder_stays/1
        , leave_reestablish_close/1
        , force_progress_based_on_offchain_state/1
        , force_progress_based_on_snapshot/1
        , force_progress_based_on_deposit/1
        , force_progress_based_on_withdrawal/1
        , force_progress_closing_state/1
        , force_progress_with_failed_onchain/1
        , force_progress_on_force_progress/1
        , force_progress_followed_by_update/1
        , force_progress_triggers_snapshot/1
        , force_progress_triggers_slash/1
        , change_config_get_history/1
        , multiple_channels/1
        , many_chs_msg_loop/1
        , too_many_fsms/1
        , check_incorrect_create/1
        , check_incorrect_deposit/1
        , check_incorrect_withdrawal/1
        , check_incorrect_update/1
        , check_incorrect_mutual_close/1
        , check_mutual_close_with_wrong_amounts/1
        , check_mutual_close_after_close_solo/1
        , check_fsm_crash_reestablish/1
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

-define(BOGUS_PUBKEY, <<12345:32/unit:8>>).
-define(BOGUS_PRIVKEY, <<12345:64/unit:8>>).
-define(BOGUS_BLOCKHASH, <<42:32/unit:8>>).

-define(PEEK_MSGQ(_D), peek_message_queue(?LINE, _D)).
-define(LOG(_Fmt, _Args), log(_Fmt, _Args, ?LINE, true)).
-define(LOG(_D, _Fmt, _Args), log(_Fmt, _Args, ?LINE, _D)).

%% Default configuration values
-define(MINIMUM_DEPTH, 5).
-define(MINIMUM_DEPTH_FACTOR, 10).
-define(MINIMUM_DEPTH_STRATEGY, txfee).
-define(INITIATOR_AMOUNT, (10000000 * aec_test_utils:min_gas_price())).
-define(RESPONDER_AMOUNT, (10000000 * aec_test_utils:min_gas_price())).
-define(ACCOUNT_BALANCE, max(?INITIATOR_AMOUNT, ?RESPONDER_AMOUNT) * 1000).
-define(PUSH_AMOUNT, 200000).
-define(CHANNEL_RESERVE, 20000).

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

%% tracing checkpoints - to cut down on the amount of trace data collected
%% When a given checkpoint is reached, there should be a call to
%%
%% trace_checkpoint(Checkpoint, Config).
%%
%% When tracing should be activated is controlled by adding the config option
%% {activate_trace, Checkpoint}, for example:
%%
%% with_trace(fun my_test/1, [{activate_trace, ?TR_CHANNEL_CREATED}|Cfg], "my_test")
%%
%% If tracing is to be started from the beginning, no `activate_trace` option is needed,
%% and {activate_trace, ?TR_START} is implied.
-define(TR_START, start).
-define(TR_CHANNEL_CREATED, channel_created).

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
                             , {group, failed_onchain}
                             ]},
     {transactions_only, [sequence], transactions_sequence()},  %% if you don't also want to run GA tests
     {transactions, [sequence], transactions_sequence()},
     {throughput, [sequence],
      [
        multiple_channels
      , many_chs_msg_loop
      ]},
     {limits, [sequence],
      [
       too_many_fsms
      ]},
     {errors, [sequence],
      [
        check_mutual_close_with_wrong_amounts
      , check_mutual_close_after_close_solo
      , check_fsm_crash_reestablish
      ]},
     {signing_abort, [sequence],
      [
        update_with_signing_abort
      , deposit_with_signing_abort
      , withdraw_with_signing_abort
      , shutdown_with_signing_abort
      ]},
     {failed_onchain, [sequence],
      [ deposit_with_failed_onchain
      , withdraw_with_failed_onchain
      , close_solo_with_failed_onchain
      , close_mutual_with_failed_onchain
      , snapshot_with_failed_onchain
      , slash_with_failed_onchain
      , settle_with_failed_onchain
      , force_progress_with_failed_onchain
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
     {force_progress, [sequence],
      [ force_progress_based_on_offchain_state
      , force_progress_based_on_snapshot
      , force_progress_based_on_deposit
      , force_progress_based_on_withdrawal
      , force_progress_on_force_progress
      , force_progress_followed_by_update
      , force_progress_triggers_snapshot
      , force_progress_triggers_slash
      ]}
    ].

transactions_sequence() ->
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
      , withdraw_high_amount_static_confirmation_time
      , withdraw_high_amount_short_confirmation_time
      , withdraw_low_amount_long_confirmation_time
      , withdraw_low_amount_long_confirmation_time_negative_test
      , channel_detects_close_solo_and_settles
      , leave_reestablish_responder_stays
      , leave_reestablish_close
      , change_config_get_history
      , slash
      , {group, force_progress}
      ].

ga_sequence() ->
    [ {group, transactions}
    , {group, errors}
    , {group, signatures}
    , {group, channel_ids}
    , {group, pinned_env}
    ].

update_sequence() ->
    [ check_incorrect_update
    ].

update_sequence_() ->
    [ check_incorrect_deposit
    , check_incorrect_withdrawal
    , check_incorrect_update
    ].

suite() ->
    [].

init_per_suite(Config) ->
    {ok, StartedApps} = application:ensure_all_started(gproc),
    {_PrivKey, PubKey} = aecore_suite_utils:sign_keys(dev1),
    TableOwner = new_config_table(),
    ct:log("network_id ~p", [aec_governance:get_network_id()]),
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
init_per_group(throughput, Config0) ->
    Config = init_per_group_(Config0),
    set_configs([ {minimum_depth, 2}
                , {minimum_depth_factor, 0}
                , {minimum_depth_channel, 2}
                ], Config);
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
init_per_group(_Group, Config) ->
    init_per_group_(Config).

init_per_group_(Config) ->
    case proplists:get_value(ga_group, Config, false) of
        true ->
            Config;
        false ->
            aecore_suite_utils:start_node(dev1, Config),
            aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1), [block_pow]),
            ?LOG("dev1 connected", []),
            try begin
                    Node = dev1,
                    prepare_patron(Node),
                    Initiator = prep_initiator(?ACCOUNT_BALANCE, Node),
                    Responder = prep_responder(?ACCOUNT_BALANCE, Node),
                    Responder2 = prep_responder(?ACCOUNT_BALANCE, Node),
                    set_configs([ {initiator, Initiator}
                                , {responder, Responder}
                                , {responder2, Responder2}
                                , {port, ?PORT}
                                ], Config)
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
    Debug = (os:getenv("CT_DEBUG") == "1"),
    Config1 = load_idx(Config),
    Config2 = case is_above_roma_protocol() of
                  true ->
                      set_configs([ {minimum_depth, ?MINIMUM_DEPTH}
                                  , {minimum_depth_factor, ?MINIMUM_DEPTH_FACTOR}
                                  , {minimum_depth_strategy, ?MINIMUM_DEPTH_STRATEGY}
                                  ], Config1, false);
                  false ->
                      % Because the tx fees used to be lower in roma, more
                      % blocks are required for tx to be confirmed.
                      set_configs([ {minimum_depth, ?MINIMUM_DEPTH}
                                  , {minimum_depth_factor, ?MINIMUM_DEPTH_FACTOR * 2}
                                  , {minimum_depth_strategy, ?MINIMUM_DEPTH_STRATEGY}
                                  ], Config1, false)
              end,
    set_configs([{debug, Debug}], Config2).

end_per_testcase(T, _Config) when T == multiple_channels;
                                  T == many_chs_msg_loop ->
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
    ?LOG(Debug, "Responder2 = ~p", [Responder2]),
    Cfg2 = lists:keyreplace(responder, 1, Cfg, {responder, Responder2}),
    Me = self(),
    Initiator = maps:get(pub, ?config(initiator, Cfg)),
    InitiatorAccountType = account_type(Initiator),
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Initiator]),
    MinerHelper = spawn_miner_helper(),
    CreateMultiChannel =
        fun(N, CustomCfg) ->
            ChannelCfg0 = set_configs([ {port, ?PORT}
                                      , {ack_to, Me}
                                      , {slogan, {Slogan, N}}
                                      , {minimum_depth_factor, 0}
                                      ], CustomCfg),
            ChannelCfg =
                case InitiatorAccountType of
                    basic -> % give away nonces in advance to avoid race conditions
                        [{nonce, Nonce + N - 1} | ChannelCfg0];
                    generalized -> % nonce is 0
                        ChannelCfg0
                end,
            C = create_multi_channel(ChannelCfg, #{ mine_blocks => {ask, MinerHelper}
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
    ?LOG(Debug, "channel pids collected: ~p", [Cs]),
    %% At this point, we know the pairing worked
    [begin
         MRef = erlang:monitor(process, P),
         ?LOG(Debug, "P (~p) info: ~p", [P, process_info(P)]),
         P ! die,
         receive
             {'DOWN', MRef, _, _, _} ->
                 ok
         after 5000 ->
                 ?LOG("timed out", []),
                 ?PEEK_MSGQ(Debug),
                 error({channel_not_dying, P})
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
            error(timeout)
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
    Debug = get_debug(Cfg),
    Cfg1 = [?SLOGAN | Cfg],
    assert_empty_msgq(Debug),

    #{ i := #{channel_id := ChannelId} = I
     , r := #{} = R} = create_channel_(Cfg1, #{}, Debug),
    assert_empty_msgq(Debug),

    {ok, _} = rpc(dev1, aec_chain, get_channel, [ChannelId]),

    shutdown_(I, R, Cfg),

    {ok, #{info := {log, ILog}}} = receive_log(I, Debug),
    {ok, #{info := {log, RLog}}} = receive_log(R, Debug),
    ok = check_log(expected_fsm_logs(?FUNCTION_NAME, initiator), ILog,
                   initiator),
    ok = check_log(expected_fsm_logs(?FUNCTION_NAME, responder), RLog,
                   responder),

    % Done
    assert_empty_msgq(Debug),
    ok.

channel_insufficent_tokens(Cfg) ->
    Debug = get_debug(Cfg),
    Test =
        fun(IAmt, RAmt, ChannelReserve, PushAmount, Error) ->
                Cfg1 = set_configs([ ?SLOGAN
                                   , {channel_reserve, ChannelReserve}
                                   , {push_amount, PushAmount}
                                   , {initiator_amount, IAmt}
                                   , {responder_amount, RAmt} ], Cfg),
                {_, _, Params} = channel_spec(Cfg1),
                channel_create_invalid_spec(Params, Error, Debug)
        end,
    Test(10, 10, 5, 6, insufficient_initiator_amount),
    Test(10, 1, 5, 3, insufficient_responder_amount),
    Test(1, 1, 5, 3, insufficient_amounts),
    ok.

channel_create_invalid_spec(Spec, Error, Debug) ->
    Port = 9325,
    {error, Error} =
        rpc(dev1, aesc_client, respond, [Port, Spec], Debug),
    {error, Error} =
        rpc(dev1, aesc_client, initiate, ["localhost", Port, Spec], Debug).

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
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),
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
                     ok = rpc(dev1, aesc_fsm, upd_transfer,
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
                     ok = rpc(dev1, aesc_fsm, upd_transfer,
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
                     ok = rpc(dev1, aesc_fsm, upd_transfer,
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
                                            [Fsm, #{}]
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
    ?PEEK_MSGQ(Debug),
    {BalI, BalR} = get_both_balances(FsmI, PubI, PubR),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmI]),
    ?PEEK_MSGQ(Debug),
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
    ?LOG("=== Starting benchmark ===", []),
    {Time, I1, R1} = do_n(Rounds, fun update_volley/3,
                          cache_account_type(I),
                          cache_account_type(R), C),
    Fmt = "Time (1*2*" ++ integer_to_list(Rounds) ++ "): ~.1f s; ~.1f mspt; ~.1f tps",
    Args = [Time/1000, Time/(2*Rounds), 2000*Rounds/Time],
    ?LOG(Fmt, Args),
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
            ?LOG(Debug, "update successful", []),
            {I2, R2}
    end.

msg_volley(#{fsm := FsmI, pub := PubI} = I, #{fsm := FsmR, pub := PubR} = R, _) ->
    rpc(dev1, aesc_fsm, inband_msg, [FsmI, PubR, <<"ping">>], false),
    %% A 1 second timeout might not be enough if there is a lot of CPU intensive
    %% background processes in the OS, bump to 4 seconds to avoid random failures
    {ok,_} = receive_from_fsm(
               message, R,
               fun(#{info := #{info := <<"ping">>}}) -> ok end, 4000),
    rpc(dev1, aesc_fsm, inband_msg, [FsmR, PubI, <<"pong">>]),
    {ok,_} = receive_from_fsm(
               message, I,
               fun(#{info := #{info := <<"pong">>}}) -> ok end, 4000),
    {I, R}.

deposit(Cfg) ->
    Debug = get_debug(Cfg),
    Amount = 10,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, Round0 = 1, _} = check_fsm_state(FsmI),
    check_info(0),
    {ok, I1, R1} = deposit_(I, R, Amount, Round0, Debug, Cfg),
    check_info(20),
    shutdown_(I1, R1, Cfg),
    ok.

withdraw(Cfg) ->
    % Use default values for minimum depth calculation
    Cfg1 = set_configs([ ?SLOGAN
                       , {minimum_depth, 3}
                       , {minimum_depth_channel, 5}
                       ], Cfg),
    Amount = 2,
    Round = 1,
    ok = withdraw_full_cycle_(Amount, #{}, Round, Cfg1).

withdraw_high_amount_static_confirmation_time(Cfg) ->
    % Factor of 0 sets min depths to 1 for all amounts
    Cfg1 = set_configs([ ?SLOGAN
                       , {minimum_depth, 1}
                       , {minimum_depth_channel, 1}
                       , {minimum_depth_factor, 0}
                       ], Cfg),
    Amount = 300000,
    Round = 1,
    ok = withdraw_full_cycle_(Amount, #{}, Round, Cfg1).

withdraw_high_amount_short_confirmation_time(Cfg) ->
    % High amount and high factor should lead to single block required
    Cfg1 = set_configs([ ?SLOGAN
                       , {minimum_depth, 2}
                       , {minimum_depth_channel, 1}
                       , {minimum_depth_factor, 60}
                       ], Cfg),
    Amount = 300000,
    Round = 1,
    ok = withdraw_full_cycle_(Amount, #{}, Round, Cfg1).

withdraw_low_amount_long_confirmation_time(Cfg) ->
    % Low amount and low factor should lead to comparitively long confirmation time
    Cfg1 = set_configs([?SLOGAN, {minimum_depth_factor, 8}], Cfg),
    Amount = 1,
    Round = 1,
    Cfg2 =
        case config(ga_group, Cfg, false) orelse not is_above_roma_protocol() of
            true ->
                set_configs([ {minimum_depth, 24}
                            , {minimum_depth_channel, 20}
                            ], Cfg1);
            false ->
                set_configs([ {minimum_depth, 12}
                            , {minimum_depth_channel, 10}
                            ], Cfg1)
        end,
    ok = withdraw_full_cycle_(Amount, #{}, Round, Cfg2).

withdraw_low_amount_long_confirmation_time_negative_test(Cfg) ->
    % Low amount and low factor should lead to comparitively long confirmation time
    Cfg1 = set_configs([ ?SLOGAN
                       , {minimum_depth, 3}
                       , {minimum_depth_channel, 10}
                       , {minimum_depth_factor, 4}
                       ], Cfg),
    Amount = 1,
    Round = 1,
    try
        ok = withdraw_full_cycle_(Amount, #{}, Round, Cfg1),
        ct:fail("Expected withdraw test to fail due to min depth being to small.")
    catch
        error:timeout ->
            % This timeout occurs when checking the fsm messages is expected because the
            % chosen min depth is too low.
            ok
    end.

channel_detects_close_solo_and_settles(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := I, r := R, spec := Spec } = create_channel_([?SLOGAN|Cfg]),
    {ok, Tx} = close_solo_tx(I, <<>>),
    {SignedCloseSoloTx, I1} = sign_tx(I, Tx, Cfg),
    ok = rpc(dev1, aec_tx_pool, push, [SignedCloseSoloTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedCloseSoloTx)),
    mine_blocks_until_txs_on_chain(dev1, [TxHash]),
    LockPeriod = maps:get(lock_period, Spec),
    UnlockHeight = current_height(dev1) + LockPeriod,
    ?LOG(Debug, "Expected height at which settle is allowed = ~p",
         [UnlockHeight]),
    SignedTx = await_on_chain_report(I1, #{info => solo_closing}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, #{info => solo_closing}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I1, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun closing/1, ?TIMEOUT, Debug),
    check_info(20),
    LockPeriod = maps:get(lock_period, Spec),
    mine_blocks(dev1, LockPeriod),
    settle_(maps:get(minimum_depth, Spec), I1, R, Debug, Cfg),
    check_info(20),
    ok.

close_solo_tx(#{ fsm        := Fsm
               , channel_id := ChannelId }, Payload) ->
    {ok, #{ round      := Round
          , initiator  := IPubKey
          , responder  := RPubKey
          , round      := Round }} = St = rpc(dev1, aesc_fsm, get_state, [Fsm]),
    ?LOG("St = ~p", [St]),
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
              , fee        => 40000 * aec_test_utils:min_gas_price()
              , nonce      => Nonce },
    {ok, _Tx} = aesc_close_solo_tx:new(TxSpec).

% Test leave-reestablish flow with unclean leave, where the FSM is closed upon timeout
leave_reestablish_close(Cfg) ->
    Debug = get_debug(Cfg),
    Cfg1 = [?SLOGAN | Cfg],
    assert_empty_msgq(Debug),

    % Do common leave-reestablish routine
    #{i := I, r := R, spec := Spec} = leave_reestablish_loop_(Cfg1),

    % Verify that we can still perform updates
    #{initiator := PubI, responder := PubR} = Spec,
    {I1, R1} = do_update(PubI, PubR, 1, I, R, Debug, Cfg1),

    % Ensure state is still in ram pre shutdown
    ChId = maps:get(channel_id, I1),
    assert_cache_is_in_ram(ChId),

    % Do the shutdown
    shutdown_(I1, R1, Cfg1),

    {ok, #{info := {log, ILog}}} = receive_log(I1, Debug),
    {ok, #{info := {log, RLog}}} = receive_log(R1, Debug),
    ok = check_log(expected_fsm_logs(?FUNCTION_NAME, initiator), ILog,
                   initiator),
    ok = check_log(expected_fsm_logs(?FUNCTION_NAME, responder), RLog,
                   responder),

    % Ensure state is flushed gone post shutdown
    assert_cache_is_gone_after_on_disk(ChId),

    % Done
    assert_empty_msgq(Debug),
    ok.

leave_reestablish_responder_stays(Cfg) ->
    Cfg1 = set_configs([?SLOGAN , {keep_running, true}], Cfg),
    leave_reestablish_close(Cfg1).

change_config_get_history(Cfg) ->
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R } = create_channel_([?SLOGAN|Cfg]),
    ILog = rpc(dev1, aesc_fsm, get_history, [FsmI]),
    ok = check_log(expected_fsm_logs(?FUNCTION_NAME, initiator), ILog,
                   initiator),
    ok = rpc(dev1, aesc_fsm, change_config, [FsmI, log_keep, 17]),
    Status = rpc(dev1, sys, get_status, [FsmI]),
    check_w_param(17,Status),
    {error, invalid_config} =
        rpc(dev1, aesc_fsm, change_config, [FsmI, log_keep, -1]),
    {error, invalid_config} =
        rpc(dev1, aesc_fsm, change_config, [FsmI, invalid, config]),
    shutdown_(I, R, Cfg),
    check_info(20).

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

get_balances(Fsm, Pubkeys) ->
    {ok, Result} = rpc(dev1, aesc_fsm, get_balances, [Fsm, Pubkeys]),
    lists:map(
        fun({{Pubkey, Balance}, Pubkey}) -> %% same pubkey
            Balance
        end,
        lists:zip(Result, Pubkeys)).
    

check_log([{optional, Op, Type}|T], [{Op, Type, _, _}|T1], Participant) ->
    check_log(T, T1, Participant);
check_log([{optional, _Op, _Type}|T], T1, Participant) ->
    check_log(T, T1, Participant);
check_log([{Op, Type}|T], [{Op, Type, _, _}|T1], Participant) ->
    check_log(T, T1, Participant);
check_log([H|_], [H1|_], Participant) ->
    ?LOG("ERROR: ~p expected ~p in log; got ~p",
         [Participant, H, H1]),
    error(log_inconsistent);
check_log([_|_], [], _) ->
    %% the log is a sliding window; events may be flushed at the tail
    ok;
check_log([], _, _) ->
    ok.

fsm_up(#{info := {fsm_up, FsmId}}) when is_function(FsmId) -> ok.

died_normal(#{info := {died, normal}}) -> ok.

closing(#{info := closing} = Msg) ->
    ?LOG("matches #{info := closing} - ~p", [Msg]),
    ok.

aborted_update(#{info := aborted_update} = Msg) ->
    ?LOG("matches #{info := aborted_update} - ~p", [Msg]),
    ok.

multiple_channels(Cfg) ->
    multiple_channels_t(10, 9360, {transfer, 30}, ?SLOGAN, Cfg).

many_chs_msg_loop(Cfg) ->
    multiple_channels_t(10, 9400, {msgs, 100}, ?SLOGAN, Cfg).

multiple_channels_t(NumCs, FromPort, Msg, {slogan, Slogan}, Cfg) ->
    Debug = get_debug(Cfg),
    F = fun(Cs, _MinerHelper) ->
                [P ! Msg || P <- Cs],
                T0 = erlang:system_time(millisecond),
                Cs = collect_acks(Cs, loop_ack, NumCs),
                T1 = erlang:system_time(millisecond),
                Time = T1 - T0,
                N = loop_n(Msg),
                Transfers = NumCs*2*N,
                Fmt = "Time (~w*2*~w) ~.1f s: ~.1f mspt; ~.1f tps",
                Args = [NumCs, N, Time/1000, Time/Transfers, (Transfers*1000)/Time],
                ?LOG(Debug, Fmt, Args),
                ct:comment(Fmt, Args)
        end,
    spawn_multiple_channels(F, NumCs, FromPort, Slogan, Cfg).

loop_n({transfer, N}) ->
    N;
loop_n({msgs, N}) ->
    N.

spawn_multiple_channels(F, NumCs, FromPort, Slogan, Cfg) when is_function(F, 2) ->
    Debug = get_debug(Cfg),
    ?LOG(Debug, "spawning ~p channels", [NumCs]),
    Me = self(),
    Node = dev1,
    NodeName = aecore_suite_utils:node_name(Node),
    aecore_suite_utils:mock_mempool_nonce_offset(NodeName, NumCs),
    MinerHelper = spawn_miner_helper(),
    prepare_patron(Node, 1),
    Participants =
        lists:map(
            fun(N) ->
                Initiator = prep_initiator(?ACCOUNT_BALANCE, Node),
                Responder = prep_responder(?ACCOUNT_BALANCE, Node),
                {N, Initiator, Responder}
            end,
            lists:seq(1, NumCs)),
    Cs = lists:map(
          fun({N, Initiator, Responder}) ->
                  CustomCfg = set_configs([ {slogan, {Slogan, N}}
                                          , {port, FromPort}
                                          , {ack_to, Me}
                                          , {initiator, Initiator}
                                          , {responder, Responder}
                                          ], Cfg),
                  CustomOpts = #{ mine_blocks => {ask, MinerHelper}
                                , debug => Debug},
                  create_multi_channel(CustomCfg, CustomOpts)
          end, Participants),
    ?LOG(Debug, "channels spawned", []),
    Cs = collect_acks(Cs, channel_ack, NumCs),
    ?LOG(Debug, "channel pids collected: ~p", [Cs]),
    F(Cs, MinerHelper),

    % Before stopping the process we need to set up monitors to not run into a
    % race condition
    Monitored = [{P, erlang:monitor(process, P)} || P <- Cs],
    [P ! die || P <- Cs],
    ok = receive_downs(Monitored, normal, Debug),

    % All processes have been taken down, final cleanup
    ok = stop_miner_helper(MinerHelper),
    ok.

receive_downs([{P, MRef}|T], Reason, Debug) ->
    receive
        {'DOWN', MRef, process, P, ActualReason} ->
            {ActualReason, Reason} = {Reason, ActualReason},   % assertion
            ?LOG(Debug, "~p died in the expected manner (~p)", [P, Reason]),
            receive_downs(T, Reason, Debug)
    after
        ?TIMEOUT ->
            error(timeout)
    end;
receive_downs([], Reason, Debug) ->
    ?LOG(Debug, "All fsms died with reason ~p", [Reason]),
    ok.

too_many_fsms(Cfg) ->
    Debug = get_debug(Cfg),
    MaxChannels = 10,
    FSMLimit = MaxChannels * 2,  % both endpoints on same one
    set_fsm_limit(FSMLimit),
    F = fun(_Cs, _MinerHelper) ->
                ?LOG(Debug, "Current limit info: ~p", [rpc(dev1, aesc_limits, info, [])]),
                ?LOG(Debug, "******** Now trying to exceed the limit", []),
                ok = fail_to_create_fsm(channel_count_limit_exceeded, Cfg),
                ?LOG(Debug, "Got expected error trying to create fsm", [])
        end,
    ok = spawn_multiple_channels(F, MaxChannels, 9450, ?SLOGAN, Cfg),
    %% With the other fsms gone, creating a channel should work again
    timer:sleep(100),
    #{i := _, r := _} = create_channel_(Cfg),
    ok.

set_fsm_limit(N) ->
    rpc(dev1, aeu_env, update_config, [#{<<"channels">> => #{<<"max_count">> => N}}]).

check_incorrect_create(Cfg0) ->
    Cfg = set_configs([{keep_running, true}], Cfg0),
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
    Debug = get_debug(Cfg),

    Fun = proplists:get_value(wrong_action, Cfg),
    Test =
        fun({Updator, Malicious}, Cfg1) ->
            ?LOG(Debug, "check incorrect update from ~p and malicious is ~p",
                 [Updator, Malicious]),
            Port = proplists:get_value(port, Cfg1, ?PORT),
            Cfg2 = set_configs([?SLOGAN], load_idx(Cfg1)),

            #{ i := #{pub := IPub, fsm := FsmI} = I
             , r := #{pub := RPub, fsm := FsmR} = R
             , spec := Spec} = CSpec = create_channel_(Cfg2),
            Data = {I, R, Spec, Port, Debug},
            MakeUpdate =
                fun() ->
                    Fun(Data, Updator, Malicious,
                        {upd_transfer, [IPub, RPub, 1], update, update_ack})
                end,
            MakeUpdate(),
            {AliveFsm, OtherFsm} =
                case Updator of
                    initiator -> {FsmI, FsmR};
                    responder -> {FsmR, FsmI}
                end,
            ok = gen_statem:stop(AliveFsm),
            receive_dying_declarations(AliveFsm, OtherFsm, CSpec, Debug),
            bump_idx(),

            % Verify dying log
            {ok, #{info := {log, ILog}}} = receive_log(I, Debug),
            {ok, #{info := {log, RLog}}} = receive_log(R, Debug),
            LogInfo = #{depositor => Updator, malicious => Malicious},
            ok = check_log(expected_fsm_logs(?FUNCTION_NAME, initiator,
                                             LogInfo), ILog, initiator),
            ok = check_log(expected_fsm_logs(?FUNCTION_NAME, responder,
                                             LogInfo), RLog, responder),

            set_configs([{port, Port + 1}], Cfg2)
        end,
    Roles = [initiator, responder],
    Combinations = [{Updator, Malicious} || Updator <- Roles,
                                            Malicious <- Roles],
    lists:foldl(Test, Cfg, Combinations),
    ok.

receive_dying_declarations(Fsm, OtherFsm, CSpec, Debug) ->
    #{r := #{fsm := FsmR}} = CSpec,
    TRefI = erlang:start_timer(?TIMEOUT, self(), {dying_declaration_i, ?LINE}),
    TRefR = erlang:start_timer(?TIMEOUT, self(), {dying_declaration_r, ?LINE}),
    receive_dying_(Fsm, TRefI, Debug),
    case {OtherFsm =:= FsmR, responder_stays(CSpec)} of
        {true, true} ->
            ?LOG(Debug, "OtherFsm (~p) is responder, and is expected to remain", [OtherFsm]),
            receive
                {aesc_fsm, OtherFsm, #{ tag  := info
                                      , type := report
                                      , info := peer_disconnected }} ->
                    erlang:cancel_timer(TRefR),
                    ok;
                {timeout, TRefR, Msg} ->
                    error({timeout, Msg})
            end;
        _ ->
            ?LOG(Debug, "OtherFsm (~p) is expected to die", [OtherFsm]),
            receive_dying_(OtherFsm, TRefR, Debug)
    end.

receive_dying_(Fsm, TRef, Debug) ->
    receive_dying_(Fsm, TRef, false, Debug).

receive_dying_(_, TRef, true, _) ->
    erlang:cancel_timer(TRef),
    ok;
receive_dying_(Fsm, TRef, Died, Debug) ->
    ?LOG(Debug, "receive_dying_(~p, ~p, ~p, _)", [Fsm, TRef, Died]),
    receive
        {aesc_fsm, Fsm, #{info := {died, _}} = Msg} ->
            ?LOG(Debug, "from ~p: ~p", [Fsm, Msg]),
            receive_dying_(Fsm, TRef, true, Debug);
        {timeout, TRef, TOMsg} ->
            ?LOG(Debug, "timeout (~p). Q = ~p", [TOMsg, element(2,process_info(self(),messages))]),
            error({timeout, TOMsg})
    after
        ?TIMEOUT + 1000 ->
            ?LOG(Debug, "OUTER timeout. Q = ~p", [element(2,process_info(self(),messages))]),
            error(outer_timeout)
    end.

check_incorrect_mutual_close(Cfg) ->
    config(Cfg),
    Debug = get_debug(Cfg),

    Fun = proplists:get_value(wrong_action_detailed, Cfg),
    Test =
        fun({Closer, Malicious}, Cfg1) ->
            ?LOG(Debug, "check incorrect mutual_close from ~p where malicious is the ~p", [Closer, Malicious]),
            Port = proplists:get_value(port, Cfg1, ?PORT),
            Cfg2 = set_configs([?SLOGAN], load_idx(Cfg1)),
            #{ i := #{fsm := FsmI} = I
             , r := #{fsm := FsmR} = R
             , spec := Spec } = create_channel_(Cfg2),
            Data = {I, R, Spec, Port, Debug},

            Fun(Data, Closer, Malicious,
                {shutdown, [#{}], shutdown, shutdown_ack},
                fun(#{fsm := FsmPid} = Who, Debug1) ->
                    ?LOG(Debug1, "checking state of ~p (Closer=~p, Malicious=~p, FsmI = ~p, FsmR = ~p)",
                        [FsmPid, Closer, Malicious, FsmI, FsmR]),
                    {ok, _} = receive_from_fsm(conflict, Who, any_msg(),
                                               ?TIMEOUT, Debug1),
                    case Closer =:= Malicious of
                        true ->
                            {ok, _} = receive_from_fsm(conflict, Who, any_msg(),
                                                      ?TIMEOUT, Debug1);
                        false ->
                            pass
                    end,
                    open = fsm_state(FsmPid, Debug),
                    ok
                end),
            bump_idx(),

            assert_empty_msgq(Debug),

            set_configs([{port, Port + 1}], Cfg2)
          end,
    Roles = [initiator, responder],
    Combinations = [{Closer, Malicious} || Closer <- Roles,
                                           Malicious <- Roles],
    lists:foldl(Test, Cfg, Combinations),
    ok.

check_mutual_close_with_wrong_amounts(Cfg) ->
    Debug = get_debug(Cfg),
    Cfg1 = set_configs([ ?SLOGAN
                       , {channel_reserve, 5000}
                       , {push_amount, 0}
                       , {initiator_amount, 5001}
                       , {responder_amount, 5001} ], Cfg),
    {Si, Sr, Spec} = channel_spec(Cfg1),
    Port = proplists:get_value(port, Cfg, 9325),
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R } =
        create_channel_from_spec(Si, Sr, Spec, Port, Debug, Cfg),
    %% We don't have enough funds to cover the closing fee
    {error, insufficient_funds} = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),
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
    StartAmt = 100000 * 3 * aec_test_utils:min_gas_price(),
    Cfg1 = set_configs([ ?SLOGAN
                       , {channel_reserver, 5000}
                       , {push_amount, 0}
                       , {initiator_amount, StartAmt}
                       , {responder_amount, StartAmt}
                       ], Cfg),
    {Si, Sr, Spec} = channel_spec(Cfg1),
    MutualCloseTimeout = 2000,
    SignTimeout = 2 * MutualCloseTimeout,
    Spec1 = Spec#{
        timeouts => #{
            sign => SignTimeout,
            accept => MutualCloseTimeout
        }
    },
    Port = proplists:get_value(port, Cfg, 9325),
    #{ i := #{fsm := FsmI} = I
     , r := #{fsm := FsmR} = R } =
        create_channel_from_spec(Si, Sr, Spec1, Port, Debug, Cfg),
    %% One of the parties solo closes the channel
    ok = rpc(dev1, aesc_fsm, close_solo, [FsmI, #{}]),
    {_, SignedCloseSoloTx} = await_signing_request(close_solo_tx, I, Cfg),
    wait_for_signed_transaction_in_block(dev1, SignedCloseSoloTx),
    %% Before the lima fork the FSM should make sure that we
    %% cannot shutdown the channel after it was closed but before the TTL runs out
    case aect_test_utils:latest_protocol_version() < ?LIMA_PROTOCOL_VSN of
        true ->
            {error, unknown_request} = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),
            {error, unknown_request} = rpc(dev1, aesc_fsm, shutdown, [FsmR, #{}]),

            %% Check that a malicious shutdown request does not kill the FSM
            ok = rpc(dev1, aesc_fsm, strict_checks, [FsmI, false]),
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),
            {_, _} = await_signing_request(shutdown, I, Cfg),

            %% TODO: Check if we receive an ?UpdateErr message
            channel_closing = fsm_state(FsmR, Debug),
            timer:sleep(SignTimeout + 100), %% For now just wait for a timeout
            channel_closing = fsm_state(FsmI, Debug);
        false ->
            % Test that timeouts do not kill the FSM
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmR, #{}]),
            timer:sleep(SignTimeout + 100),
            channel_closing = fsm_state(FsmI, Debug),
            channel_closing = fsm_state(FsmR, Debug),

            % Test that after sending the SHUTDOWN message and timing out we
            % are still alive
            ok = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),
            {_, _} = await_signing_request(shutdown, I, Cfg),
            {ok, _} = receive_info(R, shutdown, Debug),
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
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
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

fsm_crash_reestablish(#{channel_id := ChId, fsm := FsmI, fsm_id := IFsmId} = I,
                      #{fsm := FsmR, fsm_id := RFsmId} = R, Spec, Cfg, Action) ->
    Debug = get_debug(Cfg),
    {ok, State} = rpc(dev1, aesc_fsm, get_offchain_state, [FsmI]),
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    ?LOG(Debug, "Performing action before crashing the FSM", []),
    ok = Action(I, R, Cfg),
    ?LOG(Debug, "Simulating random crash", []),
    erlang:exit(FsmI, test_fsm_random_crash),
    erlang:exit(FsmR, test_fsm_random_crash),
    timer:sleep(20), %% Give some time for the cache to persist the state

    Cache = cache_status(ChId),
    [] = in_ram(Cache),
    [_, _] = on_disk(Cache),
    check_info(20),

    ?LOG(Debug, "reestablish with wrong fsm id...", []),
    Info = #{i => I, r => R, spec => Spec},
    reestablish_wrong_fsm_id(Info, SignedTx, ?PORT, Debug),

    ?LOG(Debug, "reestablishing ...", []),
    #{i := #{fsm_id := IFsmId1} = I1, r := #{fsm_id := RFsmId1} = R1} = reestablish_(Info, SignedTx, ?PORT, Debug),
    %% Assert that the FSM ID's have changed
    true = IFsmId /= IFsmId1,
    true = RFsmId /= RFsmId1,
    true = IFsmId1 /= RFsmId1,
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
    rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}], Debug),
    {_, _} = await_signing_request(shutdown, I, Debug, Cfg),
    {ok, _} = receive_from_fsm(shutdown_ack, R, signing_req(), ?TIMEOUT, Debug),
    ok.

fsm_state(Pid, Debug) ->
    {State, _Data} = rpc(dev1, sys, get_state, [Pid, 1000], _RpcDebug = false),
    ?LOG(Debug, "fsm_state(~p) -> ~p", [Pid, State]),
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

wrong_id_action(ChannelStuff, Poster, Malicious, FsmStuff) ->
    wrong_id_action(ChannelStuff, Poster, Malicious, FsmStuff,
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
                    ?LOG(Debug1, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
                    SignedTx = SignTxFun(SignedTx0, Priv),
                    Action(Tag, Signer, SignedTx, Updates)
            after ?TIMEOUT ->
                    error(timeout)
            end
        end,
    IAmt = IAmt0 - PushAmount,
    RAmt = RAmt0 + PushAmount,
    {ok, FsmR} = rpc(dev1, aesc_client, respond, [Port, Spec], Debug),
    {ok, FsmI} = rpc(dev1, aesc_client, initiate, ["localhost", Port, Spec], Debug),

    ?LOG(Debug, "FSMs, I = ~p, R = ~p", [FsmI, FsmR]),

    I1 = I#{fsm => FsmI, initiator_amount => IAmt, responder_amount => RAmt},
    R1 = R#{fsm => FsmR, initiator_amount => IAmt, responder_amount => RAmt},

    _ = receive_fsm_id(dev1, I1, Debug),
    _ = receive_fsm_id(dev1, R1, Debug),

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
            {ok,_} = receive_from_fsm(info, I1, fun(#{info := {died, disconnect}}) -> ok end,
                                      ?TIMEOUT, Debug);
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
            {ok,_} = receive_from_fsm(info, R1, fun(#{info := {died, disconnect}}) -> ok end,
                                      ?TIMEOUT, Debug)
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
                              ?LOG(Debug1, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
                              Tx0 = aetx_sign:innermost_tx(SignedTx0),
                              Tx = ModifyTxFun(Tx0, Fsm),
                              ?LOG(Debug1, "modified ~p", [Tx]),
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
    ?LOG(Debug, "Testing with Poster ~p, Malicious ~p", [Poster, Malicious]),
    #{fsm := FsmI} = I,
    #{fsm := FsmR} = R,
    {D, A, FsmD, FsmA} =
        case Poster of
            initiator -> {I, R, FsmI, FsmR};
            responder -> {R, I, FsmR, FsmI}
        end,
    Post =
        fun() ->
            Action = fun() -> rpc(dev1, aesc_fsm, FsmFun, [FsmD | FsmFunArg])
                     end,
            case Action() of
                ok -> ok;
                {error, unknown_request} -> % race in test
                    timer:sleep(100),
                    %% retry
                    ok = Action()
            end
        end,
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
            DetectConflictFun(D, Debug),
            %% at this point the acknowledger is malicious and is convinced
            %% that the transaction was valid so it is rightfully in an `open`
            %% state when one receives the conflict message by the other party
            %% since this test is checking if the starting party (D) is
            %% reporting the conflict, we don't exepect the malicious
            %% acknowledger to report anything
            rpc(dev1, aesc_fsm, strict_checks, [FsmA, true], Debug)
    end,
    check_info(50),
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
            ?LOG(Debug, "Fsm state (~p) is ~p - retrying for ~p", [FsmPid, Other, St]),
            timer:sleep(50),
            wait_for_fsm_state(St, FsmPid, Retries-1, Debug)
    end.

shutdown_(#{fsm := FsmI, channel_id := ChannelId} = I, R, Cfg) ->
    Debug = get_debug(Cfg),
    assert_empty_msgq(Debug),

    % Send shutdown
    AlreadyClosing = proplists:get_value(already_closing, Cfg, false),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),

    % Verify shutdown process
    {I1, _} = await_signing_request(shutdown, I, Cfg),
    {R1, _} = await_signing_request(shutdown_ack, R, Cfg),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, ?TIMEOUT), % same tx

    % Mine until tx is on chain, but not confirmed
    wait_for_signed_transaction_in_block(dev1, SignedTx),

    verify_close_mutual_tx(SignedTx, ChannelId),
    if AlreadyClosing ->
           ok;
       true ->
           {ok, _} = receive_info(I1, closing, Debug),
           {ok, _} = receive_info(R1, closing, Debug)
    end,
    SignedTx = await_on_chain_report(I1, #{info => channel_closed}, ?TIMEOUT), % same tx
    SignedTx = await_on_chain_report(R1, #{info => channel_closed}, ?TIMEOUT), % same tx

    % Mine until tx is confirmed
    MinDepth = config(minimum_depth, Cfg, ?MINIMUM_DEPTH),
    mine_blocks(dev1, MinDepth,
                opt_add_to_debug(#{ signed_tx => SignedTx
                                  , current_height => current_height(dev1) }, Debug)),

    % Final checks
    {ok, _} = receive_info(I1, closed_confirmed, Debug),
    {ok, _} = receive_info(R1, closed_confirmed, Debug),
    {ok, _} = receive_info(R1, shutdown, Debug),
    {ok, _} = receive_info(I1, fun died_normal/1, Debug),
    {ok, _} = receive_info(R1, fun died_normal/1, Debug),

    % We don't assert that no messages are left here because we don't know the
    % context in which the shutdown was called. This must be handled by the
    % caller.
    ok.

settle_(MinDepth, #{fsm := FsmI, channel_id := ChannelId} = I, R, Debug,
       Cfg) ->
    ok = rpc(dev1, aesc_fsm, settle, [FsmI, #{}]),
    {_, SignedTx} = await_signing_request(settle_tx, I, Cfg),
    ?LOG(Debug, "settle_tx signed", []),
    {ok, _MinedKeyBlocks} = mine_blocks_until_txs_on_chain(
                             dev1,
                             [aeser_api_encoder:encode(tx_hash,
                                                       aetx_sign:hash(SignedTx))]),
    SignedTx = await_on_chain_report(I, #{info => channel_closed}, ?TIMEOUT), % same tx
    ?LOG(Debug, "I received On-chain report: ~p", [SignedTx]),
    SignedTx = await_on_chain_report(R, #{info => channel_closed}, ?TIMEOUT), % same tx
    ?LOG(Debug, "R received On-chain report: ~p", [SignedTx]),
    verify_settle_tx(SignedTx, ChannelId),
    ?LOG(Debug, "settle_tx verified", []),
    mine_blocks(dev1, MinDepth),
    {ok, _} = receive_from_fsm(info, I, closed_confirmed, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(info, R, closed_confirmed, ?TIMEOUT, Debug),
    ?LOG(Debug, "closed_confirmed received from both", []),
    {ok,_} = receive_from_fsm(info, I, fun died_normal/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun died_normal/1, ?TIMEOUT, Debug),
    ?LOG(Debug, "died_normal detected from both", []),
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
    #{ proxy := Proxy } = RoleI = maps:get(MapKey, Ch),
    ?LOG(Debug, "Ch = ~p", [Ch]),
    {error, _} = Err = try_reconnect(RoleI, Debug),
    ?LOG(Debug, "Reconnecting before disconnecting failed: ~p", [Err]),
    unlink(Proxy),
    exit(Proxy, kill),
    timer:sleep(50),  % give the above exit time to propagate
    ok = things_that_should_fail_if_no_client(Role, I, R, Debug, Cfg),
    Res = reconnect(RoleI, Debug),
    ?LOG(Debug, "Reconnect req -> ~p", [Res]),
    %% run tests here
    shutdown_(I, R, Cfg).

things_that_should_fail_if_no_client(ClientRole, I, R, Debug, Cfg) ->
    update_req_conflict(ClientRole, I, R, Debug, Cfg).

update_req_conflict(Role, I, R, Debug, Cfg) ->
    %% Note that the A and B represent I and R, possibly switched, to
    %% indicate who should initiate the update request. Here, A will be
    %% the side that currently has no client.
    { #{ pub := PubA, fsm := FsmA } = A
    , #{ pub := PubB, fsm := FsmB } = B } = roles_for_update(Role, I, R),
    %% cannot request an update from the side with client disconnected
    {error, client_disconnected} = rpc(dev1, aesc_fsm, upd_transfer,
                                       [FsmA, PubA, PubB, 1]),
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

reconnect(#{fsm := Fsm, fsm_id := FsmId} = R, Debug) ->
    Me = self(),
    NewProxy = spawn_link(fun() ->
                                  erlang:monitor(process, Me),
                                  ok = try_reconnect(R, Debug),
                                  %% The same ID should be sent as before to indicate that this is the same FSM as before
                                  FsmId = receive_fsm_id(dev1, R, Debug),
                                  ?LOG(Debug, "Reconnect successful; Fsm = ~p", [Fsm]),
                                  Me ! {self(), reconnected},
                                  fsm_relay(R, Me, Debug)
                     end),
    receive
        {NewProxy, reconnected} ->
            ok
    after
        1000 ->
            error(timeout)
    end,
    R#{ proxy => NewProxy }.

try_reconnect(#{ fsm  := Fsm
               , fsm_id := FsmId
               }, Debug) ->
    rpc(dev1, aesc_fsm, reconnect_client, [Fsm, self(), aesc_fsm_id:from_binary(FsmId)], Debug).

%% @doc Retry N times, T ms apart, if F() raises an exception.
%% Used in places where there could be a race.
retry(N, T, F) ->
    retry(N, T, F, undefined, []).

retry(0, _, _, E, ST) ->
    error(E, ST);
retry(N, T, F, _, _) when N > 0 ->
    try F()
    ?_catch_(error, E, ST)
        timer:sleep(T),
        retry(N-1, T, F, E, ST)
    end.

cache_status(ChId) ->
    rpc(dev1, aesc_state_cache, cache_status, [ChId]).

in_ram(St)  -> proplists:get_value(in_ram, St).
on_disk(St) -> proplists:get_value(on_disk, St).

collect_acks(Pids, Tag, N) ->
    ?LOG("collect_acks, Tag = ~p, N = ~p, Pids = ~p", [Tag, N, Pids]),
    collect_acks_(Pids, Tag, N).

collect_acks_([Pid | Pids], Tag, N) ->
    Timeout = 90000 + (N div 10)*5000,  % wild guess
    receive
        {Pid, Tag} ->
            ?LOG("Ack from ~p (~p)", [Pid, Tag]),
            [Pid | collect_acks_(Pids, Tag, N)]
    after Timeout ->
            ?LOG("Missing ack (Tag = ~p) from ~p", [Tag, Pid]),
            error(timeout)
    end;
collect_acks_([], _Tag, _) ->
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
    #{i := I, r := R} = create_channel_([{timeout, ?LONG_TIMEOUT} | Cfg], Debug),
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
            ?LOG("Got {transfer, ~p}", [N]),
            {_, I1, R1} = do_n(N, fun update_volley/3, I, R, Cfg),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg);
        {msgs, N} ->
            {_, I1, R1} = do_n(N, fun msg_volley/3, I, R, Cfg),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg);
        die ->
            ?LOG("~p got die request", [self()]),
            #{ proxy := ProxyI } = I,
            #{ proxy := ProxyR } = R,
            ProxyI ! {self(), die},
            ProxyR ! {self(), die},
            exit(normal);
        Other ->
            ?LOG(get_debug(Cfg), "Got Other = ~p, I = ~p~nR = ~p", [Other, I, R]),
            ch_loop(I, R, Parent, Cfg)
    end.

create_channel_from_spec(I, R, Spec, Port, Debug, Cfg) ->
    create_channel_from_spec(I, R, Spec, Port, false, Debug, Cfg).

create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg) ->
    assert_empty_msgq(Debug),
    %% TODO: Somehow there is a CI only race condition which rarely occurs in
    %% round_too_high.check_incorrect_* and round_too_low.check_incorrect_* tests
    %% For now just wrap this operation in a retry loop and come back to it later
    ?LOG("=== Create channel~n", []),
    ?LOG("Spec = ~p", [Spec]),
    RSpec = customize_spec(responder, Spec, Cfg),
    ISpec = customize_spec(initiator, Spec, Cfg),
    RProxy = spawn_responder(Port, RSpec, R, UseAny, Debug),
    timer:sleep(100),
    IProxy = spawn_initiator(Port, ISpec, I, Debug),
    ?LOG("RProxy = ~p, IProxy = ~p", [RProxy, IProxy]),
    Timeout = proplists:get_value(timeout, Cfg, ?TIMEOUT),
    Info = match_responder_and_initiator(RProxy, Debug, Timeout),
    #{ i := #{ fsm := FsmI, fsm_id := IFsmId } = I1
     , r := #{ fsm := FsmR, fsm_id := RFsmId } = R1 } = Info,
    %% Assert that the ID's are different
    true = IFsmId /= RFsmId,
    ?LOG(Debug, "channel paired: ~p", [Info]),
    ?LOG(Debug, "FSMs, I = ~p, R = ~p", [FsmI, FsmR]),
    {I2, R2} = try await_create_tx_i(I1, R1, Debug, Cfg)
               ?_catch_(error, Err, ST)
                   ?LOG("Caught Err = ~p", [Err]),
                   ?PEEK_MSGQ(Debug),
                   error(Err, ST)
               end,
    SignedTx = await_on_chain_report(R2, #{info => funding_created}, ?TIMEOUT),
    ?LOG(Debug, "=== SignedTx = ~p", [SignedTx]),
    SignedTx = await_on_chain_report(I2, #{info => funding_signed}, ?TIMEOUT), % same tx
    {ok, _} = wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    ?LOG(Debug, "=== Signed tx in block: ~p", [SignedTx]),

    SignedTx = await_channel_changed_report(I2, ?TIMEOUT),
    SignedTx = await_channel_changed_report(R2, ?TIMEOUT),
    CurrentHeight = current_height(dev1),
    MinDepth = config(minimum_depth_channel, Cfg, ?MINIMUM_DEPTH),
    ?LOG(Debug, "mining blocks on dev1 for minimum depth = ~p", [MinDepth]),
    mine_blocks(dev1, MinDepth, opt_add_to_debug(#{ signed_tx => SignedTx
                                                  , current_height => CurrentHeight }, Debug)),
    %% in case of multiple channels starting in parallel - the mining above
    %% has no effect (the blocks are mined in another process)
    %% The following line makes sure this process is blocked until the proper
    %% height is reached
    aecore_suite_utils:wait_for_height(aecore_suite_utils:node_name(dev1),
                                       CurrentHeight + MinDepth),
    ?LOG(Debug, "=== Min-depth height of ~p on top of ~p achieved", [MinDepth, CurrentHeight]),
    %% we've seen 10-15 second block times in CI, so wait a while longer

    await_own_funding_locked(I2, ?TIMEOUT, Debug),
    await_own_funding_locked(R2, ?TIMEOUT, Debug),

    % check the channel is present on-chain
    {ok, ChannelId} = aesc_utils:channel_pubkey(SignedTx),
    {ok, _} = rpc(dev1, aec_chain, get_channel, [ChannelId]),

    I3 = await_funding_locked(I2, ?TIMEOUT, Debug),
    R3 = await_funding_locked(R2, ?TIMEOUT, Debug),

    ?LOG(Debug, "=== Funding locked ===", []),
    I4 = await_update_report(I3, ?TIMEOUT, Debug),
    R4 = await_update_report(R3, ?TIMEOUT, Debug),

    ?LOG(Debug, "=== Update reports received ===", []),
    I5 = await_open_report(I4, ?TIMEOUT, Debug),
    R5 = await_open_report(R4, ?TIMEOUT, Debug),
    assert_empty_msgq(Debug),

    trace_checkpoint(?TR_CHANNEL_CREATED, Cfg),
    maps:merge(#{i => I5, r => R5, spec => Spec},
               maps:from_list([{K,V} || {K,V} <- Cfg,
                                        lists:member(K, [initiator_opts,
                                                         responder_opts])])).

customize_spec(Role, Spec, Cfg) ->
    maps:merge(Spec, custom_spec_opts(Role, Cfg)).

custom_spec_opts(responder, Cfg) ->
    proplists:get_value(responder_opts, Cfg, #{});
custom_spec_opts(initiator, Cfg) ->
    proplists:get_value(initiator_opts, Cfg, #{}).

spawn_responder(Port, Spec, R, UseAny, Debug) ->
    Me = self(),
    spawn_link(fun() ->
                       ?LOG("responder spawned: ~p", [Spec]),
                       Spec1 = maybe_use_any(UseAny, Spec#{ client => self() }),
                       {ok, Fsm} = rpc(dev1, aesc_client, respond, [Port, Spec1], Debug),
                       responder_instance_(Fsm, Spec1, R, Me, Debug)
               end).

maybe_use_any(true, Spec) ->
    Spec#{initiator => any};
maybe_use_any(false, Spec) ->
    Spec.

spawn_initiator(Port, Spec, I, Debug) ->
    Me = self(),
    spawn_link(fun() ->
                       ?LOG(Debug, "initiator spawned: ~p", [Spec]),
                       Spec1 = Spec#{ client => self() },
                       {ok, Fsm} = rpc(dev1, aesc_client, initiate,
                                       ["localhost", Port, Spec1], Debug),
                       initiator_instance_(Fsm, Spec1, I, Me, Debug)
               end).

move_fsm_id_to_spec(#{fsm_id := FsmId}, Spec) ->
    Spec#{existing_fsm_id_wrapper => aesc_fsm_id:from_binary(FsmId)}.

match_responder_and_initiator(RProxy, Debug, Timeout) ->
    receive
        {channel_up, RProxy, Info} ->
            ?LOG(Debug, "Matched initiator/responder pair: ~p", [Info]),
            Info
    after Timeout ->
            ?LOG(Debug, "Timed out waiting for matched pair", []),
            error(timeout)
    end.

responder_instance_(Fsm, Spec, R0, Parent, Debug) ->
    R = fsm_map(Fsm, Spec, R0),
    FsmId = receive_fsm_id(dev1, R, Debug),
    {ok, ChOpen} = receive_from_fsm(info, R, channel_open, ?LONG_TIMEOUT, Debug),
    ?LOG(Debug, "Got ChOpen: ~p~nSpec = ~p", [ChOpen, Spec]),
    {ok, #{ channel_id := TmpChanId }} = rpc(dev1, aesc_fsm, get_state, [Fsm]),
    ?LOG(Debug, "TmpChanId = ~p", [TmpChanId]),
    R1 = R#{ proxy => self(), parent => Parent, fsm_id => FsmId },
    gproc:reg({n,l,{?MODULE,TmpChanId,responder}}, #{ r => R1 }),
    {_IPid, #{ i := I1 , channel_accept := ChAccept }}
        = gproc:await({n,l,{?MODULE,TmpChanId,initiator}}, ?TIMEOUT),
    Parent ! {channel_up, self(), #{ i => I1#{parent => Parent}
                                     , r => R1
                                     , channel_accept => ChAccept
                                     , channel_open   => ChOpen }},
    fsm_relay(R, Parent, Debug).

initiator_instance_(Fsm, Spec, I0, Parent, Debug) ->
    I = fsm_map(Fsm, Spec, I0),
    FsmId = receive_fsm_id(dev1, I, Debug),
    {ok, ChAccept} = receive_from_fsm(info, I, channel_accept, ?LONG_TIMEOUT, Debug),
    ?LOG(Debug, "Got ChAccept: ~p~nSpec = ~p", [ChAccept, Spec]),
    {ok, #{ channel_id := TmpChanId }} = rpc(dev1, aesc_fsm, get_state, [Fsm]),
    ?LOG(Debug, "TmpChanId = ~p", [TmpChanId]),
    I1 = I#{ proxy => self(), fsm_id => FsmId },
    gproc:reg({n,l,{?MODULE,TmpChanId,initiator}}, #{ i => I1
                                                    , channel_accept => ChAccept }),
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
    ?LOG(Debug, "fsm_relay(~p, ~p, Debug)", [Map, Parent]),
    fsm_relay_(Map, #relay_st{ parent = Parent
                             , debug  = Debug }).

fsm_relay_(#{ fsm := Fsm } = Map, #relay_st{ parent = Parent
                                           , debug  = Debug } = St) ->
    St1 = receive
              {aesc_fsm, Fsm, _} = Msg ->
                  ?LOG(Debug, "Relaying(~p) ~p", [Parent, Msg]),
                  Parent ! Msg,
                  St;
              {Parent, debug, NewDebug} when is_boolean(NewDebug) ->
                  ?LOG(NewDebug, "Applying new debug mode: ~p", [NewDebug]),
                  Parent ! {self(), debug_ack, Debug},
                  St#relay_st{ debug = NewDebug };
              {Parent, die} ->
                  ?LOG(Debug, "Got 'die' from parent", []),
                  rpc(dev1, aesc_fsm, stop, [Fsm]),
                  ?LOG(Debug, "relay stopping (die)", []),
                  exit(normal);
              Other ->
                  ?LOG(Debug, "Relay got Other: ~p", [Other]),
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
    {I1, SCreateTx} = await_signing_request(create_tx, I, Debug, Cfg),
    CreateTx = aetx_sign:innermost_tx(SCreateTx),
    await_funding_created_p(I1, R, CreateTx, Debug, Cfg).

await_funding_created_p(I, R, CreateTx, Debug, Cfg) ->
    receive_from_fsm(info, R, funding_created, ?TIMEOUT, Debug),
    {R1, SCreateTx} = await_signing_request(funding_created, R, Debug, Cfg),
    CreateTx = aetx_sign:innermost_tx(SCreateTx), %% assert same tx
    await_funding_signed_i(I, R1, Debug).

await_funding_signed_i(I, R, Debug) ->
    receive_info(I, funding_signed, Debug),
    {I, R}.

await_funding_locked(R, Timeout, Debug) ->
    await_locked_rpt(funding_locked, R, Timeout, Debug).

await_deposit_locked(R, Timeout, Debug) ->
    await_locked_rpt(deposit_locked, R, Timeout, Debug).

await_withdraw_locked(R, Timeout, Debug) ->
    await_locked_rpt(withdraw_locked, R, Timeout, Debug).

await_locked_rpt(Type, #{role := Role} = R, Timeout, Debug) when Type==funding_locked;
                                                                 Type==deposit_locked;
                                                                 Type==withdraw_locked ->
    {ok, Msg} = receive_from_fsm(info, R, Type, Timeout, Debug),
    ?LOG(Debug, "~p got ~p: ~p", [Role, Type, Msg]),
    R#{channel_id => maps:get(channel_id, Msg)}.

await_update_report(#{channel_id := ChId} = R, Timeout, Debug) ->
    Fun = fun(#{ channel_id := ChId1 , info := SignedTx } ) ->
                  true = ChId1 == ChId andalso element(1, SignedTx) == signed_tx
          end,
    {ok, Msg} = receive_from_fsm(update, R, Fun, Timeout, Debug),
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
    ?LOG(Debug, "await_signing_request, Fsm = ~p (Pub = ~p, Other = ~p)",
        [Fsm, Pub, [P || #{pub := P} <- OtherSigs]]),
    receive
        {aesc_fsm, Fsm, #{ type := sign, tag := Tag
                         , info := #{ signed_tx := SignedTx0
                                    , updates   := Updates } } = Msg} ->
            ?LOG(Debug, "await_signing(~p, ~p, ~p, ~p) <- ~p",
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
            ?LOG(Debug,"SIGNED TX ~p", [SignedTx]),
            Action(Tag, Signer1, SignedTx, Updates)
    after Timeout ->
            error(timeout)
    end.

abort_signing_request(Tag, #{fsm := Fsm}, Code, Timeout, Debug) ->
    ?LOG(Debug, "waiting to abort signing req, Fsm = ~p, (Code = ~p)",
        [Fsm, Code]),
    receive
        {aesc_fsm, Fsm, #{ type := sign, tag := Tag
                         , info := #{ signed_tx := SignedTx
                                    , updates   := _ } } = Msg} ->
            ?LOG(Debug, "abort_signing(~p, ~p, ~p) <- ~p", [Tag, Code, Fsm, Msg]),
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
    ?LOG("~p awaiting on-chain from ~p", [self(), Fsm]),
    receive
        {aesc_fsm, Fsm, #{info := {died, _}}} = Died ->
            ?LOG("Fsm died while waiting for on-chain report:~n"
                   "~p", [Died]),
            error(fsm_died);
        {aesc_fsm, Fsm, #{type := report, tag := on_chain_tx,
                          info := #{tx := SignedTx} = I}} = M ->
            ?LOG("OnChainRpt = ~p", [M]),
            ok = match_info(I, Match),
            SignedTx
    after Timeout ->
            ?PEEK_MSGQ(true),
            error(timeout)
    end.

await_channel_changed_report(#{fsm := Fsm}, Timeout) ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := on_chain_tx
                         , info := #{ info := channel_changed
                                    , tx   := SignedTx } }} ->
            SignedTx
    after Timeout ->
            ?PEEK_MSGQ(true),
            error(timeout)
    end.

await_min_depth_reached(#{fsm := Fsm}, TxHash, TxType, Timeout) ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := info
                         , info := #{ event   := min_depth_achieved 
                                    , tx_hash := TxHash
                                    , type    := TxType} }} ->
            ok
    after Timeout ->
            ?PEEK_MSGQ(true),
            error(timeout)
    end.

await_own_funding_locked(Role, Timeout, Debug) ->
    await_own_locked_rpt(own_funding_locked, Role, Timeout, Debug).

await_own_deposit_locked(Role, Timeout, Debug) ->
    await_own_locked_rpt(own_deposit_locked, Role, Timeout, Debug).

await_own_withdraw_locked(Role, Timeout, Debug) ->
    await_own_locked_rpt(own_withdraw_locked, Role, Timeout, Debug).

await_own_locked_rpt(Type, #{role := Role, fsm := Fsm}, Timeout, Debug)
  when Type==own_funding_locked;
       Type==own_deposit_locked;
       Type==own_withdraw_locked ->
    receive
        {aesc_fsm, Fsm, #{ type := report
                         , tag  := info
                         , info := Type } = Msg} ->
            ?LOG(Debug, "~p got ~p: ~p", [Role, Type, Msg]),
            ok
    after Timeout ->
            ?PEEK_MSGQ(Debug),
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

await_open_report(#{fsm := Fsm} = R, Timeout, Debug) ->
    receive {aesc_fsm, Fsm, #{type := report, tag := info, info := open} = Msg} ->
                {ok, ChannelId} = maps:find(channel_id, Msg),
                R#{channel_id => ChannelId}
    after Timeout ->
            ?PEEK_MSGQ(Debug),
            error(timeout)
    end.

await_update_incoming_report(#{fsm := Fsm, channel_id := ChannelId}, Timeout, Debug) ->
    receive {aesc_fsm, Fsm, #{type := report, tag := info, info := update} = Msg} ->
            {ok, ChannelId} = maps:find(channel_id, Msg)
    after Timeout ->
            ?PEEK_MSGQ(Debug),
            error(timeout)
    end.

await_leave(#{channel_id := ChId0} = R, Timeout, Debug) ->
    receive_from_fsm(
      leave, R,
      fun(#{channel_id := ChId, info := SignedTx})
            when ChId == ChId0, element(1, SignedTx) == signed_tx ->
              ok
      end, Timeout, Debug).

receive_log(P, Debug) ->
    Fun = fun(#{info := {log, _}}) -> ok end,
    {ok, _} = receive_from_fsm(debug, P, Fun, ?LONG_TIMEOUT, Debug).

receive_fsm_id(Node, Peer, Debug) ->
    {ok, #{info := {fsm_up, FsmIdWrapper}}} = receive_info(Peer, fun fsm_up/1, Debug),
    %% We need to perform an RPC call as the actual crypto key is stored on the node itself
    rpc(Node, aesc_fsm_id, retrieve, [FsmIdWrapper], Debug).

receive_info(R, Msg, Debug) ->
    {ok, _} = receive_from_fsm(info, R, Msg, ?LONG_TIMEOUT, Debug).

receive_from_fsm(Tag, R, Info, Timeout) ->
    receive_from_fsm(Tag, R, Info, Timeout, false).

receive_from_fsm(Tag, R, Info, Timeout, Debug) ->
    {ok, _} = receive_from_fsm(Tag, R, Info, Timeout, Debug, false).

receive_from_fsm(Tag, #{role := Role, fsm := Fsm} = R, Info, Timeout, Debug, _Cont)
  when is_atom(Info) ->
    ?LOG(Debug, "receive_from_fsm_(~p, ~p, ~p, ~p, ~p, _Cont)",
        [Tag, R, Info, Timeout, Debug]),
    receive
        {aesc_fsm, Fsm, #{type := _Type, tag := Tag, info := Info} = Msg} ->
            ?LOG(Debug, "~p: received ~p:~p", [Role, Tag, Msg]),
            {ok, Msg}
    after Timeout ->
            flush(),
            error(timeout)
    end;
receive_from_fsm(Tag, R, Msg, Timeout, Debug, Cont) ->
    TRef = erlang:start_timer(Timeout, self(), receive_from_fsm),
    receive_from_fsm_(Tag, R, Msg, TRef, Debug, Cont).

receive_from_fsm_(Tag, #{role := Role, fsm := Fsm} = R, Msg, TRef, Debug, Cont) ->
    ?LOG(Debug, "receive_from_fsm_(~p, ~p, ~p, ~p, ~p, ~p)",
        [Tag, R, Msg, TRef, Debug, Cont]),
    receive
        {aesc_fsm, Fsm, #{type := Type, tag := Tag} = Msg1} ->
            ?LOG(Debug, "~p: received ~p:~p/~p", [Role, Type, Tag, Msg1]),
            try match_msgs(Msg, Msg1, Cont)
            catch
                throw:continue ->
                    ?LOG(Debug, "Failed match: ~p", [Msg1]),
                    receive_from_fsm_(Tag, R, Msg, TRef, Debug, Cont)
            after
                erlang:cancel_timer(TRef)
            end;
        {timeout, TRef, receive_from_fsm} ->
            flush(),
            error(timeout)
    after ?TIMEOUT ->
            ?LOG(Debug, "Outer timeout. Q = ~p", [element(2,process_info(self(), messages))]),
            error(outer_timeout)
    end.

flush() ->
    receive
        M ->
            ?LOG("<== ~p", [M]),
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
                    ?LOG("Message doesn't match fun: ~p / ~w:~w/1",
                           [Msg, Mod, Name]),
                    error({message_mismatch, [Msg]})
            end
    end;
match_msgs(M, #{info := M} = Msg, _) ->
    {ok, Msg};
match_msgs(M, M, _) ->
    {ok, M};
match_msgs(Pat, M, Cont) when is_map(M), is_map(Pat) ->
    try match_info(M, Pat),
        {ok, M}
    catch
        error:_ when Cont ->
            throw(continue)
    end;
match_msgs(_, _, true) ->
    throw(continue);
match_msgs(A, B, false) ->
    ?LOG("Messages don't match: ~p / ~p", [A, B]),
    error({message_mismatch, [A, B]}).

check_info(Timeout) -> check_info(Timeout, false).

check_info(Timeout, Debug) ->
    receive
        Msg when element(1, Msg) == aesc_fsm ->
            ?LOG(Debug, "UNEXPECTED: ~p", [Msg]),
            [Msg|check_info(Timeout, Debug)];
        Other ->
            ?LOG(Debug, "UNKNOWN MESSAGE: ~p", [Other]),
            check_info(Timeout, Debug)
    after
        Timeout ->
            ?PEEK_MSGQ(Debug),
            []
    end.

mine_blocks(Node) ->
    mine_blocks(Node, ?MINIMUM_DEPTH).

mine_blocks(Node, N) ->
    mine_blocks(Node, N, true).

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

prepare_patron(Node) ->
    prepare_patron(Node, 40).

prepare_patron(Node, Blocks) ->
    mine_key_blocks(Node, Blocks),
    {_PatronPriv, PatronPub} = patron_keys(Node),
    {ok, Balance} = rpc(Node, aehttp_logic, get_account_balance, [PatronPub]),
    ?LOG("patron: ~p blocks mined on ~p, now has ~p aettos", [Blocks, Node, Balance]),
    ok.

patron_keys(Node) ->
    {_PrivKey, _PubKey} = aecore_suite_utils:sign_keys(Node).

prep_initiator(Amount, Node) ->
    Responder = prep_account(Amount, Node),
    Responder#{role => initiator}.

prep_responder(Amount, Node) ->
    Responder = prep_account(Amount, Node),
    Responder#{role => responder}.

prep_account(Amount, Node) ->
    NodeName = aecore_suite_utils:node_name(Node),
    {_PatronPriv, PatronPub} = patron_keys(Node),
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    {ok, SignedTx} = aecore_suite_utils:spend(NodeName, PatronPub, Pub, Amount,
                                              20000 * aec_test_utils:min_gas_price()),
    {ok, _} = wait_for_signed_transaction_in_block(Node, SignedTx),
    {ok, Amount} = rpc(Node, aehttp_logic, get_account_balance, [Pub]),
    #{priv => Priv,
      pub  => Pub,
      balance => Amount,
      auth_idx => 1}.

rpc(Node, Mod, Fun, Args) -> rpc(Node, Mod, Fun, Args, false).

rpc(Node, Mod, Fun, Args, Debug) ->
    Res = rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000),
    ?LOG(Debug, "rpc(~p,~p,~p,~p) -> ~p", [Node, Mod, Fun, Args, Res]),
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

apply_updates([], R) ->
    R;
apply_updates([{meta, _} | T], R) ->
    apply_updates(T, R);
apply_updates([{create_contract, OwnerId, _VMVersion, _ABIVersion, _Code,
                Deposit, _CallData} | T], R) ->
    Owner = aeser_id:specialize(OwnerId, account),
    apply_updates(T, apply_updates_modify_amount(Owner, -1 * Deposit, R));
apply_updates([{call_contract, CallerId, _ContractId, _ABIVersion, Amount,
                _CallData, _CallStack, _GasPrice, _Gas} | T], R) ->
    Caller = aeser_id:specialize(CallerId, account),
    apply_updates(T, apply_updates_modify_amount(Caller, -1 * Amount, R));
apply_updates([{transfer, FromId, ToId, Amount} | T], R) ->
    From = aeser_id:specialize(FromId, account),
    To = aeser_id:specialize(ToId, account),
    R1 = apply_updates_modify_amount(From, -1 * Amount, R),
    R2 = apply_updates_modify_amount(To, Amount, R1),
    apply_updates(T, R2).

apply_updates_modify_amount(Who, Delta, R) ->
    case role(Who, R) of
        initiator ->
            #{initiator_amount := IAmt} = R,
            R#{initiator_amount => IAmt + Delta};
        responder ->
            #{responder_amount := RAmt} = R,
            R#{responder_amount => RAmt + Delta}
    end.
            
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
            {ok, []}
    after ?TIMEOUT ->
            error(timeout)
    end;
wait_for_signed_transaction_in_block(Node, SignedTx, Debug) ->
    ?LOG(Debug, "waiting for tx ~p", [SignedTx]),
    TxHash = aetx_sign:hash(SignedTx),
    case rpc(dev1, aec_chain, find_tx_location, [TxHash], Debug) of
        BH when is_binary(BH) ->
            ?LOG(Debug, "Found tx in block", []),
            ok;
        NotConfirmed when NotConfirmed =:= mempool;
                          NotConfirmed =:= not_found ->
            EncTxHash = aeser_api_encoder:encode(tx_hash, TxHash),
            case mine_blocks_until_txs_on_chain(Node, [EncTxHash]) of
                {ok, Blocks} ->
                    ?LOG(Debug, "Tx on-chain after mining ~p blocks", [length(Blocks)]),
                    {ok, Blocks};
                {error, _Reason} ->
                    ?LOG(Debug, "Error: ~p - did not mine", [_Reason]),
                    did_not_mine
            end
    end.

check_fsm_state(Fsm) ->
    {ok, #{ initiator       := _Initiator
          , responder       := _Responder
          , init_amt        := IAmt
          , resp_amt        := RAmt
          , state_hash      := StateHash
          , round           := Round
          , channel_status  := Status}} = aesc_fsm:get_state(Fsm),
%    Trees =
%        aec_test_utils:create_state_tree_with_accounts(
%            [aec_accounts:new(Pubkey, Balance) ||
%                {Pubkey, Balance} <- [{Initiator, IAmt}, {Responder, RAmt}]],
%            no_backend),
%    Hash = aec_trees:hash(Trees),
%    StateHash = Hash, %% assert same root hash
    {IAmt, RAmt, StateHash, Round, Status}.

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

with_trace(F, Config0, File, When) ->
    ?LOG("with_trace ...", []),
    Config = [{trace_destination, File}|Config0],
    trace_checkpoint(?TR_START, Config),
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
            ?LOG("Discarding trace", []),
            aesc_ttb:stop_nofetch();
        always ->
            ttb_stop()
    end,
    ok.

trace_checkpoint(CheckPoint, Config) ->
    case ?config(activate_trace, Config) of
        CheckPoint ->
            {_, File} = lists:keyfind(trace_destination, 1, Config),
            TTBRes = aesc_ttb:on_nodes([node()|get_nodes()], File),
            ?LOG("Trace set up: ~p", [TTBRes]);
        _ ->
            ok
    end.


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
    #{ contract    := GAContract
     , auth_fun    := GAAuthFun
     , auth_params := #{initiator := #{init_params := GAParams}}} =
        proplists:get_value(ga, Cfg),
    attach({IPub, IPrivKey}, GAContract, GAAuthFun, GAParams),
    ok.

attach_responder(Cfg) ->
    #{ pub := RPub, priv := RPrivKey } = ?config(responder, Cfg),
    #{ contract    := GAContract
     , auth_fun    := GAAuthFun
     , auth_params := #{responder := #{init_params := GAParams}}} = ?config(ga, Cfg),
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
    ok.

responder_spend(Cfg) ->
    ga_spend(responder, initiator, 333, Cfg),
    ok.

ga_spend(From, To, Amt, Cfg) ->
    #{pub := SendPub, auth_idx := _N} = ?config(From, Cfg),
    #{pub := ReceivePub} = ?config(To, Cfg),
    #{auth_params := BothAuths} = ?config(ga, Cfg),
    Auth = maps:get(From, BothAuths),
    SpendProps = #{ sender_id    => aeser_id:create(account, SendPub)
                  , recipient_id => aeser_id:create(account, ReceivePub)
                  , amount       => Amt
                  , fee          => 20000 * aec_test_utils:min_gas_price()
                  , nonce        => 0
                  , payload      => <<>>
                  },
    {ok, SpendAetx} = aec_spend_tx:new(SpendProps),
    SignedTx = meta(SendPub, Auth, 1, aetx_sign:new(SpendAetx, [])),
    ok = rpc(dev1, aec_tx_pool, push, [SignedTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(
        aecore_suite_utils:node_name(dev1), [TxHash], ?MAX_MINED_BLOCKS),
    ok.

basic_spend(From, To, Amt, Cfg) ->
    #{ pub  := SendPub} = FromAcc = ?config(From, Cfg),
    #{pub := ReceivePub} = ?config(To, Cfg),
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [SendPub]),
    SpendProps = #{ sender_id    => aeser_id:create(account, SendPub)
                  , recipient_id => aeser_id:create(account, ReceivePub)
                  , amount       => Amt
                  , fee          => 20000 * aec_test_utils:min_gas_price()
                  , nonce        => Nonce
                  , payload      => <<>>
                  },
    {ok, SpendAetx} = aec_spend_tx:new(SpendProps),
    {SignedTx, FromAcc1} = sign_tx(FromAcc, SpendAetx, Cfg),
    ok = rpc(dev1, aec_tx_pool, push, [SignedTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(
        aecore_suite_utils:node_name(dev1), [TxHash], ?MAX_MINED_BLOCKS),
    FromAcc1.

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
    Debug = get_debug(Cfg),
    NOT = 10,
    NNT = 1,
    Amount = 1,
    Cfg1 = set_configs([ ?SLOGAN
                       , {block_hash_delta, #{ not_older_than => NOT
                                             , not_newer_than => NNT
                                             , pick           => 1 }}
                       % Factor of 0 sets min depths to 1 for all amounts
                       , {minimum_depth, 1}
                       , {minimum_depth_channel, 1}
                       , {minimum_depth_factor, 0} ], Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := R
     , spec := _Spec} = create_channel_(Cfg1),
    mine_key_blocks(dev1, NOT + 2), % do not rely on min depth
    TestByDelta =
        fun(Delta, Acc) ->
            lists:foldl(
                fun(Fun, {AccRound, #{fsm := AccIFsm} = AccI, AccR}) ->
                    {_, _, _, AccRound, _} = check_fsm_state(AccIFsm),
                    BlockHash = aecore_suite_utils:get_key_hash_by_delta(dev1, Delta),
                    {ok, #{fsm := AccIFsm1} = AccI1, AccR1} = Fun(AccI, AccR, #{block_hash => BlockHash}, AccRound),
                    % We trust the sub-routines to checks that the funds are correct
                    {_, _, _, AccRound1, _} = check_fsm_state(AccIFsm1),
                    true = (AccRound < AccRound1),
                    {AccRound1, AccI1, AccR1}
                end,
                Acc,
                [ fun(IntI, IntR, IntOpts, IntRound) ->
                      deposit_(IntI, IntR, Amount, IntOpts, IntRound, Debug, Cfg1)
                  end,
                  fun(IntI, IntR, IntOpts, IntRound) ->
                      withdraw_(IntI, IntR, Amount, IntOpts, IntRound, Debug, Cfg1)
                  end
                ])
        end,
    {_, _, _, Round0, _} = check_fsm_state(FsmI),
    {_, IFinal, RFinal} =
        lists:foldl(
            TestByDelta,
            {Round0, I, R},
            [ NOT      %% border condition
            , NNT      %% border condition
            , NOT - 1  %% in the range
            , NNT + 1  %% in the range
            ]),
    shutdown_(IFinal, RFinal, Cfg1),
    ok.

%% ==================================================================
%% Shared test case logic

leave_reestablish_loop_(Cfg) ->
    Debug = get_debug(Cfg),

    % Start channel
    #{ i := I } = Info0 = create_channel_(Cfg, #{}, Debug),
    ?LOG(Debug, "I = ~p", [I]),

    % Mark that we opened the channel for the first time
    Info1 = Info0#{initial_channel_open => true},

    % Run steps 10 times
    leave_reestablish_loop_step_(10, Info1, Debug).

leave_reestablish_loop_step_(0, Info, _Debug) ->
    Info;
leave_reestablish_loop_step_(Idx, Info, Debug) ->
    assert_empty_msgq(Debug),
    #{ i := #{ channel_id := ChId
             , fsm := Fsm } = I
     , r := #{} = R } = Info,

    % Leave channel, but don't follow proper leave flow
    ?LOG(Debug, "Starting leave_reestablish attempt ~p", [Idx]),
    assert_cache_is_in_ram(ChId),
    ok = rpc(dev1, aesc_fsm, leave, [Fsm]),

    % Verify leave and FSM timeout
    {ok, #{info := SignedTx}} = await_leave(I, ?TIMEOUT, Debug),
    {ok, #{info := SignedTx}} = await_leave(R, ?TIMEOUT, Debug),
    {ok, _} = receive_info(I, fun died_normal/1, Debug),
    {ok, _} = receive_info(R, fun died_normal/1, Debug),
    {ok, #{info := {log, ILog}}} = receive_log(I, Debug),
    {ok, #{info := {log, RLog}}} = receive_log(R, Debug),
    ok = check_log(expected_fsm_logs(?FUNCTION_NAME, initiator, Info), ILog,
                   initiator),
    ok = check_log(expected_fsm_logs(?FUNCTION_NAME, responder, Info), RLog,
                   responder),

    retry(3, 100, fun() -> assert_cache_is_on_disk(ChId) end),
    assert_empty_msgq(Debug),

    % Mine to ensure all transaction are on chain and confirmed
    mine_key_blocks(dev1, ?MINIMUM_DEPTH),

    % Reestablish connection to channel
    ?LOG(Debug, "reestablishing ...", []),
    Info2 = reestablish_(Info, SignedTx, ?PORT, Debug),
    Info3 = Info2#{initial_channel_open => false},

    % Done, repeat
    ?LOG(Debug, "Ending leave_reestablish attempt ~p", [Idx]),
    assert_empty_msgq(Debug),
    leave_reestablish_loop_step_(Idx - 1, Info3, Debug).

reestablish_(Info, SignedTx, Port, Debug) ->
    assert_empty_msgq(Debug),
    #{ i := #{ channel_id := ChId
             , initiator_amount := IAmt
             , responder_amount := RAmt } = I0
     , r := #{ initiator_amount := IAmt
             , responder_amount := RAmt } = R0
     , spec := Spec0
     } = Info,
    ResponderStays = responder_stays(Spec0),

    Spec = Spec0#{ existing_channel_id => ChId
                 , offchain_tx => SignedTx
                 , initiator_amount => IAmt
                 , responder_amount => RAmt
                 },

    % Start new FSMs
    ISpec = move_fsm_id_to_spec(I0, Spec),
    RSpec = move_fsm_id_to_spec(R0, Spec),
    {FsmR, RFsmId} = if ResponderStays ->
                             ?LOG(Debug, "Not trying to reestablish responder", []),
                             {maps:get(fsm, R0), maps:get(fsm_id, R0)};
                         true ->
                             {ok, NewFsmR} = rpc(dev1, aesc_client, respond, [Port, RSpec]),
                             FsmId = receive_fsm_id(dev1, R0#{fsm => NewFsmR}, Debug),
                             {NewFsmR, FsmId}
           end,
    {ok, FsmI} = rpc(dev1, aesc_client, initiate, ["localhost", Port, ISpec], Debug),

    IFsmId = receive_fsm_id(dev1, I0#{fsm => FsmI}, Debug),
    I1 = I0#{fsm => FsmI, fsm_id => IFsmId},
    R1 = R0#{fsm => FsmR, fsm_id => RFsmId},

    % Verify FSM startup completed
    I2 = await_open_report(I1, ?TIMEOUT, Debug),
    R2 = await_open_report(R1, ?TIMEOUT, Debug),
    {ok, _} = receive_info(I2, channel_reestablished, Debug),
    {ok, _} = receive_info(R2, channel_reestablished, Debug),
    {I4, R4} = if ResponderStays ->
                      #{signed_tx := SignedTx} = I3 = await_update_report(I2, ?TIMEOUT, Debug),
                      {I3, R2};
                  true ->
                      #{signed_tx := SignedTx} = I3 = await_update_report(I2, ?TIMEOUT, Debug),
                      #{signed_tx := SignedTx} = R3 = await_update_report(R2, ?TIMEOUT, Debug),
                      {I3, R3}
               end,

    % Done
    assert_empty_msgq(Debug),
    Info#{i => I4, r => R4, spec => Spec}.

reestablish_wrong_fsm_id(Info, SignedTx, Port, Debug) ->
    assert_empty_msgq(Debug),
    #{ i := #{ channel_id := ChId
             , initiator_amount := IAmt
             , responder_amount := RAmt } = I0
     , r := #{ initiator_amount := IAmt
             , responder_amount := RAmt } = R0
     , spec := Spec0
     } = Info,

    Spec = Spec0#{ existing_channel_id => ChId
                 , offchain_tx => SignedTx
                 , initiator_amount => IAmt
                 , responder_amount => RAmt
                 },

    % Fail to start new FSMs
    BogusFsmId1 = aesc_fsm_id:retrieve(aesc_fsm_id:new()),
    BogusFsmId2 = aesc_fsm_id:retrieve(aesc_fsm_id:new()),
    ISpec = move_fsm_id_to_spec(I0#{fsm_id => BogusFsmId1}, Spec),
    RSpec = move_fsm_id_to_spec(R0#{fsm_id => BogusFsmId2}, Spec),
    {error, invalid_fsm_id} = rpc(dev1, aesc_client, respond, [Port, RSpec]),
    {error, invalid_fsm_id} = rpc(dev1, aesc_client, initiate, ["localhost", Port, ISpec], Debug),

    % Done
    assert_empty_msgq(Debug),
    Info#{spec => Spec}.

withdraw_full_cycle_(Amount, Opts, Round, Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{} = I
     , r := #{} = R
     , spec := #{}
     } = create_channel_(Cfg, Debug),
    ?LOG(Debug, "I = ~p", [I]),
    {ok, _, _} = withdraw_(I, R, Amount, Opts, Round, Debug, Cfg),
    shutdown_(I, R, Cfg),
    ok.

withdraw_(#{fsm := FsmI} = I, R, Amount, Debug, Cfg) ->
    {_IAmt0, _RAmt0, _, Round0, _} = check_fsm_state(FsmI),
    withdraw_(#{fsm := FsmI} = I, R, Amount, #{}, Round0, Debug, Cfg).

withdraw_(#{fsm := FsmI} = I, R, Amount, Opts, Round0, Debug, Cfg) ->
    assert_empty_msgq(Debug),
    % Check initial fsm state
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, Round0, _} = FsmState0 = check_fsm_state(FsmI),
    ?LOG(Debug, "Round0 = ~p, IAmt0 = ~p, RAmt0 = ~p", [Round0, IAmt0, RAmt0]),

    % Perform update and verify flow and reports
    ok = rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, Opts#{amount => Amount}]),
    {I1, _} = await_signing_request(withdraw_tx, I, Cfg),
    {R1, _} = await_signing_request(withdraw_created, R, Cfg),
    SignedTx = await_on_chain_report(I1, #{info => withdraw_signed}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, #{info => withdraw_created}, ?TIMEOUT), % same tx
    ?LOG(Debug, "=== SignedTx = ~p", [SignedTx]),
    receive_info(R1, withdraw_created, Debug), % probably an obsolete message?
    assert_empty_msgq(Debug),

    VerifyFun = fun() ->
        % Verify flow and reports after transactions has been confirmed
        await_own_withdraw_locked(I1, ?TIMEOUT, Debug),
        await_own_withdraw_locked(R1, ?TIMEOUT, Debug),
        await_withdraw_locked(I1, ?TIMEOUT, Debug),
        await_withdraw_locked(R1, ?TIMEOUT, Debug),
        SignedTx = await_channel_changed_report(I1, ?TIMEOUT), % same tx
        SignedTx = await_channel_changed_report(R1, ?TIMEOUT), % same tx
        #{signed_tx := SignedTx} = I2 = await_update_report(I1, ?TIMEOUT, Debug),
        #{signed_tx := SignedTx} = R2 = await_update_report(R1, ?TIMEOUT, Debug),
        {I2, R2}
    end,

    % Verify changes of fsm state
    MinDepth = config(minimum_depth, Cfg, ?MINIMUM_DEPTH),
    {I3, R3} = assert_fsm_states(SignedTx, I1, MinDepth, (-1 * Amount), FsmState0, VerifyFun,
                                 channel_withdraw_tx, aesc_withdraw_tx, Debug),

    % Done
    assert_empty_msgq(Debug),
    {ok, I3, R3}.

deposit_(#{fsm := FsmI} = I, R, Amount, Debug, Cfg) ->
    {_IAmt0, _RAmt0, _, Round0, _} = check_fsm_state(FsmI),

    deposit_(I, R, Amount, Round0, Debug, Cfg).

deposit_(I, R, Amount, Round0, Debug, Cfg) ->
    deposit_(I, R, Amount, #{}, Round0, Debug, Cfg).

deposit_(#{fsm := FsmI} = I, R, Amount, Opts, Round0, Debug, Cfg) ->
    assert_empty_msgq(Debug),
    % Check initial fsm state
    {IAmt0, RAmt0, _, Round0, _} = FsmState0 = check_fsm_state(FsmI),
    ?LOG(Debug, "Round0 = ~p, IAmt0 = ~p, RAmt0 = ~p", [Round0, IAmt0, RAmt0]),

    % Perform update and verify flow and reports
    ok = rpc(dev1, aesc_fsm, upd_deposit, [FsmI, Opts#{ amount => Amount}]),
    {I1, _} = await_signing_request(deposit_tx, I, Cfg),
    {R1, _} = await_signing_request(deposit_created, R, Cfg),
    SignedTx = await_on_chain_report(I1, #{info => deposit_signed}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R1, #{info => deposit_created},  ?TIMEOUT), % same tx
    ?LOG(Debug, "=== SignedTx = ~p", [SignedTx]),
    receive_info(R1, deposit_created, Debug), % probably an obsolete message?
    assert_empty_msgq(Debug),

    VerifyFun = fun() ->
        % Verify flow and reports after transactions has been confirmed
        await_own_deposit_locked(I1, ?TIMEOUT, Debug),
        await_own_deposit_locked(R1, ?TIMEOUT, Debug),
        await_deposit_locked(I1, ?TIMEOUT, Debug),
        await_deposit_locked(R1, ?TIMEOUT, Debug),
        SignedTx = await_channel_changed_report(I1, ?TIMEOUT), % same tx
        SignedTx = await_channel_changed_report(R1, ?TIMEOUT), % same tx
        #{signed_tx := SignedTx} = I2 = await_update_report(I1, ?TIMEOUT, Debug),
        #{signed_tx := SignedTx} = R2 = await_update_report(R1, ?TIMEOUT, Debug),
        {I2, R2}
    end,

    % Verify changes of fsm state
    MinDepth = config(minimum_depth, Cfg, ?MINIMUM_DEPTH),
    {I3, R3} = assert_fsm_states(SignedTx, I1, MinDepth, Amount, FsmState0, VerifyFun,
                                 channel_deposit_tx, aesc_deposit_tx, Debug),

    % Done
    assert_empty_msgq(Debug),
    {ok, I3, R3}.

%% @doc Assert FSM state before and after a deposit/withdraw transaction.
assert_fsm_states(SignedTx, FsmSpec, MinDepth, Amount, {IAmt0, RAmt0, _, Round0, _},
                  VerifyFun, TxType, TxCb, Debug) ->
    % Mine blocks until transaction is seen
    #{fsm := Fsm, channel_id := ChannelId} = FsmSpec,
    {ok, BlocksMined} = wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),

    % Find position of transaction in the chain
    SignedTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    {ok, TxPos} = tx_position_in_blocks(SignedTxHash, lists:reverse(BlocksMined)),
    ?LOG(Debug, "Tx position in blocks = ~p, with min depth = ~p", [TxPos, MinDepth]),

    % Verify fsm state before additional mining
    % In case we mined further than the min depth the transaction might already
    % have been confirmed.
    {ExpectedState1, VerifyFunRes} = case MinDepth - TxPos > 0 of
        true ->
            % no confirmation
            {{IAmt0, RAmt0, Round0}, undefined};
        false ->
            % confirmed, so also verify the incoming messages
            Res = VerifyFun(),
            {{IAmt0 - (-1 * Amount), RAmt0, Round0 + 1}, Res}
    end,
    {IAmt1, RAmt1, _, Round1, _} = check_fsm_state(Fsm),
    ?LOG(Debug, "After tx in block - Round1 = ~p, IAmt1 = ~p, RAmt1 = ~p", [Round1, IAmt1, RAmt1]),
    {IAmt1, RAmt1, Round1} = ExpectedState1,

    % Mine until transaction confirmation is expected to occur
    mine_blocks(dev1, MinDepth),

    % Verify fsm state after additional mining
    {ExpectedState2, VerifyFunRes1} = case MinDepth - TxPos > 0 of
        true ->
            % confirmed, so also verify the incoming messages
            Res1 = VerifyFun(),
            {{IAmt1 - (-1 * Amount), RAmt0, Round1 + 1}, Res1};
        false ->
            % previously confirmed and verified
            {{IAmt1, RAmt0, Round1}, VerifyFunRes}
    end,
    {IAmt2, RAmt2, StateHash, Round2, _} = check_fsm_state(Fsm),
    ?LOG(Debug, "After tx min depth - Round2 = ~p, IAmt2 = ~p, RAmt2 = ~p", [Round2, IAmt2, RAmt2]),
    {IAmt2, RAmt2, Round2} = ExpectedState2,

    % Verify channel round
    {ok, Channel} = rpc(dev1, aec_chain, get_channel, [ChannelId]),
    {TxCb, Tx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    ChannelRound = aesc_channels:round(Channel),
    TxRound = TxCb:round(Tx),
    ?LOG(Debug, "Channel on-chain round ~p, expected round ~p", [ChannelRound, TxRound]),
    {ChannelRound, ChannelRound} = {ChannelRound, TxRound},

    % Verify state hash
    {TxType, Tx} =
        aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    ct:log("Channel round ~p, Tx round ~p", [Round2, TxCb:round(Tx)]),
    {Round2, _} = {TxCb:round(Tx), Round2}, %% assert correct round
    {StateHash, _} = {TxCb:state_hash(Tx), StateHash}, %% assert correct state hash

    assert_empty_msgq(Debug),
    VerifyFunRes1.

assert_cache_is_in_ram(ChannelId) ->
    Cache = cache_status(ChannelId),
    [_,_] = in_ram(Cache),
    [] = on_disk(Cache),
    ok.

assert_cache_is_on_disk(ChannelId) ->
    Cache = cache_status(ChannelId),
    [] = in_ram(Cache),
    [_, _] = on_disk(Cache),
    ok.

assert_cache_is_gone(ChannelId) ->
    Cache = cache_status(ChannelId),
    [] = in_ram(Cache),
    [] = on_disk(Cache),
    ok.

assert_cache_is_gone_after_on_disk(ChannelId) ->
    try
        retry(3, 100, fun() -> assert_cache_is_on_disk(ChannelId) end),
        mine_blocks(dev1),
        assert_cache_is_gone(ChannelId)
    catch
        error:{badmatch, []} ->
            % At this point the cache could already be gone
            assert_cache_is_gone(ChannelId),
            ok
    end.

assert_empty_msgq(Debug) ->
    Msgs = check_info(20, Debug),
    case Msgs of
        [] ->
            ok;
        _ ->
            %% Always output these log messages
            ?LOG("Message queue length: ~p", [length(Msgs)]),
            ?LOG("Message queue entries: ~p", [Msgs]),
            ct:fail("Message queue is not empty")
    end.

%% ==================================================================
%% Internal functions

create_channel_on_port(Port) ->
    Node = dev1,
    prepare_patron(Node),
    Initiator = prep_initiator(?ACCOUNT_BALANCE, Node),
    Responder = prep_responder(?ACCOUNT_BALANCE, Node),
    Cfg1 = set_configs([ ?SLOGAN
                       , {port, Port}
                       , {initiator, Initiator}
                       , {responder, Responder}
                       , {initiator_amount, 500000}
                       , {responder_amount, 500000} ], config()),
    create_channel_(Cfg1).

create_channel_(Cfg) ->
    create_channel_(Cfg, get_debug(Cfg)).

create_channel_(Cfg, Debug) ->
    create_channel_(Cfg, #{}, Debug).

create_channel_(Cfg, XOpts, Debug) ->
    {I, R, Spec} = channel_spec(Cfg, XOpts),
    ?LOG(Debug, "channel_spec: ~p", [{I, R, Spec}]),
    Port = proplists:get_value(port, Cfg, 9325),
    UseAny = proplists:get_bool(use_any, Cfg),
    create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg).

fail_to_create_fsm(Reason, Cfg) ->
    Debug = get_debug(Cfg),
    {_I, _R, Spec} = channel_spec(Cfg, #{}),
    Port = proplists:get_value(port, Cfg, 9325),
    {error, Reason} = rpc(dev1, aesc_fsm, respond, [Port, Spec], Debug),
    ok.

channel_spec(Cfg) ->
    channel_spec(Cfg, #{}).

channel_spec(Cfg, XOpts) ->
    I = ?config(initiator, Cfg),
    R = ?config(responder, Cfg),

    %% dynamic key negotiation
    Proto = <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>,

    Spec0 = #{ initiator            => maps:get(pub, I)
             , responder            => maps:get(pub, R)
             , initiator_amount     => config(initiator_amount, Cfg, ?INITIATOR_AMOUNT)
             , responder_amount     => config(responder_amount, Cfg, ?RESPONDER_AMOUNT)
             , push_amount          => config(push_amount, Cfg, ?PUSH_AMOUNT)
             , lock_period          => 10
             , channel_reserve      => config(channel_reserve, Cfg, ?CHANNEL_RESERVE)
             , minimum_depth        => config(minimum_depth_factor, Cfg, ?MINIMUM_DEPTH_FACTOR)
             , client               => self()
             , noise                => [{noise, Proto}]
             , timeouts             => #{idle => 200000}
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
          Spec1,
          [nonce, block_hash_delta]),
    {I, R, Spec2}.

log(Fmt, Args, L, #{debug := true}) ->
    log(Fmt, Args, L, true);
log(Fmt, Args, L, true) ->
    ct:log("~p at ~p: " ++ Fmt, [self(), L | Args]);
log(_, _, _, _) ->
    ok.

config() ->
    Cfg = get(config),
    case Cfg of
        undefined ->
            [];
        _ ->
            Cfg
    end.

config(Cfg) when is_list(Cfg) -> put(config, Cfg).

config(K, Cfg, Def) when is_list(Cfg) ->
    case ?config(K, Cfg) of
        undefined -> Def;
        Other     -> Other
    end.

set_configs(New, Cfg) ->
    set_configs(New, Cfg, true).

set_configs(New, Cfg, Replace) when is_list(Cfg) ->
    lists:foldl(fun({K, V}, Acc) -> set_config(K, V, Acc, Replace) end, Cfg, New).

set_config(K, V, Cfg, Replace) when is_list(Cfg) ->
    case lists:keyfind(K, 1, Cfg) of
        false ->
            lists:keystore(K, 1, Cfg, {K, V});
        _ ->
            case (K =/= slogan) andalso Replace of
                true ->
                    lists:keystore(K, 1, Cfg, {K, V});
                false ->
                    Cfg
            end
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
           ?LOG(Debug,
                "==================================================~n"
                "~p: message Q: ~p~n"
                "==================================================~n",
                [L, Msgs])
    end,
    ok.

slogan(Cfg) ->
    ?config(slogan, Cfg).

%% @doc Find position of block including the given encoded tx hash in a list of key blocks.
%% Starting at position 1.
tx_position_in_blocks(TxHash, Blocks) ->
    tx_position_in_blocks_(TxHash, Blocks, 1).

tx_position_in_blocks_(_TxHash, [], _Pos) ->
    error;
tx_position_in_blocks_(TxHash, [Block | Rest], Pos) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    case rpc(dev1, aec_chain, get_generation_by_hash, [BlockHash, backward]) of
        error ->
            error;
        {ok, #{micro_blocks := MBs}} ->
            CheckTx = fun(Tx0) ->
                              TxHash0 = aetx_sign:hash(Tx0),
                              TxHash1 = aeser_api_encoder:encode(tx_hash, TxHash0),
                              TxHash1 == TxHash
                      end,
            Found = lists:any(fun(MB) ->
                                      lists:any(CheckTx, aec_blocks:txs(MB))
                              end, MBs),
            case Found of
                true ->
                    {ok, Pos};
                false ->
                    tx_position_in_blocks_(TxHash, Rest, Pos + 1)
            end
    end.

is_above_roma_protocol() ->
    aect_test_utils:latest_protocol_version() > ?ROMA_PROTOCOL_VSN.

responder_stays(#{responder_opts := #{keep_running := Bool}}) ->
    Bool;
responder_stays(_) ->
    false.

expected_fsm_logs(Name, Role) ->
    expected_fsm_logs(Name, Role, #{}).

expected_fsm_logs(leave_reestablish_loop_step_, initiator = R, #{initial_channel_open := true}) ->
    [ {evt, close}
    , {rcv, leave_ack}
    , {snd, leave}
    ] ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(leave_reestablish_loop_step_, initiator, #{initial_channel_open := false}) ->
    [ {evt, close}
    , {rcv, leave_ack}
    , {snd, leave}
    , {rcv, channel_reest_ack}
    , {snd, channel_reestablish}
    ];
expected_fsm_logs(leave_reestablish_loop_step_, responder = R, #{initial_channel_open := true}) ->
    [ {evt, close}
    , {rcv, leave}
    ] ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(leave_reestablish_loop_step_, responder, #{initial_channel_open := false}) ->
    [ {evt, close}
    , {rcv, leave}
    , {snd, channel_reest_ack}
    , {rcv, channel_reestablish}
    ];
expected_fsm_logs(t_create_channel_, initiator = R, _) ->
    expected_fsm_logs(channel_shutdown, R) ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(t_create_channel_, responder = R, _) ->
    expected_fsm_logs(channel_shutdown, R) ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(leave_reestablish_close, initiator = R, _) ->
    expected_fsm_logs(channel_shutdown, R) ++
    [ {rcv, update_ack}
    , {snd, update}
    , {rcv, signed}
    , {req, sign}
    , {rcv, channel_reest_ack}
    , {snd, channel_reestablish}
    ];
expected_fsm_logs(leave_reestablish_close, responder = R, _) ->
    expected_fsm_logs(channel_shutdown, R) ++
    [ {snd, update_ack}
    , {rcv, signed}
    , {req, sign}
    , {rcv, update}
    , {snd, channel_reest_ack}
    , {rcv, channel_reestablish}
    ];
expected_fsm_logs(check_incorrect_update, R, #{updator := U, malicious := M})
  when (R =:= initiator andalso U =:= R andalso M =:= R) orelse
       (R =:= responder andalso U =:= R andalso M =:= R) ->
    [ {rcv, update_error}
    , {snd, update}
    , {rcv, signed}
    , {req, sign}
    , {snd, undefined}
    , {req, sign}
    ] ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(check_incorrect_update, R, #{updator := U, malicious := M})
  when (R =:= initiator andalso U =/= R andalso M =/= R) orelse
       (R =:= responder andalso U =/= R andalso M =/= R) ->
    [ {evt, close}
    , {rcv, disconnect}
    , {snd, update_error}
    ] ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(check_incorrect_update, R, #{updator := U, malicious := M})
  when (R =:= initiator andalso U =:= R andalso M =/= R) orelse
       (R =:= responder andalso U =:= R andalso M =/= R) ->
    [ {snd, update_error}
    , {snd, update}
    , {rcv, signed}
    , {req, sign}
    ] ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(check_incorrect_update, R, #{updator := U, malicious := M})
  when (R =:= initiator andalso U =/= R andalso M =:= R) orelse
       (R =:= responder andalso U =/= R andalso M =:= R) ->
    [ {evt, close}
    , {rcv, disconnect}
    , {snd, update_ack}
    , {rcv, signed}
    , {req, sign}
    , {rcv, update}
    ] ++
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(chane_config_get_history, initiator = R, #{}) ->
    expected_fsm_logs(channel_open, R);
expected_fsm_logs(channel_open, initiator, #{}) ->
    [ {rcv, funding_locked}
    , {snd, funding_locked}
    , {rcv, channel_changed}
    , {rcv, funding_signed}
    , {snd, funding_created}
    , {rcv, signed}
    , {req, sign}
    , {rcv, channel_accept}
    , {snd, channel_open}
    ];
expected_fsm_logs(channel_open, responder, #{}) ->
    [ {rcv, funding_locked}
    , {snd, funding_locked}
    , {rcv, channel_changed}
    , {snd, funding_signed}
    , {rcv, signed}
    , {req, sign}
    , {rcv, funding_created}
    , {snd, channel_accept}
    , {rcv, channel_open}
    ];
expected_fsm_logs(channel_shutdown, initiator, #{}) ->
    [ {evt, close}
    , {optional, rcv, disconnect} % this can occur in case the min depth achieved is delayed
    , {rcv, channel_closed}
    , {rcv, shutdown_ack}
    , {snd, shutdown}
    , {rcv, signed}
    , {req, sign}
    ];
expected_fsm_logs(channel_shutdown, responder, #{}) ->
    [ {evt, close}
    , {optional, rcv, disconnect} % this can occur in case the min depth achieved is delayed
    , {rcv, channel_closed}
    , {snd, shutdown_ack}
    , {rcv, signed}
    , {req, sign}
    , {rcv, shutdown}
    ];
expected_fsm_logs(_, _, _) -> [].

deposit_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    Amount = 10,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, Round0 = 1, _} = check_fsm_state(FsmI),
    check_info(0),
    ?LOG(Debug, "Round0 = ~p, IAmt0 = ~p, RAmt0 = ~p", [Round0, IAmt0, RAmt0]),

    % Perform update and verify flow and reports
    ok = rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => Amount}]),
    {I1, _} = await_signing_request(deposit_tx, I, Cfg),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {R1, _} = await_signing_request(deposit_created, R, Cfg),
    _SignedTx = await_on_chain_report(R1, #{info => deposit_created},  ?TIMEOUT),
    receive_info(R1, deposit_created, Debug), % probably an obsolete message?
    {ok, _} = receive_from_fsm(conflict, I1, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R1, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),
    % Verify changes of fsm state
    
    % Done
    assert_empty_msgq(true),
    check_info(20),
    shutdown_(I1, R1, Cfg),
    ok.

withdraw_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    Amount = 10,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, Round0 = 1, _} = check_fsm_state(FsmI),
    check_info(0),
    ?LOG(Debug, "Round0 = ~p, IAmt0 = ~p, RAmt0 = ~p", [Round0, IAmt0, RAmt0]),

    % Perform update and verify flow and reports
    ok = rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, #{amount => Amount}]),
    {I1, _} = await_signing_request(withdraw_tx, I, Cfg),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {R1, _} = await_signing_request(withdraw_created, R, Cfg),
    _SignedTx = await_on_chain_report(R1, #{info => withdraw_created},  ?TIMEOUT),
    receive_info(R1, withdraw_created, Debug), % probably an obsolete message?
    {ok, _} = receive_from_fsm(conflict, I1, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R1, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),
    % Verify changes of fsm state
    
    % Done
    assert_empty_msgq(true),
    check_info(20),
    shutdown_(I1, R1, Cfg),
    ok.

close_mutual_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),

    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI, #{}]),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {I1, _SignedShutdownTx} = await_signing_request(shutdown, I, Cfg),
    {R1, _CoSignedShutdownTx} = await_signing_request(shutdown_ack, R, Cfg),
    _CoSignedShutdownTx = await_on_chain_report(R1, ?TIMEOUT), % same tx
    {ok,_} = receive_from_fsm(info, R1, #{info => shutdown}, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R1, fun closing/1, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, I1, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(conflict, R1, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),
    R2 = await_open_report(R1, ?TIMEOUT, Debug),

    % Done
    assert_empty_msgq(true),
    check_info(20),
    shutdown_(I1, R2, Cfg),
    ok.

close_solo_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),

    ok = rpc(dev1, aesc_fsm, close_solo, [FsmI, #{}]),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {I1, _SignedCloseSoloTx} = await_signing_request(close_solo_tx, I, Cfg),
    {ok, _} = receive_from_fsm(conflict, I1, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),

    % Done
    assert_empty_msgq(true),
    check_info(20),
    shutdown_(I1, R, Cfg),
    ok.

snapshot_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_(
                                            [?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),

    %% snapshot solo provides off-chain state on-chain. Since it makes no sense
    %% if the latest off-chain state is already on-chain, we make an off-chain
    %% update to progress the channel round one step further.
    {I1, R1} = do_update(PubI, PubR, 2, I, R, Debug, Cfg),
    ok = rpc(dev1, aesc_fsm, snapshot_solo, [FsmI, #{}]),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {I2, _SignedCloseSoloTx} = await_signing_request(snapshot_solo_tx, I1, Cfg),
    %% because we made an off-chain update, the latest stable state is the one
    %% produced by the off-chain transfer, hence the round is 2
    {ok, _} = receive_from_fsm(conflict, I2, #{info => #{ error_code => 5
                                                        , round => 2}},
                               ?TIMEOUT, Debug),

    % Done
    assert_empty_msgq(true),
    check_info(20),
    shutdown_(I2, R1, Cfg),
    ok.

slash(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := Spec } = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),

    {I3, R2, _SlashTx} = prepare_for_slashing(I, R, Spec, Cfg),

    LockPeriod = maps:get(lock_period, Spec),
    ok = rpc(dev1, aesc_fsm, slash, [FsmI, #{}]),
    {_, SignedSlashTx} = await_signing_request(slash_tx, I3, Cfg),
    SlashTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSlashTx)),
    mine_blocks_until_txs_on_chain(dev1, [SlashTxHash]),
    mine_blocks(dev1, LockPeriod),
    check_info(20),
    settle_(maps:get(minimum_depth, Spec), I3, R2, Debug, Cfg),
    check_info(20),
    assert_empty_msgq(true),
    ok.

slash_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := Spec } = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),

    {I2, R2, _SlashTx} = prepare_for_slashing(I, R, Spec, Cfg),

    ok = rpc(dev1, aesc_fsm, slash, [FsmI, #{}]),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {I3, _SignedCloseSoloTx} = await_signing_request(slash_tx, I2, Cfg),
    %% because we made an off-chain update, the latest stable state is the one
    %% produced by the off-chain transfer, hence the round is 3
    {ok, _} = receive_from_fsm(conflict, I3, #{info => #{ error_code => 5
                                                        , round => 3}},
                               ?TIMEOUT, Debug),
    
    % Done. Check if we can still make a valid slash and then settle the
    % channel
    ok = rpc(dev1, aesc_fsm, slash, [FsmI, #{}]),
    {_, SignedSlashTx} = await_signing_request(slash_tx, I3, Cfg),
    SlashTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSlashTx)),
    mine_blocks_until_txs_on_chain(dev1, [SlashTxHash]),
    LockPeriod = maps:get(lock_period, Spec),
    mine_blocks(dev1, LockPeriod),
    check_info(20),
    settle_(maps:get(minimum_depth, Spec), I3, R2, Debug, Cfg),
    check_info(20),
    assert_empty_msgq(true),
    ok.

prepare_for_slashing(#{fsm := FsmI} = I, R, Spec, Cfg) ->
    Debug = get_debug(Cfg),
     #{ initiator := PubI
      , responder := PubR } = Spec,
    %% we don't want the close solo to be based on the last on-chain state, so
    %% we make an off-chain transfer
    {I1, R1} = do_update(PubI, PubR, 2, I, R, Debug, Cfg),

    %% make a close solo but don't post it yet
    {ok, State} = rpc(dev1, aesc_fsm, get_offchain_state, [FsmI]),
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    {ok, Tx} = close_solo_tx(I1, aetx_sign:serialize_to_binary(SignedTx)),
    {SignedCloseSoloTx, I2} = sign_tx(I1, Tx, Cfg),

    %% make another update so the close solo we have will not be based on the
    %% latest state
    {I3, R2} = do_update(PubI, PubR, 2, I2, R1, Debug, Cfg),

    %% make the close solo on-chain
    ok = rpc(dev1, aec_tx_pool, push, [SignedCloseSoloTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedCloseSoloTx)),
    mine_blocks_until_txs_on_chain(dev1, [TxHash]),
    LockPeriod = maps:get(lock_period, Spec),
    UnlockHeight = current_height(dev1) + LockPeriod,
    ?LOG(Debug, "Expected height at which settle is allowed = ~p",
         [UnlockHeight]),

    %% both participants detect the close solo
    SlashTx = await_on_chain_report(I3, #{info => can_slash}, ?TIMEOUT),
    SlashTx = await_on_chain_report(R2, #{info => can_slash}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I3, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R2, fun closing/1, ?TIMEOUT, Debug),
    {I3, R2, SlashTx}.

settle_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR } = Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),

    ok = rpc(dev1, aesc_fsm, close_solo, [FsmI, #{}]),
    {I1, SignedCloseSoloTx} = await_signing_request(close_solo_tx, I, Cfg),
    wait_for_signed_transaction_in_block(dev1, SignedCloseSoloTx),

    LockPeriod = maps:get(lock_period, Spec),
    mine_blocks(dev1, LockPeriod),
    SignedTx = await_on_chain_report(I, #{info => solo_closing}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, #{info => solo_closing}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun closing/1, ?TIMEOUT, Debug),

    check_info(20),
    ok = rpc(dev1, aesc_fsm, settle, [FsmI, #{}]),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {I2, _SignedCloseSoloTx} = await_signing_request(settle_tx, I1, Cfg),
    %% because we made an off-chain update, the latest stable state is the one
    %% produced by the off-chain transfer, hence the round is 2
    {ok, _} = receive_from_fsm(conflict, I2, #{info => #{ error_code => 5
                                                        , round => 1}},
                               ?TIMEOUT, Debug),
    % Done
    settle_(maps:get(minimum_depth, Spec), I2, R, Debug, Cfg),
    check_info(20),
    assert_empty_msgq(true),
    ok.

force_progress_triggers_slash(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),

    {ok, State} = rpc(dev1, aesc_fsm, get_offchain_state, [FsmI]),
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    {ok, Tx} = close_solo_tx(I1, aetx_sign:serialize_to_binary(SignedTx)),
    {SignedCloseSoloTx, I3} = sign_tx(I2, Tx, Cfg),

    %% create a force progress but abort it; still keep the not yet signed
    %% transaction
    trigger_force_progress(ContractPubkey, counter, "tick", [], 10, R2),
    ErrorCode = 123,
    {ok, UnSignedFP0} = abort_signing_request(force_progress_tx, R2, ErrorCode, ?TIMEOUT, Debug),
    %% this force progress is later on to be based on the close tx we've
    %% prepared. Since the FP is to rely on on-chain data, it must have an
    %% empty payload
    AetxFP0 = aetx_sign:tx(UnSignedFP0),
    {aesc_force_progress_tx, FP0} = aetx:specialize_callback(AetxFP0),
    FP = aesc_force_progress_tx:set_payload(FP0, <<>>),
    AetxFP = aetx:new(aesc_force_progress_tx, FP),
    

    {ok, _} = receive_from_fsm(info, R2, fun aborted_update/1, ?TIMEOUT, Debug),

    %% make an off-chain update
    {I4, R3} = update_volley(I3, R2, Cfg),
    %% at this point, the FP has a lower round than the last on-chain

    %% make the close solo on-chain
    ok = rpc(dev1, aec_tx_pool, push, [SignedCloseSoloTx]),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedCloseSoloTx)),
    mine_blocks_until_txs_on_chain(dev1, [TxHash]),
    LockPeriod = maps:get(lock_period, Spec),
    UnlockHeight = current_height(dev1) + LockPeriod,
    ?LOG(Debug, "Expected height at which settle is allowed = ~p",
         [UnlockHeight]),

    %% both participants detect the close solo
    SlashTx = await_on_chain_report(I4, #{info => can_slash}, ?TIMEOUT),
    SlashTx = await_on_chain_report(R3, #{info => can_slash}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I4, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R3, fun closing/1, ?TIMEOUT, Debug),

    {SignedFP, R4} = sign_tx(R3, AetxFP, Cfg),
    ok = rpc(dev1, aec_tx_pool, push, [SignedFP]),
    wait_for_signed_transaction_in_block(dev1, SignedFP, Debug),

    SlashTx = await_on_chain_report(I4, #{info => can_slash}, ?TIMEOUT),
    SlashTx = await_on_chain_report(R4, #{info => can_slash}, ?TIMEOUT),
    check_info(20),
    ok = rpc(dev1, aesc_fsm, slash, [FsmI, #{}]),
    {_, SignedSlashTx} = await_signing_request(slash_tx, I4, Cfg),
    SlashTxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSlashTx)),
    mine_blocks_until_txs_on_chain(dev1, [SlashTxHash]),
    mine_blocks(dev1, LockPeriod),
    check_info(20),
    settle_(maps:get(minimum_depth, Spec), I4, R3, Debug, Cfg),
    check_info(20),
    assert_empty_msgq(true),
    ok.

force_progress_triggers_snapshot(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := _FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),

    %% create a force progress but abort it; still keep the not yet signed
    %% transaction
    trigger_force_progress(ContractPubkey, counter, "tick", [], 10, I2),
    ErrorCode = 123,
    {ok, UnSignedFP} = abort_signing_request(force_progress_tx, I2, ErrorCode, ?TIMEOUT, Debug),
    {ok, _} = receive_from_fsm(info, I1, fun aborted_update/1, ?TIMEOUT, Debug),

    %% make an off-chain update
    {I3, R3} = update_volley(I2, R2, Cfg),
    %% at this point, the FP has a lower round than the last on-chain
    {SignedFP, I4} = co_sign_tx(I3, UnSignedFP, Cfg),
    ok = rpc(dev1, aec_tx_pool, push, [SignedFP]),
    wait_for_signed_transaction_in_block(dev1, SignedFP, Debug),

    SlashTx = await_on_chain_report(I4, #{info => can_snapshot}, ?TIMEOUT),
    SlashTx = await_on_chain_report(R3, #{info => can_snapshot}, ?TIMEOUT),
    {ok, I5, R4} = snapshot_solo_(I4, R3, #{}, Cfg),
    check_info(20),
    shutdown_(I5, R4, Cfg),
    ok.

force_progress_followed_by_update(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := _FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    {I3, R3} = force_progress_(ContractPubkey, counter, "tick", [], 10, I2, R2, Cfg),
    {I4, R4} = update_volley(I3, R3, Cfg),

    check_info(20),
    shutdown_(I4, R4, Cfg),
    ok.

force_progress_based_on_offchain_state(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := _FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    {I3, R3} = force_progress_(ContractPubkey, counter, "tick", [], 10, I2, R2, Cfg),
    {I4, R4} = update_volley(I3, R3, Cfg),

    check_info(20),
    shutdown_(I4, R4, Cfg),
    ok.


force_progress_based_on_snapshot(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := _FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    {ok, I3, R3} = snapshot_solo_(I2, R2, #{}, Cfg),
    {I4, R4} = force_progress_(ContractPubkey, counter, "tick", [], 10, I3,
                               R3, Cfg),

    check_info(20),
    shutdown_(I4, R4, Cfg),
    ok.

force_progress_based_on_deposit(Cfg) ->
    Debug = get_debug(Cfg),
    check_info(20),
    #{ i := #{ fsm := _FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    {ok, I3, R3} = deposit_(I2, R2, _Amount = 123, Debug, Cfg),
    {I4, R4} = force_progress_(ContractPubkey, counter, "tick", [], 10, I3,
                               R3, Cfg),

    check_info(20),
    shutdown_(I4, R4, Cfg),
    ok.

force_progress_based_on_withdrawal(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := _FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    {ok, I3, R3} = withdraw_(I2, R2, _Amount = 123, Debug, Cfg),
    {I4, R4} = force_progress_(ContractPubkey, counter, "tick", [], 10, I3,
                               R3, Cfg),

    check_info(20),
    shutdown_(I4, R4, Cfg),
    ok.

force_progress_on_force_progress(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := _FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    {ok, I3, R3} = withdraw_(I2, R2, _Amount = 123, Debug, Cfg),
    {I4, R4} = force_progress_(ContractPubkey, counter, "tick", [], 10, I3,
                               R3, Cfg),

    {I5, R5} = force_progress_(ContractPubkey, counter, "tick", [], 10, I4,
                               R4, Cfg),

    check_info(20),
    shutdown_(I5, R5, Cfg),
    ok.

force_progress_with_failed_onchain(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := FsmI
             , channel_id := ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := _PubR } = _Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    {ok, I3, R3} = deposit_(I2, R2, _Amount = 123, Debug, Cfg),
    {ok, Round} = rpc(dev1, aesc_fsm, get_round, [FsmI]),

    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), counter),
    {ok, CallData} =
        aect_test_utils:encode_call_data(aect_test_utils:sophia_version(), BinSrc,
                                         "tick", []),
    FPArgs = #{ contract    => ContractPubkey
              , abi_version => aect_test_utils:abi_version() 
              , amount      => 10
              , call_data   => CallData
              , gas         => 1000000
              , gas_price    => aec_test_utils:min_gas_price() },
    ok = rpc(dev1, aesc_fsm, force_progress, [FsmI, FPArgs]),
    basic_spend(initiator, % from
                initiator, % to
                1,         % some amount 
                Cfg),
    {I4, _} = await_signing_request(force_progress_tx, I3, Cfg),
    {ok, _} = receive_from_fsm(conflict, I4, #{info => #{ error_code => 5
                                                        , round => Round}},
                               ?TIMEOUT, Debug),

    check_info(20),
    shutdown_(I4, R3, Cfg),
    ok.

force_progress_closing_state(Cfg) ->
    Debug = get_debug(Cfg),
    #{ i := #{ fsm := FsmI
             , channel_id := _ChannelId } = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR } = Spec} = create_channel_([?SLOGAN|Cfg]),
    ?LOG(Debug, "I = ~p", [I]),
    ?LOG(Debug, "R = ~p", [R]),
    {I1, R1, ContractPubkey} = create_contract(counter, ["42"], 10, I, R, Cfg),
    {I2, R2} = call_contract(ContractPubkey, counter, "tick", [], 10, I1, R1, Cfg),
    ok = rpc(dev1, aesc_fsm, close_solo, [FsmI, #{}]),
    {I3, SignedCloseSoloTx} = await_signing_request(close_solo_tx, I2, Cfg),
    wait_for_signed_transaction_in_block(dev1, SignedCloseSoloTx),

    LockPeriod = maps:get(lock_period, Spec),
    mine_blocks(dev1, LockPeriod),
    SignedTx = await_on_chain_report(I1, #{info => solo_closing}, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, #{info => solo_closing}, ?TIMEOUT),
    {ok,_} = receive_from_fsm(info, I1, fun closing/1, ?TIMEOUT, Debug),
    {ok,_} = receive_from_fsm(info, R, fun closing/1, ?TIMEOUT, Debug),
    {_I4, _R3} = force_progress_(ContractPubkey, counter, "tick", [], 10, I3,
                                 R2, Cfg),
    {_, _, _, _, closing} = check_fsm_state(FsmI),

    check_info(20),
    ok.


create_contract(ContractName, InitArgs, Deposit,
                #{ fsm := FsmC
                 , pub := Owner} = Creator, Acknowledger, Cfg) ->
    Debug = get_debug(Cfg),
    {ok, Round0} = rpc(dev1, aesc_fsm, get_round, [FsmC]),
    {ok, BinCode} = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), ContractName),
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), ContractName),
    {ok, CallData} =
        aect_test_utils:encode_call_data(aect_test_utils:sophia_version(), BinSrc,
                                         "init", InitArgs),
    CreateArgs = #{ vm_version  => aect_test_utils:vm_version()
                  , abi_version => aect_test_utils:abi_version() 
                  , deposit     => Deposit
                  , code        => BinCode
                  , call_data   => CallData},
    aesc_fsm:upd_create_contract(FsmC, CreateArgs),
    {Creator1, _} = await_signing_request(update, Creator, Cfg),
    await_update_incoming_report(Acknowledger, ?TIMEOUT, Debug),
    {Acknowledger1, _} = await_signing_request(update_ack, Acknowledger, Cfg),
    Creator2 = await_update_report(Creator1, ?TIMEOUT, Debug),
    Acknowledger2 = await_update_report(Acknowledger1, ?TIMEOUT, Debug),

    ContractPubkey = aect_contracts:compute_contract_pubkey(Owner, Round0 + 1),
    assert_empty_msgq(Debug),
    {Creator2, Acknowledger2, ContractPubkey}.

call_contract(ContractId,
              ContractName, FunName, FunArgs, Amount,
              #{fsm := FsmC} = Caller, Acknowledger, Cfg) ->
    Debug = get_debug(Cfg),
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), ContractName),
    {ok, CallData} =
        aect_test_utils:encode_call_data(aect_test_utils:sophia_version(), BinSrc,
                                         FunName, FunArgs),
    CallArgs = #{ contract    => ContractId
                , abi_version => aect_test_utils:abi_version() 
                , amount      => Amount
                , call_data   => CallData},
    aesc_fsm:upd_call_contract(FsmC, CallArgs),
    {Caller1, _} = await_signing_request(update, Caller, Cfg),
    await_update_incoming_report(Acknowledger, ?TIMEOUT, Debug),
    {Acknowledger1, _} = await_signing_request(update_ack, Acknowledger, Cfg),
    Caller2 = await_update_report(Caller1, ?TIMEOUT, Debug),
    Acknowledger2 = await_update_report(Acknowledger1, ?TIMEOUT, Debug),
    assert_empty_msgq(Debug),
    {Caller2, Acknowledger2}.

trigger_force_progress(ContractPubkey,
                       ContractName, FunName, FunArgs, Amount,
                       #{ fsm := FsmC}) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), ContractName),
    {ok, CallData} =
        aect_test_utils:encode_call_data(aect_test_utils:sophia_version(), BinSrc,
                                         FunName, FunArgs),
    FPArgs = #{ contract    => ContractPubkey
              , abi_version => aect_test_utils:abi_version() 
              , amount      => Amount
              , call_data   => CallData
              , gas         => 1000000
              , gas_price   => aec_test_utils:min_gas_price() },
    aesc_fsm:force_progress(FsmC, FPArgs).


force_progress_(ContractPubkey,
                ContractName, FunName, FunArgs, Amount,
                #{ fsm := FsmC
                , channel_id:= ChannelId
                , pub := CallerPubkey} = Caller,
                #{ pub := OtherPubkey
                , fsm := FsmO } = Other, Cfg) ->
    Debug = get_debug(Cfg),
    %% check both FSMs share same view of balances
    Pubkeys = [CallerPubkey, OtherPubkey, ContractPubkey],
    [CallerBal0, OtherBal0, ContractBal0] = get_balances(FsmC, Pubkeys),
    [CallerBal0, OtherBal0, ContractBal0] = get_balances(FsmO, Pubkeys),
        get_balances(FsmC, [CallerPubkey, OtherPubkey, ContractPubkey]),
    {ok, Round0} = aesc_fsm:get_round(FsmC),
    {ok, Round0} = aesc_fsm:get_round(FsmO),
    trigger_force_progress(ContractPubkey,
                ContractName, FunName, FunArgs, Amount, Caller),
    {Caller1, SignedTx} = await_signing_request(force_progress_tx, Caller, Cfg),
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {aesc_force_progress_tx, Tx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    StateHash = aesc_force_progress_tx:state_hash(Tx),
    Round = aesc_force_progress_tx:round(Tx),
    {ok, Channel} = rpc(dev1, aec_chain, get_channel, [ChannelId]),
    %% assert channel had been changed on-chain
    {Round, Round} = {Round, aesc_channels:round(Channel)},
    {StateHash, StateHash} = {StateHash, aesc_channels:state_hash(Channel)},
    SignedTx = await_on_chain_report(Caller, #{info => consumed_forced_progress}, ?TIMEOUT),
    SignedTx = await_on_chain_report(Other, #{info => consumed_forced_progress}, ?TIMEOUT),
    %% check both FSMs still share same view of balances
    [CallerBal1, OtherBal1, ContractBal1] = get_balances(FsmC, Pubkeys),
    [CallerBal1, OtherBal1, ContractBal1] = get_balances(FsmO, Pubkeys),

    %% check that balances are as expected
    %% NB: this relies on the contract called not spending tokens to any of
    %% the participant's balance and only consuming the Amount

    %% Other's balance had not changed
    {OtherBal0, OtherBal0} = {OtherBal0, OtherBal1},
    %% Caller had spend Amount to the contract
    {CallerBal0, CallerBal0} = {CallerBal0, CallerBal1 + Amount},
    {ContractBal0, ContractBal0} = {ContractBal0, ContractBal1 - Amount},

    {ok, Round1} = aesc_fsm:get_round(FsmC),
    {ok, Round1} = aesc_fsm:get_round(FsmO),
    {Round0, Round0} = {Round0, Round1 - 1},
    assert_empty_msgq(Debug),

    {Caller1, Other}.

produce_close_mutual_tx(ChannelId, FromId, I, R, Cfg, Opts) ->
    Debug = get_debug(Cfg),
    Nonce =
        case account_type(FromId) of
            basic ->
                {ok, N} = rpc(dev1, aec_next_nonce, pick_for_account, [FromId]),
                N;
            generalized ->
                0
        end,
    CloseMutualTxOpts =
        #{ channel_id => aeser_id:create(channel, ChannelId)
         , from_id => aeser_id:create(account, FromId)
         , initiator_amount_final => 0
         , responder_amount_final => 0
         , fee => 20000 * aec_test_utils:min_gas_price()
         , nonce => Nonce
        },
    {ok, Aetx} = aesc_close_mutual_tx:new(maps:merge(Opts, CloseMutualTxOpts)),
    {Tx1, I1} = sign_tx(I, Aetx, Cfg),
    {SignedTx, R1} = co_sign_tx(R, Tx1, Cfg),
    ok = rpc(dev1, aec_tx_pool, push, [SignedTx]),
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {I1, R1}.

snapshot_solo_(#{fsm := FsmI} = I, R, Opts, Cfg) ->
    Debug = get_debug(Cfg),
    assert_empty_msgq(Debug),
    % Check initial fsm state
    {IAmt0, RAmt0, _, Round0, _} = FsmState0 = check_fsm_state(FsmI),
    ?LOG(Debug, "Round0 = ~p, IAmt0 = ~p, RAmt0 = ~p", [Round0, IAmt0, RAmt0]),

    % Perform update and verify flow and reports
    ok = rpc(dev1, aesc_fsm, snapshot_solo, [FsmI, Opts]),
    {I1, SignedTx} = await_signing_request(snapshot_solo_tx, I, Cfg),
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    SignedTx = await_channel_changed_report(I1, ?TIMEOUT), % same tx
    SignedTx = await_channel_changed_report(R, ?TIMEOUT), % same tx
    ?LOG(Debug, "=== SignedTx = ~p", [SignedTx]),
    assert_empty_msgq(Debug),

    %% Channel off-chain didn't change
    {IAmt0, RAmt0, _, Round0, _} = FsmState0 = check_fsm_state(FsmI),
    ?LOG(Debug, "Round0 = ~p, IAmt0 = ~p, RAmt0 = ~p", [Round0, IAmt0, RAmt0]),
    % Done
    mine_blocks(dev1),
    await_min_depth_reached(I, aetx_sign:hash(SignedTx), channel_snapshot_solo_tx, ?TIMEOUT),
    assert_empty_msgq(Debug),
    {ok, I1, R}.
