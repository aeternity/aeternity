%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Test utils for State Channels
%%% @end
%%%=============================================================================

-module(aesc_bench_SUITE).

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
          upd_transfer/1
        , multiple_channels/1
        , many_chs_msg_loop/1
        , many_chs_contract_calls/1
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
     {all_tests, [sequence], [ {group, throughput}
                             ]},
     {throughput, [sequence],
      [
        upd_transfer
      , multiple_channels
      , many_chs_msg_loop
      , many_chs_contract_calls
      ]}
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
    Config1 = aecore_suite_utils:init_per_suite(
                [dev1], DefCfg, [ {instant_mining, true}
                                , {symlink_name, "latest.aesc_fsm"}
                                , {test_module, ?MODULE}] ++ Config),
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

init_per_group(throughput, Config0) ->
    Config = init_per_group_(Config0),
    set_configs([ {minimum_depth, 2}
                , {minimum_depth_factor, 0}
                , {minimum_depth_channel, 2}
                ], Config);
init_per_group(_Group, Config) ->
    init_per_group_(Config).

init_per_group_(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
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
    end.

end_per_group(_Group, Config) ->
    _Config1 = stop_node(dev1, Config),
    ok.

init_per_testcase(_, Config) ->
    Debug = (os:getenv("CT_DEBUG") == "1"),
    Config1 = case is_above_roma_protocol() of
                  true ->
                      set_configs([ {minimum_depth, ?MINIMUM_DEPTH}
                                  , {minimum_depth_factor, ?MINIMUM_DEPTH_FACTOR}
                                  , {minimum_depth_strategy, ?MINIMUM_DEPTH_STRATEGY}
                                  ], Config, false);
                  false ->
                      % Because the tx fees used to be lower in roma, more
                      % blocks are required for tx to be confirmed.
                      set_configs([ {minimum_depth, ?MINIMUM_DEPTH}
                                  , {minimum_depth_factor, ?MINIMUM_DEPTH_FACTOR * 2}
                                  , {minimum_depth_strategy, ?MINIMUM_DEPTH_STRATEGY}
                                  ], Config, false)
              end,
    set_configs([{debug, Debug}], Config1).

end_per_testcase(T, _Config) when T == multiple_channels;
                                  T == many_chs_msg_loop;
                                  T == many_chs_contract_calls ->
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
        {'$call', From, Req} ->
            St1 = miner_handle_call(From, Req, St),
            miner_loop(St1);
        {Parent, done} ->
            ct:pal("miner helper done", []),
            if St#miner.mining ->
                    stop_mining();
               true ->
                    ok
            end,
            exit(normal);
        {gproc_ps_event, top_changed, #{ info := #{ height := NewH } }} ->
            ct:pal("top_changed; new height = ~p", [NewH]),
            St1 = check_if_reqs_fulfilled(NewH, St),
            miner_loop(St1);
        Other ->
            ct:pal("miner loop got Other = ~p", [Other]),
            miner_loop(St)
    end.

miner_handle_call(From, report_status, St) ->
    ?LOG("Miner loop state: ~p", [St]),
    miner_reply(From, ok),
    St;
miner_handle_call(From, {mine_until_txs_on_chain, Txs}, St) ->
    case txs_not_already_on_chain(Txs) of
        [] ->
            ct:pal("Txs already on-chain: ~p", [Txs]),
            miner_reply(From, txs_on_chain),
            St;
        [_|_] = TxsLeft ->
            add_mining_req({From, {txs, TxsLeft}}, St)
    end;
miner_handle_call(From, {mine_blocks, N, #{current_height := CurH}}, St) ->
    WantedHeight = CurH + N,
    if St#miner.height >= WantedHeight ->
            miner_reply(From, blocks_mined),
            St;
       true ->
            add_mining_req({From, {height, WantedHeight}}, St)
    end.

call_miner(Pid, Req, Timeout) ->
    MRef = monitor(process, Pid),
    Pid ! {'$call', {self(), MRef}, Req},
    receive
        {MRef, Res} ->
            demonitor(MRef),
            Res;
        {'DOWN', MRef, _, _, Reason} ->
            error(Reason)
    after Timeout ->
            ?LOG("Timeout in call to miner_loop: ~p", [Req]),
            report_miner_loop_status(Pid),
            demonitor(MRef),
            error(timeout)
    end.

miner_reply({From, Ref}, Reply) ->
    From ! {Ref, Reply}.

report_miner_loop_status(Pid) ->
    call_miner(Pid, report_status, ?TIMEOUT).

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
                              miner_reply(From, blocks_mined),
                              [];
                         true ->
                              [Req]
                      end;
                 ({From, {txs, Txs}}) ->
                      case txs_not_already_on_chain(Txs) of
                          [] ->
                              miner_reply(From, txs_on_chain),
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

call_volley(I, R, Contract, Cfg) ->
    {I1 ,  R1} = call_contract(Contract, counter, "tick", [], 10, I, R, Cfg),
    {_I2, _R2} = call_contract(Contract, counter, "tick", [], 10, I1, R1, Cfg).


get_both_balances(Fsm, PubI, PubR) ->
    {ok, [{PubI, BalI},
          {PubR, BalR}]} = rpc(dev1, aesc_fsm, get_balances, [Fsm, [PubI, PubR]]),
    {BalI, BalR}.

fsm_up(#{info := {fsm_up, FsmId}}) when is_function(FsmId) -> ok.

multiple_channels(Cfg) ->
    multiple_channels_t(10, 9360, {transfer, 20}, ?SLOGAN, Cfg).

many_chs_msg_loop(Cfg) ->
    multiple_channels_t(10, 9400, {msgs, 100}, ?SLOGAN, Cfg).

many_chs_contract_calls(Cfg) ->
    multiple_channels_t(10, 9440, [ add_contract
                                  , {calls, 10}], ?SLOGAN, Cfg).

multiple_channels_t(NumCs, FromPort, Msg, {slogan, Slogan}, Cfg) ->
    Debug = get_debug(Cfg),
    F = fun(Cs, _MinerHelper) ->
                {Fmt, Args} = run_scenario(Msg, Cs),
                ?LOG(Debug, Fmt, Args),
                ct:comment(Fmt, Args)
        end,
    spawn_multiple_channels(F, NumCs, FromPort, Slogan, Cfg).

run_scenario(Msgs, Cs) when is_list(Msgs) ->
    lists:foldl(fun(M, _Acc) ->
                        run_scenario_(M, Cs)
                end, {"no action", []}, Msgs);
run_scenario(Msg, Cs) ->
    run_scenario_(Msg, Cs).

run_scenario_(Msg, Cs) ->
    NumCs = length(Cs),
    [P ! Msg || P <- Cs],
    T0 = erlang:system_time(millisecond),
    Cs = collect_acks(Cs, loop_ack, NumCs),
    T1 = erlang:system_time(millisecond),
    Time = T1 - T0,
    N = loop_n(Msg),
    Transfers = NumCs*2*N,
    Fmt = "Time (~w*2*~w) ~.1f s: ~.1f mspt; ~.1f tps",
    Args = [NumCs, N, Time/1000, Time/Transfers, (Transfers*1000)/Time],
    {Fmt, Args}.

loop_n({transfer, N}) ->
    N;
loop_n(add_contract) ->
    1;
loop_n({calls, N}) ->
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
    ch_loop(I1, R1, Parent, set_debug(false, Cfg), #{}).

ch_loop(I, R, Parent, Cfg, St) ->
    receive
        {transfer, N} ->
            ?LOG("Got {transfer, ~p}", [N]),
            {_, I1, R1} = do_n(N, fun update_volley/3, I, R, Cfg),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg, St);
        {msgs, N} = _M ->
            {_, I1, R1} = do_n(N, fun msg_volley/3, I, R, Cfg),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg, St);
        add_contract ->
            {I1, R1, ContractPubkey} =
                create_contract(counter, ["42"], 10, I, R, Cfg),
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg, St#{contract => ContractPubkey});
        {calls, N} ->
            #{contract := Contract} = St,
            {I1, R1} =
            try
            {_, I1_, R1_} = do_n(N, fun(Ix, Rx, Cx) ->
                                          call_volley(Ix, Rx, Contract, Cx)
                                  end, I, R, Cfg),
            {I1_, R1_}
            catch
                error:R ->
                    ?LOG("CAUGHT ~p / ~p", [R, erlang:get_stacktrace()]),
                    {I, R}
            end,
            Parent ! {self(), loop_ack},
            ch_loop(I1, R1, Parent, Cfg, St);
        die ->
            ?LOG("~p got die request", [self()]),
            #{ proxy := ProxyI } = I,
            #{ proxy := ProxyR } = R,
            ProxyI ! {self(), die},
            ProxyR ! {self(), die},
            exit(normal);
        Other ->
            ?LOG(get_debug(Cfg), "Got Other = ~p, I = ~p~nR = ~p", [Other, I, R]),
            ch_loop(I, R, Parent, Cfg, St)
    end.

create_channel_from_spec(I, R, Spec, Port, UseAny, Debug, Cfg) ->
    assert_empty_msgq(Debug),
    %% TODO: Somehow there is a CI only race condition which rarely occurs in
    %% round_too_high.check_incorrect_* and round_too_low.check_incorrect_* tests
    %% For now just wrap this operation in a retry loop and come back to it later
    ?LOG("=== Create channel~n", []),
    ?LOG("Spec = ~p", [Spec]),
    RSpec = customize_spec(responder, Spec, Cfg),
    ISpec = customize_spec(initiator, Spec, Cfg),
    ?LOG(Debug, "RSpec = ~p~nISpec = ~p", [RSpec, ISpec]),
    SpawnInterval = proplists:get_value(spawn_interval, Cfg, 100),
    SpawnR = fun() ->
                        spawn_responder(Port, RSpec, R, UseAny, Debug)
                end,
    SpawnI = fun() ->
                     spawn_initiator(Port, ISpec, I, Debug)
             end,
    {RProxy, IProxy} =
        case proplists:get_value(spawn_first, Cfg, responder) of
            responder ->
                RPx = SpawnR(),
                timer:sleep(SpawnInterval),
                IPx = SpawnI(),
                {RPx, IPx};
            initiator ->
                IPx = SpawnI(),
                timer:sleep(SpawnInterval),
                RPx = SpawnR(),
                {RPx, IPx}
        end,
    ?LOG("RProxy = ~p, IProxy = ~p", [RProxy, IProxy]),
    Timeout = proplists:get_value(timeout, Cfg, ?TIMEOUT),
    Info = match_responder_and_initiator(RProxy, IProxy, Debug, Timeout),
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
    maps:merge(Spec, custom_spec_opts(Role, Cfg, Spec)).

custom_spec_opts(responder, Cfg, Spec) ->
    custom_log_keep(
      responder,
      Cfg,
      maybe_apply_f(proplists:get_value(responder_opts, Cfg, #{}), Spec));
custom_spec_opts(initiator, Cfg, Spec) ->
    custom_log_keep(
      initiator,
      Cfg,
      maybe_apply_f(proplists:get_value(initiator_opts, Cfg, #{}), Spec)).

maybe_apply_f(F, Spec) when is_function(F, 1) ->
    F(Spec);
maybe_apply_f(Map, _) when is_map(Map) ->
    Map.

custom_log_keep(Role, Cfg, Acc) ->
    case ?config(expected_fsm_logs, Cfg) of
        #{ Role := L } ->
            Len = length(L),
            Acc#{log_keep => Len + (Len div 2)};  % keep some safety margin
        undefined ->
            Acc
    end.

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

match_responder_and_initiator(RProxy, IProxy, Debug, Timeout) ->
    receive
        {channel_up, RProxy, Info} ->
            ?LOG(Debug, "Matched initiator/responder pair: ~p", [Info]),
            Info
    after Timeout ->
            ?PEEK_MSGQ(true),
            ?LOG(Debug, "Timed out waiting for matched pair", []),
            ?LOG(Debug, "RProxy: ~p", [process_info(RProxy, messages)]),
            ?LOG(Debug, "IProxy: ~p", [process_info(IProxy, messages)]),
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
    gproc:reg(GprocR = {n,l,{?MODULE,TmpChanId,responder}}, #{ r => R1 }),
    GprocI = {n,l,{?MODULE,TmpChanId,initiator}},
    ?LOG(Debug, "Registered as ~p, waiting for ~p", [GprocR, GprocI]),
    {_IPid, #{ i := I1 , channel_accept := ChAccept }}
        = gproc:await(GprocI, ?TIMEOUT),
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
    gproc:reg(GprocI = {n,l,{?MODULE,TmpChanId,initiator}}, #{ i => I1
                                                             , channel_accept => ChAccept }),
    GprocR = {n,l,{?MODULE,TmpChanId,responder}},
    ?LOG(Debug, "Registered as ~p, waiting for ~p", [GprocI, GprocR]),
    {_RPid, #{ r := #{parent := NewParent}}}
        = gproc:await(GprocR, ?TIMEOUT),
    unlink(Parent),
    link(NewParent),
    ?LOG(Debug, "IProxy entering relay loop", []),
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

verify_close_mutual_tx(SignedTx, ChannelId) ->
    {aesc_close_mutual_tx, Tx} =
    aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    {_, ChInfo} = aesc_close_mutual_tx:serialize(Tx),
    true = lists:member(ChannelId,
                        [ aeser_id:specialize(ChId, channel)||
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

await_locked_rpt(Type, #{role := Role} = R, Timeout, Debug)
  when Type==funding_locked;
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

co_sign_tx(Signer, SignedTx, _Cfg) ->
    #{priv := Priv} = Signer,
    case account_type(Signer) of
        basic ->
            {aec_test_utils:co_sign_tx(SignedTx, Priv), Signer};
        generalized ->
            %% no benchmarks yet for GA
            error(nyi)
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

await_own_funding_locked(Role, Timeout, Debug) ->
    await_own_locked_rpt(own_funding_locked, Role, Timeout, Debug).

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

receive_fsm_id(Node, Peer, Debug) ->
    {ok, #{info := {fsm_up, FsmIdWrapper}}} =
        receive_info(Peer, fun fsm_up/1, Debug),
    %% We need to perform an RPC call as the actual crypto key is
    %% stored on the node itself
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

mine_blocks(_Node, N, #{mine_blocks := {ask, Pid}} = Opt) when is_pid(Pid) ->
    blocks_mined = call_miner(Pid, {mine_blocks, N, maps:without([mine_blocks], Opt)}, ?TIMEOUT),
    ok;
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
      balance => Amount}.

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
    txs_on_chain = call_miner(Pid, {mine_until_txs_on_chain, [TxHash]}, ?LONG_TIMEOUT),
    {ok, []};
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

new_config_table() ->
    spawn(fun() ->
              ets:new(fsm_suite, [set, named_table, public]),
              ets:insert(fsm_suite, {initiator, 1}),
              ets:insert(fsm_suite, {responder, 1}),
              receive
                  die -> ok
              end
          end).

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

is_above_roma_protocol() ->
    aect_test_utils:latest_protocol_version() > ?ROMA_PROTOCOL_VSN.

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
