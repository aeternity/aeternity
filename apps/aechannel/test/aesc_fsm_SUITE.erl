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
        , inband_msgs/1
        , upd_transfer/1
        , deposit/1
        , withdraw/1
        , leave_reestablish/1
        , leave_reestablish_close/1
        , change_config_get_history/1
        , multiple_channels/1
        ]).

%% exports for aehttp_integration_SUITE
-export([
          create_channel_on_port/1
        ]).

-include_lib("common_test/include/ct.hrl").

-define(TIMEOUT, 10000).
-define(LONG_TIMEOUT, 30000).

-define(OP_TRANSFER, 0).
-define(OP_WITHDRAW, 1).
-define(OP_DEPOSIT , 2).

-define(LOG(E), ct:log("LINE ~p <== ~p", [?LINE, E])).

-define(MINIMUM_DEPTH, 3).

-define(SLOGAN, {slogan, {?FUNCTION_NAME, ?LINE}}).
-define(SLOGAN(I), {slogan, {?FUNCTION_NAME, ?LINE, I}}).

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [{group, transactions}]},
     {transactions, [sequence],
      [
        create_channel
      , inband_msgs
      , upd_transfer
      , deposit
      , withdraw
      , leave_reestablish
      , leave_reestablish_close
      , change_config_get_history
      , multiple_channels
      ]
     }
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.aesc_fsm"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    aecore_suite_utils:create_configs(
      Config1, #{<<"chain">> => #{<<"persist">> => false}}),
    aecore_suite_utils:make_multi(Config1, [dev1, dev2]),
    [{nodes, [aecore_suite_utils:node_tuple(N)
              || N <- [dev1]]} | Config1].

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ct:log("dev1 connected", []),
    try begin
            Initiator = prep_initiator(dev1),
            Responder = prep_responder(Initiator, dev1),
            [{initiator, Initiator},
             {responder, Responder}
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
     , r := #{} = R} = create_channel_([{port, 9325},?SLOGAN|Cfg]),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    _ = await_signing_request(shutdown, I, ?TIMEOUT, Debug),
    _ = await_signing_request(shutdown_ack, R, ?TIMEOUT, Debug),
    SignedTx = await_on_chain_report(I, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    verify_close_mutual_tx(SignedTx, ChannelId),
    check_info(500),
    ok.

inband_msgs(Cfg) ->
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := PubR }} = create_channel_(
                                           [{port,9326},?SLOGAN|Cfg]),
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
                                           [{port,9326},?SLOGAN|Cfg]),
    {I0, R0} = do_update(PubI, PubR, 2, I, R, true),
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

deposit(Cfg) ->
    Debug = get_debug(Cfg),
    Deposit = 10,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [{port, 9327},?SLOGAN|Cfg]),
    ct:log("I = ~p", [I]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    {IAmt0, RAmt0, _, _Round0 = 1} = check_fsm_state(FsmI),
    check_info(0),
    ok = rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => Deposit}]),
    {I1, _} = await_signing_request(deposit_tx, I),
    {_R1, _} = await_signing_request(deposit_created, R),
    SignedTx = await_on_chain_report(I1, ?TIMEOUT),
    SignedTx = await_on_chain_report(R, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    {IAmt0, RAmt0, _, _Round1 = 1} = check_fsm_state(FsmI),
    mine_blocks(dev1, ?MINIMUM_DEPTH),
    {IAmt, RAmt0, StateHash, Round2 = 2} = check_fsm_state(FsmI),
    {IAmt, _} = {IAmt0 + Deposit, IAmt}, %% assert correct amounts
    {channel_deposit_tx, DepositTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
    Round2 = aesc_deposit_tx:round(DepositTx), %% assert correct round
    StateHash = aesc_deposit_tx:state_hash(DepositTx), %% assert correct state hash
    ct:log("I2 = ~p", [I1]),
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I1,
    Expected = {IAmt2, RAmt2},
    {Expected, Expected} = {{IAmt0 + Deposit, RAmt0}, Expected},
    check_info(500).

withdraw(Cfg) ->
    Debug = get_debug(Cfg),
    Withdrawal = 2,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_(
                                            [{port, 9328},?SLOGAN|Cfg]),
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
    check_info(500).

leave_reestablish(Cfg) ->
    leave_reestablish(9329, Cfg).

leave_reestablish(Port, Cfg) ->
    leave_reestablish_(Port, [?SLOGAN|Cfg]).

leave_reestablish_(Port, Cfg) ->
    Debug = get_debug(Cfg),
    {I0, R0, Spec0} = channel_spec(Cfg),
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} =
        create_channel_from_spec(I0, R0, Spec0, Port, Debug),
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
    reestablish(ChId, I0, R0, SignedTx, Spec0, Port+1, Debug).

leave_reestablish_close(Cfg) ->
    Debug = get_debug(Cfg),
    #{i := I, r := R, spec := Spec} = leave_reestablish_(9332, [?SLOGAN|Cfg]),
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
     , r := #{} = R } = create_channel_([{port,9335},?SLOGAN|Cfg]),
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

multiple_channels(Cfg) ->
    ct:log("spawning multiple channels", []),
    Initiator = maps:get(pub, ?config(initiator, Cfg)),
    ct:log("Initiator: ~p", [Initiator]),
    Me = self(),
    NumCs = 10,
    {ok, Nonce} = rpc(dev1, aec_next_nonce, pick_for_account, [Initiator]),
    Cs = [create_multi_channel([{port, 9330 + N},
                                {ack_to, Me},
                                {nonce, Nonce + N - 1},
                                ?SLOGAN(N)|Cfg], #{mine_blocks => {ask, Me},
                                                   debug => false})
          || N <- lists:seq(1, NumCs)],
    ct:log("channels spawned", []),
    Cs = collect_acks(Cs, mine_blocks, NumCs),
    ct:log("mining requests collected", []),
    mine_blocks(dev1, 10),
    Cs = collect_acks(Cs, channel_ack, NumCs),
    ct:log("channel pids collected: ~p", [Cs]),
    [P ! {transfer, 100} || P <- Cs],
    T0 = erlang:system_time(millisecond),
    Cs = collect_acks(Cs, transfer_ack, NumCs),
    T1 = erlang:system_time(millisecond),
    Time = T1 - T0,
    Transfers = NumCs*2*100,
    Fmt = "Time (~w*2*100) ~.1f s: ~.1f mspt; ~.1f tps",
    Args = [NumCs, Time/1000, Time/Transfers, (Transfers*1000)/Time],
    ct:log(Fmt, Args),
    ct:comment(Fmt, Args),
    [P ! die || P <- Cs],
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
    Timeout = 30000 + (N div 10)*5000,  % wild guess
    receive
        {Pid, Tag} ->
            [Pid | collect_acks(Pids, Tag, N)]
    after Timeout ->
            error(timeout)
    end;
collect_acks([], _Tag, _) ->
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
            Parent ! {self(), transfer_ack},
            ch_loop(I1, R1, Parent);
        die -> ok
    end.

create_channel_on_port(Port) ->
    Node = dev1,
    I = prep_initiator(Node),
    R = prep_responder(I, Node),
    Cfg = [{port, Port}, {initiator, I}, {responder, R}, ?SLOGAN],
    create_channel_(Cfg, get_debug(Cfg)).

create_channel_(Cfg) ->
    create_channel_(Cfg, get_debug(Cfg)).

create_channel_(Cfg, Debug) ->
    {I, R, Spec} = channel_spec(Cfg),
    Port = proplists:get_value(port, Cfg, 9325),
    create_channel_from_spec(I, R, Spec, Port, Debug).

channel_spec(Cfg) ->
    I = ?config(initiator, Cfg),
    R = ?config(responder, Cfg),

    %% dynamic key negotiation
    Proto = <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>,

    IAmt = 5,
    RAmt = 5,
    Spec = #{initiator        => maps:get(pub, I),
             responder        => maps:get(pub, R),
             initiator_amount => IAmt,
             responder_amount => RAmt,
             push_amount      => 2,
             lock_period      => 10,
             channel_reserve  => 3,
             minimum_depth    => ?MINIMUM_DEPTH,
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

slogan(Cfg) ->
    ?config(slogan, Cfg).

create_channel_from_spec(
  I, R, #{initiator_amount := IAmt, responder_amount := RAmt} = Spec, Port, Debug) ->
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
    CurrentHeight =
        case rpc(dev1, aec_chain, top_header, []) of
            undefined -> 0;
            Header -> aec_headers:height(Header)
        end,
    SignedTx = await_on_chain_report(I2, ?TIMEOUT),
    SignedTx = await_on_chain_report(R2, ?TIMEOUT), % same tx
    wait_for_signed_transaction_in_block(dev1, SignedTx, Debug),
    mine_blocks(dev1, ?MINIMUM_DEPTH, Debug),
    %% in case of multiple channels starting in parallel - the mining above
    %% has no effect (the blocks are mined in another process)
    %% The following line makes sure this process is blocked until the proper
    %% height is reached
    aecore_suite_utils:wait_for_height(aecore_suite_utils:node_name(dev1),
                                       CurrentHeight + ?MINIMUM_DEPTH),
    {ok, _} = receive_from_fsm(info, R2, own_funding_locked, ?TIMEOUT, Debug),
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

set_amounts(IAmt, RAmt, Map) ->
    Map#{initiator_amount => IAmt, responder_amount => RAmt}.

verify_close_mutual_tx(SignedTx, ChannelId) ->
    {channel_close_mutual_tx, Tx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
    {_, ChInfo} = aesc_close_mutual_tx:serialize(Tx),
    true = lists:member(ChannelId,
        [ aec_id:specialize(ChId, channel)||
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
    {ok, _Msg} = receive_from_fsm(
                   update, R,
                   fun(#{ channel_id := ChId1
                         , info := SignedTx }) ->
                           true =
                               ChId1 == ChId
                               andalso
                               element(1, SignedTx) == signed_tx
                   end, Timeout, Debug),
    R.

await_signing_request(Tag, R) ->
    await_signing_request(Tag, R, ?TIMEOUT, true).

await_signing_request(Tag, R, Debug) ->
    await_signing_request(Tag, R, ?TIMEOUT, Debug).

await_signing_request(Tag, #{fsm := Fsm, priv := Priv} = R, Timeout, Debug) ->
    receive {aesc_fsm, Fsm, #{type := sign, tag := Tag, info := Tx} = Msg} ->
            log(Debug, "await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
            SignedTx = aec_test_utils:sign_tx(Tx, [Priv]),
            aesc_fsm:signing_response(Fsm, Tag, SignedTx),
            {check_amounts(R, SignedTx), SignedTx}
    after Timeout ->
            error(timeout)
    end.

await_on_chain_report(#{fsm := Fsm}, Timeout) ->
    receive {aesc_fsm, Fsm, #{type := report, tag := on_chain_tx, info := SignedTx}} ->
              SignedTx
    after Timeout ->
              error(timeout)
    end.

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
                    ct:log("Message doesn't match fun: ~p / ~p", [Msg]),
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
            []
    end.

mine_blocks(Node, N) -> mine_blocks(Node, N, true).

mine_blocks(_Node, _N, #{mine_blocks := {ask, Pid}}) when is_pid(Pid) ->
    Pid ! {self(), mine_blocks},
    ok;
mine_blocks(_, _, #{mine_blocks := false}) ->
    ok;
mine_blocks(Node, N, _) ->
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(Node), N).


prep_initiator(Node) ->
    {ok, PubKey} = rpc(Node, aec_keys, pubkey, []),
    {ok, PrivKey} = rpc(Node, aec_keys, sign_privkey, []),
    ct:log("initiator Pubkey = ~p", [PubKey]),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(Node), 1),
    ct:log("initiator: 1 block mined on ~p", [Node]),
    {ok, Balance} = rpc(Node, aehttp_logic, get_account_balance, [PubKey]),
    #{role => initiator,
      priv => PrivKey,
      pub  => PubKey,
      balance => Balance}.

prep_responder(#{pub := IPub, balance := IBal} = _Initiator, Node) ->
    NodeName = aecore_suite_utils:node_name(Node),
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    Amount = IBal div 10,
    {ok, SignedTx} = aecore_suite_utils:spend(NodeName, IPub, Pub, Amount),
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

check_amounts(R, SignedTx) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of
        {aesc_offchain_tx, TxI} ->
            apply_updates(aesc_offchain_tx:updates(TxI), R);
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
apply_updates([{?OP_TRANSFER, From, To, Amount} | T], R) ->
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
    TxHash = aetx_sign:hash(SignedTx),
    MineTx =
        fun Try(0) -> did_not_mine;
            Try(Attempts) ->
                aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(Node), 2),
                case rpc(Node, aec_chain, find_tx_location, [TxHash]) of
                    none -> erlang:error(tx_not_found);
                    not_found -> erlang:error(tx_not_found);
                    mempool -> Try(Attempts - 1);
                    BlockHash when is_binary(BlockHash) -> ok
                end
        end,
    ok = MineTx(5).

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

