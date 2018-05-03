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
        ]).

%% test case exports
-export([
          create_channel/1
        , upd_transfer/1
        , inband_msgs/1
        ]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aechannel/include/channel_txs.hrl").

-define(TIMEOUT, 10000).
-define(LONG_TIMEOUT, 30000).

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [{group, transactions}]},
     {transactions, [sequence],
      [ create_channel
      , inband_msgs
      , upd_transfer
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
              || N <- [dev1, dev2]]} | Config1].

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:start_node(dev2, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ct:log("dev1 connected", []),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev2)),
    ct:log("dev2 connected", []),
    try [{initiator, prep_account(initiator, dev1)},
         {responder, prep_account(responder, dev2)}
         | Config]
    catch
        error:Reason ->
            Trace = erlang:get_stacktrace(),
            catch stop_node(dev1, Config),
            catch stop_node(dev2, Config),
            error(Reason, Trace)
    end.

end_per_group(_Group, Config) ->
    Config1 = stop_node(dev1, Config),
    _Config2 = stop_node(dev2, Config1),
    ok.

stop_node(N, Config) ->
    aecore_suite_utils:stop_node(N, Config),
    Config.


%%%===================================================================
%%% Test state
%%%===================================================================

create_channel(Cfg) ->
    #{ i := #{fsm := FsmI}
     , r := #{fsm := FsmR} } = _Ch = create_channel_([{port, 9325}|Cfg]),
    rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    rpc(dev1, aesc_fsm, shutdown, [FsmR]),
    check_info(1000),
    ok.

upd_transfer(Cfg) ->
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := PubI
                , responder := PubR }} = create_channel_([{port,9326}|Cfg]),
    check_info(),
    rpc(dev1, aesc_fsm, upd_transfer, [FsmI, PubI, PubR, 2]),
    {I1, _} = await_signing_request(update, I),
    {R1, _} = await_signing_request(update_ack, R),
    check_info(100),
    ok = rpc(dev1, aesc_fsm, shutdown, [FsmI]),
    {_I2, _} = await_signing_request(shutdown, I1),
    {_R2, _} = await_signing_request(shutdown_ack, R1),
    check_info(100),
    {ok, [Tx]} = rpc(dev1, aec_tx_pool, peek, [1]),
    ct:log("From mempool: ~p", [Tx]),
    ok.

inband_msgs(Cfg) ->
    #{ i := #{fsm := FsmI} = _I
     , r := #{fsm := FsmR} = R
     , spec := #{ initiator := _PubI
                , responder := PubR }} = create_channel_([{port,9326}|Cfg]),
    check_info(),
    ok = rpc(dev1, aesc_fsm, inband_msg, [FsmI, PubR, <<"i2r hello">>]),
    receive_from_fsm(message, R, fun(#{info := <<"i2r hello">>}) -> ok end, 1000),
    rpc(dev1, erlang, exit, [FsmI, kill]),
    rpc(dev1, erlang, exit, [FsmR, kill]),
    check_info(1000),
    ok.

create_channel_(Cfg) ->
    I = ?config(initiator, Cfg),
    R = ?config(responder, Cfg),

    Port = proplists:get_value(port, Cfg, 9325),

    %% dynamic key negotiation
    Proto = <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>,

    Spec = #{initiator        => maps:get(pub, I),
             responder        => maps:get(pub, R),
             initiator_amount => 5,
             responder_amount => 5,
             push_amount      => 2,
             lock_period      => 10,
             channel_reserve  => 3,
             minimum_depth    => 3,
             ttl              => 100,
             client           => self(),
             noise            => [{noise, Proto}],
             timeouts         => #{idle => 20000},
             report_info      => true},

    {ok, FsmR} = rpc(dev1, aesc_fsm, respond, [Port, Spec]),
    {ok, FsmI} = rpc(dev1, aesc_fsm, initiate, ["localhost", Port, Spec]),

    ct:log("FSMs, I = ~p, R = ~p", [FsmI, FsmR]),

    I1 = I#{fsm => FsmI},
    R1 = R#{fsm => FsmR},

    {I2, R2} = try await_create_tx_i(I1, R1)
               catch
                   error:Err ->
                       ct:log("Caught Err = ~p~nMessages = ~p",
                              [Err, element(2, process_info(self(), messages))]),
                       error(Err, erlang:get_stacktrace())
               end,
    #{i => I2, r => R2, spec => Spec}.

await_create_tx_i(I, R) ->
    {I1, _} = await_signing_request(create_tx, I),
    await_funding_created_p(I1, R).

await_funding_created_p(I, R) ->
    {R1, _} = await_signing_request(funding_created, R),
    await_funding_signed_i(I, R1).

await_funding_signed_i(I, R) ->
    receive_info(I, funding_signed),
    await_funding_locked(I, R).

await_funding_locked(I, R) ->
    ct:log("mining blocks on dev1 for minimum depth", []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(dev1), 4),
    await_initial_state(I, R).

await_initial_state(I, R) ->
    {I1, _} = await_signing_request(update, I, ?LONG_TIMEOUT),
    {R1, _} = await_signing_request(update_ack, R),
    {I1, R1}.

await_signing_request(Tag, R) ->
    await_signing_request(Tag, R, ?TIMEOUT).

await_signing_request(Tag, #{fsm := Fsm, priv := Priv} = R, Timeout) ->
    check_info(),
    receive {aesc_fsm, Fsm, ChanId, {sign, Tag, Tx}} = Msg ->
            ct:log("await_signing(~p, ~p) <- ~p", [Tag, Fsm, Msg]),
            SignedTx = aetx_sign:sign(Tx, [Priv]),
            aesc_fsm:signing_response(Fsm, Tag, SignedTx),
            {R#{chan_id => ChanId}, SignedTx}
    after Timeout ->
            error(timeout)
    end.

receive_info(R, Msg) ->
    receive_from_fsm(info, R, Msg, ?TIMEOUT).

receive_from_fsm(Tag, #{role := Role, fsm := Fsm}, Msg, Timeout) ->
    receive
        {aesc_fsm, Fsm, _ChanId, {Tag, Msg1}} ->
            ct:log("~p: received ~p:~p", [Role, Tag, Msg1]),
            match_msgs(Msg, Msg1)
    after Timeout ->
            error(timeout)
    end.

match_msgs(F, Msg) when is_function(F, 1) ->
    F(Msg);
match_msgs(M, M) ->
    ok;
match_msgs(A, B) ->
    ct:log("Messages don't match: ~p / ~p", [A, B]),
    erlang:error({message_mismatch, [A, B]}).


check_info() ->
    check_info(0).

check_info(Timeout) ->
    receive
        {aesc_fsm, Fsm, ChanId, {Tag, Msg}} = Info ->
            ct:log("Received ~p: ~p", [Tag, Info]),
            [{Fsm, ChanId, Msg}|check_info(Timeout)]
    after Timeout ->
            []
    end.

prep_account(Role, Node) ->
    {ok, PubKey} = rpc(Node, aec_keys, pubkey, []),
    {ok, PrivKey} = rpc(Node, aec_keys, privkey, []),
    ct:log("~p: Pubkey = ~p", [Role, PubKey]),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(Node), 9),
    ct:log("~p: 6 blocks mined on ~p", [Role, Node]),
    {ok, Balance} = rpc(Node, aehttp_logic, get_account_balance, [PubKey]),
    #{role => Role,
      priv => PrivKey,
      pub  => PubKey,
      balance => Balance}.

rpc(Node, Mod, Fun, Args) ->
    Res = rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000),
    ct:log("rpc(~p,~p,~p,~p) -> ~p", [Node, Mod, Fun, Args, Res]),
    Res.
