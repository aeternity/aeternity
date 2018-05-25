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
        , deposit/1
        , withdraw/1
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
      , deposit
      , withdraw
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

deposit(Cfg) ->
    Deposit = 10,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_([{port, 9327}|Cfg]),
    ct:log("I = ~p", [I]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    check_info(),
    ok = rpc(dev1, aesc_fsm, upd_deposit, [FsmI, #{amount => Deposit}]),
    check_info(),
    {I1, _} = await_signing_request(deposit_tx, I),
    {R1, _} = await_signing_request(deposit_created, R),
    mine_blocks(dev1, 4),
    {I2, _R2} = await_initial_state(I1, R1),
    ct:log("I2 = ~p", [I2]),
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I2,
    {IAmt2, RAmt2} = {IAmt0 + Deposit, RAmt0},
    check_info(100).

withdraw(Cfg) ->
    Withdrawal = 2,
    #{ i := #{fsm := FsmI} = I
     , r := #{} = R
     , spec := #{ initiator := _PubI
                , responder := _PubR }} = create_channel_([{port, 9328}|Cfg]),
    ct:log("I = ~p", [I]),
    #{initiator_amount := IAmt0, responder_amount := RAmt0} = I,
    check_info(),
    ok = rpc(dev1, aesc_fsm, upd_withdraw, [FsmI, #{amount => Withdrawal}]),
    check_info(),
    {I1, _} = await_signing_request(withdraw_tx, I),
    {R1, _} = await_signing_request(withdraw_created, R),
    mine_blocks(dev1, 4),
    {I2, _R2} = await_initial_state(I1, R1),
    ct:log("I2 = ~p", [I2]),
    #{initiator_amount := IAmt2, responder_amount := RAmt2} = I2,
    {IAmt2, RAmt2} = {IAmt0 - Withdrawal, RAmt0},
    check_info(100).

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
    mine_blocks(dev1, 4),
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
            {check_amounts(R#{chan_id => ChanId}, SignedTx), SignedTx}
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

mine_blocks(Node, N) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(Node), N).


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
    aecore_suite_utils:spend(NodeName, IPub, Pub, Amount),
    mine_blocks(Node, 2), % one extra for good measure (TODO: ensure tx in block)
    {ok, Amount} = rpc(Node, aehttp_logic, get_account_balance, [Pub]),
    #{role => responder,
      priv => Priv,
      pub  => Pub,
      balance => Amount}.

rpc(Node, Mod, Fun, Args) ->
    Res = rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000),
    ct:log("rpc(~p,~p,~p,~p) -> ~p", [Node, Mod, Fun, Args, Res]),
    Res.

check_amounts(R, SignedTx) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of
        {aesc_offchain_tx, TxI} ->
            IAmt = aesc_offchain_tx:initiator_amount(TxI),
            RAmt = aesc_offchain_tx:responder_amount(TxI),
            R#{ initiator_amount => IAmt
                , responder_amount => RAmt };
        _ ->
            R
    end.
