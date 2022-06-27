%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Test utils for State Channel markets, using HTLC contracts
%%% @end
%%%=============================================================================
-module(aesc_htlc_SUITE).

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
          create_3_party/1
        , simple_msg_relay/1
        , alice_pays_bob/1
        , alice_tries_early_refund/1
        , alice_gets_refund_after_timeout/1
        , bob_tries_receiving_too_late/1
        , shutdown/1
        ]).

%%% ========================================================================
%%% Strategy
%%% We reuse as much as we can from the aesc_fsm_SUITE.
%%% One quirk of the fsm_SUITE is that it spawns proxy processes which are
%%% linked to the testcase process, and here we want to use test sequences
%%% which begin with a market setup that remains throughout the sequence.
%%% To achieve this, we create yet another proxy process in `init_per_group/2`.
%%% This proxy evaluates funs provided by the testcase process, and forwards
%%% all received messages to it (the testcase process 'registers' with the
%%% proxy in `init_per_testcase/2`, etc.)
%%% ========================================================================

-import(aec_test_utils, [ get_debug/1 ]).

-import(aesc_fsm_SUITE, [ create_channel_/3
                        , channel_shutdown/3
                        , prepare_contract_create_args/3
                        , load_contract/4
                        , call_contract/9
                        , receive_log/2
                        , set_configs/2
                        , peek_message_queue/2
                        , prepare_patron/1
                        , prep_initiator/2
                        , prep_responder/2
                        , rpc/4
                        , receive_from_fsm/4
                        , get_both_balances/3
                        ]).

-include_lib("common_test/include/ct.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecore/test/aec_test_utils.hrl").

-define(MINIMUM_FEE, 100).
-define(DEFAULT_TIMEOUT, 3).

-define(TIMEOUT, 3000).
-define(SLIGHTLY_LONGER_TIMEOUT, 2000).
-define(LONG_TIMEOUT, 60000).
-define(PORT, 9325).

-define(PEEK_MSGQ(_D), peek_message_queue(?LINE, _D)).
%% -define(LOG(_Fmt, _Args), log(_Fmt, _Args, ?LINE, true)).
%% -define(LOG(_D, _Fmt, _Args), log(_Fmt, _Args, ?LINE, _D)).

-define(MINIMUM_DEPTH, 5).
-define(MINIMUM_DEPTH_FACTOR, 10).
-define(MINIMUM_DEPTH_STRATEGY, txfee).
-define(INITIATOR_AMOUNT, (10000000 * aec_test_utils:min_gas_price())).
-define(RESPONDER_AMOUNT, (10000000 * aec_test_utils:min_gas_price())).
-define(ACCOUNT_BALANCE, max(?INITIATOR_AMOUNT, ?RESPONDER_AMOUNT) * 1000).

-define(SLOGAN, {slogan, {?FUNCTION_NAME, ?LINE}}).
-define(SLOGAN(I), {slogan, {?FUNCTION_NAME, ?LINE, I}}).


all() ->
    [{group, all_tests}].

groups() ->
    [ {all_tests, [sequence], [ {group, happy_path}
                              ]}
    , {happy_path, [sequence],
       [ create_3_party
       , simple_msg_relay
       , alice_pays_bob
       , alice_tries_early_refund
       , alice_gets_refund_after_timeout
       , bob_tries_receiving_too_late
       , shutdown ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    case aec_governance:get_network_id() of
        Id when Id == <<"local_iris_testnet">>;
                Id == <<"local_ceres_testnet">> ->
            aesc_fsm_SUITE:init_per_suite([{symlink, "latest.aesc_htlc"} | Config]);
        Other ->
            {skip, {only_on_iris, Other}}
    end.

end_per_suite(Config) ->
    aesc_fsm_SUITE:end_per_suite(Config).

init_per_group(_, Config) ->
    init_per_group_(Config).

init_per_group_(Config) ->
    Node = dev1,
    aecore_suite_utils:reinit_with_ct_consensus(Node),
    prepare_patron(Node),
    Alice = prep_client(?ACCOUNT_BALANCE, Node),
    Bob = prep_client(?ACCOUNT_BALANCE, Node),
    #{alice := Ah, bob := Bh} = prep_hub(?ACCOUNT_BALANCE, ?ACCOUNT_BALANCE, Node),
    set_configs([ {alice, Alice}
                , {bob, Bob}
                , {hub_alice, Ah}
                , {hub_bob, Bh}
                , {roles, [alice, bob, hub_alice, hub_bob]}
                , {port, ?PORT}
                , {proxy, spawn_proxy()}
                ], Config).

end_per_group(_Grp, Config) ->
    proxy_stop(Config),
    ok.

init_per_testcase(TC, Config0) ->
    Debug = get_debug(Config0) orelse (os:getenv("CT_DEBUG") == "1"),
    Config = [{debug, Debug} | init_encrypt_nonce(Config0)],
    Config1 = case ?config(saved_config, Config) of
                  undefined ->
                      Config;
                  {_Saver, SavedConf} ->
                      SavedConf ++ Config
              end,
    proxy_register(Config1),
    aesc_fsm_SUITE:init_per_testcase(TC, Config1).

end_per_testcase(_Case, Config) ->
    proxy_unregister(Config),
    ok.

init_encrypt_nonce(Cfg) ->
    Nonce = crypto:strong_rand_bytes(enacl:box_NONCEBYTES()),
    lists:keystore(encrypt_nonce, 1, Cfg, {encrypt_nonce, Nonce}).

%%%===================================================================
%%% Test state
%%%
%%% Note that all testcases need to return {save_config, Config}, in
%%% order for the market data structure to carry forward through the
%%% sequence.
%%%===================================================================

create_3_party(Cfg) ->
    Debug = get_debug(Cfg),
    [Alice, Bob, Ah, Bh] = [?config(R, Cfg) || R <- [alice, bob,
                                                     hub_alice, hub_bob]],
    %%
    %% Alice side of the market (Variables: Cx = Client, Hx = Hub)
    CfgA = set_configs([{responder, Ah}, {initiator, Alice}], Cfg),

    #{i := Ca, r := Ha} = _ChannelA =
        proxy_do(fun() -> create_channel_(CfgA, #{msg_forwarding => true}, Debug) end, CfgA),
    %%
    %% Bob side of the market
    CfgB = set_configs([{responder, Bh}, {initiator, Bob}], Cfg),
    #{i := Cb, r := Hb} = _ChannelB =
        proxy_do(fun() -> create_channel_(CfgB, #{msg_forwarding => true}, Debug) end, CfgB),
    ?LOG("3-way Channel Market set up (no contract yet).", []),
    %%
    {Time, CompileRes} = timer:tc(fun compile_contract/0),
    ct:log("Time to compile contract: ~p", [Time]),
    ct:log("CompileRes = ~p", [CompileRes]),
    %%
    %% Contract for Alice
    Fee = ?MINIMUM_FEE,
    Timeout = ?DEFAULT_TIMEOUT,
    CreateArgsA = contract_create_args(
                    CompileRes, [encpub(Alice), Fee, Timeout], 10),
    {Ha1, Ca1, ContractPubKeyA} =
        proxy_do(fun() -> load_contract(CreateArgsA, Ha, Ca, CfgA) end, CfgA),
    ?LOG("HTLC contract loaded on Alice's channel", []),
    %%
    %% Contract for Bob
    CreateArgsB = contract_create_args(
                    CompileRes, [encpub(Bob), Fee, Timeout], 10),
    {Hb1, Cb1, ContractPubKeyB} =
        proxy_do(fun() -> load_contract(CreateArgsB, Hb, Cb, CfgB) end, CfgB),
    ?LOG("HTLC contract loaded on Bob's channel", []),
    {save_config, [ {market, #{ contract_meta => CompileRes
                              , encpub(Alice) => #{ client   => Ca1
                                                  , hub      => Ha1
                                                  , contract => ContractPubKeyA }
                              , encpub(Bob)   => #{ client   => Cb1
                                                  , hub      => Hb1
                                                  , contract => ContractPubKeyB } }}
                  ]}.

simple_msg_relay(Cfg) ->
    #{pub := A, priv := Apriv, encoded_pub := Ae} = ?config(alice, Cfg),
    #{pub := B, priv := Bpriv, encoded_pub := Be} = ?config(bob, Cfg),
    Msg = <<"hello there!!!">>,
    #{Ae := #{client := Ac, hub := Ah} = ChA,
      Be := #{client := Bc, hub := Bh} = ChB} = _Market = ?config(market, Cfg),
    EncryptedMsg = encrypt_msg(Msg, B, Apriv, Cfg),
    send_inband(Ac, A, B, EncryptedMsg, Cfg),
    relay_msg(Ah, Bh, Cfg),
    receive_inband(Bc, EncryptedMsg, Cfg),
    {ok, Msg} = decrypt_msg(EncryptedMsg, A, Bpriv, Cfg),
    save_config(Cfg).

encrypt_msg(Msg, TheirPub, MyPriv, Cfg) ->
    Nonce = ?config(encrypt_nonce, Cfg),
    EncPub = enacl:crypto_sign_ed25519_public_to_curve25519(TheirPub),
    EncPriv = enacl:crypto_sign_ed25519_secret_to_curve25519(MyPriv),
    enacl:box(Msg, Nonce, EncPub, EncPriv).

decrypt_msg(Msg, TheirPub, MyPriv, Cfg) ->
    Nonce = ?config(encrypt_nonce, Cfg),
    EncPub = enacl:crypto_sign_ed25519_public_to_curve25519(TheirPub),
    EncPriv = enacl:crypto_sign_ed25519_secret_to_curve25519(MyPriv),
    enacl:box_open(Msg, Nonce, EncPub, EncPriv).

alice_pays_bob(Cfg) ->
    #{encoded_pub := AlicePub} = ?config(alice, Cfg),
    #{encoded_pub := BobPub} = ?config(bob, Cfg),
    [{AlicePub, _},
     {BobPub, _}] = BalancesBefore = check_all_balances([AlicePub, BobPub], Cfg),
    Amount = 1000,
    Fee = 100,
    Timeout = 3,
    T0 = timestamp(),
    HashLock = request_hashlock(AlicePub, BobPub, Amount, Cfg),
    ?LOG("Hashlock Res = ~p", [HashLock]),
    %%
    %% Transaction
    %%
    {AbsTimeout, Cfg1} = new_send(AlicePub, BobPub, Amount, Fee, Timeout, HashLock, Cfg),
    {{ok, _}, Cfg2} = new_receive(AlicePub, BobPub, Amount, AbsTimeout, HashLock, Cfg1),
    {{ok, _}, Cfg3} = recv(AlicePub, BobPub, Amount, HashLock, Cfg2),
    {_, Cfg4} = collect(AlicePub, BobPub, Amount, HashLock, Cfg3),
    %%
    %%
    T1 = timestamp(),
    ?LOG("Time for Alice Pays Bob: ~p ms", [T1-T0]),
    {ok, _BalancesAfter} =
        expect_balances(BalancesBefore, [{AlicePub, [{client_balance, -(Amount + Fee)},
                                                     {hub_balance, (Amount + Fee)}]},
                                         {BobPub, [{client_balance, Amount},
                                                   {hub_balance, -Amount}]}], Cfg4),
    save_config(Cfg4).

alice_tries_early_refund(Cfg) ->
    #{encoded_pub := A} = ?config(alice, Cfg),
    #{encoded_pub := B} = ?config(bob, Cfg),
    Amount = 1000,
    Fee = 100,
    Timeout = 3,
    %%
    Before = check_all_balances([A, B], Cfg),
    HashLock = request_hashlock(A, B, Amount, Cfg),
    ?LOG("Hashlock Res = ~p", [HashLock]),
    %%
    %% Transaction
    %%
    {AbsTimeout, Cfg1} = new_send(A, B, Amount, Fee, Timeout, HashLock, Cfg),
    {{error, <<"NOT_YET_REFUNDABLE">>}, Cfg2} =
        refund(A, B, Amount, HashLock, Cfg1),
    {ok, AfterSend} =
        expect_balances(Before, [{A, [{client_balance, -(Amount + Fee)}]}], Cfg2),
    {{ok, _}, Cfg3} = new_receive(A, B, Amount, AbsTimeout, HashLock, Cfg2),
    {{error, <<"NOT_YET_REFUNDABLE">>}, Cfg4} =
        refund(A, B, Amount, HashLock, Cfg3),
    {ok, AfterNewRecv} =
        expect_balances(AfterSend, [{B, [{hub_balance, -Amount}]}], Cfg4),
    {{ok,_}, Cfg5} = recv(A, B, Amount, HashLock, Cfg4),
    {{error, <<"NOT_YET_REFUNDABLE">>}, Cfg6} =
        refund(A, B, Amount, HashLock, Cfg5),
    {ok, AfterRecv} =
        expect_balances(AfterNewRecv, [{B, [{client_balance, Amount}]}], Cfg6),
    {_, Cfg7} = collect(A, B, Amount, HashLock, Cfg6),
    {ok, AfterCollect} =
        expect_balances(AfterRecv, [{A, [{hub_balance, (Amount + Fee)}]}], Cfg7),
    {{error, <<"NOT_ACTIVE">>}, Cfg8} =
        refund(A, B, Amount, HashLock, Cfg7),
    {ok, _} = expect_balances(AfterCollect, [], Cfg8),
    %%
    %%
    save_config(Cfg8).

alice_gets_refund_after_timeout(Cfg) ->
    #{encoded_pub := A} = ?config(alice, Cfg),
    #{encoded_pub := B} = ?config(bob, Cfg),
    Amount = 1000,
    Fee = 100,
    Timeout = 3,
    %%
    Before = check_all_balances([A, B], Cfg),
    HashLock = request_hashlock(A, B, Amount, Cfg),
    ?LOG("Hashlock Res = ~p", [HashLock]),
    %%
    %% Transaction
    %%
    {_AbsTimeout, Cfg1} = new_send(A, B, Amount, Fee, Timeout, HashLock, Cfg),
    {ok, AfterSend} =
        expect_balances(Before, [{A, [{client_balance, -(Amount + Fee)}]}], Cfg1),
    %% Refund timeout is `Timeout + 3`
    mine_key_blocks(Timeout),
    {{error, <<"NOT_YET_REFUNDABLE">>}, Cfg2} =
        refund(A, B, Amount, HashLock, Cfg1),
    mine_key_blocks(3),
    {{ok, _}, Cfg3} =
        refund(A, B, Amount, HashLock, Cfg2),
    {ok, _AfterRefund} =
        expect_balances(AfterSend, [{A, [{client_balance, Amount},
                                         {hub_balance, Fee}]}], Cfg3),
    save_config(Cfg3).

bob_tries_receiving_too_late(Cfg) ->
    #{encoded_pub := A} = ?config(alice, Cfg),
    #{encoded_pub := B} = ?config(bob, Cfg),
    Amount = 1000,
    Fee = 100,
    Timeout = 3,
    %%
    Before = check_all_balances([A, B], Cfg),
    HashLock = request_hashlock(A, B, Amount, Cfg),
    ?LOG("Hashlock Res = ~p", [HashLock]),
    %%
    %% Transaction
    %%
    {AbsTimeout, Cfg1} = new_send(A, B, Amount, Fee, Timeout, HashLock, Cfg),
    {{ok, {{bytes,32},
           {bytes, Id}}}, Cfg2} = new_receive(A, B, Amount, AbsTimeout, HashLock, Cfg1),
    {ok, AfterNewRecv} =
        expect_balances(Before, [{A, [{client_balance, -(Amount + Fee)}]},
                                 {B, [{hub_balance, -Amount}]}], Cfg2),
    mine_key_blocks(Timeout),
    {{error, <<"RECEIVE_TIMEOUT">>}, Cfg3} =
        recv(A, B, Amount, HashLock, Cfg2),
    %%
    %% Hub must wait Timeout + 3 before claiming a collateral refund
    %%
    {{error, <<"NOT_YET_REFUNDABLE">>}, Cfg4} =
        refund_receive(B, Id, Cfg3),
    mine_key_blocks(3),
    {{ok,_}, Cfg5} =
        refund_receive(B, Id, Cfg4),
    {ok, AfterRefundRecv} =
        expect_balances(AfterNewRecv, [{B, [{hub_balance, Amount}]}], Cfg4),
    {{ok,_}, Cfg6} =
        refund(A, B, Amount, HashLock, Cfg5),
    {ok, _} =
        expect_balances(AfterRefundRecv, [{A, [{client_balance, Amount},
                                               {hub_balance, Fee}]}], Cfg6),
    save_config(Cfg6).

shutdown(Config) ->
    Debug = get_debug(Config),
    Market = ?config(market, Config),
    ?LOG("Market = ~p", [Market]),
    maps:fold(
      fun(_ClientPub, #{client := C, hub := H}, ok) ->
              ConfigX = set_configs([{responder, H}, {initiator, C}], Config),
              proxy_do(fun() ->
                               shutdown(C, H, ConfigX, Debug)
                       end, ConfigX),
              ok
      end, ok, maps:remove(contract_meta, Market)),
    ok.

save_config(Cfg) ->
    {save_config, [{market, ?config(market, Cfg)}]}.

encpub(#{encoded_pub := P}) ->
    P.

timestamp() ->
    erlang:system_time(millisecond).

%%%===================================================================
%%% Market operations
%%%===================================================================

request_hashlock(A, B, Amount, Cfg) ->
    #{A := ChA, B := ChB} = _Market = ?config(market, Cfg),
    Seq = erlang:unique_integer([positive, monotonic]),
    Req = #{ <<"req">> => <<"SEND">>
           , <<"id">>  => Seq
           , <<"amount">> => Amount },
    ok = inband_msg_via_hub(Req, ChA, ChB, Cfg),
    ?LOG("Inband msg send request:~n~p", [Req]),
    %%
    Secret = crypto:strong_rand_bytes(32),
    HashLock = crypto:hash(sha256, Secret),
    Rep = #{ <<"reply">> => <<"OK">>
           , <<"id">>    => Seq
           , <<"hash_lock">> => aeser_api_encoder:encode(bytearray, HashLock) },
    ok = inband_msg_via_hub(Rep, ChB, ChA, Cfg),
    ?LOG("Inband msg OK:~n~p", [Rep]),
    #{hashlock => HashLock, secret => Secret}.

new_send(A, B, Amount, Fee, Timeout, #{hashlock := HashLock}, Cfg) ->
    {{ok, {integer, AbsTimeout}}, Cfg1} =
        client_calls_contract(A, <<"new_send">>, [Amount, B, Fee, Timeout, HashLock],
                              Amount + Fee, Cfg),
    ?LOG("AbsTimeout from new_send(): ~p", [AbsTimeout]),
    {AbsTimeout, Cfg1}.

new_receive(A, B, Amount, AbsTimeout, #{hashlock := HashLock}, Cfg) ->
    {Res, Cfg1} =
        hub_calls_contract(B, <<"new_receive">>, [Amount, A, AbsTimeout, HashLock],
                           Amount, Cfg),
    ?LOG("new_receive: ~p", [Res]),
    {Res, Cfg1}.

recv(A, B, Amount, #{hashlock := HashLock, secret := Secret}, Cfg) ->
    {Res, Cfg1} =
        client_calls_contract(B, <<"receive">>, [A, Amount, HashLock, Secret], 0, Cfg),
    ?LOG("receive Res: ~p", [Res]),
    {Res, Cfg1}.

collect(A, B, Amount, #{hashlock := HashLock, secret := Secret}, Cfg) ->
    {Res, Cfg1} =
        hub_calls_contract(A, <<"collect">>, [B, Amount, HashLock, Secret], 0, Cfg),
    ?LOG("collect Res: ~p", [Res]),
    {Res, Cfg1}.

refund(A, B, Amount, #{hashlock := HashLock}, Cfg) ->
    {Res, Cfg1} =
        client_calls_contract(A, <<"refund">>, [B, Amount, HashLock], 0, Cfg),
    ?LOG("refund Res: ~p", [Res]),
    {Res, Cfg1}.

refund_receive(B, Id, Cfg) ->
    {Res, Cfg1} =
        hub_calls_contract(B, <<"refund_receive">>, [Id], 0, Cfg),
    ?LOG("refund_receive Res: ~p", [Res]),
    {Res, Cfg1}.

inband_msg_via_hub(Msg0, ChA, ChB, Cfg) ->
    Msg = jsx:encode(Msg0),
    #{ client := #{fsm := FsmC} = Ac, hub := #{pub := HubPub} = Ah } = ChA,
    #{ hub := #{fsm := FsmH} = Bh, client := #{pub := BPub} = Bc } = ChB,
    ok = send_inband(Ac, HubPub, Msg, Cfg),
    ok = receive_inband(Ah, Msg, Cfg),
    ok = send_inband(Bh, BPub, Msg, Cfg),
    ok = receive_inband(Bc, Msg, Cfg).

send_inband(#{fsm := Fsm}, Pub, Msg, Cfg) ->
    ok = proxy_do(fun() ->
                          rpc(dev1, aesc_fsm, inband_msg,
                              [Fsm, Pub, Msg])
                  end, Cfg),
    ok.

send_inband(#{fsm := Fsm}, From, To, Msg, Cfg) ->
    ok = proxy_do(fun() ->
                        rpc(dev1, aesc_fsm, inband_msg,
                            [Fsm, From, To, Msg])
                    end, Cfg).

receive_inband(R, Msg, Cfg) ->
    {ok, Res} =
        receive_from_fsm(
          message, R, fun(#{info := #{info := M}}) when M == Msg ->
                              ok
                      end, 1000),
    ?LOG(get_debug(Cfg), "received inband: ~p", [Res]),
    ok.

relay_msg(Ah, Bh, Cfg) ->
    {ok, Res} =
        receive_from_fsm(
            message, Ah, fun(#{notice := please_forward}) -> ok
                         end, 1000),
    ?LOG(get_debug(Cfg), "relay got ~p", [Res]),
    #{info := #{from := From, to := To, info := Msg}} = Res,
    send_inband(Bh, From, To, Msg, Cfg),
    ok.

client_calls_contract(ChId, F, Args, Deposit, Cfg) ->
    call_contract_({client, hub}, ChId, F, Args, Deposit, Cfg).

hub_calls_contract(ChId, F, Args, Deposit, Cfg) ->
    call_contract_({hub, client}, ChId, F, Args, Deposit, Cfg).

call_contract_({A, B}, ChId, F, Args, Deposit, Cfg) ->
    Debug = get_debug(Cfg),
    #{contract_meta := CMeta, ChId := Ch} = ?config(market, Cfg),
    #{contract := Contract, A := Caller, B := Responder} = Ch,
    {Caller1, Responder1, CallRes} =
        contract_call(Contract, F, Args, Deposit, Caller, Responder, CMeta, Cfg),
    Cfg1 = update_market(ChId, Ch#{A := Caller1, B := Responder1}, Cfg),
    ?LOG(Debug, "Client contract call (~p) -> ~p", [F, CallRes]),
    {CallRes, Cfg1}.

update_market(ChId, Ch, Cfg) ->
    M = ?config(market, Cfg),
    set_configs([{market, M#{ChId := Ch}}], Cfg).

check_all_balances(Keys, Cfg) ->
    Market = ?config(market, Cfg),
    lists:map(fun(K) -> {K, check_channel_balances(K, Market)} end, Keys).

check_channel_balances(Id, Market) ->
    #{Id := #{client := #{fsm := Fsm, pub := CPub},
              hub := #{pub := HPub}}} = Market,
    {CBal, HBal} = get_both_balances(Fsm, CPub, HPub),
    #{client_balance => CBal,
      hub_balance => HBal}.

expect_balances(Before, Changes, Cfg) ->
    Keys = [K || {K, _} <- Before],
    After = check_all_balances(Keys, Cfg),
    Expected = lists:foldl(fun({K, Changes1}, Acc) ->
                                   apply_balance_changes(K, Changes1, Acc)
                           end, Before, Changes),
    ?LOG("Balances~n"
         "   before: ~p~n"
         "   after : ~p~n"
         "   expect: ~p", [Before, After, Expected]),
    {balances_after, After} = {balances_after, Expected},
    {ok, After}.

apply_balance_changes(K, Changes, Bs) ->
    lists:map(fun({K1, Map}) when K1 == K ->
                      {K1, lists:foldl(fun({Side, Amt}, Acc) ->
                                               add_amt(Side, Amt, Acc)
                                       end, Map, Changes)};
                 (Other) -> Other
              end, Bs).

add_amt(K, Amt, Map) ->
    maps:update_with(K, fun(V) -> V + Amt end, Map).

%%%===================================================================
%%% Account preparation
%%%===================================================================

prep_client(Amount, Node) ->
    I = prep_initiator(Amount, Node),
    I#{encoded_pub => encode_pub(I)}.

encode_pub(#{pub := Pub}) ->
    aeser_api_encoder:encode(account_pubkey, Pub).

prep_hub(AmountI, AmountR, Node) ->
    Alice = prep_responder(AmountI, Node),
    Bob   = prep_responder(AmountR, Node),
    #{alice => Alice,
      bob   => Bob}.


shutdown(I, R, Cfg, Debug) ->
    channel_shutdown(I, R, Cfg),
    {ok, _} = receive_log(I, Debug),
    {ok, _} = receive_log(R, Debug),
    ok.

%%%===================================================================
%%% Proxy process
%%%===================================================================
    
spawn_proxy() ->
    Parent = self(),
    spawn(fun() ->
                  proxy_loop(#{parent => Parent})
          end).

proxy_register(Config) ->
    Proxy = ?config(proxy, Config),
    link(Proxy),
    call_proxy({register, self()}, Config).

proxy_unregister(Config) ->
    Proxy = ?config(proxy, Config),
    call_proxy({register, undefined}, Config),
    unlink(Proxy),
    ok.

proxy_stop(Config) ->
    Proxy = ?config(proxy, Config),
    proxy_do(fun() -> ok end, Config),  % just checking
    exit(Proxy, kill).

proxy_do(F, Config) ->
    call_proxy({do, F}, Config).

call_proxy(Req, Config) ->
    Proxy = ?config(proxy, Config),
    MRef = erlang:monitor(process, Proxy),
    Proxy ! {'$proxy_call', {self(), MRef}, Req},
    receive
        {MRef, Result} ->
            erlang:demonitor(MRef),
            Result;
        {'DOWN', MRef, _, _, Reason} ->
            error(Reason)
    after 10000 ->
            error(timeout)
    end.

proxy_loop(#{parent := Parent} = St) ->
    receive
        {'$proxy_call', {From, Ref}, Req} ->
            {Reply, St1} = proxy_handle_call(Req, From, St),
            From ! {Ref, Reply},
            proxy_loop(St1);
        Msg ->
            case Parent of
                undefined ->
                    ct:pal("PROXY (no parent): ~p", [Msg]);
                _ when is_pid(Parent) ->
                    Parent ! Msg
            end,
            proxy_loop(St)
    end.

proxy_handle_call(Req, _From, St) ->
    case Req of
        {register, Pid} ->
            {ok, St#{parent := Pid}};
        {do, F} ->
            {F(), St}
    end.


compile_contract() ->
    Fname = filename:join(code:lib_dir(aecontract),
                          "../../extras/test/contracts/channel_htlc.aes"),
    {ok, Res} = aeso_compiler:file(Fname, [{backend, fate}]),
    Res.

contract_create_args(CompileRes, Args, Deposit) ->
    {ok, CallData} = aefa_fate_code:encode_calldata(
                       maps:get(fate_code, CompileRes), <<"init">>, Args),
    Code = aect_sophia:serialize(CompileRes, _SophiaVsn = 3),
    #{ vm_version  => aect_test_utils:vm_version()
     , abi_version => aect_test_utils:abi_version()
     , deposit     => Deposit
     , code        => Code
     , call_data   => CallData }.

contract_call(ContractId, F, Args, Amount, A, B, Meta, Cfg) ->
    {ok, CallData} = aefa_fate_code:encode_calldata(
                       maps:get(fate_code, Meta), F, Args),
    CallArgs = #{ contract      => ContractId
                , abi_version   => aect_test_utils:abi_version()
                , amount        => Amount
                , call_data     => CallData
                , return_result => true },
    {A1, B1, CallRes} =
        aesc_fsm_SUITE:upd_call_contract(A, B, CallArgs, Cfg),
    {A1, B1, decode_callres(CallRes, F, Meta)}.

%% NOTE: the `unit' data type comes back as `{{tuple, []}, {tuple, {}}}'
%%
decode_callres({ok, Value}, F, Meta) ->
    {ok, aefa_fate_code:decode_result(maps:get(fate_code, Meta), F, Value)};
decode_callres({error, Reason}, _, _) ->
    {error, Reason};
decode_callres({revert, Reason}, _F, _Meta) ->
    {error, aeb_fate_encoding:deserialize(Reason)}.

mine_key_blocks(N) ->
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(dev1), N).
