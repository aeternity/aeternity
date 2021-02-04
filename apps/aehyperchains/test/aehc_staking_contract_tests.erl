-module(aehc_staking_contract_tests).


-include("../../aecontract/src/aect_sophia.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(cid(__x__), {'@ct', __x__}).
-define(hsh(__x__), {'#', __x__}).
-define(sig(__x__), {'$sg', __x__}).
-define(oid(__x__), {'@ok', __x__}).
-define(qid(__x__), {'@oq', __x__}).

-define(ALOT, (1000000000000000000000000000000000000000000000000 * aec_test_utils:min_gas_price())).
-define(CALL_COST, (1000 * aec_test_utils:min_gas_price())).
-define(CALL_GAS, 1000000).


setup_log_file() ->
    LogFileDir = filename:join(
        hd(string:split(code:priv_dir(aesophia), "_build", trailing)),
        "_build/test/logs/"),
    LogFileName =
        begin
            {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
            io_lib:format("chain_simulator_~p-~p-~p_~p:~p:~p.log", [Year, Month, Day, Hour, Min, Sec])
        end,
    LogFilePath = filename:join(LogFileDir, LogFileName),
    filelib:ensure_dir(LogFilePath),
    {ok, LogFile} = file:open(LogFilePath, [write]),
    LogFile.

setup() ->
    erlang:system_flag(backtrace_depth, 100),
    application:ensure_all_started(gproc),
    aec_test_utils:mock_genesis_and_forks(),
    mock_protocol(),
    ok = lager:start(),
    group_leader(setup_log_file(), self()),
    aec_keys:start_link(),
    {ok, _} = aec_chain_sim:start(),
    ok.

unsetup(ok) ->
    aec_chain_sim:stop(),
    unmock_protocol(),
    aec_test_utils:unmock_genesis_and_forks().


mock_protocol() ->
    meck:new(aec_hard_forks, [passthrough]),
    meck:expect(aec_hard_forks, protocol_effective_at_height, 1,
                fun(_) -> ?IRIS_PROTOCOL_VSN end),
    ok.

unmock_protocol() ->
    meck:unload(aec_hard_forks).

-define(PROTOCOL_GATE(X), case init:get_argument(network_id) of
                              {ok,[["local_iris_testnet"]]} -> X;
                              _ -> []
                          end).

staking_contract_test_() ->
    {setup, fun setup/0, fun unsetup/1, ?PROTOCOL_GATE(
        [ staking_contract_unit_test_group()
        , staking_contract_scenarios_test_group()
        ])}.

staking_contract_unit_test_group() ->
    {timeout, 20,
        [ {"Unit fun protocol_restrict", fun test_fun_protocol_restrict/0}
        , {"Unit fun valuate", fun test_fun_valuate/0}
        , {"Unit fun staked_tokens", fun test_fun_staked_tokens/0}
        , {"Unit fun requested_withdrawals", fun test_fun_requested_withdrawals/0}
        , {"Unit fun retracted_stake", fun test_fun_retracted_stake/0}
        , {"Unit fun extract_ripe_withdrawals", fun test_fun_extract_ripe_withdrawals/0}
        , {"Unit fun decrease_stake", fun test_fun_decrease_stake/0}
        , {"Unit fun punish", fun test_fun_punish/0}
        ]}.

staking_contract_scenarios_test_group() ->
    {timeout, 40,
        [ {"Simple deposit/withdraw scenario", fun test_deposit_withdraw/0}
        , {"Complex deposit/withdraw scenario", fun test_complex_deposit_withdraw/0}
        , {"Greedy guy trying to steal tokens", fun test_greedy_dude_trying_to_steal_tokens/0}
        , {"Simple election", fun test_simple_election/0}
        ]}.


make_calldata(Fun, Args) when is_atom(Fun) ->
    make_calldata(atom_to_binary(Fun, latin1), Args);
make_calldata(Fun, Args) when is_binary(Fun) ->
    Args1 = format_fate_args(if is_tuple(Args) -> Args;
                                is_list(Args) -> list_to_tuple(Args);
                                true -> {Args}
                             end),
    FunctionId = aeb_fate_code:symbol_identifier(Fun),
    aeb_fate_encoding:serialize(aefate_test_utils:encode({FunctionId, Args1})).

format_fate_args(?cid(B)) ->
    {contract, B};
format_fate_args(?hsh(B)) ->
    {bytes, B};
format_fate_args(?sig(B)) ->
    {bytes, B};
format_fate_args(?oid(B)) ->
    {oracle, B};
format_fate_args(?qid(B)) ->
    {oracle_query, B};
format_fate_args(<<_:256>> = B) ->
    {address, B}; %% Assume it is an address
format_fate_args({bytes, B}) ->
    {bytes, B};
format_fate_args([H|T]) ->
    [format_fate_args(H) | format_fate_args(T)];
format_fate_args(T) when is_tuple(T) ->
    list_to_tuple(format_fate_args(tuple_to_list(T)));
format_fate_args(M) when is_map(M) ->
    maps:from_list(format_fate_args(maps:to_list(M)));
format_fate_args(X) ->
    X.

new_account(Balance) ->
    {ok, #{pubkey := Acct}} = aec_chain_sim:new_account(Balance),
    Acct.

get_balance(Acct) ->
    aec_chain_sim:get_balance(Acct).

restricted_account() ->
    case aec_chain_sim:dict_get(restricted_account, undefined) of
        undefined ->
            Restricted = new_account(?ALOT),
            aec_chain_sim:dict_set(restricted_account, Restricted),
            Restricted;
        Restricted -> Restricted
    end.

staking_contract() ->
    case aec_chain_sim:dict_get(staking_contract, undefined) of
        undefined ->
            error("Staking contract is undefined");
        Con -> Con
    end.

compiler_version() ->
    ?SOPHIA_IRIS_FATE.

create_staking_contract(#{ deposit_delay := D
                        ,  stake_retraction_delay := R
                        ,  withdraw_delay := W
                        }, Stakers0) ->
    Restricted = restricted_account(),
    {Amount, Stakers1} = case Stakers0 of
                             mock_stakers -> {10, #{Restricted => 10}};
                             _ -> {lists:sum(maps:values(Stakers0)), Stakers0}
                         end,
    {ok, Code} = aect_test_utils:compile_filename(compiler_version()
        , "apps/aehyperchains/src/contracts/SimpleElection.aes"
        , [debug_mode]),
    CallData = make_calldata(init, [{D, R, W}, Stakers1, Restricted]),
    Nonce = aec_chain_sim:next_nonce(Restricted),
    {ok, Tx} = aect_create_tx:new(
                 #{ fee         => ?CALL_COST
                 ,  owner_id    => aeser_id:create(account, Restricted)
                 ,  nonce       => Nonce
                 ,  vm_version  => ?VM_FATE_SOPHIA_2
                 ,  abi_version => ?ABI_FATE_SOPHIA_1
                 ,  deposit     => 2137
                 ,  amount      => Amount
                 ,  gas         => ?CALL_GAS
                 ,  gas_price   => aec_test_utils:min_gas_price()
                 ,  ttl         => 0
                 ,  code        => Code
                 ,  call_data   => CallData
                 }),
    aec_chain_sim:sign_and_push(Restricted, Tx),
    Contract = aect_contracts:compute_contract_pubkey(Restricted, Nonce),
    aec_chain_sim:dict_set(staking_contract, Contract),
    staking_contract().


call_staking_contract(Fun, Args, Type) ->
    call_staking_contract(restricted_account(), Fun, Args, Type).
call_staking_contract(Acct, Fun, Args, Type) ->
    call_staking_contract(Acct, 0, Fun, Args, Type).
call_staking_contract(Acct, Value, Fun, Args, Type) ->
    CallData = make_calldata(Fun, Args),
    Nonce = aec_chain_sim:next_nonce(Acct),
    {ok, Tx} = aect_call_tx:new(
               #{ caller_id   => aeser_id:create(account, Acct)
               ,  nonce       => Nonce
               ,  contract_id => aeser_id:create(contract, staking_contract())
               ,  abi_version => ?ABI_FATE_SOPHIA_1
               ,  fee         => ?CALL_COST
               ,  amount      => Value
               ,  gas         => ?CALL_GAS
               ,  gas_price   => aec_test_utils:min_gas_price()
               ,  call_data   => CallData
               }),
    aec_chain_sim:sign_and_push(Acct, Tx),
    add_microblock(),
    CallId = aect_call:id(Acct, Nonce, staking_contract()),
    {Result, Gas} = get_result(Type, CallId),
    Restricted = restricted_account(),
    {ok, PayBackTx} = aec_spend_tx:new(
        #{ sender_id    => aeser_id:create(account, Restricted)
        ,  recipient_id => aeser_id:create(account, Acct)
        ,  amount       => Gas * aec_test_utils:min_gas_price() + ?CALL_COST
        ,  fee          => ?CALL_COST
        ,  nonce        => aec_chain_sim:next_nonce(Restricted)
        ,  payload      => <<"payback">>
        }),
    aec_chain_sim:sign_and_push(Restricted, PayBackTx),
    add_microblock(),
    Result.

get_result(Type, CallId) ->
    Call = aec_chain_sim:get_call(staking_contract(), CallId),
    Result = case aect_call:return_type(Call) of
                 ok ->
                     Res = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
                     case aefate_test_utils:decode(Res, Type) of
                        {variant, [0,1], 0, {}} when element(1, Type) =:= option ->
                            none;
                        {variant, [0,1], 1, {Decoded}} when element(1, Type) =:= option ->
                            {some, Decoded};
                        Decoded ->
                            Decoded
                     end;
                 error ->
                     {error, aect_call:return_value(Call)};
                 revert ->
                     Res = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
                     {revert, aefate_test_utils:decode(Res)}
             end,
    {Result, aect_call:gas_used(Call)}.

fake_random() ->  %% We want to make it predictable
    <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32>>.

%%
%% ENTRYPOINTS
%%
-define(assertSuccess(X), (fun () -> case X of
                              {error, AssertSuccess_Error} -> error(AssertSuccess_Error);
                              {revert, AssertSuccess_Error} -> error(AssertSuccess_Error);
                              AssertSuccess_Success -> AssertSuccess_Success
                          end end)() ).

staked_tokens(Acct) ->
    ?assertSuccess(call_staking_contract(restricted_account(), staked_tokens, [Acct], word)).

retracted_stake(Acct) ->
    call_staking_contract(restricted_account(), retracted_stake, [Acct], word).

requested_withdrawals(Acct) ->
    call_staking_contract(restricted_account(), requested_withdrawals, [Acct], word).

deposit_stake(Acct, Amount) ->
    call_staking_contract(Acct, Amount, deposit_stake, [], word).

request_withdraw(Acct, Amount) ->
    call_staking_contract(Acct, request_withdraw, [Amount], {tuple, []}).

withdraw(Acct) ->
    call_staking_contract(Acct, withdraw, [], word).

get_computed_leader() ->
    call_staking_contract(get_computed_leader, [], word).

get_leader(Delegates, Rand) ->
    call_staking_contract(get_leader, [Delegates, Rand], word).

punish(BadGuy) ->
    call_staking_contract(punish, [BadGuy], {tuple, []}).


add_microblock() ->
    aec_chain_sim:add_microblock().

add_keyblock() ->
    add_keyblock([restricted_account()]).
add_keyblock(Delegates) ->
    aec_chain_sim:add_keyblock(),
    {address, Leader} = get_leader(Delegates, {bytes, fake_random()}),
    aec_chain_sim:add_microblock(),
    <<Leader:256>>.


%%
%% TESTS
%%

-define(assertAbort(MSG, X), ?assertEqual({revert, atom_to_binary(MSG, latin1)}, X)).
-define(assertNegBalance(ACCT, BALANCE), ?assertEqual(?ALOT - BALANCE, get_balance(ACCT))).
-define(assertStakedTokens(ACCT, TOKENS), ?assertEqual(TOKENS, staked_tokens(ACCT))).
-define(assertRequestedWithdrawals(ACCT, TOKENS), ?assertEqual(TOKENS, requested_withdrawals(ACCT))).
-define(assertRetractedStake(ACCT, TOKENS), ?assertEqual(TOKENS, retracted_stake(ACCT))).

-define(INIT_SCENARIO(DD, SRD, WD, ACCS, STAKERS),
        begin
            aec_chain_sim:add_keyblock(),
            create_staking_contract(#{ deposit_delay => DD
                                    ,  stake_retraction_delay => SRD
                                    ,  withdraw_delay => WD
                                    }, STAKERS),
            add_microblock(),
            add_keyblock(),

            HackAccs =
                fun Go(ACCS) ->
                        [new_account(?ALOT) || _ <- ACCS];
                    Go(TooFew) -> Go([{}|TooFew])
                end,
            ACCS = HackAccs([])
        end).
-define(INIT_SCENARIO(DD, SRD, WD, ACCS), ?INIT_SCENARIO(DD, SRD, WD, ACCS, mock_stakers)).


test_fun_protocol_restrict() ->
    ?INIT_SCENARIO(0, 0, 0, []),

    R1 = call_staking_contract(restricted_account(), 0, protocol_restrict, [], {tuple, []}),
    ?assertEqual({}, R1),

    R2 = call_staking_contract(new_account(?ALOT), 0, protocol_restrict, [], {tuple, []}),
    ?assertAbort('PROTOCOL_RESTRICTED', R2).

test_fun_valuate() ->
    ?INIT_SCENARIO(0, 0, 0, []),

    %% Just to ensure that aging does not lower the value
    R1 = call_staking_contract(restricted_account(), 0,
                               valuate, [{100, 0}], word),
    R2 = call_staking_contract(restricted_account(), 0,
                               valuate, [{100, 1}], word),
    R3 = call_staking_contract(restricted_account(), 0,
                               valuate, [{100, 100}], word),
    ?assert(R2 < R1),
    ?assert(R3 < R2).

test_fun_staked_tokens() ->
    ?INIT_SCENARIO(0, 0, 0, [Acct1, Acct2, Acct3]),

    deposit_stake(Acct1, 10),
    ?assertStakedTokens(Acct1, 10),
    ?assertStakedTokens(Acct2, 0),
    ?assertStakedTokens(Acct3, 0),
    deposit_stake(Acct2, 100),
    ?assertStakedTokens(Acct1, 10),
    ?assertStakedTokens(Acct2, 100),
    ?assertStakedTokens(Acct3, 0),
    deposit_stake(Acct3, 1000),
    ?assertStakedTokens(Acct1, 10),
    ?assertStakedTokens(Acct2, 100),
    ?assertStakedTokens(Acct3, 1000),
    deposit_stake(Acct3, 20),
    ?assertStakedTokens(Acct1, 10),
    ?assertStakedTokens(Acct2, 100),
    ?assertStakedTokens(Acct3, 1020),
    deposit_stake(Acct2, 200),
    ?assertStakedTokens(Acct1, 10),
    ?assertStakedTokens(Acct2, 300),
    ?assertStakedTokens(Acct3, 1020),
    deposit_stake(Acct1, 2000),
    ?assertStakedTokens(Acct1, 2010),
    ?assertStakedTokens(Acct2, 300),
    ?assertStakedTokens(Acct3, 1020),

    ?assertSuccess(request_withdraw(Acct2, 150)),
    W1 = ?assertSuccess(withdraw(Acct2)),
    ?assertEqual(150, W1),
    ?assertStakedTokens(Acct2, 150).


test_fun_requested_withdrawals() ->
    ?INIT_SCENARIO(0, 0, 0, [Acct1, Acct2]),

    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(Acct1, 1)),

    deposit_stake(Acct1, 1000),
    deposit_stake(Acct2, 1000),

    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(Acct2, 10000000)),

    request_withdraw(Acct1, 100),
    ?assertRequestedWithdrawals(Acct1, 100),
    ?assertRequestedWithdrawals(Acct2, 0),
    request_withdraw(Acct2, 10),
    ?assertRequestedWithdrawals(Acct1, 100),
    ?assertRequestedWithdrawals(Acct2, 10),
    request_withdraw(Acct2, 2),
    ?assertRequestedWithdrawals(Acct1, 100),
    ?assertRequestedWithdrawals(Acct2, 12),
    request_withdraw(Acct1, 20),
    ?assertRequestedWithdrawals(Acct1, 120),
    ?assertRequestedWithdrawals(Acct2, 12),
    withdraw(Acct1),
    ?assertRequestedWithdrawals(Acct1, 0),
    ?assertRequestedWithdrawals(Acct2, 12),
    withdraw(Acct2),
    ?assertRequestedWithdrawals(Acct1, 0),
    ?assertRequestedWithdrawals(Acct2, 0),

    ok.

test_fun_retracted_stake() ->
    ?INIT_SCENARIO(0, 2, 2, [Acct1, Acct2]),

    deposit_stake(Acct1, 1000),
    deposit_stake(Acct2, 100),

    ?assertRetractedStake(Acct1, 0),
    ?assertRetractedStake(Acct2, 0),

    request_withdraw(Acct1, 100),
    request_withdraw(Acct2, 10),

    ?assertRetractedStake(Acct1, 0),
    ?assertRetractedStake(Acct2, 0),

    add_keyblock(),

    ?assertSuccess(request_withdraw(Acct1, 20)),
    ?assertSuccess(request_withdraw(Acct2, 2)),

    ?assertRetractedStake(Acct1, 0),
    ?assertRetractedStake(Acct2, 0),

    add_keyblock(),

    ?assertRetractedStake(Acct1, 100),
    ?assertRetractedStake(Acct2, 10),

    add_keyblock(),

    ?assertRetractedStake(Acct1, 120),
    ?assertRetractedStake(Acct2, 12).


test_fun_extract_ripe_withdrawals() ->
    ?INIT_SCENARIO(0, 0, 5, []),

    InitHeight = aec_chain_sim:get_height(),

    [add_keyblock() || _ <- lists:seq(1, 10)],
    % Height should be around 10, so requests of height < 5
    % would get extracted and > 5 would remain.

    Test =
        % Takes: - list of HEIGHTS of consequential withdrawals;
        %        - expected list of VALUES & INDICES of remaining withdrawals;
        % Will assign values/indices accordingly: 1, 2, 3...
        fun(WithdrawalHeights, ExpWithdrawalValues) ->
                ValueSeq = lists:seq(1, length(WithdrawalHeights)),
                Withdrawals = [{V, InitHeight + T}
                          || {V, T} <- lists:zip(
                                         ValueSeq,
                                         WithdrawalHeights
                                        )
                         ],
                {T, ResWithdrawals} = call_staking_contract(
                            extract_ripe_withdrawals, [Withdrawals],
                            {tuple, [word, {list, {tuple, [word, word]}}]}),
                ResValues = lists:sort([V || {V, _} <- ResWithdrawals]),
                ?assertEqual(lists:sort(ExpWithdrawalValues), ResValues),
                ?assertEqual(lists:sum(ValueSeq) - lists:sum(ResValues), T)
        end,

    % TODO quickcheck?
    Test([0,9], [2]),
    Test([0,0,0,0], []),
    Test([1,2,3,4,5,6,7,8,9,0], [5,6,7,8,9]),
    Test([9,8,9,8], [1,2,3,4]),
    Test([9,1,7,2,8], [1,3,5]),
    Test([1,9,9,8,4,8,9,9,2], [2,3,4,6,7,8]),

    ok.


test_fun_decrease_stake() ->
    ?INIT_SCENARIO(0, 0, 0, []),
    [add_keyblock() || _ <- [1,2,3,4,5,6,7,8]],

    S = fun(Value, Created) -> {Value, Created} end,
    Test =
        fun(Stakes, Tokens, ExpStakes) ->
                Remaining = call_staking_contract(
                              decrease_stake, [Stakes, Tokens], {list, {tuple, [word, word]}}),
                ?assertEqual(lists:sort(ExpStakes), lists:sort(Remaining))
        end,

    % Assuming that aging doesn't have negative effect on the value
    % TODO quickcheck?
    Test(
      [S(1, 2), S(3, 5), S(1, 0)],
      0,
      [S(1, 2), S(3, 5), S(1, 0)]
     ),

    Test(
      [S(10, 0), S(10, 1), S(10, 2), S(10, 3)],
      10,
      [S(10, 0), S(10, 1), S(10, 2)]
     ),

    Test(
      [S(10, 0), S(10, 1), S(10, 2), S(10, 3)],
      15,
      [S(10, 0), S(10, 1), S(5, 2)]
     ),

    Test(
      [S(5, 3), S(10, 2), S(20, 1), S(30, 0)],
      55,
      [S(10, 0)]
     ),
    ok.


test_fun_punish() ->
    ?INIT_SCENARIO(1, 1, 2, [Acct]),

    deposit_stake(Acct, 10),
    request_withdraw(Acct, 5),
    ?assertStakedTokens(Acct, 10),
    ?assertRequestedWithdrawals(Acct, 5),

    punish(Acct),

    ?assertStakedTokens(Acct, 0),
    ?assertRequestedWithdrawals(Acct, 0),
    ?assertRetractedStake(Acct, 0).


test_deposit_withdraw() ->
    ?INIT_SCENARIO(1, 1, 2, [Acct]),

    deposit_stake(Acct, 10),
    ?assertStakedTokens(Acct, 10),
    ?assertNegBalance(Acct, 10),

    ?assertSuccess(request_withdraw(Acct, 5)),

    add_keyblock(),

    ?assertEqual(0, withdraw(Acct)),
    ?assertStakedTokens(Acct, 10),
    ?assertRequestedWithdrawals(Acct, 5),
    ?assertRetractedStake(Acct, 5),
    ?assertNegBalance(Acct, 10),

    add_keyblock(),

    ?assertEqual(5, withdraw(Acct)),
    ?assertStakedTokens(Acct, 5),
    ?assertRequestedWithdrawals(Acct, 0),
    ?assertRetractedStake(Acct, 0),
    ?assertNegBalance(Acct, 5),

    ok.


test_complex_deposit_withdraw() ->
    ?INIT_SCENARIO(2, 2, 4, [A1, A2, A3]),

    deposit_stake(A1, 1000),
    deposit_stake(A2, 500),
    deposit_stake(A1, 1000),
    deposit_stake(A3, 10),
    deposit_stake(A2, 50),
    request_withdraw(A3, 5),

    ?assertStakedTokens(A1, 2000),
    ?assertStakedTokens(A2, 550),
    ?assertStakedTokens(A3, 10),
    ?assertNegBalance(A3, 10),

    add_keyblock(),

    deposit_stake(A1, 1000),
    request_withdraw(A1, 500),
    deposit_stake(A2, 450),
    withdraw(A3),

    ?assertNegBalance(A3, 10),

    ?assertStakedTokens(A1, 3000),
    ?assertStakedTokens(A2, 1000),
    ?assertStakedTokens(A3, 10),

    ?assertRequestedWithdrawals(A1, 500),
    ?assertRequestedWithdrawals(A2, 0),
    ?assertRequestedWithdrawals(A3, 5),

    add_keyblock(),

    withdraw(A1),
    request_withdraw(A2, 1000),
    withdraw(A2),
    withdraw(A3),
    ?assertNegBalance(A1, 3000),
    ?assertNegBalance(A2, 1000),
    ?assertNegBalance(A3, 10),

    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(A1, 3000)),
    ?assertSuccess(request_withdraw(A1, 2500)),
    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(A3, 10)),
    ?assertSuccess(request_withdraw(A3, 5)),

    add_keyblock(),

    withdraw(A1),
    withdraw(A2),
    withdraw(A3),
    ?assertNegBalance(A1, 3000),
    ?assertNegBalance(A2, 1000),
    ?assertNegBalance(A3, 10),

    add_keyblock(),

    withdraw(A1),
    withdraw(A2),
    withdraw(A3),
    ?assertNegBalance(A1, 3000),
    ?assertNegBalance(A2, 1000),
    ?assertNegBalance(A3, 5),

    add_keyblock(),

    withdraw(A1),
    withdraw(A2),
    withdraw(A3),
    ?assertNegBalance(A1, 2500),
    ?assertNegBalance(A2, 1000),
    ?assertNegBalance(A3, 5),

    add_keyblock(),

    withdraw(A1),
    withdraw(A2),
    withdraw(A3),
    ?assertNegBalance(A1, 0),
    ?assertNegBalance(A2, 0),
    ?assertNegBalance(A3, 0),
    ok.

test_greedy_dude_trying_to_steal_tokens() ->
    ?INIT_SCENARIO(1, 1, 2, [BadGuy, GoodGuy]),

    ?assertAbort('ZERO_STAKE_DEPOSIT', deposit_stake(BadGuy, 0)),
    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(BadGuy, 1)),
    ?assertAbort('NON_POSITIVE_WITHDRAW_REQUEST', request_withdraw(BadGuy, -1)),

    deposit_stake(GoodGuy, 10000),

    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(BadGuy, 1)),

    add_keyblock(),

    request_withdraw(GoodGuy, 5000),

    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(BadGuy, 1)),
    deposit_stake(BadGuy, 100),
    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(BadGuy, 500)),

    add_keyblock(),

    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(BadGuy, 500)),

    add_keyblock(),

    withdraw(GoodGuy),
    ?assertAbort('WITHDRAW_TOO_MUCH', request_withdraw(BadGuy, 500)),

    ok.

test_simple_election() ->
    ?INIT_SCENARIO(0, 0, 0, [ChadStaker, VirginStaker1, VirginStaker2, RisingStar]),
    Stakers = [ChadStaker, VirginStaker1, VirginStaker2, RisingStar], % Do not inline into above!


    deposit_stake(RisingStar, 1),
    deposit_stake(VirginStaker1, 10),
    deposit_stake(ChadStaker, 1000000000000),

    ?assertEqual(ChadStaker, add_keyblock(Stakers)),

    deposit_stake(VirginStaker2, 15),

    ?assertEqual(ChadStaker, add_keyblock(Stakers)),
    ?assertEqual(ChadStaker, add_keyblock([])),

    deposit_stake(RisingStar, 1000000000000000000000000000000000000),
    ?assertEqual(ChadStaker, add_keyblock([])),
    ?assertEqual(RisingStar, add_keyblock(Stakers)),

    ok.
