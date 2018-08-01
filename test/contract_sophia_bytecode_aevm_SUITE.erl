-module(contract_sophia_bytecode_aevm_SUITE).
-behaviour(aevm_chain_api).

%% common_test exports
-export([all/0]).

%% test case exports
-export(
   [ execute_identity_fun_from_sophia_file/1
   , sophia_remote_call/1
   , sophia_factorial/1
   , simple_multi_argument_call/1
   , remote_multi_argument_call/1
   , spend_tests/1
   , complex_types/1
   , environment/1
   , counter/1
   , stack/1
   , simple_storage/1
   , dutch_auction/1
   , oracles/1
   ]).

%% chain API exports
-export([ spend/3, get_balance/2, call_contract/6, get_store/1, set_store/2,
          oracle_register/7, oracle_query/6, oracle_query_spec/2, oracle_response_spec/2,
          oracle_query_oracle/2, oracle_respond/5, oracle_get_answer/3,
          oracle_query_fee/2, oracle_get_question/3, oracle_extend/5]).

-include("apps/aecontract/src/aecontract.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_sophia_file,
           sophia_remote_call,
           sophia_factorial,
           simple_multi_argument_call,
           remote_multi_argument_call,
           spend_tests,
           complex_types,
           environment,
           counter,
           stack,
           simple_storage,
           dutch_auction,
           oracles].

compile_contract(Name) ->
    CodeDir           = code:lib_dir(aesophia, test),
    FileName          = filename:join([CodeDir, "contracts", lists:concat([Name, ".aes"])]),
    {ok, ContractBin} = file:read_file(FileName),
    Options           = [],
    %% Options           = [pp_ast, pp_icode, pp_assembler, pp_bytecode],
    Code = aeso_compiler:from_string(binary_to_list(ContractBin), Options),
    aeu_hex:hexstring_encode(Code).

%% execute_call(Contract, CallData, ChainState) ->
%%     execute_call(Contract, CallData, ChainState, #{}).

execute_call(Contract, CallData, ChainState, Options) ->
    #{Contract := Code} = ChainState,
    ChainState1 = ChainState#{ running => Contract },
    Trace = false,
    Res = aect_evm:execute_call(
          maps:merge(
          #{ code              => Code,
             address           => Contract,
             caller            => 0,
             data              => CallData,
             gas               => 1000000,
             gasPrice          => 1,
             origin            => 0,
             value             => 0,
             currentCoinbase   => 0,
             currentDifficulty => 0,
             currentGasLimit   => 1000000,
             currentNumber     => 0,
             currentTimestamp  => 0,
             chainState        => ChainState1,
             chainAPI          => ?MODULE,
             vm_version        => ?AEVM_01_Sophia_01}, Options),
          Trace),
    case Res of
        {ok, #{ out := RetVal, chain_state := S }} ->
            {ok, RetVal, S};
        Err = {error, _, _}      -> Err
    end.

make_call(Contract, Fun, Args, Env, Options) ->
    #{ Contract := Code } = Env,
    CallData = aect_sophia:create_call(Code,
                    list_to_binary(atom_to_list(Fun)),
                    list_to_binary(Args)),
    execute_call(Contract, CallData, Env, Options).

create_contract(Address, Code, Args, Env) ->
    Env1 = Env#{Address => Code},
    {ok, InitS, Env2} = make_call(Address, init, Args, Env1, #{}),
    set_store(aevm_eeevm_store:from_sophia_state(InitS), Env2).

create_contract(Address, Code, Args, Env, Options) ->
    Env1 = Env#{Address => Code},
    {ok, InitS, Env2} = make_call(Address, init, Args, Env1, Options),
    set_store(aevm_eeevm_store:from_sophia_state(InitS), Env2).

successful_call_(Contract, Type, Fun, Args, Env) ->
    {Res, _Env1} = successful_call(Contract, Type, Fun, Args, Env),
    Res.

successful_call_(Contract, Type, Fun, Args, Env, Options) ->
    {Res, _Env1} = successful_call(Contract, Type, Fun, Args, Env, Options),
    Res.

successful_call(Contract, Type, Fun, Args, Env) ->
    successful_call(Contract, Type, Fun, Args, Env, #{}).

successful_call(Contract, Type, Fun, Args, Env, Options) ->
    case make_call(Contract, Fun, Args, Env, Options) of
        {ok, Result, Env1} ->
            case aeso_data:from_binary(Type, Result) of
                {ok, V} -> {V, Env1};
                {error, _} = Err -> exit(Err)
            end;
        {error, Err, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit({error, Err})
    end.

%% failing_call(Contract, Fun, Args, Env) ->
%%     failing_call(Contract, Fun, Args, Env, #{}).

failing_call(Contract, Fun, Args, Env, Options) ->
    case make_call(Contract, Fun, Args, Env, Options) of
        {ok, Result, _} ->
            Words = aeso_test_utils:dump_words(Result),
            exit({expected_failure, {ok, Words}});
        {error, Err, _} ->
            Err
    end.

execute_identity_fun_from_sophia_file(_Cfg) ->
    Code = compile_contract(identity),
    Env  = initial_state(#{101 => Code}),
    42   = successful_call_(101, word, main, "42", Env),
    ok.

sophia_remote_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    CallerCode = compile_contract(remote_call),
    Env        = initial_state(#{ 1234 => IdCode, 1 => CallerCode }),
    42         = successful_call_(1, word, call42, "1234", Env),
    ok.

sophia_factorial(_Cfg) ->
    Code    = compile_contract(factorial),
    Env     = initial_state(#{ 999001 => Code, 999002 => Code }),
    3628800 = successful_call_(999001, word, main, "999002", Env),
    ok.

simple_multi_argument_call(_Cfg) ->
    RemoteCode = compile_contract(remote_call),
    Env        = initial_state(#{ 103 => RemoteCode }),
    19911      = successful_call_(103, word, plus, "(9900,10011)", Env),
    ok.

remote_multi_argument_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    RemoteCode = compile_contract(remote_call),
    Env        = initial_state(#{ 101 => IdCode, 102 => RemoteCode, 103 => RemoteCode }),
    42         = successful_call_(102, word, staged_call, "(102,101,42)", Env),
    ok.

spend_tests(_Cfg) ->
    Code = compile_contract(spend_test),
    Env  = initial_state(#{ 101 => Code, 102 => Code },
                         #{ 101 => 1000, 102 => 2000, 1 => 10000 }),
    {900, Env1}  = successful_call(101, word, withdraw, "100", Env, #{caller => 102}),
    {900, Env2}  = successful_call(101, word, get_balance, "()", Env1),
    {2100, Env3} = successful_call(102, word, get_balance, "()", Env2),
    %% The call fails with out_of_gas
    out_of_gas   = failing_call(101, withdraw, "1000", Env3, #{caller => 102}),
    900          = successful_call_(101, word, get_balance, "()", Env3),
    2100         = successful_call_(102, word, get_balance, "()", Env3),
    %% Spending in nested call
    {900, Env4}  = successful_call(101, word, withdraw_from, "(102,1000)", Env3, #{caller => 1}),
    #{1 := 11000, 101 := 900, 102 := 1100} = maps:get(accounts, Env4),
    ok.

complex_types(_Cfg) ->
    Code = compile_contract(complex_types),
    Env  = initial_state(#{101 => Code}),
    21                      = successful_call_(101, word, sum, "[1,2,3,4,5,6]", Env),
    [1, 2, 3, 4, 5, 6]      = successful_call_(101, {list, word}, up_to, "6", Env),
    [1, 2, 3, 4, 5, 6]      = successful_call_(101, {list, word}, remote_list, "6", Env),
    {<<"answer:">>, 21}     = successful_call_(101, {tuple, [string, word]}, remote_triangle, "(101,6)", Env),
    <<"string">>            = successful_call_(101, string, remote_string, "()", Env),
    {99, <<"luftballons">>} = successful_call_(101, {tuple, [word, string]}, remote_pair, "(99,\"luftballons\")", Env),
    [1, 2, 3]               = successful_call_(101, {list, word}, filter_some, "[None, Some(1), Some(2), None, Some(3)]", Env),
    [1, 2, 3]               = successful_call_(101, {list, word}, remote_filter_some, "[None, Some(1), Some(2), None, Some(3)]", Env),

    {some, [1, 2, 3]}       = successful_call_(101, {option, {list, word}}, all_some, "[Some(1), Some(2), Some(3)]", Env),
    none                    = successful_call_(101, {option, {list, word}}, all_some, "[Some(1), None, Some(3)]", Env),

    {some, [1, 2, 3]}       = successful_call_(101, {option, {list, word}}, remote_all_some, "[Some(1), Some(2), Some(3)]", Env),
    none                    = successful_call_(101, {option, {list, word}}, remote_all_some, "[Some(1), None, Some(3)]", Env),

    N       = 10,
    Squares = [ {I, I * I} || I <- lists:seq(1, N) ],
    Squares = successful_call_(101, {list, {tuple, [word, word]}}, squares, integer_to_list(N), Env),
    Squares = successful_call_(101, {list, {tuple, [word, word]}}, remote_squares, integer_to_list(N), Env),
    ok.

environment(_Cfg) ->
    Code          = compile_contract(environment),
    Address       = 1001,
    Address2      = 1002,
    Balance       = 9999,
    CallerBalance = 500,
    Caller        = 1,
    Origin        = 11,
    Value         = 100,
    GasPrice      = 3,
    Coinbase      = 22022,
    Timestamp     = 1234,
    BlockHeight   = 701,
    Difficulty    = 18,
    GasLimit      = 1000001,
    ChainEnv      =
              #{ origin            => Origin
               , gasPrice          => GasPrice
               , currentCoinbase   => Coinbase
               , currentTimestamp  => Timestamp
               , currentNumber     => BlockHeight
               , currentDifficulty => Difficulty
               , currentGasLimit   => GasLimit
               },
    State = initial_state(#{Address => Code, Address2 => Code, environment => ChainEnv},
                          #{Address => Balance, Caller => CallerBalance}),
    Options = maps:merge(#{ caller => Caller, value  => Value }, ChainEnv),
    Call1   = fun(Fun, Arg) -> successful_call_(Address, word, Fun, integer_to_list(Arg), State, Options) end,
    Call    = fun(Fun)      -> successful_call_(Address, word, Fun, "()", State, Options) end,

    Address  = Call(contract_address),
    Address2 = Call1(nested_address, Address2),
    Balance  = Call(contract_balance),

    Origin   = Call(call_origin),
    Origin   = Call(nested_origin),
    Caller   = Call(call_caller),
    Address  = Call(nested_caller),
    Value    = Call(call_value),
    99       = Call1(nested_value, 99),
    GasPrice = Call(call_gas_price),

    CallerBalance = Call1(get_balance, Caller),
    0             = Call1(block_hash, BlockHeight), %% TODO: BLOCKHASH not implemented in EVM
    Coinbase      = Call(coinbase),
    Timestamp     = Call(timestamp),
    BlockHeight   = Call(block_height),
    Difficulty    = Call(difficulty),
    GasLimit      = Call(gas_limit),

    ok.

%% State tests

counter(_Cfg) ->
    Code       = compile_contract(counter),
    Env        = initial_state(#{}),
    Env1       = create_contract(101, Code, "5", Env),
    {5,  Env2} = successful_call(101, word, get, "()", Env1),
    {{}, Env3} = successful_call(101, {tuple, []}, tick, "()", Env2),
    {6, _Env4} = successful_call(101, word, get, "()", Env3),
    ok.

stack(_Cfg) ->
    Code      = compile_contract(stack),
    Env       = create_contract(101, Code, "[\"bar\"]", initial_state(#{})),
    {1, Env1} = successful_call(101, word, size, "()", Env),
    {2, Env2} = successful_call(101, word, push, "\"foo\"", Env1),
    {<<"foo">>,  Env3} = successful_call(101, string, pop, "()", Env2),
    {<<"bar">>, _Env4} = successful_call(101, string, pop, "()", Env3),
    ok.

simple_storage(_Cfg) ->
    Code = compile_contract(simple_storage),
    Env0 = initial_state(#{}),
    Env1 = create_contract(101, Code, "(42)", Env0),
    {42, Env2} = successful_call(101, word, get, "()", Env1),
    {{}, Env3} = successful_call(101, {tuple, []}, set, "84", Env2),
    {84, _Env4} = successful_call(101, word, get, "()", Env3),
    ok.

dutch_auction(_Cfg) ->
    DaCode = compile_contract(dutch_auction),
    Badr = 101,                                 %Beneficiary address
    Cadr = 102,                                 %Caller address
    Dadr = 110,                                 %Dutch auction address
    ChainEnv = #{origin => 11, currentNumber => 20},
    Env0 = initial_state(#{ environment => ChainEnv },
                         #{ Badr => 1000, Cadr => 1000, 1 => 10000 }),
    Env1 = create_contract(Dadr, DaCode, "(101, 500, 5)", Env0,
                           #{ currentNumber => 42 }),
    %% Currently this is how we can add value to an account for a call.
    Env2 = increment_account(Dadr, 500, Env1),
    {_,Env3} = successful_call(Dadr, {tuple,[]}, bid, "()", Env2,
                               #{ caller => Cadr, currentNumber => 60 }),
    %% The auction has been closed so bidding again should fail.
    failing_call(Dadr, bid, "()", Env3,
                 #{ caller => Cadr, currentNumber => 70}),
    %% Decrement is 5 per block and 18 blocks so 410 have been
    %% transferred to benficiary and 90 back to caller.
    1410 = get_balance(<<Badr:256>>, Env3),
    1090 = get_balance(<<Cadr:256>>, Env3),
    ok.

increment_account(Account, Value, Env) ->
    UpdAcc = fun (Amount) -> Amount + Value end,
    maps:update_with(accounts,
                     fun (Accs) ->
                             maps:update_with(Account, UpdAcc, Value, Accs)
                     end,
                     Env).


oracles(_Cfg) ->
    Code = compile_contract(oracles),
    Env0 = initial_state(#{}),
    Env1 = create_contract(101, Code, "()", Env0),
    QFee = 100,
    {101, Env2} = successful_call(101, word, registerOracle, "(101, 3, 4, "++ integer_to_list(QFee) ++", 10)", Env1),
    {Q, Env3}   = successful_call(101, word, createQuery, "(101, \"why?\", "++ integer_to_list(QFee) ++", 10, 11)", Env2, #{value => QFee}),
    QArg        = integer_to_list(Q),
    OandQArg    = "(101, "++ QArg ++")",
    none        = successful_call_(101, {option, word}, getAnswer, OandQArg, Env3),
    {{}, Env4}  = successful_call(101, {tuple, []}, respond, "(101," ++ QArg ++ ",111,42)", Env3),
    {some, 42}  = successful_call_(101, {option, word}, getAnswer, OandQArg, Env4),
    <<"why?">>  = successful_call_(101, string, getQuestion, OandQArg, Env4),
    QFee        = successful_call_(101, word, queryFee, "101", Env4),
    {{}, Env5}  = successful_call(101, {tuple, []}, extendOracle, "(101, 1111, 10, 100)", Env4),
    #{oracles :=
          #{101 := #{
             nonce := 1,
             query_spec := string,
             response_spec := word,
             sign := 3,
             ttl := 100}},
      oracle_queries :=
          #{Q := #{ oracle := 101,
                    query := <<"why?">>,
                    q_ttl := 10,
                    r_ttl := 11,
                    answer := {some, 42} }}} = Env5,
    ok.




%% -- Chain API implementation -----------------------------------------------

initial_state(Contracts) ->
    initial_state(Contracts, #{}).

initial_state(Contracts, Accounts) ->
    maps:merge(#{environment => #{}, store => #{}},
        maps:merge(Contracts, #{accounts => Accounts})).

spend(_To, Amount, _) when Amount < 0 ->
    {error, negative_spend};
spend(ToId, Amount, S = #{ running := From, accounts := Accounts }) ->
    <<To:256>> = aec_id:specialize(ToId, account),
    Balance = get_balance(<<From:256>>, S),
    case Amount =< Balance of
        true -> {ok, S#{ accounts := Accounts#{ From => Balance            - Amount,
                                                To   => get_balance(<<To:256>>, S) + Amount } }};
        false -> {error, insufficient_funds}
    end.

get_balance(<<Account:256>>, #{ accounts := Accounts }) ->
    maps:get(Account, Accounts, 0).

get_store(#{ running := Contract, store := Store }) ->
    Data = maps:get(Contract, Store, undefined),
    case Data of
        undefined -> aevm_eeevm_store:from_sophia_state(<<>>);
        _         -> Data
    end.

set_store(Data, State = #{ running := Contract, store := Store }) ->
    State#{ store => Store#{ Contract => Data } }.

call_contract(<<Contract:256>>, _Gas, Value, CallData, _, S = #{running := Caller}) ->
    case maps:is_key(Contract, S) of
        true ->
            #{environment := Env0} = S,
            Env = maps:merge(Env0, #{address => Contract, caller => Caller, value => Value}),
            Res = execute_call(Contract, CallData, S, Env),
            case Res of
                {ok, Ret, #{ accounts := Accounts }} ->
                    {ok, aevm_chain_api:call_result(Ret, 0), S#{ accounts := Accounts }};
                {error, out_of_gas, _} ->
                    io:format("  result = out_of_gas\n"),
                    {ok, aevm_chain_api:call_exception(out_of_gas, 0), S}
            end;
        false ->
            io:format("  oops, no such contract!\n"),
            {error, {no_such_contract, Contract}}
    end.

oracle_register(PubKey = <<Account:256>>, <<Sign:256>>, QueryFee, TTL, QuerySpec, ResponseSpec, State) ->
    io:format("oracle_register(~p, ~p, ~p, ~p, ~p, ~p)\n", [Account, Sign, QueryFee, TTL, QuerySpec, ResponseSpec]),
    Oracles = maps:get(oracles, State, #{}),
    State1 = State#{ oracles => Oracles#{ Account =>
                        #{sign          => Sign,
                          nonce         => 1,
                          query_fee     => QueryFee,
                          query_spec    => QuerySpec,
                          response_spec => ResponseSpec,
                          ttl           => TTL} } },
    {ok, PubKey, State1}.

oracle_query(<<Oracle:256>>, Q, Value, QTTL, RTTL, State) ->
    io:format("oracle_query(~p, ~p, ~p, ~p, ~p)\n", [Oracle, Q, Value, QTTL, RTTL]),
    QueryKey = <<QueryId:256>> = crypto:hash(sha256, term_to_binary(make_ref())),
    Queries = maps:get(oracle_queries, State, #{}),
    State1  = State#{ oracle_queries =>
                Queries#{ QueryId =>
                    #{oracle => Oracle,
                      query  => Q,
                      q_ttl  => QTTL,
                      r_ttl  => RTTL} } },
    {ok, QueryKey, State1}.

oracle_respond(<<_Oracle:256>>, <<Query:256>>, Sign, R, State) ->
    io:format("oracle_respond(~p, ~p, ~p)\n", [Query, Sign, R]),
    case maps:get(oracle_queries, State, #{}) of
        #{Query := Q} = Queries ->
            State1 = State#{ oracle_queries := Queries#{ Query := Q#{ answer => {some, R} } } },
            {ok, State1};
        _ -> {error, {no_such_query, Query}}
    end.

oracle_extend(<<Oracle:256>>,_Sign,_Fee, TTL, State) ->
    io:format("oracle_extend(~p, ~p, ~p, ~p)\n", [Oracle,_Sign,_Fee, TTL]),
    case maps:get(oracles, State, #{}) of
        #{Oracle := O} = Oracles ->
            State1 = State#{ oracles := Oracles#{ Oracle := O#{ ttl => TTL } } },
            {ok, State1};
        _ -> foo= Oracle, {error, {no_such_oracle, Oracle}}
    end.

oracle_get_answer(<<_Oracle:256>>, <<Query:256>>, State) ->
    case maps:get(oracle_queries, State, #{}) of
        #{Query := Q} ->
            Answer = maps:get(answer, Q, none),
            io:format("oracle_get_answer() -> ~p\n", [Answer]),
            {ok, Answer};
        _ -> {ok, none}
    end.

oracle_get_question(<<_Oracle:256>>, <<Query:256>>, State) ->
    case maps:get(oracle_queries, State, #{}) of
        #{Query := Q} ->
            Question = maps:get(query, Q, none),
            io:format("oracle_get_question() -> ~p\n", [Question]),
            {ok, Question};
        _             -> {ok, none}
    end.

oracle_query_fee(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, []) of
        #{ Oracle := #{query_fee := Fee} } -> {ok, Fee};
        _ -> {error, {no_such_oracle, Oracle}}
    end.

oracle_query_spec(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, []) of
        #{ Oracle := #{query_spec := Spec} } -> {ok, Spec};
        _ -> {error, {no_such_oracle, Oracle}}
    end.

oracle_response_spec(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, #{}) of
        #{ Oracle := #{response_spec := Spec} } -> {ok, Spec};
        _ -> {error, {no_such_oracle, Oracle}}
    end.

oracle_query_oracle(<<Query:256>>, State) ->
    case maps:get(oracle_queries, State, #{}) of
        #{ Query := #{oracle := Oracle} } -> {ok, <<Oracle:256>>};
        _ -> {error, {no_such_oracle_query, Query}}
    end.

