-module(contract_sophia_bytecode_aevm_SUITE).
-behaviour(aevm_chain_api).

-include_lib("stdlib/include/assert.hrl").

%% common_test exports
-export([all/0]).

%% test case exports
-export(
   [ execute_identity_fun_from_sophia_file/1
   , sophia_remote_call/1
   , sophia_remote_call_negative/1
   , sophia_factorial/1
   , simple_multi_argument_call/1
   , remote_multi_argument_call/1
   , spend_tests/1
   , complex_types/1
   , environment/1
   , counter/1
   , stack/1
   , strings/1
   , simple_storage/1
   , dutch_auction/1
   , oracles/1
   , blockhash/2
   ]).

%% chain API exports
-export([ get_height/1,
          spend_tx/3,
          spend/2,
          get_balance/2,
          get_contract_fun_types/4,
          call_contract/6,
          get_store/1,
          set_store/2,
          oracle_register_tx/7,
          oracle_register/3,
          oracle_query_tx/6,
          oracle_query/2,
          oracle_query_format/2,
          oracle_response_format/2,
          oracle_respond_tx/5,
          oracle_respond/3,
          oracle_get_answer/3,
          oracle_query_fee/2,
          oracle_query_response_ttl/3,
          oracle_get_question/3,
          oracle_extend_tx/3,
          oracle_extend/3]).

-include("apps/aecontract/src/aecontract.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_sophia_file,
           sophia_remote_call,
           sophia_remote_call_negative,
           sophia_factorial,
           simple_multi_argument_call,
           remote_multi_argument_call,
           spend_tests,
           complex_types,
           environment,
           counter,
           stack,
           strings,
           simple_storage,
           dutch_auction,
           oracles].

compile_contract(Name) ->
    %% TODO get this path from the common test configuration
    CodeDir           = filename:join(code:lib_dir(aevm), "../../extras/test/contracts"),
    FileName          = filename:join(CodeDir, lists:concat([Name, ".aes"])),
    {ok, ContractBin} = file:read_file(FileName),
    Options           = <<>>,
    %% Options           = <<"pp_ast pp_icode pp_assembler pp_bytecode">>,
    {ok, Serialized} = aect_sophia:compile(ContractBin, Options),
    Serialized.

%% execute_call(Contract, CallData, ChainState) ->
%%     execute_call(Contract, CallData, ChainState, #{}).

execute_call(Contract, CallData, ChainState, Options) ->
    #{Contract := SerializedCode} = ChainState,
    ChainState1 = ChainState#{ running => Contract },
    #{ byte_code := Code,
       type_info := TypeInfo
     } = aect_sophia:deserialize(SerializedCode),
    case aeso_abi:check_calldata(CallData, TypeInfo) of
        {ok, CallDataType, OutType} ->
            execute_call_1(Contract, CallData, CallDataType, OutType, Code, ChainState1, Options);
        {error, _} = Err ->
            Err
    end.

execute_call_1(Contract, CallData, CallDataType, OutType, Code, ChainState, Options) ->
    Trace = false,
    Res = aect_evm:execute_call(
          maps:merge(
          #{ code              => Code,
             store             => get_store(ChainState),
             address           => Contract,
             caller            => 0,
             data              => CallData,
             call_data_type    => CallDataType,
             out_type          => OutType,
             gas               => 1500000,
             gasPrice          => 1,
             origin            => 0,
             value             => 0,
             currentCoinbase   => 0,
             currentDifficulty => 0,
             currentGasLimit   => 2000000,
             currentNumber     => 0,
             currentTimestamp  => 0,
             chainState        => ChainState,
             chainAPI          => ?MODULE,
             vm_version        => ?CURRENT_AEVM_SOPHIA}, Options),
          Trace),
    case Res of
        {ok, #{ out := RetVal, chain_state := S }} ->
            {ok, RetVal, S};
        Err = {error, _, _}      -> Err;
        Rev = {revert,_, _}      -> Rev
    end.

make_call(Contract, Fun, Type, Args, Env, Options) ->
    #{ Contract := Code } = Env,
    TypeBin = list_to_binary(Type),
    FunBin  = atom_to_binary(Fun, utf8),
    TypeSig = <<FunBin/binary, " : ", TypeBin/binary, " => _">>,
    {ok, CallData} = aect_sophia:encode_call_data(Code, TypeSig,
                                                  list_to_binary(Args)),
    execute_call(Contract, CallData, Env, Options).

create_contract(Address, Code, ArgType, Args, Env) ->
    Env1 = Env#{Address => Code},
    {ok, InitS, Env2} = make_call(Address, init, ArgType, Args, Env1, #{}),
    set_store(aevm_eeevm_store:from_sophia_state(InitS), Env2).

create_contract(Address, Code, ArgType, Args, Env, Options) ->
    Env1 = Env#{Address => Code},
    {ok, InitS, Env2} = make_call(Address, init, ArgType, Args, Env1, Options),
    set_store(aevm_eeevm_store:from_sophia_state(InitS), Env2).

successful_call_(Contract, Type, Fun, ArgType, Args, Env) ->
    {Res, _Env1} = successful_call(Contract, Type, Fun, ArgType, Args, Env),
    Res.

successful_call_(Contract, Type, Fun, ArgType, Args, Env, Options) ->
    {Res, _Env1} = successful_call(Contract, Type, Fun, ArgType, Args, Env, Options),
    Res.

successful_call(Contract, Type, Fun, ArgType, Args, Env) ->
    successful_call(Contract, Type, Fun, ArgType, Args, Env, #{}).

successful_call(Contract, Type, Fun, ArgType, Args, Env, Options) ->
    case make_call(Contract, Fun, ArgType, Args, Env, Options) of
        {ok, Result, Env1} ->
            case aeso_heap:from_binary(Type, Result) of
                {ok, V} -> {V, Env1};
                {error, _} = Err -> exit(Err)
            end;
        {error, Err, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit({error, Err});
        {revert, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit(revert)
    end.

failing_call(Contract, Fun, Type, Args, Env) ->
    failing_call(Contract, Fun, Type, Args, Env, #{}).

failing_call(Contract, Fun, Type, Args, Env, Options) ->
    case make_call(Contract, Fun, Type, Args, Env, Options) of
        {ok, Result, _} ->
            Words = aevm_test_utils:dump_words(Result),
            exit({expected_failure, {ok, Words}});
        {revert, _} ->
            exit({expected_failure, revert});
        {error, Err, _} ->
            Err
    end.

reverting_call(Contract, Fun, Type, Args, Env) ->
    reverting_call(Contract, Fun, Type, Args, Env, #{}).

reverting_call(Contract, Fun, Type, Args, Env, Options) ->
    case make_call(Contract, Fun, Type, Args, Env, Options) of
        {ok, Result, _} ->
            Words = aevm_test_utils:dump_words(Result),
            exit({expected_revert, {ok, Words}});
        {error, Err, _} ->
            exit({expected_revert, {error, Err}});
        {revert, _, _} ->
            revert
    end.


execute_identity_fun_from_sophia_file(_Cfg) ->
    Code = compile_contract(identity),
    Env0 = initial_state(#{}),
    Env  = create_contract(101, Code, "()", "", Env0),
    42   = successful_call_(101, word, main, "int", "42", Env),
    ok.

sophia_remote_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    CallerCode = compile_contract(remote_call),
    Env        = create_contract(16#1234, IdCode, "()", "",
                 create_contract(1, CallerCode, "()", "",
                    initial_state(#{}, #{1 => 20000}))),
    42         = successful_call_(1, word, call, "(address, int)", "#1234, 42", Env),
    ok.

sophia_remote_call_negative(_Cfg) ->
    IdCode     = compile_contract(identity),
    CallerCode = compile_contract(remote_call),
    %% The call fails for insufficient balance for the value specified in the remote call.
    Env        = create_contract(16#1234, IdCode, "()", "",
                 create_contract(1, CallerCode, "()", "",
                    initial_state(#{}))),
    out_of_gas = failing_call(1, call, "(address, int)", "#1234, 42", Env),
    ok.

sophia_factorial(_Cfg) ->
    Code    = compile_contract(factorial),
    Env     = create_contract(999001, Code, "int", "999002",
              create_contract(999002, Code, "int", "999001",
                initial_state(#{}))),
    3628800 = successful_call_(999001, word, fac, "int", "10", Env),
    ok.

simple_multi_argument_call(_Cfg) ->
    RemoteCode = compile_contract(remote_call),
    Env        = create_contract(103, RemoteCode, "()", "", initial_state(#{})),
    19911      = successful_call_(103, word, plus, "(int, int)", "9900,10011", Env),
    ok.

remote_multi_argument_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    RemoteCode = compile_contract(remote_call),
    Env        = create_contract(16#101, IdCode, "()", "",
                 create_contract(16#102, RemoteCode, "()", "",
                 create_contract(16#103, RemoteCode, "()", "",
                 initial_state(#{}, #{16#102 => 20000})))),
    42         = successful_call_(16#102, word, staged_call, "(address, address, int)", "#101,#102,42", Env),
    ok.

spend_tests(_Cfg) ->
    Code = compile_contract(spend_test),
    Env  = create_contract(101, Code, "()", "",
           create_contract(102, Code, "()", "",
           initial_state(#{}, #{ 101 => 1000, 102 => 2000, 1 => 10000 }))),
    {900, Env1}  = successful_call(101, word, withdraw, "int", "100", Env, #{caller => 102}),
    {900, Env2}  = successful_call(101, word, get_balance, "()", "", Env1),
    {2100, Env3} = successful_call(102, word, get_balance, "()", "", Env2),
    %% The call fails with out_of_gas
    out_of_gas   = failing_call(101, withdraw, "int", "1000", Env3, #{caller => 102}),
    900          = successful_call_(101, word, get_balance, "()", "", Env3),
    2100         = successful_call_(102, word, get_balance, "()", "", Env3),
    %% Spending in nested call
    {900, Env4}  = successful_call(101, word, withdraw_from, "(int, int)", "102,1000", Env3, #{caller => 1}),
    #{1 := 11000, 101 := 900, 102 := 1100} = maps:get(accounts, Env4),
    ok.

complex_types(_Cfg) ->
    Code = compile_contract(complex_types),
    Env0 = initial_state(#{}),
    Env  = create_contract(101, Code, "int", "101", Env0),
    21                      = successful_call_(101, word, sum, "list(int)", "[1,2,3,4,5,6]", Env),
    [1, 2, 3, 4, 5, 6]      = successful_call_(101, {list, word}, up_to, "int", "6", Env),
    [1, 2, 3, 4, 5, 6]      = successful_call_(101, {list, word}, remote_list, "int", "6", Env),
    {<<"answer:">>, 21}     = successful_call_(101, {tuple, [string, word]}, remote_triangle, "(int, int)", "101,6", Env),
    <<"string">>            = successful_call_(101, string, remote_string, "()", "", Env),
    {99, <<"luftballons">>} = successful_call_(101, {tuple, [word, string]}, remote_pair, "(int, string)", "99,\"luftballons\"", Env),
    [1, 2, 3]               = successful_call_(101, {list, word}, filter_some, "list(option(int))", "[None, Some(1), Some(2), None, Some(3)]", Env),
    [1, 2, 3]               = successful_call_(101, {list, word}, remote_filter_some, "list(option(int))", "[None, Some(1), Some(2), None, Some(3)]", Env),

    {some, [1, 2, 3]}       = successful_call_(101, {option, {list, word}}, all_some, "list(option(int))", "[Some(1), Some(2), Some(3)]", Env),
    none                    = successful_call_(101, {option, {list, word}}, all_some, "list(option(int))", "[Some(1), None, Some(3)]", Env),

    {some, [1, 2, 3]}       = successful_call_(101, {option, {list, word}}, remote_all_some, "list(option(int))", "[Some(1), Some(2), Some(3)]", Env),
    none                    = successful_call_(101, {option, {list, word}}, remote_all_some, "list(option(int))", "[Some(1), None, Some(3)]", Env),

    N       = 10,
    Squares = [ {I, I * I} || I <- lists:seq(1, N) ],
    Squares = successful_call_(101, {list, {tuple, [word, word]}}, squares, "int", integer_to_list(N), Env),
    Squares = successful_call_(101, {list, {tuple, [word, word]}}, remote_squares, "int", integer_to_list(N), Env),
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
    State0 = initial_state(#{environment => ChainEnv},
                           #{Address => Balance, Caller => CallerBalance}),
    State   = create_contract(Address2, Code, "int", integer_to_list(Address),
              create_contract(Address, Code, "int", integer_to_list(Address2), State0)),
    Options = maps:merge(#{ caller => Caller, value  => Value }, ChainEnv),
    Call1   = fun(Fun, Arg) -> successful_call_(Address, word, Fun, "int", integer_to_list(Arg), State, Options) end,
    Call    = fun(Fun)      -> successful_call_(Address, word, Fun, "()", "", State, Options) end,

    Address  = Call(contract_address),
    Address2 = Call1(nested_address, Address2),
    Balance  = Call(contract_balance),

    Origin   = Call(call_origin),
    Origin   = Call(nested_origin),
    Caller   = Call(call_caller),
    Address  = Call(nested_caller),
    Value    = Call(call_value),
    49       = Call1(nested_value, 99),
    GasPrice = Call(call_gas_price),

    CallerBalance = Call1(get_balance, Caller),
    0             = Call1(block_hash, BlockHeight),
    0             = Call1(block_hash, BlockHeight + 1),
    0             = Call1(block_hash, BlockHeight - 257),
    BlockHashHeight1 = BlockHeight - 1,
    BlockHashHeight1 = Call1(block_hash, BlockHashHeight1),
    BlockHashHeight2 = BlockHeight - 256,
    BlockHashHeight2 = Call1(block_hash, BlockHashHeight2),
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
    Env1       = create_contract(101, Code, "int", "5", Env),
    {5,  Env2} = successful_call(101, word, get, "()", "", Env1),
    {{}, Env3} = successful_call(101, {tuple, []}, tick, "()", "", Env2),
    {6, _Env4} = successful_call(101, word, get, "()", "", Env3),
    ok.

stack(_Cfg) ->
    Code      = compile_contract(stack),
    Env       = create_contract(101, Code, "list(string)", "[\"bar\"]", initial_state(#{})),
    {1, Env1} = successful_call(101, word, size, "()", "", Env),
    {2, Env2} = successful_call(101, word, push, "string", "\"foo\"", Env1),
    {<<"foo">>,  Env3} = successful_call(101, string, pop, "()", "", Env2),
    {<<"bar">>, _Env4} = successful_call(101, string, pop, "()", "", Env3),
    ok.

strings(_Cfg) ->
    Code      = compile_contract(strings),
    Env       = initial_state(#{}),
    Env1      = create_contract(101, Code, "()", "", Env),
    Strings = ["",
               "five?",
               "this_is_sixteen!",
               "surely_this_is_twentyseven!",
               "most_likely_this_is_thirtytwo?!?",
               "guvf_vf_zbfg_yvxryl_guveglgjb?!?guvf_vf_zbfg_yvxryl_guveglgjb?!?"],
    TestStrings =
        fun(Str1, Str2) ->
            ExpectedLen = length(Str1 ++ Str2),
            {ActualLen, Env1} = successful_call(101, word, str_len, "string", "\"" ++ Str1 ++ Str2 ++ "\"", Env1),
            ct:log("str_len(~p) = ~p - expected ~p", [Str1++Str2, ActualLen, ExpectedLen]),
            ?assertEqual(ExpectedLen, ActualLen),

            ExpectedStr = Str1 ++ Str2,
            {ActualStr, Env1} = successful_call(101, string, str_concat, "(string, string)", "\"" ++ Str1 ++ "\", \"" ++ Str2 ++ "\"", Env1),
            ct:log("str_concat(~p, ~p) = ~p", [Str1, Str2, binary_to_list(ActualStr)]),
            ?assertEqual(ExpectedStr, binary_to_list(ActualStr))
        end,
    [ TestStrings(X, Y) || X <- Strings, Y <- Strings ],
    ok.

simple_storage(_Cfg) ->
    Code = compile_contract(simple_storage),
    Env0 = initial_state(#{}),
    Env1 = create_contract(101, Code, "int", "42", Env0),
    {42, Env2} = successful_call(101, word, get, "()", "", Env1),
    {{}, Env3} = successful_call(101, {tuple, []}, set, "int", "84", Env2),
    {84, _Env4} = successful_call(101, word, get, "()", "", Env3),
    ok.

dutch_auction(_Cfg) ->
    DaCode = compile_contract(dutch_auction),
    Badr = 101,                                 %Beneficiary address
    Cadr = 102,                                 %Caller address
    Dadr = 110,                                 %Dutch auction address
    ChainEnv = #{origin => 11, currentNumber => 20},
    Env0 = initial_state(#{ environment => ChainEnv },
                         #{ Badr => 1000, Cadr => 1000, 1 => 10000 }),
    Env1 = create_contract(Dadr, DaCode, "(int, int, int)", "101, 500, 5", Env0,
                           #{ currentNumber => 42 }),
    %% Currently this is how we can add value to an account for a call.
    Env2 = increment_account(Dadr, 500, Env1),
    {_,Env3} = successful_call(Dadr, {tuple,[]}, bid, "()", "", Env2,
                               #{ caller => Cadr, currentNumber => 60 }),
    %% The auction has been closed so bidding again should fail.
    reverting_call(Dadr, bid, "()", "", Env3,
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
    QFee = 100,
    ChainEnv = #{ currentNumber => 20 },
    Env0 = initial_state(#{ environment => ChainEnv }, #{101 => QFee}),
    Env1 = create_contract(101, Code, "()", "", Env0),
    {101, Env2} = successful_call(101, word, registerOracle, "(int, int, Chain.ttl)", "101, "++ integer_to_list(QFee) ++", RelativeTTL(10)", Env1),
    {Q, Env3}   = successful_call(101, word, createQuery, "(int, string, int, Chain.ttl, Chain.ttl)",
                                  "101, \"why?\", "++ integer_to_list(QFee) ++", RelativeTTL(10), RelativeTTL(11)", Env2, #{value => QFee}),
    QArg        = integer_to_list(Q),
    OandQArg    = "101, "++ QArg,
    none        = successful_call_(101, {option, word}, getAnswer, "(int, int)", OandQArg, Env3),
    {{}, Env4}  = successful_call(101, {tuple, []}, respond, "(int, int, int)", "101," ++ QArg ++ ",42", Env3),
    {some, 42}  = successful_call_(101, {option, word}, getAnswer, "(int, int)", OandQArg, Env4),
    <<"why?">>  = successful_call_(101, string, getQuestion, "(int, int)", OandQArg, Env4),
    QFee        = successful_call_(101, word, queryFee, "int", "101", Env4),
    {{}, Env5}  = successful_call(101, {tuple, []}, extendOracle, "(int, Chain.ttl)", "101, RelativeTTL(100)", Env4),
    #{oracles :=
          #{101 := #{
             nonce := 1,
             query_format := QFormat,
             response_format := RFormat,
             ttl := {delta, 100}}},
      oracle_queries :=
          #{Q := #{ oracle := 101,
                    query := <<"why?">>,
                    q_ttl := {delta, 10},
                    r_ttl := {delta, 11},
                    answer := {some, 42} }}} = Env5,
    {ok, string} = aeso_heap:from_binary(typerep, QFormat),
    {ok, word}   = aeso_heap:from_binary(typerep, RFormat),
    ok.




%% -- Chain API implementation -----------------------------------------------

initial_state(Contracts) ->
    initial_state(Contracts, #{}).

initial_state(Contracts, Accounts) ->
    maps:merge(#{environment => #{}, store => #{}},
        maps:merge(Contracts, #{accounts => Accounts})).

get_height(#{ environment := #{ currentNumber := Height } }) ->
    Height.

spend_tx(_To, Amount, _) when Amount < 0 ->
    {error, negative_spend};
spend_tx(ToId, Amount, State = #{running := From}) ->
    Balance = get_balance(<<From:256>>, State),
    case Amount =< Balance of
        true ->
            Spec =
                #{sender_id    => aec_id:create(account, <<From:256>>),
                  recipient_id => ToId,
                  amount       => Amount,
                  fee          => 0,
                  nonce        => 1,
                  payload      => <<>>},
            aec_spend_tx:new(Spec);
        false ->
            {error, insufficient_funds}
    end.

spend(Tx, State = #{accounts := Accounts}) ->
    {aec_spend_tx, STx} = aetx:specialize_callback(Tx),
    <<To:256>> = aec_id:specialize(aec_spend_tx:recipient_id(STx), account),
    <<From:256>> = aec_id:specialize(aec_spend_tx:sender_id(STx), account),
    Amount = aec_spend_tx:amount(STx),
    FromBalance = get_balance(<<From:256>>, State),
    ToBalance = get_balance(<<To:256>>, State),
    {ok, State#{accounts := Accounts#{From => FromBalance - Amount,
                                      To   => ToBalance + Amount}}}.

get_balance(<<Account:256>>, #{ accounts := Accounts }) ->
    maps:get(Account, Accounts, 0).

get_store(#{ running := Contract, store := Store }) ->
    Data = maps:get(Contract, Store, undefined),
    case Data of
        undefined -> aect_contracts_store:new();
        _         -> Data
    end.

set_store(Data, State = #{ running := Contract, store := Store }) ->
    State#{ store => Store#{ Contract => Data } }.

get_contract_fun_types(<<Contract:256>>,_VMVersion, TypeHash, S) ->
    case maps:get(Contract, S, undefined) of
        undefined ->
            io:format("   oops, no such contract!\n"),
            {error, {no_such_contract, Contract}};
        SerializedCode ->
            #{type_info := TypeInfo} = aect_sophia:deserialize(SerializedCode),
            aeso_abi:typereps_from_type_hash(TypeHash, TypeInfo)
    end.


call_contract(<<Contract:256>>, Gas, Value, CallData, _, S = #{running := Caller}) ->
    case maps:is_key(Contract, S) of
        true ->
            #{environment := Env0} = S,
            Env = maps:merge(Env0, #{address => Contract, caller => Caller, value => Value}),
            Res = execute_call(Contract, CallData, S, Env),
            case Res of
                {ok, Ret, #{ accounts := Accounts }} ->
                    {aevm_chain_api:call_result(Ret, 0), S#{ accounts := Accounts }};
                {error, out_of_gas, _} ->
                    io:format("  result = out_of_gas\n"),
                    {aevm_chain_api:call_exception(out_of_gas, 0), S}
            end;
        false ->
            io:format("  oops, no such contract!\n"),
            {aevm_chain_api:call_exception(unknown_contract, Gas), S}
    end.

oracle_register_tx(PubKey = <<Account:256>>, QueryFee, TTL, QFormat, RFormat, VMVersion,_State) ->
    io:format("oracle_register(~p, ~p, ~p, ~p, ~p)\n", [Account, QueryFee, TTL, QFormat, RFormat]),
    Spec =
        #{account_id      => aec_id:create(account, PubKey),
          nonce           => 1,
          query_format    => aeso_heap:to_binary(QFormat),
          response_format => aeso_heap:to_binary(RFormat),
          query_fee       => QueryFee,
          oracle_ttl      => TTL,
          ttl             => 0,
          fee             => 0,
          vm_version      => VMVersion
         },
    aeo_register_tx:new(Spec).

oracle_register(Tx, Signature, State) ->
    {aeo_register_tx, OTx} = aetx:specialize_callback(Tx),
    <<PubKey:256>> = aeo_register_tx:account_pubkey(OTx),
    Oracles = maps:get(oracles, State, #{}),
    State1 =
        State#{
          oracles =>
            Oracles#{
              PubKey =>
                #{sign            => Signature,
                  nonce           => aeo_register_tx:nonce(OTx),
                  query_fee       => aeo_register_tx:query_fee(OTx),
                  query_format    => aeo_register_tx:query_format(OTx),
                  response_format => aeo_register_tx:response_format(OTx),
                  ttl             => aeo_register_tx:ttl(OTx)}}},
    {ok, <<PubKey:256>>, State1}.

oracle_query_tx(OracleKey = <<Oracle:256>>, Q, Value, QTTL, RTTL,_State) ->
    io:format("oracle_query(~p, ~p, ~p, ~p, ~p)\n", [Oracle, Q, Value, QTTL, RTTL]),
    Spec =
        #{sender_id     => aec_id:create(account, OracleKey),
          nonce         => 1,
          oracle_id     => aec_id:create(oracle, OracleKey),
          query         => aeso_heap:to_binary(Q),
          query_fee     => 0,
          query_ttl     => QTTL,
          response_ttl  => RTTL,
          fee           => 0,
          ttl           => 0
         },
    aeo_query_tx:new(Spec).

oracle_query(Tx, State) ->
    {aeo_query_tx, OTx} = aetx:specialize_callback(Tx),
    <<Oracle:256>> = aec_id:specialize(aeo_query_tx:sender_id(OTx), account),
    <<QueryId:256>> = aeo_query_tx:query_id(OTx),
    QBin = aeo_query_tx:query(OTx),
    {ok, Fmt} = oracle_query_format(<<Oracle:256>>, State),
    {ok, Q} = aeso_heap:from_binary(Fmt, QBin),
    QTTL = aeo_query_tx:query_ttl(OTx),
    RTTL = aeo_query_tx:response_ttl(OTx),
    Queries = maps:get(oracle_queries, State, #{}),
    State1 =
        State#{
          oracle_queries =>
            Queries#{
              QueryId =>
                #{oracle => Oracle,
                  query  => Q,
                  q_ttl  => QTTL,
                  r_ttl  => RTTL}}},
    {ok, <<QueryId:256>>, State1}.

oracle_respond_tx(OracleKey, QueryIdKey = <<QueryId:256>>, R, RTTL,_State) ->
    io:format("oracle_respond(~p, ~p)\n", [QueryId, R]),
    Spec =
        #{oracle_id    => aec_id:create(oracle, OracleKey),
          nonce        => 1,
          query_id     => QueryIdKey,
          response     => aeso_heap:to_binary(R),
          response_ttl => RTTL,
          fee          => 0,
          ttl          => 0 %% Not used
         },
    aeo_response_tx:new(Spec).

oracle_respond(Tx, _Signature, State) ->
    {aeo_response_tx, OTx} = aetx:specialize_callback(Tx),
    <<QueryId:256>> = aeo_response_tx:query_id(OTx),
    Oracle = aec_id:specialize(aeo_response_tx:oracle_id(OTx), oracle),
    RBin = aeo_response_tx:response(OTx),
    {ok, Fmt} = oracle_response_format(Oracle, State),
    {ok, R} = aeso_heap:from_binary(Fmt, RBin),
    case maps:get(oracle_queries, State, #{}) of
        #{QueryId := Q} = Queries ->
            State1 =
                State#{
                  oracle_queries := Queries#{QueryId := Q#{answer => {some, R}}}},
            {ok, State1};
        _ -> {error, {no_such_query, QueryId}}
    end.

oracle_extend_tx(OracleKey = <<Oracle:256>>, OTTL, _State) ->
    io:format("oracle_extend(~p, ~p)\n", [Oracle, OTTL]),
    Spec =
        #{oracle_id  => aec_id:create(oracle, OracleKey),
          nonce      => 1,
          oracle_ttl => OTTL,
          fee        => 0,
          ttl        => 0 %% Not used
         },
    aeo_extend_tx:new(Spec).

oracle_extend(Tx, _Signature, State) ->
    {aeo_extend_tx, OTx} = aetx:specialize_callback(Tx),
    <<Oracle:256>> = aec_id:specialize(aeo_extend_tx:oracle_id(OTx), oracle),
    OTTL = aeo_extend_tx:oracle_ttl(OTx),
    case maps:get(oracles, State, #{}) of
        #{Oracle := O} = Oracles ->
            State1 = State#{oracles := Oracles#{Oracle := O#{ttl => OTTL}}},
            {ok, State1};
        _ -> foo= Oracle, {error, {no_such_oracle, Oracle}}
    end.

oracle_get_answer(<<_Oracle:256>>, <<QueryId:256>>, State) ->
    case maps:get(oracle_queries, State, #{}) of
        #{QueryId := Q} ->
            Answer = maps:get(answer, Q, none),
            io:format("oracle_get_answer() -> ~p\n", [Answer]),
            {ok, Answer};
        _ -> {ok, none}
    end.

oracle_get_question(<<_Oracle:256>>, <<QueryId:256>>, State) ->
    case maps:get(oracle_queries, State, #{}) of
        #{QueryId := Q} ->
            Question = maps:get(query, Q, none),
            io:format("oracle_get_question() -> ~p\n", [Question]),
            {ok, Question};
        _             -> {ok, none}
    end.

oracle_query_fee(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, []) of
        #{Oracle := #{query_fee := Fee}} -> {ok, Fee};
        _ -> {error, {no_such_oracle, Oracle}}
    end.

oracle_query_response_ttl(<<_Oracle:256>>, <<QueryId:256>>, State) ->
    case maps:get(oracle_queries, State, #{}) of
        #{QueryId := Q} -> {ok, maps:get(r_ttl, Q)};
        _ -> {error, no_such_oracle_query}
    end.

oracle_query_format(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, []) of
        #{Oracle := #{query_format := Format}} -> aeso_heap:from_binary(typerep, Format);
        _ -> {error, {no_such_oracle, Oracle}}
    end.

oracle_response_format(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, #{}) of
        #{Oracle := #{response_format := Format}} -> aeso_heap:from_binary(typerep, Format);
        _ -> {error, {no_such_oracle, Oracle}}
    end.

blockhash(N,_State) ->
    %% Dummy value to check that we reach this.
    <<N:256/unsigned-integer>>.

