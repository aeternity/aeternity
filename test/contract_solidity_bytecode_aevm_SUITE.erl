-module(contract_solidity_bytecode_aevm_SUITE).

%% Commented for avoiding warnings without implementing all dummy callback. %% -behaviour(aevm_chain_api).

%% common_test exports
-export(
   [ all/0
   ]).

%% test case exports
-export(
   [ execute_counter_fun_from_bytecode/1
   , execute_identity_fun_from_solidity_binary/1
   , events_from_solidity_binary/1
   ]).

%% aevm_chain_api callbacks
-export([ spend/3,
          get_balance/2,
          call_contract/7,
          get_store/1,
          set_store/2,
          oracle_register/7,
          oracle_query/6,
          oracle_query_format/2,
          oracle_response_format/2,
          oracle_respond/5,
          oracle_get_answer/3,
          oracle_query_fee/2,
          oracle_query_response_ttl/3,
          oracle_get_question/3,
          oracle_extend/4]).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

%% ------------------------------------------------------------------------
%% Test cases
%% ------------------------------------------------------------------------

all() -> [
          %% This test works when running as a standalone test,
          %% but not with "make test" where other tests have run before.
          %% Taken out of test suite for now.
          %% TODO: Turn into a "dev1" node test.
          execute_counter_fun_from_bytecode
         , execute_identity_fun_from_solidity_binary
         , events_from_solidity_binary
         ].

execute_identity_fun_from_solidity_binary(_Cfg) ->
    Code = aeu_hex:hexstring_decode(id_bytecode()),
    Env  = initial_state(#{101 => Code}),
    NewCode = successful_call_(101, word, main_, <<42>>, Env),
    {ok, <<42:256>>, _, _} =  execute_call(101, <<26,148,216,62,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 42>>, Env#{101 => NewCode}, #{}),
    ok.


id_bytecode() ->
    <<"0x6060604052341561000f57600080fd5b60ae8061001d6000396000f300606060"
      "405260043610603f576000357c0100000000000000000000000000000000000000"
      "000000000000000000900463ffffffff1680631a94d83e146044575b600080fd5b"
      "3415604e57600080fd5b606260048080359060200190919050506078565b604051"
      "8082815260200191505060405180910390f35b60008190509190505600a165627a"
      "7a723058205cc378b9229138b9feea0e5d1a4c82df2ff3e18e9db005d866e7158b"
      "e405cbf70029">>.


events_from_solidity_binary(_Cfg) ->
    Code =  aeu_hex:hexstring_decode(receipt_bytecode()),
    CallData = aeu_hex:hexstring_decode(<<"0xb214faa51700000000000000000000000000000000000000000000000000000000000000">>),
    ContractAddress = 101,
    Env  = initial_state(#{ContractAddress => Code}),
    {ok, NewCode, NewEnv, _} = execute_call(ContractAddress, CallData, Env, #{}),
    Env2 = NewEnv#{ContractAddress => NewCode},
    {ok,_RetVal, _, #{logs := L}} = execute_call(ContractAddress, CallData, Env2, #{}),
    [{<<ContractAddress:256>>,
      [<<25,218,203,248,60,93,230,101,142,20,203,247,188,174,92,
         21,236,162,238,222,207,28,102,251,202,146,142,77,53,27,
         234,15>>,
       <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0>>,
       <<23,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0>>],
      <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0>>}] = L,
    ok.

receipt_bytecode() ->
    %% pragma solidity ^0.4.21;
    %%
    %% contract ClientReceipt {
    %%     event Deposit(
    %%         address indexed _from,
    %%         bytes32 indexed _id,
    %%         uint _value
    %%     );
    %%
    %%     function deposit(bytes32 _id) public payable {
    %%         // Events are emitted using `emit`, followed by
    %%         // the name of the event and the arguments
    %%         // (if any) in parentheses. Any such invocation
    %%         // (even deeply nested) can be detected from
    %%         // the JavaScript API by filtering for `Deposit`.
    %%         emit Deposit(msg.sender, _id, msg.value);
    %%     }
    %% }
    <<"0x6080604052348015600f57600080fd5b5060e88061001e6000396000f300608060"
      "405260043610603f576000357c0100000000000000000000000000000000000000"
      "000000000000000000900463ffffffff168063b214faa5146044575b600080fd5b"
      "606460048036038101908080356000191690602001909291905050506066565b00"
      "5b80600019163373ffffffffffffffffffffffffffffffffffffffff167f19dacb"
      "f83c5de6658e14cbf7bcae5c15eca2eedecf1c66fbca928e4d351bea0f34604051"
      "8082815260200191505060405180910390a3505600a165627a7a72305820ae5aa9"
      "3cc8c862b2660f25285f4c9fee0bd9751471be450c4d49ce6e9793f61c0029">>.

execute_counter_fun_from_bytecode(_Cfg) ->
    Code = aeu_hex:hexstring_decode(counter_bytecode()),
    CallData =  aeu_hex:hexstring_decode(<< "0x6d4ce63c" >>),
    Env  = initial_state(#{101 => Code}),
    {ok, NewCode, NewEnv, _} = execute_call(101, CallData, Env, #{}),
    {ok, _, _, _} = execute_call(101, CallData, NewEnv#{ 101 => NewCode}, #{}),
    ok.


counter_bytecode() ->
    %% pragma solidity ^0.4.0;
    %% contract Counter {
    %%   uint32 public value;
    %%
    %%    constructor() public {
    %%        value = 0;
    %%    }
    %%
    %%    function inc(uint32 x) public {
    %%       value = value + x;
    %%      }
    %%
    %%     function get() public constant returns (uint32) {
    %%         return value;
    %%     }
    %%  }


      <<"0x608060405234801561001057600080fd5b5060008060006101000a81548163"
        "ffffffff021916908363ffffffff16021790555061018d8061004160003960"
        "00f300608060405260043610610057576000357c0100000000000000000000"
        "000000000000000000000000000000000000900463ffffffff1680633fa4f2"
        "451461005c5780636d4ce63c14610093578063dd5d5211146100ca575b6000"
        "80fd5b34801561006857600080fd5b506100716100fd565b604051808263ff"
        "ffffff1663ffffffff16815260200191505060405180910390f35b34801561"
        "009f57600080fd5b506100a8610112565b604051808263ffffffff1663ffff"
        "ffff16815260200191505060405180910390f35b3480156100d657600080fd"
        "5b506100fb600480360381019080803563ffffffff16906020019092919050"
        "505061012b565b005b6000809054906101000a900463ffffffff1681565b60"
        "008060009054906101000a900463ffffffff16905090565b80600080905490"
        "6101000a900463ffffffff16016000806101000a81548163ffffffff021916"
        "908363ffffffff160217905550505600a165627a7a72305820d465419d8b4c"
        "7adf48551bcf6e438080d2c45e27489fc706002b4c638d420ebd0029">>.



%% ------------------------------------------------------------------------
%% Helper Functions
%% ------------------------------------------------------------------------

execute_call(Contract, CallData, ChainState, Options) ->
    #{Contract := Code} = ChainState,
    ChainState1 = ChainState#{ running => Contract },
    Trace = true,
    Res = do_execute_call(
          maps:merge(
          #{ code              => Code,
             store             => aect_contracts_store:new(),
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
             vm_version        => ?VM_AEVM_SOLIDITY_1,
             abi_version       => ?ABI_SOLIDITY_1}, Options),
          Trace),
    case Res of
        {ok, #{ out := RetVal, chain_state := S } = ResRec} ->
            {ok, RetVal, S, ResRec};
        {revert, RRes}  -> {revert, RRes};
        Err = {error, _, _}      -> Err
    end.

do_execute_call(#{ code := Code
              , store := Store
              , address := Address
              , caller := Caller
              , data := CallData
              , gas := Gas
              , gasPrice := GasPrice
              , origin := Origin
              , value := Value
              , currentCoinbase := CoinBase
              , currentDifficulty := Difficulty
              , currentGasLimit := GasLimit
              , currentNumber := Number
              , currentTimestamp := TS
              , chainState := ChainState
              , chainAPI := ChainAPI
              , vm_version := VmVersion
              , abi_version := ABIVersion
              } = CallDef, Trace) ->
    %% TODO: Handle Contract In State.
    Spec =
        #{ exec => #{ code => Code
                    , store => Store
                    , address => Address
                    , caller => Caller
                    , data => CallData
                    , call_data_type => maps:get(call_data_type, CallDef, undefined)
                    , out_type => maps:get(out_type, CallDef, undefined)
                    , gas => Gas
                    , gasPrice => GasPrice
                    , origin => Origin
                    , value => Value
                    , creator => undefined %% Not in solidity VM
                    },
           env => #{ currentCoinbase => CoinBase
                   , currentDifficulty => Difficulty
                   , currentGasLimit => GasLimit
                   , currentNumber => Number
                   , currentTimestamp => TS
                   , chainState => ChainState
                   , chainAPI => ChainAPI
                   , protocol_version => ?ROMA_PROTOCOL_VSN
                   , vm_version => VmVersion
                   , abi_version => ABIVersion
                   },
           pre => #{}},
    TraceSpec =
        #{ trace_fun =>
               fun(S,A) -> lager:debug(S,A) end
         , trace => Trace
         },
    State = aevm_eeevm_state:init(Spec, TraceSpec),
    Result = aevm_eeevm:eval(State),
    Result.

make_call(Contract, Fun, Args, Env, Options) ->
    CallData = encode_calldata(list_to_binary(atom_to_list(Fun)), Args),
    execute_call(Contract, CallData, Env, Options).

encode_calldata(Function, Argument) ->
    {ok, <<Function/binary, Argument/binary>>}.

successful_call_(Contract, Type, Fun, Args, Env) ->
    {Res, _Env1} = successful_call(Contract, Type, Fun, Args, Env),
    Res.

successful_call(Contract, Type, Fun, Args, Env) ->
    successful_call(Contract, Type, Fun, Args, Env, #{}).

successful_call(Contract,_Type, Fun, Args, Env, Options) ->
    case make_call(Contract, Fun, Args, Env, Options) of
        {ok, Result, Env1, _} -> {Result, Env1};
        {error, Err, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit({error, Err})
    end.


%% -- Chain API implementation -----------------------------------------------



initial_state(Contracts) ->
    initial_state(Contracts, #{}).

initial_state(Contracts, Accounts) ->
    maps:merge(#{environment => #{}, store => #{}},
        maps:merge(Contracts, #{accounts => Accounts})).

spend(_To, Amount, _) when Amount < 0 ->
    {error, negative_spend};
spend(ToId, Amount, S = #{ running := From, accounts := Accounts }) ->
    <<To:256>> = aeser_id:specialize(ToId, account),
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
        undefined -> #{};
        _         -> Data
    end.

set_store(Data, State = #{ running := Contract, store := Store }) ->
    State#{ store => Store#{ Contract => Data } }.

call_contract(<<Contract:256>>, Gas, Value, CallData, _, _, S = #{running := Caller}) ->
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

oracle_register(PubKey = <<Account:256>>, <<Sign:256>>, QueryFee, TTL, QueryFormat, ResponseFormat, State) ->
    io:format("oracle_register(~p, ~p, ~p, ~p, ~p, ~p)\n", [Account, Sign, QueryFee, TTL, QueryFormat, ResponseFormat]),
    Oracles = maps:get(oracles, State, #{}),
    State1 = State#{ oracles => Oracles#{ Account =>
                        #{sign            => Sign,
                          nonce           => 1,
                          query_fee       => QueryFee,
                          query_format    => QueryFormat,
                          response_format => ResponseFormat,
                          ttl             => TTL} } },
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

oracle_extend(<<Oracle:256>>,_Sign, TTL, State) ->
    io:format("oracle_extend(~p, ~p, ~p)\n", [Oracle,_Sign, TTL]),
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

oracle_query_response_ttl(<<_Oracle:256>>, <<Query:256>>, State) ->
    case maps:get(oracle_queries, State, #{}) of
        #{Query := Q} -> {ok, maps:get(r_ttl, Q)};
        _ -> {error, no_such_oracle_query}
    end.

oracle_query_format(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, []) of
        #{ Oracle := #{query_format := Format} } -> {ok, Format};
        _ -> {error, {no_such_oracle, Oracle}}
    end.

oracle_response_format(<<Oracle:256>>, State) ->
    case maps:get(oracles, State, #{}) of
        #{ Oracle := #{response_format := Format} } -> {ok, Format};
        _ -> {error, {no_such_oracle, Oracle}}
    end.
