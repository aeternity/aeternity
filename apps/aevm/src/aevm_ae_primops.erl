%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Handle interaction with the aeternity chain
%%%     through calls to AEternity primitive operations at address 0.
%%% @end
%%% Created : 22 May 2018
%%%-------------------------------------------------------------------

-module(aevm_ae_primops).
-export([ call/4
        , check_type_hash/4
        , is_local_primop/1
        , types/4
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("aevm_ae_primops.hrl").

-record(chain, {api, state, abi_version, vm_version}).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-define(ST, erlang:get_stacktrace()).
-else.
-define(TEST_LOG(Format, Data), ok).
-define(ST, ok).
-endif.

-spec call(Gas::non_neg_integer(), Value :: non_neg_integer(), Data::binary(), StateIn) ->
                  {ok, binary(), GasSpent :: non_neg_integer(), StateOut} |
                  {primop_error, Reason} when
      State       :: aevm_eeevm_state:state(),
      StateIn     :: State,
      StateOut    :: State,
      Reason      :: ?AEVM_PRIMOP_ERR_REASON_OOG(OogResource, OogGas, State) | any(),
      OogResource :: any(),
      OogGas      :: pos_integer().
%% Wrapper function for logging error in tests.
call(Gas, Value, Data, State) ->
    case call_(Gas, Value, Data, State) of
        {ok, {ok, Return}, GasUsed, State1} ->
            {ok, Return, GasUsed, State1};
        {ok, {error, Return},_GasUsed,_State1} ->
            {primop_error, Return};
        {error, Return} ->
            {primop_error, Return}
    end.

call_(Gas, Value, Data, StateIn) ->
    try
        PrimOp = get_primop(Data),
        BaseGas = aec_governance:primop_base_gas(PrimOp),
        InAuth  = undefined /= aevm_eeevm_state:auth_tx_hash(StateIn),
        case BaseGas =< Gas of
            false ->
                {error, ?AEVM_PRIMOP_ERR_REASON_OOG({base, PrimOp}, BaseGas, StateIn)};
            true when ?PRIM_CALL_IN_MAP_RANGE(PrimOp) ->
                %% Map primops need the full state
                map_call(Gas, PrimOp, Value, Data, StateIn);
            true when ?PRIM_CALL_IN_CRYPTO_RANGE(PrimOp) ->
                crypto_call(Gas, PrimOp, Value, Data, StateIn);
            true when ?PRIM_CALL_IN_AUTH_RANGE(PrimOp) ->
                auth_call(Gas, PrimOp, Value, Data, StateIn);
            true when InAuth ->
                {error, ?AEVM_PRIMOP_ERR_REASON_OOG({base, PrimOp}, BaseGas, StateIn)};
            true ->
                ChainIn = #chain{api = aevm_eeevm_state:chain_api(StateIn),
                                 state = aevm_eeevm_state:chain_state(StateIn),
                                 abi_version = aevm_eeevm_state:abi_version(StateIn),
                                 vm_version  = aevm_eeevm_state:vm_version(StateIn)
                                },
                case call_primop(PrimOp, Value, Data, ChainIn) of
                    {error, _} = Err ->
                        Err;
                    {ok, DynamicGas, Cb} ->
                        case (BaseGas + DynamicGas) =< Gas of
                            false ->
                                {error, ?AEVM_PRIMOP_ERR_REASON_OOG({dyn, PrimOp}, DynamicGas, StateIn)};
                            true ->
                                case Cb() of
                                    {error, _} = Err ->
                                        Err;
                                    {ok, CbReturnValue, CbChainStateOut} ->
                                        StateOut = aevm_eeevm_state:set_chain_state(CbChainStateOut, StateIn),
                                        {ok, CbReturnValue, BaseGas + DynamicGas, StateOut}
                                end
                        end
                end
        end
    of
        {ok, _, _, _} = Ok ->
            Ok;
        {error, _} = OuterErr ->
            OuterErr
    catch _T:_Err ->
            _ST = ?ST,
            ?TEST_LOG("Primop illegal call ~p:~p:~p~n~p:~p(~p, ~p, State)",
                      [_T, _Err,_ST,
                       ?MODULE, ?FUNCTION_NAME, Value, Data]),
            %% TODO: Better error for illegal call.
            {error, illegal_primop_call}
    end.

call_primop(?PRIM_CALL_SPEND, Value, Data, State) ->
    spend_call(Value, Data, State);
call_primop(PrimOp, Value, Data, State)
  when ?PRIM_CALL_IN_ORACLE_RANGE(PrimOp) ->
    oracle_call(PrimOp, Value, Data, State);
call_primop(PrimOp, Value, Data, State)
  when ?PRIM_CALL_IN_AENS_RANGE(PrimOp) ->
    aens_call(PrimOp, Value, Data, State);
call_primop(PrimOp, Value, Data, State)
  when ?PRIM_CALL_IN_ADDRESS_RANGE(PrimOp) ->
     address_call(PrimOp, Value, Data, State).

is_local_primop(Data) ->
    aeb_primops:is_local_primop_op(get_primop(Data)).

%% ------------------------------------------------------------------
%% Primop types
%% ------------------------------------------------------------------

%% This is used to find the argument and return types for primops.
%% For some of them, we need to dig around in the actual argument to
%% find the types, and for some we need to know what types for which
%% the argument was built.

types(?PRIM_CALL_AENS_CLAIM,_HeapValue,_Store,_State) ->
    {[word, string, word, sign_t()], tuple0_t()};
types(?PRIM_CALL_AENS_PRECLAIM,_HeapValue,_Store,_State) ->
    {[word, word, sign_t()], tuple0_t()};
types(?PRIM_CALL_AENS_RESOLVE, HeapValue, Store,_State) ->
    %% The out type is given in the third argument
    T = {tuple, [word, word, word, typerep]},
    {ok, Bin} = aevm_data:heap_to_binary(T, Store, HeapValue),
    {ok, {_Prim, _, _, OutType}} = aeb_heap:from_binary(T, Bin),
    {[string, string, typerep], option_t(OutType)};
types(?PRIM_CALL_AENS_REVOKE,_HeapValue,_Store, State) ->
    case aevm_eeevm_state:vm_version(State) < ?VM_AEVM_SOPHIA_4 of
        true ->
            {[word, word, sign_t()], tuple0_t()};
        false ->
            {[word, string, sign_t()], tuple0_t()}
    end;
types(?PRIM_CALL_AENS_TRANSFER,_HeapValue,_Store, State) ->
    case aevm_eeevm_state:vm_version(State) < ?VM_AEVM_SOPHIA_4 of
        true ->
            {[word, word, word, sign_t()], tuple0_t()};
        false ->
            {[word, word, string, sign_t()], tuple0_t()}
    end;
%%types(?PRIM_CALL_AENS_UPDATE,_HeapValue,_Store,_State) ->
%%TODO: Not implemented
types(?PRIM_CALL_MAP_DELETE,_HeapValue,_Store,_State) ->
    {[word, word], word};
types(?PRIM_CALL_MAP_EMPTY,_HeapValue,_Store,_State) ->
    {[typerep, typerep], word};
types(?PRIM_CALL_MAP_GET, HeapValue, Store, State) ->
    T = {tuple, [word, word]},
    {ok, Bin} = aevm_data:heap_to_binary(T, Store, HeapValue),
    {ok, {_Prim, Id}} = aeb_heap:from_binary(T, Bin),
    {_KeyType, ValType} = aevm_eeevm_maps:map_type(Id, State),
    {[word, word], option_t(ValType)};
types(?PRIM_CALL_MAP_PUT,_HeapValue,_Store,_State) ->
    {[word, word, word], word};
types(?PRIM_CALL_MAP_SIZE,_HeapValue,_Store,_State) ->
    {[word], word};
types(?PRIM_CALL_MAP_TOLIST, HeapValue, Store, State) ->
    T = {tuple, [word, word]},
    {ok, Bin} = aevm_data:heap_to_binary(T, Store, HeapValue),
    {ok, {_Prim, Id}} = aeb_heap:from_binary(T, Bin),
    {KeyType, ValType} = aevm_eeevm_maps:map_type(Id, State),
    {[word], {list, {tuple, [KeyType, ValType]}}};
types(?PRIM_CALL_ORACLE_EXTEND,_HeapValue,_Store,_State) ->
    {[word, sign_t(), ttl_t()], tuple0_t()};
types(?PRIM_CALL_ORACLE_GET_ANSWER, HeapValue, Store, State) ->
    case oracle_response_type_from_chain(HeapValue, Store, State) of
        {ok, RType} ->
            {[word, word], option_t(RType)};
        {error, _} ->
            {[], tuple0_t()}
    end;
types(?PRIM_CALL_ORACLE_GET_QUESTION, HeapValue, Store, State) ->
    case oracle_query_type_from_chain(HeapValue, Store, State) of
        {ok, QType} ->
            {[word, word], QType};
        {error, _} ->
            {[], tuple0_t()}
    end;
types(?PRIM_CALL_ORACLE_QUERY, HeapValue, Store, State) ->
    case oracle_query_type_from_chain(HeapValue, Store, State) of
        {ok, QType} ->
            {[word, QType, ttl_t(), ttl_t()], word};
        {error, _} ->
            {[], tuple0_t()}
    end;
types(?PRIM_CALL_ORACLE_QUERY_FEE,_HeapValue,_Store,_State) ->
    {[word], word};
types(?PRIM_CALL_ORACLE_REGISTER,_HeapValue,_Store,_State) ->
    {[word, sign_t(), word, ttl_t(), typerep, typerep], word};
types(?PRIM_CALL_ORACLE_RESPOND, HeapValue, Store, State) ->
    case oracle_response_type_from_chain(HeapValue, Store, State) of
        {ok, RType} ->
            {[word, word, sign_t(), RType], tuple0_t()};
        {error, _} ->
            {[], tuple0_t()}
    end;
types(?PRIM_CALL_ORACLE_CHECK,_HeapValue,_Store,_State) ->
    {[word, typerep, typerep], word};
types(?PRIM_CALL_ORACLE_CHECK_QUERY,_HeapValue,_Store,_State) ->
    {[word, word, typerep, typerep], word};
types(?PRIM_CALL_SPEND,_HeapValue,_Store,_State) ->
    {[word], tuple0_t()};
types(?PRIM_CALL_CRYPTO_ECVERIFY, _HeapValue, _Store, _State) ->
    {[word, word, sign_t()], word};
types(?PRIM_CALL_CRYPTO_ECVERIFY_SECP256K1, _HeapValue, _Store, _State) ->
    {[hash_t(), bytes_t(64), bytes_t(64)], word};
types(?PRIM_CALL_CRYPTO_SHA3, _HeapValue, _Store, _State) ->
    {[typerep, word], word};
types(?PRIM_CALL_CRYPTO_SHA256, _HeapValue, _Store, _State) ->
    {[typerep, word], word};
types(?PRIM_CALL_CRYPTO_BLAKE2B, _HeapValue, _Store, _State) ->
    {[typerep, word], word};
types(?PRIM_CALL_CRYPTO_SHA256_STRING, _HeapValue, _Store, _State) ->
    {[string], word};
types(?PRIM_CALL_CRYPTO_BLAKE2B_STRING, _HeapValue, _Store, _State) ->
    {[string], word};
types(?PRIM_CALL_AUTH_TX_HASH, _HeapValue, _Store, _State) ->
    {[], option_t(word)};
types(?PRIM_CALL_ADDR_IS_CONTRACT, _HeapValue, _Store, _State) ->
    {[word], word};
types(?PRIM_CALL_ADDR_IS_ORACLE, _HeapValue, _Store, _State) ->
    {[word], word};
types(_, _, _, _) ->
    {[], tuple0_t()}.

tuple0_t() ->
    {tuple, []}.

ttl_t() ->
    {variant, [[word], [word]]}.

option_t(T) ->
    {variant, [[], [T]]}.

oracle_response_type_from_chain(HeapValue, Store, State) ->
    oracle_type_from_chain(HeapValue, Store, State, response).

oracle_query_type_from_chain(HeapValue, Store, State) ->
    oracle_type_from_chain(HeapValue, Store, State, query).

oracle_type_from_chain(HeapValue, Store, State, Which) ->
    T = {tuple, [word, word]},
    {ok, Bin} = aevm_data:heap_to_binary(T, Store, HeapValue),
    {ok, {_Prim, OracleID}} = aeb_heap:from_binary(T, Bin),
    API        = aevm_eeevm_state:chain_api(State),
    ChainState = aevm_eeevm_state:chain_state(State),
    case Which of
        query    -> API:oracle_query_format(<<OracleID:256>>, ChainState);
        response -> API:oracle_response_format(<<OracleID:256>>, ChainState)
    end.

check_type_hash(Op, ArgTypes, OutType, TypeHash) ->
    case aeb_primops:op_needs_type_check(Op) of
        false ->
            ok;
        true ->
            PrimBin = binary:encode_unsigned(Op),
            ArgType = {tuple, ArgTypes},
            case aeb_aevm_abi:function_type_hash(PrimBin, ArgType, OutType) of
                TypeHash -> ok;
                _Other -> error
            end
    end.

%% ------------------------------------------------------------------
%% Basic account operations.
%% ------------------------------------------------------------------

spend_call(Value, Data, #chain{api = API, state = State} = Chain) ->
    [Recipient] = get_args([word], Data),
    %% TODO: This assumes that we are spending to an account
    RecipientId = aeser_id:create(account, <<Recipient:256>>),
    {ok, Tx} = API:spend_tx(RecipientId, Value, State),
    Callback = fun(ChainAPI, ChainState) ->
                       ChainAPI:spend(Tx, ChainState) end,
    no_dynamic_gas(fun() -> cast_chain(Callback, Chain) end).

%% ------------------------------------------------------------------
%% Oracle operations.
%% ------------------------------------------------------------------

oracle_call(?PRIM_CALL_ORACLE_REGISTER, Value, Data, State) ->
    oracle_call_register(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_QUERY, Value, Data, State) ->
    oracle_call_query(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_RESPOND, Value, Data, State) ->
    oracle_call_respond(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_EXTEND, Value, Data, State) ->
    oracle_call_extend(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_ANSWER, Value, Data, State) ->
    oracle_call_get_answer(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_QUESTION, Value, Data, State) ->
    oracle_call_get_question(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_QUERY_FEE, Value, Data, State) ->
    oracle_call_query_fee(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_CHECK, Value, Data, State = #chain{ vm_version = VM })
        when ?IS_AEVM_SOPHIA(VM), VM >= ?VM_AEVM_SOPHIA_3 ->
    oracle_call_check(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_CHECK_QUERY, Value, Data, State = #chain{ vm_version = VM })
        when ?IS_AEVM_SOPHIA(VM), VM >= ?VM_AEVM_SOPHIA_3 ->
    oracle_call_check_query(Value, Data, State);
oracle_call(PrimOp, _, _, _) ->
    {error, {illegal_oracle_primop_call, PrimOp}}.

call_chain1(Callback, State) ->
    Callback(State#chain.api, State#chain.state).

query_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, Res} ->
            Return = {ok, aeb_heap:to_binary(Res)},
            {ok, Return, State#chain.state};
        {error, _} = Err -> Err
    end.

cast_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, ChainState1} ->
            UnitReturn = {ok, <<0:256>>},
            {ok, UnitReturn, ChainState1};
        {error, _} = Err -> Err
    end.

call_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, Retval, ChainState1} ->
            Return     = {ok, aeb_heap:to_binary(Retval)},
            {ok, Return, ChainState1};
        {error, _} = Err -> Err
    end.

%% Sophia representation of aeo_oracles:ttl().
oracle_ttl_t() ->
    {variant_t, [{delta, [word]}, {block, [word]}]}. %% `word` decoded as non-negative integer.

sign_t() -> bytes_t(64).
hash_t() -> bytes_t(32).

bytes_t(Len) when Len =< 32 -> word;
bytes_t(Len)                -> {tuple, lists:duplicate((Len + 31) div 32, word)}.

oracle_call_register(_Value, Data,
                     #chain{api = API, state = State, abi_version = ABIVersion} = Chain) ->
    ArgumentTypes = [word, sign_t(), word, oracle_ttl_t(), typerep, typerep],
    [Acct, Sign0, QFee, TTL, QFormat, RFormat] = get_args(ArgumentTypes, Data),
    %% The aeo_register_tx expects the formats to be binary, althought, atoms are provided.
    %% This should be solved in a different ticket.
    case chain_ttl_delta(TTL, Chain) of
        {error, _} = Err -> Err;
        {ok, DeltaTTL = {delta, _}} ->
            case API:oracle_register_tx(<<Acct:256>>, QFee, TTL, QFormat,
                                        RFormat, ABIVersion, State) of
                {error, _} = Err -> Err;
                {ok, Tx} ->
                    Callback =
                        fun(ChainAPI, ChainState) ->
                            case ChainAPI:oracle_register(Tx, to_sign(Sign0), ChainState) of
                                {ok, <<OKey:256>>, ChainState1} -> {ok, OKey, ChainState1};
                                {error, _} = Err                -> Err
                            end
                        end,
                    SizeGas = size_gas(Tx),
                    StateGas = state_gas(oracle_register_tx, DeltaTTL),
                    DynGas = SizeGas + StateGas,
                    ?TEST_LOG("~s computed gas ~p - size gas: ~p, state gas: ~p (relative TTL: ~p)",
                                [?FUNCTION_NAME, DynGas, SizeGas, StateGas, DeltaTTL]),
                    {ok, DynGas, fun() -> call_chain(Callback, Chain) end}
            end
    end.

oracle_call_query(Value, Data, #chain{api = API, state = State} = Chain) ->
    [Oracle]  = get_args([word], Data),  %% We need the oracle address before we can decode the query
    case API:oracle_query_format(<<Oracle:256>>, State) of
        {ok, QFormat} ->
            ArgumentTypes = [word, QFormat, oracle_ttl_t(), oracle_ttl_t()],
            [Oracle, Q, QTTL, RTTL] = get_args(ArgumentTypes, Data),
            case chain_ttl_delta(QTTL, Chain) of
                {error, _} = Err -> Err;
                {ok, DeltaQTTL = {delta, _}} ->
                    case API:oracle_query_tx(<<Oracle:256>>, Q, _QFee=Value, QTTL, RTTL, State) of
                        {error, _} = Err -> Err;
                        {ok, Tx} ->
                            Callback =
                                fun(ChainAPI, ChainState) ->
                                    case ChainAPI:oracle_query(Tx, ChainState) of
                                        {ok, <<QKey:256>>, ChainState1} -> {ok, QKey, ChainState1};
                                        {error, _} = Err                -> Err
                                    end
                                end,
                            SizeGas = size_gas(Tx),
                            StateGas = state_gas(oracle_query_tx, DeltaQTTL),
                            DynGas = SizeGas + StateGas,
                            ?TEST_LOG("~s computed gas ~p - size gas: ~p, state gas: ~p (relative TTL: ~p)",
                                        [?FUNCTION_NAME, DynGas, SizeGas, StateGas, DeltaQTTL]),
                            {ok, DynGas, fun() -> call_chain(Callback, Chain) end}
                    end
            end;
        {error, _} = Err -> Err
    end.

oracle_call_respond(_Value, Data, #chain{api = API, state = State} = Chain) ->
    [Oracle, QueryId] = get_args([word, word], Data),
    OracleKey = <<Oracle:256>>,
    QueryKey = <<QueryId:256>>,
    case API:oracle_query_response_ttl(OracleKey, QueryKey, State) of
        {error, _} = Err -> Err;
        {ok, RTTL} ->
            case chain_ttl_delta(RTTL, Chain) of
                {error, _} = Err -> Err;
                {ok, DeltaRTTL = {delta, _}} ->
                    case API:oracle_response_format(OracleKey, State) of
                        {error, _} = Err -> Err;
                        {ok, RFormat} ->
                            ArgumentTypes = [word, word, sign_t(), RFormat],
                            [_, _, Sign0, R] = get_args(ArgumentTypes, Data),
                            case API:oracle_respond_tx(OracleKey, QueryKey, R, DeltaRTTL, State) of
                                {error, _} = Err -> Err;
                                {ok, Tx} ->
                                    SizeGas = size_gas(Tx),
                                    StateGas = state_gas(oracle_response_tx, DeltaRTTL),
                                    DynGas = SizeGas + StateGas,
                                    ?TEST_LOG("~s computed gas ~p - size gas: ~p, state gas: ~p (relative TTL: ~p)",
                                            [?FUNCTION_NAME, DynGas, SizeGas, StateGas, DeltaRTTL]),
                                    Callback = fun(ChainAPI, ChainState) ->
                                        ChainAPI:oracle_respond(Tx, to_sign(Sign0), ChainState) end,
                                    {ok, DynGas, fun() -> cast_chain(Callback, Chain) end}
                            end
                    end
            end
    end.

oracle_call_extend(_Value, Data, #chain{api = API, state = State} = Chain) ->
    ArgumentTypes = [word, sign_t(), oracle_ttl_t()],
    [Oracle, Sign0, TTL] = get_args(ArgumentTypes, Data),
    case chain_ttl_delta(TTL, Chain) of
        {error, _} = Err -> Err;
        {ok, DeltaTTL = {delta, _}} ->
            case API:oracle_extend_tx(<<Oracle:256>>, TTL, State) of
                {error, _} = Err -> Err;
                {ok, Tx} ->
                    Callback = fun(ChainAPI, ChainState) ->
                                    ChainAPI:oracle_extend(Tx, to_sign(Sign0), ChainState)
                            end,
                    StateGas = state_gas(oracle_extend_tx, DeltaTTL),
                    ?TEST_LOG("~s computed gas ~p - state gas: ~p (relative TTL: ~p)",
                                [?FUNCTION_NAME, StateGas, StateGas, DeltaTTL]),
                    {ok, StateGas, fun() -> cast_chain(Callback, Chain) end}
            end
    end.


oracle_call_get_answer(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_answer(<<O:256>>, <<Q:256>>, ChainState) end,
    no_dynamic_gas(fun() -> query_chain(Callback, State) end).


oracle_call_get_question(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_question(<<O:256>>, <<Q:256>>, ChainState) end,
    no_dynamic_gas(fun() -> query_chain(Callback, State) end).


oracle_call_query_fee(_Value, Data, State) ->
    ArgumentTypes = [word],
    [Oracle] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_query_fee(<<Oracle:256>>, ChainState) end,
    no_dynamic_gas(fun() -> query_chain(Callback, State) end).


oracle_call_check(_Value, Data, State) ->
    [Oracle, QFormat, RFormat] = get_args([word, typerep, typerep], Data),
    Callback = fun(API, ChainState) -> API:oracle_check(<<Oracle:256>>, QFormat, RFormat, ChainState) end,
    no_dynamic_gas(fun() -> query_chain(Callback, State) end).


oracle_call_check_query(_Value, Data, State) ->
    [Oracle, Query, QFormat, RFormat] = get_args([word, word, typerep, typerep], Data),
    Callback = fun(API, ChainState) ->
                   API:oracle_check_query(<<Oracle:256>>, <<Query:256>>, QFormat,
                                          RFormat, ChainState)
               end,
    no_dynamic_gas(fun() -> query_chain(Callback, State) end).

%% ------------------------------------------------------------------
%% AENS operations.
%% ------------------------------------------------------------------

aens_call(?PRIM_CALL_AENS_RESOLVE, _Value, Data, State) ->
    aens_call_resolve(Data, State);
aens_call(?PRIM_CALL_AENS_PRECLAIM, _Value, Data, State) ->
    aens_call_preclaim(Data, State);
aens_call(?PRIM_CALL_AENS_CLAIM, _Value, Data, State) ->
    aens_call_claim(Data, State);
aens_call(?PRIM_CALL_AENS_TRANSFER, _Value, Data, State) ->
    aens_call_transfer(Data, State);
aens_call(?PRIM_CALL_AENS_REVOKE, _Value, Data, State) ->
    aens_call_revoke(Data, State);
aens_call(PrimOp, _, _, _) ->
    {error, {illegal_aens_primop_call, PrimOp}}.

aens_call_resolve(Data, Chain) ->
    [Name, Key, Type] = get_args([string, string, typerep], Data),
    Callback = fun(ChainAPI, ChainState) -> ChainAPI:aens_resolve(Name, Key, Type, ChainState) end,
    no_dynamic_gas(fun() -> query_chain(Callback, Chain) end).

aens_call_preclaim(Data, #chain{api = API, state = State} = Chain) ->
    [Addr, CHash, Sign0] = get_args([word, word, sign_t()], Data),
    case API:aens_preclaim_tx(<<Addr:256>>, <<CHash:256>>, State) of
        {ok, Tx} ->
            SizeGas = size_gas(Tx),
            Callback = fun(ChainAPI, ChainState) -> ChainAPI:aens_preclaim(Tx, to_sign(Sign0), ChainState) end,
            {ok, SizeGas, fun() -> cast_chain(Callback, Chain) end};
        {error, _} = Err -> Err
    end.

aens_call_claim(Data, #chain{api = API, state = State} = Chain) ->
    [Addr, Name, Salt, Sign0] = get_args([word, string, word, sign_t()], Data),
    case API:aens_claim_tx(<<Addr:256>>, Name, Salt, State) of
        {ok, Tx} ->
            SizeGas = size_gas(Tx),
            Callback = fun(ChainAPI, ChainState) -> ChainAPI:aens_claim(Tx, to_sign(Sign0), ChainState) end,
            {ok, SizeGas, fun() -> cast_chain(Callback, Chain) end};
        {error, _} = Err -> Err
    end.

aens_call_transfer(Data, #chain{vm_version = VMVersion} = Chain) when VMVersion < ?VM_AEVM_SOPHIA_4 ->
    [From, To, Hash, Sign] = get_args([word, word, word, sign_t()], Data),
    aens_call_transfer_common(<<From:256>>, <<To:256>>, <<Hash:256>>, to_sign(Sign), Chain);
aens_call_transfer(Data, #chain{} = Chain) ->
    [From, To, Name, Sign] = get_args([word, word, string, sign_t()], Data),
    case aens:get_name_hash(Name) of
        {ok, Hash} ->
            aens_call_transfer_common(<<From:256>>, <<To:256>>, Hash, to_sign(Sign), Chain);
        {error, _} = Err ->
            Err
    end.

aens_call_transfer_common(From, To, Hash, Sign, #chain{api = API, state = State} = Chain) ->
    case API:aens_transfer_tx(From, To, Hash, State) of
        {ok, Tx} ->
            Callback = fun(ChainAPI, ChainState) -> ChainAPI:aens_transfer(Tx, Sign, ChainState) end,
            no_dynamic_gas(fun() -> cast_chain(Callback, Chain) end);
        {error, _} = Err -> Err
    end.

aens_call_revoke(Data, #chain{vm_version = VMVersion} = Chain) when VMVersion < ?VM_AEVM_SOPHIA_4 ->
    [Addr, Hash, Sign] = get_args([word, word, sign_t()], Data),
    aens_call_revoke_common(<<Addr:256>>, <<Hash:256>>, to_sign(Sign), Chain);
aens_call_revoke(Data, #chain{} = Chain) ->
    [Addr, Name, Sign] = get_args([word, string, sign_t()], Data),
    case aens:get_name_hash(Name) of
        {ok, Hash} ->
            aens_call_revoke_common(<<Addr:256>>, Hash, to_sign(Sign), Chain);
        {error, _} = Err -> Err
    end.

aens_call_revoke_common(Addr, Hash, Sign, #chain{api = API, state = State} = Chain) ->
    case API:aens_revoke_tx(Addr, Hash, State) of
        {ok, Tx} ->
            Callback = fun(ChainAPI, ChainState) -> ChainAPI:aens_revoke(Tx, Sign, ChainState) end,
            no_dynamic_gas(fun() -> cast_chain(Callback, Chain) end);
        {error, _} = Err -> Err
    end.


%% ------------------------------------------------------------------
%% Address operations.
%% ------------------------------------------------------------------
address_call(Op, _Value, Data, State = #chain{ vm_version = VMVersion })
        when ?IS_AEVM_SOPHIA(VMVersion), VMVersion >= ?VM_AEVM_SOPHIA_3 ->
    address_call(Op, Data, State);
address_call(_, _, _, _) ->
    {error, out_of_gas}.

address_call(?PRIM_CALL_ADDR_IS_ORACLE, Data, State) ->
    [Addr] = get_args([word], Data),
    Callback = fun(API, ChainState) -> API:addr_is_oracle(<<Addr:256>>, ChainState) end,
    no_dynamic_gas(fun() -> query_chain(Callback, State) end);
address_call(?PRIM_CALL_ADDR_IS_CONTRACT, Data, State) ->
    [Addr] = get_args([word], Data),
    Callback = fun(API, ChainState) -> API:addr_is_contract(<<Addr:256>>, ChainState) end,
    no_dynamic_gas(fun() -> query_chain(Callback, State) end);
address_call(_, _, _) ->
    {error, out_of_gas}.

%% ------------------------------------------------------------------
%% Auth operations.
%% ------------------------------------------------------------------
auth_call(Gas, Op, _Value, Data, State) ->
    case {aevm_eeevm_state:vm_version(State), Op} of
        {VMVersion, _} when ?IS_AEVM_SOPHIA(VMVersion), VMVersion >= ?VM_AEVM_SOPHIA_3 ->
            auth_call(Gas, Op, Data, State);
        {_, _} -> {error, out_of_gas}
    end.

auth_call(Gas, ?PRIM_CALL_AUTH_TX_HASH, Data, State) ->
    auth_call_tx_hash(Gas, Data, State);
auth_call(_, _, _, _) ->
    {error, out_of_gas}.

auth_call_tx_hash(_Gas, _Data, State) ->
    Res =
        case aevm_eeevm_state:auth_tx_hash(State) of
            undefined      -> {ok, aeb_heap:to_binary(none)};
            <<TxHash:256>> -> {ok, aeb_heap:to_binary({some, TxHash})}
        end,
    {ok, Res, aec_governance:primop_base_gas(?PRIM_CALL_AUTH_TX_HASH), State}.

%% ------------------------------------------------------------------
%% Crypto operations.
%% ------------------------------------------------------------------
crypto_call(Gas, Op, _Value, Data, State) ->
    case aevm_eeevm_state:vm_version(State) of
        ?VM_AEVM_SOPHIA_1 ->
            {error, out_of_gas};
        VMVersion when ?IS_AEVM_SOPHIA(VMVersion) andalso
                       VMVersion >= ?VM_AEVM_SOPHIA_2 andalso
                       Op < ?PRIM_CALL_CRYPTO_ECVERIFY_SECP256K1 ->
            crypto_call(Gas, Op, Data, State);
        VMVersion when ?IS_AEVM_SOPHIA(VMVersion) andalso
                       VMVersion >= ?VM_AEVM_SOPHIA_3 ->
            crypto_call(Gas, Op, Data, State)
    end.

crypto_call(Gas, ?PRIM_CALL_CRYPTO_ECVERIFY, Data, State) ->
    crypto_call_ecverify(Gas, Data, State);
crypto_call(Gas, ?PRIM_CALL_CRYPTO_ECVERIFY_SECP256K1, Data, State) ->
    crypto_call_ecverify_secp256k1(Gas, Data, State);
crypto_call(Gas, ?PRIM_CALL_CRYPTO_SHA3, Data, State) ->
    crypto_call_generic_hash(?PRIM_CALL_CRYPTO_SHA3, Gas, Data, State);
crypto_call(Gas, ?PRIM_CALL_CRYPTO_SHA256, Data, State) ->
    crypto_call_generic_hash(?PRIM_CALL_CRYPTO_SHA256, Gas, Data, State);
crypto_call(Gas, ?PRIM_CALL_CRYPTO_BLAKE2B, Data, State) ->
    crypto_call_generic_hash(?PRIM_CALL_CRYPTO_BLAKE2B, Gas, Data, State);
crypto_call(_Gas, ?PRIM_CALL_CRYPTO_SHA256_STRING, Data, State) ->
    crypto_call_string_hash(?PRIM_CALL_CRYPTO_SHA256_STRING, Data, State);
crypto_call(_Gas, ?PRIM_CALL_CRYPTO_BLAKE2B_STRING, Data, State) ->
    crypto_call_string_hash(?PRIM_CALL_CRYPTO_BLAKE2B_STRING, Data, State);
crypto_call(_, _, _, _) ->
    {error, out_of_gas}.

crypto_call_ecrecover_secp256k1(_Gas, Data, State) ->
    [Hash, V, R, S] = get_args([bytes_t(32), bytes_t(32),
				bytes_t(32), bytes_t(32)], Data),
    aeu_crypto:ecrecover(sec256k1, Hash, V, R, S).

crypto_call_ecverify(_Gas, Data, State) ->
    [MsgHash, PK, Sig] = get_args([hash_t(), word, sign_t()], Data),
    Res =
        case enacl:sign_verify_detached(to_sign(Sig), <<MsgHash:256>>, <<PK:256>>) of
            {ok, _}    -> {ok, <<1:256>>};
            {error, _} -> {ok, <<0:256>>}
        end,
    {ok, Res, aec_governance:primop_base_gas(?PRIM_CALL_CRYPTO_ECVERIFY), State}.

crypto_call_ecverify_secp256k1(_Gas, Data, State) ->
    [MsgHash0, Pubkey0, Sig0] = get_args([hash_t(), bytes_t(64), bytes_t(64)], Data),
    MsgHash = <<MsgHash0:32/unit:8>>,
    Pubkey  = words_to_bin(64, Pubkey0),
    Sig     = words_to_bin(64, Sig0),
    Res = case aeu_crypto:ecverify(secp256k1, MsgHash, Pubkey, Sig) of
            true  -> {ok, <<1:256>>};
            false -> {ok, <<0:256>>}
          end,
    {ok, Res, aec_governance:primop_base_gas(?PRIM_CALL_CRYPTO_ECVERIFY_SECP256K1), State}.

crypto_call_generic_hash(PrimOp, Gas, Data, State) ->
    [Type, Ptr] = get_args([typerep, word], Data),
    case aevm_eeevm_state:heap_to_binary(Type, Ptr, State) of
        {ok, Bin, GasUsed} ->
            Hash = case PrimOp of
                       ?PRIM_CALL_CRYPTO_SHA3    -> aec_hash:hash(evm, Bin);
                       ?PRIM_CALL_CRYPTO_SHA256  -> aec_hash:sha256_hash(Bin);
                       ?PRIM_CALL_CRYPTO_BLAKE2B -> aec_hash:blake2b_256_hash(Bin)
                   end,
            {ok, {ok, Hash}, GasUsed + aec_governance:primop_base_gas(PrimOp), State};
        {error, _} = Err ->
            {ok, Err, Gas, State}
    end.

crypto_call_string_hash(PrimOp, Data, State) ->
    [String] = get_args([string], Data),
    Hash = case PrimOp of
               ?PRIM_CALL_CRYPTO_SHA256_STRING  -> aec_hash:sha256_hash(String);
               ?PRIM_CALL_CRYPTO_BLAKE2B_STRING -> aec_hash:blake2b_256_hash(String)
           end,
    {ok, {ok, Hash}, aec_governance:primop_base_gas(PrimOp), State}.

%% ------------------------------------------------------------------
%% Map operations.
%% ------------------------------------------------------------------

map_call(Gas, ?PRIM_CALL_MAP_EMPTY, _Value, Data, State) ->
    map_call_empty(Gas, Data, State);
map_call(Gas, ?PRIM_CALL_MAP_GET, _Value, Data, State) ->
    map_call_get(Gas, Data, State);
map_call(Gas, ?PRIM_CALL_MAP_PUT, _Value, Data, State) ->
    map_call_put(Gas, Data, State);
map_call(Gas, ?PRIM_CALL_MAP_DELETE, _Value, Data, State) ->
    map_call_delete(Gas, Data, State);
map_call(Gas, ?PRIM_CALL_MAP_SIZE, _Value, Data, State) ->
    map_call_size(Gas, Data, State);
map_call(Gas, ?PRIM_CALL_MAP_TOLIST, _Value, Data, State) ->
    map_call_tolist(Gas, Data, State);
map_call(_, _, _, _, _) ->
    {error, out_of_gas}.

map_call_empty(_Gas, Data, State) ->
    [KeyType, ValType] = get_args([typerep, typerep], Data),
    {MapId, State1} = aevm_eeevm_maps:empty(KeyType, ValType, State),
    {ok, {ok, <<MapId:32/unit:8>>}, 0, State1}.

map_call_size(_Gas, Data, State) ->
    [MapId] = get_args([word], Data),
    Size = aevm_eeevm_maps:size(MapId, State),
    {ok, {ok, <<Size:256>>}, 0, State}.

map_call_get(Gas, Data, State) ->
    [MapId]   = get_args([word], Data),
    {KeyType, _ValType} = aevm_eeevm_maps:map_type(MapId, State),
    [_, KeyPtr]  = get_args([word, word], Data),
    case aevm_eeevm_state:heap_to_binary(KeyType, KeyPtr, State) of
        {ok, KeyBin, GasUsed} ->
            Res = case aevm_eeevm_maps:get(MapId, KeyBin, State) of
                    false -> aeb_heap:to_binary(none);
                    <<ValPtr:256, ValBin/binary>> ->
                        %% Some hacky juggling to build an option value.
                        NewPtr = 32 + byte_size(ValBin),
                        <<NewPtr:256, ValBin/binary, 1:256, ValPtr:256>>
                  end,
            {ok, {ok, Res}, GasUsed, State};
        {error, _} = Err ->
            {ok, Err, Gas, State}
    end.

map_call_put(Gas, Data, State) ->
    [MapId]             = get_args([word], Data),
    {KeyType, ValType}  = aevm_eeevm_maps:map_type(MapId, State),
    [_, KeyPtr, ValPtr] = get_args([word, word, word], Data),
    case aevm_eeevm_state:heap_to_binary(KeyType, KeyPtr, State) of
        {ok, KeyBin, GasUsed} ->
            case aevm_eeevm_state:heap_to_heap(ValType, ValPtr, State) of
                {ok, HeapVal, GasUsed1} ->
                    ValBin = <<(aeb_heap:heap_value_pointer(HeapVal)):256,
                               (aeb_heap:heap_value_heap(HeapVal))/binary>>,
                    {NewMapId, State1}  = aevm_eeevm_maps:put(MapId, KeyBin, ValBin, State),
                    {ok, {ok, <<NewMapId:256>>}, GasUsed + GasUsed1, State1};
                {error, _} = Err ->
                    {ok, Err, Gas, State}
            end;
        {error, _} = Err ->
            {ok, Err, Gas, State}
    end.

map_call_delete(Gas, Data, State) ->
    [MapId]      = get_args([word], Data),
    {KeyType, _} = aevm_eeevm_maps:map_type(MapId, State),
    [_, KeyPtr]  = get_args([word, word], Data),
    case aevm_eeevm_state:heap_to_binary(KeyType, KeyPtr, State) of
        {ok, KeyBin, GasUsed} ->
            {NewMapId, State1} = aevm_eeevm_maps:delete(MapId, KeyBin, State),
            {ok, {ok, <<NewMapId:256>>}, GasUsed, State1};
        {error, _} = Err ->
            {ok, Err, Gas, State}
    end.

map_call_tolist(_Gas, Data, State) ->
    [MapId] = get_args([word], Data),
    {KeyType, ValType} = aevm_eeevm_maps:map_type(MapId, State),
    {ok, Map} = aevm_eeevm_maps:get_flat_map(MapId, State),
    List = maps:to_list(Map),
    HeapBin = build_heap_list(aevm_eeevm_state:maps(State), KeyType, ValType, List),
    {ok, {ok, HeapBin}, 0, State}.

build_heap_list(_, _, _, []) -> <<(-1):256>>;
build_heap_list(Maps, KeyType, ValType, KVs) ->
    build_heap_list(Maps, KeyType, ValType, KVs, 32, []).

build_heap_list(_, _, _, [], _, Acc) ->
    <<32:256, (list_to_binary(lists:reverse(Acc)))/binary>>;
build_heap_list(Maps, KeyType, ValType, [{K, V} | KVs], Offs, Acc) ->
    %% Addr:  Offs      Offs + 32  HeadPtr  HeadPtr + 32  KeyPtr    ValPtr  TailPtr
    %% Data:  [HeadPtr] [TailPtr]  [KeyPtr]   [ValPtr]    KeyBin    ValBin  NextConsCell
    HeadPtr      = Offs + 64,
    KeyPtr       = HeadPtr + 64,
    NextId       = 0,   %% There are no maps in map keys
    {ok, KeyVal} = aevm_data:binary_to_heap(KeyType, K, NextId, KeyPtr),
    KeyBin       = aeb_heap:heap_value_heap(KeyVal),
    KeyPtr1      = aeb_heap:heap_value_pointer(KeyVal),
    ValPtr       = KeyPtr + byte_size(KeyBin),
    <<VP:256, VB/binary>> = V,
    {ok, ValVal} = aevm_data:heap_to_heap(ValType, aeb_heap:heap_value(Maps, VP, VB, 32), ValPtr),
    ValPtr1      = aeb_heap:heap_value_pointer(ValVal),
    ValBin       = aeb_heap:heap_value_heap(ValVal),
    TailPtr      =
        case KVs of
            [] -> -1;
            _  -> ValPtr + byte_size(ValBin)
        end,
    ConsCell = <<HeadPtr:256, TailPtr:256>>,
    PairCell = <<KeyPtr1:256, ValPtr1:256>>,
    build_heap_list(Maps, KeyType, ValType, KVs, TailPtr,
                    [[ConsCell, PairCell, KeyBin, ValBin] | Acc]).


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

get_primop(Data) ->
    {ok, T} = aeb_heap:from_binary({tuple, [word]}, Data),
    {PrimOp} = T,
    PrimOp.

get_args(Types, Data) ->
    {ok, V} = aeb_heap:from_binary({tuple, [word | Types]}, Data),
    [_ | Args] = tuple_to_list(V),
    Args.

no_dynamic_gas(Cb) when is_function(Cb, 0) ->
    {ok, _DynGas=0, Cb}.

chain_ttl_delta(TTL, #chain{api = API, state = State}) ->
    ChainHeight = API:get_height(State),
    ttl_delta(ChainHeight, TTL).

ttl_delta(_, {delta, D}) when
      D >= 0 -> %% Guaranteed by caller.
    {ok, {delta, D}};
ttl_delta(CurrHeight, {block, H}) when
      CurrHeight >= 0, %% Guaranteed by chain API.
      H >= 0 -> %% Guaranteed by caller.
    D = H - CurrHeight,
    case D >= 0 of
        true ->
            {ok, {delta, D}};
        false ->
            {error, too_low_abs_ttl}
    end.

size_gas(Tx) ->
    aetx:size(Tx) * aec_governance:byte_gas().

state_gas(Tag, {delta, TTL}) ->
    aec_governance_utils:state_gas(
      aec_governance:state_gas_per_block(Tag),
      TTL).

to_sign(Sig) -> words_to_bin(64, Sig).

words_to_bin(Len, Ws) when is_tuple(Ws) ->
    binary:part(<< <<W:32/unit:8>> || W <- tuple_to_list(Ws) >>, 0, Len).
