%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Handle interaction with the aeternity chain
%%%     through calls to AEternity primitive operations at address 0.
%%% @end
%%% Created : 22 May 2018
%%%-------------------------------------------------------------------

-module(aevm_ae_primops).
-export([call/3]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aevm_ae_primops.hrl").

-record(chain, {api, state}).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-else.
-define(TEST_LOG(Format, Data), ok).
-endif.

-spec call(Value::non_neg_integer(), Data::binary(), StateIn) ->
                  {ok, ReturnValue, GasSpent::non_neg_integer(), StateOut} |
                  {error, Reason} when
      StateIn :: State,
      StateOut :: State,
      State :: aevm_eeevm_state:state(),
      ReturnValue :: {ok, binary()} | {error, any()},
      Reason :: ?AEVM_PRIMOP_ERR_REASON_OOG(OogResource, OogCost, State)
              | any(),
      OogResource :: any(),
      OogCost :: pos_integer().
%% Wrapper function for logging error in tests.
call(Value, Data, State) ->
    case call_(Value, Data, State) of
        {ok, _, _, _} = Ok ->
            Ok;
        {error, _} = Err ->
            ?TEST_LOG("Primop call error ~p~n~p:~p(~p, ~p, State)",
                      [Err, ?MODULE, ?FUNCTION_NAME, Value, Data]),
            Err
    end.

call_(Value, Data, StateIn) ->
    Gas = aevm_eeevm_state:gas(StateIn),
    try
        PrimOp = get_primop(Data),
        BaseCost = aec_governance:primop_base_gas_cost(PrimOp),
        case BaseCost =< Gas of
            false ->
                {error, ?AEVM_PRIMOP_ERR_REASON_OOG({base, PrimOp}, BaseCost, StateIn)};
            true ->
                ChainIn = #chain{api = aevm_eeevm_state:chain_api(StateIn),
                                 state = aevm_eeevm_state:chain_state(StateIn)},
                case call_primop(PrimOp, Value, Data, ChainIn) of
                    {error, _} = Err ->
                        Err;
                    {ok, DynamicCost, Cb} ->
                        case (BaseCost + DynamicCost) =< Gas of
                            false ->
                                {error, ?AEVM_PRIMOP_ERR_REASON_OOG({dyn, PrimOp}, DynamicCost, StateIn)};
                            true ->
                                case Cb() of
                                    {error, _} = Err ->
                                        Err;
                                    {ok, CbReturnValue, CbChainStateOut} ->
                                        {ok, CbReturnValue, BaseCost + DynamicCost, CbChainStateOut}
                                end
                        end
                end
        end
    of
        {ok, ReturnValue, GasSpent, ChainStateOut} ->
            StateOut = aevm_eeevm_state:set_chain_state(ChainStateOut, StateIn),
            {ok, ReturnValue, GasSpent, StateOut};
        {error, _} = OuterErr ->
            OuterErr
    catch _T:_Err ->
            ?TEST_LOG("Primop illegal call ~p:~p:~p~n~p:~p(~p, ~p, State)",
                      [_T, _Err,
                       erlang:get_stacktrace(), %% Absent from non-test bytecode.
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
    aens_call(PrimOp, Value, Data, State).

%% ------------------------------------------------------------------
%% Basic account operations.
%% ------------------------------------------------------------------

spend_call(Value, Data, State) ->
    [Recipient] = get_args([word], Data),
    %% TODO: This assumes that we are spending to an account
    RecipientId = aec_id:create(account, <<Recipient:256>>),
    Callback = fun(API, ChainState) ->
                       API:spend(RecipientId, Value, ChainState) end,
    no_dynamic_cost(fun() -> cast_chain(Callback, State) end).

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
oracle_call(PrimOp, _, _, _) ->
    {error, {illegal_oracle_primop_call, PrimOp}}.

call_chain1(Callback, State) ->
    Callback(State#chain.api, State#chain.state).

query_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, Res} ->
            Return = {ok, aeso_data:to_binary(Res)},
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
            Return     = {ok, aeso_data:to_binary(Retval)},
            {ok, Return, ChainState1};
        {error, _} = Err -> Err
    end.

%% Sophia representation of aeo_oracles:ttl().
oracle_ttl_t() ->
    {variant_t, [{delta, [word]}, {block, [word]}]}. %% `word` decoded as non-negative integer.

sign_t() ->
    {tuple, [word, word]}.

oracle_call_register(_Value, Data, State) ->
    ArgumentTypes = [word, sign_t(), word, oracle_ttl_t(), typerep, typerep],
    [Acct, Sign0, QFee, TTL, QType, RType] = get_args(ArgumentTypes, Data),
    case chain_ttl_delta(TTL, State) of
        {error, _} = Err -> Err;
        {ok, DeltaTTL = {delta, _}} ->
            Callback =
                fun(API, ChainState) ->
                    case API:oracle_register(<<Acct:256>>, to_sign(Sign0), QFee, TTL, QType, RType, ChainState) of
                        {ok, <<OKey:256>>, ChainState1} -> {ok, OKey, ChainState1};
                        {error, _} = Err                -> Err
                    end
                end,
            DynCost = state_gas_cost(oracle_registration, DeltaTTL),
            ?TEST_LOG("~s computed gas cost ~p from relative TTL ~p", [?FUNCTION_NAME, DynCost, DeltaTTL]),
            {ok, DynCost, fun() -> call_chain(Callback, State) end}
    end.

oracle_call_query(Value, Data, State) ->
    [Oracle]  = get_args([word], Data),  %% We need the oracle address before we can decode the query
    OracleKey = <<Oracle:256>>,
    case call_chain1(fun(API, ChainState) -> API:oracle_query_format(OracleKey, ChainState) end, State) of
        {ok, QueryType} ->
            ArgumentTypes = [word, QueryType, oracle_ttl_t(), oracle_ttl_t()],
            [_Oracle, Q, QTTL, RTTL] = get_args(ArgumentTypes, Data),
            case chain_ttl_delta(QTTL, State) of
                {error, _} = Err -> Err;
                {ok, DeltaQTTL = {delta, _}} ->
                    Callback =
                        fun(API, ChainState) ->
                            case API:oracle_query(OracleKey, Q, _QFee=Value, QTTL, RTTL, ChainState) of
                                {ok, <<QKey:256>>, ChainState1} -> {ok, QKey, ChainState1};
                                {error, _} = Err                -> Err
                            end
                        end,
                    DynCost = state_gas_cost(oracle_query, DeltaQTTL),
                    ?TEST_LOG("~s computed gas cost ~p from relative TTL ~p", [?FUNCTION_NAME, DynCost, DeltaQTTL]),
                    {ok, DynCost, fun() -> call_chain(Callback, State) end}
            end;
        {error, _} = Err -> Err
    end.

oracle_call_respond(_Value, Data, State) ->
    [Oracle, Query] = get_args([word, word], Data),
    OracleKey = <<Oracle:256>>,
    QueryKey = <<Query:256>>,
    case call_chain1(fun(API, ChainState) -> API:oracle_query_response_ttl(OracleKey, QueryKey, ChainState) end, State) of
        {error, _} = Err -> Err;
        {ok, RTTL} ->
            case chain_ttl_delta(RTTL, State) of
                {error, _} = Err -> Err;
                {ok, DeltaRTTL = {delta, _}} ->
                    Callback2 =
                        fun() ->
                            case call_chain1(fun(API, ChainState) ->
                                API:oracle_response_format(OracleKey, ChainState) end, State) of
                                {ok, RType} ->
                                    ArgumentTypes = [word, word, sign_t(), RType],
                                    [_, _, Sign0, R] = get_args(ArgumentTypes, Data),
                                    Callback = fun(API, ChainState) ->
                                        API:oracle_respond(OracleKey, QueryKey, to_sign(Sign0), R, ChainState) end,
                                    cast_chain(Callback, State);
                                {error, _} = Err -> Err
                            end
                        end,
                    DynCost = state_gas_cost(oracle_response, DeltaRTTL),
                    ?TEST_LOG("~s computed gas cost ~p from relative TTL ~p", [?FUNCTION_NAME, DynCost, DeltaRTTL]),
                    {ok, DynCost, Callback2}
            end
    end.

oracle_call_extend(_Value, Data, State) ->
    ArgumentTypes = [word, sign_t(), oracle_ttl_t()],
    [Oracle, Sign0, TTL] = get_args(ArgumentTypes, Data),
    case chain_ttl_delta(TTL, State) of
        {error, _} = Err -> Err;
        {ok, DeltaTTL = {delta, _}} ->
            Callback = fun(API, ChainState) -> API:oracle_extend(<<Oracle:256>>, to_sign(Sign0), TTL, ChainState) end,
            DynCost = state_gas_cost(oracle_extension, DeltaTTL),
            ?TEST_LOG("~s computed gas cost ~p from relative TTL ~p", [?FUNCTION_NAME, DynCost, DeltaTTL]),
            {ok, DynCost, fun() -> cast_chain(Callback, State) end}
    end.


oracle_call_get_answer(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_answer(<<O:256>>, <<Q:256>>, ChainState) end,
    no_dynamic_cost(fun() -> query_chain(Callback, State) end).


oracle_call_get_question(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_question(<<O:256>>, <<Q:256>>, ChainState) end,
    no_dynamic_cost(fun() -> query_chain(Callback, State) end).


oracle_call_query_fee(_Value, Data, State) ->
    ArgumentTypes = [word],
    [Oracle] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_query_fee(<<Oracle:256>>, ChainState) end,
    no_dynamic_cost(fun() -> query_chain(Callback, State) end).

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

aens_call_resolve(Data, State) ->
    [Name, Key, Type] = get_args([string, string, typerep], Data),
    Callback = fun(API, ChainState) -> API:aens_resolve(Name, Key, Type, ChainState) end,
    no_dynamic_cost(fun() -> query_chain(Callback, State) end).

aens_call_preclaim(Data, State) ->
    [Addr, CHash, Sign] = get_args([word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_preclaim(<<Addr:256>>, <<CHash:256>>, <<Sign:256>>, ChainState) end,
    no_dynamic_cost(fun() -> cast_chain(Callback, State) end).

aens_call_claim(Data, State) ->
    [Addr, Name, Salt, Sign] = get_args([word, string, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_claim(<<Addr:256>>, Name, Salt, <<Sign:256>>, ChainState) end,
    no_dynamic_cost(fun() -> cast_chain(Callback, State) end).

aens_call_transfer(Data, State) ->
    [From, To, Hash, Sign] = get_args([word, word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_transfer(<<From:256>>, <<To:256>>, <<Hash:256>>, <<Sign:256>>, ChainState) end,
    no_dynamic_cost(fun() -> cast_chain(Callback, State) end).

aens_call_revoke(Data, State) ->
    [Addr, Hash, Sign] = get_args([word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_revoke(<<Addr:256>>, <<Hash:256>>, <<Sign:256>>, ChainState) end,
    no_dynamic_cost(fun() -> cast_chain(Callback, State) end).

%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

get_primop(Data) ->
    {ok, T} = aeso_data:from_binary({tuple, [word]}, Data),
    {PrimOp} = T,
    PrimOp.

get_args(Types, Data) ->
    {ok, Val} = aeso_data:from_binary({tuple, [word | Types]}, Data),
    [_ | Args] = tuple_to_list(Val),
    Args.

no_dynamic_cost(Cb) when is_function(Cb, 0) ->
    {ok, _DynCost=0, Cb}.

chain_ttl_delta(TTL, State) ->
    ChainHeight = call_chain1(fun(API, ChainState) -> API:get_height(ChainState) end, State),
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

state_gas_cost(Tag, {delta, TTL}) ->
    aec_governance_utils:state_gas_cost(
      aec_governance:state_gas_cost_per_block(Tag),
      TTL).

to_sign({W1, W2}) ->
    <<W1:256, W2:256>>.
