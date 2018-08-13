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

-define(BASE_ADDRESS, 32). %% Byte offset for data

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

-spec call( non_neg_integer(), binary(), aevm_eeevm_state:state()) ->
                  {ok, binary(), non_neg_integer(), aevm_eeevm_state:state()}
                      | {error, any()}.
call(Value, Data, State) ->
    case call_(Value, Data, State) of
        {ok, _, _, _} = Ok ->
            Ok;
        {error, _} = Err ->
            ?TEST_LOG("Primop call error ~p~n~p:~p(~p, ~p, State)",
                      [Err, ?MODULE, ?FUNCTION_NAME, Value, Data]),
            Err
    end.

call_(Value, Data, State) ->
    try
        case get_primop(Data) of
            ?PRIM_CALL_SPEND ->
                spend_call(Value, Data, State);
            PrimOp when ?PRIM_CALL_IN_ORACLE_RANGE(PrimOp) ->
                oracle_call(PrimOp, Value, Data, State);
            PrimOp when ?PRIM_CALL_IN_AENS_RANGE(PrimOp) ->
                aens_call(PrimOp, Value, Data, State)
        end
    catch _T:_Err ->
        ?TEST_LOG("Primop illegal call ~p:~p:~p~n~p:~p(~p, ~p, State)",
                  [_T, _Err,
                   erlang:get_stacktrace(), %% Absent from non-test bytecode.
                   ?MODULE, ?FUNCTION_NAME, Value, Data]),
	%% TODO: Better error for illegal call.
        {error, out_of_gas}
    end.

%% ------------------------------------------------------------------
%% Basic account operations.
%% ------------------------------------------------------------------

spend_call(Value, Data, State) ->
    [Recipient] = get_args([word], Data),
    %% TODO: This assumes that we are spending to an account
    RecipientId = aec_id:create(account, <<Recipient:256>>),
    Callback = fun(API, ChainState) -> API:spend(RecipientId, Value, ChainState) end,
    call_chain(Callback, State).

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
oracle_call(_, _, _, _) ->
    {error, out_of_gas}.

call_chain1(Callback, State) ->
    ChainAPI   = aevm_eeevm_state:chain_api(State),
    ChainState = aevm_eeevm_state:chain_state(State),
    Callback(ChainAPI, ChainState).

query_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, Res} ->
            Return = {ok, aeso_data:to_binary(Res, 0)},
            {ok, Return, 0, State};
        {error, _} = Err -> Err
    end.

call_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, ChainState1} ->
            UnitReturn = {ok, <<0:256>>},
            GasSpent   = 0,         %% Already costs lots of gas
            {ok, UnitReturn, GasSpent,
             aevm_eeevm_state:set_chain_state(ChainState1, State)};
        {ok, Retval, ChainState1} ->
            GasSpent   = 0,         %% Already costs lots of gas
            Return     = {ok, aeso_data:to_binary(Retval, 0)},
            {ok, Return, GasSpent,
             aevm_eeevm_state:set_chain_state(ChainState1, State)};
        {error, _} = Err -> Err
    end.

oracle_call_register(_Value, Data, State) ->
    ArgumentTypes = [word, word, word, word, typerep, typerep],
    [Acct, Sign, QFee, TTL, QType, RType] = get_args(ArgumentTypes, Data),
    Callback =
        fun(API, ChainState) ->
            case API:oracle_register(<<Acct:256>>, <<Sign:256>>, QFee, TTL, QType, RType, ChainState) of
                {ok, <<OKey:256>>, ChainState1} -> {ok, OKey, ChainState1};
                {error, _} = Err                -> Err
            end end,
    call_chain(Callback, State).

oracle_call_query(Value, Data, State) ->
    [Oracle]  = get_args([word], Data),  %% We need the oracle address before we can decode the query
    OracleKey = <<Oracle:256>>,
    case call_chain1(fun(API, ChainState) -> API:oracle_query_spec(OracleKey, ChainState) end, State) of
        {ok, QueryType} ->
            ArgumentTypes = [word, QueryType, word, word],
            [_Oracle, Q, QTTL, RTTL] = get_args(ArgumentTypes, Data),
            Callback = fun(API, ChainState) ->
                case API:oracle_query(OracleKey, Q, _QFee=Value, QTTL, RTTL, ChainState) of
                    {ok, <<QKey:256>>, ChainState1} -> {ok, QKey, ChainState1};
                    {error, _} = Err                -> Err
                end end,
            call_chain(Callback, State);
        {error, _} = Err -> Err
    end.


oracle_call_respond(_Value, Data, State) ->
    [Oracle, Query] = get_args([word, word], Data),
    OracleKey = <<Oracle:256>>,
    case call_chain1(fun(API, ChainState) -> API:oracle_response_spec(OracleKey, ChainState) end, State) of
        {ok, RType} ->
            ArgumentTypes = [word, word, word, RType],
            [_, _, Sign, R] = get_args(ArgumentTypes, Data),
            QueryKey = <<Query:256>>,
            Callback = fun(API, ChainState) -> API:oracle_respond(OracleKey, QueryKey, Sign, R, ChainState) end,
            call_chain(Callback, State);
        {error, _} = Err -> Err
    end.


oracle_call_extend(_Value, Data, State) ->
    ArgumentTypes = [word, word, word],
    [Oracle, Sign, TTL] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_extend(<<Oracle:256>>, Sign, TTL, ChainState) end,
    call_chain(Callback, State).


oracle_call_get_answer(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_answer(<<O:256>>, <<Q:256>>, ChainState) end,
    query_chain(Callback, State).


oracle_call_get_question(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_question(<<O:256>>, <<Q:256>>, ChainState) end,
    query_chain(Callback, State).


oracle_call_query_fee(_Value, Data, State) ->
    ArgumentTypes = [word],
    [Oracle] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_query_fee(<<Oracle:256>>, ChainState) end,
    query_chain(Callback, State).

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
aens_call(_, _, _, _) ->
    {error, out_of_gas}.

aens_call_resolve(Data, State) ->
    [Name, Key, Type] = get_args([string, string, typerep], Data),
    Callback = fun(API, ChainState) -> API:aens_resolve(Name, Key, Type, ChainState) end,
    query_chain(Callback, State).

aens_call_preclaim(Data, State) ->
    [Addr, CHash, Sign] = get_args([word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_preclaim(<<Addr:256>>, <<CHash:256>>, <<Sign:256>>, ChainState) end,
    call_chain(Callback, State).

aens_call_claim(Data, State) ->
    [Addr, Name, Salt, Sign] = get_args([word, string, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_claim(<<Addr:256>>, Name, Salt, <<Sign:256>>, ChainState) end,
    call_chain(Callback, State).

aens_call_transfer(Data, State) ->
    [From, To, Hash, Sign] = get_args([word, word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_transfer(<<From:256>>, <<To:256>>, <<Hash:256>>, <<Sign:256>>, ChainState) end,
    call_chain(Callback, State).

aens_call_revoke(Data, State) ->
    [Addr, Hash, Sign] = get_args([word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_revoke(<<Addr:256>>, <<Hash:256>>, <<Sign:256>>, ChainState) end,
    call_chain(Callback, State).

%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

get_primop(Data) ->
    {ok, T} = aeso_data:from_binary(?BASE_ADDRESS, {tuple, [word]}, Data),
    {PrimOp} = T,
    PrimOp.

get_args(Types, Data) ->
    {ok, Val} = aeso_data:from_binary(?BASE_ADDRESS, {tuple, [word | Types]}, Data),
    [_ | Args] = tuple_to_list(Val),
    Args.

