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
-spec call( non_neg_integer(), binary(), aevm_eeevm_state:state()) ->
                  {ok, binary(), non_neg_integer(), aevm_eeevm_state:state()}
                      | {error, any()}.
call(Value, Data, State) ->
    try
        DecodeAs = fun(T) -> aeso_data:from_binary(?BASE_ADDRESS, T, Data) end,
        case DecodeAs({tuple, [word]}) of
            {?PRIM_CALL_SPEND} ->
                {?PRIM_CALL_SPEND, Recipient} = DecodeAs({tuple, [word, word]}),
                spend(Recipient, Value, State);
            {PrimOp} when ?PRIM_CALL_IN_ORACLE_RANGE(PrimOp) ->
                oracle_call(PrimOp, Value, Data, State)
        end
    catch _:Err ->
	%% TODO: Better error for illegal call.
        io:format("Illegal call\nData = ~p\n~p\n~p\n", [aeso_test_utils:dump_words(Data), Err, erlang:get_stacktrace()]),    %% tmp
        {error, out_of_gas}
    end.

%% ------------------------------------------------------------------
%% Basic account operations.
%% ------------------------------------------------------------------


spend(Recipient, Value, State) ->
    ChainAPI   = aevm_eeevm_state:chain_api(State),
    ChainState = aevm_eeevm_state:chain_state(State),

    case ChainAPI:spend(<<Recipient:256>>, Value, ChainState) of
        {ok, ChainState1} ->
            UnitReturn = {ok, <<0:256>>}, %% spend returns unit
            GasSpent   = 0,         %% Already costs lots of gas
            {ok, UnitReturn, GasSpent,
             aevm_eeevm_state:set_chain_state(ChainState1, State)};
        {error, _} = Err -> Err
    end.

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

query_chain(Callback, State) ->
    ChainAPI   = aevm_eeevm_state:chain_api(State),
    ChainState = aevm_eeevm_state:chain_state(State),
    Callback(ChainAPI, ChainState).

call_chain(Callback, State) ->
    case query_chain(Callback, State) of
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
    ArgumentTypes = [word, word, word, typerep, typerep],
    [Acct, Sign, TTL, QType, RType] = get_args(ArgumentTypes, Data),
    Callback =
        fun(API, ChainState) ->
                API:oracle_register(<<Acct:256>>, <<Sign:256>>, TTL, QType, RType, ChainState)
        end,
    call_chain(Callback, State).

oracle_call_query(Value, Data, State) ->
    [Oracle]  = get_args([word], Data),  %% We need the oracle address before we can decode the query
    OracleKey = <<Oracle:256>>,
    case query_chain(fun(API, ChainState) -> API:oracle_query_spec(OracleKey, ChainState) end, State) of
        {ok, QueryType} ->
            ArgumentTypes = [word, QueryType, word, word],
            [_Oracle, Q, QTTL, RTTL] = get_args(ArgumentTypes, Data),
            Callback = fun(API, ChainState) -> API:oracle_query(OracleKey, Q, Value, QTTL, RTTL, ChainState) end,
            call_chain(Callback, State);
        {error, _} = Err -> Err
    end.


oracle_call_respond(_Value, Data, State) ->
    %% [word, word, ast_type(RType)]
    ArgumentTypes = [word, word, word],
    [Oracle, Sign, R] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_respond(Oracle, Sign, R, ChainState) end,
    call_chain(Callback, State).


oracle_call_extend(_Value, Data, State) ->
    ArgumentTypes = [word, word, word],
    [Oracle, Sign, TTL] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_extend(Oracle, Sign, TTL, ChainState) end,
    call_chain(Callback, State).


oracle_call_get_answer(_Value, Data, State) ->
    ArgumentTypes = [word],
    [Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_answer(Q, ChainState) end,
    call_chain(Callback, State).


oracle_call_get_question(_Value, Data, State) ->
    ArgumentTypes = [word],
    [Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_question(Q, ChainState) end,
    call_chain(Callback, State).


oracle_call_query_fee(_Value, Data, State) ->
    ArgumentTypes = [word],
    [Oracle] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_query_fee(Oracle, ChainState) end,
    call_chain(Callback, State).

get_args(Types, Data) ->
    [_ | Args] = tuple_to_list(aeso_data:from_binary(?BASE_ADDRESS, {tuple, [word | Types]}, Data)),
    Args.

