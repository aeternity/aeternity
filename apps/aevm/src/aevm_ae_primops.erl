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

-define(BASE_ADDRESS, 64). %% Byte offset for data
-spec call( non_neg_integer(), binary(), aevm_eeevm_state:state()) ->
                  {ok, binary(), non_neg_integer(), aevm_eeevm_state:state()}
                      | {error, any()}.
call(Value, Data, State) ->
    %% TODO: use aeso_data:from_binary() (but it doesn't take a base address at the moment)
    <<Offset:256, CallData/binary>> = Data,
    if Offset >= ?BASE_ADDRESS ->
            FirstWord = Offset * 8,
            case <<0:256, Data/binary>> of
                <<_Skip:FirstWord, ?PRIM_CALL_SPEND:256, Recipient:256>> ->
                    spend(Recipient, Value, State);
                <<_Skip:FirstWord, Type:256,_Argument/binary>>
                  when ?PRIM_CALL_IN_ORACLE_RANGE(Type) ->
                    Size = size(Data),
		    oracle_call(Type, Offset + 64, <<Size:256,0:256, Data/binary>>, State);
		_ ->
                    2 = Data,
		    %% Throw out of gas for illegal call
		    %% TODO: Better error for illegal call.
		    {error, out_of_gas}
	    end;
       true ->
            %% Throw out of gas for illegal call
            %% TODO: Better error for illegal call.
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

oracle_call(?PRIM_CALL_ORACLE_REGISTER, Offset, Data, State) ->
    oracle_call_register(Offset, Data, State);
oracle_call(?PRIM_CALL_ORACLE_QUERY, Offset, Data, State) ->
    oracle_call_query(Offset, Data, State);
oracle_call(?PRIM_CALL_ORACLE_RESPOND, Offset, Data, State) ->
    oracle_call_respond(Offset, Data, State);
oracle_call(?PRIM_CALL_ORACLE_EXTEND, Offset, Data, State) ->
    oracle_call_extend(Offset, Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_ANSWER, Offset, Data, State) ->
    oracle_call_get_answer(Offset, Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_QUESTION, Offset, Data, State) ->
    oracle_call_get_question(Offset, Data, State);
oracle_call(?PRIM_CALL_ORACLE_QUERY_FEE, Offset, Data, State) ->
    oracle_call_query_fee(Offset, Data, State);
oracle_call(_, _, _,_) ->
    {error, out_of_gas}.

call_chain(Callback, State) ->
    ChainAPI   = aevm_eeevm_state:chain_api(State),
    ChainState = aevm_eeevm_state:chain_state(State),

    case Callback(ChainAPI, ChainState) of
        {ok, ChainState1} ->
            UnitReturn = {ok, <<0:256>>},
            GasSpent   = 0,         %% Already costs lots of gas
            {ok, UnitReturn, GasSpent,
             aevm_eeevm_state:set_chain_state(ChainState1, State)};
        {ok, ChainState1, Retval} ->
            GasSpent   = 0,         %% Already costs lots of gas
            {ok, Retval, GasSpent,
             aevm_eeevm_state:set_chain_state(ChainState1, State)};
        {error, _} = Err -> Err
    end.

oracle_call_register(Offset, Data, State) ->
    %% TODO: Use the right argument types when typreps can be decode.
    %% ArgumentTypes = [word, word, word, typerep, typerep],
    ArgumentTypes = [word, word, word, word, word],
    [Acct, Sign, TTL, QType, RType] = get_args(ArgumentTypes, Offset, Data),
    Callback =
        fun(API, ChainState) ->
                API:oracle_register(Acct, Sign, TTL, QType, RType, ChainState)
        end,
    call_chain(Callback, State).

oracle_call_query(Offset, Data, State) ->
    %% TODO: the querytype has to be encoded by the caller
    %% and decoded here
    %% ArgumentTypes = [word, 'unnkown', word, word],
    ArgumentTypes = [word, word, word, word],
    [Oracle, Q, QTTL, RTTL] = get_args(ArgumentTypes, Offset, Data),
    Callback = fun(API, ChainState) -> API:oracle_query(Oracle, Q, QTTL, RTTL, ChainState) end,
    call_chain(Callback, State).


oracle_call_respond(Offset, Data, State) ->
    %% [word, word, ast_type(RType)]
    ArgumentTypes = [word, word, word],
    [Oracle, Sign, R] = get_args(ArgumentTypes, Offset, Data),
    Callback = fun(API, ChainState) -> API:oracle_respond(Oracle, Sign, R, ChainState) end,
    call_chain(Callback, State).


oracle_call_extend(Offset, Data, State) ->
    ArgumentTypes = [word, word, word],
    [Oracle, Sign, TTL] = get_args(ArgumentTypes, Offset, Data),
    Callback = fun(API, ChainState) -> API:oracle_extend(Oracle, Sign, TTL, ChainState) end,
    call_chain(Callback, State).


oracle_call_get_answer(Offset, Data, State) ->
    ArgumentTypes = [word],
    [Q] = get_args(ArgumentTypes, Offset, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_answer(Q, ChainState) end,
    call_chain(Callback, State).


oracle_call_get_question(Offset, Data, State) ->
    ArgumentTypes = [word],
    [Q] = get_args(ArgumentTypes, Offset, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_question(Q, ChainState) end,
    call_chain(Callback, State).


oracle_call_query_fee(Offset, Data, State) ->
    ArgumentTypes = [word],
    [Oracle] = get_args(ArgumentTypes, Offset, Data),
    Callback = fun(API, ChainState) -> API:oracle_query_fee(Oracle, ChainState) end,
    call_chain(Callback, State).



get_type(TypeDef,_Mem) ->
    TypeDef.

-define(TYPEREP_WORD_TAG,   0).
-define(TYPEREP_STRING_TAG, 1).
-define(TYPEREP_LIST_TAG,   2).
-define(TYPEREP_TUPLE_TAG,  3).
-define(TYPEREP_OPTION_TAG, 4).

get_args([],_Offset,_Data) -> [];
get_args([word|TypeSpecs], Offset, Data) ->
    [get_word(Offset, Data) |
     get_args(TypeSpecs, Offset + 32, Data)];
get_args([{tuple,Cpts}|TypeSpecs], Offset, Data) ->
    Value =
        list_to_tuple([get_args([T], Offset+32*I, Data)
                       || {T,I} <- lists:zip(Cpts,lists:seq(0,length(Cpts)-1))]),
    [Value| get_args(TypeSpecs, Offset + 32, Data)];
get_args([typerep|TypeSpecs], Offset, Data) ->
    %% TODO: There is something strange with the enocding of typedefs
    Address = get_word(Offset, Data),
    Tag = get_word(Address, Data),
    Arg = fun(T) -> get_args([T], Offset + 32, Data) end,
    Value =
        case Tag of
            ?TYPEREP_WORD_TAG   -> word;
            ?TYPEREP_STRING_TAG -> string;
            ?TYPEREP_LIST_TAG   -> {list,   Arg(typerep)};
            ?TYPEREP_OPTION_TAG -> {option, Arg(typerep)};
            ?TYPEREP_TUPLE_TAG  -> {tuple,  Arg({list, typerep})}
        end,
    [Value| get_args(TypeSpecs, Offset + 32, Data)];
get_args([{list, Elem}|TypeSpecs], Offset, Data) ->
    <<Nil:256>> = <<(-1):256>>,
    V = get_word(Offset, Data),
    Value =
        if V==Nil ->
                [];
           true ->
                {H,T} = get_args([{tuple,[Elem,{list,Elem}]}],V, Data),
                [H|T]
        end,
    [Value| get_args(TypeSpecs, Offset + 32, Data)].


get_word(Offset, Data) ->
    BitOffset = Offset*8,
    <<_:BitOffset, Word:256, _/binary>> = Data,
    Word.
