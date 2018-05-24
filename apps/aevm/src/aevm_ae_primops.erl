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
            FirstWord = Offset - ?BASE_ADDRESS,
            case CallData of
                <<_Skip:FirstWord, ?PRIM_CALL_SPEND:256, Recipient:256>> ->
                    spend(Recipient, Value, State);
                <<_Skip:FirstWord, Type:256,_Argument/binary>>
                  when ?PRIM_CALL_IN_ORACLE_RANGE(Type) ->
                    Size = size(Data),
		    oracle_call(Type, Offset + 32 , <<Size:256,0:256, Data/binary>>, State);
		_ ->
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
oracle_call(?PRIM_CALL_ORACLE_QUERY,_Offset, Data, State) ->
    oracle_call_query(Data, State);
oracle_call(?PRIM_CALL_ORACLE_RESPOND,_Offset, Data, State) ->
    oracle_call_respond(Data, State);
oracle_call(?PRIM_CALL_ORACLE_EXTEND,_Offset, Data, State) ->
    oracle_call_extend(Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_ANSWER,_Offset, Data, State) ->
    oracle_call_get_answer(Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_QUESTION,_Offset, Data, State) ->
    oracle_call_get_question(Data, State);
oracle_call(?PRIM_CALL_ORACLE_QUERY_FEE,_Offset, Data, State) ->
    oracle_call_query_fee(Data, State);
oracle_call(_, _, _,_) ->
    {error, out_of_gas}.

oracle_call_register(Offset, Data, State) ->
    [Acct, Sign, TTL, QType, RType] = get_args([word, word, word, typerep, typerep], Offset, Data),
    ChainAPI   = aevm_eeevm_state:chain_api(State),
    ChainState = aevm_eeevm_state:chain_state(State),

    case ChainAPI:oracle_register(Acct, Sign, TTL, QType, RType, ChainState) of
        {ok, ChainState1} ->
            UnitReturn = {ok, <<0:256>>},
            GasSpent   = 0,         %% Already costs lots of gas
            {ok, UnitReturn, GasSpent,
             aevm_eeevm_state:set_chain_state(ChainState1, State)};
        {error, _} = Err -> Err
    end.


oracle_call_query(Data, State) ->
    {ok, {ok, <<0:256>>}, 0, State}.

oracle_call_respond(Data, State) ->
    {ok, {ok, <<0:256>>}, 0, State}.

oracle_call_extend(Data, State) ->
    {ok, {ok, <<0:256>>}, 0, State}.

oracle_call_get_answer(Data, State) ->
    {ok, {ok, <<0:256>>}, 0, State}.

oracle_call_get_question(Data, State) ->
    {ok, {ok, <<0:256>>}, 0, State}.

oracle_call_query_fee(Data, State) ->
    {ok, {ok, <<0:256>>}, 0, State}.


get_type(TypeDef,_Mem) ->
    TypeDef.

-define(TYPEREP_WORD_TAG,   0).
-define(TYPEREP_STRING_TAG, 1).
-define(TYPEREP_LIST_TAG,   2).
-define(TYPEREP_TUPLE_TAG,  3).
-define(TYPEREP_OPTION_TAG, 4).

get_args([word|TypeSpecs], Offset, Data) ->
    [get_word(Offset, Data) |
     get_args(TypeSpecs, Offset + 32, Data)];
get_args([typerep|TypeSpecs], Offset, Data) ->
    Tag = get_word(Offset, Data),
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
