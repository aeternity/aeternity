%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% ADT for contract call objects.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_dispatch).

%% API
-export([ call/4
	, encode_call_data/4]).


call(<<"ring">>, Code, Function, Argument) ->
    aect_ring:simple_call(Code, Function, Argument);
call(<<"evm">>, Code, _, Argument) ->
    aect_evm:call(Code, Argument);
call(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.


encode_call_data(<<"ring">>, Code, Function, Argument) ->
    aect_ring:encode_call_data(Code, Function, Argument);
encode_call_data(<<"evm">>, Code, Function, Argument) ->
    aect_evm:encode_call_data(Code, Function, Argument);
encode_call_data(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.


