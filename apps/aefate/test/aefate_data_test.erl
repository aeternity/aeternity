%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate data
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_data_test).

-include_lib("eunit/include/eunit.hrl").

format_integer_test() ->
    "0" = aefa_data:format(0).
