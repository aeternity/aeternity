%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utilities for the Ring language tests.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aer_test_utils).

-export([read_contract/1, contract_path/0]).

%% TODO: does eunit have some path machinery?
contract_path() ->
    "apps/aering/test/contracts".

%% Read a contract file from the test/contracts directory.
-spec read_contract(string() | atom()) -> string().
read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aer"]))),
    binary_to_list(Bin).

