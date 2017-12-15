%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Test utilities for the Ring language tests.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aer_test_utils).

-export([read_contract/1, contract_path/0]).

contract_path() ->
    {ok, Cwd} = file:get_cwd(),
    N   = length(filename:split(Cwd)),
    Rel = ["apps", "aering", "test", "contracts"],
    %% Try the first matching directory (../)*Rel
    Cand = fun(I) -> filename:join(lists:duplicate(I, "..") ++ Rel) end,
    case [ Dir || Dir <- lists:map(Cand, lists:seq(0, N)), filelib:is_dir(Dir) ] of
        [Dir | _] -> Dir;
        []        -> error(failed_to_find_contract_dir)
    end.

%% Read a contract file from the test/contracts directory.
-spec read_contract(string() | atom()) -> string().
read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aer"]))),
    binary_to_list(Bin).

