%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%% -------------------------------------------------------------------
-module(aehc_db).

-export([create_tables/1    %% (Mode) -> ok
        , check_tables/1    %% (Acc)  -> Acc1
        ]).

%% Place here for mocking
%% write_persisted_staking_contract_address(Address) -

create_tables(Mode) ->
    case aehc_utils:hc_enabled() of
        true ->
            AllSpecs = all_specs(Mode),
            Specs = lists:flatten([proplists:lookup(Table, AllSpecs) || {missing_table, Table} <- check_tables([])]),
            [{atomic, ok} = mnesia:create_table(Tab, Spec) || {Tab, Spec} <- Specs];
        false ->
            []
    end.

check_tables(Acc) ->
    case aehc_utils:hc_enabled() of
        true ->
            lists:foldl(
              fun(M, Acc1) ->
                      M:check_tables(Acc1)
              end, Acc, modules());
        false ->
            Acc
    end.

modules() ->
    [aehc_parent_db].

all_specs(Mode) ->
    lists:flatten([M:table_specs(Mode) || M <- modules()]).
