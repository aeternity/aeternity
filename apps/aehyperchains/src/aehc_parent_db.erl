%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Persistent storage for parent chain data
%%% @end
%%%-------------------------------------------------------------------

-module(aehc_parent_db).

%% API
-export([]).

-export([
          table_specs/1
        , check_tables/1
        ]).

-define(TAB(Record),
        {Record, aec_db:tab(Mode, Record, record_info(fields, Record), [])}).
-define(TAB(Record, Extra),
        {Record, aec_db:tab(Mode, Record, record_info(fields, Record), Extra)}).

%% start a transaction if there isn't already one
-define(t(Expr), aec_db:ensure_transaction(fun() -> Expr end)).
-define(t(Expr, ErrorKeys), aec_db:ensure_transaction(fun() -> Expr end, ErrorKeys)).

-record(hc_db_pogf, {key, value}).
-record(hc_db_commitment_header, {key, value}).
-record(hc_db_parent_block, {key, value}).

table_specs(Mode) ->
    [ ?TAB(hc_db_pogf)
    , ?TAB(hc_db_commitment_header)
    , ?TAB(hc_db_parent_block)
    ].

check_tables(Acc) ->
    lists:foldl(
      fun({Tab, Spec}, Acc1) ->
              aec_db:check_table(Tab, Spec, Acc1)
      end, Acc, table_specs(disc)).
