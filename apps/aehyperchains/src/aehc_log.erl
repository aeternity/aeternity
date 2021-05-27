%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Generic logging abstraction
%%% @end
%%% -------------------------------------------------------------------
-module(aehc_log).
-author("ra").

-export([
    ldebug/1, ldebug/2,
    lerror/1, lerror/2,
    linfo/1, linfo/2]).


%% API

-spec ldebug(string()) -> ok.
ldebug(Msg) -> lager:debug(Msg).

-spec ldebug(string(), [term()]) -> ok.
ldebug(Msg, Data) -> lager:debug(Msg, Data).

-spec lerror(string()) -> ok.
lerror(Msg) -> lager:error(Msg).

-spec lerror(string(), [term()]) -> ok.
lerror(Msg, Data) -> lager:error(Msg, Data).

-spec linfo(string()) -> ok.
linfo(Msg) -> lager:info(Msg).

-spec linfo(string(), [term()]) -> ok.
linfo(Msg, Data) -> lager:info(Msg, Data).
