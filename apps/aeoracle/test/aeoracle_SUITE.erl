%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc CT test suite for AE Oracles
%%% @end
%%%-------------------------------------------------------------------
-module(aeoracle_SUITE).

%% common_test exports
-export([all/0]).

%% test case exports
-export([aeoracle_exist/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [aeoracle_exist].

aeoracle_exist(_) ->
    case application:load(aeoracle) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok
    end.
