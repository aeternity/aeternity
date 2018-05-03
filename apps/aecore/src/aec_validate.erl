%%%-------------------------------------------------------------------
%%% @author sennui
%%% @copyright (C) 2018, AE
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_validate).
-author("michal").

%% API
-export([block/2]).

%% quick aec_conductor cleanup. TODO: route nicely validations
block(Block, LeaderKey) ->
    Header = aec_blocks:to_header(Block),
    case {aec_headers:validate(Header, LeaderKey), aec_blocks:validate(Block, LeaderKey)} of
        {ok, ok} -> {ok, ok};
        {{ok, Type}, ok} -> {ok, Type};
        {{error, Reason}, _} -> {error, {header, Reason}};
        {_, {error, Reason}} -> {error, {block, Reason}}
    end.


