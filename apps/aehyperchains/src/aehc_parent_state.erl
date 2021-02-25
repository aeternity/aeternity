%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% The parent chain state entry which is accessed by state machine
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_state).
-author("sojourner").

%% API
-export([parent_state/3]).

-export([genesis/1, genesis/2]).
-export([top/1, top/2]).
-export([height/1, height/2]).

%% NOTE We can supply additional state data: chain difficulty, forks, meta-info, etc.
-record(parent_state, { genesis::binary(), top::binary(), height::non_neg_integer() }).

-type parent_state() :: #parent_state{}.

-export_type([parent_state/0]).
%% TODO To rename into pointer
-spec parent_state(binary(), binary(), non_neg_integer()) -> parent_state().
parent_state(Pointer, Top, Height) when is_binary(Pointer), is_binary(Top), is_integer(Height) ->
    #parent_state{
        genesis = Pointer,
        top = Top,
        height = Height
    }.

-spec genesis(parent_state()) -> binary().
genesis(ParentState) ->
    ParentState#parent_state.genesis.

-spec genesis(parent_state(), binary()) -> parent_state().
genesis(ParentState, Genesis) ->
    ParentState#parent_state{ genesis = Genesis }.

-spec top(parent_state()) -> binary().
top(ParentState) ->
    ParentState#parent_state.top.

-spec top(parent_state(), binary()) -> parent_state().
top(ParentState, Top) ->
    ParentState#parent_state{ top = Top }.

-spec height(parent_state()) -> non_neg_integer().
height(ParentState) ->
    ParentState#parent_state.height.

-spec height(parent_state(), non_neg_integer()) -> parent_state().
height(ParentState, Height) ->
    ParentState#parent_state{ height = Height }.

%% TODO To add support of custom vocabulary
