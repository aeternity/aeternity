%%%-------------------------------------------------------------------
%%% @author michal
%%% @copyright
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(co_ctrl).

%% Cowboy callbacks
-export([handle_post/2, handle_get/2, content_types_accepted/2, content_types_provided/2, allowed_methods/2]).

%% AE callbacks
-export([path/0]).

%%%=============================================================================
%%% REST callbacks
%%%=============================================================================

path() ->
    Version = <<"v2">>, %% include in module name for backward compat?
    <<"/", Version/binary, <<"/top">> >>.

%% this will happen
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

%% do we need that?
content_types_provided(Req, State) ->
    {[{<<"text/plain">>, handle_get}], Req, State}.

%% do we need that?
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

handle_get(Req, State) ->
    {<<"Some response">>, Req, State}.

handle_post(Req, State) ->
    {ok, Bin, Req1} = cowboy_req:body(Req),
    lager:debug("Params: ~p", [Bin]),

    {true, <<"Post response">>, State}.
