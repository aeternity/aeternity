%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Cross-Origin Resource Sharing (CORS) middleware for Cowboy
%%% @end
%%%=============================================================================
-module(aehttp_cors_middleware).

-behaviour(cowboy_middleware).

%% Behavior API
-export([execute/2]).

-define(ACCESS_CONTROL_ALLOWED_DOMAINS   , [<<"*">>]).
-define(ACCESS_CONTROL_ALLOW_METHODS     , <<"DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT">>).
-define(ACCESS_CONTROL_MAX_AGE           , 30 * 60). %% 30 minutes
-define(ACCESS_CONTROL_ALLOW_CREDENTIALS , <<"true">>).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec execute(cowboy_req:req(), cowboy_middleware:env()) -> {ok, cowboy_req:req(), cowboy_middleware:env()}.
execute(Req, Env) ->
    Req2 = set_cors_headers(Req),
    case cowboy_req:method(Req2) of
        <<"OPTIONS">> -> {stop, cowboy_req:reply(200, Req2)};
        _             -> {ok, Req2, Env}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec set_cors_headers(cowboy_req:req()) -> cowboy_req:req().
set_cors_headers(Req1) ->
    case cowboy_req:header(<<"origin">>, Req1) of
        undefined ->
            Req1;
        Origin ->
            AllowedDomains = aeu_env:user_config(
                               [<<"http">>, <<"cors">>, <<"allow_domains">>],
                               ?ACCESS_CONTROL_ALLOWED_DOMAINS),
            case is_allowed(Origin, AllowedDomains) of
                true ->
                    set_cors_headers(Req1, Origin);
                false ->
                    Req1
            end
    end.

is_allowed(Origin, AllowedDomains) ->
    case lists:member(<<"*">>, AllowedDomains) of
        true  -> true;
        false -> lists:member(Origin, AllowedDomains)
    end.

set_cors_headers(Req, Origin) ->
    SetHeader =
        fun({Header, Value}, R) ->
                cowboy_req:set_resp_header(Header, Value, R)
        end,
    Headers = cors_headers(Req, Origin),
    lists:foldl(SetHeader, Req, Headers).

cors_headers(Req, Origin) ->
    CorsConfig   = aeu_env:user_config([<<"http">>, <<"cors">>], []),
    AllowMethods = proplists:get_value(<<"allow_methods">> , CorsConfig, ?ACCESS_CONTROL_ALLOW_METHODS),
    MaxAge       = proplists:get_value(<<"max_age">>       , CorsConfig, ?ACCESS_CONTROL_MAX_AGE),
    CorsHeaders =
        [{<<"vary">>                             , <<"origin">>},
         {<<"access-control-allow-origin">>      , Origin},
         {<<"access-control-allow-methods">>     , AllowMethods},
         {<<"access-control-allow-credentials">> , ?ACCESS_CONTROL_ALLOW_CREDENTIALS},
         {<<"access-control-max-age">>           , integer_to_list(MaxAge)}],
    case cowboy_req:header(<<"access-control-request-headers">>, Req) of
        undefined ->
            CorsHeaders;
        AllowHeaders0 ->
            AllowHeaders = proplists:get_value(<<"allow_headers">>, CorsConfig, AllowHeaders0),
            [{<<"access-control-allow-headers">>, AllowHeaders} | CorsHeaders]
    end.
