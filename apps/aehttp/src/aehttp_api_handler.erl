-module(aehttp_api_handler).

-behaviour(cowboy_rest).

-export([init/2]).

-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([forbidden/2]).
-export([handle_request_json/2]).
-export([generate_etag/2]).
-export([expires/2]).

-record(state, {
    operation_id :: atom(),
    allowed_method :: binary(),
    logic_handler :: atom(),
    validator :: jesse_state:state()
}).

-include_lib("aeutils/include/aeu_stacktrace.hrl").

-define(DEFAULT_HTTP_CACHE_ENABLED, false).

init(Req, {OperationId, AllowedMethod, LogicHandler}) ->
    %% TODO: make this validator a proper service.
    JsonSpec = aehttp_api_validate:json_spec(),
    Validator = aehttp_api_validate:validator(JsonSpec),

    State = #state{
        operation_id = OperationId,
        allowed_method = AllowedMethod,
        logic_handler = LogicHandler,
        validator = Validator
    },
    {cowboy_rest, Req, State}.


allowed_methods(Req, State = #state{ allowed_method = Method }) ->
    {[Method], Req, State}.

forbidden(Req, State = #state{
        operation_id = OperationId,
        logic_handler = LogicHandler
    }) ->
    IsForbidden = LogicHandler:forbidden(OperationId),
    {IsForbidden, Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_request_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

delete_resource(Req, State) ->
    handle_request_json(Req, State).

generate_etag(Req, State = #state{operation_id = OperationId}) ->
    case cache_enabled() of
        true -> aehttp_cache_etag:generate(OperationId, Req, State);
        false -> {undefined, Req, State}
    end.

expires(Req, State = #state{operation_id = OperationId}) ->
    case cache_enabled() of
        true -> aehttp_cache_expires:expires(OperationId, Req, State);
        false -> {undefined, Req, State}
    end.

handle_request_json(Req0, State = #state{
        operation_id = OperationId,
        logic_handler = LogicHandler,
        validator = Validator
    }) ->
    Method = cowboy_req:method(Req0),
    try aehttp_api_validate:request(OperationId, Method, Req0, Validator) of
        {ok, Params, Req1} ->
            Context = #{},
            {Code, Headers, Body} = LogicHandler:handle_request(OperationId, Params, Context),

            _ = aehttp_api_validate:response(OperationId, Method, Code, Body, Validator),

            Req = cowboy_req:reply(Code, to_headers(Headers), jsx:encode(Body), Req1),
            {stop, Req, State};
        {error, Reason, Req1} ->
            lager:info("Unable to process params for ~p: ~p", [OperationId, Reason]),
            Body = jsx:encode(to_error(Reason)),
            Req = cowboy_req:reply(400, #{}, Body, Req1),
            {stop, Req, State}
    ?_catch_(error, Error, StackTrace)
        lager:warning("Unexpected validate result: ~p / ~p", [Error, StackTrace]),
        Body = jsx:encode(to_error({validation_error, <<>>, <<>>})),
        {stop, cowboy_req:reply(400, #{}, Body, Req0), State}
    end.

to_headers(Headers) when is_list(Headers) ->
    maps:from_list(Headers);
to_headers(Headers) ->
    Headers.

to_error({Reason, Name, Info}) ->
    #{ reason => Reason,
       parameter => Name,
       info => Info }.

-spec cache_enabled() -> boolean().
cache_enabled() ->
    aeu_env:user_config_or_env([<<"http">>, <<"cache">>, <<"enabled">>],
                               aehttp, [cache, enabled], ?DEFAULT_HTTP_CACHE_ENABLED).
