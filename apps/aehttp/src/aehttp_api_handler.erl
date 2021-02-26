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
    validator :: jesse_state:state(),
    endpoints :: module()
}).

-include_lib("aeutils/include/aeu_stacktrace.hrl").

-define(DEFAULT_HTTP_CACHE_ENABLED, false).

init(Req, {SpecVsn, OperationId, AllowedMethod, LogicHandler}) ->
    Mod =
        case SpecVsn of
            oas3 -> oas_endpoints;
            swagger2 -> endpoints
        end,
    State = #state{
        operation_id = OperationId,
        allowed_method = AllowedMethod,
        logic_handler = LogicHandler,
        validator = aehttp_api_validate:validator(SpecVsn),
        endpoints = Mod
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
        validator = Validator,
        endpoints = Mod
    }) ->
    Method = cowboy_req:method(Req0),
    try aehttp_api_validate:request(OperationId, Method, Req0, Validator, Mod) of
        {ok, Params, Req1} ->
            Context = #{},
            {Code, Headers, Body0} = LogicHandler:handle_request(OperationId, Params, Context),
            Body =
                case maps:get('big-int-as-string', Params) of
                    false -> Body0;
                    true -> convert_all_ints_to_string(Body0)
                end,
            _ = aehttp_api_validate:response(OperationId, Method, Code, Body,
                                             Validator, Mod),
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

convert_all_ints_to_string(Map) ->
   maps:map(
      fun(_, I) when is_integer(I) -> integer_to_binary(I);
         (_, M) when is_map(M) -> convert_all_ints_to_string(M);
         (_, Other) -> Other
      end,
      Map).
