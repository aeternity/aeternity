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
        logic_handler = LogicHandler,
        endpoints = Mod
    }) ->
    IsForbidden = LogicHandler:forbidden(Mod, OperationId),
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
                case maps:get('int-as-string', Params, false) of
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
    catch error:Error:StackTrace ->
        lager:warning("Unexpected validate result: ~p / ~p", [Error, StackTrace]),
        Body = jsx:encode(to_error({validation_error, <<>>, <<>>})),
        {stop, cowboy_req:reply(400, #{}, Body, Req0), State}
    end.

to_headers(Headers) when is_list(Headers) ->
    maps:from_list(Headers);
to_headers(Headers) ->
    Headers.

to_error({Reason, Name, Info0}) ->
    Info = 
        case is_map(Info0) of
            true -> Info0;
            false ->
                [{K, V} || {K, V} <- Info0, K =:= error
                                    orelse K =:= data
                                    orelse K =:= path]
        end,
    #{ reason => Reason,
       parameter => Name,
       info => Info }.

-spec cache_enabled() -> boolean().
cache_enabled() ->
    aeu_env:user_config_or_env([<<"http">>, <<"cache">>, <<"enabled">>],
                               aehttp, [cache, enabled], ?DEFAULT_HTTP_CACHE_ENABLED).

convert_all_ints_to_string(Val) ->
   case Val of
      _ when is_integer(Val) -> integer_to_binary(Val);
      _ when is_map(Val) -> maps:map(fun(_, V) -> convert_all_ints_to_string(V) end, Val);
      _ when is_list(Val) -> lists:map(fun(I) -> convert_all_ints_to_string(I) end, Val);
      Other -> Other
   end.
