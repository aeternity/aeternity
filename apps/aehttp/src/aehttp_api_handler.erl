-module(aehttp_api_handler).

-behaviour(cowboy_rest).

-export([allowed_methods/2]).
-export([init/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([handle_request_json/2]).

-record(state, {
    operation_id :: atom(),
    allowed_method :: binary(),
    logic_handler :: atom(),
    validator :: jesse_state:state()
}).

init(Req, {OperationId, AllowedMethod, LogicHandler, Validator}) ->
    State = #state{
        operation_id = OperationId,
        allowed_method = AllowedMethod,
        logic_handler = LogicHandler,
        validator = Validator
    },
    {cowboy_rest, Req, State}.


allowed_methods(Req, State = #state{ allowed_method = Method }) ->
    {[Method], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

delete_resource(Req, State) ->
    handle_request_json(Req, State).

handle_request_json(Req0, State = #state{
        operation_id = OperationId,
        logic_handler = LogicHandler,
        validator = Validator
    }) ->
    Method = cowboy_req:method(Req0),
    case aehttp_api_validate:request(OperationId, Method, Req0, Validator) of
        {ok, Params, Req1} ->
            Context = #{},
            {Code, Headers, Body} = LogicHandler:handle_request(OperationId, Params, Context),

            _ = aehttp_api_validate:response(OperationId, Method, Code, Body, Validator),

            Req = cowboy_req:reply(Code, to_headers(Headers), jsx:encode(Body), Req1),
            {stop, Req, State};
        {error, Reason, Req1} ->
            error_logger:error_msg("Unable to process params for ~p: ~p", [OperationId, Reason]),
            Req = cowboy_req:reply(400, Req1),
            {stop, Req, State}
    end.

to_headers(Headers) when is_list(Headers) ->
    maps:from_list(Headers);
to_headers(Headers) ->
    Headers.
