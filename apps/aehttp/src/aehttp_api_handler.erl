-module(aehttp_api_handler).

-export([allowed_methods/2]).
-export([init/3]).
-export([rest_init/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([handle_request_json/2]).

-record(state, {
    operation_id :: atom(),
    operation_params :: #{},
    allowed_methods :: [binary()],
    logic_handler :: atom()
}).

init(_Transport, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, {OperationId, AllowedMethods, LogicHandler, Params}) ->
    State = #state{
        operation_id = OperationId,
        operation_params = Params,
        allowed_methods = AllowedMethods,
        logic_handler = LogicHandler
    },
    {ok, Req, State}.

allowed_methods(Req, State = #state{ allowed_methods = Methods }) ->
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

delete_resource(Req, State) ->
    handle_request_json(Req, State).

handle_request_json(Req0, State = #state{
        operation_id = OperationId,
        operation_params = Params,
        logic_handler = LogicHandler
    }) ->
    Context = #{},
    {Code, Headers, Body} = LogicHandler:handle_request(OperationId, Params, Context),
    {ok, Req} = cowboy_req:reply(Code, Headers, jsx:encode(Body), Req0),
    {halt, Req, State}.
