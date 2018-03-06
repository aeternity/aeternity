-module(aehttp_api_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env0) ->
    {Operations, LogicHandler, ValidatorState} = proplists:get_value(handler_opts, Env0),

    {Method, Req} = cowboy_req:method(Req0),
    case maps:get(Method, Operations, undefined) of
        undefined -> {error, 405, Req};
        OperationId ->
            case swagger_api:populate_request(OperationId, Req, ValidatorState) of
                {ok, Params, Req1} ->
                    Env1 = proplists:delete(handler_opts, Env0),
                    AllowedMethods = maps:keys(Operations),
                    HandlerOpts = {OperationId, AllowedMethods, LogicHandler, Params},
                    {ok, Req1, [{handler_opts, HandlerOpts} | Env1]};
                {error, Reason, Req1} ->
                    error_logger:error_msg("Unable to process params for ~p: ~p", [OperationId, Reason]),
                    {error, 400, Req1}
            end
    end.
