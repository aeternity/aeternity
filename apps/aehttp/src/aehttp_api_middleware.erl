-module(aehttp_api_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env0) ->
    {OperationId, Method, LogicHandler, Validator} = proplists:get_value(handler_opts, Env0),

    case cowboy_req:method(Req0) of
        {Method, Req} ->
            case aehttp_api_validate:request(OperationId, Method, Req, Validator) of
                {ok, Params, Req1} ->
                    Env1 = proplists:delete(handler_opts, Env0),
                    AllowedMethods = [Method],
                    HandlerOpts = {OperationId, AllowedMethods, LogicHandler, Params, Validator},
                    {ok, Req1, [{handler_opts, HandlerOpts} | Env1]};
                {error, Reason, Req1} ->
                    error_logger:error_msg("Unable to process params for ~p: ~p", [OperationId, Reason]),
                    {error, 400, Req1}
            end;
        _ ->
            {error, 405, Req0}
    end.
