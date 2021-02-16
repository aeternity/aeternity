%% Cowboy handler for endpoint /api to obtain the specification
%% of the endpoints as JSON object.

-module(aehttp_spec_handler).

-behaviour(cowboy_rest).

-export([allowed_methods/2]).
-export([init/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([generate_etag/2]).
-export([handle_request_json/2]).

-record(state, {
    operation_id :: atom(),
    allowed_method :: binary(),
    spec :: jsx:json_text()
}).

-include("aehttp_spec.hrl").

init(Req, {OperationId, AllowedMethod}) ->
    Qs = cowboy_req:parse_qs(Req),
    Spec =
        case proplists:get_value(<<"oas3">>, Qs, false) of
            true -> aehttp_api_validate:json_spec(?OAS3);
            false -> aehttp_api_validate:json_spec(?SWAGGER2)
        end,
    State = #state{
        operation_id = OperationId,
        allowed_method = AllowedMethod,
        spec = Spec
    },
    {cowboy_rest, Req, State}.


allowed_methods(Req, State = #state{ allowed_method = Method }) ->
    {[Method], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

generate_etag(Req, State = #state{spec = Spec}) ->
    Etag = aeu_hex:bin_to_hex(crypto:hash(sha, Spec)),
    {{strong, list_to_binary(Etag)}, Req, State}.

handle_request_json(Req, State = #state{
        operation_id = 'Api',
        spec = Spec
    }) ->
    Repl = cowboy_req:reply(200, #{}, Spec, Req),
    {stop, Repl, State}.
