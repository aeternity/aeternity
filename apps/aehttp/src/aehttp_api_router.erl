-module(aehttp_api_router).

-export([get_paths/2]).

-type init_args()  :: {
    Operations :: atom(),
    Method :: binary(),
    LogicHandler :: atom(),
    ValidatorState :: jesse_state:state()
}.

-spec get_paths(Target :: atom(), LogicHandler :: atom()) -> [{
    Path :: binary(),
    Handler :: aehttp_api_handler,
    InitArgs :: init_args()
}].

get_paths(Target, LogicHandler) ->
    % Swagger validation will be replaced
    % we're using it as a intermediate step
    [{'_', [
            {_, _, {_, _, SwaggerValidator}}
            | _ ]}] = swagger_router:get_paths(LogicHandler),

    [{path(Path), aehttp_api_handler,
        {OperationId, method(Method), LogicHandler, SwaggerValidator}}
        || {OperationId, Spec} <- maps:to_list(endpoints:operations()),
            {Method, #{path := Path, tags := Tags}} <- maps:to_list(Spec),
            is_enabled(Target, Tags)
    ].

path(Path0) ->
    Path1 = binary:replace(Path0, <<"}">>, <<"">>, [global]),
    binary:replace(Path1, <<"{">>, <<":">>, [global]).

method(Atom) ->
    list_to_binary(string:uppercase(atom_to_list(Atom))).

is_enabled(Target, Tags) when is_atom(Target) ->
    is_enabled(atom_to_binary(Target, utf8), Tags);
is_enabled(Target, Tags) ->
    lists:member(Target, Tags).
