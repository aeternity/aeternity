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
    {ok, EnabledGroups} = application:get_env(aehttp, enabled_endpoint_groups),
    Validator = aehttp_api_validate:validator(),

    Paths = [{path(Path), aehttp_api_handler,
        {OperationId, method(Method), LogicHandler, Validator}}
        || {OperationId, Spec} <- maps:to_list(endpoints:operations()),
            {Method, #{path := Path, tags := Tags}} <- maps:to_list(Spec),
            is_enabled(Target, Tags, EnabledGroups)
    ],
    lists:reverse(lists:sort(Paths)).

path(Path0) ->
    Path1 = binary:replace(Path0, <<"}">>, <<"">>, [global]),
    binary:replace(Path1, <<"{">>, <<":">>, [global]).

method(Atom) ->
    list_to_binary(string:uppercase(atom_to_list(Atom))).

is_enabled(Target, Tags, EnabledGroups) when is_atom(Target) ->
    TargetBin = atom_to_binary(Target, utf8),
    lists:member(TargetBin, Tags) andalso
    lists:any(fun(Tag) -> lists:member(Tag, EnabledGroups) end, Tags).

