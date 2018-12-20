-module(aehttp_api_router).

-export([get_paths/2]).

-type init_args()  :: {
    Operations :: atom(),
    Method :: binary(),
    LogicHandler :: atom()

}.

-spec get_paths(Target :: atom(), LogicHandler :: atom()) -> [{
    Path :: binary(),
    Handler :: aehttp_api_handler,
    InitArgs :: init_args()
}].

get_paths(Target, LogicHandler) ->
    {ok, EnabledGroups} = application:get_env(aehttp, enabled_endpoint_groups),
    JsonSpec = aehttp_api_validate:json_spec(),

    Paths = [{path(Path), aehttp_api_handler,
        {OperationId, method(Method), LogicHandler}}
        || {OperationId, Spec} <- maps:to_list(endpoints:operations()),
            {Method, #{path := Path, tags := Tags}} <- maps:to_list(Spec),
            is_enabled(Target, Tags, EnabledGroups)
    ],

  %% Dirty hack to make sure /tx is not matched before /tx/{hash} is evaluated:
  %% sort it and reverse, such that longest part is matched first
  lists:reverse(lists:sort(Paths)) ++
    [{<<"/api">>, aehttp_spec_handler, {'Api', <<"GET">>, JsonSpec}}].

path(Path0) ->
    Path1 = binary:replace(Path0, <<"}">>, <<"">>, [global]),
    binary:replace(Path1, <<"{">>, <<":">>, [global]).

method(Atom) ->
    list_to_binary(string:uppercase(atom_to_list(Atom))).

is_enabled(Target, Tags, EnabledGroups) ->
    case lists:member(<<"debug">>, Tags) of
        true -> aehttp_app:enable_internal_debug_endpoints();
        false -> is_enabled_(Target, Tags, EnabledGroups)
    end.

is_enabled_(Target, Tags, EnabledGroups) ->
    TargetBin = atom_to_binary(Target, utf8),
    lists:member(TargetBin, Tags) andalso
    lists:any(fun(Tag) -> lists:member(Tag, EnabledGroups) end, Tags).

