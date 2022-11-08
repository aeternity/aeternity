-module(aehttp_api_router).

-export([get_paths/2, get_paths/3]).

-type init_args()  :: {
    SpecVsn :: aehttp_spec:version(),
    Operations :: atom(),
    Method :: binary(),
    LogicHandler :: atom(),
    Context :: any()

}.

-include("aehttp_spec.hrl").

-spec get_paths(Target :: atom(), LogicHandler :: atom()) -> [{
    Path :: binary(),
    Handler :: atom(),
    InitArgs :: init_args()
}].

get_paths(Target, LogicHandler) ->
    get_paths(Target, LogicHandler, #{}).

get_paths(Target, LogicHandler, Context) ->
    {ok, EnabledGroups} = application:get_env(aehttp, enabled_endpoint_groups),
    PathsFun =
        fun(SpecVsn) ->
            Endpoints =
                case SpecVsn of
                    ?OAS3 -> oas_endpoints;
                    ?SWAGGER2 -> endpoints;
                    ?ROSETTA -> rosetta_endpoints;
                    ?ROSETTA_OFFLINE -> rosetta_endpoints
                end,
            [{path(Path), aehttp_api_handler,
                {SpecVsn, OperationId, method(Method), LogicHandler, Context}}
                || {OperationId, #{path := Path, tags := Tags, method := Method}} <- maps:to_list(Endpoints:operations()),
                    is_enabled(Target, Tags, EnabledGroups)
                ]
        end,
    Paths = case Target of
                rosetta -> PathsFun(?ROSETTA);
                rosetta_offline -> PathsFun(?ROSETTA_OFFLINE);
                _ -> PathsFun(?SWAGGER2) ++ PathsFun(?OAS3)
            end,
    %% Dirty hack to make sure /tx is not matched before /tx/{hash} is evaluated:
    %% sort it and reverse, such that longest part is matched first
    lists:reverse(lists:sort(Paths)) ++
        [{<<"/api">>, aehttp_spec_handler, {'Api', <<"GET">>}}].

path(Path0) ->
    Path1 = binary:replace(Path0, <<"}">>, <<"">>, [global]),
    binary:replace(Path1, <<"{">>, <<":">>, [global]).

method(Method) ->
    list_to_binary(string:uppercase(Method)).

is_enabled(rosetta, Tags, EnabledGroups) ->
    lists:any(fun(Tag) -> lists:member(Tag, EnabledGroups) end, Tags);
is_enabled(rosetta_offline, Tags, EnabledGroups) ->
    lists:any(fun(Tag) -> lists:member(Tag, EnabledGroups) end, Tags);
is_enabled(Target, Tags, EnabledGroups) when is_atom(Target) ->
    TargetBin = atom_to_binary(Target, utf8),
    lists:member(TargetBin, Tags) andalso
    lists:any(fun(Tag) -> lists:member(Tag, EnabledGroups) end, Tags).
