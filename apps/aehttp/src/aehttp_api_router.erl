-module(aehttp_api_router).

-export([get_paths/2]).

-type init_args()  :: {
    SpecVsn :: aehttp_spec:version(),
    Operations :: atom(),
    Method :: binary(),
    LogicHandler :: atom()

}.

-include("aehttp_spec.hrl").

-spec get_paths(Target :: atom(), LogicHandler :: atom()) -> [{
    Path :: binary(),
    Handler :: aehttp_api_handler,
    InitArgs :: init_args()
}].

get_paths(Target, LogicHandler) ->
    {ok, EnabledGroups} = application:get_env(aehttp, enabled_endpoint_groups),
    PathsFun =
        fun(SpecVsn) ->
            Endpoints =
                case SpecVsn of
                    ?OAS3 -> oas_endpoints;
                    ?SWAGGER2 -> endpoints
                end,
            [{path(Path), aehttp_api_handler,
                {SpecVsn, OperationId, method(Method), LogicHandler}}
                || {OperationId, Spec} <- maps:to_list(Endpoints:operations()),
                    {Method, #{path := Path, tags := Tags}} <- maps:to_list(Spec),
                    is_enabled(Target, Tags, EnabledGroups)
            ]
        end,
  Paths = PathsFun(?SWAGGER2) ++ PathsFun(?OAS3),
  %% Dirty hack to make sure /tx is not matched before /tx/{hash} is evaluated:
  %% sort it and reverse, such that longest part is matched first
  lists:reverse(lists:sort(Paths)) ++
    [{<<"/api">>, aehttp_spec_handler, {'Api', <<"GET">>}}].

path(Path0) ->
    Path1 = binary:replace(Path0, <<"}">>, <<"">>, [global]),
    binary:replace(Path1, <<"{">>, <<":">>, [global]).

method(Atom) ->
    list_to_binary(string:uppercase(atom_to_list(Atom))).

is_enabled(Target, Tags, EnabledGroups) when is_atom(Target) ->
    TargetBin = atom_to_binary(Target, utf8),
    lists:member(TargetBin, Tags) andalso
    lists:any(fun(Tag) -> lists:member(Tag, EnabledGroups) end, Tags).
