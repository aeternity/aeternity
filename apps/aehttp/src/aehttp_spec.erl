-module(aehttp_spec).
-include("aehttp_spec.hrl").

-export([json/1, yaml_to_json/1]).
-export_type([version/0]).
%%
%% @doc
%% Get the json specification by parsing the swagger.yaml file and change it's version dynamically.
-spec json(version()) -> jsx:json_text().
json(SpecVsn) ->
    Filename = filename:join(code:priv_dir(aehttp), filename(SpecVsn)),
    [Yaml0] = yamerl_constr:file(Filename, [str_node_as_binary]),
    Yaml =
        lists:foldl(
            fun(Fun, Accum) -> Fun(Accum) end,
            Yaml0,
            [fun set_version/1
            ]),
    Json = jsx:prettify(jsx:encode(Yaml)),
    Json.

yaml_to_json(Yaml0) when is_binary(Yaml0) ->
    Yaml =
        lists:foldl(
            fun(Fun, Accum) -> Fun(Accum) end,
            Yaml0,
            []),
    Json = jsx:prettify(jsx:encode(Yaml)),
    Json;
yaml_to_json({filename, Filename}) ->
    [Yaml0] = yamerl_constr:file(Filename, [str_node_as_binary]),
    yaml_to_json(Yaml0).

set_version(Yaml) ->
    OldInfo = proplists:get_value(<<"info">>, Yaml),
    UpdatedInfo = lists:keyreplace(<<"version">>, 1, OldInfo, {<<"version">>, aeu_info:get_version()}),
    lists:keyreplace(<<"info">>, 1, Yaml, {<<"info">>, UpdatedInfo}).

filename(?SWAGGER2) -> "swagger.yaml";
filename(?OAS3) -> "oas3.yaml";
filename(?ROSETTA) -> "rosetta.yaml".

