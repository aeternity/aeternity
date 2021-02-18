-module(aehttp_spec).
-include("aehttp_spec.hrl").

-export([json/1]).
-export_type([version/0]).
%%
%% @doc
%% Get the json specification by parsing the swagger.yaml file and change it's version dynamicly.
-spec json(version()) -> jsx:json_text().
json(SpecVsn) ->
    Filename = filename:join(code:priv_dir(aehttp), filename(SpecVsn)),
    Yamls = yamerl_constr:file(Filename, [str_node_as_binary]),
    YamlTpl = lists:last(Yamls),
    OldInfo = proplists:get_value(<<"info">>, YamlTpl),
    UpdatedInfo = lists:keyreplace(<<"version">>, 1, OldInfo, {<<"version">>, aeu_info:get_version()}),
    Yaml = lists:keyreplace(<<"info">>, 1, YamlTpl, {<<"info">>, UpdatedInfo}),
    Json = jsx:prettify(jsx:encode(Yaml)),
    Json.

filename(?SWAGGER2) -> "swagger.yaml";
filename(?OAS3) -> "oas3.yaml".
