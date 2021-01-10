-module(aehttp_spec).

-export([json/0]).


%% @doc
%% Get the json specification by parsing the swagger.yaml file and change it's version dynamicly.
-spec json() -> jsx:json_text().
json() ->
    Filename = filename:join(code:priv_dir(aehttp), "swagger.yaml"),
    Yamls = yamerl_constr:file(Filename, [str_node_as_binary]),
    YamlTpl = lists:last(Yamls),
    OldInfo = proplists:get_value(<<"info">>, YamlTpl),
    UpdatedInfo = lists:keyreplace(<<"version">>, 1, OldInfo, {<<"version">>, aeu_info:get_version()}),
    Yaml = lists:keyreplace(<<"info">>, 1, YamlTpl, {<<"info">>, UpdatedInfo}),
    Json = jsx:prettify(jsx:encode(Yaml)),
    Json.
