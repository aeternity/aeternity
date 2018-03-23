-module(aehttp_api_validate).

-export([request/4]).
-export([response/5]).
-export([validator/0]).


-spec validator() -> jesse_state:state().

validator() ->
    {ok, AppName} = application:get_application(?MODULE),
    Filename = filename:join(code:priv_dir(AppName), "swagger.json"),
    R = jsx:decode(element(2, file:read_file(Filename))),
    jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]).

-spec response(
    OperationId :: atom(),
    Methohd :: binary(),
    Code :: 200..599,
    Response :: jesse:json_term(),
    Validator :: jesse_state:state()
    ) -> ok | no_return().

response(OperationId, Method0, Code, Response, Validator) ->
    Method = to_method(Method0),
    #{responses := Resps} = maps:get(Method, endpoints:operation(OperationId)),
    case maps:get(Code, Resps, not_found) of
        undefined -> ok;
        not_found -> throw({error, {Code, unspecified_response_code}});
        #{<<"$ref">> := Ref} ->
            Schema = #{<<"$ref">> => <<"#", Ref/binary>>},
            _ = jesse_schema_validator:validate_with_state(Schema, Response, Validator),
            ok;
        #{<<"items">> := #{<<"$ref">> := Ref}, <<"type">> := <<"array">>} ->
            Schema = #{<<"$ref">> => <<"#", Ref/binary>>},
            [ _ = jesse_schema_validator:validate_with_state(Schema, Acc, Validator)
                || Acc <- Response ],
            ok;
        Schema ->
            _ = jesse_schema_validator:validate_with_state(Schema, Response, Validator),
            ok
    end.

-spec request(
    OperationId :: atom(),
    Methohd :: binary(),
    Req :: cowboy_req:req(),
    Validator :: jesse_state:state()
    ) ->
    {ok, Model :: #{}, cowboy_req:req()}
    | {error, Reason :: any(), cowboy_req:req()}.

request(OperationId, Method0, Req, Validator) ->
    Method = to_method(Method0),
    #{parameters := Params} = maps:get(Method, endpoints:operation(OperationId)),
    params(Params, #{}, Req, Validator).

params([], Model, Req, _) -> {ok, Model, Req};
params([Param | Params], Model, Req0, Validator) ->
   case populate_param(Param, Req0, Validator) of
        {ok, K, V, Req} ->
            NewModel = maps:put(to_atom(K), V, Model),
            params(Params, NewModel, Req, Validator);
        Error ->
            Error
    end.

populate_param(Param, Req, Validator) ->
    In = proplists:get_value("in", Param),
    Name = proplists:get_value("name", Param),
    case get_param_value(In, Name, Req) of
        {ok, Value, Req1} ->
            case prepare_param(Param, Value, Name, Validator) of
                {ok, NewName, NewValue} -> {ok, NewName, NewValue, Req1};
                {error, Reason} -> {error, Reason, Req1}
            end;
        Error -> Error
    end.

prepare_param([], Value, Name, _) -> {ok, Name, Value};
prepare_param([ Rule | Rules ], Value, Name, Validator) ->
    case prepare_param_(Rule, Value, Name, Validator) of
        ok -> prepare_param(Rules, Value, Name, Validator);
        {ok, NewValue} -> prepare_param(Rules, NewValue, Name, Validator);
        {ok, NewValue, NewName} -> prepare_param(Rules, NewValue, NewName, Validator);
        Error -> Error
    end.

% unused rules
prepare_param_({"in", _}, _, _, _) -> ok;
prepare_param_({"name", _}, _, _, _) -> ok;
prepare_param_({"description", _}, _, _, _) -> ok;
prepare_param_({"default", _}, _, _, _) -> ok;
% requred
prepare_param_(R = {"required",true}, undefined, Name, _) -> param_error(R, Name);
prepare_param_({"required",_}, _, _, _) -> ok;
prepare_param_(_, undefined, _, _) -> ok;
% {type, _}
prepare_param_(R = {"type", "binary"}, Value, Name, _) ->
    case is_binary(Value) of
        true -> ok;
        false -> param_error(R, Name)
    end;
prepare_param_({"type", "boolean"}, Value, _, _) when is_boolean(Value) ->
    ok;
prepare_param_(R = {"type", "boolean"}, Value, Name, _) ->
    V = list_to_binary(string:to_lower(to_list(Value))),
    try
        case binary_to_existing_atom(V, utf8) of
            B when is_boolean(B) -> {ok, B};
            _ -> param_error(R, Name)
        end
    catch
        error:badarg ->
            param_error(R, Name)
    end;
prepare_param_(R = {"type", "date"}, Value, Name, _) ->
    case is_binary(Value) of
        true -> ok;
        false -> param_error(R, Name)
    end;
prepare_param_(R = {"type", "datetime"}, Value, Name, _) ->
    case is_binary(Value) of
        true -> ok;
        false -> param_error(R, Name)
    end;
prepare_param_(R = {"type", "float"}, Value, Name, _) ->
    try {ok, to_float(Value)}
    catch
        error:badarg ->
            param_error(R, Name)
    end;
prepare_param_(R = {"type", "integer"}, Value, Name, _) ->
    try {ok, to_int(Value)}
    catch
        error:badarg ->
            param_error(R, Name)
    end;
prepare_param_({"type", "string"}, _, _, _) -> ok;
% schema
prepare_param_(R = {"schema", #{<<"$ref">> := <<"/definitions/", Ref/binary>>}},
               Value, Name, Validator) ->
    try
        Schema = #{<<"$ref">> => <<"#/definitions/", Ref/binary>>},
        jesse_schema_validator:validate_with_state(Schema, Value, Validator),
        {ok, Value, Ref}
    catch
        throw:[{schema_invalid, _, Error} | _] ->
            Info = #{
                type => schema_invalid,
                error => Error
            },
            param_error(R, Name, Info);
        throw:[{data_invalid, S, Error, _, Path} | _] ->
            Info = #{
                type => data_invalid,
                error => Error,
                schema => S,
                path => Path
            },
            param_error(R, Name, Info)
    end;
prepare_param_({"schema",#{<<"type">> := _}}, _, _, _) -> ok;
% enum
prepare_param_(R = {"enum", Values0}, Value0, Name, _) ->
    try
        Values = [ to_atom(Acc) || Acc <- Values0 ],
        Value = to_existing_atom(Value0),
        case lists:member(Value, Values) of
            true -> {ok, Value};
            false -> param_error(R, Name)
        end
    catch
        error:badarg ->
            param_error(R, Name)
    end;
% arythmetic
prepare_param_(R = {"minimum", Min}, Value, Name, _) ->
    case Value >= Min of
        true -> ok;
        false -> param_error(R, Name)
    end;
prepare_param_(R = {"maximum", Max}, Value, Name, _) ->
    case Value =< Max of
        true -> ok;
        false -> param_error(R, Name)
    end.

get_param_value("body", _, Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    case prepare_body(Body) of
        {error, Reason} ->
            {error, Reason, Req};
        Value ->
            {ok, Value, Req}
    end;
get_param_value("query", Name, Req) ->
    QS = cowboy_req:parse_qs(Req),
    Value = get_opt(to_qs(Name), QS),
    {ok, Value, Req};
get_param_value("header", Name, Req) ->
    Value = cowboy_req:header(to_header(Name), Req),
    {ok, Value, Req};
get_param_value("path", Name, Req) ->
    Value = cowboy_req:binding(to_binding(Name), Req),
    {ok, Value, Req}.

prepare_body(Body) ->
    case Body of
        <<"">> -> <<"">>;
        _ ->
            try
                jsx:decode(Body, [return_maps])
            catch
              error:_ ->
                {error, {invalid_body, not_json, Body}}
            end
    end.

param_error(Rule, Name) ->
    param_error(Rule, Name, #{}).

param_error(Rule, Name, Info) ->
    {error, {wrong_param, Name, Rule, Info}}.

%%====================================================================
%% Utils
%%====================================================================

-spec to_atom(binary() | list()) -> atom().

to_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, utf8);
to_atom(List) when is_list(List) ->
    list_to_atom(List).

-spec to_existing_atom(binary() | list()) -> atom().

to_existing_atom(Bin) when is_binary(Bin) ->
    binary_to_existing_atom(Bin, utf8);
to_existing_atom(List) when is_list(List) ->
    list_to_existing_atom(List).

-spec to_float(iodata()) -> number().

to_float(V) ->
    Data = iolist_to_binary([V]),
    case binary:split(Data, <<$.>>) of
        [Data] ->
            binary_to_integer(Data);
        [<<>>, _] ->
            binary_to_float(<<$0, Data/binary>>);
        _ ->
            binary_to_float(Data)
    end.

-spec to_int(integer() | binary() | list()) -> integer().

to_int(Data) when is_integer(Data) -> Data;
to_int(Data) when is_binary(Data) -> binary_to_integer(Data);
to_int(Data) when is_list(Data) -> list_to_integer(Data).

-spec to_list(iodata() | atom() | number()) -> string().

to_list(V) when is_list(V) -> V;
to_list(V)  -> binary_to_list(to_binary(V)).

-spec to_binary(iodata() | atom() | number()) -> binary().

to_binary(V) when is_binary(V)  -> V;
to_binary(V) when is_list(V)    -> iolist_to_binary(V);
to_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V)   -> float_to_binary(V).

-spec get_opt(any(), []) -> any().

get_opt(Key, Opts) ->
    get_opt(Key, Opts, undefined).

-spec get_opt(any(), [], any()) -> any().

get_opt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        false -> Default
    end.

-spec to_qs(iodata() | atom() | number()) -> binary().

to_qs(Name) -> to_binary(Name).

-spec to_header(iodata() | atom() | number()) -> binary().

to_header(Name) -> string:lowercase(to_binary(Name)).

-spec to_binding(iodata() | atom() | number()) -> atom().

to_binding(Name) ->
    Prepared = to_binary(Name),
    binary_to_atom(Prepared, utf8).

-spec to_method(binary()) -> atom().

to_method(Method) ->
    to_existing_atom(string:lowercase(Method)).
