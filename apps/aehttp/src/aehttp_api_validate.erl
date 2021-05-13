-module(aehttp_api_validate).

-export([request/5]).
-export([response/6]).
-export([validator/1]).
-export([json_spec/1]).

-define(ETS_TABLE, swagger_json).


%% @doc
%% Get the json specification and keep it cached, such that if we have read it once, we don't read it again.
-spec json_spec(aehttp_spec:version()) -> jsx:json_text().
json_spec(SpecVsn) ->
    EtsKey = {spec, SpecVsn},
    try
        [{EtsKey, CachedJson}] = ets:lookup(?ETS_TABLE, EtsKey),
        CachedJson
    catch
        _:_ ->
            case ets:whereis(?ETS_TABLE) of
                undefined ->
                    ets:new(?ETS_TABLE, [named_table, {read_concurrency, true}, public]);
                _ -> pass
            end,
            Json = aehttp_spec:json(SpecVsn),
            ets:insert(?ETS_TABLE, {EtsKey, Json}),
            Json
    end.

-spec validator(aehttp_spec:version()) -> jesse_state:state().
validator(SpecVsn) ->
    EtsKey = {validator, SpecVsn},
    try
        [{EtsKey, CachedValidator}] = ets:lookup(?ETS_TABLE, EtsKey),
        CachedValidator
    catch
        _:_ ->
            Json = json_spec(SpecVsn),
            R = jsx:decode(Json),
            Validator = jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
            ets:insert(?ETS_TABLE, {EtsKey, Validator}),
            Validator
    end.

-spec response(
    OperationId :: atom(),
    Method :: binary(),
    Code :: 200..599,
    Response :: jesse:json_term(),
    Validator :: jesse_state:state(),
    EndpointsMod :: module()
    ) -> ok | no_return().

response(OperationId, Method, Code, Response, Validator, EndpointsMod) ->
    try response_(OperationId, Method, Code, Response, Validator, EndpointsMod)
    catch
	error:R:S ->
	    lager:debug("CAUGHT ~p / ~p", [R, S]),
	    error(R)
    end.

response_(_OperationId, _Method0, Code, _Response, _Validator, _EndpointsMod) when Code >= 500 andalso Code < 600 ->
    ok;
response_(OperationId, Method0, Code, Response, Validator, EndpointsMod) ->
    Method = to_method(Method0),
    #{responses := Resps} = maps:get(Method, EndpointsMod:operation(OperationId)),
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
        Schema0 ->
            Schema = fix_def_refs(Schema0),
            _ = jesse_schema_validator:validate_with_state(Schema, Response, Validator),
            ok
    end.

fix_def_refs(Map) ->
    fix_def_refs(unused, Map).

fix_def_refs(_, Map) when is_map(Map) ->
    %% Recursively fix refs (and [drop extra, to be ignored,
    %% fields](https://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03#section-3)
    %% since jesse is broken)
    case maps:get(<<"$ref">>, Map, none) of
        none -> maps:map(fun fix_def_refs/2, Map);
        Ref  -> #{ <<"$ref">> => <<"#", Ref/binary>> }
    end;
fix_def_refs(_, X) ->
    X.

-spec request(
    OperationId :: atom(),
    Methohd :: binary(),
    Req :: cowboy_req:req(),
    Validator :: jesse_state:state(),
    Mod :: module()
    ) ->
    {ok, Model :: #{}, cowboy_req:req()}
    | {error, Reason :: any(), cowboy_req:req()}.

request(OperationId, Method0, Req, Validator, EndpointsMod) ->
    Method = to_method(Method0),
    #{parameters := Params} = maps:get(Method, EndpointsMod:operation(OperationId)),
    params(Params, #{}, Req, Validator, EndpointsMod).

params([], Model, Req, _, _) -> {ok, Model, Req};
params([Param | Params], Model, Req0, Validator, EndpointsMod) ->
   case populate_param(Param, Req0, Validator, EndpointsMod) of
        {ok, K, V, Req} ->
            NewModel = maps:put(to_atom(K), V, Model),
            params(Params, NewModel, Req, Validator, EndpointsMod);
        Error ->
            Error
    end.

populate_param(Param, Req, Validator, EndpointsMod) ->
    In = proplists:get_value("in", Param),
    Name = proplists:get_value("name", Param),
    case get_param_value(In, Name, Req) of
        {ok, Value, Req1} ->
            case prepare_param(params_priority(Param), Value, Name, Validator, EndpointsMod) of
                {ok, NewName, NewValue} -> {ok, NewName, NewValue, Req1};
                {error, Reason} -> {error, Reason, Req1}
            end;
        Error -> Error
    end.

params_priority(Rules) ->
    Priority =
        fun("default") -> 1;
           (_) -> 100
        end,
    lists:sort(
        fun({K1, _}, {K2, _}) ->
            Priority(K1) =< Priority(K2)
        end,
        Rules).

prepare_param([], Value, Name, _, _) -> {ok, Name, Value};
prepare_param([ Rule | Rules ], Value, Name, Validator, EndpointsMod) ->
    case prepare_param_(Rule, Value, Name, Validator, EndpointsMod) of
        ok -> prepare_param(Rules, Value, Name, Validator, EndpointsMod);
        {ok, NewValue} -> prepare_param(Rules, NewValue, Name, Validator, EndpointsMod);
        {ok, NewValue, NewName} -> prepare_param(Rules, NewValue, NewName, Validator, EndpointsMod);
        Error -> Error
    end.

% unused rules
prepare_param_({"in", _}, _, _, _, _) -> ok;
prepare_param_({"name", _}, _, _, _, _) -> ok;
prepare_param_({"description", _}, _, _, _, _) -> ok;
prepare_param_({"example", _}, _, _, _, _) -> ok;
prepare_param_({"default", Default}, _Val = undefined, _, _, _) -> {ok, Default};
prepare_param_({"default", _Default}, _, _, _, _) -> ok;
% required
prepare_param_({"required",true}, undefined, Name, _, _) -> param_error(required, Name);
prepare_param_({"required",_}, _, _, _, _) -> ok;
prepare_param_({"type", "binary"}, Value, Name, _, _) ->
    case is_binary(Value) of
        true -> ok;
        false -> param_error({type, Value}, Name)
    end;
prepare_param_({"type", "boolean"}, Value, _, _, _) when is_boolean(Value) ->
    ok;
prepare_param_({"type", "boolean"}, Value, Name, _, _) ->
    V = list_to_binary(string:to_lower(to_list(Value))),
    try
        case binary_to_existing_atom(V, utf8) of
            B when is_boolean(B) -> {ok, B};
            _ -> param_error({type, Value}, Name)
        end
    catch
        error:badarg ->
            param_error({type, Value}, Name)
    end;
prepare_param_({"type", "date"}, Value, Name, _, _) ->
    case is_binary(Value) of
        true -> ok;
        false -> param_error({type, Value}, Name)
    end;
prepare_param_({"type", "datetime"}, Value, Name, _, _) ->
    case is_binary(Value) of
        true -> ok;
        false -> param_error({type, Value}, Name)
    end;
prepare_param_({"type", "float"}, Value, Name, _, _) ->
    try {ok, to_float(Value)}
    catch
        error:badarg ->
            param_error({type, Value}, Name)
    end;
prepare_param_({"type", "integer"}, Value, Name, _, _) ->
    try {ok, to_int(Value)}
    catch
        error:badarg ->
            param_error({type, Value}, Name)
    end;
prepare_param_({"type", "string"}, _, _, _, _) -> ok;
% schema
prepare_param_({"schema", #{<<"$ref">> := FullRef}},
               Value, Name, Validator, EndpointsMod) ->
    try
        Prefix = list_to_binary(EndpointsMod:definitions_prefix()),
        Ref =
            case EndpointsMod:definitions_prefix() of
                "/definitions/" ->
                    <<"/definitions/", Ref0/binary>> = FullRef,
                    Ref0;
                "/components/schemas/" ->
                    <<"/components/schemas/", Ref0/binary>> = FullRef,
                    Ref0
            end,
        Schema = #{<<"$ref">> => <<"#", Prefix/binary, Ref/binary>>},
        jesse_schema_validator:validate_with_state(Schema, Value, Validator),
        {ok, Value, Ref}
    catch
        throw:[ Reason | _] ->
            Info0 = jesse_error:reason_to_jsx(Reason),
            Info1 = proplists:delete(schema, Info0),
            Info2 = proplists:delete(invalid, Info1),
            param_error({schema, Info2}, Name)
    end;
prepare_param_({"schema", [{"type",  _} = _Type | _] = Schema}, Value, Name, Validator,
               EndpointsMod) ->
    case prepare_param(params_priority(Schema), Value, Name, Validator, EndpointsMod) of
        {ok, NewName, NewValue} -> {ok, NewValue, NewName};
        {error, Err} -> 
            param_error_(Name, #{error => Err, schema => Schema, value => Value})
    end;
prepare_param_({"enum", Values0}, Value0, Name, _, _) ->
    try
        Values = [ to_atom(Acc) || Acc <- Values0 ],
        Value = 
            case Value0 of
                undefined -> %% maybe a missing default value?
                    undefined;
                _ -> to_existing_atom(Value0)
            end,
        case lists:member(Value, Values) of
            true -> {ok, Value};
            false -> param_error({enum, Value0}, Name)
        end
    catch
        error:badarg ->
            param_error({enum, Value0}, Name)
    end;
% arythmetic
prepare_param_({"minimum", Min}, Value, Name, _, _) ->
    case Value >= Min of
        true -> ok;
        false -> param_error({not_in_range, Value}, Name)
    end;
prepare_param_({"maximum", Max}, Value, Name, _, _) ->
    case Value =< Max of
        true -> ok;
        false -> param_error({not_in_range, Value}, Name)
    end.

get_param_value("body", _, Req0) ->
    %% Cowboy will attempt to read up to ~5MB of data for up to 10s. The call
    %% will return when there is up to ~5MB of data or at the end of the 10s
    %% period. If there is more data to read (after reading the initial 5MB),
    %% {more, Body, Req} is returned and we can consider the request body to
    %% be too big and the request is invalid.
    case cowboy_req:read_body(Req0, #{length => 5000000, period => 10000}) of
        {ok, <<>>, Req} -> {ok, <<>>, Req};
        {ok, Body, Req} ->
            try
                Value = jsx:decode(Body, [return_maps]),
                {ok, Value, Req}
            catch
              error:_ ->
                {error, Reason} = param_error({body, Body}, <<>>),
                {error, Reason, Req}
            end;
        {more, Body, Req} ->
            {error, Reason} = param_error({body_too_big, Body}, <<>>),
            {error, Reason, Req}
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

param_error({enum, Value}, Name) ->
    param_error_(Name, #{error => not_in_enum, data => Value});
param_error({not_in_range, Value}, Name) ->
    param_error_(Name, #{error => not_in_range, data => Value});
param_error(required, Name) ->
    param_error_(Name, #{error => missing_required_property});
param_error({schema, Info}, Name) ->
    param_error_(Name, Info);
param_error({type, Value}, Name) ->
    param_error_(Name, #{error => wrong_type, data => Value});
param_error({body, Value}, Name) ->
    param_error_(Name, #{error => invalid_body, data => Value});
param_error({body_too_big, Value}, Name) ->
    param_error_(Name, #{error => body_too_big, data => Value}).

param_error_(Name, Info) ->
    {error, {validation_error, to_binary(Name), Info}}.


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
