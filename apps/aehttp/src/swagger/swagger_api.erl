-module(swagger_api).

-export([request_params/1]).
-export([request_param_info/2]).
-export([populate_request/3]).
-export([validate_response/4]).
%% exported to silence swagger complains
-export([get_value/3, validate_response_body/4]).

-type operation_id() :: atom().
-type request_param() :: atom().

-export_type([operation_id/0]).

-spec request_params(OperationID :: operation_id()) -> [Param :: request_param()].


request_params('GetAccountBalance') ->
    [
        'pub_key'
    ];

request_params('GetAccountsBalances') ->
    [
    ];

request_params('GetBlockByHash') ->
    [
        'hash'
    ];

request_params('GetBlockByHeight') ->
    [
        'height'
    ];

request_params('GetInfo') ->
    [
    ];

request_params('GetTop') ->
    [
    ];

request_params('GetTxs') ->
    [
    ];

request_params('GetVersion') ->
    [
    ];

request_params('Ping') ->
    [
        'Ping'
    ];

request_params('PostBlock') ->
    [
        'Block'
    ];

request_params('PostTx') ->
    [
        'Tx'
    ];


request_params('GetActiveRegisteredOracles') ->
    [
    ];

request_params('GetOracleQuestions') ->
    [
        'oracle_pub_key'
    ];

request_params('GetPubKey') ->
    [
    ];

request_params('PostOracleQueryTx') ->
    [
        'OracleQueryTx'
    ];

request_params('PostOracleRegisterTx') ->
    [
        'OracleRegisterTx'
    ];

request_params('PostOracleResponseTx') ->
    [
        'OracleResponseTx'
    ];

request_params('PostOracleSubscribe') ->
    [
        'OracleSubscribe'
    ];

request_params('PostOracleUnsubscribe') ->
    [
        'OracleSubscribe'
    ];

request_params('PostSpendTx') ->
    [
        'SpendTx'
    ];

request_params(_) ->
    error(unknown_operation).

-type rule() ::
    {type, 'binary'} |
    {type, 'integer'} |
    {type, 'float'} |
    {type, 'binary'} |
    {type, 'boolean'} |
    {type, 'date'} |
    {type, 'datetime'} |
    {enum, [atom()]} |
    {max, Max :: number()} |
    {exclusive_max, Max :: number()} |
    {min, Min :: number()} |
    {exclusive_min, Min :: number()} |
    {max_length, MaxLength :: integer()} |
    {min_length, MaxLength :: integer()} |
    {pattern, Pattern :: string()} |
    schema |
    required |
    not_required.

-spec request_param_info(OperationID :: operation_id(), Name :: request_param()) -> #{
    source => qs_val | binding | header | body,
    rules => [rule()]
}.



request_param_info('GetAccountBalance', 'pub_key') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('GetBlockByHash', 'hash') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('GetBlockByHeight', 'height') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'integer'},
            required
        ]
    };

request_param_info('Ping', 'Ping') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PostBlock', 'Block') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PostTx', 'Tx') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };


request_param_info('GetOracleQuestions', 'oracle_pub_key') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('PostOracleQueryTx', 'OracleQueryTx') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PostOracleRegisterTx', 'OracleRegisterTx') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PostOracleResponseTx', 'OracleResponseTx') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PostOracleSubscribe', 'OracleSubscribe') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PostOracleUnsubscribe', 'OracleSubscribe') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PostSpendTx', 'SpendTx') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info(OperationID, Name) ->
    error({unknown_param, OperationID, Name}).

-spec populate_request(
    OperationID :: operation_id(),
    Req :: cowboy_req:req(),
    ValidatorState :: jesse_state:state()
) ->
    {ok, Model :: #{}, Req :: cowboy_req:req()} |
    {error, Reason :: any(), Req :: cowboy_req:req()}.

populate_request(OperationID, Req, ValidatorState) ->
    Params = request_params(OperationID),
    populate_request_params(OperationID, Params, Req, ValidatorState, #{}).

populate_request_params(_, [], Req, _, Model) ->
    {ok, Model, Req};

populate_request_params(OperationID, [FieldParams | T], Req0, ValidatorState, Model) ->
    case populate_request_param(OperationID, FieldParams, Req0, ValidatorState) of
        {ok, K, V, Req} ->
            populate_request_params(OperationID, T, Req, ValidatorState, maps:put(K, V, Model));
        Error ->
            Error
    end.

populate_request_param(OperationID, Name, Req0, ValidatorState) ->
    #{rules := Rules, source := Source} = request_param_info(OperationID, Name),
    case get_value(Source, Name, Req0) of
        {error, Reason, Req} ->
            {error, Reason, Req};
        {Value, Req} ->
            case prepare_param(Rules, Name, Value, ValidatorState) of
                {ok, Result} -> {ok, Name, Result, Req};
                {error, Reason} ->
                    {error, Reason, Req}
            end
    end.

-spec validate_response(
    OperationID :: operation_id(),
    Code :: 200..599,
    Body :: jesse:json_term(),
    ValidatorState :: jesse_state:state()
) -> ok | no_return().


validate_response('GetAccountBalance', 200, Body, ValidatorState) ->
    validate_response_body('Balance', 'Balance', Body, ValidatorState);
validate_response('GetAccountBalance', 400, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);
validate_response('GetAccountBalance', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('GetAccountsBalances', 200, Body, ValidatorState) ->
    validate_response_body('AccountsBalances', 'AccountsBalances', Body, ValidatorState);
validate_response('GetAccountsBalances', 403, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('GetBlockByHash', 200, Body, ValidatorState) ->
    validate_response_body('Block', 'Block', Body, ValidatorState);
validate_response('GetBlockByHash', 400, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);
validate_response('GetBlockByHash', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('GetBlockByHeight', 200, Body, ValidatorState) ->
    validate_response_body('Block', 'Block', Body, ValidatorState);
validate_response('GetBlockByHeight', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('GetInfo', 200, Body, ValidatorState) ->
    validate_response_body('Info', 'Info', Body, ValidatorState);
validate_response('GetInfo', 403, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('GetTop', 200, Body, ValidatorState) ->
    validate_response_body('Top', 'Top', Body, ValidatorState);

validate_response('GetTxs', 200, Body, ValidatorState) ->
    validate_response_body('Transactions', 'Transactions', Body, ValidatorState);

validate_response('GetVersion', 200, Body, ValidatorState) ->
    validate_response_body('Version', 'Version', Body, ValidatorState);

validate_response('Ping', 200, Body, ValidatorState) ->
    validate_response_body('Ping', 'Ping', Body, ValidatorState);
validate_response('Ping', 403, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);
validate_response('Ping', 409, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostBlock', 200, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('PostBlock', 400, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostTx', 200, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('PostTx', 400, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);


validate_response('GetActiveRegisteredOracles', 200, Body, ValidatorState) ->
    validate_response_body('RegisteredOracles', 'RegisteredOracles', Body, ValidatorState);

validate_response('GetOracleQuestions', 200, Body, ValidatorState) ->
    validate_response_body('OracleQuestions', 'OracleQuestions', Body, ValidatorState);
validate_response('GetOracleQuestions', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('GetPubKey', 200, Body, ValidatorState) ->
    validate_response_body('PubKey', 'PubKey', Body, ValidatorState);
validate_response('GetPubKey', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostOracleQueryTx', 200, Body, ValidatorState) ->
    validate_response_body('OracleInteractionId', 'OracleInteractionId', Body, ValidatorState);
validate_response('PostOracleQueryTx', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostOracleRegisterTx', 200, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('PostOracleRegisterTx', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostOracleResponseTx', 200, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('PostOracleResponseTx', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostOracleSubscribe', 200, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('PostOracleSubscribe', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostOracleUnsubscribe', 200, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('PostOracleUnsubscribe', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);

validate_response('PostSpendTx', 200, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('PostSpendTx', 404, Body, ValidatorState) ->
    validate_response_body('Error', 'Error', Body, ValidatorState);


validate_response(_OperationID, _Code, _Body, _ValidatorState) ->
    ok.

validate_response_body('list', ReturnBaseType, Body, ValidatorState) ->
    [
        validate(schema, ReturnBaseType, Item, ValidatorState)
    || Item <- Body];

validate_response_body(_, ReturnBaseType, Body, ValidatorState) ->
    validate(schema, ReturnBaseType, Body, ValidatorState).

%%%
validate(Rule = required, Name, Value, _ValidatorState) ->
    case Value of
        undefined -> validation_error(Rule, Name);
        _ -> ok
    end;

validate(not_required, _Name, _Value, _ValidatorState) ->
    ok;

validate(_, _Name, undefined, _ValidatorState) ->
    ok;

validate(Rule = {type, 'integer'}, Name, Value, _ValidatorState) ->
    try
        {ok, swagger_utils:to_int(Value)}
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'float'}, Name, Value, _ValidatorState) ->
    try
        {ok, swagger_utils:to_float(Value)}
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'binary'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(_Rule = {type, 'boolean'}, _Name, Value, _ValidatorState) when is_boolean(Value) ->
    {ok, Value};

validate(Rule = {type, 'boolean'}, Name, Value, _ValidatorState) ->
    V = binary_to_lower(Value),
    try
        case binary_to_existing_atom(V, utf8) of
            B when is_boolean(B) -> {ok, B};
            _ -> validation_error(Rule, Name)
        end
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'date'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {type, 'datetime'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {enum, Values}, Name, Value, _ValidatorState) ->
    try
        FormattedValue = erlang:binary_to_existing_atom(Value, utf8),
        case lists:member(FormattedValue, Values) of
            true -> {ok, FormattedValue};
            false -> validation_error(Rule, Name)
        end
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {max, Max}, Name, Value, _ValidatorState) ->
    case Value =< Max of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {exclusive_max, ExclusiveMax}, Name, Value, _ValidatorState) ->
    case Value > ExclusiveMax of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {min, Min}, Name, Value, _ValidatorState) ->
    case Value >= Min of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {exclusive_min, ExclusiveMin}, Name, Value, _ValidatorState) ->
    case Value =< ExclusiveMin of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {max_length, MaxLength}, Name, Value, _ValidatorState) ->
    case size(Value) =< MaxLength of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {min_length, MinLength}, Name, Value, _ValidatorState) ->
    case size(Value) >= MinLength of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {pattern, Pattern}, Name, Value, _ValidatorState) ->
    {ok, MP} = re:compile(Pattern),
    case re:run(Value, MP) of
        {match, _} -> ok;
        _ -> validation_error(Rule, Name)
    end;

validate(Rule = schema, Name, Value, ValidatorState) ->
    Definition =  list_to_binary("#/definitions/" ++ swagger_utils:to_list(Name)),
    try
        _ = validate_with_schema(Value, Definition, ValidatorState),
        ok
    catch
        throw:[{schema_invalid, _, Error} | _] ->
            Info = #{
                type => schema_invalid,
                error => Error
            },
            validation_error(Rule, Name, Info);
        throw:[{data_invalid, Schema, Error, _, Path} | _] ->
            Info = #{
                type => data_invalid,
                error => Error,
                schema => Schema,
                path => Path
            },
            validation_error(Rule, Name, Info)
    end;

validate(Rule, Name, _Value, _ValidatorState) ->
    error_logger:info_msg("Can't validate ~p with ~p", [Name, Rule]),
    error({unknown_validation_rule, Rule}).

-spec validation_error(Rule :: any(), Name :: any()) -> no_return().

validation_error(ViolatedRule, Name) ->
    validation_error(ViolatedRule, Name, #{}).

-spec validation_error(Rule :: any(), Name :: any(), Info :: #{}) -> no_return().

validation_error(ViolatedRule, Name, Info) ->
    throw({wrong_param, Name, ViolatedRule, Info}).

-spec get_value(body | qs_val | header | binding, Name :: any(), Req0 :: cowboy_req:req()) ->
    {Value :: any(), Req :: cowboy_req:req()} | 
    {error, Reason :: any(), Req :: cowboy_req:req()}.
get_value(body, _Name, Req0) ->
    {ok, Body, Req} = cowboy_req:body(Req0),
    case prepare_body(Body) of
        {error, Reason} ->
            {error, Reason, Req};
        Value ->
            {Value, Req}
    end;

get_value(qs_val, Name, Req0) ->
    {QS, Req} = cowboy_req:qs_vals(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_qs(Name), QS),
    {Value, Req};

get_value(header, Name, Req0) ->
    {Headers, Req} = cowboy_req:headers(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_header(Name), Headers),
    {Value, Req};

get_value(binding, Name, Req0) ->
    {Bindings, Req} = cowboy_req:bindings(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_binding(Name), Bindings),
    {Value, Req}.

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

validate_with_schema(Body, Definition, ValidatorState) ->
    jesse_schema_validator:validate_with_state(
        [{<<"$ref">>, Definition}],
        Body,
        ValidatorState
    ).

prepare_param(Rules, Name, Value, ValidatorState) ->
    try
        Result = lists:foldl(
            fun(Rule, Acc) ->
                case validate(Rule, Name, Acc, ValidatorState) of
                    ok -> Acc;
                    {ok, Prepared} -> Prepared
                end
            end,
            Value,
            Rules
        ),
        {ok, Result}
    catch
        throw:Reason ->
            {error, Reason}
    end.

binary_to_lower(V) when is_binary(V) ->
    list_to_binary(string:to_lower(swagger_utils:to_list(V))).
