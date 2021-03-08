%% This code is generated from apps/aehttp/priv/oas3.yaml
%% Using swagger_endpoints rebar3 plugin version: 0.2.1
%% Do not manually change this code!
%%
%% json_schema/0 implements a JSON Schema for the definitions
%% Reference should be fixed!
%% Use jsx:prettify(jsx:encode(json_schema())) to get a JSON string.

-module(oas_endpoints).

-export([operation/1, operations/0, definitions/0, json_schema/0,
         validate_request/3, validate_response/4, path/3, query/3,
         validate/2]).

operations() ->
    [70,97,105,108,101,100,32,116,111,32,112,97,114,115,101,32,
 "\"apps/aehttp/priv/oas3.yaml\"",32,40,
 [123,
  ["case_clause",44,10,
   [32,
    [32,[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],
    32,
    [32,[32,"  ",32,32],32,"  ",32,32],
    32,
    [32,"  ",32,32],
    32,"  ",32,32],
   "\"#/components/parameters/intAsString\""],
  125],
 41].

definitions() ->
    error.

json_schema() ->
    maps:merge(#{<<"$schema">> =>
                     <<"http://json-schema.org/draft-04/schema#">>},
               #{<<"properties">> =>
                     maps:from_list([{list_to_binary(K), V}
                                     || {K, V} <- definitions()])}).

operation(Id) -> maps:get(Id, operations()).

path(Method, OperationId, Args) when is_map(Args) ->
    path(Method, OperationId, maps:to_list(Args));
path(Method, OperationId, Args) ->
    begin
        #{path := Endpoint, parameters := Parameters} =
            maps:get(Method, operation(OperationId)),
        InPath = [Param
                  || Param <- Parameters,
                     lists:member({"in", "path"}, Param)],
        lists:foldl(fun (Param, Path) ->
                            Name = proplists:get_value("name", Param),
                            case {proplists:get_value("required", Param, false),
                                  get_by_similar_key(Name, Args)}
                                of
                                {false, undefined} -> Path;
                                {true, undefined} ->
                                    throw({error,
                                           {required,
                                            Name,
                                            Param,
                                            OperationId}});
                                {_, {_Key, Value}} ->
                                    iolist_to_binary(string:replace(Path,
                                                                    "{" ++
                                                                        Name ++
                                                                            "}",
                                                                    to_str(Value)))
                            end
                    end,
                    Endpoint,
                    InPath)
    end.

query(Method, OperationId, Args) when is_map(Args) ->
    query(Method, OperationId, maps:to_list(Args));
query(Method, OperationId, Args) ->
    begin
        #{parameters := Parameters} = maps:get(Method,
                                               operation(OperationId)),
        InQuery = [Param
                   || Param <- Parameters,
                      lists:member({"in", "query"}, Param)],
        Query = lists:foldr(fun (Param, Query) ->
                                    Name = proplists:get_value("name", Param),
                                    case {proplists:get_value("required",
                                                              Param,
                                                              false),
                                          get_by_similar_key(Name, Args)}
                                        of
                                        {false, undefined} -> Query;
                                        {true, undefined} ->
                                            throw({error,
                                                   {required,
                                                    Name,
                                                    Param,
                                                    OperationId}});
                                        {_, {_, Value}} ->
                                            [{Name,
                                              http_uri:encode(to_str(Value))}
                                             | Query]
                                    end
                            end,
                            [],
                            InQuery),
        case [[K, "=", V] || {K, V} <- Query] of
            [] -> <<>>;
            Qs -> iolist_to_binary(["?" | lists:join("&", Qs)])
        end
    end.

prepare_validation() ->
    case ets:info(jesse_ets) of
        undefined ->
            [jesse:add_schema(Def, Schema)
             || {Def, Schema} <- definitions()];
        _ -> []
    end.

validate(Schema, Term) ->
    try jesse_schema_validator:validate(Schema, Term, [])
    catch
        Error -> {error, Error}
    end.

validate_request(OperationId, Method, Args)
    when is_map(Args) ->
    validate_request(OperationId,
                     Method,
                     maps:to_list(Args));
validate_request(OperationId, Method, Args)
    when is_list(Args) ->
    begin
        prepare_validation(),
        #{parameters := Parameters} = maps:get(Method,
                                               endpoints:operation(OperationId)),
        ToCheck = [Param
                   || Param <- Parameters,
                      not lists:member({"in", "path"}, Param)],
        Errors = lists:foldl(fun (Param, Errs) ->
                                     Name = proplists:get_value("name", Param),
                                     case {proplists:get_value("required",
                                                               Param,
                                                               false),
                                           get_by_similar_key(Name, Args)}
                                         of
                                         {false, undefined} -> Errs;
                                         {true, undefined} ->
                                             [{required,
                                               Name,
                                               Param,
                                               OperationId}
                                              | Errs];
                                         {_, {_, Value}} ->
                                             case
                                                 validate(proplists:get_value("schema",
                                                                              Param,
                                                                              #{}),
                                                          Value)
                                                 of
                                                 {error, E} -> [E | Errs];
                                                 _ -> Errs
                                             end
                                     end
                             end,
                             [],
                             ToCheck),
        case Errors of
            [] -> ok;
            _ -> {errors, {OperationId, Args, Errors}}
        end
    end.

validate_response(OperationId, Method, StatusCode,
                  Response) ->
    begin
        #{responses := Resps} = maps:get(Method,
                                         endpoints:operation(OperationId)),
        prepare_validation(),
        case maps:get(StatusCode, Resps, error) of
            error -> {error, {StatusCode, unspecified}};
            undefined -> {ok, StatusCode, Response};
            Schema ->
                case validate(Schema, Response) of
                    {ok, _} -> {ok, StatusCode, Response};
                    {error, E} -> {error, {validation, E}}
                end
        end
    end.

get_by_similar_key(Name, KVs) when is_list(Name) ->
    case lists:keyfind(Name, 1, KVs) of
        false ->
            case lists:keyfind(list_to_binary(Name), 1, KVs) of
                false ->
                    case lists:keyfind(catch list_to_existing_atom(Name),
                                       1,
                                       KVs)
                        of
                        false -> undefined;
                        AtomTuple -> AtomTuple
                    end;
                BinTuple -> BinTuple
            end;
        Tuple -> Tuple
    end.

to_str(Str) ->
    begin
        if is_list(Str); is_binary(Str) -> Str;
           true -> lists:concat([Str])
        end
    end.