-module(aehttp_client).

-export([
         request/3, request/7,
         base_url/3
        ]).

request(BaseUrl, OpId, Params) ->
    Method = operation_method(OpId),
    Path = operation_path(Method, OpId, Params),
    Params1 = maybe_encode_params(Params),
    request(Method, BaseUrl, Path, Params1, [], [], []).

request(get, BaseUrl, Path, QueryParams, Headers, HttpOpts, Opts) ->
    Url = make_url(BaseUrl, Path, QueryParams),
    ct:log("GET ~p", [Url]),
    Resp = httpc:request(get, {Url, Headers}, HttpOpts, Opts),
    process_response(Resp);
request(post, BaseUrl, Path, BodyParams, Headers, HttpOpts, Opts) ->
    Url = make_url(BaseUrl, Path, #{}),
    {ContentType, Body} = make_body(BodyParams, Path),
    ct:log("POST ~p~nContentType ~p~n~p", [Url, ContentType, BodyParams]),
    Resp = httpc:request(post, {Url, Headers, ContentType, Body}, HttpOpts, Opts),
    process_response(Resp).

base_url(Scheme, Addr, Port) ->
    Url = [atom_to_list(Scheme), <<"://">>, Addr, $:, integer_to_list(Port)],
    iolist_to_binary(Url).

maybe_encode_params(#{recipient_pubkey := PubKey} = Params) ->
    EncPubKey = aec_base58c:encode(account_pubkey, PubKey),
    maps:update(recipient_pubkey, EncPubKey, Params).

process_response({ok, {{_, Code, _State}, _Head, Body}}) ->
    try
        case iolist_to_binary(Body) of
            <<>> ->
                ct:log("Return code: ~p", [Code]),
                {ok, Code, #{}};
            B ->
                B1 = jsx:decode(B, [return_maps]),
                ct:log("Return code: ~p, body: ~p", [Code, B1]),
                {ok, Code, B1}
        end
    catch
        error:E -> {error, {parse_error, [E, erlang:get_stacktrace()]}}
    end;
process_response({error, _} = Error) ->
    Error.

operation_method(OpId) ->
    case endpoints:operation(OpId) of
        #{get := _OpSpec} -> get;
        #{post := _OpSpec} -> post
    end.

operation_path(Method, OpId, Params) ->
    endpoints:path(Method, OpId, Params).

make_url(BaseUrl, Path, Params) ->
    Url = [BaseUrl, Path, make_query(maps:to_list(Params))],
    binary_to_list(iolist_to_binary(Url)).

make_query([{K, V} | T]) ->
    [$?, [encode(K), $= , encode(V)
          | [[$&, encode(K1), $=, encode(V1)] || {K1, V1} <- T]]];
make_query([]) ->
    [].

make_body(Params, _Path) when is_map(Params) ->
    {"application/json", jsx:encode(Params)};
make_body([], Path) ->
    {"application/x-www-form-urlencoded", http_uri:encode(Path)}.

encode(X) when is_atom(X) -> encode(atom_to_list(X));
encode(X) when is_integer(X) -> encode(integer_to_list(X));
encode(X) when is_list(X); is_binary(X) -> http_uri:encode(X).

