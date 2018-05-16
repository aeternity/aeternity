-module(aehttp_client).

-export([ request/3 ]).

%%=============================================================================
%% API
%%=============================================================================

%% Required config values: int_http, ext_http
%% Optional config values: ct_log (true by default)
request(OpId, Params, Cfg) ->
    Op = endpoints:operation(OpId),
    {Method, Interface} = operation_spec(Op),
    BaseUrl = string:trim(proplists:get_value(Interface, Cfg), trailing, "/"),
    Path = endpoints:path(Method, OpId, Params),
    Query = endpoints:query(Method, OpId, Params),
    inets:start(httpc, [{profile, test_browser}]),
    Response = request(Method, BaseUrl, Path, Query, Params, [], [{timeout, 15000}], [], Cfg),
    inets:stop(httpc, test_browser),
    Response.

request(get, BaseUrl, Path, Query, _Params, Headers, HttpOpts, Opts, Cfg) ->
    Url = binary_to_list(iolist_to_binary([BaseUrl, Path, Query])),
    log("GET ~p", [Url], Cfg),
    Resp = httpc:request(get, {Url, Headers}, HttpOpts, Opts, test_browser),
    process_response(Resp, Cfg);
request(post, BaseUrl, Path, _Query, BodyParams, Headers, HttpOpts, Opts, Cfg) ->
    Url = binary_to_list(iolist_to_binary([BaseUrl, Path])),
    {ContentType, Body} = make_body(BodyParams, Path),
    log("POST ~p~nContentType ~p~n~p", [Url, ContentType, BodyParams], Cfg),
    Resp = httpc:request(post, {Url, Headers, ContentType, Body},
                         HttpOpts, Opts, test_browser),
    process_response(Resp, Cfg).

%%=============================================================================
%% Internal functions
%%=============================================================================

process_response({ok, {{_, Code, _State}, _Head, Body}}, Cfg) ->
  %% httpc request format
  process_response({ok, Code, _Head, Body}, Cfg);
process_response({ok, Code, _Head, Body}, Cfg) ->
    case iolist_to_binary(Body) of
        <<>> ->
            log("Return code: ~p", [Code], Cfg),
            {ok, Code, #{}};
        Body1 ->
            Body2 = jsx:decode(Body1, [{labels, attempt_atom}, return_maps]),
            log("Return code: ~p, body: ~p", [Code, Body2], Cfg),
            {ok, Code, Body2}
    end;
process_response({error, _} = Error, _Cfg) ->
    Error.

log(Fmt, Params, Cfg) ->
    case proplists:get_value(ct_log, Cfg, true) of
        true -> ct:log(Fmt, Params);
        F when is_function(F) -> F(Fmt, Params);
        false -> ok
    end.

operation_spec(#{get := GetSpec}) ->
    {get, operation_interface(GetSpec)};
operation_spec(#{post := PostSpec}) ->
    {post, operation_interface(PostSpec)}.

operation_interface(#{tags := Tags}) ->
    IsExt = lists:member(<<"external">>, Tags),
    IsInt = lists:member(<<"internal">>, Tags),
    case {IsExt, IsInt} of
        {true, _} -> ext_http;
        {_, true} -> int_http
    end.

make_body(Params, _Path) when is_map(Params) ->
    {"application/json", jsx:encode(Params)};
make_body([], Path) ->
    {"application/x-www-form-urlencoded", http_uri:encode(Path)}.

