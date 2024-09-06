%%% @copyright (C) 2022, Aeternity
-module(aehttpc_btc).

%% Low level subset of bitcoin API required to interact with a hyperchain

%% Required exports:
-export([get_latest_block/2,
         get_header_by_hash/3,
         get_header_by_height/3,
         hash_to_integer/1]).

-behavior(aehttpc).

%% Handy for use in test suites and other BTC based chains

get_latest_block(NodeSpec, Seed) ->
    case getbestblockhash(NodeSpec, Seed) of
        {ok, Hash} ->
            case getblock(NodeSpec, Seed, Hash, _Verbosity = 1) of
                {ok, {Height, Hash, PrevHash, _Txs}} ->
                    {ok, Hash, PrevHash, Height};
                Err = {error, _} ->
                    Err
            end;
        Err = {error, _} ->
            Err
    end.

get_header_by_hash(Hash, NodeSpec, Seed) ->
    {ok, {Height, Hash, PrevHash, _Txs}}
      = getblock(NodeSpec, Seed, Hash, _Verbosity = 1),
    {ok, Hash, PrevHash, Height}.

get_header_by_height(Height, NodeSpec, Seed) ->
    case getblockhash(NodeSpec, Seed, Height) of
        {ok, Hash} ->
            {ok, {_Height, Hash, PrevHash, _Txs}}
                = getblock(NodeSpec, Seed, Hash, _Verbosity = 1),
            {ok, Hash, PrevHash, Height};
        {error, not_found} -> {error, not_found}
    end.

hash_to_integer(Hash) ->
    binary_to_integer(Hash, 16).


%%%===================================================================
%%%  BTC protocol
%%%===================================================================
-spec getbestblockhash(aehttpc:node_spec(), binary()) -> {ok, binary()} | {error, term()}.
getbestblockhash(NodeSpec, Seed) ->
    try
        Body = jsx:encode(request_body(<<"getbestblockhash">>, [], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        Hash = result(Res),
        {ok, Hash}
    catch E:R ->
        {error, {E, R}}
    end.

-spec getblockhash(aehttpc:node_spec(), binary(), integer()) -> {ok, binary()} | {error, term()}.
getblockhash(NodeSpec, Seed, Height) ->
    try
      Body = jsx:encode(request_body(<<"getblockhash">>, [Height], seed_to_utf8(Seed))),
      case request(<<"/">>, Body, NodeSpec, 5000) of
        {ok, Res} ->
            Hash = result(Res),
            {ok, Hash};
        {error, not_found} ->
            {error, not_found}
      end
    catch E:R:S ->
      {error, {E, R, S}}
    end.

-spec getblock(aehttpc:node_spec(), binary(), binary(), integer()) -> {ok, tuple()} | {error, term()}.
getblock(NodeSpec, Seed, Hash, Verbosity) ->
    try
        Body = jsx:encode(request_body(<<"getblock">>, [Hash, Verbosity], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        Block = block(result(Res)),
        {ok, Block}
    catch E:R:S ->
        {error, {E, R, S}}
    end.

-spec result(map()) -> term().
result(Response) ->
    maps:get(<<"result">>, Response).

-spec block(map()) -> tuple().
block(Obj) ->
    Hash = maps:get(<<"hash">>, Obj), true = is_binary(Hash),
    Height = maps:get(<<"height">>, Obj), true = is_integer(Height),

    PrevHash = prev_hash(Obj),
    {Height, Hash, PrevHash, maps:get(<<"tx">>, Obj)}.


-spec request(binary(), binary(), aehttpc:node_spec(), integer()) -> {ok, map()} | {error, term()}.
request(Path, Body, NodeSpec, Timeout) ->
    try
        #{user := User, password := Password} = NodeSpec,
        Url = aehttpc:url(NodeSpec),
        Auth = auth(User, Password),
        UrlPath = lists:concat([Url, binary_to_list(Path)]),
        Headers = [
            {"Authorization", lists:concat(["Basic ", Auth])}
        ],
        Req = {UrlPath, Headers, "application/json", Body},
        HTTPOpt = [{timeout, Timeout}],
        Opt = [],
        case httpc:request(post, Req, HTTPOpt, Opt) of
            {ok, {{_, 200 = _Code, _}, _, Res}} ->
                %% lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
                {ok, jsx:decode(list_to_binary(Res), [return_maps])};
            {ok, {{_, 500 = _Code, _}, _, "{\"result\":null,\"error\":{\"code\":-8,\"message\":\"Block height out of range\"}" ++ _}} ->
                {error, not_found}
        end
    catch E:R:S ->
        lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
        {error, {E, R, S}}
    end.

request_body(Method, Params, Seed) ->
    #{
      <<"jsonrpc">> => <<"2.0">>,
      <<"method">> => Method,
      <<"params">> => Params,
      <<"id">> => base64:encode(Seed)
    }.

-spec auth(binary(), binary()) -> list().
auth(User, Password) when is_binary(User), is_binary(Password) ->
    base64:encode_to_string(lists:concat([binary_to_list(User), ":", binary_to_list(Password)])).

-spec prev_hash(map()) -> null | binary().
prev_hash(Obj) ->
    PrevHash = maps:get(<<"previousblockhash">>, Obj, null),
    if is_binary(PrevHash) ->
            PrevHash;
        true ->
            null
    end.

seed_to_utf8(Seed) when is_binary(Seed) ->
    base64:encode(Seed).

