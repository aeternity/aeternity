%%% @copyright (C) 2022, Aeternity
-module(aehttpc_btc).

%% Low level subset of bitcoin API required to interact with a hyperchain

%% Required exports:
-export([get_latest_block/5, get_commitment_tx_in_block/6, post_commitment/6]).

get_latest_block(Host, Port, User, Password, Seed) ->
    getbestblockhash(Host, Port, User, Password, Seed, true).

get_commitment_tx_in_block(Host, Port, User, Password, Seed, BlockHash) ->
    getblock(Host, Port, User, Password, Seed, true, BlockHash, _Verbosity = 2).

%% @doc Post commitment to BTC parent chain.
%% Commitment is a simple spend transaction to a known BTC account
%% Spend TX must be signed by the BTC account owned by the staking node
%% SpendTx must include the AE validator account pubkey in its MetaData
%% Bitcoin doc suggests 4 ops for this (https://gist.github.com/gavinandresen/2839617):
%% 1. Get a list of not-yet-spent outputs with listunspent
%% 2. Create a transaction using createrawtransaction
%% 3. Apply signatures using signrawtransaction
%% 4. Submit it using sendrawtransaction
post_commitment(_Host, _Port, _BTCAcc, _Signature, _AeValidatorPubkey, _Commitment) ->
    ok.


%%%===================================================================
%%%  BTC protocol
%%%===================================================================
-spec getbestblockhash(binary(), binary(), binary(), binary(), binary(), boolean()) -> {ok, binary()} | {error, term()}.
getbestblockhash(Host, Port, User, Password, Seed, SSL) ->
    try
        Body = jsx:encode(request_body(<<"getbestblockhash">>, [], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
        Hash = result(Res),
        {ok, from_hex(Hash)}
    catch E:R ->
        {error, {E, R}}
    end.

-spec getblock(binary(), binary(), binary(), binary(), binary(), boolean(), binary(), integer()) -> {ok, tuple()} | {error, term()}.
getblock(Host, Port, User, Password, Seed, SSL, Hash, Verbosity) ->
  try
    Seed = <<>>,
    Body = jsx:encode(request_body(<<"getblock">>, [Hash, Verbosity], seed_to_utf8(Seed))),
    {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
    Block = block(result(Res)),
    {ok, Block}
  catch E:R ->
    {error, {E, R}}
  end.

%% WIP - Suppress dializer warnings to keep build happy until these two functions
%% are hooked into post_commitment
-dialyzer({nowarn_function, signrawtransactionwithkey/8}).
-spec signrawtransactionwithkey(binary(), binary(), binary(), binary(), binary(), boolean(), binary(), binary()) -> {ok, binary()} | {error, term()}.
signrawtransactionwithkey(Host, Port, User, Password, Seed, SSL, RawTx, PrivKey) ->
  try
    Body = jsx:encode(request_body(<<"signrawtransactionwithkey">>, [RawTx, [PrivKey]], seed_to_utf8(Seed))),
    {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
    SignedTx = result(Res),
    Complete = maps:get(<<"complete">>, SignedTx), true = Complete,
    Hex = maps:get(<<"hex">>, SignedTx),
    {ok, Hex}
  catch E:R ->
    {error, {E, R}}
  end.

-dialyzer({nowarn_function, sendrawtransaction/7}).
-spec sendrawtransaction(binary(), binary(), binary(), binary(), binary(), boolean(), binary()) -> {ok, binary()} | {error, term()}.
sendrawtransaction(Host, Port, User, Password, Seed, SSL, Hex) ->
  try
    Body = jsx:encode(request_body(<<"sendrawtransaction">>, [Hex], seed_to_utf8(Seed))),
    {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
    {ok, result(Res)}
  catch E:R ->
    {error, {E, R}}
  end.

-spec result(map()) -> term().
result(Response) ->
  maps:get(<<"result">>, Response).

-spec block(map()) -> tuple().
block(Obj) ->
  HexHash = maps:get(<<"hash">>, Obj), true = is_binary(HexHash),
  Height = maps:get(<<"height">>, Obj), true = is_integer(Height),

  %% TODO: To analyze the size field;
  FilteredTxs = lists:filter(fun (Tx) -> is_nulldata(Tx) end, maps:get(<<"tx">>, Obj)),
  Txs = [tx(Tx)||Tx <- FilteredTxs],

  Hash = from_hex(HexHash),
  PrevHash = prev_hash(Obj),
  {Height, Hash, PrevHash, Txs}.

-spec request(binary(), binary(), binary(), integer(),binary(), binary(),boolean(),integer()) -> {ok, map()} | {error, term()}.
request(Path, Body, Host, Port, User, Password, SSL, Timeout) ->
  try
    Url = url(binary_to_list(Host), Port, SSL),
    Auth = auth(User, Password),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Headers = [
        {"Authorization", lists:concat(["Basic ", Auth])}
      ],
    Req = {UrlPath, Headers, "application/json", Body},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    {ok, {{_, 200 = _Code, _}, _, Res}} = httpc:request(post, Req, HTTPOpt, Opt),
    lager:info("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
    {ok, jsx:decode(list_to_binary(Res), [return_maps])}
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

url(Host, Port, true = _SSL) when is_list(Host), is_integer(Port) ->
  path("https://", Host, Port);
url(Host, Port, _) when is_list(Host), is_integer(Port) ->
  path("http://", Host, Port).

path(Scheme, Host, Port) ->
  lists:concat([Scheme, Host, ":", Port]).



-spec from_hex(binary()) -> binary().
from_hex(HexData) ->
  ToInt = fun (H, L) -> binary_to_integer(<<H, L>>,16) end,
  _Payload = << <<(ToInt(H, L))>> || <<H:8, L:8>> <= HexData >>.

-spec prev_hash(map()) -> null | binary().
prev_hash(Obj) ->
  HexPrevHash = maps:get(<<"previousblockhash">>, Obj, null),
  if is_binary(HexPrevHash) ->
        from_hex(HexPrevHash);
    true ->
        null
  end.

-spec is_nulldata(map()) -> boolean().
is_nulldata(Obj) ->
  Outputs = maps:get(<<"vout">>, Obj),
  Res = [Output || Output = #{ <<"scriptPubKey">> := #{ <<"type">> := T} } <- Outputs, T == <<"nulldata">>],
  Res /= [].

-spec tx(map()) -> {Pubkey :: binary(), Payload :: binary()}.
tx(Obj) ->
    HexPubKey = account(Obj),
    HexPayload = payload(Obj),
    %% HexPubKey = <<"0014c0be2b090aab44c79d0883c8f3bc5d32afbcc9a7">>,
    %% <<"6a", HexPayload/binary>>= <<"6a6b685f324a7a4857345a5842314346435a77344a465237535159524246357851357857415172615035486f357434384b4d50554b6e">>,

    PubKey = from_hex(HexPubKey),
    Payload = from_hex(HexPayload),
    {PubKey, Payload}.

-spec account(map()) -> binary().
account(#{ <<"vin">> := [#{ <<"txinwitness">> := [_, Res]}] }) ->
  Res.

-spec payload(map()) -> binary().
payload(Obj) ->
  Outputs = maps:get(<<"vout">>, Obj),
  [Res] = [Hex || #{ <<"scriptPubKey">> := #{ <<"hex">> := Hex, <<"type">> := T} } <- Outputs, T == <<"nulldata">>],
  Res.

seed_to_utf8(Seed) when is_binary(Seed) ->
    base64:encode(Seed).