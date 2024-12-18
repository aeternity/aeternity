%%% @copyright (C) 2022, Aeternity
-module(aehttpc_doge).

%% Low level subset of dogecoin API required to interact with a hyperchain

%% Required exports:
-export([get_latest_block/2,
         get_header_by_hash/3,
         get_header_by_height/3,
         hash_to_integer/1,
         get_chain_type/0,
         pin_to_pc/2,
         get_pin_by_tx_hash/2]).

%% Temporary exports for keeping useful chain utils around
-export([select_utxo/2,
unspent_to_satoshis/1,
float_btc_to_satoshi/1,
pad8/1,
create_outputs/6,
satoshi_to_btc/1,
drop_trailing_zeroes/1,
drop_until_point/1,
listunspent/1,
getblock/4,
createrawtransaction/3,
signrawtransaction/2,
sendrawtransaction/2,
getrawtransaction/2,
to_hex/1]).

-behavior(aehttpc).

get_latest_block(NodeSpec, Seed) ->
    {ok, Hash} = getbestblockhash(NodeSpec, Seed),
    {ok, {Height, Hash, PrevHash, Time, _Txs}}
        = getblock(NodeSpec, Seed, Hash, _Verbosity = 1),
    {ok, Hash, PrevHash, Height, Time}.

get_header_by_hash(Hash, NodeSpec, Seed) ->
    {ok, {Height, Hash, PrevHash, Time, _Txs}}
      = getblock(NodeSpec, Seed, Hash, _Verbosity = 1),
    {ok, Hash, PrevHash, Height, Time}.

get_header_by_height(Height, NodeSpec, Seed) ->
    case getblockhash(NodeSpec, Seed, Height) of
        {ok, Hash} ->
            {ok, {_Height, Hash, PrevHash, Time, _Txs}}
                = getblock(NodeSpec, Seed, Hash, _Verbosity = 1),
            {ok, Hash, PrevHash, Height, Time};
        {error, not_found} -> {error, not_found}
    end.

hash_to_integer(Hash) ->
    binary_to_integer(Hash, 16).

get_chain_type() ->
    {ok, doge}.

%%%=============================================================================
%%% Pinning
%%%=============================================================================

pin_to_pc({PinningData, _Who, Amount, Fee, _NetworkId, _SignModule}, NodeSpec) ->
    PinPayload = aeser_hc:encode_parent_pin_payload(PinningData),
    post_pin(NodeSpec, <<"msnDTk5YoFU5M3pSbFsjTqXCCPx6FcshPL">>, <<"my89WXb7qESppiVnPnSeEWqA1MMaNv8KYC">>, Amount, Fee, PinPayload).

get_pin_by_tx_hash(TxHash, NodeSpec) ->
    lager:debug("in doge with: ~p", [TxHash]),
    {ok, #{<<"hex">> := RawTx}} = getrawtransaction(NodeSpec, TxHash),
    lager:debug("raw_tx_hex: ~p", [RawTx]),
    decoderawtransaction(NodeSpec, RawTx).


% get_commitment_tx_in_block(NodeSpec, Seed, BlockHash, _PrevHash, ParentHCAccountPubKey) ->
%     {ok, {_Height, _Hash, __PrevHash, Txs}}
%       = getblock(NodeSpec, Seed, BlockHash, _Verbosity = 2),
%     Commitments = find_commitments(Txs, ParentHCAccountPubKey),
%     {ok, Commitments}.

% get_commitment_tx_at_height(NodeSpec, Seed, Height, ParentHCAccountPubKey) ->
%     {ok, Hash} = getblockhash(NodeSpec, Seed, Height),
%     {ok, {_Height, _Hash, _PrevHash, Txs}}
%       = getblock(NodeSpec, Seed, Hash, _Verbosity = 2),
%     Commitments = find_commitments(Txs, ParentHCAccountPubKey),
%     {ok, Commitments}.

%% @doc Post commitment to BTC parent chain.
%% Commitment is a simple spend transaction to a known BTC account
%% Spend TX must be signed by the BTC account owned by the staking node
%% SpendTx must include the AE validator account pubkey in its MetaData
%% Bitcoin doc suggests 4 ops for this (https://gist.github.com/gavinandresen/2839617):
%% 1. Get a list of not-yet-spent outputs with listunspent
%% 2. Create a transaction using createrawtransaction
%% 3. Apply signatures using signrawtransaction
%% 4. Submit it using sendrawtransaction

% post_commitment(NodeSpec, StakerPubkey, HCCollectPubkey, Amount, Fee, Commitment,
%                 _NetworkId, _SignModule) ->
%         post_commitment(NodeSpec, StakerPubkey, HCCollectPubkey, Amount, Fee, Commitment).

post_pin(NodeSpec, BTCAcc, HCCollectPubkey, Amount, Fee, PinPayload) ->
    {ok, Unspent} = listunspent(NodeSpec),
    lager:debug("unspent: ~p", [Unspent]),
    UnspentSatoshis = unspent_to_satoshis(Unspent),
    lager:debug("satoshis: ~p", [UnspentSatoshis]),
    {ok, {Inputs, TotalAmount}} = select_utxo(UnspentSatoshis, Fee + Amount),
    lager:debug("inputs, amount: ~p, ~p", [Inputs, TotalAmount]),
    Outputs = create_outputs(PinPayload, BTCAcc, HCCollectPubkey, TotalAmount, Amount, Fee),
    lager:debug("outputs: ~p" , [Outputs]),
    {ok, Tx} = createrawtransaction(NodeSpec, Inputs, Outputs),
    lager:debug("tx: ~p", [Tx]),
    {ok, SignedTx} = signrawtransaction(NodeSpec, Tx),
    lager:debug("signed: ~p", [SignedTx]),
    {ok, TxHash} = sendrawtransaction(NodeSpec, SignedTx),
    lager:debug("txhash: ~p", [TxHash]),
    {ok, #{<<"tx_hash">> => TxHash}}.

select_utxo([#{<<"spendable">> := true, <<"amount">> := Amount} = Unspent | _Us], Needed) when Amount >= Needed ->
    %% For now just pick first spendable UTXO with enough funds. There is no end to how fancy this can get
    %% https://bitcoin.stackexchange.com/questions/32145/what-are-the-trade-offs-between-the-different-algorithms-for-deciding-which-utxo
    #{<<"txid">> := TxId, <<"vout">> := VOut} = Unspent,
    {ok, {[#{<<"txid">> => TxId, <<"vout">> => VOut}], Amount}};
select_utxo([_U|Us], Fee) ->
    select_utxo(Us, Fee);
select_utxo([], _Fee) ->
    {error, no_suitable_utxo}.

unspent_to_satoshis([#{<<"amount">> := Amount} = U | Us]) when is_float(Amount) ->
    Sats = float_btc_to_satoshi(Amount),
    [maps:put(<<"amount">>, Sats, U) | unspent_to_satoshis(Us)];
unspent_to_satoshis([U|Us]) ->
    [U|unspent_to_satoshis(Us)];
unspent_to_satoshis([]) ->
    [].

-define(SATOSHIS_PER_BTC, 100000000).
float_btc_to_satoshi(Amount) ->
    SatsStr = erlang:float_to_list(Amount, [{decimals, 8}, compact]),
    [BTCStr, SatStr] = string:tokens(SatsStr, "."),
    list_to_integer(BTCStr) * ?SATOSHIS_PER_BTC + list_to_integer(pad8(SatStr)).

pad8(Str) when length(Str) == 8 -> Str;
pad8(Str) when length(Str) < 8 ->
    Padding = lists:duplicate(8 - length(Str), $0),
    Str ++ Padding.

create_outputs(PinPayload, BTCAcc, HCCollectPubkey, TotalAmount, Amount, Fee) ->
    %% Three outputs needed:
    %% 1. for the OP_RETURN containing the Commitment
    %% 2. to return the total UTXO amount minus the Fee and Amount to our own account.
    %% 3. to register this transaction against the common HC BTC account
    %% The "data" field gets magically turned by the bitcoind API into an output
    %% in a correctly formatted OP_RETURN
    Refund = satoshi_to_btc(TotalAmount - float_btc_to_satoshi(Fee) - Amount),
    HCAmount = satoshi_to_btc(Amount),
    HexPinPayload = to_hex(PinPayload),
    #{BTCAcc => Refund, HCCollectPubkey => HCAmount, <<"data">> => HexPinPayload}.

satoshi_to_btc(Sats) when Sats >= 0 ->
    SatsStr = integer_to_list(Sats),
    NumDigits = length(SatsStr),
    BTCStr = if NumDigits =< 8 ->
                    "0." ++ lists:duplicate(8 - NumDigits, $0) ++ SatsStr;
                NumDigits > 8 ->
                    {A,B} = lists:split(NumDigits - 8, SatsStr),
                    A ++ "." ++ B
             end,
    list_to_binary(drop_trailing_zeroes(BTCStr)).

drop_trailing_zeroes(BTCStr) ->
    lists:reverse(drop_until_point(lists:reverse(BTCStr))).

drop_until_point([_, $. | _] = X) -> X;
drop_until_point("0" ++ X) -> drop_until_point(X);
drop_until_point(X) -> X.

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

-spec getblockhash(aehttpc:node_spec(), binary(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
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

%% doge and bitcoin core differ here. bitcoind at Verbosity 2 will return fully detailed txs
%% dogecoin 1.14.6 doesn't have level 2 so we must fetch all the Txs separately.
-spec getblock(aehttpc:node_spec(), binary(), binary(), integer()) -> {ok, tuple()} | {error, term()}.
getblock(NodeSpec, Seed, Hash, 2) ->
    try
        Body = jsx:encode(request_body(<<"getblock">>, [Hash, true], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        Obj = result(Res),
        TxHashes = maps:get(<<"tx">>, Obj),
        Txs = lists:flatmap(
            fun(TxHash) ->
                case getrawtransaction(NodeSpec, TxHash) of
                    {ok, Tx} -> [Tx];
                    _ -> []
                end
            end, TxHashes),
        Block = block(maps:put(<<"tx">>, Txs, Obj)),
        {ok, Block}
    catch E:R:S ->
        {error, {E, R, S}}
    end;
getblock(NodeSpec, Seed, Hash, _Verbosity) ->
    try
        Body = jsx:encode(request_body(<<"getblock">>, [Hash, true], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        Block = block(result(Res)),
        {ok, Block}
    catch E:R:S ->
        {error, {E, R, S}}
    end.

listunspent(NodeSpec) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"listunspent">>, [0, 9999999], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        Unspent = result(Res),
        {ok, Unspent}
    catch E:R ->
        {error, {E, R}}
    end.

createrawtransaction(NodeSpec, Inputs, Outputs) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"createrawtransaction">>, [Inputs, Outputs], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        Tx = result(Res),
        {ok, Tx}
    catch E:R ->
        {error, {E, R}}
    end.

%% Rely on the wallet embedded in the local bitcoind
%% FIXME: Allow this to use a different host/port so users can use a separate offline bitcoind
%% holding their commitments wallet.
-spec signrawtransaction(aehttpc:node_spec(), binary()) -> {ok, binary()} | {error, term()}.
signrawtransaction(NodeSpec, RawTx) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"signrawtransaction">>, [RawTx], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        SignedTx = result(Res),
        Complete = maps:get(<<"complete">>, SignedTx),
        true = Complete,
        Hex = maps:get(<<"hex">>, SignedTx),
        {ok, Hex}
    catch E:R ->
        {error, {E, R}}
    end.

-spec sendrawtransaction(aehttpc:node_spec(), binary()) -> {ok, binary()} | {error, term()}.
sendrawtransaction(NodeSpec, Hex) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"sendrawtransaction">>, [Hex], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        {ok, result(Res)}
    catch E:R ->
        {error, {E, R}}
    end.

-spec getrawtransaction(aehttpc:node_spec(), binary()) -> {ok, binary()} | {error, term()}.
getrawtransaction(NodeSpec, TxHash) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"getrawtransaction">>, [TxHash, true], seed_to_utf8(Seed))),
        lager:debug("gettxbody: ~p", [Body]),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        lager:debug("httpres:", [Res]),
        {ok, result(Res)}
    catch E:R ->
        {error, {E, R}}
    end.

    -spec decoderawtransaction(aehttpc:node_spec(), binary()) -> {ok, binary()} | {error, term()}.
decoderawtransaction(NodeSpec, Tx) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"decoderawtransaction">>, [Tx], seed_to_utf8(Seed))),
        lager:debug("decode body: ~p", [Body]),
        {ok, Res} = request(<<"/">>, Body, NodeSpec, 5000),
        lager:debug("decoderes:", [Res]),
        {ok, result(Res)}
    catch E:R ->
        {error, {E, R}}
    end.


-spec result(map()) -> term().
result(Response) ->
    maps:get(<<"result">>, Response).

-spec block(map()) -> tuple().
block(Obj) ->
    Hash = maps:get(<<"hash">>, Obj), true = is_binary(Hash),
    Height = maps:get(<<"height">>, Obj), true = is_integer(Height),
    Time = maps:get(<<"time">>, Obj), true = is_integer(Time),
    PrevHash = prev_hash(Obj),
    {Height, Hash, PrevHash, Time * 1000, maps:get(<<"tx">>, Obj)}.

-spec find_pin(list(), binary()) -> [{Pubkey :: binary(), Payload :: binary()}].
find_pin(Txs, _ParentHCAccountPubKey) ->
    lists:foldl(fun(#{<<"vout">> := Vout}, Acc) ->
                        case parse_vout(Vout) of
                            {ok, ParsedPin} ->
                                [ParsedPin | Acc];
                            {error, _Reason} ->
                                Acc
                        end
                end, [], Txs).

parse_vout(Vout) when length(Vout) == 3 ->
    case find_op_return(Vout) of
        {ok, PinEnc} ->
            PinBTC = from_hex(PinEnc),
            case PinTC of
                <<Pin:80/binary>> ->
                    case aeser_hc:decode_parent_pin_payload(Pin) of
                        {btc, Signature, StakerHash, TopKeyHash} ->
                            {ok, {Signature, StakerHash, TopKeyHash}};
                        _ ->
                            {error, not_btc_pin}
                    end;
                _ ->
                    {error, not_pin_op}
            end;
        _ ->
            {error, no_op_return}
    end;
parse_vout(_Vout) ->
    {error, not_pin}.

find_op_return([#{<<"scriptPubKey">> := #{<<"type">> := <<"nulldata">>, <<"asm">> := <<"OP_RETURN ", PinEnc/binary>>}} | _]) ->
    {ok, PinEnc};
find_op_return([_ | Vs]) ->
    find_op_return(Vs);
find_op_return([]) ->
    {error, no_op_return}.

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
                lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
                {ok, jsx:decode(list_to_binary(Res), [return_maps])};
             {ok, {{_, 500 = _Code, _}, _, "{\"result\":null,\"error\":{\"code\":-8,\"message\":\"Block height out of range\"}" ++ _}} ->
                {error, not_found};
             {ok, {{_, 500 = _Code, _}, _, "{\"result\":null,\"error\":{\"code\":-5,\"message\":\"No such mempool or blockchain" ++ _}} ->
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

-spec to_hex(binary()) -> binary().
to_hex(Payload) ->
    list_to_binary(aeu_hex:bin_to_hex(Payload)).

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



