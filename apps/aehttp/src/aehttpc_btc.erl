%%% @copyright (C) 2022, Aeternity
-module(aehttpc_btc).

%% Low level subset of bitcoin API required to interact with a hyperchain

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
createrawtransaction/3,
signrawtransactionwithwallet/2,
sendrawtransaction/2,
from_hex/1,
find_op_return/1,
to_hex/1]).


-behavior(aehttpc).

-type hex() :: binary().%[byte()].

get_latest_block(NodeSpec, Seed) ->
    case getbestblockhash(NodeSpec, Seed) of
        {ok, Hash} ->
            case getblock(NodeSpec, Seed, Hash, _Verbosity = 1) of
                {ok, {Height, Hash, PrevHash, Time, _Txs}} ->
                    {ok, Hash, PrevHash, Height, Time};
                Err = {error, _} ->
                    Err
            end;
        Err = {error, _} ->
            Err
    end.

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
    {ok, btc}.


%%%=============================================================================
%%% Pinning
%%%=============================================================================

pin_to_pc({_PinningData, _Who, _Amount, _Fee, _NetworkId, _SignModule}, _NodeSpec) ->
    erlang:error(not_implemented).

get_pin_by_tx_hash(_TxHash, _NodeSpec) ->
    erlang:error(not_implemented).


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

create_outputs(Commitment, BTCAcc, HCCollectPubkey, TotalAmount, Amount, Fee) ->
    %% Three outputs needed:
    %% 1. for the OP_RETURN containing the Commitment
    %% 2. to return the total UTXO amount minus the Fee and Amount to our own account.
    %% 3. to register this transaction against the common HC BTC account
    %% The "data" field gets magically turned by the bitcoind API into an output
    %% in a correctly formatted OP_RETURN
    Refund = satoshi_to_btc(TotalAmount - Fee - Amount),
    HCAmount = satoshi_to_btc(Amount),
    HexCommitment = to_hex(Commitment),
    [#{BTCAcc => Refund}, #{HCCollectPubkey => HCAmount}, #{<<"data">> => HexCommitment}].

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
-spec signrawtransactionwithwallet(aehttpc:node_spec(), binary()) -> {ok, binary()} | {error, term()}.
signrawtransactionwithwallet(NodeSpec, RawTx) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"signrawtransactionwithwallet">>, [RawTx], seed_to_utf8(Seed))),
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

-spec result(map()) -> term().
result(Response) ->
    maps:get(<<"result">>, Response).

-spec block(map()) -> tuple().
block(Obj) ->
    Hash = maps:get(<<"hash">>, Obj), true = is_binary(Hash),
    Height = maps:get(<<"height">>, Obj), true = is_integer(Height),
    Time = maps:get(<<"time">>, Obj), true = is_integer(Time),

    %% TODO: To analyze the size field;
    %% Txs that might be Commitments will have at least one type:nulldata entry - just do some pre-filtering
    %% FilteredTxs = lists:filter(fun (Tx) -> is_nulldata(Tx) end, maps:get(<<"tx">>, Obj)),

    PrevHash = prev_hash(Obj),
    {Height, Hash, PrevHash, Time * 1000, maps:get(<<"tx">>, Obj)}.

% -spec find_commitments(list(), binary()) -> [{Pubkey :: binary(), Payload :: binary()}].
% find_commitments(Txs, _ParentHCAccountPubKey) ->
%     lists:foldl(fun(#{<<"vout">> := Vout}, Acc) ->
%                         case parse_vout(Vout) of
%                             {ok, ParsedCommitment} ->
%                                 [ParsedCommitment | Acc];
%                             {error, _Reason} ->
%                                 Acc
%                         end
%                 end, [], Txs).

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

-spec from_hex(hex()) -> binary().
from_hex(HexData) ->
    aeu_hex:hex_to_bin(HexData).

-spec to_hex(binary()) -> hex().
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

find_op_return([#{<<"scriptPubKey">> := #{<<"type">> := <<"nulldata">>, <<"asm">> := <<"OP_RETURN ", CommitmentEnc/binary>>}} | _]) ->
    {ok, CommitmentEnc};
find_op_return([_ | Vs]) ->
    find_op_return(Vs);
find_op_return([]) ->
    {error, no_op_return}.
