%%% @copyright (C) 2022, Aeternity
-module(aehttpc_btc).

%% Low level subset of bitcoin API required to interact with a hyperchain

%% Required exports:
-export([get_latest_block/5,
         get_header_by_hash/6,
         get_header_by_height/6,
         get_commitment_tx_in_block/8,
         get_commitment_tx_at_height/7,
         post_commitment/11]).


-type hex() :: binary().%[byte()].

-define(HC_COMMITMENT_VSN, 1).

get_latest_block(Host, Port, User, Password, Seed) ->
    {ok, Hash} = getbestblockhash(Host, Port, User, Password, Seed, false),
    {ok, {Height, Hash, PrevHash, _Txs}}
        = getblock(Host, Port, User, Password, Seed, false, Hash, _Verbosity = 1),
    {ok, Hash, PrevHash, Height}.

get_header_by_hash(Hash, Host, Port, User, Password, Seed) ->
    {ok, {Height, Hash, PrevHash, _Txs}}
      = getblock(Host, Port, User, Password, Seed, false, Hash, _Verbosity = 1),
    {ok, Hash, PrevHash, Height}.

get_header_by_height(Height, Host, Port, User, Password, Seed) ->
    case getblockhash(Host, Port, User, Password, Seed, false, Height) of
        {ok, Hash} ->
            {ok, {Height, Hash, PrevHash, _Txs}}
                = getblock(Host, Port, User, Password, Seed, false, Hash, _Verbosity = 1),
            {ok, Hash, PrevHash, Height};
        {error, not_found} -> {error, not_found}
    end.

get_commitment_tx_in_block(Host, Port, User, Password, Seed, BlockHash, _PrevHash, ParentHCAccountPubKey) ->
    {ok, {_Height, _Hash, __PrevHash, Txs}}
      = getblock(Host, Port, User, Password, Seed, false, BlockHash, _Verbosity = 2),
    Commitments = find_commitments(Txs, ParentHCAccountPubKey),
    {ok, Commitments}.

get_commitment_tx_at_height(Host, Port, User, Password, Seed, Height, ParentHCAccountPubKey) ->
    {ok, Hash} = getblockhash(Host, Port, User, Password, Seed, false, Height),
    {ok, {_Height, _Hash, _PrevHash, Txs}}
      = getblock(Host, Port, User, Password, Seed, false, Hash, _Verbosity = 2),
    Commitments = find_commitments(Txs, ParentHCAccountPubKey),
    {ok, Commitments}.

%% @doc Post commitment to BTC parent chain.
%% Commitment is a simple spend transaction to a known BTC account
%% Spend TX must be signed by the BTC account owned by the staking node
%% SpendTx must include the AE validator account pubkey in its MetaData
%% Bitcoin doc suggests 4 ops for this (https://gist.github.com/gavinandresen/2839617):
%% 1. Get a list of not-yet-spent outputs with listunspent
%% 2. Create a transaction using createrawtransaction
%% 3. Apply signatures using signrawtransaction
%% 4. Submit it using sendrawtransaction

post_commitment(Host, Port, User, Password, StakerPubkey, HCCollectPubkey, Amount, Fee, Commitment,
                _NetworkId, _SignModule) ->
        post_commitment(Host, Port, User, Password, StakerPubkey, HCCollectPubkey, Amount, Fee, Commitment).

post_commitment(Host, Port, User, Password, BTCAcc, HCCollectPubkey, Amount, _Fee, Commitment) ->
    {ok, Unspent} = listunspent(Host, Port, User, Password, false),
    UnspentSatoshis = unspent_to_satoshis(Unspent),
    Fee = 7000, %% FIXME, Set this to a suitable value at the calling site. Based on https://btc.network/estimate
    {ok, {Inputs, TotalAmount}} = select_utxo(UnspentSatoshis, Fee + Amount),
    Outputs = create_outputs(Commitment, BTCAcc, HCCollectPubkey, TotalAmount, Amount, Fee),
    {ok, Tx} = createrawtransaction(Host, Port, User, Password, false, Inputs, Outputs),
    {ok, SignedTx} = signrawtransactionwithwallet(Host, Port, User, Password, false, Tx),
    {ok, TxHash} = sendrawtransaction(Host, Port, User, Password, false, SignedTx),
    lager:debug("Posted BTC Commitment in Tx: ~p", [TxHash]),
    {ok, #{<<"tx_hash">> => TxHash}}.

select_utxo([#{<<"spendable">> := true, <<"amount">> := Amount} = Unspent | _Us], Needed) when Amount >= Needed ->
    %% For now just pick first spendable UTXO with enough funds. There is no end to how fancy this can get
    %% https://bitcoin.stackexchange.com/questions/32145/what-are-the-trade-offs-between-the-different-algorithms-for-deciding-which-utxo
    #{<<"txid">> := TxId, <<"vout">> := VOut} = Unspent,
    {ok, {[#{<<"txid">> => TxId, <<"vout">> => VOut}], Amount}};
select_utxo([U|Us], Fee) ->
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
-spec getbestblockhash(binary(), binary(), binary(), binary(), binary(), boolean()) -> {ok, binary()} | {error, term()}.
getbestblockhash(Host, Port, User, Password, Seed, SSL) ->
    try
        Body = jsx:encode(request_body(<<"getbestblockhash">>, [], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
        Hash = result(Res),
        {ok, Hash}
    catch E:R ->
        {error, {E, R}}
    end.

-spec getblockhash(binary(), binary(), binary(), binary(), binary(), boolean(), hex()) -> {ok, tuple()} | {error, term()}.
getblockhash(Host, Port, User, Password, Seed, SSL, Height) ->
    try
      Body = jsx:encode(request_body(<<"getblockhash">>, [Height], seed_to_utf8(Seed))),
      case request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000) of
        {ok, Res} ->
            Hash = result(Res),
            {ok, Hash};
        {error, not_found} ->
            {error, not_found}
      end
    catch E:R:S ->
      {error, {E, R, S}}
    end.

-spec getblock(binary(), binary(), string(), string(), binary(), boolean(), binary(), integer()) -> {ok, tuple()} | {error, term()}.
getblock(Host, Port, User, Password, Seed, SSL, Hash, Verbosity) ->
    try
        Body = jsx:encode(request_body(<<"getblock">>, [Hash, Verbosity], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
        Block = block(result(Res)),
        {ok, Block}
    catch E:R:S ->
        {error, {E, R, S}}
    end.

listunspent(Host, Port, User, Password, SSL) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"listunspent">>, [0, 9999999], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
        Unspent = result(Res),
        {ok, Unspent}
    catch E:R ->
        {error, {E, R}}
    end.

createrawtransaction(Host, Port, User, Password, SSL, Inputs, Outputs) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"createrawtransaction">>, [Inputs, Outputs], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
        Tx = result(Res),
        {ok, Tx}
    catch E:R ->
        {error, {E, R}}
    end.

%% Rely on the wallet embedded in the local bitcoind
%% FIXME: Allow this to use a different host/port so users can use a separate offline bitcoind
%% holding their commitments wallet.
-spec signrawtransactionwithwallet(binary(), integer(), binary(), binary(), boolean(), binary()) -> {ok, binary()} | {error, term()}.
signrawtransactionwithwallet(Host, Port, User, Password, SSL, RawTx) ->
    try
        Seed = <<>>,
        Body = jsx:encode(request_body(<<"signrawtransactionwithwallet">>, [RawTx], seed_to_utf8(Seed))),
        {ok, Res} = request(<<"/">>, Body, Host, Port, User, Password, SSL, 5000),
        SignedTx = result(Res),
        Complete = maps:get(<<"complete">>, SignedTx),
        true = Complete,
        Hex = maps:get(<<"hex">>, SignedTx),
        {ok, Hex}
    catch E:R ->
        {error, {E, R}}
    end.

-spec sendrawtransaction(binary(), binary(), binary(), binary(), boolean(), binary()) -> {ok, binary()} | {error, term()}.
sendrawtransaction(Host, Port, User, Password, SSL, Hex) ->
    try
        Seed = <<>>,
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
    Hash = maps:get(<<"hash">>, Obj), true = is_binary(Hash),
    Height = maps:get(<<"height">>, Obj), true = is_integer(Height),

    %% TODO: To analyze the size field;
    %% Txs that might be Commitments will have at least one type:nulldata entry - just do some pre-filtering
    %% FilteredTxs = lists:filter(fun (Tx) -> is_nulldata(Tx) end, maps:get(<<"tx">>, Obj)),

    PrevHash = prev_hash(Obj),
    {Height, Hash, PrevHash, maps:get(<<"tx">>, Obj)}.

-define(OP_RETURN, 16#6a).

-spec find_commitments(map(), binary()) -> {Pubkey :: binary(), Payload :: binary()}.
find_commitments(Txs, ParentHCAccountPubKey) ->
    lists:foldl(fun(Tx, Acc) ->
                    case Tx of
                        #{<<"vout">> :=
                             [#{<<"n">> := 0, <<"scriptPubKey">> := #{<<"address">> := _Staker}},
                              #{<<"n">> := 1, <<"scriptPubKey">> := #{<<"address">> := _ParentHCAccountPubKey}},
                              #{<<"n">> := 2, <<"scriptPubKey">> := #{<<"type">> := <<"nulldata">>, <<"hex">> := CommitmentEnc}}]} ->
                            %% This Tx matches a very specific pattern for a HC commitment
                            %% FIXME: Reject commitments sent to a different ParentHCAccountPubKey
                            %% FIXME: Consider being less rigid here WRT ordering or other.
                            Commitment = from_hex(CommitmentEnc),
                            case Commitment of
                                <<?OP_RETURN, 65, ?HC_COMMITMENT_VSN, StakerPubkey:32/binary, TopHash:32/binary>> ->
                                    [{StakerPubkey, TopHash} | Acc];
                                _ ->
                                    lager:debug("Invalid BTC Commitment, skipping ~p", [CommitmentEnc]),
                                    Acc
                            end;
                        _Else ->
                            lager:debug("Invalid BTC Tx, skipping ~p", [Tx]),
                            Acc
                    end
                end, [], Txs).

-spec request(binary(), binary(), binary(), integer(),binary(), binary(),boolean(),integer()) -> {ok, map()} | {error, term()}.
request(Path, Body, Host, Port, User, Password, SSL, Timeout) ->
    try
        Url = url(Host, Port, SSL),
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

% url(Host, Port, true = _SSL) when is_binary(Host), is_integer(Port) ->
%     path("https://", binary_to_list(Host), Port);
% url(Host, Port, true = _SSL) when is_list(Host), is_integer(Port) ->
%     path("https://", Host, Port);
url(Host, Port, _) when is_binary(Host), is_integer(Port) ->
    path("http://", binary_to_list(Host), Port);
url(Host, Port, _) when is_list(Host), is_integer(Port) ->
    path("http://", Host, Port).

path(Scheme, Host, Port) ->
    lists:concat([Scheme, Host, ":", Port]).

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

%% Commitment TXs contain at least one output with type:nulldata - that's our 
-spec is_nulldata(map()) -> boolean().
is_nulldata(Obj) ->
    Outputs = maps:get(<<"vout">>, Obj),
    Res = [Output || Output = #{ <<"scriptPubKey">> := #{ <<"type">> := T} } <- Outputs, T == <<"nulldata">>],
    Res /= [].

seed_to_utf8(Seed) when is_binary(Seed) ->
    base64:encode(Seed).
