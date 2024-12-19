%%% @copyright (C) 2022, Aeternity
-module(aehttpc_aeternity).

-include_lib("aecontract/include/aecontract.hrl").

%% Subset of Aeternity HTTP client API required to interact with a hyperchain

%% Required exports for hyperchain:
-export([get_latest_block/2,
         get_header_by_hash/3,
         get_header_by_height/3,
         hash_to_integer/1,
         pin_to_pc/2,
         get_pin_by_tx_hash/2
        ]).

%% Util exports
-export([get_generation/2,
get_generation_by_height/2,
post_request/4,
get_chain_type/0]).

-behavior(aehttpc).

%% @doc fetch latest top hash
get_latest_block(NodeSpec, _Seed) ->
    get_top_block_header(NodeSpec).

get_header_by_hash(Hash, NodeSpec, _Seed) ->
    get_key_block_header(Hash, NodeSpec).

get_header_by_height(Height, NodeSpec, _Seed) ->
    get_key_block_header_by_height(Height, NodeSpec).

get_chain_type() ->
    {ok, aeternity}.

hash_to_integer(Hash) ->
    {_, HashBin} = aeser_api_encoder:decode(Hash),
    <<Int:256/unsigned-integer>> = HashBin,
    Int.


%%%===================================================================
%%%  AE HTTP protocol
%%%===================================================================
-spec get_top_block_header(aehttpc:node_spec()) -> {ok, binary()} | {error, term()}.
get_top_block_header(NodeSpec) ->
    try
        {ok, #{<<"hash">> := Hash,
               <<"prev_key_hash">> := PrevHash,
               <<"height">> := Height,
               <<"time">> := Time}} =
            get_request(<<"/v3/key-blocks/current">>, NodeSpec, 5000),
        {ok, Hash, PrevHash, Height, Time}
    catch error:{badmatch, {error, not_found}} -> {error, not_found};
          E:R -> {error, {E, R}}
    end.

get_key_block_header(Hash, NodeSpec) ->
    try
        case get_request(<<"/v3/key-blocks/hash/", Hash/binary>>, NodeSpec, 5000) of
            {ok, #{<<"hash">> := Hash,
                <<"prev_key_hash">> := PrevHash,
                <<"height">> := Height,
                <<"time">> := Time}} ->
                {ok, Hash, PrevHash, Height, Time};
            {error, not_found} -> {error, not_found}
        end
    catch E:R ->
        {error, {E, R}}
    end.

get_key_block_header_by_height(Height, NodeSpec) ->
    HeightB = integer_to_binary(Height),
    try
        case  get_request(<<"/v3/key-blocks/height/", HeightB/binary>>, NodeSpec, 5000) of
            {ok, #{ <<"hash">> := Hash,
                    <<"prev_key_hash">> := PrevHash,
                    <<"height">> := Height,
                    <<"time">> := Time}} ->
                {ok, Hash, PrevHash, Height, Time};
            {error, not_found} -> {error, not_found}
        end
    catch E:R ->
        {error, {E, R}}
    end.


-spec get_generation(aehttpc:node_spec(), binary()) -> {ok, map()} | {error, term()}.
get_generation(NodeSpec, Hash) ->
    Path = <<"/v3/generations/hash/", Hash/binary>>,
    get_request(Path, NodeSpec, 5000).

-spec get_generation_by_height(aehttpc:node_spec(), integer()) -> {ok, map()} | {error, term()}.
get_generation_by_height(NodeSpec, Height) ->
    HeightBin = list_to_binary(integer_to_list(Height - 1 )), %% previous generation!!!
    Path = <<"/v3/generations/height/", HeightBin/binary>>,
    get_request(Path, NodeSpec, 5000).


%%%=============================================================================
%%% Pinning
%%%=============================================================================

pin_to_pc({PinningData, Who, Amount, Fee, NetworkId, SignModule}, NodeSpec) ->
    PinPayload = aeser_hc:encode_parent_pin_payload(PinningData),
    Nonce = get_pc_nonce(Who, NodeSpec),
    SpendTx = create_pin_tx_(Who, Who, Nonce, Amount, Fee, PinPayload),
    SignedSpendTx = sign_tx(SpendTx, NetworkId, Who, SignModule),
    post_pin_tx(SignedSpendTx, NodeSpec).


create_pin_tx_(SenderPubkey, ReceiverPubkey, Nonce, Amount, Fee, PinPayload) ->
    TxArgs = #{ sender_id    => aeser_id:create(account, SenderPubkey),
                recipient_id => aeser_id:create(account, ReceiverPubkey),
                amount       => Amount,
                fee          => Fee,
                nonce        => Nonce,
                payload      => PinPayload},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    SpendTx.

get_pc_nonce(Who, NodeSpec) ->
    WhoEnc = aeser_api_encoder:encode(account_pubkey, Who),
    NoncePath = <<"/v3/accounts/", WhoEnc/binary, "/next-nonce">>,
    {ok, #{<<"next_nonce">> := Nonce}} = get_request(NoncePath, NodeSpec, 5000),
    Nonce.

sign_tx(Tx, NetworkId, Signer, SignModule) when is_binary(Signer) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_custom_network_id(NetworkId, Bin0),
    {ok, Signature} = SignModule:sign_binary(BinForNetwork, Signer),
    aetx_sign:new(Tx, [Signature]).

post_pin_tx(SignedSpendTx, NodeSpec) ->
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedSpendTx)),
    Body = #{<<"tx">> => Transaction},
    Path = <<"/v3/transactions">>,
    {ok, #{<<"tx_hash">> := TxHash}} = post_request(Path, Body, NodeSpec, 5000),
    TxHash.

get_pin_by_tx_hash(TxHash, NodeSpec) ->
    TxPath = <<"/v3/transactions/", TxHash/binary>>,
    case get_request(TxPath, NodeSpec, 5000) of
        {ok, #{<<"tx">> := #{<<"payload">> := EncPin}, <<"block_height">> := Height}} ->
            {ok, Pin} = aeser_api_encoder:safe_decode(bytearray, EncPin),
            {ok, DecPin} = aeser_hc:decode_parent_pin_payload(Pin),
            {ok, maps:put(pc_height, Height, DecPin)}; % add the pc block height to pin map, -1 = not on chain yet.
        OtherErr -> {error, OtherErr}
    end.

get_request(Path, NodeSpec, Timeout) ->
    try
    Url = aehttpc:url(NodeSpec),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, []},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    case httpc:request(get, Req, HTTPOpt, Opt) of
        {ok, {{_, 200 = _Code, _}, _, Res}} ->
            {ok, jsx:decode(list_to_binary(Res), [return_maps])};
        {ok, {{_, 404 = _Code, _}, _, "{\"reason\":\"Block not found\"}"}} ->
            {error, not_found};
        {ok, {{_, Code, _}, _, Res}} when Code >= 400 ->
            lager:debug("Req: ~p, Res: ~p with URL: ~ts, Code ~p", [Req, Res, Url, Code]),
            {error, not_found};
        {error, Err} ->
            lager:debug("Req: ~p, ERROR: ~p with URL: ~ts", [Req, Err, Url]),
            {error, not_found}
    end
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}}
  end.

-spec post_request(binary(), map(), aehttpc:node_spec(), integer()) -> {ok, map()} | {error, term()}.
post_request(Path, Body, NodeSpec, Timeout) ->
  try
    Url = aehttpc:url(NodeSpec),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, [], "application/json", jsx:encode(Body)},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    case httpc:request(post, Req, HTTPOpt, Opt) of
        {ok, {{_, 200 = _Code, _}, _, Res}} ->
            lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
            {ok, jsx:decode(list_to_binary(Res), [return_maps])};
        {ok, {{_ ,400, _}, _, Res}} ->
            lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
            {error, 400, jsx:decode(list_to_binary(Res), [return_maps])}
    end
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}}
  end.
