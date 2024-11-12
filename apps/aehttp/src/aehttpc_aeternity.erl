%%% @copyright (C) 2022, Aeternity
-module(aehttpc_aeternity).

%% Subset of Aeternity HTTP client API required to interact with a hyperchain

%% Required exports for hyperchain:
-export([get_latest_block/2,
         get_header_by_hash/3,
         get_header_by_height/3,
         hash_to_integer/1,
         pin_to_pc/2,
         pin_tx_to_cc/5,
         pin_contract_call/6,
         create_pin_tx/2,
         post_pin_tx/2,
         get_pin_by_tx_hash/2,
         encode_parent_pin_payload/1,
         decode_parent_pin_payload/1,
         encode_child_pin_payload/1,
         decode_child_pin_payload/1,
         is_pin/1
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

-spec encode_parent_pin_payload(#{epoch => integer(), height => integer(), block_hash => binary()}) -> binary().
encode_parent_pin_payload(#{epoch := Epoch, height := Height, block_hash := BlockHash}) ->
    EpochHex = list_to_binary(erlang:integer_to_list(Epoch, 16)),
    HeightHex = list_to_binary(erlang:integer_to_list(Height, 16)),
    EncBlockHash = aeser_api_encoder:encode(key_block_hash, BlockHash),
    <<EpochHex/binary, ":", HeightHex/binary, " ", EncBlockHash/binary>>.

%-spec decode_parent_pin_payload(binary()) -> #{epoch => integer(), height => integer(), block_hash => binary()}.
decode_parent_pin_payload(Binary) ->
    try
        [HexEpoch, HexHeight, EncBlockHash] = binary:split(Binary, [<<":">>, <<" ">>], [global]),
        Epoch = erlang:list_to_integer(binary_to_list(HexEpoch), 16),
        Height = erlang:list_to_integer(binary_to_list(HexHeight), 16),
        {ok, BlockHash} = aeser_api_encoder:safe_decode(key_block_hash, EncBlockHash),
        {ok, #{epoch => Epoch, height => Height, block_hash => BlockHash}}
    catch
        _ -> {error, {bad_parent_pin_payload, Binary}}
    end.

encode_child_pin_payload(TxHash) ->
    <<"pin", TxHash/binary>>.

decode_child_pin_payload(<<"pin", TxHash/binary>>) ->
    {ok, TxHash};
decode_child_pin_payload(BadTxHash) ->
    {error, {bad_child_pin_payload, BadTxHash}}.

is_pin(Pin) ->
    case decode_child_pin_payload(Pin) of
        {error,_} -> false;
        _ -> true
    end.

% -spec create_pin_tx(binary(), binary(), binary(), integer(), integer(), binary()) -> aetx:tx().
create_pin_tx({SenderPubkey, ReceiverPubkey, Amount, Fee, PinningData}, NodeSpec) ->
    Nonce = get_pc_nonce(SenderPubkey, NodeSpec),
    PinPayload = encode_parent_pin_payload(PinningData),
    create_pin_tx_({SenderPubkey, ReceiverPubkey, Nonce, Amount, Fee, PinPayload}).

create_pin_tx_({SenderPubkey, ReceiverPubkey, Nonce, Amount, Fee, PinPayload}) ->
    TxArgs = #{sender_id    => aeser_id:create(account, SenderPubkey),
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

get_local_nonce(Who) ->
    case aec_next_nonce:pick_for_account(Who, max) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

pin_to_pc({PinningData, Who, Amount, Fee, NetworkId, SignModule} = Args, NodeSpec) ->
    PinPayload = encode_parent_pin_payload(PinningData),
    Nonce = get_pc_nonce(Who, NodeSpec),
    SpendTx = create_pin_tx_({Who, Who, Nonce, Amount, Fee, PinPayload}),
    SignedSpendTx = sign_tx(SpendTx, NetworkId, Who, SignModule),
    post_pin_tx(SignedSpendTx, NodeSpec).

pin_tx_to_cc(PinTxHash, Who, Amount, Fee, SignModule) ->
    Nonce = get_local_nonce(Who),
    SpendTx = create_pin_tx_({Who, Who, Nonce, Amount, Fee, PinTxHash}),
    NetworkId = aec_governance:get_network_id(),
    SignedSpendTx = sign_tx(SpendTx, NetworkId, Who, SignModule),
    aec_tx_pool:push(SignedSpendTx, tx_received).

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
    %lager:debug("PINNING: wrote to PC tx hash: ~p", [TxHash]),
    encode_child_pin_payload(TxHash).

pin_contract_call(ContractPubkey, PinTx, Who, Amount, Fee, SignModule) ->
    Nonce = get_local_nonce(Who),
    {ok, CallData} = aeb_fate_abi:create_calldata("pin", [bytes_literal(PinTx)]),
    ABI = 3, % not really nice, what is the supported version of getting the latest ABI version
    TxSpec =
        #{  caller_id   => aeser_id:create(account, Who)
          , nonce       => Nonce
          , contract_id => aeser_id:create(contract, ContractPubkey)
          , abi_version => ABI
          , fee         => Fee %1000000 * min_gas_price()
          , amount      => Amount
          , gas         => 1000000
          , gas_price   => min_gas_price()
          , call_data   => CallData},
    {ok, Tx} = aect_call_tx:new(TxSpec),
    NetworkId = aec_governance:get_network_id(),
    SignedCallTx = sign_tx(Tx, NetworkId, Who, SignModule),
    aec_tx_pool:push(SignedCallTx, tx_received).


bytes_literal(Bin) ->
    [_, _ | PinLit] = binary_to_list(aeu_hex:hexstring_encode(Bin)),
    "#" ++ PinLit.

min_gas_price() ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    max(aec_governance:minimum_gas_price(Protocol),
        aec_tx_pool:minimum_miner_gas_price()).

get_pin_by_tx_hash(TxHashEnc, NodeSpec) ->
    case decode_child_pin_payload(TxHashEnc) of
         {error, _} -> {error, {bad_child_pin_tx_hash, TxHashEnc}};
         {ok, TxHash} ->
            TxPath = <<"/v3/transactions/", TxHash/binary>>,
            case get_request(TxPath, NodeSpec, 5000) of
                {ok, #{<<"tx">> := #{<<"payload">> := EncPin}, <<"block_height">> := Height}} = Tx ->
                    lager:debug("TXXXX: ~p", [Tx]),
                    {ok, Pin} = aeser_api_encoder:safe_decode(bytearray, EncPin),
                    {ok, DecPin} = decode_parent_pin_payload(Pin),
                    {ok, maps:put(pc_height, Height, DecPin)}; % add the pc block height to pin map, -1 = not on chain yet.
                OtherErr -> {error, OtherErr}
            end
    end.


get_request(Path, NodeSpec, Timeout) ->
    try
    Url = aehttpc:url(NodeSpec),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, []},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    %% lager:debug("Req: ~p, with URL: ~ts", [Req, Url]),
    case httpc:request(get, Req, HTTPOpt, Opt) of
        {ok, {{_, 200 = _Code, _}, _, Res}} ->
            %% lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
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
