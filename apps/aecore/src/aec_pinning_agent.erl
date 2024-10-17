%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc
%%% Gather child->parent chain pinning information for AE HC and make it
%%% available through API
%%% @end
%%%-------------------------------------------------------------------

-module(aec_pinning_agent).
-define(PIN_TAG, 101).

-export(
    [
    get_pinning_data/0,
    encode_pin_payload/1,
    decode_pin_payload/1,
    create_pin_tx/6,
    post_pin_tx/2,
    get_pin_by_tx_hash/2,
    encode_child_pin_payload/1,
    decode_child_pin_payload/1,
    is_pin/1
    ]).

% PINREFAC aec_chains_hc?
-spec get_pinning_data() -> {ok, map()} | {error, atom()}.
get_pinning_data() ->
    {ok, #{epoch := Epoch,
           first := First,
           last  := Last}} = aec_chain_hc:epoch_info(),
    lager:debug("Get pin data for epoch ~p for leader of block ~p", [Epoch - 1, Last]),
    {ok, BlockHash} = aec_chain_state:get_key_block_hash_at_height(First-1),
    {ok, ChainType} = aec_parent_connector:get_parent_chain_type(),
    PrevEpoch = Epoch - 1,
    Height = First - 1,
    case aec_consensus_hc:leader_for_height(Last) of
      {ok, Leader} ->
        {ok, #{epoch             => PrevEpoch,
               height            => Height,
               block_hash        => BlockHash,
               parent_payload    => encode_pin_payload(#{epoch => PrevEpoch, height => Height, block_hash => BlockHash}),
               last_leader       => Leader,
               parent_type       => ChainType,
               parent_network_id => aec_parent_connector:get_network_id()}};
      error ->
          %% schedule not yet cached
          {error, last_leader_unknown}
    end.


-spec encode_pin_payload(#{epoch => integer(), height => integer(), block_hash => binary()}) -> binary().
encode_pin_payload(#{epoch := Epoch, height := Height, block_hash := BlockHash}) ->
    EpochHex = list_to_binary(erlang:integer_to_list(Epoch, 16)),
    HeightHex = list_to_binary(erlang:integer_to_list(Height, 16)),
    EncBlockHash = aeser_api_encoder:encode(key_block_hash, BlockHash),
    <<EpochHex/binary, ":", HeightHex/binary, " ", EncBlockHash/binary>>.

-spec decode_pin_payload(binary()) -> #{epoch => integer(), height => integer(), block_hash => binary()}.
decode_pin_payload(Binary) ->
    [HexEpoch, HexHeight, EncBlockHash] = binary:split(Binary, [<<":">>, <<" ">>], [global]),
    Epoch = erlang:list_to_integer(binary_to_list(HexEpoch), 16),
    Height = erlang:list_to_integer(binary_to_list(HexHeight), 16),
    {ok, BlockHash} = aeser_api_encoder:safe_decode(key_block_hash, EncBlockHash),
    #{epoch => Epoch, height => Height, block_hash => BlockHash}.

encode_child_pin_payload(TxHash) ->
    <<$p,$i,$n, TxHash/binary>>.

decode_child_pin_payload(<<$p,$i,$n, TxHash/binary>>) ->
    TxHash;
decode_child_pin_payload(_) ->
    error.

is_pin(Pin) -> 
    case decode_child_pin_payload(Pin) of
        error -> false;
        _ -> true
    end.

% PINREFAC aec_parent_connector?
-spec create_pin_tx(binary(), binary(), binary(), integer(), integer(), binary()) -> aetx:tx().
create_pin_tx(NodeSpec, SenderEnc, ReceiverPubkey, Amount, Fee, PinPayload) ->
    %% 1. get the next nonce for our account over at the parent chain
    {ok, SenderPubkey} = aeser_api_encoder:safe_decode(account_pubkey, SenderEnc),
    NoncePath = <<"/v3/accounts/", SenderEnc/binary, "/next-nonce">>,
    {ok, #{<<"next_nonce">> := Nonce}} = get_request(NoncePath, NodeSpec, 5000),
    %% 2. Create a SpendTx containing the Pin in its payload
    TxArgs = #{sender_id    => aeser_id:create(account, SenderPubkey),
               recipient_id => aeser_id:create(account, ReceiverPubkey),
               amount       => Amount,
               fee          => Fee,
               nonce        => Nonce,
               payload      => PinPayload},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    SpendTx.

% PINREFAC aec_parent_connector?
post_pin_tx(SignedSpendTx, NodeSpec) ->
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedSpendTx)),
    Body = #{<<"tx">> => Transaction},
    Path = <<"/v3/transactions">>,
    post_request(Path, Body, NodeSpec, 5000).

% PINREFAC aec_parent_connector?
get_pin_by_tx_hash(TxHash, NodeSpec) ->
    %SerHash = aeser_api_encoder:encode(tx_hash, TxHash),
    TxPath = <<"/v3/transactions/", TxHash/binary>>,
    {ok, #{<<"tx">> := #{<<"payload">> := EncPin}}} = get_request(TxPath, NodeSpec, 5000),
    {ok, Pin} = aeser_api_encoder:safe_decode(bytearray, EncPin),
    decode_pin_payload(Pin).
    %get_request(TxPath, NodeSpec, 5000).



%% TODO This function copied from aec_test_utils as that module is not available
%% in normal builds
%% TODO - wallet interaction of some kind to get privKey
%% This is a horrible hack for now
%% Maybe aec_preset_keys??
%% {ok, Sig} = aec_preset_keys:sign_tx(SpendTx, AccountId)
%% TODO: redo this whole thing
-define(VALID_PRIVK(K), byte_size(K) =:= 64).

% PINREFAC aec_parent_connector
get_request(Path, NodeSpec, Timeout) ->
    try
        Url = aehttpc:url(NodeSpec),
        UrlPath = lists:concat([Url, binary_to_list(Path)]),
        Req = {UrlPath, []},
        HTTPOpt = [{timeout, Timeout}],
        Opt = [],
        lager:debug("Req: ~p, with URL: ~ts", [Req, Url]),
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

% PINREFAC aec_parent_connector
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
