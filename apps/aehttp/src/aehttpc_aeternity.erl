%%% @copyright (C) 2022, Aeternity
-module(aehttpc_aeternity).

%% Subset of Aeternity HTTP client API required to interact with a hyperchain

%% Required exports for hyperchain:
-export([get_latest_block/2,
         get_header_by_hash/3,
         get_header_by_height/3,
         get_commitment_tx_in_block/5,
         get_commitment_tx_at_height/4,
         post_commitment/8]).

-behavior(aehttpc).

%% @doc fetch latest top hash
get_latest_block(NodeSpec, _Seed) ->
    get_top_block_header(NodeSpec).

get_header_by_hash(Hash, NodeSpec, _Seed) ->
    get_key_block_header(Hash, NodeSpec).

get_header_by_height(Height, NodeSpec, _Seed) ->
    get_key_block_header_by_height(Height, NodeSpec).

get_commitment_tx_in_block(NodeSpec, _Seed, _BlockHash,
                           PrevHash, ParentHCAccountPubKey) ->
    case get_generation(NodeSpec, PrevHash) of
        {ok, #{<<"micro_blocks">> := MBs}} ->
            get_commitments(NodeSpec, MBs, ParentHCAccountPubKey);
        {error, not_found} = Err -> Err
    end.

get_commitment_tx_at_height(NodeSpec, _Seed, Height, ParentHCAccountPubKey) ->
    case get_generation_by_height(NodeSpec, Height) of
        {ok, #{<<"micro_blocks">> := MBs}} ->
            get_commitments(NodeSpec, MBs, ParentHCAccountPubKey);
        {error, not_found} = Err -> Err
    end.

%% @doc Post commitment to AE parent chain.
post_commitment(NodeSpec, StakerPubkey, HCCollectPubkey, Amount, Fee, Commitment,
                NetworkId, SignModule) ->
    post_commitment_tx(NodeSpec, StakerPubkey, HCCollectPubkey, Amount,
                       Fee, Commitment,
                       NetworkId, SignModule).


%%%===================================================================
%%%  AE HTTP protocol
%%%===================================================================
-spec get_top_block_header(aehttpc:node_spec()) -> {ok, binary()} | {error, term()}.
get_top_block_header(NodeSpec) ->
    try
        {ok, #{<<"hash">> := Hash,
               <<"prev_key_hash">> := PrevHash,
               <<"height">> := Height}} =
            get_request(<<"/v3/key-blocks/current">>, NodeSpec, 5000),
        {ok, Hash, PrevHash, Height}
    catch error:{badmatch, {error, not_found}} -> {error, not_found};
          E:R -> {error, {E, R}}
    end.

get_key_block_header(Hash, NodeSpec) ->
    try
        case get_request(<<"/v3/key-blocks/hash/", Hash/binary>>, NodeSpec, 5000) of
            {ok, #{<<"hash">> := Hash,
                <<"prev_key_hash">> := PrevHash,
                <<"height">> := Height}} ->
                {ok, Hash, PrevHash, Height};
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
                    <<"height">> := Height}} ->
                {ok, Hash, PrevHash, Height};
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

-spec get_commitments(aehttpc:node_spec(), [binary()], binary()) -> {ok, list()} | {error, term()}.
get_commitments(NodeSpec, MBs, ParentHCAccountPubKey) ->
    Txs = lists:flatmap(
        fun(MB) ->
            get_hc_commitments(NodeSpec, MB, ParentHCAccountPubKey)
        end, MBs),
    {ok, Txs}.

get_hc_commitments(NodeSpec, MB, ParentHCAccountPubKey) ->
    Path =  <<"/v3/micro-blocks/hash/", MB/binary, "/transactions">>,
    {ok, Res} = get_request(Path, NodeSpec, 5000),
    #{<<"transactions">> := Txs} = Res,
    %% TODO - take hc commitment account from some config
    %% Commitments include:
    %%   [{Committer, Committers view of child chain top hash}]
    ExpectedRecipient = aeser_api_encoder:encode(account_pubkey, ParentHCAccountPubKey),
    lists:foldl(
            fun(#{<<"tx">> := Tx}, Acc) ->
                    case Tx of
                        #{<<"type">> := <<"SpendTx">>,
                          <<"recipient_id">> := ExpectedRecipient,
                          <<"sender_id">> := _SenderId,
                          <<"payload">> := CommitmentEnc} ->
                                {ok, Commitment} = aeser_api_encoder:safe_decode(bytearray, CommitmentEnc),
                                {btc, Signature, StakerHash, TopKeyHash} = aec_parent_chain_block:decode_commitment(Commitment),
                                [{Signature, StakerHash, TopKeyHash} | Acc];
                        _ ->
                            Acc
                    end
            end, [], Txs).

post_commitment_tx(NodeSpec, SenderEnc, ReceiverPubkey, Amount, Fee,
                   Commitment,
                   NetworkId, SignModule) ->
    %% 1. get the next nonce for our account over at the parent chain
    {ok, SenderPubkey} = aeser_api_encoder:safe_decode(account_pubkey,
                                                        SenderEnc),
    %% FIXME consider altenrative approaches to fetching a nonce: ex. if there
    %% is a hanging transaction in the pool this would produce another hanging
    %% one
    NoncePath = <<"/v3/accounts/", SenderEnc/binary, "/next-nonce">>,
    {ok, #{<<"next_nonce">> := Nonce}} = get_request(NoncePath, NodeSpec, 5000),
    %% 2. Create a SpendTx containing the commitment in its payload
    TxArgs =
        #{sender_id    => aeser_id:create(account, SenderPubkey),
          recipient_id => aeser_id:create(account, ReceiverPubkey),
          amount       => Amount,
          %% TODO: automatic fee computation
          %% This might not be trivial as the fee depends on a bunch of
          %% parameters, namely:
          %% * parent chain's protocol version
          %% * expected min_gas on both miner and protocol level
          %% * current gas prices
          fee          => Fee,
          nonce        => Nonce,
          payload      => Commitment},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    SignedSpendTx = sign_tx(SpendTx, NetworkId, SenderPubkey, SignModule),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedSpendTx)),
    Body = #{<<"tx">> => Transaction},
    Path = <<"/v3/transactions">>,
    post_request(Path, Body, NodeSpec, 5000).

%% TODO This function copied from aec_test_utils as that module is not available
%% in normal builds
%% TODO - wallet interaction of some kind to get privKey
%% This is a horrible hack for now
%% Maybe aec_preset_keys??
%% {ok, Sig} = aec_preset_keys:sign_tx(SpendTx, AccountId)
%% TODO: redo this whole thing
-define(VALID_PRIVK(K), byte_size(K) =:= 64).

sign_tx(Tx, NetworkId, Signer, SignModule) when is_binary(Signer) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_custom_network_id(NetworkId, Bin0),
    {ok, Signature} = SignModule:sign_binary(BinForNetwork, Signer),
    aetx_sign:new(Tx, [Signature]).

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
