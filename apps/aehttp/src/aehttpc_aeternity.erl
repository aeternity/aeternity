%%% @copyright (C) 2022, Aeternity
-module(aehttpc_aeternity).

%% Subset of Aeternity HTTP client API required to interact with a hyperchain

%% Required exports for hyperchain:
-export([get_latest_block/5,
         get_header_by_hash/6,
         get_header_by_height/6,
         get_commitment_tx_in_block/7,
         get_commitment_tx_at_height/7,
         post_commitment/9]).

%% @doc fetch latest top hash
get_latest_block(Host, Port, _User, _Password, _Seed) ->
    get_top_block_header(Host, Port).

get_header_by_hash(Hash, Host, Port, _User, _Password, _Seed) ->
    get_key_block_header(Hash, Host, Port).

get_header_by_height(Height, Host, Port, _User, _Password, _Seed) ->
    get_key_block_header_by_height(Height, Host, Port).

get_commitment_tx_in_block(Host, Port, _User, _Password, _Seed, BlockHash, ParentHCAccountPubKey) ->
    {ok, #{<<"micro_blocks">> := MBs}} = get_generation(Host, Port, BlockHash),
    get_transactions(Host, Port, MBs, ParentHCAccountPubKey).

get_commitment_tx_at_height(Host, Port, _User, _Password, _Seed, Height, ParentHCAccountPubKey) ->
    {ok, #{<<"micro_blocks">> := MBs}} = get_generation_by_height(Host, Port, Height),
    get_transactions(Host, Port, MBs, ParentHCAccountPubKey).

%% @doc Post commitment to AE parent chain.
%% FIXME: When should this be called, how often, and by which accounts?
%% FIXME: Sort out what we actually need to post to the parent chain
post_commitment(Host, Port, StakerPubkey, HCCollectPubkey, Amount, Fee, CurrentTop,
                NetworkId, SignModule) ->
    post_commitment_tx(Host, Port, StakerPubkey, HCCollectPubkey, Amount,
                       Fee, CurrentTop,
                       NetworkId, SignModule).


%%%===================================================================
%%%  AE HTTP protocol
%%%===================================================================
-spec get_top_block_header(binary(), integer()) -> {ok, binary()} | {error, term()}.
get_top_block_header(Host, Port) ->
    try
        {ok, #{<<"hash">> := Hash,
               <<"prev_key_hash">> := PrevHash,
               <<"height">> := Height}} =
            get_request(<<"/v3/key-blocks/current">>, Host, Port, 5000),
        {ok, Hash, PrevHash, Height}
    catch E:R ->
        {error, {E, R}}
    end.

get_key_block_header(Hash, Host, Port) ->
    try
        {ok, #{<<"hash">> := Hash,
               <<"prev_key_hash">> := PrevHash,
               <<"height">> := Height}} =
            get_request(<<"/v3/key-blocks/hash/", Hash/binary>>, Host, Port, 5000),
        {ok, Hash, PrevHash, Height}
    catch E:R ->
        {error, {E, R}}
    end.

get_key_block_header_by_height(Height, Host, Port) ->
    HeightB = integer_to_binary(Height),
    try
        case  get_request(<<"/v3/key-blocks/height/", HeightB/binary>>, Host, Port, 5000) of
            {ok, #{ <<"hash">> := Hash,
                    <<"prev_key_hash">> := PrevHash,
                    <<"height">> := Height}} ->
                {ok, Hash, PrevHash, Height};
            {error, not_found} -> {error, not_found}
        end
    catch E:R ->
        {error, {E, R}}
    end.


-spec get_generation(binary(), integer(), binary()) -> {ok, map()} | {error, term()}.
get_generation(Host, Port, Hash) ->
    Path = <<"/v3/generations/hash/", Hash/binary>>,
    get_request(Path, Host, Port, 5000).

-spec get_generation_by_height(binary(), integer(), integer()) -> {ok, map()} | {error, term()}.
get_generation_by_height(Host, Port, Height) ->
    HeightBin = list_to_binary(integer_to_list(Height)),
    Path = <<"/v3/generations/height/", HeightBin/binary>>,
    get_request(Path, Host, Port, 5000).

-spec get_transactions(binary(), integer(), [binary()], binary()) -> {ok, list()} | {error, term()}.
get_transactions(Host, Port, MBs, ParentHCAccountPubKey) ->
    Txs = lists:flatmap(
        fun(MB) ->
            get_hc_commitments(Host, Port, MB, ParentHCAccountPubKey)
        end, MBs),
    {ok, Txs}.

get_hc_commitments(Host, Port, MB, ParentHCAccountPubKey) ->
    Path =  <<"/v3/micro-blocks/hash/", MB/binary, "/transactions">>,
    {ok, Res} = get_request(Path, Host, Port, 5000),
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
                          <<"sender_id">> := SenderId,
                          <<"payload">> := Commitment} ->
                            [{SenderId, Commitment} | Acc];
                        _ ->
                            Acc
                    end
            end, [], Txs).

post_commitment_tx(Host, Port, SenderPubkey, ReceiverPubkey, Amount, Fee,
                   CurrentTopHash,
                   NetworkId, SignModule) ->
    %% 1. get the next nonce for our account over at the parent chain
    SenderEnc = aeser_api_encoder:encode(account_pubkey, SenderPubkey),
    %% FIXME consider altenrative approaches to fetching a nonce: ex. if there
    %% is a hanging transaction in the pool this would produce another hanging
    %% one
    NoncePath = <<"/v3/accounts/", SenderEnc/binary, "/next-nonce">>,
    {ok, #{<<"next_nonce">> := Nonce}} = get_request(NoncePath, Host, Port, 5000),
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
          payload      => CurrentTopHash},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    SignedSpendTx = sign_tx(SpendTx, NetworkId, SenderPubkey, SignModule),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedSpendTx)),
    Body = #{<<"tx">> => Transaction},
    Path = <<"/v3/transactions">>,
    post_request(Path, Body, Host, Port, 5000),
    ok.

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

get_request(Path, Host, Port, Timeout) ->
    try
    Url = url(binary_to_list(Host), Port, false),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, []},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    lager:debug("Req: ~p, with URL: ~ts", [Req, Url]),
    case httpc:request(get, Req, HTTPOpt, Opt) of
        {ok, {{_, 200 = _Code, _}, _, Res}} ->
            lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
            {ok, jsx:decode(list_to_binary(Res), [return_maps])};
        {ok, {{_, 404 = _Code, _}, _, "{\"reason\":\"Block not found\"}"}} ->
            {error, not_found}
    end
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}}
  end.
    
-spec post_request(binary(), map(), binary(), integer(),integer()) -> {ok, map()} | {error, term()}.
post_request(Path, Body, Host, Port, Timeout) ->
  try
    Url = url(binary_to_list(Host), Port, false),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, [], "application/json", jsx:encode(Body)},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    lager:debug("Req: ~p, with URL: ~ts", [Req, Url]),
    case httpc:request(post, Req, HTTPOpt, Opt) of
        {ok, {{_, 200 = _Code, _}, _, Res}} ->
            lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
            {ok, jsx:decode(list_to_binary(Res), [return_maps])};
        {ok, {{_, 400, _}, _, Res}} ->
            {error, 400, jsx:decode(list_to_binary(Res), [return_maps])}
    end
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}}
  end.

url(Host, Port, false) when is_list(Host), is_integer(Port) ->
  path("http://", Host, Port).

path(Scheme, Host, Port) ->
  lists:concat([Scheme, Host, ":", Port]).

