%%% @copyright (C) 2022, Aeternity
-module(aehttpc_aeternity).

%% Subset of Aeternity HTTP client API required to interact with a hyperchain

%% Required exports for hyperchain:
-export([get_latest_block/5,
        get_commitment_tx_in_block/7,
        get_commitment_tx_at_height/7,
        post_commitment/6]).

%% @doc fetch latest top hash
get_latest_block(Host, Port, _User, _Password, _Seed) ->
    get_top_block_hash(Host, Port).

get_commitment_tx_in_block(Host, Port, _User, _Password, _Seed, BlockHash, ParentHCAccountPubKey) ->
    {ok, #{<<"micro_blocks">> := MBs}} = get_generation(Host, Port, BlockHash),
    get_transactions(Host, Port, MBs, ParentHCAccountPubKey).

get_commitment_tx_at_height(Host, Port, _User, _Password, _Seed, Height, ParentHCAccountPubKey) ->
    {ok, #{<<"micro_blocks">> := MBs}} = get_generation_by_height(Host, Port, Height),
    get_transactions(Host, Port, MBs, ParentHCAccountPubKey).

%% @doc Post commitment to AE parent chain.
%% FIXME: When should this be called, how often, and by which accounts?
%% FIXME: Sort out what we actually need to post to the parent chain
post_commitment(Host, Port, AccountId, Signature, HCAccountId, CurrentTop) ->
    post_commitment_tx(Host, Port, AccountId, Signature, HCAccountId, CurrentTop).


%%%===================================================================
%%%  AE HTTP protocol
%%%===================================================================
-spec get_top_block_hash(binary(), integer()) -> {ok, binary()} | {error, term()}.
get_top_block_hash(Host, Port) ->
    try
        {ok, #{<<"hash">> := Hash}} =
            get_request(<<"/v3/key-blocks/current/hash">>, Host, Port, 5000),
        {ok, Hash}
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

post_commitment_tx(Host, Port, AccountId, StakerPrivKey, HCAccountId, CurrentTopHash) ->
    %% 1. get the next nonce for our account over at the parent chain
    SenderEnc = aeser_api_encoder:encode(account_pubkey, AccountId),
    NoncePath = <<"/v3/accounts/", SenderEnc/binary, "/next-nonce">>,
    {ok, #{<<"next_nonce">> := Nonce}} = get_request(NoncePath, Host, Port, 5000),
    %% 2. Create a SpendTx containing the commitment in its payload
    TxArgs =
        #{sender_id    => aeser_id:create(account, AccountId),
          recipient_id => aeser_id:create(account, HCAccountId),
          amount       => 10000,       %% FIXME config
          fee          => 1000000000000,  %% FIXME config
          nonce        => Nonce,
          payload      => CurrentTopHash},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    %% FIXME: We need to sign this parent chain Tx with the networkId of the parent chain
    SignedSpendTx = sign_tx(SpendTx, StakerPrivKey),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedSpendTx)),
    Body = #{<<"tx">> => Transaction},
    Path = <<"/v3/transactions">>,
    post_request(Path, Body, Host, Port, 5000),
    ok.

%% TODO This function copied from aec_test_utils as that module is not available
%% in normal builds
%% TODO the network ID here should be for the parent chain, but the
%% test_utils code will pick up the local node.
%% TODO - wallet interaction of some kind to get privKey
%% This is a horrible hack for now
%% Maybe aec_preset_keys??
%% {ok, Sig} = aec_preset_keys:sign_tx(SpendTx, AccountId)
%% TODO: redo this whole thing
-define(VALID_PRIVK(K), byte_size(K) =:= 64).

sign_tx(Tx, PrivKey) when is_binary(PrivKey) ->
    sign_tx(Tx, [PrivKey]);
sign_tx(Tx, PrivKeys) when is_list(PrivKeys) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_network_id(Bin0),
    case lists:filter(fun(PrivKey) -> not (?VALID_PRIVK(PrivKey)) end, PrivKeys) of
        [_|_]=BrokenKeys -> erlang:error({invalid_priv_key, BrokenKeys});
        [] -> pass
    end,
    Signatures = [ enacl:sign_detached(BinForNetwork, PrivKey) || PrivKey <- PrivKeys ],
    aetx_sign:new(Tx, Signatures).

get_request(Path, Host, Port, Timeout) ->
    try
    Url = url(binary_to_list(Host), Port, false),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, []},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    lager:info("Req: ~p, with URL: ~ts", [Req, Url]),
    {ok, {{_, 200 = _Code, _}, _, Res}} = httpc:request(get, Req, HTTPOpt, Opt),
    lager:info("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
    {ok, jsx:decode(list_to_binary(Res), [return_maps])}
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
    lager:info("Req: ~p, with URL: ~ts", [Req, Url]),
    {ok, {{_, 200 = _Code, _}, _, Res}} = httpc:request(post, Req, HTTPOpt, Opt),
    lager:info("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
    {ok, jsx:decode(list_to_binary(Res), [return_maps])}
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}}
  end.

url(Host, Port, false) when is_list(Host), is_integer(Port) ->
  path("http://", Host, Port).

path(Scheme, Host, Port) ->
  lists:concat([Scheme, Host, ":", Port]).

