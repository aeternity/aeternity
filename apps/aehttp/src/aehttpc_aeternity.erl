-module(aehttpc_aeternity).

%% Low level subset of Aeternity API required to interact with a hyperchain

%% Required exports:
-export([get_latest_block/5, get_commitment_tx_in_block/6, post_commitment/6]).

%% @doc fetch latest top hash
get_latest_block(Host, Port, _User, _Password, _Seed) ->
    get_top_block_hash(Host, Port).

get_commitment_tx_in_block(Host, Port, _User, _Password, _Seed, BlockHash) ->
    {ok, #{<<"micro_blocks">> := MBs}} = get_generation(Host, Port, BlockHash),
    get_transactions(Host, Port, MBs).

%% @doc Post commitment to AE parent chain.
%% FIXME: When should this be called, how often, and by which accounts?
post_commitment(Host, Port, AccountId, Signature, HCAccountId, CurrentTop) ->
    post_commitment_tx(Host, Port, AccountId, Signature, HCAccountId, CurrentTop).


%%%===================================================================
%%%  AE HTTP protocol
%%%===================================================================
-spec get_top_block_hash(binary(), integer()) -> {ok, binary()} | {error, term()}.
get_top_block_hash(Host, Port) ->
    try
        {ok, #{<<"hash">> := Hash}} =
            get_request(<<"/v2/key-blocks/current/hash">>, Host, Port, 5000),
        {ok, Hash}
    catch E:R ->
        {error, {E, R}}
    end.

-spec get_generation(binary(), integer(), binary()) -> {ok, map()} | {error, term()}.
get_generation(Host, Port, Hash) ->
    Path = <<"/v2/generations/hash/", Hash/binary>>,
    get_request(Path, Host, Port, 5000).

-spec get_transactions(binary(), integer(), [binary()]) -> {ok, map()} | {error, term()}.
get_transactions(Host, Port, MBs) ->
    Txs = lists:flatmap(
        fun(MB) ->
            get_hc_commitments(Host, Port, MB)
        end, MBs),
    {ok, Txs}.

get_hc_commitments(Host, Port, MB) ->
    Path =  <<"/v2/micro-blocks/hash/", MB/binary, "/transactions">>,
    {ok, Res} = get_request(Path, Host, Port, 5000),
    #{<<"transactions">> := Txs} = Res,
    %% TODO - take hc commitment account from some config
    %% Commitments include:
    %%   [{Committer, Committers view of child chain top hash}]
    Commitments = lists:foldl(
            fun(#{<<"tx">> := Tx}, Acc) ->
                    case Tx of
                        #{<<"type">> := <<"SpendTx">>,
                          <<"recipient_id">> := <<"ak_hc_account">>,
                          <<"sender_id">> := SenderId,
                          <<"payload">> := Commitment} ->
                            [{SenderId, Commitment} | Acc];
                        _ ->
                            Acc
                    end
            end, [], Txs),
    {ok, Commitments}.

post_commitment_tx(Host, Port, AccountId, Signature, HCAccountId, CurrentTop) ->
    %% 1. get the next nonce for our account
    NoncePath = <<"/v2/accounts/", AccountId/binary, "next-nonce">>,
    {ok, #{<<"next_nonce">> := Nonce}} = get_request(NoncePath, Host, Port, 5000),
    %% 2. Create a SpendTx containing the commitment in its payload
    TxArgs =
        #{sender_id    => aeser_id:create(account, AccountId),
          recipient_id => aeser_id:create(account, HCAccountId),
          amount       => 10000,       %% FIXME config
          fee          => 1000000000,  %% FIXME config
          nonce        => Nonce,
          payload      => CurrentTop},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    SignedSpendTx = sign_tx(SpendTx, AccountId),
    Body = #{<<"tx">> => SignedSpendTx},
    Path = <<"/v2/transaction">>,
    post_request(Path, Body, Host, Port, 5000),
    ok.

sign_tx(SpendTx, AccountId) ->
    %% TODO - wallet interaction of some kind
    %% For now aec_preset_keys??
    %% {ok, Sig} = aec_preset_keys:sign_tx(SpendTx, AccountId)
    SpendTx.

get_request(Path, Host, Port, Timeout) ->
    try
    Url = url(binary_to_list(Host), Port, false),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, []},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    {ok, {{_, 200 = _Code, _}, _, Res}} = httpc:request(get, Req, HTTPOpt, Opt),
    lager:info("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
    {ok, jsx:decode(list_to_binary(Res), [return_maps])}
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}}
  end.
    
-spec post_request(binary(), binary(), binary(), integer(),integer()) -> {ok, map()} | {error, term()}.
post_request(Path, Body, Host, Port, Timeout) ->
  try
    Url = url(binary_to_list(Host), Port, false),
    UrlPath = lists:concat([Url, binary_to_list(Path)]),
    Req = {UrlPath, [], "application/json", jsx:encode(Body)},
    HTTPOpt = [{timeout, Timeout}],
    Opt = [],
    {ok, {{_, 200 = _Code, _}, _, Res}} = httpc:request(post, Req, HTTPOpt, Opt),
    lager:info("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
    {ok, jsx:decode(list_to_binary(Res), [return_maps])}
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}}
  end.

url(Host, Port, true = _SSL) when is_list(Host), is_integer(Port) ->
  path("https://", Host, Port);
url(Host, Port, _) when is_list(Host), is_integer(Port) ->
  path("http://", Host, Port).

path(Scheme, Host, Port) ->
  lists:concat([Scheme, Host, ":", Port]).
