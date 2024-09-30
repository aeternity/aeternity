%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc
%%% Gather cild->parent chain pinning information for AE HC and make it 
%%% available through API
%%% @end
%%%-------------------------------------------------------------------

-module(aec_pinning_agent).

-export([get_pinning_data/0,
        encode_pin_payload/1,
        decode_pin_payload/1,
        get_pins/3,
        create_pin_tx/6,
        post_pin_tx/2]).

-define(PIN_TAG, <<"pin">>).

-spec get_pinning_data() -> 
    #{epoch => integer(), 
        height => integer(), 
        block_hash => binary(), 
        parent_type => binary(), 
        parent_network_id => binary()}.
get_pinning_data() ->
    {ok, #{epoch := Epoch,
        first := First,
        last := _Last,
        length := _Length}} = aec_chain_hc:epoch_info(),
    %BlockHash = aec_chain:get_block_hash_optionally_by_hash_or_height(First-1),
    {ok, BlockHash} = aec_chain_state:get_key_block_hash_at_height(First-1),
    ConMod = aec_parent_connector:get_parent_conn_mod(),
    {_,Type} = lists:split(8,atom_to_list(ConMod)), % split off "aehttpc_" from mod name to get type
    
    #{epoch => Epoch-1,
        height => First-1,
        block_hash => BlockHash,
        parent_type => Type,
        parent_network_id => aec_parent_connector:get_network_id()}.


-spec encode_pin_payload(#{epoch => integer(), height => integer(), block_hash => binary()}) -> binary(). 
encode_pin_payload(#{epoch := _Epoch, height := _Height, block_hash := BlockHash}) when byte_size(BlockHash) > 64 ->
    {error, <<"pin payload larger than 80 bytes">>};
encode_pin_payload(#{epoch := Epoch, height := Height, block_hash := BlockHash}) ->
    <<Epoch:32/integer, Height:32/integer, BlockHash/binary>>.

-spec decode_pin_payload(binary()) -> #{epoch => integer(), height => integer(), block_hash => binary()}. 
decode_pin_payload(<<Epoch:32/integer,Height:32/integer,BlockHash/binary>>) ->
    #{epoch => Epoch, height => Height, block_hash => BlockHash}.


-spec get_pins(aehttpc:node_spec(), [binary()], binary()) -> {ok, list()} | {error, term()}.
get_pins(NodeSpec, MBs, ParentHCAccountPubKey) ->
    Txs = lists:flatmap(
        fun(MB) ->
            get_hc_pins(NodeSpec, MB, ParentHCAccountPubKey)
        end, MBs),
    {ok, Txs}.

get_hc_pins(NodeSpec, MB, ParentHCAccountPubKey) ->
    Path =  <<"/v3/micro-blocks/hash/", MB/binary, "/transactions">>,
    {ok, Res} = get_request(Path, NodeSpec, 5000),
    #{<<"transactions">> := Txs} = Res,
    ExpectedRecipient = aeser_api_encoder:encode(account_pubkey, ParentHCAccountPubKey),
    lists:foldl(
            fun(#{<<"tx">> := Tx}, Acc) ->
                    case Tx of
                        #{<<"type">> := <<"SpendTx">>,
                          <<"recipient_id">> := ExpectedRecipient,
                          <<"sender_id">> := _SenderId,
                          <<"payload">> := PinEnc} ->
                                {ok, Pin} = aeser_api_encoder:safe_decode(bytearray, PinEnc),
                                DecodedPin = decode_pin_payload(Pin),
                                [DecodedPin | Acc];
                        _ ->
                            Acc
                    end
            end, [], Txs).

-spec create_pin_tx(binary(), binary(), binary(), integer(), integer(), binary()) -> binary().
create_pin_tx(NodeSpec, SenderEnc, ReceiverPubkey, Amount, Fee,
                   PinPayload) ->
    %% 1. get the next nonce for our account over at the parent chain
    {ok, SenderPubkey} = aeser_api_encoder:safe_decode(account_pubkey,
                                                        SenderEnc),
    %% FIXME consider altenrative approaches to fetching a nonce: ex. if there
    %% is a hanging transaction in the pool this would produce another hanging
    %% one
    NoncePath = <<"/v3/accounts/", SenderEnc/binary, "/next-nonce">>,
    {ok, #{<<"next_nonce">> := Nonce}} = get_request(NoncePath, NodeSpec, 5000),
    %% 2. Create a SpendTx containing the Pin in its payload
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
          payload      => PinPayload},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    SpendTx.

post_pin_tx(SignedSpendTx, NodeSpec) ->
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




%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    #{epoch := 123, height := 456, block_hash := <<"a binary?">>} = 
        decode_pin_payload(encode_pin_payload(#{epoch => 123, height => 456, block_hash => <<"a binary?">>})).

-endif.