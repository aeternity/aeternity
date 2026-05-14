%%%-------------------------------------------------------------------
%%% @doc Method dispatcher. One clause per supported `ae_*' method;
%%% the catch-all clause returns the JSON-RPC method-not-found error.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_methods).

-export([dispatch_method/2]).

-spec dispatch_method(binary(), term()) ->
    {ok, term()} | {error, integer(), binary()}.
dispatch_method(<<"ae_clientVersion">>, _Params) ->
    Vsn = aeapi:node_version(),
    Rev = aeapi:node_revision(),
    {ok, <<"aeternity/", Vsn/binary, "/", Rev/binary>>};

dispatch_method(<<"ae_blockNumber">>, _Params) ->
    case aec_chain:top_header() of
        undefined ->
            {error, -32603, <<"Chain not initialized">>};
        Header ->
            Height = aec_headers:height(Header),
            {ok, aerpc_encoding:to_quantity(Height)}
    end;

dispatch_method(<<"ae_accounts">>, _Params) ->
    %% Node never holds user wallet keys.
    {ok, []};

dispatch_method(<<"ae_netListening">>, _Params) ->
    %% If the node is able to answer this RPC it is, by definition, listening.
    {ok, true};

dispatch_method(<<"ae_getStorageAt">>, _Params) ->
    %% FATE contracts use a typed key-value store that does not map onto
    %% EVM-style 32-byte slot indices. v1 returns a zero word; a real
    %% mapping is deferred until a slot-derivation convention is agreed.
    {ok, <<"0x", (binary:copy(<<"0">>, 64))/binary>>};

dispatch_method(<<"ae_getUncleCountByBlockHash">>, _Params) ->
    %% AE has no uncles (Bitcoin-NG generations replace the uncle scheme).
    {ok, <<"0x0">>};

dispatch_method(<<"ae_getUncleCountByBlockNumber">>, _Params) ->
    %% AE has no uncles.
    {ok, <<"0x0">>};

dispatch_method(<<"ae_getUncleByBlockHashAndIndex">>, _Params) ->
    %% AE has no uncles.
    {ok, null};

dispatch_method(<<"ae_getUncleByBlockNumberAndIndex">>, _Params) ->
    %% AE has no uncles.
    {ok, null};

dispatch_method(<<"ae_netPeerCount">>, _Params) ->
    %% Active connections, not "known peers" -- closer to eth's net_peerCount
    %% semantics.
    Count = aec_peers:count(connections),
    {ok, aerpc_encoding:to_quantity(Count)};

dispatch_method(<<"ae_protocolVersion">>, _Params) ->
    %% AE consensus protocol number for the current top key-block, emitted
    %% as a decimal string. Spec is largely advisory (Geth itself dropped it).
    case aec_chain:top_header() of
        undefined ->
            {error, -32603, <<"Chain not initialized">>};
        Header ->
            {ok, integer_to_binary(aec_headers:version(Header))}
    end;

dispatch_method(<<"ae_gasPrice">>, _Params) ->
    %% v1: report the mempool's minimum-miner-gas-price filter (operator-
    %% controlled plus protocol minimum). A walk over recent blocks for
    %% a network-derived estimate would belong in aecore (not aehttp) to
    %% keep aerpc dependency-clean; deferred.
    Price = aec_tx_pool:minimum_miner_gas_price(),
    {ok, aerpc_encoding:to_quantity(Price)};

dispatch_method(<<"ae_sha3">>, [HexIn]) when is_binary(HexIn) ->
    %% Keccak-256 of the supplied bytes. Uses the same `sha3' dep that
    %% backs aec_hash:hash(evm, _) -- which is configured to produce the
    %% Ethereum-flavoured Keccak variant, not NIST SHA3-256.
    try aerpc_encoding:from_hex_data(HexIn) of
        Bin ->
            Digest = sha3:hash(256, Bin),
            {ok, aerpc_encoding:to_hex_data(Digest)}
    catch
        _:_ -> {error, -32602, <<"Invalid params">>}
    end;
dispatch_method(<<"ae_sha3">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_syncing">>, _Params) ->
    %% Returns `false' when fully synced or an object with starting/current/
    %% highest block heights. AE's sync_progress emits {Syncing, Progress,
    %% Top}; we derive `highestBlock' from progress and emit "0x0" for
    %% `startingBlock' until we capture the boot-time height somewhere
    %% durable.
    case aeapi:sync_progress() of
        {false, _, _} ->
            {ok, false};
        {true, Progress, Top} ->
            Highest = case Progress of
                          P when is_float(P), P > 0.001 ->
                              max(Top, round(Top / P));
                          _ ->
                              Top
                      end,
            {ok, #{<<"startingBlock">> => <<"0x0">>,
                   <<"currentBlock">>  => aerpc_encoding:to_quantity(Top),
                   <<"highestBlock">>  => aerpc_encoding:to_quantity(Highest)}}
    end;

dispatch_method(_Method, _Params) ->
    {error, -32601, <<"Method not found">>}.
