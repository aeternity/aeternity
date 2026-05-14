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

dispatch_method(<<"ae_chainId">>, _Params) ->
    NetworkId = aec_governance:get_network_id(),
    Numeric = aerpc_chain_id:to_numeric(NetworkId),
    {ok, aerpc_encoding:to_quantity(Numeric)};

dispatch_method(<<"ae_netVersion">>, _Params) ->
    %% Same numeric id as ae_chainId, but emitted as a decimal string
    %% per the net_version convention.
    NetworkId = aec_governance:get_network_id(),
    Numeric = aerpc_chain_id:to_numeric(NetworkId),
    {ok, integer_to_binary(Numeric)};

dispatch_method(<<"ae_getBlockByHash">>, [HashIn, FullTxs])
  when is_binary(HashIn), is_boolean(FullTxs) ->
    aerpc_block:by_hash(HashIn, FullTxs);
dispatch_method(<<"ae_getBlockByHash">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getBlockByNumber">>, [TagOrHex, FullTxs])
  when is_binary(TagOrHex), is_boolean(FullTxs) ->
    aerpc_block:by_height(TagOrHex, FullTxs);
dispatch_method(<<"ae_getBlockByNumber">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getBlockTransactionCountByHash">>, [HashIn])
  when is_binary(HashIn) ->
    aerpc_block:tx_count_by_hash(HashIn);
dispatch_method(<<"ae_getBlockTransactionCountByHash">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getBlockTransactionCountByNumber">>, [TagOrHex])
  when is_binary(TagOrHex) ->
    aerpc_block:tx_count_by_height(TagOrHex);
dispatch_method(<<"ae_getBlockTransactionCountByNumber">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getBalance">>, [AddrIn, TagOrHex])
  when is_binary(AddrIn), is_binary(TagOrHex) ->
    aerpc_account:balance(AddrIn, TagOrHex);
dispatch_method(<<"ae_getBalance">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getCode">>, [AddrIn, _TagOrHex])
  when is_binary(AddrIn) ->
    %% v1: code is read from the latest state only. Historical contract
    %% bytecode lookups need a state-tree walk that does not yet exist
    %% in this module; documented as a v1 limitation.
    aerpc_account:code(AddrIn);
dispatch_method(<<"ae_getCode">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getTransactionCount">>, [AddrIn, TagOrHex])
  when is_binary(AddrIn), is_binary(TagOrHex) ->
    aerpc_account:tx_count(AddrIn, TagOrHex);
dispatch_method(<<"ae_getTransactionCount">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getTransactionByHash">>, [HashIn])
  when is_binary(HashIn) ->
    aerpc_tx:by_hash(HashIn);
dispatch_method(<<"ae_getTransactionByHash">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getTransactionByBlockHashAndIndex">>,
                [BlockHashIn, IndexHex])
  when is_binary(BlockHashIn), is_binary(IndexHex) ->
    try aerpc_encoding:from_quantity(IndexHex) of
        Index -> aerpc_tx:by_block_hash_index(BlockHashIn, Index)
    catch _:_ -> {error, -32602, <<"Invalid params">>}
    end;
dispatch_method(<<"ae_getTransactionByBlockHashAndIndex">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getTransactionByBlockNumberAndIndex">>,
                [TagOrHex, IndexHex])
  when is_binary(TagOrHex), is_binary(IndexHex) ->
    try aerpc_encoding:from_quantity(IndexHex) of
        Index -> aerpc_tx:by_block_height_index(TagOrHex, Index)
    catch _:_ -> {error, -32602, <<"Invalid params">>}
    end;
dispatch_method(<<"ae_getTransactionByBlockNumberAndIndex">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getTransactionReceipt">>, [HashIn])
  when is_binary(HashIn) ->
    aerpc_tx:receipt(HashIn);
dispatch_method(<<"ae_getTransactionReceipt">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_call">>, _Params) ->
    %% v1: gated off. A real implementation builds a synthetic
    %% contract_call_tx and runs it through aec_dry_run:dry_run/4. FATE
    %% contracts cannot produce Eth-ABI-compatible return data without a
    %% bridge, and AEVM-Solidity contracts are effectively unused on
    %% mainnet, so v1 returns -32004 ("operation not supported") for all
    %% callers. Spec stub kept here so the dispatcher table is complete.
    {error, -32004, <<"Operation not supported (FATE contract)">>};

dispatch_method(<<"ae_estimateGas">>, _Params) ->
    %% Same v1 gating as ae_call: returns -32004. A real estimate is the
    %% gas_used field of the dry-run call object.
    {error, -32004, <<"Operation not supported (FATE contract)">>};

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
