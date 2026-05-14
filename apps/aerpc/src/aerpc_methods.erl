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

%% Bulk receipt fetch. Accepts either a tag/height (latest, 0x..., kh_..)
%% or a block hash (kh_..., 0x...-32 bytes). The hash form gives the
%% indexer reorg safety: ask for the hash of the block whose logs you
%% just processed, not "latest" -- which might have advanced.
dispatch_method(<<"ae_getBlockReceipts">>, [HashOrTag])
  when is_binary(HashOrTag) ->
    case HashOrTag of
        <<"kh_", _/binary>> -> aerpc_tx:block_receipts_by_hash(HashOrTag);
        <<"0x", Hex/binary>> when byte_size(Hex) =:= 64 ->
            aerpc_tx:block_receipts_by_hash(HashOrTag);
        _Other ->
            aerpc_tx:block_receipts_by_height(HashOrTag)
    end;
dispatch_method(<<"ae_getBlockReceipts">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_call">>, [TxObj, BlockId])
  when is_map(TxObj), is_binary(BlockId) ->
    aerpc_call:call(TxObj, BlockId);
dispatch_method(<<"ae_call">>, [TxObj]) when is_map(TxObj) ->
    aerpc_call:call(TxObj, <<"latest">>);
dispatch_method(<<"ae_call">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_estimateGas">>, [TxObj, BlockId])
  when is_map(TxObj), is_binary(BlockId) ->
    aerpc_call:estimate_gas(TxObj, BlockId);
dispatch_method(<<"ae_estimateGas">>, [TxObj]) when is_map(TxObj) ->
    aerpc_call:estimate_gas(TxObj, <<"latest">>);
dispatch_method(<<"ae_estimateGas">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getLogs">>, [Filter]) when is_map(Filter) ->
    aerpc_logs:get_logs(Filter);
dispatch_method(<<"ae_getLogs">>, _Params) ->
    {error, -32602, <<"Invalid params">>};

dispatch_method(<<"ae_getFilterChanges">>, _Params) ->
    %% Requires a server-side filter registry (aerpc_filter_registry),
    %% deferred to v1.5 alongside ae_newFilter / ae_newBlockFilter /
    %% ae_newPendingTransactionFilter / ae_uninstallFilter.
    {error, -32004, <<"Filter registry not yet implemented (v1.5)">>};

dispatch_method(<<"ae_getFilterLogs">>, _Params) ->
    %% Same gating as ae_getFilterChanges: needs the filter registry.
    {error, -32004, <<"Filter registry not yet implemented (v1.5)">>};

dispatch_method(<<"ae_newBlockFilter">>, _Params) ->
    %% Allocates a server-side filter that fires on every new key-block.
    %% Needs the filter registry (aerpc_filter_registry); deferred to v1.5
    %% alongside the rest of the filter family.
    {error, -32004, <<"Filter registry not yet implemented (v1.5)">>};

dispatch_method(<<"ae_newFilter">>, _Params) ->
    %% Allocates a server-side log filter from the supplied criteria
    %% (address / topics / fromBlock / toBlock). Needs the filter registry
    %% (aerpc_filter_registry) for state + idle-TTL eviction; deferred to
    %% v1.5 alongside the rest of the filter family.
    {error, -32004, <<"Filter registry not yet implemented (v1.5)">>};

dispatch_method(<<"ae_newPendingTransactionFilter">>, _Params) ->
    %% Allocates a server-side filter that fires on every new mempool
    %% insertion (aec_events:tx_received). Needs the filter registry
    %% (aerpc_filter_registry); deferred to v1.5 alongside the rest of
    %% the filter family.
    {error, -32004, <<"Filter registry not yet implemented (v1.5)">>};

dispatch_method(<<"ae_subscribe">>, _Params) ->
    %% Subscriptions require an open WS connection so the registry can
    %% monitor + route notifications. Over plain HTTP we return -32004
    %% with a hint pointing at /v3/rpc/ws.
    {error, -32004,
     <<"Subscriptions require the WebSocket transport (/v3/rpc/ws)">>};

dispatch_method(<<"ae_unsubscribe">>, _Params) ->
    {error, -32004,
     <<"Subscriptions require the WebSocket transport (/v3/rpc/ws)">>};

dispatch_method(<<"ae_uninstallFilter">>, _Params) ->
    %% Releases a previously-registered filter (id passed as a hex
    %% QUANTITY). Trivial counterpart to the new*Filter callers; needs
    %% the filter registry (aerpc_filter_registry); deferred to v1.5
    %% alongside the rest of the filter family.
    {error, -32004, <<"Filter registry not yet implemented (v1.5)">>};

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

dispatch_method(<<"ae_sendRawTransaction">>, _Params) ->
    %% Submits a pre-signed RLP-encoded payload to the mempool. Out of
    %% scope for v1: AE uses ed25519 + a tagged-RLP envelope, so an
    %% externally-signed payload has no AE pubkey to settle against and
    %% would fail aetx_sign:deserialize_from_binary/1. A future phase
    %% bridges this via a GA-auth contract (per-EOA secp256k1 verifier)
    %% or a node-controlled relay key. Explicit clause kept here so the
    %% dispatcher table is complete and the deliberate exclusion is
    %% visible in the source.
    {error, -32601, <<"Method not found">>};

dispatch_method(<<"ae_sendTransaction">>, _Params) ->
    %% Build, sign (with a node-held key), and submit a tx. Out of scope
    %% for v1: the node does not host user wallet keys, and the signature
    %% scheme + envelope shape differ from AE-native txs. Explicit clause
    %% kept here so the dispatcher table is complete.
    {error, -32601, <<"Method not found">>};

dispatch_method(<<"ae_sign">>, _Params) ->
    %% Signs an arbitrary message with a node-held key under the eth
    %% "\x19Ethereum Signed Message:\n" prefix. Out of scope for v1:
    %% the node does not host user wallet keys, AE accounts use ed25519
    %% (not secp256k1), and the prefix has no AE analogue. Explicit
    %% clause kept here so the dispatcher table is complete.
    {error, -32601, <<"Method not found">>};

dispatch_method(<<"ae_signTransaction">>, _Params) ->
    %% Signs a tx object with a node-held key and returns the RLP-encoded
    %% bytes. Out of scope for v1, same reasons as ae_sign +
    %% ae_sendTransaction: no node-held keys, no secp256k1 path, no
    %% Eth-RLP envelope. Real signing belongs in client-side wallets.
    {error, -32601, <<"Method not found">>};

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
