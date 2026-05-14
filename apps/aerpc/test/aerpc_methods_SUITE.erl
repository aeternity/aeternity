%%%-------------------------------------------------------------------
%%% @doc Hermetic unit tests for the aerpc dispatcher, JSON-RPC
%%% framing helpers, and the encoding module. Does not boot a node;
%%% the method bodies that touch chain state are covered by the
%%% integration suite (`aehttp_integration_SUITE') in a follow-up.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_methods_SUITE).

%% common_test exports
-export([
          all/0
        ]).

%% test case exports
-export([
          dispatch_invalid_envelope/1
        , dispatch_method_not_found/1
        , dispatch_batch_returns_array/1
        , dispatch_empty_batch_is_invalid/1
        , jsonrpc_result_shape/1
        , jsonrpc_error_shape/1
        , encoding_quantity_roundtrip/1
        , encoding_quantity_zero/1
        , method_ae_accounts/1
        , method_ae_netListening/1
        , method_ae_getStorageAt/1
        , method_ae_getUncleCountByBlockHash/1
        , method_ae_getUncleCountByBlockNumber/1
        , method_ae_getUncleByBlockHashAndIndex/1
        , method_ae_getUncleByBlockNumberAndIndex/1
        , method_ae_netPeerCount/1
        , method_ae_protocolVersion/1
        , method_ae_gasPrice/1
        , method_ae_syncing/1
        , method_ae_sha3_hello_world/1
        , method_ae_sha3_empty/1
        , method_ae_sha3_invalid_params/1
        , encoding_hex_data_roundtrip/1
        , method_ae_chainId/1
        , chain_id_lookup_table/1
        , method_ae_netVersion/1
        , method_ae_getBlockByHash/1
        , method_ae_getBlockByHash_invalid_params/1
        , block_resolve_tag/1
        , method_ae_getBlockByNumber/1
        , method_ae_getBlockTransactionCountByHash/1
        , method_ae_getBlockTransactionCountByNumber/1
        , method_ae_getBalance/1
        , method_ae_getBalance_invalid_address/1
        , account_decode_address/1
        , method_ae_getCode/1
        , method_ae_getTransactionCount/1
        , method_ae_getTransactionByHash/1
        , method_ae_getTransactionByBlockHashAndIndex/1
        , method_ae_getTransactionByBlockNumberAndIndex/1
        , method_ae_getTransactionReceipt/1
        , bloom_empty/1
        , method_ae_call/1
        , method_ae_call_missing_to/1
        , method_ae_call_invalid_params_shape/1
        , method_ae_call_invalid_input_hex/1
        , method_ae_estimateGas/1
        , block_resolve_dry_run_top/1
        , encoding_optional_quantity/1
        , jsonrpc_error_with_data/1
        , method_ae_getLogs/1
        , method_ae_getLogs_invalid_params/1
        , method_ae_getLogs_range_too_wide/1
        , method_ae_getFilterChanges/1
        , method_ae_getFilterLogs/1
        , method_ae_newBlockFilter/1
        , method_ae_newFilter/1
        , method_ae_newPendingTransactionFilter/1
        , method_ae_sendRawTransaction/1
        , method_ae_sendTransaction/1
        , method_ae_sign/1
        , method_ae_signTransaction/1
        , method_ae_uninstallFilter/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [ dispatch_invalid_envelope
    , dispatch_method_not_found
    , dispatch_batch_returns_array
    , dispatch_empty_batch_is_invalid
    , jsonrpc_result_shape
    , jsonrpc_error_shape
    , encoding_quantity_roundtrip
    , encoding_quantity_zero
    , method_ae_accounts
    , method_ae_netListening
    , method_ae_getStorageAt
    , method_ae_getUncleCountByBlockHash
    , method_ae_getUncleCountByBlockNumber
    , method_ae_getUncleByBlockHashAndIndex
    , method_ae_getUncleByBlockNumberAndIndex
    , method_ae_netPeerCount
    , method_ae_protocolVersion
    , method_ae_gasPrice
    , method_ae_syncing
    , method_ae_sha3_hello_world
    , method_ae_sha3_empty
    , method_ae_sha3_invalid_params
    , encoding_hex_data_roundtrip
    , method_ae_chainId
    , chain_id_lookup_table
    , method_ae_netVersion
    , method_ae_getBlockByHash
    , method_ae_getBlockByHash_invalid_params
    , block_resolve_tag
    , method_ae_getBlockByNumber
    , method_ae_getBlockTransactionCountByHash
    , method_ae_getBlockTransactionCountByNumber
    , method_ae_getBalance
    , method_ae_getBalance_invalid_address
    , account_decode_address
    , method_ae_getCode
    , method_ae_getTransactionCount
    , method_ae_getTransactionByHash
    , method_ae_getTransactionByBlockHashAndIndex
    , method_ae_getTransactionByBlockNumberAndIndex
    , method_ae_getTransactionReceipt
    , bloom_empty
    , method_ae_call
    , method_ae_call_missing_to
    , method_ae_call_invalid_params_shape
    , method_ae_call_invalid_input_hex
    , method_ae_estimateGas
    , block_resolve_dry_run_top
    , encoding_optional_quantity
    , jsonrpc_error_with_data
    , method_ae_getLogs
    , method_ae_getLogs_invalid_params
    , method_ae_getLogs_range_too_wide
    , method_ae_getFilterChanges
    , method_ae_getFilterLogs
    , method_ae_newBlockFilter
    , method_ae_newFilter
    , method_ae_newPendingTransactionFilter
    , method_ae_sendRawTransaction
    , method_ae_sendTransaction
    , method_ae_sign
    , method_ae_signTransaction
    , method_ae_uninstallFilter
    ].

%% ===================================================================
%% Dispatcher
%% ===================================================================

dispatch_invalid_envelope(_Config) ->
    Reply = aerpc:dispatch(#{<<"foo">> => <<"bar">>}),
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">>      := null,
                   <<"error">>   := #{<<"code">>    := -32600,
                                      <<"message">> := <<"Invalid Request">>}}, Reply),
    ok.

dispatch_method_not_found(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 7,
            <<"method">>  => <<"ae_doesNotExist">>},
    Reply = aerpc:dispatch(Req),
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">>      := 7,
                   <<"error">>   := #{<<"code">>    := -32601,
                                      <<"message">> := <<"Method not found">>}}, Reply),
    ok.

dispatch_batch_returns_array(_Config) ->
    Req1 = #{<<"jsonrpc">> => <<"2.0">>,
             <<"id">>      => 1,
             <<"method">>  => <<"ae_doesNotExist">>},
    Req2 = #{<<"jsonrpc">> => <<"2.0">>,
             <<"id">>      => 2,
             <<"method">>  => <<"ae_alsoMissing">>},
    Reply = aerpc:dispatch([Req1, Req2]),
    ?assertMatch([#{<<"id">> := 1, <<"error">> := _},
                  #{<<"id">> := 2, <<"error">> := _}], Reply),
    ok.

dispatch_empty_batch_is_invalid(_Config) ->
    Reply = aerpc:dispatch([]),
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32600}}, Reply),
    ok.

%% ===================================================================
%% Envelope helpers
%% ===================================================================

jsonrpc_result_shape(_Config) ->
    R = aerpc_jsonrpc:result(42, <<"ok">>),
    ?assertEqual(#{<<"jsonrpc">> => <<"2.0">>,
                   <<"id">>      => 42,
                   <<"result">>  => <<"ok">>}, R),
    ok.

jsonrpc_error_shape(_Config) ->
    R = aerpc_jsonrpc:error(null, -32700, <<"Parse error">>),
    ?assertEqual(#{<<"jsonrpc">> => <<"2.0">>,
                   <<"id">>      => null,
                   <<"error">>   => #{<<"code">>    => -32700,
                                      <<"message">> => <<"Parse error">>}}, R),
    ok.

%% ===================================================================
%% Encoding
%% ===================================================================

encoding_quantity_roundtrip(_Config) ->
    Cases = [1, 15, 16, 255, 256, 65535, 1207, 16#deadbeef],
    [ ?assertEqual(N, aerpc_encoding:from_quantity(aerpc_encoding:to_quantity(N)))
      || N <- Cases ],
    %% Sanity-check the hex form for a couple of values
    ?assertEqual(<<"0x1">>,       aerpc_encoding:to_quantity(1)),
    ?assertEqual(<<"0xff">>,      aerpc_encoding:to_quantity(255)),
    ?assertEqual(<<"0x4b7">>,     aerpc_encoding:to_quantity(1207)),
    ok.

encoding_quantity_zero(_Config) ->
    ?assertEqual(<<"0x0">>, aerpc_encoding:to_quantity(0)),
    ?assertEqual(0, aerpc_encoding:from_quantity(<<"0x0">>)),
    ok.

%% ===================================================================
%% Method dispatchers
%% ===================================================================

method_ae_accounts(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_accounts">>},
    ?assertMatch(#{<<"id">> := 1, <<"result">> := []},
                 aerpc:dispatch(Req)),
    ok.

method_ae_netListening(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_netListening">>},
    ?assertMatch(#{<<"id">> := 1, <<"result">> := true},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getStorageAt(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getStorageAt">>,
            <<"params">>  => [<<"ct_anything">>, <<"0x0">>, <<"latest">>]},
    Expected = <<"0x", (binary:copy(<<"0">>, 64))/binary>>,
    ?assertEqual(#{<<"jsonrpc">> => <<"2.0">>,
                   <<"id">>      => 1,
                   <<"result">>  => Expected},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getUncleCountByBlockHash(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getUncleCountByBlockHash">>,
            <<"params">>  => [<<"0xdeadbeef">>]},
    ?assertMatch(#{<<"id">> := 1, <<"result">> := <<"0x0">>},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getUncleCountByBlockNumber(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getUncleCountByBlockNumber">>,
            <<"params">>  => [<<"latest">>]},
    ?assertMatch(#{<<"id">> := 1, <<"result">> := <<"0x0">>},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getUncleByBlockHashAndIndex(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getUncleByBlockHashAndIndex">>,
            <<"params">>  => [<<"0xdeadbeef">>, <<"0x0">>]},
    ?assertMatch(#{<<"id">> := 1, <<"result">> := null},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getUncleByBlockNumberAndIndex(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getUncleByBlockNumberAndIndex">>,
            <<"params">>  => [<<"latest">>, <<"0x0">>]},
    ?assertMatch(#{<<"id">> := 1, <<"result">> := null},
                 aerpc:dispatch(Req)),
    ok.

method_ae_netPeerCount(_Config) ->
    %% Hermetic: only verify dispatcher routes the method. Actual count
    %% requires aecore running and is covered by the integration suite.
    routed(<<"ae_netPeerCount">>).

method_ae_protocolVersion(_Config) ->
    routed(<<"ae_protocolVersion">>).

method_ae_gasPrice(_Config) ->
    routed(<<"ae_gasPrice">>).

method_ae_syncing(_Config) ->
    routed(<<"ae_syncing">>).

%% Canonical Keccak-256 vector. If this fails, the underlying `sha3'
%% dep is producing NIST SHA3-256 instead -- a release-blocking issue.
method_ae_sha3_hello_world(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_sha3">>,
            <<"params">>  => [<<"0x68656c6c6f20776f726c64">>]},
    Expected = <<"0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad">>,
    ?assertEqual(#{<<"jsonrpc">> => <<"2.0">>,
                   <<"id">>      => 1,
                   <<"result">>  => Expected},
                 aerpc:dispatch(Req)),
    ok.

method_ae_sha3_empty(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_sha3">>,
            <<"params">>  => [<<"0x">>]},
    Expected = <<"0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470">>,
    ?assertEqual(#{<<"jsonrpc">> => <<"2.0">>,
                   <<"id">>      => 1,
                   <<"result">>  => Expected},
                 aerpc:dispatch(Req)),
    ok.

method_ae_sha3_invalid_params(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_sha3">>,
            <<"params">>  => [<<"not-hex">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_chainId(_Config) ->
    routed(<<"ae_chainId">>).

method_ae_netVersion(_Config) ->
    routed(<<"ae_netVersion">>).

method_ae_getBlockByHash(_Config) ->
    routed(<<"ae_getBlockByHash">>).

method_ae_getBlockByHash_invalid_params(_Config) ->
    %% Non-boolean second arg triggers -32602 without touching the chain.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getBlockByHash">>,
            <<"params">>  => [<<"kh_xxx">>, <<"not-a-bool">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getBlockByNumber(_Config) ->
    routed(<<"ae_getBlockByNumber">>).

method_ae_getBlockTransactionCountByHash(_Config) ->
    routed(<<"ae_getBlockTransactionCountByHash">>).

method_ae_getBlockTransactionCountByNumber(_Config) ->
    routed(<<"ae_getBlockTransactionCountByNumber">>).

method_ae_getBalance(_Config) ->
    routed(<<"ae_getBalance">>).

method_ae_getBalance_invalid_address(_Config) ->
    %% Garbage address fails decoding before any chain call.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getBalance">>,
            <<"params">>  => [<<"not-an-address">>, <<"latest">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getCode(_Config) ->
    routed(<<"ae_getCode">>).

method_ae_getTransactionCount(_Config) ->
    routed(<<"ae_getTransactionCount">>).

method_ae_getTransactionByHash(_Config) ->
    routed(<<"ae_getTransactionByHash">>).

method_ae_getTransactionByBlockHashAndIndex(_Config) ->
    routed(<<"ae_getTransactionByBlockHashAndIndex">>).

method_ae_getTransactionByBlockNumberAndIndex(_Config) ->
    routed(<<"ae_getTransactionByBlockNumberAndIndex">>).

method_ae_getTransactionReceipt(_Config) ->
    routed(<<"ae_getTransactionReceipt">>).

bloom_empty(_Config) ->
    %% Hermetic: the v1 bloom is always 256 zero bytes (= 512 hex chars
    %% plus the 0x prefix).
    Empty = aerpc_bloom:empty(),
    ?assertEqual(<<"0x", (binary:copy(<<"0">>, 512))/binary>>, Empty),
    ?assertEqual(Empty, aerpc_bloom:of_logs([])),
    ok.

method_ae_call(_Config) ->
    %% Well-formed envelope, malformed `to' value -> -32602 (address
    %% decoding fails before dry-run is invoked).
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_call">>,
            <<"params">>  => [#{<<"to">> => <<"not-a-contract-id">>},
                              <<"latest">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_call_missing_to(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_call">>,
            <<"params">>  => [#{}, <<"latest">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_call_invalid_params_shape(_Config) ->
    %% A non-map TxObj should be rejected by the dispatcher guard.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_call">>,
            <<"params">>  => [<<"not-a-map">>, <<"latest">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_call_invalid_input_hex(_Config) ->
    %% `input' present but not hex-decodable -> -32602.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_call">>,
            <<"params">>  => [#{<<"to">> =>
                                    <<"0x", (binary:copy(<<"00">>, 32))/binary>>,
                                <<"input">> => <<"not-hex">>},
                              <<"latest">>]},
    %% Either -32602 (input rejected) or -32603 (dry-run error
    %% propagated from the no-aecore hermetic env). Both indicate the
    %% method was routed and reached the call adapter; either is
    %% acceptable for the hermetic case.
    Reply = aerpc:dispatch(Req),
    ?assertMatch(#{<<"id">> := 1, <<"error">> := #{<<"code">> := Code}}
                   when Code =:= -32602 orelse Code =:= -32603, Reply),
    ok.

method_ae_estimateGas(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_estimateGas">>,
            <<"params">>  => [#{<<"to">> => <<"not-a-contract-id">>}]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

%% Hermetic check on the dry-run-top resolver: `latest' (and friends)
%% map to the symbolic `top'; explicit hex maps to {height, N}.
block_resolve_dry_run_top(_Config) ->
    ?assertEqual({ok, top},          aerpc_block:resolve_dry_run_top(<<"latest">>)),
    ?assertEqual({ok, top},          aerpc_block:resolve_dry_run_top(<<"pending">>)),
    ?assertEqual({ok, top},          aerpc_block:resolve_dry_run_top(<<"safe">>)),
    ?assertEqual({ok, top},          aerpc_block:resolve_dry_run_top(<<"finalized">>)),
    ?assertEqual({ok, {height, 0}},  aerpc_block:resolve_dry_run_top(<<"earliest">>)),
    ?assertEqual({ok, {height, 16}}, aerpc_block:resolve_dry_run_top(<<"0x10">>)),
    ?assertMatch({error, -32602, _}, aerpc_block:resolve_dry_run_top(<<"banana">>)),
    ok.

%% Hermetic check on the new optional-quantity helper.
encoding_optional_quantity(_Config) ->
    ?assertEqual(0,       aerpc_encoding:from_optional_quantity(undefined, 0)),
    ?assertEqual(7,       aerpc_encoding:from_optional_quantity(undefined, 7)),
    ?assertEqual(0,       aerpc_encoding:from_optional_quantity(<<>>, 0)),
    ?assertEqual(255,     aerpc_encoding:from_optional_quantity(<<"0xff">>, 0)),
    ?assertEqual(15,      aerpc_encoding:from_optional_quantity(<<"f">>, 0)),
    ok.

%% Hermetic check on the new 4-arg envelope helper. Verifies that the
%% optional `data' field is rendered into the error object.
jsonrpc_error_with_data(_Config) ->
    R = aerpc_jsonrpc:error(1, -32003, <<"execution reverted">>,
                            <<"0xdeadbeef">>),
    ?assertEqual(#{<<"jsonrpc">> => <<"2.0">>,
                   <<"id">>      => 1,
                   <<"error">>   => #{<<"code">>    => -32003,
                                      <<"message">> => <<"execution reverted">>,
                                      <<"data">>    => <<"0xdeadbeef">>}}, R),
    ok.

method_ae_getLogs(_Config) ->
    routed(<<"ae_getLogs">>).

method_ae_getLogs_invalid_params(_Config) ->
    %% Garbage `topics' shape (a bare binary instead of a list) fails
    %% before any chain access.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getLogs">>,
            <<"params">>  => [#{<<"topics">> => <<"not-a-list">>}]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32602}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getLogs_range_too_wide(_Config) ->
    %% A 0..2000 generation request exceeds the v1 max range cap; this is
    %% hermetically testable because the range check happens before any
    %% chain access.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getLogs">>,
            <<"params">>  =>
                [#{<<"fromBlock">> => <<"0x0">>,
                   <<"toBlock">>   => <<"0x7d0">>}]},  %% 2000
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32005}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getFilterChanges(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getFilterChanges">>,
            <<"params">>  => [<<"0x1">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32004}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_getFilterLogs(_Config) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_getFilterLogs">>,
            <<"params">>  => [<<"0x1">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32004}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_newBlockFilter(_Config) ->
    %% v1.5-deferred: filter registry not yet implemented.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_newBlockFilter">>},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32004}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_newFilter(_Config) ->
    %% v1.5-deferred: filter registry not yet implemented.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_newFilter">>,
            <<"params">>  => [#{<<"address">> => <<"ct_xxx">>}]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32004}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_newPendingTransactionFilter(_Config) ->
    %% v1.5-deferred: filter registry not yet implemented.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_newPendingTransactionFilter">>},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32004}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_sendRawTransaction(_Config) ->
    %% v1: write-path methods are explicitly out-of-scope and return
    %% -32601. Same observable shape as the catch-all, but kept as an
    %% explicit clause so the dispatcher table documents the
    %% deliberate exclusion.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_sendRawTransaction">>,
            <<"params">>  => [<<"0xdeadbeef">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">>    := -32601,
                                    <<"message">> := <<"Method not found">>}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_sendTransaction(_Config) ->
    %% v1: write-path stub. Node does not host wallet keys.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_sendTransaction">>,
            <<"params">>  => [#{<<"from">> => <<"ak_xxx">>,
                                <<"to">>   => <<"ak_yyy">>,
                                <<"value">> => <<"0x1">>}]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">>    := -32601,
                                    <<"message">> := <<"Method not found">>}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_sign(_Config) ->
    %% v1: write-path stub. AE accounts use ed25519, not secp256k1, and
    %% the eth signed-message prefix has no AE analogue.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_sign">>,
            <<"params">>  => [<<"ak_xxx">>, <<"0xdeadbeef">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">>    := -32601,
                                    <<"message">> := <<"Method not found">>}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_signTransaction(_Config) ->
    %% v1: write-path stub. Same reasoning as ae_sign + ae_sendTransaction.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_signTransaction">>,
            <<"params">>  => [#{<<"from">> => <<"ak_xxx">>,
                                <<"to">>   => <<"ak_yyy">>,
                                <<"value">> => <<"0x1">>}]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">>    := -32601,
                                    <<"message">> := <<"Method not found">>}},
                 aerpc:dispatch(Req)),
    ok.

method_ae_uninstallFilter(_Config) ->
    %% v1.5-deferred: filter registry not yet implemented.
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => <<"ae_uninstallFilter">>,
            <<"params">>  => [<<"0x1">>]},
    ?assertMatch(#{<<"id">> := 1,
                   <<"error">> := #{<<"code">> := -32004}},
                 aerpc:dispatch(Req)),
    ok.

%% Hermetic: a 0x-hex of the right length is always accepted; anything
%% else without the expected prefix is rejected.
account_decode_address(_Config) ->
    %% 32-byte 0x form.
    Bin = binary:copy(<<16#aa>>, 32),
    Hex = aerpc_encoding:to_hex_data(Bin),
    ?assertEqual({ok, Bin}, aerpc_account:decode_address(Hex)),
    %% Wrong byte length.
    ?assertMatch({error, -32602, _},
                 aerpc_account:decode_address(<<"0xdeadbeef">>)),
    %% Garbage.
    ?assertMatch({error, -32602, _},
                 aerpc_account:decode_address(<<"banana">>)),
    ok.

%% Hermetic: tag resolution for any tag that does NOT consult the
%% chain. "earliest" returns 0 deterministically; bad input returns
%% an invalid-params error tuple.
block_resolve_tag(_Config) ->
    ?assertEqual({ok, 0}, aerpc_block:resolve_tag(<<"earliest">>)),
    ?assertEqual({ok, 16#deadbeef},
                 aerpc_block:resolve_tag(<<"0xdeadbeef">>)),
    ?assertMatch({error, -32602, _},
                 aerpc_block:resolve_tag(<<"banana">>)),
    ok.

chain_id_lookup_table(_Config) ->
    %% Pure lookup: hermetically testable.
    ?assertEqual(1247, aerpc_chain_id:to_numeric(<<"ae_mainnet">>)),
    ?assertEqual(1248, aerpc_chain_id:to_numeric(<<"ae_uat">>)),
    ?assertEqual(9991, aerpc_chain_id:to_numeric(<<"ae_dev1">>)),
    ?assertEqual(0,    aerpc_chain_id:to_numeric(<<"ae_unknown_net">>)),
    ok.

encoding_hex_data_roundtrip(_Config) ->
    Cases = [<<>>, <<0>>, <<"abc">>, <<16#de, 16#ad, 16#be, 16#ef>>],
    [ ?assertEqual(B, aerpc_encoding:from_hex_data(aerpc_encoding:to_hex_data(B)))
      || B <- Cases ],
    ?assertEqual(<<"0x">>, aerpc_encoding:to_hex_data(<<>>)),
    ?assertEqual(<<"0xdeadbeef">>,
                 aerpc_encoding:to_hex_data(<<16#de, 16#ad, 16#be, 16#ef>>)),
    ok.

%% ===================================================================
%% Helpers
%% ===================================================================

%% Verify the dispatcher has a clause for the given method. Returns ok
%% whether the underlying call succeeds, fails with an internal error,
%% or crashes (typical in a hermetic environment with no apps started).
%% Fails only if the catch-all method-not-found clause fires.
routed(Method) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">>      => 1,
            <<"method">>  => Method},
    try aerpc:dispatch(Req) of
        #{<<"error">> := #{<<"code">> := -32601}} ->
            ct:fail({not_routed, Method});
        _ ->
            ok
    catch
        _:_ -> ok
    end.
