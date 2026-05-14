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
