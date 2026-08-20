%%%-------------------------------------------------------------------
%%% @doc Unit coverage for the eth-compatibility contract: the wire
%%% namespace, the `0x' encoding policy, the read-only surface, the
%%% operator-settable chain id and batch cap, and the fact that nothing
%%% in this app schedules per-block work on its own.
%%%
%%% Deliberately hermetic -- no node, no chain, no ETS beyond what the
%%% code under test creates. Everything asserted here is a property of
%%% the wire contract, so it must hold without a chain behind it.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_p2_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CHAIN_ID, 1234).

%% Keccak-256 of the empty string, the Ethereum value. NIST SHA3-256 of
%% the same input is a7ffc6f8...; asserting this one is what proves the
%% `sha3' dep is configured for the eth-flavoured variant, which the
%% logs bloom also depends on.
-define(KECCAK_EMPTY,
        <<"0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470">>).

%% ===================================================================
%% Namespace: eth_* / net_* / web3_*, and no ae_* aliases
%% ===================================================================

namespace_test_() ->
    [{"the eth namespace answers",
      fun() ->
          ?assertEqual({ok, []},   dispatch(<<"eth_accounts">>)),
          ?assertEqual({ok, true}, dispatch(<<"net_listening">>)),
          ?assertMatch({ok, <<"0x0">>},
                       dispatch(<<"eth_getUncleCountByBlockHash">>)),
          ?assertMatch({ok, null},
                       dispatch(<<"eth_getUncleByBlockHashAndIndex">>))
      end},
     {"the folded namespaces are net_* / web3_*, not eth_*",
      fun() ->
          %% These four were ae_netVersion / ae_netListening /
          %% ae_netPeerCount / ae_clientVersion on the source branch.
          %% A blanket ae_ -> eth_ rename would have produced
          %% eth_netVersion, which no client ever calls.
          ?assertMatch({error, -32601, _}, dispatch(<<"eth_netVersion">>)),
          ?assertMatch({error, -32601, _}, dispatch(<<"eth_netListening">>)),
          ?assertMatch({error, -32601, _}, dispatch(<<"eth_netPeerCount">>)),
          ?assertMatch({error, -32601, _}, dispatch(<<"eth_clientVersion">>)),
          ?assertMatch({error, -32601, _}, dispatch(<<"eth_sha3">>))
      end},
     {"no ae_* alias survives",
      fun() ->
          [?assertMatch({error, -32601, _}, dispatch(M))
           || M <- [<<"ae_accounts">>, <<"ae_chainId">>, <<"ae_netVersion">>,
                    <<"ae_netListening">>, <<"ae_clientVersion">>,
                    <<"ae_getLogs">>, <<"ae_call">>, <<"ae_sha3">>,
                    <<"ae_blockNumber">>, <<"ae_subscribe">>]]
      end},
     {"web3_sha3 is Keccak-256, not NIST SHA3",
      fun() ->
          ?assertEqual({ok, ?KECCAK_EMPTY},
                       aerpc_methods:dispatch_method(<<"web3_sha3">>,
                                                     [<<"0x">>]))
      end}].

%% ===================================================================
%% Read-only: nothing on this surface can mutate chain state
%% ===================================================================

read_only_test_() ->
    [{"every write method is absent",
      fun() ->
          [?assertMatch({error, -32601, _}, dispatch(M))
           || M <- [<<"eth_sendRawTransaction">>, <<"eth_sendTransaction">>,
                    <<"eth_sign">>, <<"eth_signTransaction">>]]
      end},
     {"eth_accounts is empty -- the node holds no wallet keys",
      fun() -> ?assertEqual({ok, []}, dispatch(<<"eth_accounts">>)) end}].

%% ===================================================================
%% Encoding: 0x everywhere, 20-byte addresses, 32-byte hashes
%% ===================================================================

encoding_test_() ->
    Pubkey = <<7:32/unit:8>>,
    Hash   = <<9:32/unit:8>>,
    [{"addresses are 0x + 40 hex, hashes 0x + 64, all lower-case",
      fun() ->
          %% Widths are not interchangeable: ethers validates every
          %% address-typed field and reads a non-20-byte string in a
          %% request as an ENS name.
          [begin
               ?assertMatch(<<"0x", _/binary>>, V),
               ?assertEqual(42, byte_size(V)),
               ?assertEqual(V, string:lowercase(V))
           end
           || V <- [aerpc_encoding:format_account(Pubkey),
                    aerpc_encoding:format_contract(Pubkey)]],
          [begin
               ?assertMatch(<<"0x", _/binary>>, V),
               ?assertEqual(66, byte_size(V)),
               ?assertEqual(V, string:lowercase(V))
           end
           || V <- [aerpc_encoding:format_key_block_hash(Hash),
                    aerpc_encoding:format_micro_block_hash(Hash),
                    aerpc_encoding:format_tx_hash(Hash)]]
      end},
     {"quantities are minimal hex, data keeps its leading zeros",
      fun() ->
          ?assertEqual(<<"0x0">>,  aerpc_encoding:to_quantity(0)),
          ?assertEqual(<<"0xff">>, aerpc_encoding:to_quantity(255)),
          ?assertEqual(<<"0x0001">>, aerpc_encoding:to_hex_data(<<0, 1>>)),
          ?assertEqual(255, aerpc_encoding:from_quantity(<<"0xFF">>)),
          ?assertEqual(<<0, 1>>, aerpc_encoding:from_hex_data(<<"0x0001">>))
      end},
     {"eth_getStorageAt returns a full zero word",
      fun() ->
          ?assertEqual({ok, aerpc_encoding:zero_word()},
                       dispatch(<<"eth_getStorageAt">>))
      end},
     {"the 32-byte form is still accepted on input",
      fun() ->
          %% Only the emitted side narrows; an AE-aware caller holding a
          %% full pubkey must not be locked out.
          Wide = aerpc_encoding:to_hex_data(Pubkey),
          ?assertEqual({ok, Pubkey}, aerpc_account:decode_address(Wide)),
          ?assertMatch({error, -32602, _},
                       aerpc_account:decode_address(<<"0xdeadbeef">>)),
          ?assertMatch({error, -32602, _},
                       aerpc_account:decode_address(<<"not-an-address">>))
      end}].

%% ===================================================================
%% Chain id: operator-settable, placeholder table as fallback
%% ===================================================================

chain_id_test_() ->
    {setup,
     fun() -> application:get_env(aerpc, chain_id) end,
     fun(Saved) -> restore_env(chain_id, Saved) end,
     [{"an operator value wins over the placeholder table",
       fun() ->
           application:set_env(aerpc, chain_id, 424242),
           ?assertEqual(424242, aerpc_chain_id:configured()),
           ?assertEqual(424242, aerpc_chain_id:to_numeric(<<"ae_mainnet">>)),
           ?assertEqual(424242, aerpc_chain_id:current())
       end},
      {"with no override the placeholder table applies",
       fun() ->
           application:unset_env(aerpc, chain_id),
           ?assertEqual(undefined, aerpc_chain_id:configured()),
           ?assertEqual(1247, aerpc_chain_id:to_numeric(<<"ae_mainnet">>)),
           ?assertEqual(1248, aerpc_chain_id:to_numeric(<<"ae_uat">>)),
           ?assertEqual(0,    aerpc_chain_id:to_numeric(<<"ae_unknown">>))
       end},
      {"a nonsense override is ignored rather than served",
       fun() ->
           application:set_env(aerpc, chain_id, <<"1234">>),
           ?assertEqual(undefined, aerpc_chain_id:configured()),
           application:set_env(aerpc, chain_id, -1),
           ?assertEqual(undefined, aerpc_chain_id:configured())
       end}]}.

%% ===================================================================
%% Batch cap: enforced in aerpc:dispatch/1, so both transports get it
%% ===================================================================

batch_cap_test_() ->
    {setup,
     fun() -> application:get_env(aerpc, max_batch_size) end,
     fun(Saved) -> restore_env(max_batch_size, Saved) end,
     [{"the default cap is 1024",
       fun() ->
           application:unset_env(aerpc, max_batch_size),
           ?assertEqual(1024, aerpc:max_batch_size())
       end},
      {"an operator value is honoured",
       fun() ->
           application:set_env(aerpc, max_batch_size, 3),
           ?assertEqual(3, aerpc:max_batch_size())
       end},
      {"a batch over the cap is rejected as -32006, unexecuted",
       fun() ->
           application:set_env(aerpc, max_batch_size, 2),
           Req = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                   <<"method">>  => <<"eth_accounts">>},
           ?assertMatch(#{<<"error">> := #{<<"code">> := -32006}},
                        aerpc:dispatch([Req, Req, Req])),
           %% At the cap it still runs, and returns one reply per entry.
           ?assertMatch([#{<<"result">> := []}, #{<<"result">> := []}],
                        aerpc:dispatch([Req, Req]))
       end},
      {"an out-of-range cap falls back to the default",
       fun() ->
           application:set_env(aerpc, max_batch_size, 0),
           ?assertEqual(1024, aerpc:max_batch_size()),
           application:set_env(aerpc, max_batch_size, <<"lots">>),
           ?assertEqual(1024, aerpc:max_batch_size())
       end},
      {"an empty batch is still an invalid request",
       fun() ->
           ?assertMatch(#{<<"error">> := #{<<"code">> := -32600}},
                        aerpc:dispatch([]))
       end}]}.

%% ===================================================================
%% No unconditional per-block work
%% ===================================================================

no_background_work_test_() ->
    [{"the supervisor does not start the log indexer",
      fun() ->
          {ok, {_Flags, Children}} = aerpc_sup:init([]),
          Ids = [maps:get(id, C) || C <- Children],
          ?assertNot(lists:member(aerpc_log_indexer, Ids))
      end},
     {"the log store reads as empty when nobody called init/0",
      fun() ->
          %% ets:lookup/2 on a missing table raises badarg, and
          %% indexed/1 sits directly on top of these two -- so without
          %% this tolerance eth_getLogs would crash instead of falling
          %% back to the inline walker.
          ?assertEqual(undefined, aerpc_log_store:floor_height()),
          ?assertEqual(undefined, aerpc_log_store:watermark()),
          ?assertEqual(false, aerpc_log_store:indexed({0, 10}))
      end}].

%% ===================================================================
%% The eth transaction object
%% ===================================================================

eth_tx_shape_test_() ->
    {setup,
     fun() ->
         Saved = application:get_env(aerpc, chain_id),
         application:set_env(aerpc, chain_id, ?TEST_CHAIN_ID),
         Saved
     end,
     fun(Saved) -> restore_env(chain_id, Saved) end,
     [{"a pending spend tx maps onto the eth shape",
       fun() ->
           Sender    = <<1:32/unit:8>>,
           Recipient = <<2:32/unit:8>>,
           STx = spend_tx(Sender, Recipient, 500, 7),
           Tx  = aerpc_tx:to_eth_tx(STx, null, null, null),

           %% `to' is the regression that matters here: aec_spend_tx
           %% exports recipient_id/1 and not recipient_pubkey/1, so the
           %% earlier function_exported/3 probe always missed and every
           %% spend reported to: null.
           ?assertEqual(aerpc_encoding:format_account(Recipient),
                        maps:get(<<"to">>, Tx)),
           ?assertEqual(aerpc_encoding:format_account(Sender),
                        maps:get(<<"from">>, Tx)),
           ?assertEqual(42, byte_size(maps:get(<<"to">>, Tx))),
           ?assertEqual(<<"0x1f4">>, maps:get(<<"value">>, Tx)),
           ?assertEqual(<<"0x7">>,   maps:get(<<"nonce">>, Tx)),
           ?assertEqual(<<"0x0">>,   maps:get(<<"gasPrice">>, Tx)),
           ?assertEqual(<<"0x">>,    maps:get(<<"input">>, Tx)),
           ?assertEqual(<<"0x0">>,   maps:get(<<"type">>, Tx)),
           ?assertEqual(aerpc_encoding:to_quantity(?TEST_CHAIN_ID),
                        maps:get(<<"chainId">>, Tx)),

           %% Pending: no block position.
           ?assertEqual(null, maps:get(<<"blockHash">>, Tx)),
           ?assertEqual(null, maps:get(<<"blockNumber">>, Tx)),
           ?assertEqual(null, maps:get(<<"transactionIndex">>, Tx))
       end},
      {"a mined position is emitted as hex, not null",
       fun() ->
           BlockHash = <<3:32/unit:8>>,
           STx = spend_tx(<<1:32/unit:8>>, <<2:32/unit:8>>, 1, 1),
           Tx  = aerpc_tx:to_eth_tx(STx, BlockHash, 99, 4),
           ?assertEqual(aerpc_encoding:to_hex_data(BlockHash),
                        maps:get(<<"blockHash">>, Tx)),
           ?assertEqual(<<"0x63">>, maps:get(<<"blockNumber">>, Tx)),
           ?assertEqual(<<"0x4">>,  maps:get(<<"transactionIndex">>, Tx))
       end},
      {"no AE-native encoding leaks into the tx object",
       fun() ->
           STx = spend_tx(<<1:32/unit:8>>, <<2:32/unit:8>>, 1, 1),
           Tx  = aerpc_tx:to_eth_tx(STx, <<3:32/unit:8>>, 1, 0),
           [?assert(is_hex_or_null(V)) || V <- maps:values(Tx)]
       end},
      {"the ed25519 signature is exposed as r/s halves, v as 0x0",
       fun() ->
           Sig = << <<B>> || B <- lists:seq(1, 64) >>,
           {ok, Aetx} = aec_spend_tx:new(
                          #{sender_id    => aeser_id:create(account, <<1:32/unit:8>>),
                            recipient_id => aeser_id:create(account, <<2:32/unit:8>>),
                            amount       => 1, fee => 1, nonce => 1,
                            payload      => <<>>}),
           STx = aetx_sign:new(Aetx, [Sig]),
           Tx  = aerpc_tx:to_eth_tx(STx, null, null, null),
           <<R:32/binary, S:32/binary>> = Sig,
           ?assertEqual(<<"0x0">>, maps:get(<<"v">>, Tx)),
           ?assertEqual(aerpc_encoding:to_hex_data(R), maps:get(<<"r">>, Tx)),
           ?assertEqual(aerpc_encoding:to_hex_data(S), maps:get(<<"s">>, Tx))
       end},
      {"an unsigned tx still produces well-formed r/s",
       fun() ->
           STx = spend_tx(<<1:32/unit:8>>, <<2:32/unit:8>>, 1, 1),
           Tx  = aerpc_tx:to_eth_tx(STx, null, null, null),
           Zero = aerpc_encoding:zero_word(),
           ?assertEqual(Zero, maps:get(<<"r">>, Tx)),
           ?assertEqual(Zero, maps:get(<<"s">>, Tx))
       end}]}.

%% ===================================================================
%% Helpers
%% ===================================================================

dispatch(Method) ->
    aerpc_methods:dispatch_method(Method, []).

spend_tx(Sender, Recipient, Amount, Nonce) ->
    {ok, Aetx} = aec_spend_tx:new(
                   #{sender_id    => aeser_id:create(account, Sender),
                     recipient_id => aeser_id:create(account, Recipient),
                     amount       => Amount,
                     fee          => 20000,
                     nonce        => Nonce,
                     payload      => <<>>}),
    aetx_sign:new(Aetx, []).

is_hex_or_null(null)                    -> true;
is_hex_or_null(<<"0x", _/binary>>)      -> true;
is_hex_or_null(V) when is_binary(V)     -> false;
is_hex_or_null(_Other)                  -> true.

restore_env(Key, undefined)   -> application:unset_env(aerpc, Key);
restore_env(Key, {ok, Value}) -> application:set_env(aerpc, Key, Value).

-endif.
