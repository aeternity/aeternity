%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks_tests).

-ifdef(TEST).

%% Stands in as the callback module of aetx:min_gas_probe/3 - see probe_tx/1.
-export([abi_version/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aeminer/include/aeminer.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include("blocks.hrl").

-import(aec_blocks, [raw_micro_block/0
                    ]).

-define(TEST_MODULE, aec_blocks).
-define(FAKE_TXS_TREE_HASH, <<42:?TXS_HASH_BYTES/unit:8>>).

%% gas/1 routes through aetx:tx_min_gas/2 for these and through aetx:gas_limit/3
%% below them, so this is exactly the set of protocols the tx_min_gas/2 clause
%% list governs. Ceres is what ae_mainnet runs.
-define(PROTOCOLS_ABOVE_IRIS, [?CERES_PROTOCOL_VSN, ?ARCUS_PROTOCOL_VSN,
                               ?SALUS_PROTOCOL_VSN]).
-define(PROBE_SIZE, 100).

validate_test_() ->
    {setup,
     fun() ->
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             meck:new(enacl, [passthrough]),
             meck:expect(enacl, sign_verify_detached, 3, {ok, <<>>}),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, dirty_get_header, 1, error),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             meck:unload(aec_chain),
             meck:unload(enacl)
     end,
     [ {"Malformed txs merkle tree hash",
        fun validate_test_malformed_txs_root_hash/0}
     , {"Pass validation - case no txs",
        fun validate_test_pass_validation_no_txs/0}
     , {"Pass validation - case some txs",
        fun validate_test_pass_validation/0}
     ] ++ micro_block_gas_covers_every_tx_type()
       ++ validate_micro_block_holding_a_no_base_gas_tx()}.

%% gas/1 is the only caller of aetx:tx_min_gas/2, and above Iris it calls it on
%% every transaction of a received micro block. A type aec_governance cannot
%% price has to come back as 0 rather than raise function_clause, so enumerate
%% every entry of aetx:tx_types/0 here - the next new type fails this test
%% instead of the node.
micro_block_gas_covers_every_tx_type() ->
    NoBaseGas = aetx:no_base_gas_tx_types(),
    [ {lists:concat(["gas/1 for ", Type, " at protocol ", Protocol]),
       fun() ->
               Block = micro_block_at(Protocol, [probe_tx(Type)],
                                      ?FAKE_TXS_TREE_HASH),
               Gas = ?TEST_MODULE:gas(Block),
               case lists:member(Type, NoBaseGas) of
                   true  -> ?assertEqual(0, Gas);
                   false -> ?assert(Gas > 0)
               end
       end}
      || Type <- aetx:tx_types(), Protocol <- ?PROTOCOLS_ABOVE_IRIS ].

%% The regression that matters: the whole validator pipeline, not one function.
%% aec_conductor:add_block/2 runs aec_validation:validate_block/2 in the calling
%% process - the peer connection or the sync worker - and the micro block
%% signature is not checked before this point (aec_headers:validate_micro_block_
%% header/2 says so itself). aeu_validation:run/2 is a bare apply/2 recursion
%% with no catch, so before the fix validate_gas_limit/1 took that process down
%% on remote, unsigned input. It has to return instead.
validate_micro_block_holding_a_no_base_gas_tx() ->
    [ {lists:concat(["validate_micro_block/2 with a channel_offchain_tx",
                     " at protocol ", Protocol]),
       fun() ->
               Txs     = [aetx_sign:new(offchain_tx(), [])],
               TxsHash = txs_hash(Txs),
               Block   = micro_block_at(Protocol, Txs, TxsHash),
               ?assertEqual(0, ?TEST_MODULE:gas(Block)),
               %% The validators pass, but the block is still never admitted:
               %% aesc_offchain_tx:process/3 is error(off_chain_tx) and the
               %% strict apply aborts on it. This line used to raise instead.
               ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block, Protocol))
       end}
      || Protocol <- ?PROTOCOLS_ABOVE_IRIS ].

offchain_tx() ->
    {ok, Tx} = aesc_offchain_tx:new(#{channel_id => aeser_id:create(channel, <<1:32/unit:8>>),
                                      state_hash => <<2:32/unit:8>>,
                                      round      => 1}),
    Tx.

%% aetx:min_gas_probe/3 rather than a real transaction of each type: tx_min_gas/2
%% reads only the type, the size and - for the contract types - CB:abi_version/1,
%% and channel_client_reconnect_tx has no aetx:type_to_cb/1 clause, so it is
%% reachable no other way.
probe_tx(Type) ->
    aetx_sign:new(aetx:min_gas_probe(Type, ?MODULE, ?PROBE_SIZE), []).

abi_version(undefined) -> ?ABI_FATE_SOPHIA_1.

txs_hash(Txs) ->
    aec_txs_trees:pad_empty(aec_txs_trees:root_hash(aec_txs_trees:from_txs(Txs))).

micro_block_at(Protocol, Txs, TxsHash) ->
    Header = aec_headers:set_version(aec_headers:raw_micro_header(), Protocol),
    Block  = aec_blocks:new_micro_from_header(Header, Txs, no_fraud),
    aec_blocks:set_txs_hash(Block, TxsHash).

validate_test_malformed_txs_root_hash() ->
    SignedSpend =
        aec_test_utils:signed_spend_tx(
          #{recipient_id => aeser_id:create(account, <<1:32/unit:8>>),
            amount => 1,
            fee => 1,
            nonce => 1,
            payload => <<>>}),

    {ok, Spend} = aec_spend_tx:new(#{sender_id => aeser_id:create(account, <<42:32/unit:8>>),
                                     recipient_id => aeser_id:create(account, <<4242:32/unit:8>>),
                                     amount => 1,
                                     fee => 1,
                                     nonce => 1,
                                     payload => <<>>}),
    BadSignedSpend = aec_test_utils:sign_tx(Spend, <<0:64/unit:8>>),

    MalformedTxs = [SignedSpend, BadSignedSpend],
    MalformedTree = aec_txs_trees:from_txs(MalformedTxs),
    {ok, MalformedTxsRootHash} = aec_txs_trees:root_hash(MalformedTree),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, MalformedTxsRootHash,
              aec_blocks:root_hash(RawBlock),
              [SignedSpend]),
    ?assertEqual({error, {block, malformed_txs_hash}},
                 ?TEST_MODULE:validate_micro_block(Block, aec_blocks:version(Block))).

validate_test_pass_validation_no_txs() ->
    Txs = [],
    Tree = aec_txs_trees:from_txs(Txs),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(Tree)),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, TxsRootHash,
              aec_blocks:root_hash(RawBlock),
              []),
    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block, aec_blocks:version(Block))).

validate_test_pass_validation() ->
    SignedSpend =
        aec_test_utils:signed_spend_tx(
          #{recipient_id => aeser_id:create(account, <<1:32/unit:8>>),
            amount => 1,
            fee => 20000,
            nonce => 1,
            payload => <<>>}),
    Txs = [SignedSpend],
    Tree = aec_txs_trees:from_txs(Txs),
    {ok, TxsRootHash} = aec_txs_trees:root_hash(Tree),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, TxsRootHash,
              aec_blocks:root_hash(RawBlock),
              Txs),

    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block, aec_blocks:version(Block))).

-endif.
