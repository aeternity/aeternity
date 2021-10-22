-module(aehttp_mempool_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% use include_lib for aecontract to compile under system test
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

%% tests exports
-export([
          mine_2_txs/1
        , mine_5_txs_from_the_same_account/1
        , txs_without_balance_are_not_mined/1
        , block_pack/1
        , gc_txs/1
        , delete_tx_from_mempool/1
        ]).

-define(NODE, dev1).
-define(NODENAME, aecore_suite_utils:node_name(?NODE)).
-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(MAX_MINED_BLOCKS, 5).
-define(TX_TTL, 11).
-define(MINE_BLOCKS(N), aecore_suite_utils:mine_key_blocks(?NODENAME, N)).
-define(MINE_TXS(Txs, MaxKeyBlocks), aecore_suite_utils:mine_blocks_until_txs_on_chain(?NODENAME, Txs, MaxKeyBlocks)).
-define(MINE_TXS(Txs), ?MINE_TXS(Txs, ?MAX_MINED_BLOCKS)).

all() ->
    [{group, oas3}
    ].

groups() ->
    [{oas3, [sequence],
      [{group, spend_txs_in_generation}]},
     {spend_txs_in_generation, [sequence],
      [ mine_2_txs
      , mine_5_txs_from_the_same_account
      , txs_without_balance_are_not_mined
      , block_pack
      , gc_txs
      , delete_tx_from_mempool
      ]}
    ].

suite() ->
    [].

init_per_suite(Config0) ->
    Forks = aecore_suite_utils:forks(),
    %% We want to run some suites with and some suites without client side cache
    %% Arbitrary run this suite with cache disabled.
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => false,
                     <<"hard_forks">> => Forks},
               <<"mempool">> =>
                   #{<<"tx_ttl">> => ?TX_TTL},
              <<"http">> =>
                  #{<<"endpoints">> => #{<<"node-operator">> => true},
                    <<"cache">> => #{<<"enabled">> => false}}},
    Config1 = [{instant_mining, true}, {symlink_name, "latest.http_mempool"}, {test_module, ?MODULE}] ++ Config0,
    Config2 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, Config1),
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config2),
    aecore_suite_utils:connect(NodeName, []),
    [{node_name, NodeName}, {nodes, [aecore_suite_utils:node_tuple(?NODE)]}] ++ Config2.

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    ok.

init_per_group(SwaggerVsn, Config0) when SwaggerVsn =:= oas3 ->
    Config = [{swagger_version, SwaggerVsn} | Config0],
    VM =
        case aect_test_utils:latest_protocol_version() of
            PreIris when PreIris < ?IRIS_PROTOCOL_VSN -> aevm;
            _PostIris -> fate
        end,
    aect_test_utils:init_per_group(VM, Config);
init_per_group(_GAGroup, Config) ->
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(0, aecore_suite_utils:latest_fork_height()),
    ct:pal("ToMine ~p\n", [ToMine]),
    [ ?MINE_BLOCKS(ToMine) || ToMine > 0 ],

    %% Prepare accounts
    StartAmt = 1000 * 1000 * 1000000 * ?DEFAULT_GAS_PRICE,
    {APK, ASK, STx1} = new_account(StartAmt),
    {BPK, BSK, STx2} = new_account(StartAmt),
    {CPK, CSK, STx3} = new_account(1),
    {DPK, DSK, STx4} = new_account(1),

    {ok, _} = ?MINE_TXS([STx1, STx2, STx3, STx4]),

    %% Save account information
    Accounts = #{acc_a => #{pub_key => APK, priv_key => ASK},
                 acc_b => #{pub_key => BPK, priv_key => BSK},
                 acc_empty  => #{pub_key => CPK, priv_key => CSK},
                 acc_empty2 => #{pub_key => DPK, priv_key => DSK}
                },
    [{accounts, Accounts} | Config].

end_per_group(_VMGroup, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    SwaggerVsn = proplists:get_value(swagger_version, Config),
    aecore_suite_utils:use_swagger(SwaggerVsn),
    put('$vm_version',     ?config(vm_version,     Config)),
    put('$abi_version',    ?config(abi_version,    Config)),
    put('$sophia_version', ?config(sophia_version, Config)),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_, N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

mine_2_txs(Config) ->
    %% precondition - no txs in the pool
    [] = pending_txs(),
    #{acc_a := #{pub_key := Alice, priv_key := AlicePrivkey},
      acc_b := #{pub_key := Bob, priv_key := BobPrivkey} } =
        ?config(accounts, Config),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    SpendTx1 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    SpendTx2 = sign_tx(spend_tx(Bob, Bob, 1, Fee), BobPrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash2}} = post_tx(SpendTx2),
    PendingTxs = pending_txs(),
    %% assert 2 txs in pool
    2 = length(PendingTxs),
    {ok, _} = ?MINE_TXS([SpendTxHash1, SpendTxHash2]),
    %% assert pool is empty
    [] = pending_txs(),
    ok.

mine_5_txs_from_the_same_account(Config) ->
    %% precondition - no txs in the pool
    [] = pending_txs(),
    #{acc_a := #{pub_key := Alice, priv_key := AlicePrivkey}} =
        ?config(accounts, Config),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    SpendTx1 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    SpendTx2 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash2}} = post_tx(SpendTx2),
    SpendTx3 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash3}} = post_tx(SpendTx3),
    SpendTx4 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash4}} = post_tx(SpendTx4),
    SpendTx5 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash5}} = post_tx(SpendTx5),
    PendingTxs = pending_txs(),
    %% assert 5 txs in pool
    5 = length(PendingTxs),
    %% the nonce offset for an account is 5
    5 = rpc:call(?NODENAME, aec_tx_pool, nonce_offset, []),
    %% posting a new tx with a higher nonce will fail:
    SpendTx6 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    {ok, 400, #{<<"reason">> := <<"Invalid tx">>}} = post_tx(SpendTx6),
    {ok, _} = ?MINE_TXS([SpendTxHash1, SpendTxHash2, SpendTxHash3,
                         SpendTxHash4, SpendTxHash5]),
    %% assert pool is empty
    [] = pending_txs(),
    %% now the tx makes the node to accept it:
    {ok, 200, #{<<"tx_hash">> := SpendTxHash6}} = post_tx(SpendTx6),
    [_] = pending_txs(),
    {ok, _} = ?MINE_TXS([SpendTxHash6]),
    [] = pending_txs(),
    ok.

txs_without_balance_are_not_mined(Config) ->
    %% precondition - no txs in the pool
    [] = pending_txs(),
    #{acc_a := #{pub_key := Alice, priv_key := AlicePrivkey},
      acc_empty := #{pub_key := EmptyAccPubkey, priv_key := EmptyAccPrivkey} } =
        ?config(accounts, Config),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    SpendTx1 = sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    SpendTx2 = sign_tx(spend_tx(EmptyAccPubkey, EmptyAccPubkey, 1, Fee),
                       EmptyAccPrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash2}} = post_tx(SpendTx2),
    {ok, _} = ?MINE_TXS([SpendTxHash1]),
    %% we can not mine this tx
    {error, max_reached} = ?MINE_TXS([SpendTxHash2]),
    SpendTx3 = sign_tx(spend_tx(Alice, EmptyAccPubkey, Fee + 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash3}} = post_tx(SpendTx3),
    {ok, _} = ?MINE_TXS([SpendTxHash2, SpendTxHash3]),
    [] = pending_txs(),
    ok.

block_pack(Config) ->
    [] = pending_txs(),
    #{acc_a := #{pub_key := Alice, priv_key := AlicePrivkey},
      acc_empty := #{pub_key := Empty, priv_key := EmptyPrivkey},
      acc_empty2 := #{pub_key := Empty2, priv_key := Empty2Privkey} } =
        ?config(accounts, Config),
    Fee = fun(X) -> X * ?DEFAULT_GAS_PRICE end,

    prepare_fixed_amount_account(Empty, EmptyPrivkey, Alice, AlicePrivkey, Fee(50000)),
    prepare_fixed_amount_account(Empty2, Empty2Privkey, Alice, AlicePrivkey, Fee(50000)),

    %% The trick is to post two transactions with the same nonce and then make sure that the
    %% more valuable one (higher fee makes the miner prioritize it) non-valid, but only
    %% non-valid after the previous Tx has been processed
    Nonce = next_nonce(Empty),
    SpendTx1 = sign_tx(spend_tx(Empty, Alice, Fee(200), Fee(20000), <<>>,  Nonce), EmptyPrivkey),
    %% After SpendTx1 we have only 29.8k left... But both should be accepted by the tx-pool!
    SpendTx2a = sign_tx(spend_tx(Empty, Alice, Fee(200), Fee(30000), <<>>,  Nonce + 1), EmptyPrivkey),
    SpendTx2b = sign_tx(spend_tx(Empty, Alice, Fee(200), Fee(20000), <<>>,  Nonce + 1), EmptyPrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    {ok, 200, #{<<"tx_hash">> := _SpendTxHash2a}} = post_tx(SpendTx2a),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash2b}} = post_tx(SpendTx2b),

    Nonce2 = next_nonce(Empty2),
    SpendTx3 = sign_tx(spend_tx(Empty2, Alice, Fee(200), Fee(20000), <<>>,  Nonce2), Empty2Privkey),
    %% After SpendTx3 we have only 29.8k left... But both should be accepted by the tx-pool!
    SpendTx4a = sign_tx(spend_tx(Empty2, Alice, Fee(200), Fee(20000), <<>>,  Nonce2 + 1), Empty2Privkey),
    SpendTx4b = sign_tx(spend_tx(Empty2, Alice, Fee(200), Fee(30000), <<>>,  Nonce2 + 1), Empty2Privkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash3}} = post_tx(SpendTx3),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash4a}} = post_tx(SpendTx4a),
    {ok, 200, #{<<"tx_hash">> := _SpendTxHash4b}} = post_tx(SpendTx4b),

    {ok, _} = ?MINE_TXS([SpendTxHash1, SpendTxHash2b, SpendTxHash3, SpendTxHash4a]),
    BlockHash1 = block_for_tx(SpendTxHash1),
    BlockHash2 = block_for_tx(SpendTxHash2b),
    BlockHash3 = block_for_tx(SpendTxHash3),
    BlockHash4 = block_for_tx(SpendTxHash4a),
    BlockHash1 = BlockHash2,
    BlockHash2 = BlockHash3,
    BlockHash3 = BlockHash4,
    ?MINE_BLOCKS(?TX_TTL - 1),
    ok.

prepare_fixed_amount_account(Pubkey, Privkey, PatreonPub, PatreonPriv, Amount) ->
    Fee = fun(X) -> X * ?DEFAULT_GAS_PRICE end,
    CurrentBalance = account_balance(Pubkey),

    Txs =
        case CurrentBalance - (Amount + Fee(20000)) of
            Diff when Diff > 0 ->
                SpendTx = sign_tx(spend_tx(Pubkey, PatreonPub, Diff, Fee(20000), <<>>, next_nonce(Pubkey)), Privkey),
                [SpendTx];
            _ ->
                SpendTx1 = sign_tx(spend_tx(PatreonPub, Pubkey, Amount + Fee(20000) + 1, Fee(20000), <<>>, next_nonce(PatreonPub)), PatreonPriv),
                SpendTx2 = sign_tx(spend_tx(Pubkey, PatreonPub, CurrentBalance + 1, Fee(20000), <<>>, next_nonce(Pubkey)), Privkey),
                [SpendTx1, SpendTx2]
        end,
    mine_txs(Txs).

mine_txs(Txs) -> mine_txs(Txs, []).

mine_txs([], TxHashes) ->
    ?MINE_TXS(TxHashes);
mine_txs([Tx | Txs], TxHashes) ->
    {ok, 200, #{<<"tx_hash">> := TxHash}} = post_tx(Tx),
    mine_txs(Txs, [TxHash | TxHashes]).

gc_txs(Config) ->
    %% precondition - no txs in the pool
    [] = pending_txs(),
    #{acc_a := #{pub_key := AccPubkey, priv_key := AccPrivkey} } =
        ?config(accounts, Config),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    ?TX_TTL = rpc:call(?NODENAME, aec_tx_pool, tx_ttl, []),
    %% use a bigger nonce so the txs are not included
    Nonce = next_nonce(AccPubkey),
    SpendTx2 = sign_tx(spend_tx(AccPubkey, AccPubkey, 2, Fee, <<"blocked tx">>,
                                Nonce + 1),
                       AccPrivkey),
    PendingSpendTx2 = for_client_pending(SpendTx2),
    {ok, 200, #{<<"tx_hash">> := _SpendTxHash2}} = post_tx(SpendTx2),
    %% assert a tx in pool
    [PendingSpendTx2] = pending_txs(),

    ?MINE_BLOCKS(?TX_TTL - 1),
    %% assert still a tx in pool
    [PendingSpendTx2] = pending_txs(),

    ?MINE_BLOCKS(1),
    timer:sleep(1000),
    %% assert pool is empty
    [] = pending_txs(),
    ok.

delete_tx_from_mempool(Config) ->
    %% precondition - no txs in the pool
    [] = pending_txs(),
    #{acc_a := #{pub_key := Alice, priv_key := AlicePrivkey},
      acc_b := #{pub_key := Bob, priv_key := BobPrivkey} } =
        ?config(accounts, Config),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    Nonce = next_nonce(Alice),
    SpendTx1 = sign_tx(spend_tx(Alice, Alice, 1, Fee, <<"to be deleted from pool">>,
                                Nonce + 1), AlicePrivkey),
    %% CHECK PURPOSE PendingSpendTx1 = for_client_pending(SpendTx1),
    SpendTx2 = sign_tx(spend_tx(Bob, Bob, 1, Fee), BobPrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash2}} = post_tx(SpendTx2),
    PendingTxs = pending_txs(),
    %% assert 2 txs in pool
    2 = length(PendingTxs),
    {ok, 200, #{<<"status">> := <<"deleted">>}} =
        aehttp_integration_SUITE:delete_tx_from_mempool_sut(SpendTxHash1),
    [_] = pending_txs(),
    {ok, _} = ?MINE_TXS([SpendTxHash2]),
    [] = pending_txs(),
    %% we will repost and GC the tx. In case there is a hanging reference to
    %% it in the GC, it will be GCed sooner than expected. In order to do so,
    %% we mine a few blocks
    ?MINE_BLOCKS(3),
    %% assert pool is empty
    [] = pending_txs(),
    %% tx can no longer be posted
    {ok, 400, #{<<"reason">> := <<"Invalid tx">>}} = post_tx(SpendTx1),
    [] = pending_txs(),
    ok.


%% ============================================================
%% private functions
%% ============================================================
new_account(Amount) ->
    {Pubkey, Privkey} = aecore_suite_utils:generate_key_pair(),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    SpendTx = spend_tx(patron_pubkey(), Pubkey, Amount, Fee),
    SignedSpendTx = sign_tx(SpendTx, patron_privkey()),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_tx(SignedSpendTx),
    {Pubkey, Privkey, SpendTxHash}.

next_nonce(Pubkey) ->
    ID = aeser_api_encoder:encode(account_pubkey, Pubkey),
    %% fetch what would be the next nonce
    {ok, 200, #{<<"next_nonce">> := Nonce}} = aehttp_integration_SUITE:get_accounts_next_nonce_sut(ID),
    Nonce.

spend_tx(FromPubkey, ToPubkey, Amount, Fee) ->
    spend_tx(FromPubkey, ToPubkey, Amount, Fee, <<>>).

spend_tx(FromPubkey, ToPubkey, Amount, Fee, Payload) ->
    Nonce = next_nonce(FromPubkey),
    spend_tx(FromPubkey, ToPubkey, Amount, Fee, Payload, Nonce).

spend_tx(FromPubkey, ToPubkey, Amount, Fee, Payload, Nonce) ->
    From = aeser_api_encoder:encode(account_pubkey, FromPubkey),
    To = aeser_api_encoder:encode(account_pubkey, ToPubkey),
    {ok, 200, #{<<"tx">> := EncodedSpendTx}} =
        aehttp_integration_SUITE:get_spend(
            #{sender_id    => From,
              nonce        => Nonce,
              recipient_id => To,
              amount       => Amount,
              fee          => Fee,
              payload      => Payload}),
    {ok, TxSer} = aeser_api_encoder:safe_decode(transaction, EncodedSpendTx),
    AeTx = aetx:deserialize_from_binary(TxSer),
    AeTx.

sign_tx(Tx, Privkey) ->
    STx = aec_test_utils:sign_tx(Tx, [Privkey]),
    aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(STx)).

patron_pubkey() ->
    maps:get(pubkey, aecore_suite_utils:patron()).

patron_privkey() ->
    maps:get(privkey, aecore_suite_utils:patron()).

post_tx(Tx) ->
    aehttp_integration_SUITE:post_transactions_sut(Tx).

pending_txs() ->
    {ok, 200, #{<<"transactions">> := Txs}} =
        aehttp_integration_SUITE:get_transactions_pending_sut(),
    Txs.

for_client_pending(<<"tx_", _/binary>> = EncodedTx) ->
    {ok, TxSer} = aeser_api_encoder:safe_decode(transaction, EncodedTx),
    SignedTx = aetx_sign:deserialize_from_binary(TxSer),
    for_client_pending(SignedTx);
for_client_pending(SignedTx) ->
    aetx_sign:serialize_for_client_pending(SignedTx).

block_for_tx(EncTxHash) ->
    {ok, TxHash} = aeser_api_encoder:safe_decode(tx_hash, EncTxHash),
    {Block, _} = rpc:call(?NODENAME, aec_chain, find_tx_with_location, [TxHash]),
    Block.

account_balance(PubKey) ->
    {value, Account} = rpc:call(?NODENAME, aec_chain, get_account, [PubKey]),
    aec_accounts:balance(Account).
