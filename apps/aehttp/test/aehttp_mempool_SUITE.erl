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
            PostIris -> fate
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

    {ok, _} = ?MINE_TXS([STx1, STx2, STx3]),

    %% Save account information
    Accounts = #{acc_a => #{pub_key => APK, priv_key => ASK},
                 acc_b => #{pub_key => BPK, priv_key => BSK},
                 acc_empty => #{pub_key => CPK, priv_key => CSK}
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
    SpendTx6= sign_tx(spend_tx(Alice, Alice, 1, Fee), AlicePrivkey),
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
    {error,max_reached} = ?MINE_TXS([SpendTxHash2]),
    SpendTx3 = sign_tx(spend_tx(Alice, EmptyAccPubkey, Fee + 1, Fee), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash3}} = post_tx(SpendTx3),
    {ok, _} = ?MINE_TXS([SpendTxHash2, SpendTxHash3]),
    [] = pending_txs(),
    ok.

gc_txs(Config) ->
    %% precondition - no txs in the pool
    [] = pending_txs(),
    #{acc_a := #{pub_key := AccPubkey, priv_key := AccPrivkey} } =
        ?config(accounts, Config),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    ?TX_TTL = rpc:call(?NODENAME, aec_tx_pool, tx_ttl, []),
    %% use a bigger nonce so the txs are not include
    Nonce = next_nonce(AccPubkey),
    SpendTx2 = sign_tx(spend_tx(AccPubkey, AccPubkey, 2, Fee, <<"blocked tx">>,
                                Nonce + 1),
                       AccPrivkey),
    PendingSpendTx2 = for_client_pending(SpendTx2),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash2}} = post_tx(SpendTx2),
    %% assert a tx in pool
    [PendingSpendTx2]  = pending_txs(),
    
    ?MINE_BLOCKS(?TX_TTL - 1),
    %% assert still a tx in pool
    [PendingSpendTx2]  = pending_txs(),

    ?MINE_BLOCKS(1),
    timer:sleep(1000),
    %% assert pool is empty
    [] = pending_txs(),
    {ok, 200, #{<<"tx_hash">> := _SpendTxHash2}} = post_tx(SpendTx2),
    [PendingSpendTx2]  = pending_txs(),
    %% add missing tx with nonce
    SpendTx1 = sign_tx(spend_tx(AccPubkey, AccPubkey, 2, Fee, <<"unblocking tx">>,
                                Nonce),
                       AccPrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    PendingTxs = pending_txs(),
    %% assert 2 txs in pool
    2 = length(PendingTxs),
    {ok, _} = ?MINE_TXS([SpendTxHash1, SpendTxHash2]),
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
    PendingSpendTx1 = for_client_pending(SpendTx1),
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
    %% tx can still be posted
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    %% tx can be GCed
    [PendingSpendTx1]  = pending_txs(),
    ?MINE_BLOCKS(?TX_TTL - 1),
    %% assert still in the pool
    [PendingSpendTx1]  = pending_txs(),
    ?MINE_BLOCKS(1),
    timer:sleep(1000),
    %% assert the tx has beed GCed
    [] = pending_txs(),
    %% post the tx after it had been GCed
    {ok, 200, #{<<"tx_hash">> := SpendTxHash1}} = post_tx(SpendTx1),
    [PendingSpendTx1]  = pending_txs(),
    %% post the ublocking tx with the correct nonce
    SpendTx3 = sign_tx(spend_tx(Alice, Alice, 1, Fee, <<"">>,
                                Nonce), AlicePrivkey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash3}} = post_tx(SpendTx3),
    %% those can be included
    {ok, _} = ?MINE_TXS([SpendTxHash1, SpendTxHash3]),
    [] = pending_txs(),
    {ok, 404, #{<<"reason">> := <<"included">>}} =
        aehttp_integration_SUITE:delete_tx_from_mempool_sut(SpendTxHash1),
    MissingTxHash = aeser_api_encoder:encode(tx_hash, random_hash()),
    {ok, 404, #{<<"reason">> := <<"not_found">>}} =
        aehttp_integration_SUITE:delete_tx_from_mempool_sut(MissingTxHash),
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
            #{sender_id => From,
              nonce => Nonce,
              recipient_id => To, 
              amount => Amount,
              fee => Fee,
              payload => Payload}),
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

random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).
