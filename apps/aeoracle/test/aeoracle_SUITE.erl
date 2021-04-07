%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc CT test suite for AE Oracles
%%% @end
%%%-------------------------------------------------------------------
-module(aeoracle_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% test case exports
-export([ extend_oracle/1
        , extend_oracle_negative/1
        , extend_oracle_negative_dynamic_fee/1
        , prune_oracle/1
        , prune_oracle_extend/1
        , prune_query/1
        , prune_response_short/1
        , prune_response_long/1
        , query_fee/1
        , query_oracle/1
        , query_oracle_via_name/1
        , query_oracle_negative/1
        , query_oracle_negative_dynamic_fee/1
        , query_oracle_type_check/1
        , query_response/1
        , query_response_fee_depends_on_response_size/1
        , query_response_negative/1
        , query_response_negative_dynamic_fee/1
        , query_response_type_check/1
        , register_oracle/1
        , register_oracle_negative/1
        , register_oracle_negative_dynamic_fee/1
        , register_oracle_negative_absolute_ttl/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../../aecore/include/blocks.hrl").
-include("../include/oracle_txs.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/include/hard_forks.hrl").

-define(GENESIS_HEIGHT, aec_block_genesis:height()).

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}
    ].

groups() ->
    [ {all_tests, [sequence], [ {group, transactions}
                              , {group, state_tree}
                              ]}
    , {transactions, [sequence], [ register_oracle
                                 , register_oracle_negative
                                 , register_oracle_negative_dynamic_fee
                                 , register_oracle_negative_absolute_ttl
                                 , extend_oracle
                                 , extend_oracle_negative
                                 , extend_oracle_negative_dynamic_fee
                                 , query_oracle
                                 , query_oracle_via_name
                                 , query_oracle_negative
                                 , query_oracle_negative_dynamic_fee
                                 , query_oracle_type_check
                                 , query_response
                                 , query_response_fee_depends_on_response_size
                                 , query_response_negative
                                 , query_response_negative_dynamic_fee
                                 , query_response_type_check
                                 , query_fee
                                 ]}
    , {state_tree, [ prune_oracle
                   , prune_oracle_extend
                   , prune_query
                   , prune_response_short
                   , prune_response_long
                   ]}
    ].

init_per_suite(Config) ->
    aec_test_utils:ensure_no_mocks(),
    Config.

end_per_suite(_Config) ->
    aec_test_utils:ensure_no_mocks().

init_per_testcase(query_oracle_via_name, Config) ->
    case aect_test_utils:latest_protocol_version() >= ?IRIS_PROTOCOL_VSN of
        true -> Config;
        _ -> {skip, requires_lima}
    end;
init_per_testcase(register_oracle_negative, Config) ->
    meck_new([aec_governance], Config);
init_per_testcase(Test, Config) when Test =:= query_response_negative_dynamic_fee
                              orelse Test =:= query_oracle_negative_dynamic_fee
                              orelse Test =:= extend_oracle_negative_dynamic_fee
                              orelse Test =:= register_oracle_negative_dynamic_fee ->
    Config1 = meck_new([aec_governance, aec_tx_pool], Config),
    % those tests are sensitive to the minimum fee required for including a tx.
    % The correct fee is a function of various components, one of which is the
    % gas price. Modifying the minimum gas price can change the size of the
    % serialized transaction and thus - breaking the expectations for the fee,
    % thus the mecks
    meck:expect(aec_governance, minimum_gas_price, fun(_) -> 1000000 end),
    meck:expect(aec_tx_pool, minimum_miner_gas_price, fun() -> 1 end),
    Config1;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    [meck:unload(M) || M <- meck_modules(Config)],
    ok.

meck_new(Ms, Config) ->
    Prev = meck_modules(Config),
    [meck:new(M, [passthrough]) || M <- Ms],
    lists:keystore(meck_modules, 1, Config, {meck_modules, Prev ++ Ms}).

meck_modules(Config) ->
    proplists:get_value(meck_modules, Config, []).

%%%===================================================================
%%% Register oracle
%%%===================================================================
-define(ORACLE_REG_HEIGHT, 1).

register_oracle_negative(_Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Trees        = aeo_test_utils:trees(S1),
    CurrHeight   = ?ORACLE_REG_HEIGHT,

    %% Test registering a bogus account
    BadPubKey = <<42:32/unit:8>>,
    RTx1      = aeo_test_utils:register_tx(BadPubKey, S1),
    Env       = aetx_env:tx_env(CurrHeight),
    {error, account_not_found} = aetx:process(RTx1, Trees, Env),

    %% Insufficient funds
    S2     = aeo_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aeo_test_utils:trees(S2),
    RTx2 = aeo_test_utils:register_tx(PubKey, S1),
    {error, insufficient_funds} = aetx:process(RTx2, Trees2, Env),

    %% Test too high account nonce
    RTx3 = aeo_test_utils:register_tx(PubKey, #{nonce => 0}, S1),
    {error, tx_nonce_already_used_for_account} = aetx:process(RTx3, Trees, Env),

    %% Test too low fee
    RTx4 = aeo_test_utils:register_tx(PubKey, #{fee => 0}, S1),
    {error, too_low_fee} = aetx:process(RTx4, Trees, Env),

    %% Test too low TTL
    RTx5 = aeo_test_utils:register_tx(PubKey, #{ttl => 1}, S1),
    {error, ttl_expired} = aetx:process(RTx5, Trees, aetx_env:set_height(Env, 2)),

    %% Test wrong VM version
    RTx6 = aeo_test_utils:register_tx(PubKey, #{abi_version => 42}, S1),
    RTx7 = aeo_test_utils:register_tx(PubKey, #{abi_version => ?ABI_SOLIDITY_1}, S1),
    {error, bad_abi_version} = aetx:process(RTx6, Trees, aetx_env:set_height(Env, 2)),
    {error, bad_abi_version} = aetx:process(RTx7, Trees, aetx_env:set_height(Env, 2)),

    %% Test bad format strings
    ABISophia = #{abi_version => ?ABI_AEVM_SOPHIA_1,
                  query_format => aeb_heap:to_binary(word),
                  response_format => aeb_heap:to_binary(word)
                 },
    RTx8 = aeo_test_utils:register_tx(PubKey, ABISophia#{query_format => <<"foo">>}, S1),
    RTx9 = aeo_test_utils:register_tx(PubKey, ABISophia#{response_format => <<"foo">>}, S1),
    RTx10 = aeo_test_utils:register_tx(PubKey, ABISophia, S1),
    Env1 = aetx_env:set_consensus_version(Env, ?ROMA_PROTOCOL_VSN),
    ?assertMatch({ok, _, _}, aetx:process(RTx8, Trees, Env1)),
    ?assertMatch({ok, _, _}, aetx:process(RTx9, Trees, Env1)),
    % set same minimum gas price as in Roma
    meck:expect(aec_governance, minimum_gas_price, fun(_) -> 1 end),

    %% Test Minerva release
    Env2 = aetx_env:set_consensus_version(Env, ?MINERVA_PROTOCOL_VSN),
    ?assertEqual({error, bad_query_format}, aetx:process(RTx8, Trees, Env2)),
    ?assertEqual({error, bad_response_format}, aetx:process(RTx9, Trees, Env2)),
    ?assertMatch({ok, _, _}, aetx:process(RTx10, Trees, Env2)),

    %% Test Fortuna release
    Env3 = aetx_env:set_consensus_version(Env, ?FORTUNA_PROTOCOL_VSN),
    ?assertEqual({error, bad_query_format}, aetx:process(RTx8, Trees, Env3)),
    ?assertEqual({error, bad_response_format}, aetx:process(RTx9, Trees, Env3)),
    ?assertMatch({ok, _, _}, aetx:process(RTx10, Trees, Env3)),
    ok.

register_oracle_negative_dynamic_fee(_Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Trees        = aeo_test_utils:trees(S1),
    CurrHeight   = ?ORACLE_REG_HEIGHT,
    Env          = aetx_env:tx_env(CurrHeight),

    F = fun(RegTxOpts) ->
            Tx = aeo_test_utils:register_tx(PubKey, RegTxOpts, S1),
            aetx:process(Tx, Trees, Env)
        end,

    %% Test minimum fee for increasing TTL.
    ?assertEqual({error, too_low_fee}, F(#{oracle_ttl => {delta, 0}, fee => 0})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 0},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertEqual({error, too_low_fee}, F(#{oracle_ttl => {delta, 1}, fee => 1})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 1},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({error, too_low_fee}, F(#{oracle_ttl => {delta, 1000},
                                           fee => 16500 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 1000},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({error, too_low_fee}, F(#{oracle_ttl => {delta, 4000},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 4000},
                                           fee => 17500 * aec_test_utils:min_gas_price()})),
    ?assertEqual({error, too_low_fee}, F(#{oracle_ttl => {delta, 9000},
                                           fee => 17500 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 9000},
                                           fee => 19000 * aec_test_utils:min_gas_price()})),
    %% Test more than minimum fee considering TTL.
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 9000},
                                           fee => 40000 * aec_test_utils:min_gas_price()})),
    ok.

register_oracle_negative_absolute_ttl(_Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Trees        = aeo_test_utils:trees(S1),
    CurrHeight   = ?ORACLE_REG_HEIGHT,
    Env          = aetx_env:tx_env(CurrHeight),

    F = fun(RegTxOpts) ->
            Tx = aeo_test_utils:register_tx(PubKey, RegTxOpts, S1),
            aetx:process(Tx, Trees, Env)
        end,

    ?assertEqual({error, too_low_abs_ttl}, F(#{oracle_ttl => {block, 0},
                                               fee => 20000 * aec_test_utils:min_gas_price()})),
    ?assertEqual({error, too_low_abs_ttl}, F(#{oracle_ttl => {block, 1},
                                               fee => 20000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}              , F(#{oracle_ttl => {block, 100},
                                               fee => 20000 * aec_test_utils:min_gas_price()})),
    ok.

register_oracle(Cfg) ->
    register_oracle(Cfg, #{}).

register_oracle(_Cfg, RegTxOpts) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Tx           = aeo_test_utils:register_tx(PubKey, RegTxOpts, S1),
    PrivKey      = aeo_test_utils:priv_key(PubKey, S1),

    %% Test that RegisterTX is accepted
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Trees    = aeo_test_utils:trees(S1),
    Height   = ?ORACLE_REG_HEIGHT,
    Env      = aetx_env:tx_env(Height),
    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S2       = aeo_test_utils:set_trees(Trees1, S1),
    {PubKey, S2}.

%%%===================================================================
%%% Extend oracle
%%%===================================================================
-define(ORACLE_EXT_HEIGHT, 3).
extend_oracle_negative(Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Trees        = aeo_test_utils:trees(S1),
    CurrHeight   = ?ORACLE_REG_HEIGHT,

    %% Test registering a bogus account
    BadPubKey = <<42:32/unit:8>>,
    RTx1      = aeo_test_utils:extend_tx(BadPubKey, S1),
    Env       = aetx_env:tx_env(CurrHeight),
    {error, account_not_found} = aetx:process(RTx1, Trees, Env),

    %% Test extending non-existent oracle
    RTx2 = aeo_test_utils:extend_tx(PubKey, S1),
    {error, account_is_not_an_active_oracle} = aetx:process(RTx2, Trees, Env),

    %% Register the oracle
    {OracleKey, S2} = register_oracle(Cfg),
    Trees2          = aeo_test_utils:trees(S2),
    CurrHeight2     = ?ORACLE_EXT_HEIGHT,
    Env2            = aetx_env:tx_env(CurrHeight2),

    %% Insufficient funds
    S3     = aeo_test_utils:set_account_balance(OracleKey, 0, S2),
    Trees3 = aeo_test_utils:trees(S3),
    RTx3 = aeo_test_utils:extend_tx(OracleKey, S3),
    {error, insufficient_funds} = aetx:process(RTx3, Trees3, Env2),

    %% Test too high account nonce
    RTx4 = aeo_test_utils:extend_tx(OracleKey, #{nonce => 0}, S2),
    {error, tx_nonce_already_used_for_account} = aetx:process(RTx4, Trees2, Env2),

    %% Test too low fee
    RTx5 = aeo_test_utils:extend_tx(OracleKey, #{fee => 0}, S2),
    {error, too_low_fee} = aetx:process(RTx5, Trees2, Env),

    %% Test too low TTL
    RTx6 = aeo_test_utils:extend_tx(OracleKey, #{ttl => CurrHeight2 - 1}, S2),
    {error, ttl_expired} = aetx:process(RTx6, Trees2, Env2),
    ok.

extend_oracle_negative_dynamic_fee(Cfg) ->
    {OracleKey, S2} = register_oracle(Cfg),
    Trees2          = aeo_test_utils:trees(S2),
    CurrHeight2     = ?ORACLE_EXT_HEIGHT,
    Env             = aetx_env:tx_env(CurrHeight2),

    F = fun(ExtTxOpts) ->
            Tx = aeo_test_utils:extend_tx(OracleKey, ExtTxOpts, S2),
            aetx:process(Tx, Trees2, Env)
        end,
    %% Test minimum fee for increasing TTL.
    ?assertEqual({error, too_low_fee}, F(#{oracle_ttl => {delta, 0}, fee => 0})),
    ?assertEqual({error, zero_relative_oracle_extension_ttl}, F(#{oracle_ttl => {delta, 0},
                                                                  fee => 16000 * aec_test_utils:min_gas_price()})),
    ?assertEqual({error, too_low_fee}, F(#{oracle_ttl => {delta, 1},
                                           fee => aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 1},
                                           fee => 16000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({error, too_low_fee}, F(#{oracle_ttl => {delta, 1000},
                                           fee => 16000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 1000},
                                           fee => 16500 * aec_test_utils:min_gas_price()})),
    ?assertMatch({error, too_low_fee}, F(#{oracle_ttl => {delta, 4000},
                                           fee => 16500 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 4000},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertEqual({error, too_low_fee}, F(#{oracle_ttl => {delta, 9000},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 9000},
                                           fee => 20000 * aec_test_utils:min_gas_price()})),
    %% Test more than minimum fee considering TTL.
    ?assertMatch({ok, _, _}          , F(#{oracle_ttl => {delta, 9000},
                                           fee => 40000 * aec_test_utils:min_gas_price()})),
    ok.

extend_oracle(Cfg) ->
    {OracleKey, S} = register_oracle(Cfg),
    PrivKey        = aeo_test_utils:priv_key(OracleKey, S),
    Trees          = aeo_test_utils:trees(S),
    OTrees         = aec_trees:oracles(Trees),
    Oracle         = aeo_state_tree:get_oracle(OracleKey, OTrees),
    TTL0           = aeo_oracles:ttl(Oracle),
    CurrHeight     = ?ORACLE_EXT_HEIGHT,

    %% Test that ExtendTX is accepted
    Tx       = aeo_test_utils:extend_tx(OracleKey, S),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(CurrHeight),
    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S1       = aeo_test_utils:set_trees(Trees1, S),

    OTrees1  = aec_trees:oracles(Trees1),
    Oracle1  = aeo_state_tree:get_oracle(OracleKey, OTrees1),
    TTL1     = aeo_oracles:ttl(Oracle1),
    ct:pal("TTL0 = ~p\nTTL1 = ~p\n", [TTL0, TTL1]),
    true = (TTL0 + maps:get(extend, aeo_test_utils:ttl_defaults())) == TTL1,

    {OracleKey, TTL0, TTL1, S1}.

%%%===================================================================
%%% Query oracle
%%%===================================================================
-define(ORACLE_QUERY_HEIGHT, 3).
-define(NAME, aens_test_utils:fullname(<<"someverylongnameherewithfourpartbitseasy">>)).
query_oracle_via_name(Cfg) ->
    RegisterQueryFee = 5,
    {OracleKey, InitS0}  = register_oracle(Cfg, #{query_fee => RegisterQueryFee}),
    {SenderKey, S0}  = aeo_test_utils:setup_new_account(InitS0),
    OracleId = aeser_id:create(oracle, OracleKey),

    Trees0 = aeo_test_utils:trees(S0),
    Env0   = aetx_env:tx_env(?ORACLE_QUERY_HEIGHT),
    Salt   = 123,
    CHash  = aens_hash:commitment_hash(?NAME, Salt),

    {ok, PreclaimTx} = aens_preclaim_tx:new(
                         aens_test_utils:preclaim_tx_spec(SenderKey, CHash, S0)),
    {ok, Trees1, _}  = aetx:process(PreclaimTx, Trees0, Env0),
    S1 = aeo_test_utils:set_trees(Trees1, S0),
    {value, _} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees1)),

    NameFee = case aec_hard_forks:protocol_effective_at_height(?ORACLE_QUERY_HEIGHT) of
                  V when V >= ?LIMA_PROTOCOL_VSN -> aec_governance:name_claim_fee(?NAME, V);
                  _ -> prelima
              end,
    {ok, NameHash} = aens:get_name_hash(?NAME),
    Env1   = aetx_env:tx_env(?ORACLE_QUERY_HEIGHT + 1),
    {ok, ClaimTx} = aens_claim_tx:new(
                      aens_test_utils:claim_tx_spec(SenderKey, ?NAME, Salt, NameFee,
                                                    #{nonce => 2},
                                                    S1)),
    {ok, Trees2, _}  = aetx:process(ClaimTx, Trees1, Env1),
    S2 = aeo_test_utils:set_trees(Trees2, S1),
    {value, _} = aens_state_tree:lookup_name(NameHash, aec_trees:ns(Trees2)),

    Pointer = aens_pointer:new(<<"oracle_pubkey">>, OracleId),
    Env2   = aetx_env:tx_env(?ORACLE_QUERY_HEIGHT + 2),
    {ok, UpdateTx} = aens_update_tx:new(
                       aens_test_utils:update_tx_spec(SenderKey, NameHash,
                                                      #{nonce => 3, pointers => [Pointer]},
                                                      S2)),
    {ok, Trees3, _}  = aetx:process(UpdateTx, Trees2, Env2),
    S3 = aeo_test_utils:set_trees(Trees3, S2),
    {value, NameObj} = aens_state_tree:lookup_name(NameHash, aec_trees:ns(Trees3)),
    ?assertEqual(aens_names:pointers(NameObj), [Pointer]),

    NameId = aeser_id:create(name, NameHash),
    Q1     = aeo_test_utils:query_tx(SenderKey, NameId, #{nonce => 4}, S2),
    Height = ?ORACLE_QUERY_HEIGHT + 3,
    Env3   = aetx_env:tx_env(Height),
    {ok, Trees4, _} = aetx:process(Q1, Trees3, Env3),
    S4 = aeo_test_utils:set_trees(Trees4, S3),

    {oracle_query_tx, QTx} = aetx:specialize_type(Q1),
    ID = aeo_query:id(aeo_query:new(QTx, Height)),
    Env4 = aetx_env:tx_env(Height),
    RTx1 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{nonce => 2}, S4),
    {ok, Trees5, _} = aetx:process(RTx1, Trees4, Env4),
    _TTL1 = aeo_oracles:ttl(aeo_state_tree:get_oracle(OracleKey, aec_trees:oracles(Trees5))),

    ok.


query_oracle_negative(Cfg) ->
    RegisterQueryFee = 5,
    {OracleKey, S}  = register_oracle(Cfg, #{query_fee => RegisterQueryFee}),
    {SenderKey, S2} = aeo_test_utils:setup_new_account(S),
    Trees           = aeo_test_utils:trees(S2),
    CurrHeight      = ?ORACLE_QUERY_HEIGHT,
    Env             = aetx_env:tx_env(CurrHeight),

    %% Test bad sender key
    BadSenderKey = <<42:32/unit:8>>,
    OracleId     = aeser_id:create(oracle, OracleKey),
    Q1 = aeo_test_utils:query_tx(BadSenderKey, OracleId, S2),
    {error, account_not_found} = aetx:process(Q1, Trees, Env),

    %% Test unsufficient funds.
    S3     = aeo_test_utils:set_account_balance(SenderKey, 0, S2),
    Trees1 = aeo_test_utils:trees(S3),
    Q2     = aeo_test_utils:query_tx(SenderKey, OracleId, S2),
    {error, insufficient_funds} = aetx:process(Q2, Trees1, Env),

    %% Test too high nonce in account
    Q3 = aeo_test_utils:query_tx(SenderKey, OracleId, #{nonce => 0}, S2),
    {error, tx_nonce_already_used_for_account} = aetx:process(Q3, Trees, Env),

    %% Test too low fee
    Q4 = aeo_test_utils:query_tx(SenderKey, OracleId, #{fee => 0}, S2),
    {error, too_low_fee} = aetx:process(Q4, Trees, Env),

    %% Test bad oracle key
    BadOracleId = aeser_id:create(oracle, <<42:32/unit:8>>),
    Q5 = aeo_test_utils:query_tx(SenderKey, BadOracleId, S2),
    {error, oracle_does_not_exist} = aetx:process(Q5, Trees, Env),

    %% Test too long query ttl
    Q6 = aeo_test_utils:query_tx(SenderKey, OracleId, #{ query_ttl => {block, 500} }, S2),
    {error, too_long_ttl} = aetx:process(Q6, Trees, Env),

    %% Test too long response ttl
    Q7 = aeo_test_utils:query_tx(SenderKey, OracleId, #{ response_ttl => {delta, 500} }, S2),
    {error, too_long_ttl} = aetx:process(Q7, Trees, Env),

    %% Test too short TTL
    Q8 = aeo_test_utils:query_tx(SenderKey, OracleId, #{ ttl => CurrHeight - 1 }, S2),
    {error, ttl_expired} = aetx:process(Q8, Trees, Env),

    %% Test too low query fee
    Q9 = aeo_test_utils:query_tx(SenderKey, OracleId, #{query_fee => RegisterQueryFee - 1}, S2),
    {error, query_fee_too_low} = aetx:process(Q9, Trees, Env),
    ok.

query_oracle_negative_dynamic_fee(Cfg) ->
    {OracleKey, S}  = register_oracle(Cfg, #{oracle_ttl => {block, 2000 + ?ORACLE_QUERY_HEIGHT},
                                             fee => 25000 * aec_test_utils:min_gas_price()}),
    OracleId        = aeser_id:create(oracle, OracleKey),
    {SenderKey, S2} = aeo_test_utils:setup_new_account(S),
    Trees           = aeo_test_utils:trees(S2),
    CurrHeight      = ?ORACLE_QUERY_HEIGHT,
    Env             = aetx_env:tx_env(CurrHeight),

    F = fun(QTxOpts) ->
            Tx = aeo_test_utils:query_tx(SenderKey, OracleId, QTxOpts, S2),
            aetx:process(Tx, Trees, Env)
        end,

    %% Test minimum fee for increasing TTL.
    ?assertEqual({error, too_low_fee}, F(#{query_ttl => {delta, 0},
                                           fee => 0})),
    ?assertMatch({ok, _, _}          , F(#{query_ttl => {delta, 0},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertEqual({error, too_low_fee}, F(#{query_ttl => {delta, 1},
                                           fee => 1 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{query_ttl => {delta, 1},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({error, too_low_fee}, F(#{query_ttl => {delta, 1000},
                                           fee => 17000 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{query_ttl => {delta, 1000},
                                           fee => 17250 * aec_test_utils:min_gas_price()})),
    ?assertEqual({error, too_low_fee}, F(#{query_ttl => {delta, 1500},
                                           fee => 17250 * aec_test_utils:min_gas_price()})),
    ?assertMatch({ok, _, _}          , F(#{query_ttl => {delta, 1500},
                                           fee => 17300 * aec_test_utils:min_gas_price()})),
    %% Test more than minimum fee considering TTL.
    ?assertMatch({ok, _, _}          , F(#{query_ttl => {delta, 1500},
                                           fee => 40000 * aec_test_utils:min_gas_price()})),
    ok.

query_oracle_type_check(_Cfg) ->
    case aect_test_utils:latest_sophia_abi_version() of
        ?ABI_AEVM_SOPHIA_1 ->
            IntType = word,
            StringType = string,
            MkType = fun aeb_heap:to_binary/1,
            MkVal = fun aeb_heap:to_binary/1;
        ?ABI_FATE_SOPHIA_1 ->
            IntType = integer,
            StringType = string,
            MkType = fun(X) -> iolist_to_binary(aeb_fate_encoding:serialize_type(X)) end,
            MkVal = fun aeb_fate_encoding:serialize/1
    end,

    RFmt = MkType(IntType),
    F = fun(QFmt, Query, ABIVersion) ->
                {OracleKey, S}  = register_oracle([], #{abi_version => ABIVersion,
                                                        query_format => QFmt,
                                                        response_format => RFmt
                                                       }),
                OracleId        = aeser_id:create(oracle, OracleKey),
                {SenderKey, S2} = aeo_test_utils:setup_new_account(S),
                Trees           = aeo_test_utils:trees(S2),
                CurrHeight      = ?ORACLE_QUERY_HEIGHT,
                Env             = aetx_env:tx_env(CurrHeight),
                Tx = aeo_test_utils:query_tx(SenderKey, OracleId, #{query => Query}, S2),
                aetx:process(Tx, Trees, Env)
        end,
    Int = MkVal(1),
    IntFmt = MkType(IntType),
    String = MkVal(<<"foo">>),
    StringFmt = MkType(StringType),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    ?assertEqual({error, bad_format}, F(StringFmt, Int, ABI)),
    ?assertEqual({error, bad_format}, F(StringFmt, <<123>>, ABI)),
    ?assertEqual({error, bad_format}, F(IntFmt, <<>>, ABI)),
    ?assertEqual({error, bad_format}, F(IntFmt, String, ABI)),
    ?assertMatch({ok, _, _},          F(IntFmt, Int, ABI)),
    ?assertMatch({ok, _, _},          F(StringFmt, String, ABI)),
    %% For ?AEVM_NO_VM the format is always ok
    ?assertMatch({ok, _, _},          F(StringFmt, String, ?ABI_NO_VM)),
    ?assertMatch({ok, _, _},          F(IntFmt, String, ?ABI_NO_VM)),
    ?assertMatch({ok, _, _},          F(StringFmt, Int, ?ABI_NO_VM)),
    ?assertMatch({ok, _, _},          F(IntFmt, String, ?ABI_NO_VM)),
    ok.

query_oracle(Cfg) ->
    query_oracle(Cfg, #{}, #{}).

query_oracle(Cfg, RegTxOpts, QueryTxOpts) ->
    query_oracle_(Cfg, RegTxOpts, QueryTxOpts, register_oracle(Cfg, RegTxOpts)).

query_oracle_(_Cfg, _RegTxOpts, QueryTxOpts, {OracleKey, S1}) ->
    {SenderKey, S2} = aeo_test_utils:setup_new_account(S1),
    Trees           = aeo_test_utils:trees(S2),
    CurrHeight      = ?ORACLE_QUERY_HEIGHT,
    PrivKey         = aeo_test_utils:priv_key(SenderKey, S2),
    OracleId        = aeser_id:create(oracle, OracleKey),

    Q1 = aeo_test_utils:query_tx(SenderKey, OracleId, QueryTxOpts, S2),
    %% Test that QueryTX is accepted
    SignedTx = aec_test_utils:sign_tx(Q1, PrivKey),
    Env      = aetx_env:tx_env(CurrHeight),
    {ok, [SignedTx], Trees2, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S3 = aeo_test_utils:set_trees(Trees2, S2),
    {oracle_query_tx, QTx} = aetx:specialize_type(Q1),
    ID = aeo_query:id(aeo_query:new(QTx, CurrHeight)),
    {OracleKey, ID, S3}.

%%%===================================================================
%%% Query response
%%%===================================================================
-define(ORACLE_RSP_HEIGHT, 5).
query_response_negative(Cfg) ->
    {OracleKey, ID, S1}  = query_oracle(Cfg),
    Trees                = aeo_test_utils:trees(S1),
    CurrHeight           = ?ORACLE_RSP_HEIGHT,
    Env                  = aetx_env:tx_env(CurrHeight),

    %% Test bad oracle key
    BadOracleKey = <<42:32/unit:8>>,
    RTx1 = aeo_test_utils:response_tx(BadOracleKey, ID, <<"42">>, S1),
    {error, no_matching_oracle_query} = aetx:process(RTx1, Trees, Env),

    %% Test too high nonce for account
    RTx2 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{nonce => 0}, S1),
    {error, tx_nonce_already_used_for_account} = aetx:process(RTx2, Trees, Env),

    %% Test fee too low
    RTx3 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{fee => 0}, S1),
    {error, too_low_fee} = aetx:process(RTx3, Trees, Env),

    %% Test too short TTL
    RTx4 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{ttl => CurrHeight - 1}, S1),
    {error, ttl_expired} = aetx:process(RTx4, Trees, Env),

    %% Test bad query id
    OIO = aeo_state_tree:get_query(OracleKey, ID, aec_trees:oracles(Trees)),
    BadId = aeo_query:id(aeo_query:sender_pubkey(OIO), 42, aeo_query:oracle_pubkey(OIO)),
    RTx5 = aeo_test_utils:response_tx(OracleKey, BadId, <<"42">>, S1),
    {error, no_matching_oracle_query} = aetx:process(RTx5, Trees, Env),

    %% Insufficient funds
    S2     = aeo_test_utils:set_account_balance(OracleKey, 1, S1),
    Trees2 = aeo_test_utils:trees(S2),
    RTx6 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, S1),
    {error, insufficient_funds} = aetx:process(RTx6, Trees2, Env),
    ok.

query_response_negative_dynamic_fee(Cfg) ->
    F = fun(RTTL, Fee) ->
                QTxSpec = #{ response_ttl => RTTL },
                RTxSpec = #{ response_ttl => RTTL, fee => Fee },
                {OracleKey, ID, S1}  = query_oracle(Cfg, #{oracle_ttl => {block, 2000 + ?ORACLE_RSP_HEIGHT},
                                                           fee => 25000 * aec_test_utils:min_gas_price()}, QTxSpec),
                Trees      = aeo_test_utils:trees(S1),
                CurrHeight = ?ORACLE_RSP_HEIGHT,
                Env        = aetx_env:tx_env(CurrHeight),
                Tx = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, RTxSpec, S1),
                aetx:process(Tx, Trees, Env)
        end,

    %% Test minimum fee for increasing TTL.
    ?assertEqual({error, too_low_fee}, F({delta, 1},    1 * aec_test_utils:min_gas_price())),
    ?assertMatch({ok, _, _}          , F({delta, 1},    16850 * aec_test_utils:min_gas_price())),
    ?assertMatch({ok, _, _}          , F({delta, 1000}, 16950 * aec_test_utils:min_gas_price())),
    ?assertEqual({error, too_low_fee}, F({delta, 1500}, 16950 * aec_test_utils:min_gas_price())),
    ?assertMatch({ok, _, _}          , F({delta, 1500}, 17000 * aec_test_utils:min_gas_price())),
    %% Test more than minimum fee considering TTL.
    ?assertMatch({ok, _, _}          , F({delta, 1500}, 40000 * aec_test_utils:min_gas_price())),
    ok.

query_response(Cfg) ->
    query_response(Cfg, #{}).

query_response(Cfg, QueryOpts) ->
    query_response_(Cfg, QueryOpts, query_oracle(Cfg, #{}, QueryOpts)).

query_response_(_Cfg, QueryOpts, {OracleKey, ID, S1}) ->
    Trees               = aeo_test_utils:trees(S1),
    CurrHeight          = ?ORACLE_RSP_HEIGHT,

    %% Test that ResponseTX is accepted
    RTx      = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, maps:remove(query_ttl, QueryOpts), S1),
    PrivKey  = aeo_test_utils:priv_key(OracleKey, S1),
    SignedTx = aec_test_utils:sign_tx(RTx, PrivKey),
    Env      = aetx_env:tx_env(CurrHeight),
    {ok, [SignedTx], Trees2, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),

    S2 = aeo_test_utils:set_trees(Trees2, S1),

    %% Test that the query is now closed.
    OIO = aeo_state_tree:get_query(OracleKey, ID, aec_trees:oracles(Trees2)),
    true = aeo_query:is_closed(OIO),

    %% Test that the oracle operator has at least the query fee.
    ?assertMatch({QF, Bal} when (Bal >= QF) andalso is_integer(QF) andalso (QF >= 0), {aeo_query:fee(OIO), aeo_test_utils:account_balance(OracleKey, S1)}),

    {OracleKey, ID, S2}.

query_response_fee_depends_on_response_size(Cfg) ->
    {OracleKey, ID, S1} = query_oracle(Cfg, #{}, #{}),
    Trees               = aeo_test_utils:trees(S1),
    Env                 = aetx_env:tx_env(?ORACLE_RSP_HEIGHT),
    Protocol            = aetx_env:consensus_version(Env),

    SmallResponse  = <<"small_response">>,
    BiggerResponse = <<"bigger_oracle_response">>,
    true = size(SmallResponse) < size(BiggerResponse),

    %% Deduce minimal fee for response tx with SmallResponse
    RTx0        = aeo_test_utils:response_tx(OracleKey, ID, SmallResponse, #{}, S1),
    MinimalFee  = aetx:min_fee(RTx0, ?ORACLE_RSP_HEIGHT, Protocol),

    %% Test oracle response tx with SmallResponse is accepted with MinimalFee
    RTx1        = aeo_test_utils:response_tx(OracleKey, ID, SmallResponse, #{fee => MinimalFee}, S1),
    MinimalFee1 = MinimalFee,
    {ok, _, _}  = aetx:process(RTx1, Trees, Env),

    %% Test oracle response tx with BiggerResponse is not accepted with MinimalFee
    RTx2        = aeo_test_utils:response_tx(OracleKey, ID, BiggerResponse, #{fee => MinimalFee}, S1),
    MinimalFee2 = aetx:min_fee(RTx2, ?ORACLE_RSP_HEIGHT, Protocol),
    true        = MinimalFee2 > MinimalFee1,
    {error, too_low_fee} = aetx:process(RTx2, Trees, Env),

    %% Test oracle response tx with BiggerResponse is accepted with MinimalFee2
    RTx3        = aeo_test_utils:response_tx(OracleKey, ID, BiggerResponse, #{fee => MinimalFee2}, S1),
    MinimalFee3 = aetx:min_fee(RTx3, ?ORACLE_RSP_HEIGHT, Protocol),
    MinimalFee3 = MinimalFee2,
    {ok, _, _}  = aetx:process(RTx3, Trees, Env),
    ok.

query_response_type_check(_Cfg) ->
    case aect_test_utils:latest_sophia_abi_version() of
        ?ABI_AEVM_SOPHIA_1 ->
            IntType = word,
            StringType = string,
            MkType = fun aeb_heap:to_binary/1,
            MkVal = fun aeb_heap:to_binary/1;
        ?ABI_FATE_SOPHIA_1 ->
            IntType = integer,
            StringType = string,
            MkType = fun(X) -> iolist_to_binary(aeb_fate_encoding:serialize_type(X)) end,
            MkVal = fun aeb_fate_encoding:serialize/1
    end,

    QFmt  = MkType(StringType),
    Query = MkVal(<<"who?">>),
    F = fun(RFmt, Resp, ABIVersion) ->
                RegisterOpts = #{abi_version => ABIVersion,
                                 query_format => QFmt,
                                 response_format => RFmt},
                QueryOpts = #{query => Query},
                {OracleKey, ID, S} = query_oracle([], RegisterOpts, QueryOpts),
                Trees           = aeo_test_utils:trees(S),
                CurrHeight      = ?ORACLE_RSP_HEIGHT,
                Env             = aetx_env:tx_env(CurrHeight),
                Tx = aeo_test_utils:response_tx(OracleKey, ID, Resp, S),
                aetx:process(Tx, Trees, Env)
        end,
    Int = MkVal(1),
    IntFmt = MkType(IntType),
    String = MkVal(<<"foo">>),
    StringFmt = MkType(StringType),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    ?assertEqual({error, bad_format}, F(StringFmt, Int, ABI)),
    ?assertEqual({error, bad_format}, F(StringFmt, <<123>>, ABI)),
    ?assertEqual({error, bad_format}, F(IntFmt, <<>>, ABI)),
    ?assertEqual({error, bad_format}, F(IntFmt, String, ABI)),
    ?assertMatch({ok, _, _},          F(IntFmt, Int, ABI)),
    ?assertMatch({ok, _, _},          F(StringFmt, String, ABI)),
    %% For ?ABI_NO_VM the format is always ok
    ?assertMatch({ok, _, _},          F(StringFmt, String, ?ABI_NO_VM)),
    ?assertMatch({ok, _, _},          F(IntFmt, String, ?ABI_NO_VM)),
    ?assertMatch({ok, _, _},          F(StringFmt, Int, ?ABI_NO_VM)),
    ?assertMatch({ok, _, _},          F(IntFmt, String, ?ABI_NO_VM)),
    ok.

%%%===================================================================
%%% Query fee - across query and response transactions
%%%===================================================================
query_fee(Cfg) ->
    QuerySender = fun(O, I, S) -> aeo_query:sender_pubkey(aeo_state_tree:get_query(O, I, aec_trees:oracles(aeo_test_utils:trees(S)))) end,
    Run =
        fun(QFee) when QFee >= 1 ->
            RegOpts = #{query_fee => 1},
            QueryOpts = #{query_fee => QFee},
            {OracleKey, SReg} = RReg = register_oracle(Cfg, RegOpts),
            {OracleKey, ID, SQ} = RQ = query_oracle_(Cfg, RegOpts, QueryOpts, RReg),
            QuerySenderKey = QuerySender(OracleKey, ID, SQ),
            {OracleKey, ID, SResp} = query_response_(Cfg, QueryOpts, RQ),
            QuerySenderKey = QuerySender(OracleKey, ID, SResp),
            {{OracleKey, QuerySenderKey}, {SReg, SQ, SResp}}
        end,
    Bal = fun(K, S) -> aeo_test_utils:account_balance(K, S) end,
    DeltaQFee = 1,
    BaseQFee = 11,
    {{OracleKeyA, QuerySenderKeyA}, {SRegisterA, SQueryA, SResponseA}} = Run(BaseQFee),
    {{OracleKeyB, QuerySenderKeyB}, {SRegisterB, SQueryB, SResponseB}} = Run(BaseQFee + DeltaQFee),
    %% Sanity check: register transaction does not debit or credit query fee. (There is no query sender yet so no need to check that.)
    ?assertEqual(Bal(OracleKeyA, SRegisterA), Bal(OracleKeyB, SRegisterB)),
    %% Check that query transaction debits query fee ...
    ?assertEqual(Bal(QuerySenderKeyA, SQueryA), DeltaQFee + Bal(QuerySenderKeyB, SQueryB)),
    %% ... but does not credit it yet.
    ?assertEqual(Bal(OracleKeyA, SQueryA), Bal(OracleKeyB     , SQueryB)),
    %% Check that response transaction credits query fee ...
    ?assertEqual(Bal(OracleKeyA, SResponseA) + DeltaQFee, Bal(OracleKeyB, SResponseB)),
    %% .. and does not re-debit it.
    ?assertEqual(Bal(QuerySenderKeyA, SQueryA), Bal(QuerySenderKeyA, SResponseA)),
    ?assertEqual(Bal(QuerySenderKeyA, SResponseA), DeltaQFee + Bal(QuerySenderKeyB, SResponseB)),
    ok.

%%%===================================================================
%%% Pruning tests
%%%===================================================================

prune_oracle(Cfg) ->
    {OracleKey, S} = register_oracle(Cfg),
    Trees          = aeo_test_utils:trees(S),
    OTrees         = aec_trees:oracles(Trees),
    Oracle         = aeo_state_tree:get_oracle(OracleKey, OTrees),
    TTL            = ?ORACLE_REG_HEIGHT + maps:get(oracle, aeo_test_utils:ttl_defaults()),

    %% Test that the oracle is pruned
    Gone  = prune_from_until(?GENESIS_HEIGHT, TTL + 1, Trees),
    none  = aeo_state_tree:lookup_oracle(OracleKey, aec_trees:oracles(Gone)),

    %% Test that the oracle remains
    Left      = prune_from_until(?GENESIS_HEIGHT, TTL, Trees),
    Oracle    = aeo_state_tree:get_oracle(OracleKey, aec_trees:oracles(Left)),
    OracleKey = aeo_oracles:pubkey(Oracle),
    ok.

prune_oracle_extend(Cfg) ->
    {OracleKey, Exp1, Exp2, S} = extend_oracle(Cfg),
    Trees                      = aeo_test_utils:trees(S),

    %% Test that the oracle is not pruned prematurely
    Left1 = prune_from_until(?GENESIS_HEIGHT, Exp1 + 1, Trees),
    Oracle0   = aeo_state_tree:get_oracle(OracleKey, aec_trees:oracles(Left1)),
    OracleKey = aeo_oracles:pubkey(Oracle0),

    %% Test that the oracle is pruned
    Gone  = prune_from_until(?GENESIS_HEIGHT, Exp2 + 1, Trees),
    none  = aeo_state_tree:lookup_oracle(OracleKey, aec_trees:oracles(Gone)),

    %% Test that the oracle remains
    Left2     = prune_from_until(?GENESIS_HEIGHT, Exp2, Trees),
    Oracle2   = aeo_state_tree:get_oracle(OracleKey, aec_trees:oracles(Left2)),
    OracleKey = aeo_oracles:pubkey(Oracle2),
    ok.

prune_query(Cfg) ->
    {OracleKey, ID, S} = query_oracle(Cfg),
    Trees              = aeo_test_utils:trees(S),
    OTrees             = aec_trees:oracles(Trees),
    OIO                = aeo_state_tree:get_query(OracleKey, ID, OTrees),
    TTL                = ?ORACLE_QUERY_HEIGHT + maps:get(query, aeo_test_utils:ttl_defaults()),
    SenderKey          = aeo_query:sender_pubkey(OIO),

    %% Test that the query is pruned
    Gone  = prune_from_until(?GENESIS_HEIGHT, TTL + 1, Trees),
    none  = aeo_state_tree:lookup_query(OracleKey, ID, aec_trees:oracles(Gone)),

    %% Check that the query fee was refunded
    PreAccount  = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Trees)),
    PostAccount = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Gone)),
    true = aec_accounts:balance(PreAccount) < aec_accounts:balance(PostAccount),

    %% Test that the query remains
    Left  = prune_from_until(?GENESIS_HEIGHT, TTL, Trees),
    OIO2  = aeo_state_tree:get_query(OracleKey, ID, aec_trees:oracles(Left)),
    ID    = aeo_query:id(OIO2),
    ok.

prune_response_short(Cfg) ->
    prune_response(Cfg, #{ query_ttl => {delta, 50}, response_ttl => {delta, 25} }).

prune_response_long(Cfg) ->
    prune_response(Cfg, #{ query_ttl => {delta, 50}, response_ttl => {delta, 75} }).

prune_response(Cfg, QueryOpts = #{ response_ttl := {delta, RTTL} }) ->
    {OracleKey, ID, S} = query_response(Cfg, QueryOpts),
    Trees              = aeo_test_utils:trees(S),
    OTrees             = aec_trees:oracles(Trees),
    OIO                = aeo_state_tree:get_query(OracleKey, ID, OTrees),
    TTL                = ?ORACLE_RSP_HEIGHT + RTTL,
    SenderKey          = aeo_query:sender_pubkey(OIO),

    %% Test that the query is pruned
    Gone  = prune_from_until(?GENESIS_HEIGHT, TTL + 1, Trees),
    none  = aeo_state_tree:lookup_query(OracleKey, ID, aec_trees:oracles(Gone)),

    %% Check that the query fee was not refunded
    PreAccount  = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Trees)),
    PostAccount = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Gone)),
    true = aec_accounts:balance(PreAccount) == aec_accounts:balance(PostAccount),

    %% Test that the query remains
    Left  = prune_from_until(?GENESIS_HEIGHT, TTL, Trees),
    OIO2  = aeo_state_tree:get_query(OracleKey, ID, aec_trees:oracles(Left)),
    ID    = aeo_query:id(OIO2),
    ok.

prune_from_until(From, Until, Trees) when is_integer(From),
                                          is_integer(Until),
                                          From < Until ->
    do_prune_until(From, Until, Trees).

do_prune_until(N1, N1, Trees) ->
    aeo_state_tree:prune(N1, Trees);
do_prune_until(N1, N2, Trees) ->
    do_prune_until(N1 + 1, N2, aeo_state_tree:prune(N1, Trees)).
