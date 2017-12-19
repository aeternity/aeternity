%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc CT test suite for AE Oracles
%%% @end
%%%-------------------------------------------------------------------
-module(aeoracle_SUITE).

%% common_test exports
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ,
         init_per_testcase/2, end_per_testcase/2]).

%% test case exports
-export([query_oracle/1, query_oracle_negative/1,
         query_response/1, query_response_negative/1,
         register_oracle/1, register_oracle_negative/1]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aeoracle/include/oracle_txs.hrl").

all() ->
    [query_oracle, query_oracle_negative,
     query_response, query_response_negative,
     register_oracle, register_oracle_negative].

init_per_suite(Cfg) ->
    Cfg.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

register_oracle_negative(_Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Trees        = aeo_test_utils:trees(S1),
    CurrHeight   = 1,

    %% Test registering a bogus account
    BadPubKey = <<42:65/unit:8>>,
    RTx1      = aeo_test_utils:register_tx(BadPubKey, S1),
    {error, account_not_found} = aeo_register_tx:check(RTx1, Trees, CurrHeight),

    %% Insufficient funds
    S2     = aeo_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aeo_test_utils:trees(S2),
    RTx2 = aeo_test_utils:register_tx(PubKey, S1),
    {error, insufficient_funds} =
        aeo_register_tx:check(RTx2, Trees2, CurrHeight),

    %% Test too high account nonce
    RTx3 = aeo_test_utils:register_tx(PubKey, #{nonce => 0}, S1),
    {error, account_nonce_too_high} =
        aeo_register_tx:check(RTx3, Trees, CurrHeight),

    %% Test too low fee
    RTx4 = aeo_test_utils:register_tx(PubKey, #{fee => 0}, S1),
    {error, too_low_fee} = aeo_register_tx:check(RTx4, Trees, CurrHeight),
    ok.

register_oracle(_Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Tx           = aeo_test_utils:register_tx(PubKey, S1),
    PrivKey      = aeo_test_utils:priv_key(PubKey, S1),

    %% Test that RegisterTX is accepted
    SignedTx = aec_tx_sign:sign(Tx, PrivKey),
    Trees    = aeo_test_utils:trees(S1),
    Height   = 1,
    {ok, [SignedTx], Trees1} = aec_tx:apply_signed([SignedTx], Trees, Height),
    S2       = aeo_test_utils:set_trees(Trees1, S1),
    {PubKey, S2}.

query_oracle_negative(Cfg) ->
    {OracleKey, S}  = register_oracle(Cfg),
    {SenderKey, S2} = aeo_test_utils:setup_new_account(S),
    Trees           = aeo_test_utils:trees(S2),
    CurrHeight      = 3,

    %% Test bad sender key
    BadSenderKey = <<42:65/unit:8>>,
    Q1 = aeo_test_utils:query_tx(BadSenderKey, OracleKey, S2),
    {error, account_not_found} = aeo_query_tx:check(Q1, Trees, CurrHeight),

    %% Test unsufficient funds.
    S3     = aeo_test_utils:set_account_balance(SenderKey, 0, S2),
    Trees1 = aeo_test_utils:trees(S3),
    Q2     = aeo_test_utils:query_tx(SenderKey, OracleKey, S2),
    {error, insufficient_funds} = aeo_query_tx:check(Q2, Trees1, CurrHeight),

    %% Test too high nonce in account
    Q3 = aeo_test_utils:query_tx(SenderKey, OracleKey, #{nonce => 0}, S2),
    {error, account_nonce_too_high} = aeo_query_tx:check(Q3, Trees, CurrHeight),

    %% Test too low query fee
    Q4 = aeo_test_utils:query_tx(SenderKey, OracleKey, #{fee => 0}, S2),
    {error, too_low_fee} = aeo_query_tx:check(Q4, Trees, CurrHeight),

    %% Test bad oracle key
    BadOracleKey = <<42:65/unit:8>>,
    Q5 = aeo_test_utils:query_tx(SenderKey, BadOracleKey, S2),
    {error, oracle_does_not_exist} = aeo_query_tx:check(Q5, Trees, CurrHeight),
    ok.

query_oracle(Cfg) ->
    {OracleKey, S1} = register_oracle(Cfg),
    {SenderKey, S2} = aeo_test_utils:setup_new_account(S1),
    Trees           = aeo_test_utils:trees(S2),
    CurrHeight      = 3,
    PrivKey         = aeo_test_utils:priv_key(SenderKey, S2),

    Q1 = aeo_test_utils:query_tx(SenderKey, OracleKey, S2),
    %% Test that QueryTX is accepted
    SignedTx = aec_tx_sign:sign(Q1, PrivKey),
    {ok, [SignedTx], Trees2} =
        aec_tx:apply_signed([SignedTx], Trees, CurrHeight),
    S3 = aeo_test_utils:set_trees(Trees2, S2),
    {OracleKey, aeo_interaction:new(Q1, CurrHeight), S3}.

query_response_negative(Cfg) ->
    {OracleKey, OIO, S1} = query_oracle(Cfg),
    Trees                = aeo_test_utils:trees(S1),
    CurrHeight           = 5,
    ID                   = aeo_interaction:id(OIO),

    %% Test bad oracle key
    BadOracleKey = <<42:65/unit:8>>,
    RTx1 = aeo_test_utils:response_tx(BadOracleKey, ID, <<"42">>, S1),
    {error, oracle_does_not_match_interaction_id} =
        aeo_response_tx:check(RTx1, Trees, CurrHeight),

    %% Test too high nonce for account
    RTx2 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{nonce => 0}, S1),
    {error, account_nonce_too_high} =
        aeo_response_tx:check(RTx2, Trees, CurrHeight),

    %% Test fee too low
    RTx3 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{fee => 0}, S1),
    {error, too_low_fee} = aeo_response_tx:check(RTx3, Trees, CurrHeight),

    %% Test bad interaction id
    BadId = aeo_interaction:id(aeo_interaction:set_sender_nonce(42, OIO)),
    RTx4 = aeo_test_utils:response_tx(OracleKey, BadId, <<"42">>, S1),
    {error, oracle_interaction_id_does_not_exist} =
        aeo_response_tx:check(RTx4, Trees, CurrHeight),
    ok.

query_response(Cfg) ->
    {OracleKey, OIO, S1} = query_oracle(Cfg),
    Trees                = aeo_test_utils:trees(S1),
    CurrHeight           = 5,
    ID                   = aeo_interaction:id(OIO),

    %% Test that ResponseTX is accepted
    RTx      = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, S1),
    SignedTx = aec_tx_sign:sign(RTx, <<"pkey1">>),
    {ok, [SignedTx], _Trees2} =
        aec_tx:apply_signed([SignedTx], Trees, CurrHeight).
