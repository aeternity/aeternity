%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc CT test suite for AE Oracles
%%% @end
%%%-------------------------------------------------------------------
-module(aeoracle_SUITE).

%% common_test exports
-export([all/0, init_per_suite/1, end_per_suite/1,
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
    ok = meck:new(aec_tx_sign, [passthrough]),
    meck:expect(aec_tx_sign, sign,
        fun(Data, Key) -> {signed_tx, Data, [Key]} end),
    meck:expect(aec_tx_sign, data,
        fun({signed_tx, Data, _}) -> Data end),
    Config.

end_per_testcase(_, _) ->
    meck:unload(aec_tx_sign),
    ok.

register_oracle_negative(_Cfg) ->
    Trees = aec_trees:new(),
    CurrHeight  = 1,
    OracleKey   = <<"oracle">>,

    RegisterTx = #oracle_register_tx{ account = OracleKey,
                                      nonce   = 0,
                                      query_fee = 5,
                                      ttl = {block, 25},
                                      fee = 5 },

    {error, account_not_found} =
        aeo_register_tx:check(RegisterTx, Trees, CurrHeight),

    %% Add the account with 4 tokens
    Trees1 = add_account(Trees, OracleKey, 4, CurrHeight),

    %% Insufficient funds fee = 5
    {error, insufficient_funds} =
        aeo_register_tx:check(RegisterTx, Trees1, CurrHeight),

    %% Add the account with 8 tokens
    Trees2 = add_account(Trees, OracleKey, 8, CurrHeight),

    {error, account_nonce_too_high} =
        aeo_register_tx:check(RegisterTx, Trees2, CurrHeight),

    RegisterTx1 = RegisterTx#oracle_register_tx{ nonce = 1, fee = 4 },

    {error, too_low_fee} =
        aeo_register_tx:check(RegisterTx1, Trees2, CurrHeight).

register_oracle(_Cfg) ->
    Trees = aec_trees:new(),
    CurrHeight  = 1,
    OracleKey   = <<"oracle                                                           ">>,

    RegisterTx = #oracle_register_tx{ account = OracleKey,
                                      nonce   = 1,
                                      query_fee = 5,
                                      ttl = {block, 25},
                                      fee = 5 },

    %% Add the account with enough funds
    Trees1 = add_account(Trees, OracleKey, 20, CurrHeight),

    %% Test that RegisterTX is accepted
    SignedTx = aec_tx_sign:sign(RegisterTx, <<"pkey1">>),
    {ok, [SignedTx], Trees2} = aec_tx:apply_signed([SignedTx], Trees1, CurrHeight),
    Trees2.


query_oracle_negative(Cfg) ->
    Trees = register_oracle(Cfg),

    CurrHeight   = 3,
    OracleKey    = <<"oracle                                                           ">>,
    BadOracleKey = <<"oracle2                                                          ">>,
    SenderKey    = <<"sender                                                           ">>,

    QueryTx = #oracle_query_tx{ sender = SenderKey,
                                nonce = 0,
                                oracle = OracleKey,
                                query  = <<"Hello world">>,
                                query_fee = 5,
                                query_ttl = {delta, 50},
                                response_ttl = {delta, 25},
                                fee = 3 },

    {error, account_not_found} =
        aeo_query_tx:check(QueryTx, Trees, CurrHeight),

    %% Add the account with 4 tokens
    Trees1 = add_account(Trees, SenderKey, 4, CurrHeight),

    %% Insufficient funds fee = 3 + 5
    {error, insufficient_funds} =
        aeo_query_tx:check(QueryTx, Trees1, CurrHeight),

    %% Add the account with 8 tokens
    Trees2 = add_account(Trees, SenderKey, 8, CurrHeight),

    {error, account_nonce_too_high} =
        aeo_query_tx:check(QueryTx, Trees2, CurrHeight),

    QueryTx1 = QueryTx#oracle_query_tx{ nonce = 1, fee = 2 },

    {error, too_low_fee} =
        aeo_query_tx:check(QueryTx1, Trees2, CurrHeight),

    QueryTx2 = QueryTx1#oracle_query_tx{ fee = 3, oracle = BadOracleKey },

    {error, oracle_does_not_exist} =
        aeo_query_tx:check(QueryTx2, Trees2, CurrHeight).

query_oracle(Cfg) ->
    Trees = register_oracle(Cfg),

    CurrHeight  = 3,
    OracleKey   = <<"oracle                                                           ">>,
    SenderKey   = <<"sender                                                           ">>,

    QueryTx = #oracle_query_tx{ sender = SenderKey,
                                nonce = 1,
                                oracle = OracleKey,
                                query  = <<"Hello world">>,
                                query_fee = 5,
                                query_ttl = {delta, 50},
                                response_ttl = {delta, 25},
                                fee = 8 },

    OIO = aeo_interaction:new(QueryTx, CurrHeight),

    %% Add the account with enough funds
    Trees1 = add_account(Trees, SenderKey, 20, CurrHeight),

    %% Test that QueryTX is accepted
    SignedTx = aec_tx_sign:sign(QueryTx, <<"pkey1">>),
    {ok, [SignedTx], Trees2} = aec_tx:apply_signed([SignedTx], Trees1, CurrHeight),
    {Trees2, OIO}.

query_response_negative(Cfg) ->
    {Trees, OIO} = query_oracle(Cfg),

    CurrHeight   = 5,
    OracleKey    = <<"oracle                                                           ">>,
    BadOracleKey = <<"oracle2                                                          ">>,

    ResponseTx = #oracle_response_tx{ oracle         = OracleKey,
                                      nonce          = 0,
                                      interaction_id = aeo_interaction:id(OIO),
                                      response       = <<42>>,
                                      fee            = 3 },

    {error, account_nonce_too_high} =
        aeo_response_tx:check(ResponseTx, Trees, CurrHeight),

    ResponseTx1 = ResponseTx#oracle_response_tx{ nonce = 2, fee = 2 },

    {error, too_low_fee} =
        aeo_response_tx:check(ResponseTx1, Trees, CurrHeight),

    ResponseTx2 = ResponseTx1#oracle_response_tx{ fee = 3, oracle = BadOracleKey },

    {error, oracle_does_not_match_interaction_id} =
        aeo_response_tx:check(ResponseTx2, Trees, CurrHeight),

    BadId = aeo_interaction:id(aeo_interaction:set_sender_nonce(42, OIO)),
    ResponseTx3 = ResponseTx2#oracle_response_tx{ oracle = OracleKey,
                                                  interaction_id = BadId },

    {error, oracle_interaction_id_does_not_exist} =
        aeo_response_tx:check(ResponseTx3, Trees, CurrHeight).

query_response(Cfg) ->
    {Trees, OIO} = query_oracle(Cfg),

    CurrHeight  = 5,
    OracleKey   = <<"oracle                                                           ">>,

    ResponseTx = #oracle_response_tx{ oracle         = OracleKey,
                                      nonce          = 2,
                                      interaction_id = aeo_interaction:id(OIO),
                                      response       = <<42>>,
                                      fee            = 3 },

    %% Test that ResponseTX is accepted
    SignedTx = aec_tx_sign:sign(ResponseTx, <<"pkey1">>),
    {ok, [SignedTx], _Trees2} = aec_tx:apply_signed([SignedTx], Trees, CurrHeight).

%% -- Helper functions -------------------------------------------------------

add_account(Trees, AccountKey, Balance, Height) ->
    As  = aec_trees:accounts(Trees),
    A   = aec_accounts:new(AccountKey, Balance, Height),
    As1 = aec_accounts_trees:enter(A, As),
    aec_trees:set_accounts(Trees, As1).


