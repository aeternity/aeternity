%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc CT test suite for AE Oracles
%%% @end
%%%-------------------------------------------------------------------
-module(aeoracle_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        ]).

%% test case exports
-export([ extend_oracle/1
        , extend_oracle_negative/1
        , prune_oracle/1
        , prune_oracle_extend/1
        , prune_query/1
        , prune_response/1
        , query_oracle/1
        , query_oracle_negative/1
        , query_response/1
        , query_response_negative/1
        , register_oracle/1
        , register_oracle_negative/1
        ]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aeoracle/include/oracle_txs.hrl").

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
                                 , extend_oracle
                                 , extend_oracle_negative
                                 , query_oracle
                                 , query_oracle_negative
                                 , query_response
                                 , query_response_negative
                                 ]}
    , {state_tree, [ prune_oracle
                   , prune_oracle_extend
                   , prune_query
                   , prune_response
                   ]}
    ].

%%%===================================================================
%%% Register oracle
%%%===================================================================

register_oracle_negative(_Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Trees        = aeo_test_utils:trees(S1),
    CurrHeight   = 1,

    %% Test registering a bogus account
    BadPubKey = <<42:65/unit:8>>,
    RTx1      = aeo_test_utils:register_tx(BadPubKey, S1),
    {error, account_not_found} = aetx:check(RTx1, Trees, CurrHeight),

    %% Insufficient funds
    S2     = aeo_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aeo_test_utils:trees(S2),
    RTx2 = aeo_test_utils:register_tx(PubKey, S1),
    {error, insufficient_funds} =
        aetx:check(RTx2, Trees2, CurrHeight),

    %% Test too high account nonce
    RTx3 = aeo_test_utils:register_tx(PubKey, #{nonce => 0}, S1),
    {error, account_nonce_too_high} =
        aetx:check(RTx3, Trees, CurrHeight),

    %% Test too low fee
    RTx4 = aeo_test_utils:register_tx(PubKey, #{fee => 0}, S1),
    {error, too_low_fee} = aetx:check(RTx4, Trees, CurrHeight),
    ok.

register_oracle(_Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Tx           = aeo_test_utils:register_tx(PubKey, #{ ttl => {delta, 100} }, S1),
    PrivKey      = aeo_test_utils:priv_key(PubKey, S1),

    %% Test that RegisterTX is accepted
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    Trees    = aeo_test_utils:trees(S1),
    Height   = 1,
    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),
    S2       = aeo_test_utils:set_trees(Trees1, S1),
    {PubKey, S2}.

%%%===================================================================
%%% Extend oracle
%%%===================================================================

extend_oracle_negative(Cfg) ->
    {PubKey, S1} = aeo_test_utils:setup_new_account(aeo_test_utils:new_state()),
    Trees        = aeo_test_utils:trees(S1),
    CurrHeight   = 1,

    %% Test registering a bogus account
    BadPubKey = <<42:65/unit:8>>,
    RTx1      = aeo_test_utils:extend_tx(BadPubKey, S1),
    {error, account_not_found} = aetx:check(RTx1, Trees, CurrHeight),

    %% Test extending non-existent oracle
    RTx2 = aeo_test_utils:extend_tx(PubKey, S1),
    {error, account_is_not_an_active_oracle} =
        aetx:check(RTx2, Trees, CurrHeight),

    %% Register the oracle
    {OracleKey, S2} = register_oracle(Cfg),
    Trees2          = aeo_test_utils:trees(S2),
    CurrHeight2     = 3,

    %% Insufficient funds
    S3     = aeo_test_utils:set_account_balance(OracleKey, 0, S2),
    Trees3 = aeo_test_utils:trees(S3),
    RTx3 = aeo_test_utils:extend_tx(OracleKey, S3),
    {error, insufficient_funds} =
        aetx:check(RTx3, Trees3, CurrHeight2),

    %% Test too high account nonce
    RTx4 = aeo_test_utils:extend_tx(OracleKey, #{nonce => 0}, S2),
    {error, account_nonce_too_high} =
        aetx:check(RTx4, Trees2, CurrHeight2),

    %% Test too low fee
    RTx5 = aeo_test_utils:extend_tx(OracleKey, #{fee => 0}, S2),
    {error, too_low_fee} = aetx:check(RTx5, Trees2, CurrHeight2),
    ok.

extend_oracle(Cfg) ->
    {OracleKey, S} = register_oracle(Cfg),
    PrivKey        = aeo_test_utils:priv_key(OracleKey, S),
    Trees          = aeo_test_utils:trees(S),
    OTrees         = aec_trees:oracles(Trees),
    Oracle         = aeo_state_tree:get_oracle(OracleKey, OTrees),
    Expires0       = aeo_oracles:expires(Oracle),
    CurrHeight     = 3,

    %% Test that ExtendTX is accepted
    Tx       = aeo_test_utils:extend_tx(OracleKey, #{ ttl => {delta, 50} }, S),
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, CurrHeight),
    S1       = aeo_test_utils:set_trees(Trees1, S),

    OTrees1  = aec_trees:oracles(Trees1),
    Oracle1  = aeo_state_tree:get_oracle(OracleKey, OTrees1),
    Expires1 = aeo_oracles:expires(Oracle1),
    ct:pal("Expires0 = ~p\nExpires1 = ~p\n", [Expires0, Expires1]),
    true = (Expires0 + 50) == Expires1,

    {OracleKey, Expires0, Expires1, S1}.

%%%===================================================================
%%% Query oracle
%%%===================================================================

query_oracle_negative(Cfg) ->
    {OracleKey, S}  = register_oracle(Cfg),
    {SenderKey, S2} = aeo_test_utils:setup_new_account(S),
    Trees           = aeo_test_utils:trees(S2),
    CurrHeight      = 3,

    %% Test bad sender key
    BadSenderKey = <<42:65/unit:8>>,
    Q1 = aeo_test_utils:query_tx(BadSenderKey, OracleKey, S2),
    {error, account_not_found} = aetx:check(Q1, Trees, CurrHeight),

    %% Test unsufficient funds.
    S3     = aeo_test_utils:set_account_balance(SenderKey, 0, S2),
    Trees1 = aeo_test_utils:trees(S3),
    Q2     = aeo_test_utils:query_tx(SenderKey, OracleKey, S2),
    {error, insufficient_funds} = aetx:check(Q2, Trees1, CurrHeight),

    %% Test too high nonce in account
    Q3 = aeo_test_utils:query_tx(SenderKey, OracleKey, #{nonce => 0}, S2),
    {error, account_nonce_too_high} = aetx:check(Q3, Trees, CurrHeight),

    %% Test too low query fee
    Q4 = aeo_test_utils:query_tx(SenderKey, OracleKey, #{fee => 0}, S2),
    {error, too_low_fee} = aetx:check(Q4, Trees, CurrHeight),

    %% Test bad oracle key
    BadOracleKey = <<42:65/unit:8>>,
    Q5 = aeo_test_utils:query_tx(SenderKey, BadOracleKey, S2),
    {error, oracle_does_not_exist} = aetx:check(Q5, Trees, CurrHeight),

    %% Test too long query ttl
    Q6 = aeo_test_utils:query_tx(SenderKey, OracleKey, #{ query_ttl => {block, 200} }, S2),
    {error, too_long_ttl} = aetx:check(Q6, Trees, CurrHeight),

    %% Test too long response ttl
    Q7 = aeo_test_utils:query_tx(SenderKey, OracleKey, #{ response_ttl => {delta, 50} }, S2),
    {error, too_long_ttl} = aetx:check(Q7, Trees, CurrHeight),
    ok.

query_oracle(Cfg) ->
    {OracleKey, S1} = register_oracle(Cfg),
    {SenderKey, S2} = aeo_test_utils:setup_new_account(S1),
    Trees           = aeo_test_utils:trees(S2),
    CurrHeight      = 3,
    PrivKey         = aeo_test_utils:priv_key(SenderKey, S2),

    Q1 = aeo_test_utils:query_tx(SenderKey, OracleKey, S2),
    %% Test that QueryTX is accepted
    SignedTx = aetx_sign:sign(Q1, PrivKey),
    {ok, [SignedTx], Trees2} =
        aec_trees:apply_signed_txs([SignedTx], Trees, CurrHeight),
    S3 = aeo_test_utils:set_trees(Trees2, S2),
    {aeo_query_tx, QTx} = aetx:specialize_type(Q1),
    ID = aeo_query:id(aeo_query:new(QTx, CurrHeight)),
    {OracleKey, ID, S3}.

%%%===================================================================
%%% Query resoponse
%%%===================================================================

query_response_negative(Cfg) ->
    {OracleKey, ID, S1}  = query_oracle(Cfg),
    Trees                = aeo_test_utils:trees(S1),
    CurrHeight           = 5,

    %% Test bad oracle key
    BadOracleKey = <<42:65/unit:8>>,
    RTx1 = aeo_test_utils:response_tx(BadOracleKey, ID, <<"42">>, S1),
    {error, no_matching_oracle_query} =
        aetx:check(RTx1, Trees, CurrHeight),

    %% Test too high nonce for account
    RTx2 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{nonce => 0}, S1),
    {error, account_nonce_too_high} =
        aetx:check(RTx2, Trees, CurrHeight),

    %% Test fee too low
    RTx3 = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, #{fee => 0}, S1),
    {error, too_low_fee} = aetx:check(RTx3, Trees, CurrHeight),

    %% Test bad query id
    OIO = aeo_state_tree:get_query(OracleKey, ID, aec_trees:oracles(Trees)),
    BadId = aeo_query:id(aeo_query:set_sender_nonce(42, OIO)),
    RTx4 = aeo_test_utils:response_tx(OracleKey, BadId, <<"42">>, S1),
    {error, no_matching_oracle_query} =
        aetx:check(RTx4, Trees, CurrHeight),
    ok.

query_response(Cfg) ->
    {OracleKey, ID, S1} = query_oracle(Cfg),
    Trees               = aeo_test_utils:trees(S1),
    CurrHeight          = 5,

    %% Test that ResponseTX is accepted
    RTx      = aeo_test_utils:response_tx(OracleKey, ID, <<"42">>, S1),
    SignedTx = aetx_sign:sign(RTx, <<"pkey1">>),
    {ok, [SignedTx], Trees2} =
        aec_trees:apply_signed_txs([SignedTx], Trees, CurrHeight),
    S2 = aeo_test_utils:set_trees(Trees2, S1),

    %% Test that the query is now closed.
    OIO = aeo_state_tree:get_query(OracleKey, ID, aec_trees:oracles(Trees2)),
    true = aeo_query:is_closed(OIO),

    {OracleKey, ID, S2}.

%%%===================================================================
%%% Pruning tests
%%%===================================================================

prune_oracle(Cfg) ->
    {OracleKey, S} = register_oracle(Cfg),
    Trees          = aeo_test_utils:trees(S),
    OTrees         = aec_trees:oracles(Trees),
    Oracle         = aeo_state_tree:get_oracle(OracleKey, OTrees),
    Expires        = aeo_oracles:expires(Oracle),

    %% Test that the oracle is pruned
    Gone  = prune_from_until(0, Expires + 1, Trees),
    none  = aeo_state_tree:lookup_oracle(OracleKey, aec_trees:oracles(Gone)),

    %% Test that the oracle remains
    Left      = prune_from_until(0, Expires, Trees),
    Oracle    = aeo_state_tree:get_oracle(OracleKey, aec_trees:oracles(Left)),
    OracleKey = aeo_oracles:owner(Oracle),
    ok.

prune_oracle_extend(Cfg) ->
    {OracleKey, Exp1, Exp2, S} = extend_oracle(Cfg),
    Trees                      = aeo_test_utils:trees(S),

    %% Test that the oracle is not pruned prematurely
    Left1 = prune_from_until(0, Exp1 + 1, Trees),
    Oracle0   = aeo_state_tree:get_oracle(OracleKey, aec_trees:oracles(Left1)),
    OracleKey = aeo_oracles:owner(Oracle0),

    %% Test that the oracle is pruned
    Gone  = prune_from_until(0, Exp2 + 1, Trees),
    none  = aeo_state_tree:lookup_oracle(OracleKey, aec_trees:oracles(Gone)),

    %% Test that the oracle remains
    Left2     = prune_from_until(0, Exp2, Trees),
    Oracle2   = aeo_state_tree:get_oracle(OracleKey, aec_trees:oracles(Left2)),
    OracleKey = aeo_oracles:owner(Oracle2),
    ok.

prune_query(Cfg) ->
    {OracleKey, ID, S} = query_oracle(Cfg),
    Trees              = aeo_test_utils:trees(S),
    OTrees             = aec_trees:oracles(Trees),
    OIO                = aeo_state_tree:get_query(OracleKey, ID, OTrees),
    Expires            = aeo_query:expires(OIO),
    SenderKey          = aeo_query:sender_address(OIO),

    %% Test that the query is pruned
    Gone  = prune_from_until(0, Expires + 1, Trees),
    none  = aeo_state_tree:lookup_query(OracleKey, ID, aec_trees:oracles(Gone)),

    %% Check that the query fee was refunded
    PreAccount  = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Trees)),
    PostAccount = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Gone)),
    true = aec_accounts:balance(PreAccount) < aec_accounts:balance(PostAccount),

    %% Test that the query remains
    Left  = prune_from_until(0, Expires, Trees),
    OIO2  = aeo_state_tree:get_query(OracleKey, ID, aec_trees:oracles(Left)),
    ID    = aeo_query:id(OIO2),
    ok.

prune_response(Cfg) ->
    {OracleKey, ID, S} = query_response(Cfg),
    Trees              = aeo_test_utils:trees(S),
    OTrees             = aec_trees:oracles(Trees),
    OIO                = aeo_state_tree:get_query(OracleKey, ID, OTrees),
    Expires            = aeo_query:expires(OIO),
    SenderKey          = aeo_query:sender_address(OIO),

    %% Test that the query is pruned
    Gone  = prune_from_until(0, Expires + 1, Trees),
    none  = aeo_state_tree:lookup_query(OracleKey, ID, aec_trees:oracles(Gone)),

    %% Check that the query fee was not refunded
    PreAccount  = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Trees)),
    PostAccount = aec_accounts_trees:get(SenderKey, aec_trees:accounts(Gone)),
    true = aec_accounts:balance(PreAccount) == aec_accounts:balance(PostAccount),

    %% Test that the query remains
    Left  = prune_from_until(0, Expires, Trees),
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
