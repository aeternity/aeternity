%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    CT test suite for AE Naming System
%%% @end
%%%=============================================================================

-module(aens_SUITE).

%% common_test exports
-export([all/0,
         groups/0
        ]).

%% test case exports
-export([preclaim/1,
         prune_claim/1,
         preclaim_negative/1,
         claim/1,
         claim_negative/1,
         claim_race_negative/1,
         update/1,
         update_negative/1,
         transfer/1,
         transfer_negative/1,
         revoke/1,
         revoke_negative/1,
         prune_preclaim/1]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aens/include/ns_txs.hrl").
-include_lib("apps/aens/include/aens.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [{group, transactions}]},
     {transactions, [sequence],
      [prune_preclaim,
       prune_claim,
       preclaim,
       preclaim_negative,
       claim,
       claim_negative,
       claim_race_negative,
       update,
       update_negative,
       transfer,
       transfer_negative,
       revoke,
       revoke_negative]}
    ].

-define(NAME, <<"詹姆斯詹姆斯.test"/utf8>>).
-define(PRE_CLAIM_HEIGHT, 1).

%%%===================================================================
%%% Preclaim
%%%===================================================================

preclaim(Cfg) ->
    State = case proplists:get_value(state, Cfg) of
                undefined -> aens_test_utils:new_state();
                State0 -> State0
            end,
    {PubKey, S1} = aens_test_utils:setup_new_account(State),
    PrivKey = aens_test_utils:priv_key(PubKey, S1),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT,
    Name = ?NAME,
    NameSalt = rand:uniform(10000),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash = aens_hash:commitment_hash(NameAscii, NameSalt),

    %% Create Preclaim tx and apply it on trees
    TxSpec = aens_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx} = aens_preclaim_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),
    S2 = aens_test_utils:set_trees(Trees1, S1),

    %% Check commitment created
    Trees2 = aens_test_utils:trees(S2),
    {value, C} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees2)),
    CHash      = aens_commitments:id(C),
    PubKey     = aens_commitments:owner(C),

    {PubKey, Name, NameSalt, S2}.

preclaim_negative(Cfg) ->
    {PubKey, S1} = aens_test_utils:setup_new_account(aens_test_utils:new_state()),
    Trees = aens_test_utils:trees(S1),
    Height = 1,
    {ok, NameAscii} = aens_utils:to_ascii(<<"詹姆斯詹姆斯.test"/utf8>>),
    CHash = aens_hash:commitment_hash(NameAscii, 123),

    %% Test bad account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aens_test_utils:preclaim_tx_spec(BadPubKey, CHash, S1),
    {ok, Tx1} = aens_preclaim_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx2} = aens_preclaim_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:preclaim_tx_spec(PubKey, CHash, #{nonce => 0}, S1),
    {ok, Tx3} = aens_preclaim_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height),

    %% Test commitment already present
    {PubKey2, Name, NameSalt, S3} = preclaim(Cfg),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash2 = aens_hash:commitment_hash(NameAscii, NameSalt),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec4 = aens_test_utils:preclaim_tx_spec(PubKey2, CHash2, S3),
    {ok, Tx4} = aens_preclaim_tx:new(TxSpec4),
    {error, commitment_already_present}
        = aetx:check(Tx4, Trees3, Height),
    ok.

%%%===================================================================
%%% Claim
%%%===================================================================

claim(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash = aens_hash:commitment_hash(NameAscii, NameSalt),
    NHash = aens_hash:name_hash(NameAscii),

    %% Check commitment present
    {value, C} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees)),
    CHash      = aens_commitments:id(C),

    %% Create Claim tx and apply it on trees
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),
    S2 = aens_test_utils:set_trees(Trees1, S1),

    %% Check commitment removed and name entry added
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    none       = aens_state_tree:lookup_commitment(CHash, NTrees),
    {value, N} = aens_state_tree:lookup_name(NHash, NTrees),
    NHash   = aens_names:id(N),
    PubKey  = aens_names:owner(N),
    claimed = aens_names:status(N),
    {PubKey, NHash, S2}.

claim_negative(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT,

    %% Test commitment delta too small
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx0} = aens_claim_tx:new(TxSpec),
    {error, commitment_delta_too_small} =
        aetx:check(Tx0, Trees, Height),

    %% Test bad account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aens_test_utils:claim_tx_spec(BadPubKey, Name, NameSalt, S1),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, #{nonce => 0}, S1),
    {ok, Tx3} = aens_claim_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height),

    %% Test commitment not found
    TxSpec4 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt + 1, S1),
    {ok, Tx4} = aens_claim_tx:new(TxSpec4),
    {error, name_not_preclaimed} =
        aetx:check(Tx4, Trees, Height),

    %% Test commitment not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec5 = aens_test_utils:claim_tx_spec(PubKey2, Name, NameSalt, S3),
    {ok, Tx5} = aens_claim_tx:new(TxSpec5),
    {error, commitment_not_owned} =
        aetx:check(Tx5, Trees3, Height),

    %% Test bad name
    TxSpec6 = aens_test_utils:claim_tx_spec(PubKey, <<"abcdefghi">>, NameSalt, S1),
    {ok, Tx6} = aens_claim_tx:new(TxSpec6),
    {error, no_registrar} =
        aetx:check(Tx6, Trees, Height),
    ok.

claim_race_negative(_Cfg) ->
    %% The first claim
    {_PubKey, _NHash, S1} = claim([]),

    %% The second claim of the same name (hardcoded in preclaim) decomposed
    {PubKey2, Name2, NameSalt2, S2} = preclaim([{state, S1}]),
    Trees = aens_test_utils:trees(S2),
    Height = ?PRE_CLAIM_HEIGHT + 1,

    %% Test bad account key
    TxSpec1 = aens_test_utils:claim_tx_spec(PubKey2, Name2, NameSalt2, S2),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    {error, name_already_taken} = aetx:check(Tx1, Trees, Height).

%%%===================================================================
%%% Update
%%%===================================================================

update(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Check name present, but neither pointers nor name TTL set
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    [] = aens_names:pointers(N),
    0    = aens_names:ttl(N),

    %% Create Update tx and apply it on trees
    Pointers = [{<<"account_pubkey">>, <<"aecore_suite_utils">>}],
    NameTTL  = 90000,
    TxSpec = aens_test_utils:update_tx_spec(
               PubKey, NHash, #{pointers => Pointers, name_ttl => NameTTL}, S1),
    {ok, Tx} = aens_update_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),

    %% Check name present, with both pointers and TTL set
    {value, N1} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees1)),
    Pointers = aens_names:pointers(N1),
    NameTTL  = aens_names:ttl(N1),
    ok.

update_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,

    %% Test TTL too high
    MaxTTL = aec_governance:name_claim_max_expiration(),
    TxSpec1 = aens_test_utils:update_tx_spec(PubKey, NHash, #{ttl => MaxTTL + 1}, S1),
    {ok, Tx1} = aens_update_tx:new(TxSpec1),
    {error, ttl_too_high} =
        aetx:check(Tx1, Trees, Height),

    %% Test bad account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec2 = aens_test_utils:update_tx_spec(BadPubKey, NHash, S1),
    {ok, Tx2} = aens_update_tx:new(TxSpec2),
    {error, account_not_found} =
        aetx:check(Tx2, Trees, Height),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec3 = aens_test_utils:update_tx_spec(PubKey, NHash, S1),
    {ok, Tx3} = aens_update_tx:new(TxSpec3),
    {error, insufficient_funds} =
        aetx:check(Tx3, Trees2, Height),

    %% Test too high account nonce
    TxSpec4 = aens_test_utils:update_tx_spec(PubKey, NHash, #{nonce => 0}, S1),
    {ok, Tx4} = aens_update_tx:new(TxSpec4),
    {error, account_nonce_too_high} =
        aetx:check(Tx4, Trees, Height),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(<<"othername.test">>),
    TxSpec5 = aens_test_utils:update_tx_spec(PubKey, NHash2, S1),
    {ok, Tx5} = aens_update_tx:new(TxSpec5),
    {error, name_does_not_exist} =
        aetx:check(Tx5, Trees, Height),

    %% Test name not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec6 = aens_test_utils:update_tx_spec(PubKey2, NHash, S3),
    {ok, Tx6} = aens_update_tx:new(TxSpec6),
    {error, name_not_owned} =
        aetx:check(Tx6, Trees3, Height),

    %% Test name revoked
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    S4 = aens_test_utils:revoke_name(N, S1),

    TxSpec7 = aens_test_utils:update_tx_spec(PubKey, NHash, S4),
    {ok, Tx7} = aens_update_tx:new(TxSpec7),
    {error, name_revoked} =
        aetx:check(Tx7, aens_test_utils:trees(S4), Height),
    ok.

%%%===================================================================
%%% Transfer
%%%===================================================================

transfer(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Check name present
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    PubKey = aens_names:owner(N),

    %% Create Transfer tx and apply it on trees
    {PubKey2, S2} = aens_test_utils:setup_new_account(S1),
    Trees1 = aens_test_utils:trees(S2),
    TxSpec = aens_test_utils:transfer_tx_spec(
               PubKey, NHash, PubKey2, S1),
    {ok, Tx} = aens_transfer_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees2} = aec_trees:apply_signed_txs([SignedTx], Trees1, Height),

    %% Check name new owner
    {value, N1} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees2)),
    PubKey2 = aens_names:owner(N1),
    ok.

transfer_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,

    %% Test bad account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aens_test_utils:transfer_tx_spec(BadPubKey, NHash, PubKey, S1),
    {ok, Tx1} = aens_transfer_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, S1),
    {ok, Tx2} = aens_transfer_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, #{nonce => 0}, S1),
    {ok, Tx3} = aens_transfer_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(<<"othername.test">>),
    TxSpec4 = aens_test_utils:transfer_tx_spec(PubKey, NHash2, PubKey, S1),
    {ok, Tx4} = aens_transfer_tx:new(TxSpec4),
    {error, name_does_not_exist} =
        aetx:check(Tx4, Trees, Height),

    %% Test name not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec5 = aens_test_utils:transfer_tx_spec(PubKey2, NHash, PubKey, S3),
    {ok, Tx5} = aens_transfer_tx:new(TxSpec5),
    {error, name_not_owned} =
        aetx:check(Tx5, Trees3, Height),

    %% Test name revoked
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    S4 = aens_test_utils:revoke_name(N, S1),

    TxSpec6 = aens_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, S4),
    {ok, Tx6} = aens_transfer_tx:new(TxSpec6),
    {error, name_revoked} =
        aetx:check(Tx6, aens_test_utils:trees(S4), Height),
    ok.

%%%===================================================================
%%% Revoke
%%%===================================================================

revoke(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Check name present
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    claimed = aens_names:status(N),

    %% Create Transfer tx and apply it on trees
    TxSpec = aens_test_utils:revoke_tx_spec(PubKey, NHash, S1),
    {ok, Tx} = aens_revoke_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),

    %% Check name revoked
    {value, N1} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees1)),
    revoked = aens_names:status(N1),
    ok.

revoke_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,

    %% Test bad account key
    BadPubKey = <<42:65/unit:8>>,
    TxSpec1 = aens_test_utils:revoke_tx_spec(BadPubKey, NHash, S1),
    {ok, Tx1} = aens_revoke_tx:new(TxSpec1),
    {error, account_not_found} =
        aetx:check(Tx1, Trees, Height),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:revoke_tx_spec(PubKey, NHash, S1),
    {ok, Tx2} = aens_revoke_tx:new(TxSpec2),
    {error, insufficient_funds} =
        aetx:check(Tx2, Trees2, Height),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:revoke_tx_spec(PubKey, NHash, #{nonce => 0}, S1),
    {ok, Tx3} = aens_revoke_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        aetx:check(Tx3, Trees, Height),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(<<"othername.test">>),
    TxSpec4 = aens_test_utils:revoke_tx_spec(PubKey, NHash2, S1),
    {ok, Tx4} = aens_revoke_tx:new(TxSpec4),
    {error, name_does_not_exist} =
        aetx:check(Tx4, Trees, Height),

    %% Test name not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec5 = aens_test_utils:revoke_tx_spec(PubKey2, NHash, S3),
    {ok, Tx5} = aens_revoke_tx:new(TxSpec5),
    {error, name_not_owned} =
        aetx:check(Tx5, Trees3, Height),

    %% Test name already revoked
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    S4 = aens_test_utils:revoke_name(N, S1),

    TxSpec6 = aens_test_utils:revoke_tx_spec(PubKey, NHash, S4),
    {ok, Tx6} = aens_revoke_tx:new(TxSpec6),
    {error, name_revoked} =
        aetx:check(Tx6, aens_test_utils:trees(S4), Height),
    ok.

%%%===================================================================
%%% Prune names and commitments
%%%===================================================================

prune_preclaim(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash = aens_hash:commitment_hash(NameAscii, NameSalt),
    Trees2 = aens_test_utils:trees(S1),
    {value, C} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees2)),
    CHash      = aens_commitments:id(C),
    PubKey     = aens_commitments:owner(C),

    Expires = aens_commitments:expires(C),
    NSTree = do_prune_until(0, Expires+1, aec_trees:ns(Trees2)),
    none = aens_state_tree:lookup_commitment(CHash, NSTree),
    ok.

prune_claim(Cfg) ->
    {PubKey, NHash, S2} = claim(Cfg),

    %% Re-pull values for this test
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    {value, N} = aens_state_tree:lookup_name(NHash, NTrees),

    NHash    = aens_names:id(N),
    PubKey   = aens_names:owner(N),
    claimed  = aens_names:status(N),
    Expires1 = aens_names:expires(N),


    NTree2 = aens_state_tree:prune(Expires1+1, NTrees),
    {value, N2} = aens_state_tree:lookup_name(NHash, NTree2),
    NHash    = aens_names:id(N2),
    PubKey   = aens_names:owner(N2),
    revoked  = aens_names:status(N2),
    Expires2 = aens_names:expires(N2),

    NTree3 = aens_state_tree:prune(Expires2+1, NTree2),
    none = aens_state_tree:lookup_name(NHash, NTree3),

    {PubKey, NHash, S2}.

do_prune_until(N1, N1, OTree) ->
    aens_state_tree:prune(N1, OTree);
do_prune_until(N1, N2, OTree) ->
    do_prune_until(N1 + 1, N2, aens_state_tree:prune(N1, OTree)).
