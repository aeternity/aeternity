%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    CT test suite for AE Naming System
%%% @end
%%%=============================================================================

-module(aens_SUITE).

%% common_test exports
-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2
        ]).

%% test case exports
-export([preclaim/1,
         prune_claim/1,
         preclaim_negative/1,
         claim/1,
         claim_locked_coins_holder_gets_locked_fee/1,
         claim_negative/1,
         claim_race_negative/1,
         claim_with_auction/1,
         claim_with_auction_negative/1,
         update/1,
         update_negative/1,
         update_after_auction/1,
         transfer/1,
         transfer_negative/1,
         transfer_after_auction/1,
         revoke/1,
         revoke_negative/1,
         revoke_after_auction/1,
         prune_preclaim/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../../aecore/include/blocks.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, no_auction}, {group, auction}].

groups() ->
    [
     {no_auction, [sequence],
      [prune_preclaim,
       prune_claim,
       preclaim,
       preclaim_negative,
       claim,
       claim_locked_coins_holder_gets_locked_fee,
       claim_negative,
       claim_race_negative,
       update,
       update_negative,
       transfer,
       transfer_negative,
       revoke,
       revoke_negative]},
      {auction, [sequence],
       [preclaim,
        claim_with_auction,
        claim_with_auction_negative,
        update_after_auction,
        transfer_after_auction,
        revoke_after_auction]}
    ].

%% NAME_1 len originally was 53 and ascii len is 61
%% Adding xxxx to make it more visible
-define(NAME_1, <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx詹姆斯詹姆斯.test"/utf8>>).
-define(NAME_1_LEN, size(?NAME_1)).
-define(NAME_2, <<"super.test"/utf8>>).
-define(NAME_2_LEN, size(<<"super">>)).
-define(PRE_CLAIM_HEIGHT, 1).


%% Test both flows:
%%  - when name is too long for auction
%%  - when it trggers auction

init_per_group(auction, Config) ->
    [{name, ?NAME_2}| Config];
init_per_group(no_auction, Config) ->
    [{name, ?NAME_1}| Config].

end_per_group(_Group, _Config) ->
    ok.

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
    Name = proplists:get_value(name, Cfg),
    NameSalt = rand:uniform(10000),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash = aens_hash:commitment_hash(NameAscii, NameSalt),

    %% Create Preclaim tx and apply it on trees
    TxSpec = aens_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx} = aens_preclaim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),
    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S2 = aens_test_utils:set_trees(Trees1, S1),

    %% Check commitment created
    Trees2 = aens_test_utils:trees(S2),
    {value, C} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees2)),
    CHash      = aens_commitments:hash(C),
    PubKey     = aens_commitments:owner_pubkey(C),

    {PubKey, Name, NameSalt, S2}.

preclaim_negative(Cfg) ->
    {PubKey, S1} = aens_test_utils:setup_new_account(aens_test_utils:new_state()),
    Trees = aens_test_utils:trees(S1),
    Height = 1,
    Env = aetx_env:tx_env(Height),

    {ok, NameAscii} = aens_utils:to_ascii(?NAME_1),
    CHash = aens_hash:commitment_hash(NameAscii, 123),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aens_test_utils:preclaim_tx_spec(BadPubKey, CHash, S1),
    {ok, Tx1} = aens_preclaim_tx:new(TxSpec1),
    {error, account_not_found} = aetx:process(Tx1, Trees, Env),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx2} = aens_preclaim_tx:new(TxSpec2),
    {error, insufficient_funds} = aetx:process(Tx2, Trees2, Env),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:preclaim_tx_spec(PubKey, CHash, #{nonce => 0}, S1),
    {ok, Tx3} = aens_preclaim_tx:new(TxSpec3),
    {error, account_nonce_too_high} = aetx:process(Tx3, Trees, Env),

    %% Test commitment already present
    {PubKey2, Name, NameSalt, S3} = preclaim(Cfg),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash2 = aens_hash:commitment_hash(NameAscii, NameSalt),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec4 = aens_test_utils:preclaim_tx_spec(PubKey2, CHash2, S3),
    {ok, Tx4} = aens_preclaim_tx:new(TxSpec4),
    {error, commitment_already_present} = aetx:process(Tx4, Trees3, Env),
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
    CHash      = aens_commitments:hash(C),

    %% Create Claim tx and apply it on trees
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),

    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S2 = aens_test_utils:set_trees(Trees1, S1),

    %% Check commitment removed and name entry added
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    none       = aens_state_tree:lookup_commitment(CHash, NTrees),
    {value, N} = aens_state_tree:lookup_name(NHash, NTrees),
    NHash   = aens_names:hash(N),
    PubKey  = aens_names:owner_pubkey(N),
    claimed = aens_names:status(N),
    {PubKey, NHash, S2}.

commitment_check({_PubKey1, Name, NameSalt, S1}) ->
    %% Check commitment present
    Trees1 = aens_test_utils:trees(S1),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash = aens_hash:commitment_hash(NameAscii, NameSalt),
    NHash = aens_hash:name_hash(NameAscii),
    {value, C0} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees1)),
    auction_ready = aens_commitments:get_name_auction_state(C0, Name),
    CHash      = aens_commitments:hash(C0),
    {CHash, NHash}.

claim_with_auction_negative(Cfg) ->
    PrevOut = {PubKey1, Name, NameSalt, S1} = preclaim(Cfg),
    {CHash, NHash} = commitment_check(PrevOut),

    Trees1 = aens_test_utils:trees(S1),

    %% Name example for auction test has 5 characters
    ?assertEqual(?NAME_2_LEN, aens_commitments:name_length(Name)),
    MinFeeForLength = aec_governance:name_claim_fee(?NAME_2_LEN),

    Height = ?PRE_CLAIM_HEIGHT + 1,
    PrivKey1 = aens_test_utils:priv_key(PubKey1, S1),

    %% ========================================================================
    %% Proceed with 1st Bid when account doesn't have enough funds

    TxSpec0 = aens_test_utils:claim_tx_spec(PubKey1, Name, MinFeeForLength, NameSalt, S1),
    {ok, Tx0} = aens_claim_tx:new(TxSpec0),
    Env      = aetx_env:tx_env(Height),
    {error, insufficient_funds} = aetx:process(Tx0, Trees1, Env),

    %% ========================================================================
    %% Proceed with 1st Bid when NameFee is explicitely bellow gov bid value

    TxSpec00 = aens_test_utils:claim_tx_spec(PubKey1, Name, MinFeeForLength-1, NameSalt, S1),
    {ok, Tx00} = aens_claim_tx:new(TxSpec00),
    {error, too_small_bid} = aetx:process(Tx00, Trees1, Env),

    %% Fix the PubKey1 account and re-run the 1st Bid
    S2 = aens_test_utils:set_account_balance(PubKey1, MinFeeForLength*10, S1),
    Bid1 = aec_test_utils:sign_tx(Tx0, PrivKey1),

    Trees2 = aens_test_utils:trees(S2),
    {ok, [Bid1], Trees3, _} =
        aec_block_micro_candidate:apply_block_txs([Bid1], Trees2, Env),
    S3 = aens_test_utils:set_trees(Trees3, S2),

    %% ========================================================================
    %% Proceed with 2nd Bid which is too late

    {PubKey2, S4} = aens_test_utils:setup_new_account(S3),
    PrivKey2 = aens_test_utils:priv_key(PubKey2, S4),
    S5 = aens_test_utils:set_account_balance(PubKey2, MinFeeForLength*10, S4),

    ProgressionGov = aec_governance:name_claim_bid_increment(),
    ChangeGov1 = MinFeeForLength * ProgressionGov div 100,
    MinNextBid1 = MinFeeForLength + ChangeGov1,

    TxSpec1 = aens_test_utils:claim_tx_spec(PubKey2, Name, MinNextBid1, NameSalt, S5),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    Bid2 = aec_test_utils:sign_tx(Tx1, PrivKey2),

    WrongHeight = ?PRE_CLAIM_HEIGHT + 1 + aec_governance:name_claim_bid_timeout(?NAME_2_LEN) + 1,
    Env2      = aetx_env:tx_env(WrongHeight),
    {error, too_late_for_bid} = aetx:process(Tx1, aens_test_utils:trees(S5), Env2),

    GoodHeight = ?PRE_CLAIM_HEIGHT + 1 + aec_governance:name_claim_bid_timeout(?NAME_2_LEN),
    Env3      = aetx_env:tx_env(GoodHeight),

    Trees4 = aens_test_utils:trees(S5),
    {ok, [Bid2], Trees5, _} =
        aec_block_micro_candidate:apply_block_txs([Bid2], Trees4, Env3),
    S6 = aens_test_utils:set_trees(Trees5, S5),

    %% ========================================================================
    %% Proceed with 3rd Bid which is too cheap (gov bid min increment not met)

    ChangeGov2 = MinNextBid1 * ProgressionGov div 100,
    MinNextBid2 = MinNextBid1 + ChangeGov2 - 1,

    TxSpec2 = aens_test_utils:claim_tx_spec(PubKey1, Name, MinNextBid2, NameSalt, S6),
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    Env4      = aetx_env:tx_env(GoodHeight),
    {error, bid_below_progression} = aetx:process(Tx2, aens_test_utils:trees(S6), Env4),

    %% ========================================================================
    %% Proceed with 3rd Bid which is from currently owning user

    TxSpec3 = aens_test_utils:claim_tx_spec(PubKey2, Name, MinNextBid2+1, NameSalt, S6),
    {ok, Tx3} = aens_claim_tx:new(TxSpec3),
    {error, commitment_already_owned} = aetx:process(Tx3, aens_test_utils:trees(S6), Env4),
    ok.

claim_with_auction(Cfg) ->
    PrevOut = {PubKey1, Name, NameSalt, S1} = preclaim(Cfg),
    {CHash, NHash} = commitment_check(PrevOut),

    %% Name example for auction test has 5 characters
    ?assertEqual(?NAME_2_LEN, aens_commitments:name_length(Name)),
    MinFeeForLength = aec_governance:name_claim_fee(?NAME_2_LEN),

    Height = ?PRE_CLAIM_HEIGHT + 1,
    PrivKey = aens_test_utils:priv_key(PubKey1, S1),

    %% ========================================================================
    %% Proceed with 1st Bid
    TxSpec0 = aens_test_utils:claim_tx_spec(PubKey1, Name, MinFeeForLength, NameSalt, S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec0),
    Bid1 = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),
    S2 = aens_test_utils:set_account_balance(PubKey1, MinFeeForLength*10, S1),
    Account1Bal1 = aens_test_utils:get_balance(PubKey1, S2),

    LockedAmount1 = aens_test_utils:locked_coins_balance(S2),
    ?assertEqual(0, LockedAmount1),

    Trees2 = aens_test_utils:trees(S2),
    {ok, [Bid1], Trees3, _} =
        aec_block_micro_candidate:apply_block_txs([Bid1], Trees2, Env),
    S3 = aens_test_utils:set_trees(Trees3, S2),

    %% Post bid checks:
    NTrees1 = aec_trees:ns(Trees3),
    {value, C1}       = aens_state_tree:lookup_commitment(CHash, NTrees1),
    auction_ongoing = aens_commitments:get_name_auction_state(C1, Name),
    none = aens_state_tree:lookup_name(NHash, NTrees1),

    Account1Bal2 = aens_test_utils:get_balance(PubKey1, S3),
    LockedAmount2 = aens_test_utils:locked_coins_balance(S3),

    TxFee = aens_test_utils:claim_tx_defaul_fee(),
    ?assertEqual(Account1Bal1-MinFeeForLength-TxFee, Account1Bal2),
    ?assertEqual(MinFeeForLength, LockedAmount2), %% INFO: No TxFee handling here

    %% ========================================================================
    %% Proceed with 2nd Bid
    {PubKey2, S4} = aens_test_utils:setup_new_account(S3),
    PrivKey2 = aens_test_utils:priv_key(PubKey2, S4),
    S5 = aens_test_utils:set_account_balance(PubKey2, MinFeeForLength*10, S4),

    Account2Bal1 = aens_test_utils:get_balance(PubKey2, S5),

    %% Price progression (maybe hardcode values?)
    ProgressionGov = aec_governance:name_claim_bid_increment(),
    ChangeGov = MinFeeForLength * ProgressionGov div 100,
    MinNextBid1 = MinFeeForLength + ChangeGov,

    TxSpec1 = aens_test_utils:claim_tx_spec(PubKey2, Name, MinNextBid1, NameSalt, S5),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    Bid2 = aec_test_utils:sign_tx(Tx1, PrivKey2),
    Env2      = aetx_env:tx_env(Height+1),
    Trees4 = aens_test_utils:trees(S5),
    {ok, [Bid2], Trees5, _} =
        aec_block_micro_candidate:apply_block_txs([Bid2], Trees4, Env2),
    S6 = aens_test_utils:set_trees(Trees5, S5),

    Account1Bal3 = aens_test_utils:get_balance(PubKey1, S6),
    Account2Bal2 = aens_test_utils:get_balance(PubKey2, S6),
    LockedAmount3 = aens_test_utils:locked_coins_balance(S6),

    ?assertEqual(Account1Bal3, Account1Bal2+MinFeeForLength),
    ?assertEqual(Account2Bal1-MinNextBid1-TxFee, Account2Bal2),
    ?assertEqual(LockedAmount2-MinFeeForLength+MinNextBid1, LockedAmount3),

    %% Post bid checks
    NTrees2 = aec_trees:ns(Trees5),
    {value, C2}       = aens_state_tree:lookup_commitment(CHash, NTrees2),
    PubKey2 = aens_commitments:owner_pubkey(C2),
    auction_ongoing = aens_commitments:get_name_auction_state(C2, Name),
    none = aens_state_tree:lookup_name(NHash, NTrees2),

    %% ========================================================================
    %% Proceed with 3rd Bid, larger bid + longer wait
    {PubKey3, S7} = aens_test_utils:setup_new_account(S6),
    PrivKey3 = aens_test_utils:priv_key(PubKey3, S7),
    S8 = aens_test_utils:set_account_balance(PubKey3, MinFeeForLength*10, S7),

    Account3Bal1 = aens_test_utils:get_balance(PubKey3, S8),

    ThirdBid = MinNextBid1*2,
    TxSpec2 = aens_test_utils:claim_tx_spec(PubKey3, Name, ThirdBid, NameSalt, S8),
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    Bid3 = aec_test_utils:sign_tx(Tx2, PrivKey3),
    Env3      = aetx_env:tx_env(Height+960),  %% two days bid delta
    Trees6 = aens_test_utils:trees(S8),
    {ok, [Bid3], Trees7, _} =
        aec_block_micro_candidate:apply_block_txs([Bid3], Trees6, Env3),
    S9 = aens_test_utils:set_trees(Trees7, S8),

    Account3Bal2 = aens_test_utils:get_balance(PubKey3, S9),
    Account2Bal3 = aens_test_utils:get_balance(PubKey2, S9),
    LockedAmount4 = aens_test_utils:locked_coins_balance(S9),

    ?assertEqual(Account3Bal1-ThirdBid-TxFee, Account3Bal2),
    ?assertEqual(Account2Bal3, Account2Bal2+MinNextBid1),
    ?assertEqual(LockedAmount3-MinNextBid1+ThirdBid, LockedAmount4),

    %% Post bid checks
    NTrees3 = aec_trees:ns(Trees7),
    {value, C3}       = aens_state_tree:lookup_commitment(CHash, NTrees3),
    PubKey3 = aens_commitments:owner_pubkey(C3),
    auction_ongoing = aens_commitments:get_name_auction_state(C3, Name),
    none = aens_state_tree:lookup_name(NHash, NTrees3),


    %% ========================================================================
    %% Run prune
    BidExpiration = aens_commitments:ttl(C3),
    GenesisHeight = aec_block_genesis:height(),
    Trees8 = do_prune_until(GenesisHeight, BidExpiration + 1, Trees7),

    %% The first prune run to check if name is instatiated
    none = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees8)),
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees8)),
    NHash   = aens_names:hash(N),
    PubKey3  = aens_names:owner_pubkey(N),
    claimed = aens_names:status(N),

    %% State with name for further testing
    S10 = aens_test_utils:set_trees(Trees8, S9),

    %% The second prune run to check if name is garbage collected
    NameRentTime = aec_governance:name_claim_max_expiration(),
    Trees9 = do_prune_until(GenesisHeight, BidExpiration + NameRentTime + 1, Trees8),
    none = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees9)),
    S11 = aens_test_utils:set_trees(Trees9, S10),

    %% Nothing should change from the last interaction
    Account1Final = aens_test_utils:get_balance(PubKey1, S11),
    Account2Final = aens_test_utils:get_balance(PubKey2, S11),
    Account3Final = aens_test_utils:get_balance(PubKey3, S11),
    ?assertEqual(Account1Bal3, Account1Final),
    ?assertEqual(Account2Bal3, Account2Final),
    ?assertEqual(Account3Bal2, Account3Final),

    {PubKey3, NHash, S10}.


claim_locked_coins_holder_gets_locked_fee(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Create Claim tx and apply it on trees
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),

    LockedCoinsHolderPubKey = aec_governance:locked_coins_holder_account(),
    LockedCoinsFee          = aec_governance:name_claim_fee(?NAME_1_LEN),

    %% Locked coins holder is not present in state tree
    none = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees)),

    %% Apply claim tx, and verify locked coins holder got locked coins
    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    {value, Account1} = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees1)),
    LockedCoinsFee    = aec_accounts:balance(Account1),

    %% Locked coins holder has some funds
    S2 = aens_test_utils:set_account_balance(LockedCoinsHolderPubKey, 500, S1),
    Trees2 = aens_test_utils:trees(S2),
    {value, Account2} = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees2)),

    %% Apply claim tx, and verify locked coins holder got locked coins
    {ok, [SignedTx], Trees3, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees2, Env),
    {value, Account3} = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees3)),
    LockedCoinsFee = aec_accounts:balance(Account3) - aec_accounts:balance(Account2),
    ok.

claim_negative(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT,
    Env = aetx_env:tx_env(Height),

    %% Test commitment delta too small
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx0} = aens_claim_tx:new(TxSpec),
    {error, commitment_delta_too_small} = aetx:process(Tx0, Trees, Env),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aens_test_utils:claim_tx_spec(BadPubKey, Name, NameSalt, S1),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    {error, account_not_found} = aetx:process(Tx1, Trees, Env),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    {error, insufficient_funds} = aetx:process(Tx2, Trees2, Env),

    %% Test too high account nonce
    NameFee = aec_governance:name_claim_fee_base(),
    TxSpec3 = aens_test_utils:claim_tx_spec(PubKey, Name, NameFee, NameSalt, #{nonce => 0}, S1),
    {ok, Tx3} = aens_claim_tx:new(TxSpec3),
    {error, account_nonce_too_high} = aetx:process(Tx3, Trees, Env),

    %% Test commitment not found
    TxSpec4 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt + 1, S1),
    {ok, Tx4} = aens_claim_tx:new(TxSpec4),
    {error, name_not_preclaimed} = aetx:process(Tx4, Trees, Env),

    %% Test commitment not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec5 = aens_test_utils:claim_tx_spec(PubKey2, Name, NameSalt, S3),
    {ok, Tx5} = aens_claim_tx:new(TxSpec5),
    {error, commitment_not_owned} = aetx:process(Tx5, Trees3, Env),

    %% Test bad name
    TxSpec6 = aens_test_utils:claim_tx_spec(PubKey, <<"abcdefghi">>, NameSalt, S1),
    {ok, Tx6} = aens_claim_tx:new(TxSpec6),
    {error, no_registrar} = aetx:process(Tx6, Trees, Env),
    ok.

claim_race_negative(Cfg) ->
    %% The first claim
    {_PubKey, _NHash, S1} = claim([{name, proplists:get_value(name, Cfg)}]),

    %% The second claim of the same name (hardcoded in preclaim) decomposed
    {PubKey2, Name2, NameSalt2, S2} = preclaim([{name, proplists:get_value(name, Cfg)},
                                                {state, S1}]),
    Trees = aens_test_utils:trees(S2),
    Height = ?PRE_CLAIM_HEIGHT + 1,

    %% Test bad account key
    TxSpec1 = aens_test_utils:claim_tx_spec(PubKey2, Name2, NameSalt2, S2),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    Env = aetx_env:tx_env(Height),
    {error, name_already_taken} = aetx:process(Tx1, Trees, Env).

%%%===================================================================
%%% Update
%%%===================================================================

update(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    do_update({PubKey, NHash, S1}).

update_after_auction(Cfg) ->
    {PubKey, NHash, S1} = claim_with_auction(Cfg),
    do_update({PubKey, NHash, S1}).

do_update({PubKey, NHash, S1}) ->
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Check name present, but neither pointers nor name TTL set
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    [] = aens_names:pointers(N),
    0  = aens_names:client_ttl(N),

    %% Create Update tx and apply it on trees
    Pointers = [aens_pointer:new(<<"account_pubkey">>, aeser_id:create(account, <<1:256>>))],
    NameTTL  = 40000,
    TxSpec = aens_test_utils:update_tx_spec(
               PubKey, NHash, #{pointers => Pointers, name_ttl => NameTTL}, S1),
    {ok, Tx} = aens_update_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),
    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),

    %% Check name present, with both pointers and TTL set
    {value, N1} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees1)),
    Pointers = aens_names:pointers(N1),
    NameTTL  = aens_names:ttl(N1) - Height,
    ok.

update_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    Env = aetx_env:tx_env(Height),

    %% Test TX TTL too low
    MaxTTL = aec_governance:name_claim_max_expiration(),
    TxSpec0 = aens_test_utils:update_tx_spec(PubKey, NHash, #{ttl => Height - 1}, S1),
    {ok, Tx0} = aens_update_tx:new(TxSpec0),
    {error, ttl_expired} = aetx:process(Tx0, Trees, Env),

    %% Test name TTL too high
    MaxTTL = aec_governance:name_claim_max_expiration(),
    TxSpec1 = aens_test_utils:update_tx_spec(PubKey, NHash, #{name_ttl => MaxTTL + 1}, S1),
    {ok, Tx1} = aens_update_tx:new(TxSpec1),
    {error, ttl_too_high} = aetx:process(Tx1, Trees, Env),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec2 = aens_test_utils:update_tx_spec(BadPubKey, NHash, S1),
    {ok, Tx2} = aens_update_tx:new(TxSpec2),
    {error, account_not_found} = aetx:process(Tx2, Trees, Env),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec3 = aens_test_utils:update_tx_spec(PubKey, NHash, S1),
    {ok, Tx3} = aens_update_tx:new(TxSpec3),
    {error, insufficient_funds} = aetx:process(Tx3, Trees2, Env),

    %% Test too high account nonce
    TxSpec4 = aens_test_utils:update_tx_spec(PubKey, NHash, #{nonce => 0}, S1),
    {ok, Tx4} = aens_update_tx:new(TxSpec4),
    {error, account_nonce_too_high} = aetx:process(Tx4, Trees, Env),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(<<"othername.test">>),
    TxSpec5 = aens_test_utils:update_tx_spec(PubKey, NHash2, S1),
    {ok, Tx5} = aens_update_tx:new(TxSpec5),
    {error, name_does_not_exist} = aetx:process(Tx5, Trees, Env),

    %% Test name not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec6 = aens_test_utils:update_tx_spec(PubKey2, NHash, S3),
    {ok, Tx6} = aens_update_tx:new(TxSpec6),
    {error, name_not_owned} = aetx:process(Tx6, Trees3, Env),

    %% Test name revoked
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    S4 = aens_test_utils:revoke_name(N, S1),

    TxSpec7 = aens_test_utils:update_tx_spec(PubKey, NHash, S4),
    {ok, Tx7} = aens_update_tx:new(TxSpec7),
    {error, name_revoked} =
        aetx:process(Tx7, aens_test_utils:trees(S4), Env),
    ok.

%%%===================================================================
%%% Transfer
%%%===================================================================

transfer(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    do_transfer({PubKey, NHash, S1}).

transfer_after_auction(Cfg) ->
    {PubKey, NHash, S1} = claim_with_auction(Cfg),
    do_transfer({PubKey, NHash, S1}).

do_transfer({PubKey, NHash, S1}) ->
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Check name present
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    PubKey = aens_names:owner_pubkey(N),

    %% Create Transfer tx and apply it on trees
    {PubKey2, S2} = aens_test_utils:setup_new_account(S1),
    Trees1 = aens_test_utils:trees(S2),
    TxSpec = aens_test_utils:transfer_tx_spec(
               PubKey, NHash, PubKey2, S1),
    {ok, Tx} = aens_transfer_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),
    {ok, [SignedTx], Trees2, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees1, Env),

    %% Check name new owner
    {value, N1} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees2)),
    PubKey2 = aens_names:owner_pubkey(N1),
    ok.

transfer_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    Env = aetx_env:tx_env(Height),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aens_test_utils:transfer_tx_spec(BadPubKey, NHash, PubKey, S1),
    {ok, Tx1} = aens_transfer_tx:new(TxSpec1),
    {error, account_not_found} = aetx:process(Tx1, Trees, Env),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, S1),
    {ok, Tx2} = aens_transfer_tx:new(TxSpec2),
    {error, insufficient_funds} = aetx:process(Tx2, Trees2, Env),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, #{nonce => 0}, S1),
    {ok, Tx3} = aens_transfer_tx:new(TxSpec3),
    {error, account_nonce_too_high} = aetx:process(Tx3, Trees, Env),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(<<"othername.test">>),
    TxSpec4 = aens_test_utils:transfer_tx_spec(PubKey, NHash2, PubKey, S1),
    {ok, Tx4} = aens_transfer_tx:new(TxSpec4),
    {error, name_does_not_exist} = aetx:process(Tx4, Trees, Env),

    %% Test name not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec5 = aens_test_utils:transfer_tx_spec(PubKey2, NHash, PubKey, S3),
    {ok, Tx5} = aens_transfer_tx:new(TxSpec5),
    {error, name_not_owned} = aetx:process(Tx5, Trees3, Env),

    %% Test name revoked
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    S4 = aens_test_utils:revoke_name(N, S1),

    TxSpec6 = aens_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, S4),
    {ok, Tx6} = aens_transfer_tx:new(TxSpec6),
    {error, name_revoked} =
        aetx:process(Tx6, aens_test_utils:trees(S4), Env),
    ok.

%%%===================================================================
%%% Revoke
%%%===================================================================

revoke(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    do_revoke({PubKey, NHash, S1}).

revoke_after_auction(Cfg) ->
    {PubKey, NHash, S1} = claim_with_auction(Cfg),
    do_revoke({PubKey, NHash, S1}).

do_revoke({PubKey, NHash, S1}) ->
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Check name present
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    claimed = aens_names:status(N),

    %% Create Transfer tx and apply it on trees
    TxSpec = aens_test_utils:revoke_tx_spec(PubKey, NHash, S1),
    {ok, Tx} = aens_revoke_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),
    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),

    %% Check name revoked
    {value, N1} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees1)),
    revoked = aens_names:status(N1),
    ok.

revoke_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    Env = aetx_env:tx_env(Height),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aens_test_utils:revoke_tx_spec(BadPubKey, NHash, S1),
    {ok, Tx1} = aens_revoke_tx:new(TxSpec1),
    {error, account_not_found} = aetx:process(Tx1, Trees, Env),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:revoke_tx_spec(PubKey, NHash, S1),
    {ok, Tx2} = aens_revoke_tx:new(TxSpec2),
    {error, insufficient_funds} = aetx:process(Tx2, Trees2, Env),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:revoke_tx_spec(PubKey, NHash, #{nonce => 0}, S1),
    {ok, Tx3} = aens_revoke_tx:new(TxSpec3),
    {error, account_nonce_too_high} = aetx:process(Tx3, Trees, Env),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(<<"othername.test">>),
    TxSpec4 = aens_test_utils:revoke_tx_spec(PubKey, NHash2, S1),
    {ok, Tx4} = aens_revoke_tx:new(TxSpec4),
    {error, name_does_not_exist} = aetx:process(Tx4, Trees, Env),

    %% Test name not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec5 = aens_test_utils:revoke_tx_spec(PubKey2, NHash, S3),
    {ok, Tx5} = aens_revoke_tx:new(TxSpec5),
    {error, name_not_owned} = aetx:process(Tx5, Trees3, Env),

    %% Test name already revoked
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    S4 = aens_test_utils:revoke_name(N, S1),

    TxSpec6 = aens_test_utils:revoke_tx_spec(PubKey, NHash, S4),
    {ok, Tx6} = aens_revoke_tx:new(TxSpec6),
    {error, name_revoked} =
        aetx:process(Tx6, aens_test_utils:trees(S4), Env),
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
    CHash      = aens_commitments:hash(C),
    PubKey     = aens_commitments:owner_pubkey(C),

    TTL = aens_commitments:ttl(C),
    GenesisHeight = aec_block_genesis:height(),
    Trees3 = do_prune_until(GenesisHeight, TTL + 1, Trees2),
    none = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(Trees3)),
    ok.

prune_claim(Cfg) ->
    {PubKey, NHash, S2} = claim(Cfg),

    %% Re-pull values for this test
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    {value, N} = aens_state_tree:lookup_name(NHash, NTrees),

    NHash    = aens_names:hash(N),
    PubKey   = aens_names:owner_pubkey(N),
    claimed  = aens_names:status(N),
    TTL1     = aens_names:ttl(N),


    Trees3 = aens_state_tree:prune(TTL1+1, Trees2),
    NTree2 = aec_trees:ns(Trees3),
    {value, N2} = aens_state_tree:lookup_name(NHash, NTree2),
    NHash    = aens_names:hash(N2),
    PubKey   = aens_names:owner_pubkey(N2),
    revoked  = aens_names:status(N2),
    TTL2     = aens_names:ttl(N2),

    Trees4 = aens_state_tree:prune(TTL2+1, Trees3),
    NTree3 = aec_trees:ns(Trees4),
    none = aens_state_tree:lookup_name(NHash, NTree3),

    {PubKey, NHash, S2}.

do_prune_until(N1, N1, OTree) ->
    aens_state_tree:prune(N1, OTree);
do_prune_until(N1, N2, OTree) ->
    do_prune_until(N1 + 1, N2, aens_state_tree:prune(N1, OTree)).
