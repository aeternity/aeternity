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
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2
        ]).

%% test case exports
-export([preclaim/1,
         prune_claim/1,
         prune_claim_auction/1,
         preclaim_negative/1,
         claim/1,
         claim_auction/1,
         ongoing_auction/1,
         claim_locked_coins_holder_gets_locked_fee/1,
         claim_negative/1,
         claim_empty_name/1,
         claim_race_negative/1,
         claim_race_negative_auction/1,
         update/1,
         update_negative/1,
         update_expire_at_once/1,
         transfer/1,
         transfer_negative/1,
         revoke/1,
         revoke_negative/1,
         prune_preclaim/1,
         registrar_change/1]).

-include_lib("common_test/include/ct.hrl").
-include("../../aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("stdlib/include/assert.hrl").


%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, transactions},
     {group, auction},
     {group, no_auction_long_names}].

groups() ->
    [
     {transactions, [sequence],
      [prune_preclaim,
       prune_claim,
       preclaim,
       preclaim_negative,
       claim,
       claim_locked_coins_holder_gets_locked_fee,
       claim_negative,
       claim_empty_name,
       claim_race_negative,
       update,
       update_negative,
       update_expire_at_once,
       transfer,
       transfer_negative,
       revoke,
       revoke_negative,
       registrar_change]},
     {no_auction_long_names,
      [claim,
       claim_locked_coins_holder_gets_locked_fee,
       claim_negative,
       claim_race_negative]},
     {auction,
      [prune_preclaim,
       prune_claim_auction,
       preclaim,
       claim_auction,
       ongoing_auction,
       claim_locked_coins_holder_gets_locked_fee,
       claim_race_negative_auction]}
    ].

-define(NAME, <<"詹姆斯詹姆斯"/utf8>>).
-define(PRE_CLAIM_HEIGHT, 1).

init_per_suite(Config) ->
    aec_test_utils:ensure_no_mocks(),
    Config.

end_per_suite(_Config) ->
    aec_test_utils:ensure_no_mocks().

init_per_group(transactions, Cfg) ->
    %% Disable name auction
    meck:expect(aec_governance, name_claim_bid_timeout, fun(_, _) -> 0 end),
    %% dependening on how we call 'make' we get different protocols
    [{protocol, aec_hard_forks:protocol_effective_at_height(1)},
     {name, ?NAME} | Cfg];
init_per_group(no_auction_long_names, Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    [{protocol, Protocol}, {name, gen_name(aec_governance:name_max_length_starting_auction()+1)} | Cfg];
init_per_group(_, Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    %% One day auction open with cheapest possible name (31 char)
    if Protocol < ?LIMA_PROTOCOL_VSN -> {skip, no_auction_before_lima};
       true -> [{protocol, Protocol}, {name, gen_name(aec_governance:name_max_length_starting_auction())} | Cfg]
    end.

end_per_group(transactions, Cfg) ->
    meck:unload(aec_governance),
    Cfg;
end_per_group(_, Cfg) ->
    Cfg.

%%%===================================================================
%%% Init helper
%%%===================================================================

gen_name(Length) ->
    <<<<"x">>||_<-lists:seq(1,Length)>>.

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
    Name = aens_test_utils:fullname(?config(name, Cfg)),
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

    {ok, NameAscii} = aens_utils:to_ascii(aens_test_utils:fullname(<<"詹姆斯詹姆斯"/utf8>>)),
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
    {error, tx_nonce_already_used_for_account} = aetx:process(Tx3, Trees, Env),

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
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(NameAscii, Cfg), S1),
    ct:pal("TxSpec ~p\n", [TxSpec]),
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

claim_auction(Cfg) ->
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
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(NameAscii, Cfg), S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),

    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S2 = aens_test_utils:set_trees(Trees1, S1),

    %% Check commitment removed and name entry not present and auction entry added
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    none   = aens_state_tree:lookup_commitment(CHash, NTrees),
    none   = aens_state_tree:lookup_name(NHash, NTrees),
    {value, A} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTrees),
    ct:pal("Auction started ~p", [A]),
    NHash   = aens_hash:from_auction_hash(aens_auctions:hash(A)),
    PubKey  = aens_auctions:bidder_pubkey(A),
    {PubKey, NHash, S2}.

ongoing_auction(Cfg) ->
    {PubKey2, Name, _Salt, S0} = preclaim(Cfg),

    {PubKey, NHash, S1} = claim_auction([{state, S0} | Cfg]),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    %% PrivKey = aens_test_utils:priv_key(PubKey, S1),
    PrivKey2 = aens_test_utils:priv_key(PubKey2, S1),
    {value, Account1} = aec_accounts_trees:lookup(PubKey, aec_trees:accounts(Trees)),
    BalanceWithoutNameFee  = aec_accounts:balance(Account1),

    %% Bidding over via Claim tx and apply it on trees
    TxSpec = aens_test_utils:claim_tx_spec(PubKey2, Name, 0, 2*namefee(Name, Cfg), S1),
    ct:pal("TxSpec ~p\n", [TxSpec]),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey2),
    Env      = aetx_env:tx_env(Height),

    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    S2 = aens_test_utils:set_trees(Trees1, S1),

    %% Check commitment removed and name entry not present and auction entry added
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    none   = aens_state_tree:lookup_name(NHash, NTrees),
    {value, A} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTrees),
    ct:pal("Auction ongoing ~p", [A]),
    NHash   = aens_hash:from_auction_hash(aens_auctions:hash(A)),
    PubKey2  = aens_auctions:bidder_pubkey(A),

    %% First bidder gets fee returned
    {value, NewAccount1} = aec_accounts_trees:lookup(PubKey, aec_trees:accounts(Trees2)),
    BalanceWithoutNameFee = aec_accounts:balance(NewAccount1) - namefee(Name, Cfg),

    {PubKey2, NHash, S2}.


claim_locked_coins_holder_gets_locked_fee(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Create Claim tx and apply it on trees
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(Name, Cfg), S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),

    LockedCoinsHolderPubKey = aec_governance:locked_coins_holder_account(),
    Protocol = ?config(protocol, Cfg),
    LockedCoinsFee          =
        case Protocol >= ?LIMA_PROTOCOL_VSN of
            false -> aec_governance:name_claim_locked_fee();
            true -> namefee(Name, Cfg)
        end,

    %% Locked coins holder is not present in state tree
    none = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees)),

    %% Apply claim tx, and verify locked coins holder got locked coins
    {ok, [SignedTx], Trees0, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),
    BidTimeout = aec_governance:name_claim_bid_timeout(Name, Protocol),
    Trees1 = perform_pre_transformations(Trees0, Height + BidTimeout + 1),
    {value, Account1} = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees1)),
    LockedCoinsFee    = aec_accounts:balance(Account1),

    %% Locked coins holder has some funds
    S2 = aens_test_utils:set_account_balance(LockedCoinsHolderPubKey, 500, S1),
    Trees2 = aens_test_utils:trees(S2),
    {value, Account2} = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees2)),

    %% Apply claim tx, and verify locked coins holder got locked coins
    {ok, [SignedTx], Trees3, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees2, Env),
    Trees4 = perform_pre_transformations(Trees3, Height + BidTimeout + 1),
    {value, Account3} = aec_accounts_trees:lookup(LockedCoinsHolderPubKey, aec_trees:accounts(Trees4)),
    LockedCoinsFee = aec_accounts:balance(Account3) - aec_accounts:balance(Account2),
    ok.

claim_negative(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT,
    Env0 = aetx_env:tx_env(Height),

    %% Test commitment delta too small
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(Name, Cfg), S1),
    {ok, Tx0} = aens_claim_tx:new(TxSpec),
    {error, commitment_delta_too_small} = aetx:process(Tx0, Trees, Env0),

    %% Bump height to pass commitment delta
    %% Additional errors most interesting when we potentially could claim
    Env      = aetx_env:tx_env(Height + 1),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = aens_test_utils:claim_tx_spec(BadPubKey, Name, NameSalt, namefee(Name, Cfg), S1),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    {error, account_not_found} = aetx:process(Tx1, Trees, Env),

    %% Insufficient funds
    S2 = aens_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aens_test_utils:trees(S2),
    TxSpec2 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(Name, Cfg), S1),
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    {error, insufficient_funds} = aetx:process(Tx2, Trees2, Env),

    %% Test too high account nonce
    TxSpec3 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(Name, Cfg), #{nonce => 0}, S1),
    {ok, Tx3} = aens_claim_tx:new(TxSpec3),
    {error, tx_nonce_already_used_for_account} = aetx:process(Tx3, Trees, Env),

    %% Test commitment not found
    TxSpec4 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt + 1, namefee(Name, Cfg), S1),
    {ok, Tx4} = aens_claim_tx:new(TxSpec4),
    {error, name_not_preclaimed} = aetx:process(Tx4, Trees, Env),

    %% Test commitment not owned
    {PubKey2, S3} = aens_test_utils:setup_new_account(S1),
    Trees3 = aens_test_utils:trees(S3),
    TxSpec5 = aens_test_utils:claim_tx_spec(PubKey2, Name, NameSalt, namefee(Name, Cfg), S3),
    {ok, Tx5} = aens_claim_tx:new(TxSpec5),
    {error, commitment_not_owned} = aetx:process(Tx5, Trees3, Env),

    %% Test bad name
    TxSpec6 = aens_test_utils:claim_tx_spec(PubKey, <<"abcdefghi">>, NameSalt, namefee(Name, Cfg), S1),
    {ok, Tx6} = aens_claim_tx:new(TxSpec6),
    {error, no_registrar} = aetx:process(Tx6, Trees, Env),

    %% Test name fee too low (for pre lima fees are not allowed, for Lima only fee >= 0 allowed)
    TxSpec7 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, 0, S1),
    {ok, Tx7} = aens_claim_tx:new(TxSpec7),
    case ?config(protocol, Cfg) >= ?LIMA_PROTOCOL_VSN of
        true ->
            {error, illegal_name_fee} = aetx:process(Tx7, Trees, Env);
        false ->
            {error, invalid_at_protocol} = aetx:process(Tx7, Trees, Env)

    end,

    %% Test name fee too low (for pre lima fees are not allowed, for Lima too low)
    {ok, AsciiName} = aens_utils:to_ascii(Name), %% Fee is based upon Ascii version of name
    NameFee = max(0, aec_governance:name_claim_fee(AsciiName, ?config(protocol, Cfg)) - 1),
    ct:pal("NameFee ~p", [NameFee]),
    TxSpec8 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, NameFee, S1),
    {ok, Tx8} = aens_claim_tx:new(TxSpec8),
    case ?config(protocol, Cfg) >= ?LIMA_PROTOCOL_VSN of
        true ->
            {error, invalid_name_fee} = aetx:process(Tx8, Trees, Env);
        false ->
            {error, invalid_at_protocol} = aetx:process(Tx8, Trees, Env)

    end,
    ok.

%% Assure we cannot accidentally claim the empty name
claim_empty_name(Cfg) ->
    PreLima = ?config(protocl, Cfg) < ?LIMA_PROTOCOL_VSN,
    {PubKey, S1} = aens_test_utils:setup_new_account(aens_test_utils:new_state()),
    PrivKey = aens_test_utils:priv_key(PubKey, S1),
    Trees = aens_test_utils:trees(S1),
    Height = 1,
    Env0 = aetx_env:tx_env(Height),

    %% Test empty name preclaim
    CHash = if PreLima -> aens_hash:commitment_hash(<<".test">>, 123);
               true -> aens_hash:commitment_hash(<<".chain">>, 123)
            end,
    TxSpec1 = aens_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx1} = aens_preclaim_tx:new(TxSpec1),
    SignedTx = aec_test_utils:sign_tx(Tx1, PrivKey),
    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env0),
    S2 = aens_test_utils:set_trees(Trees1, S1),
    %% Claim committed empoty name
    Trees2 = aens_test_utils:trees(S2),
    Env1 = aetx_env:tx_env(Height + 1),

    TxSpec2 = if PreLima -> aens_test_utils:claim_tx_spec(PubKey, <<".test">>, 123, prelima, S2);
                 true -> aens_test_utils:claim_tx_spec(PubKey, <<".chain">>, 123, 36000000000000000000, S2)
              end,
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    {error, _} = aetx:process(Tx2, Trees2, Env1),
    ok.

claim_race_negative(Cfg) ->
    %% The first claim
    {_PubKey, _NHash, S1} = claim(Cfg),

    %% The second claim of the same name (hardcoded in preclaim) decomposed
    {PubKey2, Name2, NameSalt2, S2} = preclaim([{state, S1}, {name, ?config(name, Cfg)}]),
    Trees = aens_test_utils:trees(S2),
    Height = ?PRE_CLAIM_HEIGHT + 1,

    %% Test bad account key
    TxSpec1 = aens_test_utils:claim_tx_spec(PubKey2, Name2, NameSalt2, namefee(Name2, Cfg), S2),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    Env = aetx_env:tx_env(Height),
    {error, name_already_taken} = aetx:process(Tx1, Trees, Env).

claim_race_negative_auction(Cfg) ->
    %% The first claim
    {_PubKey, _NHash, S1} = claim_auction(Cfg),

    %% The second claim of the same name (hardcoded in preclaim) decomposed
    {PubKey2, Name2, NameSalt2, S2} = preclaim([{state, S1}, {name, ?config(name, Cfg)}]),
    Trees = aens_test_utils:trees(S2),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    Env = aetx_env:tx_env(Height),

    %% Test second claim via preclaim of claim in auction
    TxSpec1 = aens_test_utils:claim_tx_spec(PubKey2, Name2, NameSalt2, namefee(Name2, Cfg), S2),
    {ok, Tx1} = aens_claim_tx:new(TxSpec1),
    {error, name_already_in_auction} = aetx:process(Tx1, Trees, Env),

    %% Test second claim via auction no increased fee
    TxSpec2 = aens_test_utils:claim_tx_spec(PubKey2, Name2, 0, namefee(Name2, Cfg), S2),
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    {error, name_fee_increment_too_low} = aetx:process(Tx2, Trees, Env),

    %% Test second claim via auction increased, but not enough increased
    NameFee = namefee(Name2, Cfg) + (namefee(Name2, Cfg) * aec_governance:name_claim_bid_increment() div 100),
    TxSpec3 = aens_test_utils:claim_tx_spec(PubKey2, Name2, 0, NameFee - 1, S2),
    {ok, Tx3} = aens_claim_tx:new(TxSpec3),
    {error, name_fee_increment_too_low} = aetx:process(Tx3, Trees, Env),
    ok.

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

update_expire_at_once(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = aens_test_utils:priv_key(PubKey, S1),

    %% Check name present, but neither pointers nor name TTL set
    {value, N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees)),
    [] = aens_names:pointers(N),
    0  = aens_names:client_ttl(N),

    %% Create Update tx and apply it on trees
    Pointers = [aens_pointer:new(<<"account_pubkey">>, aeser_id:create(account, <<1:256>>))],
    NameTTL  = 0,
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
    claimed = aens_names:status(N1),

    %% The name is still claimed, but from next keyblock it is expired
    Trees2 = perform_pre_transformations(Trees1, Height+1),
    {value, N2} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees2)),
    revoked = aens_names:status(N2),
    ok.

update_negative(Cfg) ->
    Protocol = ?config(protocol, Cfg),
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    Env = aetx_env:tx_env(Height),

    %% Test TX TTL too low
    TxSpec0 = aens_test_utils:update_tx_spec(PubKey, NHash, #{ttl => Height - 1}, S1),
    {ok, Tx0} = aens_update_tx:new(TxSpec0),
    {error, ttl_expired} = aetx:process(Tx0, Trees, Env),

    %% Test name TTL too high
    MaxTTL = aec_governance:name_claim_max_expiration(Protocol),
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
    {error, tx_nonce_already_used_for_account} = aetx:process(Tx4, Trees, Env),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(aens_test_utils:fullname(<<"othername">>)),
    TxSpec5 = aens_test_utils:update_tx_spec(PubKey, NHash2, S1),
    {ok, Tx5} = aens_update_tx:new(TxSpec5),
    {error, name_does_not_exist} = aetx:process(Tx5, Trees, Env),

    %% Too long pointer key - from IRIS
    BadPts1  = [aens_pointer:new(<<1:(8*257)>>, aeser_id:create(account, <<1:256>>))],
    TxSpec5b = aens_test_utils:update_tx_spec(PubKey, NHash, #{pointers => BadPts1}, S1),
    {ok, Tx5b} = aens_update_tx:new(TxSpec5b),
    case Protocol >= ?IRIS_PROTOCOL_VSN of
        true  -> {error, invalid_pointers} = aetx:process(Tx5b, Trees, Env);
        false -> {ok, _, _}                = aetx:process(Tx5b, Trees, Env)
    end,

    %% Too many pointers - from IRIS
    BadPts2  = [aens_pointer:new(<<N:256>>, aeser_id:create(account, <<1:256>>)) || N <- lists:seq(1, 33) ],
    TxSpec5c = aens_test_utils:update_tx_spec(PubKey, NHash, #{pointers => BadPts2}, S1),
    {ok, Tx5c} = aens_update_tx:new(TxSpec5c),
    case Protocol >= ?IRIS_PROTOCOL_VSN of
        true  -> {error, invalid_pointers} = aetx:process(Tx5c, Trees, Env);
        false -> {ok, _, _}                = aetx:process(Tx5c, Trees, Env)
    end,

    %% Duplicate pointer keys - from IRIS
    BadPts3  = [aens_pointer:new(<<1:256>>, aeser_id:create(account, <<N:256>>)) || N <- lists:seq(1, 2) ],
    TxSpec5d = aens_test_utils:update_tx_spec(PubKey, NHash, #{pointers => BadPts3}, S1),
    {ok, Tx5d} = aens_update_tx:new(TxSpec5d),
    case Protocol >= ?IRIS_PROTOCOL_VSN of
        true  -> {error, invalid_pointers} = aetx:process(Tx5d, Trees, Env);
        false -> {ok, _, _}                = aetx:process(Tx5d, Trees, Env)
    end,

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
    {error, tx_nonce_already_used_for_account} = aetx:process(Tx3, Trees, Env),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(aens_test_utils:fullname(<<"othername">>)),
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
    {error, tx_nonce_already_used_for_account} = aetx:process(Tx3, Trees, Env),

    %% Test name not present
    {ok, NHash2} = aens:get_name_hash(aens_test_utils:fullname(<<"othername">>)),
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
    Trees3 = perform_pre_transformations(Trees2, TTL+1),
    NSTree = aec_trees:ns(Trees3),
    none = aens_state_tree:lookup_commitment(CHash, NSTree),
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


    Trees3 = perform_pre_transformations(Trees2, TTL1 + 1),
    NTree2 = aec_trees:ns(Trees3),
    {value, N2} = aens_state_tree:lookup_name(NHash, NTree2),
    NHash    = aens_names:hash(N2),
    PubKey   = aens_names:owner_pubkey(N2),
    revoked  = aens_names:status(N2),
    TTL2     = aens_names:ttl(N2),

    Trees4 = perform_pre_transformations(Trees3, TTL2+1),
    NTree3 = aec_trees:ns(Trees4),
    none = aens_state_tree:lookup_name(NHash, NTree3),

    {PubKey, NHash, S2}.

prune_claim_auction(Cfg) ->
    {_, _, _} = claim([{name, <<"a-very-long-name-to-make-sure-lock-account-exists-in-tree">>}|Cfg]),
    {PubKey, NHash, S2} = claim_auction(Cfg),
    %% Auction has now started

    %% Re-pull values for this test
    Height = ?PRE_CLAIM_HEIGHT + 1,  %% height of the claim
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    {value, A} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTrees),

    PubKey   = aens_auctions:bidder_pubkey(A),
    TTL1     = aens_auctions:ttl(A),
    TTL1     = Height +
        aec_governance:name_claim_bid_timeout(aens_test_utils:fullname(?config(name, Cfg)),
                                              ?config(protocol, Cfg)),

    Trees3   = perform_pre_transformations(Trees2, TTL1), %% latest possible bid
    NTree2   = aec_trees:ns(Trees3),
    none     = aens_state_tree:lookup_name(NHash, NTree2),
    {value, A2} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTree2),
    PubKey   = aens_auctions:bidder_pubkey(A2),
    TTL1     = aens_auctions:ttl(A2),  %% absolute TTLs


    Trees4   = perform_pre_transformations(Trees2, TTL1+1),  %% used Trees2 on purpose
    NTree3   = aec_trees:ns(Trees4),

    none = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTree3),
    {value, N} = aens_state_tree:lookup_name(NHash, NTree3),
    ct:pal("Name auction ready ~p", [N]),
    NHash    = aens_names:hash(N),
    PubKey   = aens_names:owner_pubkey(N),
    claimed  = aens_names:status(N),

    Protocol = ?config(protocol, Cfg),
    ExpTTL   = aec_governance:name_claim_max_expiration(Protocol),
    ?assertEqual(ExpTTL, aens_names:ttl(N) - TTL1),

    {PubKey, NHash, S2}.


%%%===================================================================
%%% Change of registrar
%%%===================================================================

registrar_change(Cfg) ->
    State = aens_test_utils:new_state(),
    {PubKey, S1} = aens_test_utils:setup_new_account(State),
    PrivKey = aens_test_utils:priv_key(PubKey, S1),
    NameSalt = rand:uniform(10000),
    %% invalid name for protocol
    Name = case ?config(protocol, Cfg) >= ?LIMA_PROTOCOL_VSN of
               false -> <<"asdf.chain">>; %% we can't claim ".chain" name yet
               true -> <<"asdf.test">> %% we no longer can claim ".test" name
           end,

    %% preclaim
    Trees1 = aens_test_utils:trees(S1),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash = aens_hash:commitment_hash(NameAscii, NameSalt),
    TxSpec1 = aens_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx1} = aens_preclaim_tx:new(TxSpec1),
    SignedTx1 = aec_test_utils:sign_tx(Tx1, PrivKey),
    {ok, [_SignedTx1], Trees2, _} =
        aec_block_micro_candidate:apply_block_txs(
          [SignedTx1], Trees1, aetx_env:tx_env(1)),

    %% claim
    S2 = aens_test_utils:set_trees(Trees2, S1),
    TxSpec2 = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(Name, Cfg), S2),
    {ok, Tx2} = aens_claim_tx:new(TxSpec2),
    {error, invalid_registrar} = aetx:process(Tx2, Trees2, aetx_env:tx_env(2)).


namefee(Name, Cfg) ->
    case ?config(protocol, Cfg) >= ?LIMA_PROTOCOL_VSN of
        true ->
            aec_governance:name_claim_fee(Name, ?config(protocol, Cfg));
        false -> prelima
    end.

perform_pre_transformations(Trees, Height) ->
    TxEnv = aetx_env:tx_env(Height),
    PrevProtocol = aec_hard_forks:protocol_effective_at_height(Height - 1),
    aec_trees:perform_pre_transformations(Trees, TxEnv, PrevProtocol).
