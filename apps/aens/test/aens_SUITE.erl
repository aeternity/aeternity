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
         claim/1, immediate_claim/1,
         ongoing_auction/1,
         second_claim_auction/1,
         second_claim_extend_auction/1,
         claim_locked_coins_holder_gets_locked_fee/1,
         claim_negative/1, claim_negative2/1,
         claim_empty_name/1,
         claim_race_negative/1,
         claim_race_negative_auction/1,
         immediate_claim_race/1, immediate_claim_race2/1,
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
-include_lib("aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("stdlib/include/assert.hrl").


%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, transactions},
     {group, auction},
     {group, no_auction_long_names},
     {group, auction_protocol_update}].

groups() ->
    [
     {transactions, [sequence],
      [prune_preclaim,
       prune_claim,
       preclaim,
       preclaim_negative,
       claim,
       immediate_claim,
       claim_locked_coins_holder_gets_locked_fee,
       claim_negative,
       claim_negative2,
       claim_empty_name,
       claim_race_negative,
       immediate_claim_race,
       immediate_claim_race2,
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
       immediate_claim,
       immediate_claim_race,
       immediate_claim_race2,
       claim_locked_coins_holder_gets_locked_fee,
       claim_negative,
       claim_negative2,
       claim_race_negative]},
     {auction,
      [prune_preclaim,
       prune_claim_auction,
       preclaim,
       immediate_claim,
       immediate_claim_race,
       immediate_claim_race2,
       claim,
       ongoing_auction,
       second_claim_auction,
       second_claim_extend_auction,
       claim_locked_coins_holder_gets_locked_fee,
       claim_race_negative_auction]},
     {auction_protocol_update,
      [second_claim_auction,
       second_claim_extend_auction]}
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
    %% depending on how we call 'make' we get different protocols
    [{protocol, aec_hard_forks:protocol_effective_at_height(1)},
     {name, ?NAME} | Cfg];
init_per_group(no_auction_long_names, Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    [{protocol, Protocol}, {name, gen_name(aec_governance:name_max_length_starting_auction()+1)} | Cfg];
init_per_group(auction_protocol_update, Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    if Protocol < ?LIMA_PROTOCOL_VSN -> {skip, no_auction_before_lima};
       Protocol =< ?IRIS_PROTOCOL_VSN ->
          SwitchHeight = 10,  %% first claim within those SwitchHeight blocks
          meck:expect(aec_hard_forks, protocol_effective_at_height,
                      fun(H) -> case H of
                                  0 -> 0;
                                  _ when H < SwitchHeight  -> Protocol;
                                  _ when H >= SwitchHeight -> Protocol + 1
                               end
                      end),
        [{next_protocol, SwitchHeight}, {protocol, Protocol},
         {claim_delta, fun(Extension, undefined) when SwitchHeight < Extension div 2 ->
                           Extension div 2;
                          (_Extension, undefined) ->
                           SwitchHeight + 1;
                          (Extension, TTL) ->
                           TTL - (Extension div 2)
                        end},
         {name, gen_name(aec_governance:name_max_length_starting_auction())} | Cfg];
       Protocol >= ?CERES_PROTOCOL_VSN -> {skip, no_protocol_after_ceres}
    end;
init_per_group(_, Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    %% One day auction open with cheapest possible name (31 char)
    if Protocol < ?LIMA_PROTOCOL_VSN -> {skip, no_auction_before_lima};
       true -> [{protocol, Protocol},
                {claim_delta, fun(Extension, undefined) ->
                                  (Extension div 2);
                                 (Extension, TTL) ->
                                  TTL - (Extension div 2)
                              end},
                {name, gen_name(aec_governance:name_max_length_starting_auction())} | Cfg]
    end.

end_per_group(transactions, Cfg) ->
    meck:unload(aec_governance),
    Cfg;
end_per_group(auction_protocol_update, Cfg) ->
    meck:unload(aec_hard_forks),
    Cfg;
end_per_group(_, Cfg) ->
    Cfg.

%%%===================================================================
%%% Helper functions
%%%===================================================================

gen_name(Length) ->
    <<<<"x">>||_<-lists:seq(1,Length)>>.

auctioned_name(Name, Protocol) ->
    case aec_governance:name_claim_bid_timeout(Name, Protocol) of
        0 -> false;
        _ -> byte_size(Name) > aec_governance:name_max_length_starting_auction()
    end.

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

%% Immediate claims (no NamePreclaimTx) was introduced in Ceres
%% claim_negative2 tests the negative (pre-Ceres) case
immediate_claim(Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    if Protocol < ?CERES_PROTOCOL_VSN -> {skip, preclaimless_claim_in_ceres};
       true                            -> immediate_claim_(Cfg)
    end.

immediate_claim_(Cfg) ->
    State = case proplists:get_value(state, Cfg) of
                undefined -> aens_test_utils:new_state();
                State0 -> State0
            end,
    {PubKey, S1} = aens_test_utils:setup_new_account(State),
    PrivKey = aens_test_utils:priv_key(PubKey, S1),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    Name = aens_test_utils:fullname(?config(name, Cfg)),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    NHash = aens_hash:name_hash(NameAscii),

    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, 0, namefee(NameAscii, Cfg), S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Env      = aetx_env:tx_env(Height),

    {ok, [SignedTx], Trees1, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Env),

    NTrees = aec_trees:ns(Trees1),
    S2 = aens_test_utils:set_trees(Trees1, S1),

    case auctioned_name(NameAscii, aec_hard_forks:protocol_effective_at_height(1)) of
        false ->
            {value, N} = aens_state_tree:lookup_name(NHash, NTrees),
            NHash   = aens_names:hash(N),
            PubKey  = aens_names:owner_pubkey(N),
            claimed = aens_names:status(N);
        true ->
            none   = aens_state_tree:lookup_name(NHash, NTrees),
            {value, A} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTrees),
            NHash   = aens_hash:from_auction_hash(aens_auctions:hash(A)),
            PubKey  = aens_auctions:bidder_pubkey(A)
    end,

    {PubKey, S2}.

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
    case auctioned_name(NameAscii, aec_hard_forks:protocol_effective_at_height(1)) of
        false ->
            {value, N} = aens_state_tree:lookup_name(NHash, NTrees),
            NHash   = aens_names:hash(N),
            PubKey  = aens_names:owner_pubkey(N),
            claimed = aens_names:status(N);
        true ->
            none   = aens_state_tree:lookup_name(NHash, NTrees),
            {value, A} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTrees),
            NHash   = aens_hash:from_auction_hash(aens_auctions:hash(A)),
            PubKey  = aens_auctions:bidder_pubkey(A)
    end,
    {PubKey, NHash, S2}.

ongoing_auction(Cfg) ->
    {PubKey2, Name, _Salt, S0} = preclaim(Cfg),

    {PubKey, NHash, S1} = claim([{state, S0} | Cfg]),
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

%% This test is only run from IRIS onward.
%% If {next_protocol, Height} in Cfg, then protocol change
second_claim_auction(Cfg) ->
    {PubKey1, _, S1} = claim([{name, <<"a-very-long-name-to-make-sure-lock-account-exists-in-tree">>}|Cfg]),
    PrivKey1 = aens_test_utils:priv_key(PubKey1, S1),
    {PubKey, NHash, S2} = claim([{state, S1} | Cfg]),
    %% Auction has now started

    FullName = aens_test_utils:fullname(?config(name, Cfg)),
    Protocol = ?config(protocol, Cfg),
    NextProtocol = proplists:get_value(next_protocol, Cfg),
    ClaimDeltaFun = ?config(claim_delta, Cfg),

    FirstClaimTimeout =
      aec_governance:name_claim_bid_timeout(FullName, Protocol),
    ExtensionClaimTimeout =
      case NextProtocol of
          undefined ->
              aec_governance:name_claim_bid_extension(FullName, Protocol);
          _ ->
              aec_governance:name_claim_bid_extension(FullName, NextProtocol)
      end,

    %% Re-pull values for this test
    Height = ?PRE_CLAIM_HEIGHT + 1,  %% height of the claim
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    {value, A} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTrees),
    ct:pal("ongoing auction ~p for name ~p", [A, ?config(name, Cfg)]),

    PubKey   = aens_auctions:bidder_pubkey(A),
    TTL1     = Height + FirstClaimTimeout,
    TTL1     = aens_auctions:ttl(A),
    Delta    = ClaimDeltaFun(ExtensionClaimTimeout, proplists:get_value(extend, Cfg)),
    %% Delta such that either extends a little (after possible protocol change)
    %% or extends to just before TTL kicks in and therefore guarantees to really extend
    ?assert(FirstClaimTimeout > Height + Delta, "Auction must be ongoing during second claim"),

    Trees3   = perform_pre_transformations(Trees2, Height + Delta), %% progress Delta before doing a second claim
    NTree2   = aec_trees:ns(Trees3),
    none     = aens_state_tree:lookup_name(NHash, NTree2),

    if NextProtocol == undefined -> ct:pal("second claim at height ~p", [Height + Delta]);
       true -> ct:pal("second claim at height ~p (protocol updated at height ~p)", [Height + Delta, NextProtocol])
    end,

    %% Create Claim tx and apply it on Trees3
    {ok, NameAscii} = aens_utils:to_ascii(FullName),

    %% increased name fee
    TxSpec = aens_test_utils:claim_tx_spec(PubKey1, FullName, 0, 2*namefee(NameAscii, Cfg), S2),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey1),
    Env      = aetx_env:tx_env(Height + Delta + 1),
    ct:pal("TxSpec ~p", [TxSpec]),

    {ok, [SignedTx], TreesN, _} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees3, Env),
    S3 = aens_test_utils:set_trees(TreesN, S2),

    Trees4 = aens_test_utils:trees(S3),

    NTree4 = aec_trees:ns(Trees4),
    {value, A2} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTree4),
    PubKey1  = aens_auctions:bidder_pubkey(A2),
    TTL2     = aens_auctions:ttl(A2),  %% absolute TTLs

    ct:pal("ongoing auction ~p", [A2]),
    TTL2 =
        case Protocol of
            ?LIMA_PROTOCOL_VSN ->
                Height + Delta + 1 + FirstClaimTimeout;
            ?IRIS_PROTOCOL_VSN when NextProtocol == undefined ->
                Height + Delta + 1 + FirstClaimTimeout;
            _ ->
                NewTTL = Height + Delta + 1 + ExtensionClaimTimeout,
                if NewTTL =< TTL1 -> TTL1;
                   true -> NewTTL
                end
        end,

    %% Check auction not prematurly ended
    Trees5   = perform_pre_transformations(Trees4, TTL2),
    NTree5   = aec_trees:ns(Trees5),
    none     = aens_state_tree:lookup_name(NHash, NTree5),

    Trees6   = perform_pre_transformations(Trees4, TTL2 + 1),
    NTree6   = aec_trees:ns(Trees6),

    ct:pal("check auction ready at height ~p", [TTL2 + 1]),

    none = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTree6),
    {value, N} = aens_state_tree:lookup_name(NHash, NTree6),
    NHash    = aens_names:hash(N),
    PubKey1   = aens_names:owner_pubkey(N),
    claimed  = aens_names:status(N),
    ok.

second_claim_extend_auction(Cfg) ->
    FullName = aens_test_utils:fullname(?config(name, Cfg)),
    Protocol = ?config(protocol, Cfg),
    %% By taking a height just before expiry, the next claim must extend the expiry TTL

    FirstClaimTimeout = aec_governance:name_claim_bid_timeout(FullName, Protocol),
    second_claim_auction([{extend, FirstClaimTimeout} | Cfg]).


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

claim_negative2(Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    if Protocol >= ?CERES_PROTOCOL_VSN -> {skip, preclaimless_claim_in_ceres};
       true                            -> claim_negative2_(Cfg)
    end.

claim_negative2_(Cfg) ->
    State = case proplists:get_value(state, Cfg) of
                undefined -> aens_test_utils:new_state();
                State0 -> State0
            end,
    {PubKey, S1} = aens_test_utils:setup_new_account(State),
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT,
    Name = aens_test_utils:fullname(?config(name, Cfg)),

    %% No preclaim
    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, 0, namefee(Name, Cfg), S1),
    {ok, Tx0} = aens_claim_tx:new(TxSpec),
    Env0 = aetx_env:tx_env(Height),
    case aec_hard_forks:protocol_effective_at_height(1) of
        P when P > ?FORTUNA_PROTOCOL_VSN ->
            {error, illegal_salt_value} = aetx:process(Tx0, Trees, Env0);
        _ ->
            {error, name_not_preclaimed} = aetx:process(Tx0, Trees, Env0)
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
    {_PubKey, _NHash, S1} = claim(Cfg),

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

immediate_claim_race(Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    if Protocol < ?CERES_PROTOCOL_VSN -> {skip, preclaimless_claim_in_ceres};
       true                           -> immediate_claim_race_(Cfg)
    end.

immediate_claim_race_(Cfg) ->
    %% The pre-claim
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),

    %% Do the immediate claim
    {_PubKey, S2} = immediate_claim([{state, S1} | Cfg]),

    %% Now try the delayed claim
    Trees = aens_test_utils:trees(S2),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    Env = aetx_env:tx_env(Height),

    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, NameSalt, namefee(Name, Cfg), S2),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    case auctioned_name(Name, aec_hard_forks:protocol_effective_at_height(1)) of
        true ->
            {error, name_already_in_auction} = aetx:process(Tx, Trees, Env);
        false ->
            {error, name_already_taken} = aetx:process(Tx, Trees, Env)
    end,

    ok.

immediate_claim_race2(Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    if Protocol < ?CERES_PROTOCOL_VSN -> {skip, preclaimless_claim_in_ceres};
       true                           -> immediate_claim_race2_(Cfg)
    end.

immediate_claim_race2_(Cfg) ->
    %% Do the claim
    {PubKey, _NHash, S1} = claim(Cfg),

    %% Now try the immediate claim
    Trees = aens_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    Env = aetx_env:tx_env(Height),
    Name = aens_test_utils:fullname(?config(name, Cfg)),

    TxSpec = aens_test_utils:claim_tx_spec(PubKey, Name, 0, namefee(Name, Cfg), S1),
    {ok, Tx} = aens_claim_tx:new(TxSpec),

    case auctioned_name(Name, aec_hard_forks:protocol_effective_at_height(1)) of
        true ->
            {error, name_fee_increment_too_low} = aetx:process(Tx, Trees, Env);
        false ->
            {error, name_already_taken} = aetx:process(Tx, Trees, Env)
    end,

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

    %% Raw data pointers from CERES
    Pointers2 = [aens_pointer:new(<<"account_pubkey">>, <<"raw data pointer">>)],
    TxSpec2 = aens_test_utils:update_tx_spec(
                PubKey, NHash, #{pointers => Pointers2, name_ttl => NameTTL}, S1),
    {ok, Tx2} = aens_update_tx:new(TxSpec2),
    SignedTx2 = aec_test_utils:sign_tx(Tx2, PrivKey),

    Protocol = ?config(protocol, Cfg),
    case Protocol >= ?CERES_PROTOCOL_VSN of
        true  ->
            {ok, [SignedTx2], Trees2, _} =
                aec_block_micro_candidate:apply_block_txs([SignedTx2], Trees, Env),
            {value, N2} = aens_state_tree:lookup_name(NHash, aec_trees:ns(Trees2)),
            Pointers2 = aens_names:pointers(N2);
        false ->
            %% Transaction should not be accepted...
            {ok, [], _, _} =
                aec_block_micro_candidate:apply_block_txs([SignedTx2], Trees, Env)
    end,
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

    %% Raw data pointer - pre-CERES
    BadPts4  = [aens_pointer:new(<<"key">>, <<"raw data">>)],
    TxSpec5e = aens_test_utils:update_tx_spec(PubKey, NHash, #{pointers => BadPts4}, S1),
    {ok, Tx5e} = aens_update_tx:new(TxSpec5e),
    case Protocol >= ?CERES_PROTOCOL_VSN of
        true  -> {ok, _, _}                   = aetx:process(Tx5e, Trees, Env);
        false -> {error, invalid_at_protocol} = aetx:process(Tx5e, Trees, Env)
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
    {PubKey, NHash, S2} = claim(Cfg),
    %% Auction has now started

    %% Re-pull values for this test
    Height = ?PRE_CLAIM_HEIGHT + 1,  %% height of the claim
    Trees2 = aens_test_utils:trees(S2),
    NTrees = aec_trees:ns(Trees2),
    {value, A} = aens_state_tree:lookup_name_auction(aens_hash:to_auction_hash(NHash), NTrees),
    ct:pal("ongoing auction ~p for name ~p", [A, aens_test_utils:fullname(?config(name, Cfg))]),

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
