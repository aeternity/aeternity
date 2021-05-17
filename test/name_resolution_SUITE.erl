%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Test name resolution in transactions
%%% @end
%%%-------------------------------------------------------------------
-module(name_resolution_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        , init_per_group/2
        , end_per_group/2
        ]).

%% test case exports
-export([ spend_to_name/1
        , spend_to_name_when_multiple_pointer_entries/1
        %% , query_oracle/1
        %% , query_named_oracle/1
        , transfer_name_to_named_account/1
        , transfer_name_to_named_account_when_multiple_pointer_entries/1
        ]).

-import(aec_block_micro_candidate, [apply_block_txs_strict/3
                                   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").


%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [ {group, no_auction} ].

groups() ->
   [{no_auction,
    [ spend_to_name
    , spend_to_name_when_multiple_pointer_entries
    , transfer_name_to_named_account
    , transfer_name_to_named_account_when_multiple_pointer_entries
    ]}].

init_per_group(no_auction, Cfg) ->
    application:set_env(aecore, name_claim_bid_timeout, 0),
    Cfg.

end_per_group(_, Cfg) ->
    Cfg.


%%%===================================================================
%%% Helpers
%%%===================================================================

init_state(N) ->
    KeysList = [enacl:sign_keypair() || _ <- lists:duplicate(N, 1)],
    Keys   = maps:from_list([{Pub, Priv}
                             || #{public := Pub, secret := Priv} <- KeysList]),
    Trees0 = aec_trees:new_without_backend(),
    ATrees = lists:foldl(fun(#{public := Pubkey}, AccTrees) ->
                                 Account = aec_accounts:new(Pubkey,
                                                            400000000000000000000 * aec_test_utils:min_gas_price()),
                                 aec_accounts_trees:enter(Account, AccTrees)
                         end,
                         aec_trees:accounts(Trees0),
                         KeysList),
    Trees = aec_trees:set_accounts(Trees0, ATrees),
    { [Pubkey || #{public := Pubkey} <- KeysList]
    , #{ keys   => Keys
       , trees  => Trees
       , height => 1
       }
    }.

balance(Pubkey, #{trees := Trees}) ->
    ATrees = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(Pubkey, ATrees),
    aec_accounts:balance(Account).

apply_txs([Tx|Left], #{trees := Trees, height := Height} = S) ->
    STx = sign(Tx, S),
    Env = aetx_env:tx_env(Height),
    case apply_block_txs_strict([STx], Trees, Env) of
        {ok, [STx], Trees1, _} ->
            apply_txs(Left, S#{trees => Trees1});
        {error, What} ->
            error({failed_tx, Tx, What})
    end;
apply_txs([], #{height := Height} = S) ->
    S#{height => Height + 1}.

sign(Tx, #{keys := Keys, trees := Trees}) ->
    {ok, Signers} = aetx:signers(Tx, Trees),
    Privkeys = [maps:get(Pubkey, Keys) || Pubkey <- Signers],
    aec_test_utils:sign_tx(Tx, Privkeys).

account_id(Pubkey) ->
    aeser_id:create(account, Pubkey).

name_id(NameHash) ->
    aeser_id:create(name, NameHash).

name_from_id(NameID) ->
    {name, NameHash} = aeser_id:specialize(NameID),
    NameHash.

commitment_id(CommitmentHash) ->
    aeser_id:create(commitment, CommitmentHash).

name_owner_pubkey(NameID, #{trees := Trees}) ->
    NameHash = aeser_id:specialize(NameID, name),
    NSTrees = aec_trees:ns(Trees),
    case aens_state_tree:lookup_name(NameHash, NSTrees) of
        {value, Name} -> aens_names:owner_pubkey(Name);
        none -> error(name_not_found)
    end.

%%%===================================================================
%%% Register a name

register_name(Pubkey, #{height := Height} = S) ->
    register_name(Pubkey, aens_test_utils:fullname(<<"hello"/utf8>>, Height), S).

register_name(Pubkey, Name, #{height := Height} = S) ->
    {ok, Ascii} = aens_utils:to_ascii(Name),
    Hash   = aens_hash:name_hash(Ascii),
    Salt   = 42,
    CommitmentId = commitment_id(aens_hash:commitment_hash(Ascii, Salt)),
    PreclaimSpec = #{ account_id => account_id(Pubkey)
                    , commitment_id => CommitmentId
                    , fee  => 20000 * aec_test_utils:min_gas_price()
                    , nonce => 1
                    },
    {ok, Preclaim} = aens_preclaim_tx:new(PreclaimSpec),
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    NameFee =
        case Protocol >= ?LIMA_PROTOCOL_VSN of
            true -> aec_governance:name_claim_fee(Name, Protocol);
            false -> prelima
        end,
    ClaimSpec  = #{ account_id => account_id(Pubkey)
                  , name => Name
                  , name_salt => Salt
                  , name_fee => NameFee
                  , fee  => 20000 * aec_test_utils:min_gas_price()
                  , nonce => 2
                  },
    {ok, Claim} = aens_claim_tx:new(ClaimSpec),
    {apply_txs([Claim], apply_txs([Preclaim], S)), name_id(Hash)}.

pointers(Tag, ToPubkey) ->
    IdType = type2id(Tag),
    [aens_pointer:new(atom_to_binary(Tag, utf8), aeser_id:create(IdType, ToPubkey))].

pointers_with_duplicated_key_at_end(Tag, ToPubkey) ->
    [P] = pointers(Tag, ToPubkey),
    %% Extract pointer value i.e. identifier, create distinct identifier.
    PId = aens_pointer:id(P),
    {PTag, PVal} = aeser_id:specialize(PId),
    <<PValInt:32/unit:8>> = PVal,
    ShadowedId = aeser_id:create(PTag, <<(1+PValInt):32/unit:8>>),
    ?assertNotEqual(PId, ShadowedId), %% Hardcoded expectation on generated identifier being distinct.
    ShadowedP = aens_pointer:new(aens_pointer:key(P), ShadowedId),
    [P, ShadowedP].

update_pointers(Pointers, Pubkey, NameID, Nonce, S) ->
    UpdateSpec  = #{ account_id => account_id(Pubkey)
                   , nonce => Nonce
                   , name_id => NameID
                   , name_ttl => 100
                   , pointers => Pointers
                   , client_ttl => 100
                   , fee => 20000 * aec_test_utils:min_gas_price()
                   },
    {ok, Update} = aens_update_tx:new(UpdateSpec),
    apply_txs([Update], S).

update_and_check_pointers(Pointers, Pubkey, NameID, Nonce, S0) ->
    S1 = update_pointers(Pointers, Pubkey, NameID, Nonce, S0),
    N = name_from_id(NameID),
    #{trees := T1} = S1,
    P1 = aens_names:pointers(aens_state_tree:get_name(N, aec_trees:ns(T1))),
    ?assertEqual(Pointers, P1),
    S1.

%% TODO: it'd be nice not to have different atoms for ids and encoding.
type2id(account_pubkey)  -> account;
type2id(channel)         -> channel;
type2id(commitment)      -> commitment;
type2id(contract_pubkey) -> contract;
type2id(name)            -> name;
type2id(oracle_pubkey)   -> oracle.

%%%===================================================================
%%% Spend tests
%%%===================================================================

spend_to_name(_Cfg) ->
    spend_to_name_(fun pointers/2).

spend_to_name_when_multiple_pointer_entries(_Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    case Protocol >= ?IRIS_PROTOCOL_VSN of
        true -> ok;
        false ->
            spend_to_name_(fun pointers_with_duplicated_key_at_end/2)
    end.

spend_to_name_(PointersFun) ->
    {[Pubkey1, Pubkey2], S1} = init_state(2),
    Amount = 100,
    Fee    = 20000 * aec_test_utils:min_gas_price(),
    {S2, NameId} = register_name(Pubkey2, S1),
    S3 = update_and_check_pointers(PointersFun(account_pubkey, Pubkey2), Pubkey2, NameId, 3, S2),
    {ok, Spend} = aec_spend_tx:new(#{ sender_id    => account_id(Pubkey1)
                                    , recipient_id => NameId
                                    , amount       => Amount
                                    , fee          => Fee
                                    , nonce        => 1
                                    , payload      => <<>>}),

    S4 = apply_txs([Spend], S3),
    ?assertEqual(balance(Pubkey2, S3) + Amount,
                 balance(Pubkey2, S4)),
    ?assertEqual(balance(Pubkey1, S3) - Amount - Fee,
                 balance(Pubkey1, S4)),
    ok.


%%%===================================================================
%%% Name tests
%%%===================================================================

transfer_name_to_named_account(_Cfg) ->
    transfer_name_to_named_account_(fun pointers/2).

transfer_name_to_named_account_when_multiple_pointer_entries(_Cfg) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    case Protocol >= ?IRIS_PROTOCOL_VSN of
        true -> ok;
        false ->
            transfer_name_to_named_account_(fun pointers_with_duplicated_key_at_end/2)
    end.

transfer_name_to_named_account_(PointersFun) ->
    {[Pubkey1, Pubkey2, Pubkey3], S1} = init_state(3),

    %% Pubkey1 holds the reference to the recipient account (Pubkey3)
    {S2, NameId1} = register_name(Pubkey1, aens_test_utils:fullname(<<"foo"/utf8>>, maps:get(height, S1)), S1),
    S3 = update_and_check_pointers(PointersFun(account_pubkey, Pubkey3), Pubkey1, NameId1, 3, S2),

    %% Pubkey2 is the owner of the name
    {S4, NameId2} = register_name(Pubkey2, aens_test_utils:fullname(<<"bar"/utf8>>, maps:get(height, S3)), S3),

    %% Pubkey2 now transfers the name to Pubkey3 by referencing its name
    TransferSpec = #{ account_id => account_id(Pubkey2)
                    , nonce   => 3
                    , name_id => NameId2
                    , recipient_id => NameId1
                    , fee => 20000 * aec_test_utils:min_gas_price()
                    },
    {ok, Transfer} = aens_transfer_tx:new(TransferSpec),
    S5 = apply_txs([Transfer], S4),

    %% Check that the transfer happened
    ?assertEqual(Pubkey2, name_owner_pubkey(NameId2, S4)),
    ?assertEqual(Pubkey3, name_owner_pubkey(NameId2, S5)),
    ok.
