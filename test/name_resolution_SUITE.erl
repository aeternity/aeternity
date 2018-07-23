%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Test name resolution in transactions
%%% @end
%%%-------------------------------------------------------------------
-module(name_resolution_SUITE).

%% common_test exports
-export([ all/0
        ]).

%% test case exports
-export([ spend_to_name/1
        %% , query_oracle/1
        %% , query_named_oracle/1
        , transfer_name_to_named_account/1
        ]).

-import(aec_block_micro_candidate, [apply_block_txs_strict/4
                                   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("aecore/include/blocks.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [ spend_to_name
    , transfer_name_to_named_account
    ].


%%%===================================================================
%%% Helpers
%%%===================================================================

init_state(N) ->
    KeysList = [enacl:sign_keypair() || _ <- lists:duplicate(N, 1)],
    Keys   = maps:from_list([{Pub, Priv}
                             || #{public := Pub, secret := Priv} <- KeysList]),
    Trees0 = aec_trees:new_without_backend(),
    ATrees = lists:foldl(fun(#{public := Pubkey}, AccTrees) ->
                                 Account = aec_accounts:new(Pubkey, 1000),
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
    case apply_block_txs_strict([STx], Trees,
                                Height, ?PROTOCOL_VERSION) of
        {ok, [STx], Trees1} ->
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
    aec_id:create(account, Pubkey).

name_id(NameHash) ->
    aec_id:create(name, NameHash).

commitment_id(CommitmentHash) ->
    aec_id:create(commitment, CommitmentHash).

name_owner(NameID, #{trees := Trees}) ->
    NameHash = aec_id:specialize(NameID, name),
    NSTrees = aec_trees:ns(Trees),
    case aens_state_tree:lookup_name(NameHash, NSTrees) of
        {value, Name} -> aens_names:owner(Name);
        none -> error(name_not_found)
    end.

%%%===================================================================
%%% Register a name

register_name(Pubkey, S) ->
    register_name(Pubkey, <<"hello.test"/utf8>>, S).

register_name(Pubkey, Name, S) ->
    {ok, Ascii} = aens_utils:to_ascii(Name),
    Hash   = aens_hash:name_hash(Ascii),
    Salt   = 42,
    Commitment = commitment_id(aens_hash:commitment_hash(Ascii, Salt)),
    PreclaimSpec = #{ account => account_id(Pubkey)
                    , commitment => Commitment
                    , fee  => 1
                    , nonce => 1
                    },
    {ok, Preclaim} = aens_preclaim_tx:new(PreclaimSpec),
    ClaimSpec  = #{ account => account_id(Pubkey)
                  , name => Name
                  , name_salt => Salt
                  , fee  => 1
                  , nonce => 2
                  },
    {ok, Claim} = aens_claim_tx:new(ClaimSpec),
    {apply_txs([Claim], apply_txs([Preclaim], S)), name_id(Hash)}.

update_pointers(Tag, ToPubkey, Pubkey, NameID, Nonce, S) ->
    Pointers    = [{atom_to_binary(Tag, utf8), aec_base58c:encode(Tag, ToPubkey)}],
    UpdateSpec  = #{ account => account_id(Pubkey)
                   , nonce => Nonce
                   , name_hash => NameID
                   , name_ttl => 100
                   , pointers => Pointers
                   , client_ttl => 100
                   , fee => 1
                   },
    {ok, Update} = aens_update_tx:new(UpdateSpec),
    apply_txs([Update], S).

%%%===================================================================
%%% Spend tests
%%%===================================================================

spend_to_name(_Cfg) ->
    {[Pubkey1, Pubkey2], S1} = init_state(2),
    Amount = 100,
    Fee    = 1,
    {S2, NameId} = register_name(Pubkey2, S1),
    S3 = update_pointers(account_pubkey, Pubkey2, Pubkey2, NameId, 3, S2),
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
    {[Pubkey1, Pubkey2, Pubkey3], S1} = init_state(3),

    %% Pubkey1 holds the reference to the recipient account (Pubkey3)
    {S2, NameId1} = register_name(Pubkey1, <<"foo.test"/utf8>>, S1),
    S3 = update_pointers(account_pubkey, Pubkey3, Pubkey1, NameId1, 3, S2),

    %% Pubkey2 is the owner of the name
    {S4, NameId2} = register_name(Pubkey2, <<"bar.test"/utf8>>, S3),

    %% Pubkey2 now transfers the name to Pubkey3 by referencing its name
    TransferSpec = #{ account => account_id(Pubkey2)
                    , nonce   => 3
                    , name_hash => NameId2
                    , recipient_account => NameId1
                    , fee => 5
                    },
    {ok, Transfer} = aens_transfer_tx:new(TransferSpec),
    S5 = apply_txs([Transfer], S4),

    %% Check that the transfer happened
    ?assertEqual(Pubkey2, name_owner(NameId2, S4)),
    ?assertEqual(Pubkey3, name_owner(NameId2, S5)),
    ok.
