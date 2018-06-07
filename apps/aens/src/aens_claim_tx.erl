%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System claim transaction
%%% @end
%%%=============================================================================

-module(aens_claim_tx).

-include("ns_txs.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Getters
-export([account/1,
         name/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_CLAIM_TX_VSN, 1).
-define(NAME_CLAIM_TX_TYPE, name_claim_tx).

-opaque tx() :: #ns_claim_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account   := AccountPubKey,
      nonce     := Nonce,
      name      := Name,
      name_salt := NameSalt,
      fee       := Fee,
      ttl       := TTL}) ->
    Tx = #ns_claim_tx{account   = AccountPubKey,
                      nonce     = Nonce,
                      name      = Name,
                      name_salt = NameSalt,
                      fee       = Fee,
                      ttl       = TTL},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_CLAIM_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_claim_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aec_blocks:height().
ttl(#ns_claim_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_claim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_claim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_claim_tx{account = AccountPubKey, nonce = Nonce,
                   fee = Fee, name = Name, name_salt = NameSalt}, _Context, Trees, Height, _ConsensusVersion) ->
    case aens_utils:to_ascii(Name) of
        {ok, NameAscii} ->
            %% TODO: Maybe include burned fee in tx fee. To do so, mechanism determining
            %% which part of fee goes to miner and what gets burned is needed.
            %% One option is to change tx:fee/1 callback to tx:fee_for_miner/1.
            BurnedFee = aec_governance:name_claim_burned_fee(),

            Checks =
                [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Nonce, Fee + BurnedFee) end,
                 fun() -> check_commitment(NameAscii, NameSalt, AccountPubKey, Trees, Height) end,
                 fun() -> check_name(NameAscii, Trees) end],

            case aeu_validation:run(Checks) of
                ok              -> {ok, Trees};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#ns_claim_tx{account = AccountPubKey, nonce = Nonce, fee = Fee,
                     name = PlainName, name_salt = NameSalt} = ClaimTx, _Context, Trees0, Height, _ConsensusVersion) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NSTree0 = aec_trees:ns(Trees0),

    TotalFee = Fee + aec_governance:name_claim_burned_fee(),
    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, TotalFee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    {ok, PlainNameAscii} = aens_utils:to_ascii(PlainName),
    CommitmentHash = aens_hash:commitment_hash(PlainNameAscii, NameSalt),
    NSTree1 = aens_state_tree:delete_commitment(CommitmentHash, NSTree0),

    TTL = aec_governance:name_claim_max_expiration(),
    Name = aens_names:new(ClaimTx, TTL, Height),
    NSTree2 = aens_state_tree:enter_name(Name, NSTree1),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NSTree2),

    {ok, Trees2}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_claim_tx{account = AccountPubKey}, _) ->
    {ok, [AccountPubKey]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_claim_tx{account   = AccountPubKey,
                       nonce     = None,
                       name      = Name,
                       name_salt = NameSalt,
                       fee       = Fee,
                       ttl       = TTL}) ->
    {version(),
     [ {account, AccountPubKey}
     , {nonce, None}
     , {name, Name}
     , {name_salt, NameSalt}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_CLAIM_TX_VSN,
            [ {account, AccountPubKey}
            , {nonce, Nonce}
            , {name, Name}
            , {name_salt, NameSalt}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    #ns_claim_tx{account   = AccountPubKey,
                 nonce     = Nonce,
                 name      = Name,
                 name_salt = NameSalt,
                 fee       = Fee,
                 ttl       = TTL}.

serialization_template(?NAME_CLAIM_TX_VSN) ->
    [ {account, binary}
    , {nonce, int}
    , {name, binary}
    , {name_salt, int}
    , {fee, int}
    , {ttl, int}
    ].


-spec for_client(tx()) -> map().
for_client(#ns_claim_tx{account   = AccountPubKey,
                        nonce     = Nonce,
                        name      = Name,
                        name_salt = NameSalt,
                        fee       = Fee,
                        ttl       = TTL}) ->
    #{<<"vsn">>       => version(),
      <<"account">>   => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>     => Nonce,
      <<"name">>      => Name,
      <<"name_salt">> => NameSalt,
      <<"fee">>       => Fee,
      <<"ttl">>       => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account(tx()) -> aec_keys:pubkey().
account(#ns_claim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec name(tx()) -> binary().
name(#ns_claim_tx{name = Name}) ->
    Name.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_commitment(NameAscii, NameSalt, AccountPubKey, Trees, Height) ->
    NSTree = aec_trees:ns(Trees),
    CH = aens_hash:commitment_hash(NameAscii, NameSalt),
    case aens_state_tree:lookup_commitment(CH, NSTree) of
        {value, C} ->
            case aens_commitments:owner(C) =:= AccountPubKey of
                true  ->
                    CreatedAt = aens_commitments:created(C),
                    Delta = aec_governance:name_claim_preclaim_delta(),
                    if CreatedAt + Delta =< Height -> ok;
                       true -> {error, commitment_delta_too_small}
                    end;
                false -> {error, commitment_not_owned}
            end;
        none ->
            {error, name_not_preclaimed}
    end.

check_name(NameAscii, Trees) ->
    NSTree = aec_trees:ns(Trees),
    NHash = aens_hash:name_hash(NameAscii),
    case aens_state_tree:lookup_name(NHash, NSTree) of
        {value, _} ->
            {error, name_already_taken};
        none ->
            ok
    end.


version() ->
    ?NAME_CLAIM_TX_VSN.
