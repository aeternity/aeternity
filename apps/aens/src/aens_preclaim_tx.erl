%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System pre-claim transaction
%%% @end
%%%=============================================================================

-module(aens_preclaim_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         gas_price/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Getters
-export([account_id/1,
         commitment_id/1,
         commitment_hash/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_PRECLAIM_TX_VSN, 1).
-define(NAME_PRECLAIM_TX_TYPE, name_preclaim_tx).

-record(ns_preclaim_tx, {
          account_id    :: aec_id:id(),
          nonce         :: integer(),
          commitment_id :: aec_id:id(),
          fee           :: integer(),
          ttl           :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_preclaim_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id    := AccountId,
      nonce         := Nonce,
      commitment_id := CommitmentId,
      fee           := Fee} = Args) ->
    account    = aec_id:specialize_type(AccountId),
    commitment = aec_id:specialize_type(CommitmentId),
    Tx = #ns_preclaim_tx{account_id    = AccountId,
                         nonce         = Nonce,
                         commitment_id = CommitmentId,
                         fee           = Fee,
                         ttl           = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_PRECLAIM_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_preclaim_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_preclaim_tx{}) ->
    aec_governance:tx_gas().

-spec gas_price(tx()) -> non_neg_integer().
gas_price(#ns_preclaim_tx{}) ->
    aec_governance:tx_gas_price().

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_preclaim_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_preclaim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_preclaim_tx{} = Tx) ->
    account_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_preclaim_tx{nonce = Nonce,
                      fee   = Fee} = Tx,
      Trees,_Env) ->
    AccountPubKey  = account_pubkey(Tx),
    CommitmentHash = commitment_hash(Tx),

    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> check_not_commitment(CommitmentHash, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#ns_preclaim_tx{fee = Fee, nonce = Nonce} = PreclaimTx,
        Trees0, Env) ->
    Height = aetx_env:height(Env),
    AccountPubKey = account_pubkey(PreclaimTx),
    AccountsTree0 = aec_trees:accounts(Trees0),
    NSTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
   {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    TTL = aec_governance:name_preclaim_expiration(),
    Commitment = aens_commitments:new(PreclaimTx, TTL, Height),
    NSTree1 = aens_state_tree:enter_commitment(Commitment, NSTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NSTree1),

    {ok, Trees2}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_preclaim_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_preclaim_tx{account_id    = AccountId,
                          nonce         = Nonce,
                          commitment_id = CommitmentId,
                          fee           = Fee,
                          ttl           = TTL}) ->
    {version(),
     [ {account_id, AccountId}
     , {nonce, Nonce}
     , {commitment_id, CommitmentId}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_PRECLAIM_TX_VSN,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {commitment_id, CommitmentId}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aec_id:specialize_type(AccountId),
    commitment = aec_id:specialize_type(CommitmentId),
    #ns_preclaim_tx{account_id    = AccountId,
                    nonce         = Nonce,
                    commitment_id = CommitmentId,
                    fee           = Fee,
                    ttl           = TTL}.

serialization_template(?NAME_PRECLAIM_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {commitment_id, id}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_preclaim_tx{account_id    = AccountId,
                           nonce         = Nonce,
                           commitment_id = CommitmentId,
                           fee           = Fee,
                           ttl           = TTL}) ->
    #{<<"account_id">>    => aec_base58c:encode(id_hash, AccountId),
      <<"nonce">>         => Nonce,
      <<"commitment_id">> => aec_base58c:encode(id_hash, CommitmentId),
      <<"fee">>           => Fee,
      <<"ttl">>           => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account_id(tx()) -> aec_id:id().
account_id(#ns_preclaim_tx{account_id = AccountId}) ->
    AccountId.

-spec commitment_id(tx()) -> aec_id:id().
commitment_id(#ns_preclaim_tx{commitment_id = CommitmentId}) ->
    CommitmentId.

%%%===================================================================
%%% Internal functions
%%%===================================================================

account_pubkey(#ns_preclaim_tx{account_id = AccountId}) ->
    aec_id:specialize(AccountId, account).

commitment_hash(#ns_preclaim_tx{commitment_id = CommitmentId}) ->
    aec_id:specialize(CommitmentId, commitment).

check_not_commitment(CommitmentHash, Trees) ->
    NSTree = aec_trees:ns(Trees),
    case aens_state_tree:lookup_commitment(CommitmentHash, NSTree) of
        {value, _CommitmentHash} -> {error, commitment_already_present};
        none -> ok
    end.

version() ->
    ?NAME_PRECLAIM_TX_VSN.

