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
         commitment/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_PRECLAIM_TX_VSN, 1).
-define(NAME_PRECLAIM_TX_TYPE, name_preclaim_tx).

-record(ns_preclaim_tx, {
          account    :: aec_id:id(),
          nonce      :: integer(),
          commitment :: aec_id:id(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_preclaim_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account    := AccountPubKey,
      nonce      := Nonce,
      commitment := Commitment,
      fee        := Fee} = Args) ->
    Tx = #ns_preclaim_tx{account    = aec_id:create(account, AccountPubKey),
                         nonce      = Nonce,
                         commitment = aec_id:create(commitment, Commitment),
                         fee        = Fee,
                         ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_PRECLAIM_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_preclaim_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_preclaim_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_preclaim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_preclaim_tx{} = Tx) ->
    account(Tx).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#ns_preclaim_tx{nonce = Nonce, fee = Fee} = Tx,
      _Context, Trees, _Height, _ConsensusVersion) ->
    AccountPubKey = account(Tx),
    Commitment = commitment(Tx),
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> check_not_commitment(Commitment, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#ns_preclaim_tx{fee = Fee, nonce = Nonce} = PreclaimTx,
        _Context, Trees0, Height, _ConsensusVersion) ->
    AccountPubKey = account(PreclaimTx),
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
    {ok, [account(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_preclaim_tx{account    = AccountId,
                          nonce      = Nonce,
                          commitment = CommitmentId,
                          fee        = Fee,
                          ttl        = TTL}) ->
    {version(),
     [ {account, AccountId}
     , {nonce, Nonce}
     , {commitment, CommitmentId}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_PRECLAIM_TX_VSN,
            [ {account, AccountId}
            , {nonce, Nonce}
            , {commitment, CommitmentId}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aec_id:specialize_type(AccountId),
    commitment = aec_id:specialize_type(CommitmentId),
    #ns_preclaim_tx{account    = AccountId,
                    nonce      = Nonce,
                    commitment = CommitmentId,
                    fee        = Fee,
                    ttl        = TTL}.

serialization_template(?NAME_PRECLAIM_TX_VSN) ->
    [ {account, id}
    , {nonce, int}
    , {commitment, id}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_preclaim_tx{nonce      = Nonce,
                           fee        = Fee,
                           ttl        = TTL} = Tx) ->
    #{<<"vsn">>        => version(),
      <<"account">>    => aec_base58c:encode(account_pubkey, account(Tx)),
      <<"nonce">>      => Nonce,
      <<"commitment">> => aec_base58c:encode(commitment, commitment(Tx)),
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account(tx()) -> aec_keys:pubkey().
account(#ns_preclaim_tx{account = AccountPubKey}) ->
    aec_id:specialize(AccountPubKey, account).

-spec commitment(tx()) -> binary().
commitment(#ns_preclaim_tx{commitment = Commitment}) ->
    aec_id:specialize(Commitment, commitment).

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_not_commitment(Commitment, Trees) ->
    NSTree = aec_trees:ns(Trees),
    case aens_state_tree:lookup_commitment(Commitment, NSTree) of
        {value, _Commitment} -> {error, commitment_already_present};
        none -> ok
    end.

version() ->
    ?NAME_PRECLAIM_TX_VSN.
