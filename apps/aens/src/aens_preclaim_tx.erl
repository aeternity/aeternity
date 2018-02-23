%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System pre-claim transaction
%%% @end
%%%=============================================================================

-module(aens_preclaim_tx).

-include("ns_txs.hrl").
-include_lib("apps/aecore/include/common.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         accounts/1,
         signers/1,
         serialize/1,
         deserialize/1,
         for_client/1
        ]).

%% Getters
-export([account/1,
         commitment/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_PRECLAIM_TX_VSN, 1).

-opaque tx() :: #ns_preclaim_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account    := AccountPubKey,
      nonce      := Nonce,
      commitment := Commitment,
      fee        := Fee}) ->
    Tx = #ns_preclaim_tx{account    = AccountPubKey,
                         nonce      = Nonce,
                         commitment = Commitment,
                         fee        = Fee},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> integer().
fee(#ns_preclaim_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_preclaim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_preclaim_tx{account = AccountPubKey, nonce = Nonce,
                      fee = Fee, commitment = Commitment}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_not_commitment(Commitment, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#ns_preclaim_tx{account = AccountPubKey, fee = Fee,
                        nonce = Nonce} = PreclaimTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NSTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    TTL = aec_governance:name_preclaim_expiration(),
    Commitment = aens_commitments:new(PreclaimTx, TTL, Height),
    NSTree1 = aens_state_tree:enter_commitment(Commitment, NSTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NSTree1),

    {ok, Trees2}.

-spec accounts(tx()) -> [pubkey()].
accounts(#ns_preclaim_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec signers(tx()) -> [pubkey()].
signers(#ns_preclaim_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(tx()) -> list(map()).
serialize(#ns_preclaim_tx{account    = AccountPubKey,
                          nonce      = Nonce,
                          commitment = Commitment,
                          fee        = Fee}) ->
    [#{<<"vsn">>        => version()},
     #{<<"account">>    => AccountPubKey},
     #{<<"nonce">>      => Nonce},
     #{<<"commitment">> => Commitment},
     #{<<"fee">>        => Fee}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>        := ?NAME_PRECLAIM_TX_VSN},
             #{<<"account">>    := AccountPubKey},
             #{<<"nonce">>      := Nonce},
             #{<<"commitment">> := Commitment},
             #{<<"fee">>        := Fee}]) ->
    #ns_preclaim_tx{account    = AccountPubKey,
                    nonce      = Nonce,
                    commitment = Commitment,
                    fee        = Fee}.

-spec for_client(tx()) -> map().
for_client(#ns_preclaim_tx{account    = AccountPubKey,
                           nonce      = Nonce,
                           commitment = Commitment,
                           fee        = Fee}) ->
    #{<<"vsn">>        => version(),
      <<"account">>    => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>      => Nonce,
      <<"commitment">> => aec_base58c:encode(commitment, Commitment),
      <<"fee">>        => Fee}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account(tx()) -> pubkey().
account(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec commitment(tx()) -> binary().
commitment(#ns_preclaim_tx{commitment = Commitment}) ->
    Commitment.

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
