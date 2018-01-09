%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System pre-claim transaction
%%% @end
%%%=============================================================================

-module(aens_preclaim_tx).

-include("ns_txs.hrl").
-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/trees.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/1,
         serialize/1,
         deserialize/1,
         type/0,
         for_client/1
        ]).

%% Getters
-export([account/1,
         commitment/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_PRECLAIM_TX_TYPE, <<"name_preclaim">>).
-define(NAME_PRECLAIM_TX_VSN, 1).

-opaque preclaim_tx() :: #ns_preclaim_tx{}.

-export_type([preclaim_tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, preclaim_tx()}.
new(#{account    := AccountPubKey,
      nonce      := Nonce,
      commitment := Commitment,
      fee        := Fee}) ->
    {ok, #ns_preclaim_tx{account    = AccountPubKey,
                         nonce      = Nonce,
                         commitment = Commitment,
                         fee        = Fee}}.

-spec fee(preclaim_tx()) -> integer().
fee(#ns_preclaim_tx{fee = Fee}) ->
    Fee.

-spec nonce(preclaim_tx()) -> non_neg_integer().
nonce(#ns_preclaim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(preclaim_tx()) -> pubkey().
origin(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(preclaim_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#ns_preclaim_tx{account = AccountPubKey, nonce = Nonce,
                      fee = Fee, commitment = Commitment}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_not_commitment(Commitment, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(preclaim_tx(), trees(), height()) -> {ok, trees()}.
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

-spec signers(preclaim_tx()) -> [pubkey()].
signers(#ns_preclaim_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(preclaim_tx()) -> list(map()).
serialize(#ns_preclaim_tx{account    = AccountPubKey,
                          nonce      = Nonce,
                          commitment = Commitment,
                          fee        = Fee}) ->
    [#{<<"type">>       => type()},
     #{<<"vsn">>        => version()},
     #{<<"account">>    => AccountPubKey},
     #{<<"nonce">>      => Nonce},
     #{<<"commitment">> => Commitment},
     #{<<"fee">>        => Fee}].

-spec deserialize(list(map())) -> preclaim_tx().
deserialize([#{<<"type">>       := ?NAME_PRECLAIM_TX_TYPE},
             #{<<"vsn">>        := ?NAME_PRECLAIM_TX_VSN},
             #{<<"account">>    := AccountPubKey},
             #{<<"nonce">>      := Nonce},
             #{<<"commitment">> := Commitment},
             #{<<"fee">>        := Fee}]) ->
    #ns_preclaim_tx{account    = AccountPubKey,
                    nonce      = Nonce,
                    commitment = Commitment,
                    fee        = Fee}.

-spec type() -> binary().
type() ->
    ?NAME_PRECLAIM_TX_TYPE.

-spec for_client(preclaim_tx()) -> map().
for_client(#ns_preclaim_tx{account    = AccountPubKey,
                           nonce      = Nonce,
                           commitment = Commitment,
                           fee        = Fee}) ->
    #{<<"type">>       => <<"NamePreclaimTxObject">>, % swagger schema name
      <<"vsn">>        => version(),
      <<"account">>    => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>      => Nonce,
      <<"commitment">> => aec_base58c:encode(commitment, Commitment),
      <<"fee">>        => Fee}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account(preclaim_tx()) -> pubkey().
account(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec commitment(preclaim_tx()) -> binary().
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
