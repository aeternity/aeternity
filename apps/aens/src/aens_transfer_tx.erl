%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System transfer transaction
%%% @end
%%%=============================================================================

-module(aens_transfer_tx).

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
-export([recipient_account/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_TRANSFER_TX_VSN, 1).
-define(NAME_TRANSFER_TX_TYPE, name_transfer_tx).

-record(ns_transfer_tx, {
          account           :: aec_id:id(),
          nonce             :: integer(),
          name_hash         :: aec_id:id(),
          recipient_account :: aec_id:id(),
          fee               :: integer(),
          ttl               :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_transfer_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account           := AccountPubKey,
      nonce             := Nonce,
      name_hash         := NameHash,
      recipient_account := RecipientAccountPubKey,
      fee               := Fee} = Args) ->
    Tx = #ns_transfer_tx{account           = aec_id:create(account, AccountPubKey),
                         nonce             = Nonce,
                         name_hash         = aec_id:create(name, NameHash),
                         recipient_account = aec_id:create(account, RecipientAccountPubKey),
                         fee               = Fee,
                         ttl               = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_TRANSFER_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_transfer_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_transfer_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_transfer_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_transfer_tx{} = Tx) ->
    account(Tx).

account(#ns_transfer_tx{account = AccountId}) ->
    aec_id:specialize(AccountId, account).

name_hash(#ns_transfer_tx{name_hash = NameId}) ->
    aec_id:specialize(NameId, name).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#ns_transfer_tx{nonce = Nonce, fee = Fee} = Tx,
      _Context, Trees, _Height, _ConsensusVersion) ->
    AccountPubKey = account(Tx),
    NameHash = name_hash(Tx),
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> aens_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#ns_transfer_tx{fee = Fee, nonce = Nonce} = TransferTx,
        _Context, Trees0, _Height, _ConsensusVersion) ->
    NameHash = name_hash(TransferTx),
    AccountPubKey = account(TransferTx),
    AccountsTree0 = aec_trees:accounts(Trees0),
    NamesTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = aens_state_tree:get_name(NameHash, NamesTree0),
    Name1 = aens_names:transfer(TransferTx, Name0),
    NamesTree1 = aens_state_tree:enter_name(Name1, NamesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NamesTree1),

    {ok, Trees2}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_transfer_tx{} = Tx, _) ->
    {ok, [account(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_transfer_tx{account           = AccountId,
                          nonce             = Nonce,
                          name_hash         = NameId,
                          recipient_account = RecipientAccountId,
                          fee               = Fee,
                          ttl               = TTL}) ->
    {version(),
     [ {account, AccountId}
     , {nonce, Nonce}
     , {hash, NameId}
     , {recipient, RecipientAccountId}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_TRANSFER_TX_VSN,
            [ {account, AccountId}
            , {nonce, Nonce}
            , {hash, NameId}
            , {recipient, RecipientAccountId}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    {account, _} = aec_id:specialize(AccountId),
    {name, _}    = aec_id:specialize(NameId),
    {account, _} = aec_id:specialize(RecipientAccountId),
    #ns_transfer_tx{account           = AccountId,
                    nonce             = Nonce,
                    name_hash         = NameId,
                    recipient_account = RecipientAccountId,
                    fee               = Fee,
                    ttl               = TTL}.

serialization_template(?NAME_TRANSFER_TX_VSN) ->
    [ {account, id}
    , {nonce, int}
    , {hash, id}
    , {recipient, id}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_transfer_tx{nonce             = Nonce,
                           fee               = Fee,
                           ttl               = TTL} = Tx) ->
    #{<<"vsn">>              => version(),
      <<"account">>          => aec_base58c:encode(account_pubkey, account(Tx)),
      <<"nonce">>            => Nonce,
      <<"name_hash">>        => aec_base58c:encode(name, name_hash(Tx)),
      <<"recipient_pubkey">> => aec_base58c:encode(account_pubkey, recipient_account(Tx)),
      <<"fee">>              => Fee,
      <<"ttl">>              => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec recipient_account(tx()) -> aec_keys:pubkey().
recipient_account(#ns_transfer_tx{recipient_account = Recipient}) ->
    aec_id:specialize(Recipient, account).

%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?NAME_TRANSFER_TX_VSN.
