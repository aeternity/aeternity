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
         gas/1,
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

-export([account_id/1,
         name_id/1
        ]).

%% Getters
-ifdef(TEST).
-export([recipient_pubkey/1]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_TRANSFER_TX_VSN, 1).
-define(NAME_TRANSFER_TX_TYPE, name_transfer_tx).

-record(ns_transfer_tx, {
          account_id   :: aec_id:id(),
          nonce        :: integer(),
          name_id      :: aec_id:id(),
          recipient_id :: aec_id:id(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_transfer_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id   := AccountId,
      nonce        := Nonce,
      name_id      := NameId,
      recipient_id := RecipientId,
      fee          := Fee} = Args) ->
    account = aec_id:specialize_type(AccountId),
    name    = aec_id:specialize_type(NameId),
    case aec_id:specialize_type(RecipientId) of
        account -> ok;
        name    -> ok
    end,
    Tx = #ns_transfer_tx{account_id   = AccountId,
                         nonce        = Nonce,
                         name_id      = NameId,
                         recipient_id = RecipientId,
                         fee          = Fee,
                         ttl          = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_TRANSFER_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_transfer_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_transfer_tx{}) ->
    aec_governance:tx_base_gas(ns_transfer_tx).

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_transfer_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_transfer_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_transfer_tx{} = Tx) ->
    account_pubkey(Tx).

account_pubkey(#ns_transfer_tx{account_id = AccountId}) ->
    aec_id:specialize(AccountId, account).

name_hash(#ns_transfer_tx{name_id = NameId}) ->
    aec_id:specialize(NameId, name).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_transfer_tx{nonce = Nonce,
                      fee   = Fee} = Tx,
      Trees,_Env) ->
    AccountPubKey = account_pubkey(Tx),
    NameHash = name_hash(Tx),

    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> aens_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end,
         fun() -> check_recipient_resolvement(Tx, Trees) end
        ],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#ns_transfer_tx{fee = Fee, nonce = Nonce} = TransferTx,
        Trees0,_Env) ->
    NameHash = name_hash(TransferTx),
    AccountPubKey = account_pubkey(TransferTx),
    AccountsTree0 = aec_trees:accounts(Trees0),
    NamesTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = aens_state_tree:get_name(NameHash, NamesTree0),
    {ok, RecipientPubkey} = resolve_recipient_pubkey(TransferTx, Trees0),
    Name1 = aens_names:transfer_to(RecipientPubkey, Name0),
    NamesTree1 = aens_state_tree:enter_name(Name1, NamesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NamesTree1),

    {ok, Trees2}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_transfer_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_transfer_tx{account_id   = AccountId,
                          nonce        = Nonce,
                          name_id      = NameId,
                          recipient_id = RecipientId,
                          fee          = Fee,
                          ttl          = TTL}) ->
    {version(),
     [ {account_id, AccountId}
     , {nonce, Nonce}
     , {name_id, NameId}
     , {recipient_id, RecipientId}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_TRANSFER_TX_VSN,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name_id, NameId}
            , {recipient_id, RecipientId}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aec_id:specialize_type(AccountId),
    name    = aec_id:specialize_type(NameId),
    account = aec_id:specialize_type(RecipientId),
    #ns_transfer_tx{account_id   = AccountId,
                    nonce        = Nonce,
                    name_id      = NameId,
                    recipient_id = RecipientId,
                    fee          = Fee,
                    ttl          = TTL}.

serialization_template(?NAME_TRANSFER_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {name_id, id}
    , {recipient_id, id}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_transfer_tx{account_id   = AccountId,
                           nonce        = Nonce,
                           name_id      = NameId,
                           recipient_id = RecipientId,
                           fee          = Fee,
                           ttl          = TTL}) ->
    #{<<"account_id">>   => aec_base58c:encode(id_hash, AccountId),
      <<"nonce">>        => Nonce,
      <<"name_id">>      => aec_base58c:encode(id_hash, NameId),
      <<"recipient_id">> => aec_base58c:encode(id_hash, RecipientId),
      <<"fee">>          => Fee,
      <<"ttl">>          => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-ifdef(TEST).
recipient_pubkey(#ns_transfer_tx{recipient_id = RecipientId}) ->
    aec_id:specialize(RecipientId, account).
-endif.

-spec account_id(tx()) -> aec_id:id().
account_id(#ns_transfer_tx{account_id = AccountId}) ->
    AccountId.

-spec name_id(tx()) -> aec_id:id().
name_id(#ns_transfer_tx{name_id = NameId}) ->
    NameId.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_recipient_resolvement(Tx, Trees) ->
    case resolve_recipient_pubkey(Tx, Trees) of
        {ok,_Pubkey} -> ok;
        {error, _} = E -> E
    end.

resolve_recipient_pubkey(Tx, Trees) ->
    case resolve_recipient(Tx, Trees) of
        {id, Id} ->
            {_IdType, Pubkey} = aec_id:specialize(Id),
            {ok, Pubkey};
        {pubkey, Pubkey} ->
            {ok, Pubkey};
        {error, _Rsn} = Error ->
            Error
    end.

resolve_recipient(#ns_transfer_tx{recipient_id = RecipientId}, Trees) ->
    case aec_id:specialize(RecipientId) of
        {account, Pubkey} ->
            {pubkey, Pubkey};
        %% TODO: A registered name has pointers, a pointer has a key of type binary() and
        %% id of type aec_id:id(). To find out what id to get from all the pointers related
        %% to the name we need the key. <<"account_pubkey">> is hard-coded and might not be present
        %% for the given name/namehash.
        {name, NameHash} ->
            Key = <<"account_pubkey">>,
            case aens:resolve_from_hash(Key, NameHash, aec_trees:ns(Trees)) of
                {ok, Id} -> {id, Id};
                {error, _Rsn} = Error -> Error
            end
    end.

version() ->
    ?NAME_TRANSFER_TX_VSN.
