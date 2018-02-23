%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System transfer transaction
%%% @end
%%%=============================================================================

-module(aens_transfer_tx).

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
-export([recipient_account/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_TRANSFER_TX_VSN, 1).

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
      fee               := Fee}) ->
    Tx = #ns_transfer_tx{account           = AccountPubKey,
                         nonce             = Nonce,
                         name_hash         = NameHash,
                         recipient_account = RecipientAccountPubKey,
                         fee               = Fee},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> integer().
fee(#ns_transfer_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_transfer_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#ns_transfer_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_transfer_tx{account = AccountPubKey, nonce = Nonce,
                      fee = Fee, name_hash = NameHash}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aens_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#ns_transfer_tx{account = AccountPubKey, fee = Fee,
                        name_hash = NameHash, nonce = Nonce} = TransferTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NamesTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = aens_state_tree:get_name(NameHash, NamesTree0),
    Name1 = aens_names:transfer(TransferTx, Name0),
    NamesTree1 = aens_state_tree:enter_name(Name1, NamesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NamesTree1),

    {ok, Trees2}.

-spec accounts(tx()) -> [pubkey()].
accounts(#ns_transfer_tx{account = AccountPubKey,
                         recipient_account = RecipientPubKey}) ->
    [AccountPubKey, RecipientPubKey].

-spec signers(tx()) -> [pubkey()].
signers(#ns_transfer_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(tx()) -> list(map()).
serialize(#ns_transfer_tx{account           = AccountPubKey,
                          nonce             = Nonce,
                          name_hash         = NameHash,
                          recipient_account = RecipientAccountPubKey,
                          fee               = Fee}) ->
    [#{<<"vsn">>       => version()},
     #{<<"account">>   => AccountPubKey},
     #{<<"nonce">>     => Nonce},
     #{<<"hash">>      => NameHash},
     #{<<"recipient">> => RecipientAccountPubKey},
     #{<<"fee">>       => Fee}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>       := ?NAME_TRANSFER_TX_VSN},
             #{<<"account">>   := AccountPubKey},
             #{<<"nonce">>     := Nonce},
             #{<<"hash">>      := NameHash},
             #{<<"recipient">> := RecipientAccountPubKey},
             #{<<"fee">>       := Fee}]) ->
    #ns_transfer_tx{account           = AccountPubKey,
                    nonce             = Nonce,
                    name_hash         = NameHash,
                    recipient_account = RecipientAccountPubKey,
                    fee               = Fee}.

-spec for_client(tx()) -> map().
for_client(#ns_transfer_tx{account           = AccountPubKey,
                           nonce             = Nonce,
                           name_hash         = NameHash,
                           recipient_account = RecipientPubKey,
                           fee               = Fee}) ->
    #{<<"vsn">>              => version(),
      <<"account">>          => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>            => Nonce,
      <<"name_hash">>        => aec_base58c:encode(name, NameHash),
      <<"recipient_pubkey">> => aec_base58c:encode(account_pubkey, RecipientPubKey),
      <<"fee">>              => Fee}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec recipient_account(tx()) -> pubkey().
recipient_account(#ns_transfer_tx{recipient_account = AccountPubKey}) ->
    AccountPubKey.

%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?NAME_TRANSFER_TX_VSN.
