%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System revoke transaction
%%% @end
%%%=============================================================================
-module(aens_revoke_tx).

-include("ns_txs.hrl").
-include_lib("apps/aecore/include/common.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         nonce/1,
         origin/1,
         check/4,
         process/4,
         accounts/1,
         signers/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_REVOKE_TX_VSN, 1).
-define(NAME_REVOKE_TX_TYPE, name_revoke_tx).

-opaque tx() :: #ns_revoke_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account   := AccountPubKey,
      nonce     := Nonce,
      name_hash := NameHash,
      fee       := Fee}) ->
    Tx = #ns_revoke_tx{account   = AccountPubKey,
                       nonce     = Nonce,
                       name_hash = NameHash,
                       fee       = Fee},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_REVOKE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_revoke_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_revoke_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#ns_revoke_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_revoke_tx{account = AccountPubKey, nonce = Nonce,
                    fee = Fee, name_hash = NameHash}, _Context, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aens_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#ns_revoke_tx{account = AccountPubKey, fee = Fee,
                      name_hash = NameHash, nonce = Nonce}, _Context, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NamesTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    TTL = aec_governance:name_protection_period(),
    Name0 = aens_state_tree:get_name(NameHash, NamesTree0),
    Name1 = aens_names:revoke(Name0, TTL, Height),
    NamesTree1 = aens_state_tree:enter_name(Name1, NamesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NamesTree1),

    {ok, Trees2}.

-spec accounts(tx()) -> [pubkey()].
accounts(#ns_revoke_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec signers(tx()) -> [pubkey()].
signers(#ns_revoke_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_revoke_tx{account   = AccountPubKey,
                        nonce     = Nonce,
                        name_hash = NameHash,
                        fee       = Fee}) ->
    {version(),
     [ {account, AccountPubKey}
     , {nonce, Nonce}
     , {hash, NameHash}
     , {fee, Fee}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_REVOKE_TX_VSN,
            [ {account, AccountPubKey}
            , {nonce, Nonce}
            , {hash, NameHash}
            , {fee, Fee}]) ->
    #ns_revoke_tx{account   = AccountPubKey,
                  nonce     = Nonce,
                  name_hash = NameHash,
                  fee       = Fee}.

serialization_template(?NAME_REVOKE_TX_VSN) ->
    [ {account, binary}
    , {nonce, int}
    , {hash, binary}
    , {fee, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_revoke_tx{account   = AccountPubKey,
                         nonce     = Nonce,
                         name_hash = NameHash,
                         fee       = Fee}) ->
    #{<<"vsn">>       => version(),
      <<"account">>   => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>     => Nonce,
      <<"name_hash">> => aec_base58c:encode(name, NameHash),
      <<"fee">>       => Fee}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?NAME_REVOKE_TX_VSN.
