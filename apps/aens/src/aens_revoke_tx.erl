%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System revoke transaction
%%% @end
%%%=============================================================================
-module(aens_revoke_tx).

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

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_REVOKE_TX_TYPE, <<"name_revoke">>).
-define(NAME_REVOKE_TX_VSN, 1).

-opaque revoke_tx() :: #ns_revoke_tx{}.

-export_type([revoke_tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, revoke_tx()}.
new(#{account   := AccountPubKey,
      nonce     := Nonce,
      name_hash := NameHash,
      fee       := Fee}) ->
    {ok, #ns_revoke_tx{account   = AccountPubKey,
                       nonce     = Nonce,
                       name_hash = NameHash,
                       fee       = Fee}}.


-spec fee(revoke_tx()) -> integer().
fee(#ns_revoke_tx{fee = Fee}) ->
    Fee.

-spec nonce(revoke_tx()) -> non_neg_integer().
nonce(#ns_revoke_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(revoke_tx()) -> pubkey().
origin(#ns_revoke_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(revoke_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#ns_revoke_tx{account = AccountPubKey, nonce = Nonce,
                    fee = Fee, name_hash = NameHash}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aens_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(revoke_tx(), trees(), height()) -> {ok, trees()}.
process(#ns_revoke_tx{account = AccountPubKey, fee = Fee,
                      name_hash = NameHash, nonce = Nonce}, Trees0, Height) ->
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

-spec signers(revoke_tx()) -> [pubkey()].
signers(#ns_revoke_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(revoke_tx()) -> list(map()).
serialize(#ns_revoke_tx{account   = AccountPubKey,
                        nonce     = Nonce,
                        name_hash = NameHash,
                        fee       = Fee}) ->
    [#{<<"type">>    => type()},
     #{<<"vsn">>     => version()},
     #{<<"account">> => AccountPubKey},
     #{<<"nonce">>   => Nonce},
     #{<<"hash">>    => NameHash},
     #{<<"fee">>     => Fee}].

-spec deserialize(list(map())) -> revoke_tx().
deserialize([#{<<"type">>    := ?NAME_REVOKE_TX_TYPE},
             #{<<"vsn">>     := ?NAME_REVOKE_TX_VSN},
             #{<<"account">> := AccountPubKey},
             #{<<"nonce">>   := Nonce},
             #{<<"hash">>    := NameHash},
             #{<<"fee">>     := Fee}]) ->
    #ns_revoke_tx{account   = AccountPubKey,
                  nonce     = Nonce,
                  name_hash = NameHash,
                  fee       = Fee}.

-spec type() -> binary().
type() ->
    ?NAME_REVOKE_TX_TYPE.

-spec for_client(revoke_tx()) -> map().
for_client(#ns_revoke_tx{account   = AccountPubKey,
                         nonce     = Nonce,
                         name_hash = NameHash,
                         fee       = Fee}) ->
    #{<<"type">>      => <<"NameRevokeTxObject">>, % swagger schema name
      <<"vsn">>       => version(),
      <<"account">>   => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>     => Nonce,
      <<"name_hash">> => aec_base58c:encode(name, NameHash),
      <<"fee">>       => Fee}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?NAME_REVOKE_TX_VSN.
