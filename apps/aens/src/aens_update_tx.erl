%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System prolong transaction
%%% @end
%%%=============================================================================

-module(aens_update_tx).

-include("ns_txs.hrl").
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
-export([name_ttl/1,
         pointers/1,
         ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_UPDATE_TX_TYPE, <<"name_update">>).
-define(NAME_UPDATE_TX_VSN, 1).

-opaque update_tx() :: #ns_update_tx{}.

-export_type([update_tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, update_tx()}.
new(#{account   := AccountPubKey,
      nonce     := Nonce,
      name_hash := NameHash,
      name_ttl  := NameTTL,
      pointers  := Pointers,
      ttl       := TTL,
      fee       := Fee}) ->
    {ok, #ns_update_tx{account   = AccountPubKey,
                       nonce     = Nonce,
                       name_hash = NameHash,
                       name_ttl  = NameTTL,
                       pointers  = Pointers,
                       ttl       = TTL,
                       fee       = Fee}}.

-spec fee(update_tx()) -> integer().
fee(#ns_update_tx{fee = Fee}) ->
    Fee.

-spec nonce(update_tx()) -> non_neg_integer().
nonce(#ns_update_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(update_tx()) -> pubkey().
origin(#ns_update_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(update_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#ns_update_tx{account = AccountPubKey, nonce = Nonce,
                    fee = Fee, name_hash = NameHash, ttl = TTL}, Trees, Height) ->
    Checks =
        [fun() -> check_ttl(TTL) end,
         fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aens_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(update_tx(), trees(), height()) -> {ok, trees()}.
process(#ns_update_tx{account = AccountPubKey, nonce = Nonce, fee = Fee,
                      name_hash = NameHash} = UpdateTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NSTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = aens_state_tree:get_name(NameHash, NSTree0),
    Name1 = aens_names:update(UpdateTx, Name0, Height),
    NSTree1 = aens_state_tree:enter_name(Name1, NSTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NSTree1),

    {ok, Trees2}.

-spec signers(update_tx()) -> [pubkey()].
signers(#ns_update_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(update_tx()) -> list(map()).
serialize(#ns_update_tx{account   = AccountPubKey,
                        nonce     = Nonce,
                        name_hash = NameHash,
                        name_ttl  = NameTTL,
                        pointers  = Pointers,
                        ttl       = TTL,
                        fee       = Fee}) ->
    [#{<<"type">>     => type()},
     #{<<"vsn">>      => version()},
     #{<<"account">>  => AccountPubKey},
     #{<<"nonce">>    => Nonce},
     #{<<"hash">>     => NameHash},
     #{<<"name_ttl">> => NameTTL},
     #{<<"pointers">> => Pointers},
     #{<<"ttl">>      => TTL},
     #{<<"fee">>      => Fee}].

-spec deserialize(list(map())) -> update_tx().
deserialize([#{<<"type">>     := ?NAME_UPDATE_TX_TYPE},
             #{<<"vsn">>      := ?NAME_UPDATE_TX_VSN},
             #{<<"account">>  := AccountPubKey},
             #{<<"nonce">>    := Nonce},
             #{<<"hash">>     := NameHash},
             #{<<"name_ttl">> := NameTTL},
             #{<<"pointers">> := Pointers},
             #{<<"ttl">>      := TTL},
             #{<<"fee">>      := Fee}]) ->
    #ns_update_tx{account   = AccountPubKey,
                  nonce     = Nonce,
                  name_hash = NameHash,
                  name_ttl  = NameTTL,
                  pointers  = Pointers,
                  ttl       = TTL,
                  fee       = Fee}.

-spec type() -> binary().
type() ->
    ?NAME_UPDATE_TX_TYPE.

-spec for_client(update_tx()) -> map().
for_client(#ns_update_tx{}) ->
    %% TODO: Implement it, this is something for HTTP API
    #{}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec name_ttl(#ns_update_tx{}) -> integer().
name_ttl(#ns_update_tx{name_ttl = NameTTL}) ->
    NameTTL.

-spec pointers(update_tx()) -> binary().
pointers(#ns_update_tx{pointers = Pointers}) ->
    Pointers.

-spec ttl(update_tx()) -> non_neg_integer().
ttl(#ns_update_tx{ttl = TTL}) ->
    TTL.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_ttl(TTL) ->
    case TTL =< aec_governance:name_claim_max_expiration() of
        true ->
            ok;
        false ->
            {error, ttl_too_high}
    end.

version() ->
    ?NAME_UPDATE_TX_VSN.
