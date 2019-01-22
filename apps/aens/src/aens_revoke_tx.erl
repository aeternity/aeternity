%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System revoke transaction
%%% @end
%%%=============================================================================
-module(aens_revoke_tx).

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

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_REVOKE_TX_VSN, 1).
-define(NAME_REVOKE_TX_TYPE, name_revoke_tx).

-record(ns_revoke_tx, {
          account_id :: aec_id:id(),
          nonce      :: integer(),
          name_id    :: aec_id:id(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_revoke_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id := AccountId,
      nonce      := Nonce,
      name_id    := NameId,
      fee        := Fee} = Args) ->
    account = aec_id:specialize_type(AccountId),
    name    = aec_id:specialize_type(NameId),
    Tx = #ns_revoke_tx{account_id = AccountId,
                       nonce      = Nonce,
                       name_id    = NameId,
                       fee        = Fee,
                       ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_REVOKE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_revoke_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_revoke_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_revoke_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_revoke_tx{nonce = Nonce}) ->
    Nonce.

-spec account_id(tx()) -> aec_id:id().
account_id(#ns_revoke_tx{account_id = AccountId}) ->
    AccountId.

-spec name_id(tx()) -> aec_id:id().
name_id(#ns_revoke_tx{name_id = NameId}) ->
    NameId.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_revoke_tx{} = Tx) ->
    account_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_revoke_tx{}, Trees,_Env) ->
    %% Checks are in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#ns_revoke_tx{} = Tx, Trees, Env) ->
    Instructions =
        aec_tx_processor:name_revoke_tx_instructions(
          account_pubkey(Tx), name_hash(Tx), fee(Tx), nonce(Tx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_revoke_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_revoke_tx{account_id = AccountId,
                        nonce      = Nonce,
                        name_id    = NameId,
                        fee        = Fee,
                        ttl        = TTL}) ->
    {version(),
     [ {account_id, AccountId}
     , {nonce, Nonce}
     , {name_id, NameId}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_REVOKE_TX_VSN,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name_id, NameId}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aec_id:specialize_type(AccountId),
    name    = aec_id:specialize_type(NameId),
    #ns_revoke_tx{account_id = AccountId,
                  nonce      = Nonce,
                  name_id    = NameId,
                  fee        = Fee,
                  ttl        = TTL}.

serialization_template(?NAME_REVOKE_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {name_id, id}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_revoke_tx{account_id = AccountId,
                         nonce      = Nonce,
                         name_id    = NameId,
                         fee        = Fee,
                         ttl        = TTL}) ->
    #{<<"account_id">> => aehttp_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>      => Nonce,
      <<"name_id">>    => aehttp_api_encoder:encode(id_hash, NameId),
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

account_pubkey(#ns_revoke_tx{account_id = AccountId}) ->
    aec_id:specialize(AccountId, account).

name_hash(#ns_revoke_tx{name_id = NameId}) ->
    aec_id:specialize(NameId, name).

version() ->
    ?NAME_REVOKE_TX_VSN.

