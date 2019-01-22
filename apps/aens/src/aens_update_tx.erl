%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System prolong transaction
%%% @end
%%%=============================================================================

-module(aens_update_tx).

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

%% Getters
-export([name_ttl/1,
         pointers/1,
         client_ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_UPDATE_TX_VSN, 1).
-define(NAME_UPDATE_TX_TYPE, name_update_tx).

-record(ns_update_tx, {
          account_id :: aec_id:id(),
          nonce      :: integer(),
          name_id    :: aec_id:id(),
          name_ttl   :: integer(),
          pointers   :: list(aens_pointer:pointer()),
          client_ttl :: integer(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_update_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id := AccountId,
      nonce      := Nonce,
      name_id    := NameId,
      name_ttl   := NameTTL,
      pointers   := Pointers,
      client_ttl := ClientTTL,
      fee        := Fee} = Args) ->
    account = aec_id:specialize_type(AccountId),
    name    = aec_id:specialize_type(NameId),
    %% TODO: check pointers: length, unique keys? unique ids?
    Tx = #ns_update_tx{account_id = AccountId,
                       nonce      = Nonce,
                       name_id    = NameId,
                       name_ttl   = NameTTL,
                       pointers   = Pointers,
                       client_ttl = ClientTTL,
                       fee        = Fee,
                       ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_UPDATE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_update_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_update_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_update_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_update_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_update_tx{} = Tx) ->
    account_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_update_tx{} = Tx, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#ns_update_tx{} = UTx, Trees, Env) ->
    Instructions =
        aec_tx_processor:name_update_tx_instructions(
          account_pubkey(UTx),
          name_hash(UTx),
          name_ttl(UTx),
          client_ttl(UTx),
          pointers(UTx),
          fee(UTx),
          nonce(UTx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_update_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_update_tx{account_id = AccountId,
                        nonce      = Nonce,
                        name_id    = NameId,
                        name_ttl   = NameTTL,
                        pointers   = Pointers,
                        client_ttl = ClientTTL,
                        fee        = Fee,
                        ttl        = TTL}) ->
    {version(),
     [ {account_id, AccountId}
     , {nonce, Nonce}
     , {name_id, NameId}
     , {name_ttl, NameTTL}
     , {pointers, [{aens_pointer:key(P), aens_pointer:id(P)} || P <- Pointers]}
     , {client_ttl, ClientTTL}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_UPDATE_TX_VSN,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name_id, NameId}
            , {name_ttl, NameTTL}
            , {pointers, Pointers}
            , {client_ttl, ClientTTL}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aec_id:specialize_type(AccountId),
    name = aec_id:specialize_type(NameId),
    #ns_update_tx{account_id = AccountId,
                  nonce      = Nonce,
                  name_id    = NameId,
                  name_ttl   = NameTTL,
                  pointers   = [aens_pointer:new(Key, Id) || {Key, Id} <- Pointers],
                  client_ttl = ClientTTL,
                  fee        = Fee,
                  ttl        = TTL}.

serialization_template(?NAME_UPDATE_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {name_id, id}
    , {name_ttl, int}
    , {pointers, [{binary, id}]}
    , {client_ttl, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_update_tx{account_id = AccountId,
                         nonce      = Nonce,
                         name_id    = NameId,
                         name_ttl   = NameTTL,
                         pointers   = Pointers,
                         client_ttl = ClientTTL,
                         fee        = Fee,
                         ttl        = TTL}) ->
    #{<<"account_id">> => aehttp_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>      => Nonce,
      <<"name_id">>    => aehttp_api_encoder:encode(id_hash, NameId),
      <<"name_ttl">>   => NameTTL,
      <<"pointers">>   => [aens_pointer:serialize_for_client(P) || P <- Pointers],
      <<"client_ttl">> => ClientTTL,
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec name_ttl(tx()) -> integer().
name_ttl(#ns_update_tx{name_ttl = NameTTL}) ->
    NameTTL.

-spec pointers(tx()) -> list(aens_pointer:pointer()).
pointers(#ns_update_tx{pointers = Pointers}) ->
    Pointers.

-spec client_ttl(tx()) -> non_neg_integer().
client_ttl(#ns_update_tx{client_ttl = ClientTTL}) ->
    ClientTTL.

%%%===================================================================
%%% Internal functions
%%%===================================================================

account_pubkey(#ns_update_tx{account_id = AccountId}) ->
    aec_id:specialize(AccountId, account).

name_hash(#ns_update_tx{name_id = NameId}) ->
    aec_id:specialize(NameId, name).

version() ->
    ?NAME_UPDATE_TX_VSN.
