%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for name objects
%%% @end
%%%=============================================================================

-module(aens_names).

%% API
-export([id/1,
         new/3,
         update/3,
         revoke/3,
         transfer/2,
         serialize/1,
         deserialize/2
        ]).

%% Getters
-export([owner/1,
         status/1,
         expires/1,
         pointers/1,
         client_ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type name_status() :: claimed | revoked.

-record(name,
        {id              :: aec_id:id(),
         owner           :: aec_keys:pubkey(),
         expires         :: aec_blocks:height(),
         status          :: name_status(),
         client_ttl = 0  :: integer(),
         pointers   = [] :: list()}).

-opaque name() :: #name{}.

-type id() :: binary().
-type serialized() :: binary().

-export_type([id/0,
              name/0,
              serialized/0]).

-define(NAME_TYPE, name).
-define(NAME_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(name()) -> aens_hash:name_hash().
id(N) ->
    hash(N).

-spec new(aens_claim_tx:tx(), non_neg_integer(), aec_blocks:height()) -> name().
new(ClaimTx, Expiration, BlockHeight) ->
    Expires    = BlockHeight + Expiration,
    Name       = aens_claim_tx:name(ClaimTx),
    {ok, Hash} = aens:get_name_hash(Name),
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    #name{id      = aec_id:create(name, Hash),
          owner   = aens_claim_tx:account(ClaimTx),
          expires = Expires,
          status  = claimed}.

-spec update(aens_update_tx:tx(), name(), aec_blocks:height()) -> name().
update(UpdateTx, Name, BlockHeight) ->
    Expires = BlockHeight + aens_update_tx:name_ttl(UpdateTx),
    Name#name{expires    = Expires,
              client_ttl = aens_update_tx:client_ttl(UpdateTx),
              pointers   = aens_update_tx:pointers(UpdateTx)}.

-spec revoke(name(), non_neg_integer(), aec_blocks:height()) -> name().
revoke(Name, Expiration, BlockHeight) ->
    Expires = BlockHeight + Expiration,
    Name#name{status  = revoked,
              expires = Expires}.

-spec transfer(aens_transfer_tx:tx(), name()) -> name().
transfer(TransferTx, Name) ->
    Name#name{owner = aens_transfer_tx:recipient_account(TransferTx)}.

-spec serialize(name()) -> binary().
serialize(#name{} = N) ->
    aec_object_serialization:serialize(
      ?NAME_TYPE,
      ?NAME_VSN,
      serialization_template(?NAME_VSN),
      [ {owner, owner(N)}
      , {expires, expires(N)}
      , {status, atom_to_binary(status(N), utf8)}
      , {client_ttl, client_ttl(N)}
      , {pointers, jsx:encode(pointers(N))}]). %% TODO: This might be ambigous

-spec deserialize(aens_hash:name_hash(), binary()) -> name().
deserialize(Hash, Bin) ->
    [ {owner, Owner}
    , {expires, Expires}
    , {status, Status}
    , {client_ttl, CTTL}
    , {pointers, Pointers}
    ] = aec_object_serialization:deserialize(
          ?NAME_TYPE,
          ?NAME_VSN,
          serialization_template(?NAME_VSN),
          Bin),
    #name{id         = aec_id:create(name, Hash),
          owner      = Owner,
          expires    = Expires,
          status     = binary_to_existing_atom(Status, utf8),
          client_ttl = CTTL,
          pointers   = jsx:decode(Pointers)}. %% TODO: This might be ambigous

serialization_template(?NAME_VSN) ->
    [ {owner, binary}
    , {expires, int}
    , {status, binary}
    , {client_ttl, int}
    , {pointers, binary} %% TODO: This needs to be stricter
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec owner(name()) -> aec_keys:pubkey().
owner(N) -> N#name.owner.

-spec status(name()) -> name_status().
status(N) -> N#name.status.

-spec expires(name()) -> aec_blocks:height().
expires(N) -> N#name.expires.

-spec pointers(name()) -> list().
pointers(N) -> N#name.pointers.

-spec client_ttl(name()) -> integer().
client_ttl(N) -> N#name.client_ttl.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec hash(name()) -> aens_hash:name_hash().
hash(N) ->
    aec_id:specialize(N#name.id, name).
