%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for name objects
%%% @end
%%%=============================================================================

-module(aens_names).

%% API
-export([new/3,
         update/3,
         revoke/3,
         transfer_to/2,
         serialize/1,
         deserialize/2
        ]).

%% Getters
-export([id/1,
         hash/1,
         owner_pubkey/1,
         status/1,
         expires/1,
         pointers/1,
         client_ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type id()         :: aec_id:id().
-type status()     :: claimed | revoked.
-type serialized() :: binary().

-record(name,
        {id         :: id(),
         owner_id   :: id(),
         expires    :: aec_blocks:height(),
         status     :: status(),
         client_ttl :: integer(),
         pointers   :: list(aens_pointer:pointer())}).

-opaque name()      :: #name{}.

-export_type([name/0,
              id/0,
              status/0,
              serialized/0]).

-define(NAME_TYPE, name).
-define(NAME_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aens_claim_tx:tx(), non_neg_integer(), aec_blocks:height()) -> name().
new(ClaimTx, Expiration, BlockHeight) ->
    Expires    = BlockHeight + Expiration,
    Name       = aens_claim_tx:name(ClaimTx),
    {ok, NameHash} = aens:get_name_hash(Name),
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    #name{id         = aec_id:create(name, NameHash),
          owner_id   = aens_claim_tx:account_id(ClaimTx),
          expires    = Expires,
          status     = claimed,
          client_ttl = 0,
          pointers   = []}.

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

-spec transfer_to(aec_keys:pubkey(), name()) -> name().
transfer_to(Pubkey, Name) ->
    Name#name{owner_id = aec_id:create(account, Pubkey)}.

-spec serialize(name()) -> binary().
serialize(#name{owner_id   = OwnerId,
                expires    = Expires,
                status     = Status,
                client_ttl = ClientTTL,
                pointers   = Pointers}) ->
    aec_object_serialization:serialize(
      ?NAME_TYPE,
      ?NAME_VSN,
      serialization_template(?NAME_VSN),
      [ {owner_id, OwnerId}
      , {expires, Expires}
      , {status, atom_to_binary(Status, utf8)}
      , {client_ttl, ClientTTL}
      , {pointers, [{aens_pointer:key(P), aens_pointer:id(P)} || P <- Pointers]}]).

-spec deserialize(aens_hash:name_hash(), binary()) -> name().
deserialize(NameHash, Bin) ->
    [ {owner_id, OwnerId}
    , {expires, Expires}
    , {status, Status}
    , {client_ttl, ClientTTL}
    , {pointers, Pointers}
    ] = aec_object_serialization:deserialize(
          ?NAME_TYPE,
          ?NAME_VSN,
          serialization_template(?NAME_VSN),
          Bin),
    #name{id         = aec_id:create(name, NameHash),
          owner_id   = OwnerId,
          expires    = Expires,
          status     = binary_to_existing_atom(Status, utf8),
          client_ttl = ClientTTL,
          pointers   = [aens_pointer:new(Key, Id) || {Key, Id} <- Pointers]}.

serialization_template(?NAME_VSN) ->
    [ {owner_id, id}
    , {expires, int}
    , {status, binary}
    , {client_ttl, int}
    , {pointers, [{binary, id}]}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec id(name()) -> id().
id(#name{id = Id}) ->
    Id.

-spec hash(name()) -> aens_hash:name_hash().
hash(#name{id = Id}) ->
    aec_id:specialize(Id, name).

-spec owner_pubkey(name()) -> aec_keys:pubkey().
owner_pubkey(#name{owner_id = OwnerId}) ->
    aec_id:specialize(OwnerId, account).

-spec status(name()) -> status().
status(#name{status = Status}) ->
    Status.

-spec expires(name()) -> aec_blocks:height().
expires(#name{expires = Expires}) ->
    Expires.

-spec pointers(name()) -> list(aens_pointer:pointer()).
pointers(#name{pointers = Pointers}) ->
    Pointers.

-spec client_ttl(name()) -> integer().
client_ttl(#name{client_ttl = ClientTTL}) ->
    ClientTTL.

