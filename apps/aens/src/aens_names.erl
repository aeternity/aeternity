%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for name objects
%%% @end
%%%=============================================================================

-module(aens_names).

%% API
-export([new/3,
         update/4,
         revoke/3,
         transfer_to/2,
         serialize/1,
         deserialize/2,
         deserialize_from_fields/3,
         serialization_type/0,
         serialization_template/1
        ]).

%% Getters
-export([id/1,
         hash/1,
         owner_pubkey/1,
         status/1,
         ttl/1,
         pointers/1,
         client_ttl/1]).

-behavior(aens_cache).
%%%===================================================================
%%% Types
%%%===================================================================

-type id()         :: aeser_id:id().
-type status()     :: claimed | revoked.
-type serialized() :: binary().

-record(name,
        {id         :: id(),
         owner_id   :: id(),
         expires_by :: aec_blocks:height(),
         status     :: status(),
         client_ttl :: integer(),
         pointers   :: list(aens_pointer:pointer())}).

-opaque name()      :: #name{}.

-export_type([name/0,
              id/0,
              status/0,
              serialized/0]).

-define(NAME_TYPE, name).
-define(NAME_VSN_1, 1).
-define(NAME_VSN_2, 2).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aens_hash:name_hash(), aec_keys:pubkey(), non_neg_integer()) -> name().
new(NameHash, OwnerPubkey, AbsoluteTTL) ->
    %% TODO: add assertions on fields, similarly to what is done in aeo_oracles:new/2
    #name{id         = aeser_id:create(name, NameHash),
          owner_id   = aeser_id:create(account, OwnerPubkey),
          expires_by = AbsoluteTTL,
          status     = claimed,
          client_ttl = 0,
          pointers   = []}.

-spec update(name(), aec_blocks:height(), integer(), [aens_pointer:pointer()]) -> name().
update(Name, AbsoluteTTL, ClientTTL, Pointers) ->
    Name#name{expires_by = AbsoluteTTL,
              client_ttl = ClientTTL,
              pointers   = Pointers}.

-spec revoke(name(), non_neg_integer(), aec_blocks:height()) -> name().
revoke(Name, Expiration, BlockHeight) ->
    TTL = BlockHeight + Expiration,
    Name#name{status  = revoked,
              expires_by = TTL}.

-spec transfer_to(aec_keys:pubkey(), name()) -> name().
transfer_to(Pubkey, Name) ->
    Name#name{owner_id = aeser_id:create(account, Pubkey)}.

-spec serialize(name()) -> serialized().
serialize(#name{owner_id   = OwnerId,
                expires_by = ExpiresBy,
                status     = Status,
                client_ttl = ClientTTL,
                pointers   = Pointers}) ->
    {Vsn, SerializePtrFun} =
        case aens_pointer:has_raw_data_pointer(Pointers) of
            false -> {?NAME_VSN_1, fun aens_pointer:serialize_pointer_vsn1/1};
            true  -> {?NAME_VSN_2, fun aens_pointer:serialize_pointer_vsn2/1}
        end,
    aeser_chain_objects:serialize(
      ?NAME_TYPE,
      Vsn,
      serialization_template(Vsn),
      [ {owner_id, OwnerId}
      , {expires_by, ExpiresBy}
      , {status, atom_to_binary(Status, utf8)}
      , {client_ttl, ClientTTL}
      , {pointers, [SerializePtrFun(P) || P <- Pointers]}]).

-spec deserialize(aens_hash:name_hash(), binary()) -> name().
deserialize(NameHash, Bin) ->
    {?NAME_TYPE, Vsn, RawFields} =
        aeser_chain_objects:deserialize_type_and_vsn(Bin),
    Template = serialization_template(Vsn),
    Fields   = aeserialization:decode_fields(Template, RawFields),
    deserialize_from_fields(Vsn, NameHash, Fields).

deserialize_from_fields(Vsn, NameHash,
    [ {owner_id, OwnerId}
    , {expires_by, ExpiresBy}
    , {status, Status}
    , {client_ttl, ClientTTL}
    , {pointers, Pointers}]) ->
    DeserializePtrFun =
        case Vsn of
            ?NAME_VSN_1 -> fun aens_pointer:deserialize_pointer_vsn1/1;
            ?NAME_VSN_2 -> fun aens_pointer:deserialize_pointer_vsn2/1
        end,
    #name{id         = aeser_id:create(name, NameHash),
          owner_id   = OwnerId,
          expires_by = ExpiresBy,
          status     = binary_to_existing_atom(Status, utf8),
          client_ttl = ClientTTL,
          pointers   = [DeserializePtrFun(P) || P <- Pointers]}.

serialization_template(?NAME_VSN_1) ->
    [ {owner_id, id}
    , {expires_by, int}
    , {status, binary}
    , {client_ttl, int}
    , {pointers, [{binary, id}]}
    ];
serialization_template(?NAME_VSN_2) ->
    [ {owner_id, id}
    , {expires_by, int}
    , {status, binary}
    , {client_ttl, int}
    , {pointers, [{binary, binary}]}
    ].

serialization_type() -> ?NAME_TYPE.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec id(name()) -> id().
id(#name{id = Id}) ->
    Id.

-spec hash(name()) -> aens_hash:name_hash().
hash(#name{id = Id}) ->
    aeser_id:specialize(Id, name).

-spec owner_pubkey(name()) -> aec_keys:pubkey().
owner_pubkey(#name{owner_id = OwnerId}) ->
    aeser_id:specialize(OwnerId, account).

-spec status(name()) -> status().
status(#name{status = Status}) ->
    Status.

-spec ttl(name()) -> aec_blocks:height().
ttl(#name{expires_by = TTL}) ->
    TTL.

-spec pointers(name()) -> list(aens_pointer:pointer()).
pointers(#name{pointers = Pointers}) ->
    Pointers.

-spec client_ttl(name()) -> integer().
client_ttl(#name{client_ttl = ClientTTL}) ->
    ClientTTL.
