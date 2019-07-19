%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for commitment objects
%%% @end
%%%=============================================================================

-module(aens_commitments).

%% API
-export([new/4,
         serialize/1,
         deserialize/2,
         deserialize_from_fields/3,
         serialization_type/0,
         serialization_template/1
        ]).

%% Getters
-export([hash/1,
         owner_pubkey/1,
         ttl/1,
         created/1]).

-behavior(aens_cache).
%%%===================================================================
%%% Types
%%%===================================================================
-record(commitment,
        {id       :: aeser_id:id(),
         owner_id :: aeser_id:id(),
         created  :: aec_blocks:height(),
         ttl      :: aec_blocks:height()
         }).

-opaque commitment() :: #commitment{}.

-type id() :: aeser_id:id().
-type hash() :: aens_hash:commitment_hash().
-type pubkey() :: aec_keys:pubkey().
-type serialized() :: binary().

-export_type([id/0,
              commitment/0,
              serialized/0]).

-define(COMMITMENT_TYPE, name_commitment).
-define(COMMITMENT_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aeser_id:id(), aeser_id:id(), non_neg_integer(), aec_blocks:height()) -> commitment().
new(Id, OwnerId, DeltaTTL, BlockHeight) ->
    commitment = aeser_id:specialize_type(Id),
    account    = aeser_id:specialize_type(OwnerId),
    #commitment{id       = Id,
                owner_id = OwnerId,
                created  = BlockHeight,
                ttl      = BlockHeight + DeltaTTL}.

-spec serialize(commitment()) -> serialized().
serialize(#commitment{owner_id = OwnerId,
                      created  = Created,
                      ttl      = TTL}) ->
    aeser_chain_objects:serialize(
      ?COMMITMENT_TYPE,
      ?COMMITMENT_VSN,
      serialization_template(?COMMITMENT_VSN),
      [ {owner_id, OwnerId}
      , {created, Created}
      , {ttl, TTL}]).

-spec deserialize(hash(), binary()) -> commitment().
deserialize(CommitmentHash, Bin) ->
    Fields = aeser_chain_objects:deserialize(
                  ?COMMITMENT_TYPE,
                  ?COMMITMENT_VSN,
                  serialization_template(?COMMITMENT_VSN),
                  Bin),
    deserialize_from_fields(?COMMITMENT_VSN, CommitmentHash, Fields).

deserialize_from_fields(?COMMITMENT_VSN, CommitmentHash,
    [ {owner_id, OwnerId}
    , {created, Created}
    , {ttl, TTL}]) ->
    #commitment{id       = aeser_id:create(commitment, CommitmentHash),
                owner_id = OwnerId,
                created  = Created,
                ttl      = TTL}.

serialization_template(?COMMITMENT_VSN) ->
    [ {owner_id, id}
    , {created, int}
    , {ttl, int}
    ].

serialization_type() -> ?COMMITMENT_TYPE.
%%%===================================================================
%%% Getters
%%%===================================================================

-spec hash(commitment()) -> hash().
hash(#commitment{id = Id}) ->
    aeser_id:specialize(Id, commitment).

-spec owner_pubkey(commitment()) -> pubkey().
owner_pubkey(#commitment{owner_id = OwnerId}) ->
    aeser_id:specialize(OwnerId, account).

-spec ttl(commitment()) -> aec_blocks:height().
ttl(#commitment{ttl = TTL}) ->
    TTL.

-spec created(commitment()) -> aec_blocks:height().
created(#commitment{created = Created}) ->
    Created.

