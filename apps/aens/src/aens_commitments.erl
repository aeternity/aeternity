%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for commitment objects
%%% @end
%%%=============================================================================

-module(aens_commitments).

%% API
-export([id/1,
         new/3,
         serialize/1,
         deserialize/2
        ]).

%% Getters
-export([expires/1,
         created/1,
         hash/1,
         owner/1]).

%%%===================================================================
%%% Types
%%%===================================================================
-record(commitment,
        {id      :: aec_id:id(),
         owner   :: aec_keys:pubkey(),
         created :: aec_blocks:height(),
         expires :: aec_blocks:height()
         }).

-opaque commitment() :: #commitment{}.

-type id() :: binary().
-type serialized() :: binary().

-export_type([id/0,
              commitment/0,
              serialized/0]).

-define(COMMITMENT_TYPE, name_commitment).
-define(COMMITMENT_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(commitment()) -> aens_hash:commitment_hash().
id(C) ->
    hash(C).

-spec new(aens_preclaim_tx:tx(), non_neg_integer(), aec_blocks:height()) -> commitment().
new(PreclaimTx, Expiration, BlockHeight) ->
    Expires = BlockHeight + Expiration,
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    Id = aec_id:create(commitment, aens_preclaim_tx:commitment(PreclaimTx)),
    #commitment{id      = Id,
                owner   = aens_preclaim_tx:account(PreclaimTx),
                created = BlockHeight,
                expires = Expires}.

-spec serialize(commitment()) -> binary().
serialize(#commitment{} = C) ->
    aec_object_serialization:serialize(
      ?COMMITMENT_TYPE,
      ?COMMITMENT_VSN,
      serialization_template(?COMMITMENT_VSN),
      [ {owner, owner(C)}
      , {created, created(C)}
      , {expires, expires(C)}]).

-spec deserialize(aens_hash:commitment_hash(), binary()) -> commitment().
deserialize(Hash, Bin) ->
    [ {owner, Owner}
    , {created, Created}
    , {expires, Expires}
    ] = aec_object_serialization:deserialize(
          ?COMMITMENT_TYPE,
          ?COMMITMENT_VSN,
          serialization_template(?COMMITMENT_VSN),
          Bin),
    #commitment{id      = aec_id:create(commitment, Hash),
                owner   = Owner,
                created = Created,
                expires = Expires}.

serialization_template(?COMMITMENT_VSN) ->
    [ {owner, binary}
    , {created, int}
    , {expires, int}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec expires(commitment()) -> aec_blocks:height().
expires(C) -> C#commitment.expires.

-spec created(commitment()) -> aec_blocks:height().
created(C) -> C#commitment.created.

-spec hash(commitment()) -> aens_hash:commitment_hash().
hash(C) -> aec_id:specialize(C#commitment.id, commitment).

-spec owner(commitment()) -> aec_keys:pubkey().
owner(C) -> C#commitment.owner.
