%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for commitment objects
%%% @end
%%%=============================================================================

-module(aens_commitments).

%% API
-export([new/3,
         serialize/1,
         deserialize/2
        ]).

%% Getters
-export([hash/1,
         owner_pubkey/1,
         expires/1,
         created/1]).

%%%===================================================================
%%% Types
%%%===================================================================
-record(commitment,
        {id       :: aec_id:id(),
         owner_id :: aec_id:id(),
         created  :: aec_blocks:height(),
         expires  :: aec_blocks:height()
         }).

-opaque commitment() :: #commitment{}.

-type id() :: aec_id:id().
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

-spec new(aens_preclaim_tx:tx(), non_neg_integer(), aec_blocks:height()) -> commitment().
new(PreclaimTx, ExpirationHeight, BlockHeight) ->
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    Id      = aens_preclaim_tx:commitment_id(PreclaimTx),
    OwnerId = aens_preclaim_tx:account_id(PreclaimTx),
    commitment = aec_id:specialize_type(Id),
    account    = aec_id:specialize_type(OwnerId),
    #commitment{id       = Id,
                owner_id = OwnerId,
                created  = BlockHeight,
                expires  = BlockHeight + ExpirationHeight}.

-spec serialize(commitment()) -> binary().
serialize(#commitment{owner_id = OwnerId,
                      created  = Created,
                      expires  = Expires}) ->
    aec_object_serialization:serialize(
      ?COMMITMENT_TYPE,
      ?COMMITMENT_VSN,
      serialization_template(?COMMITMENT_VSN),
      [ {owner_id, OwnerId}
      , {created, Created}
      , {expires, Expires}]).

-spec deserialize(hash(), binary()) -> commitment().
deserialize(CommitmentHash, Bin) ->
    [ {owner_id, OwnerId}
    , {created, Created}
    , {expires, Expires}
    ] = aec_object_serialization:deserialize(
          ?COMMITMENT_TYPE,
          ?COMMITMENT_VSN,
          serialization_template(?COMMITMENT_VSN),
          Bin),
    #commitment{id       = aec_id:create(commitment, CommitmentHash),
                owner_id = OwnerId,
                created  = Created,
                expires  = Expires}.

serialization_template(?COMMITMENT_VSN) ->
    [ {owner_id, id}
    , {created, int}
    , {expires, int}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec hash(commitment()) -> hash().
hash(#commitment{id = Id}) ->
    aec_id:specialize(Id, commitment).

-spec owner_pubkey(commitment()) -> pubkey().
owner_pubkey(#commitment{owner_id = OwnerId}) ->
    aec_id:specialize(OwnerId, account).

-spec expires(commitment()) -> aec_blocks:height().
expires(#commitment{expires = Expires}) ->
    Expires.

-spec created(commitment()) -> aec_blocks:height().
created(#commitment{created = Created}) ->
    Created.

