%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for commitment objects
%%% @end
%%%=============================================================================

-module(aens_commitments).

%% API
-export([new/4,
         update/6,
         serialize/1,
         deserialize/2,
         deserialize_from_fields/3,
         serialization_type/0,
         serialization_template/1
        ]).

-export([is_auction_done_when_elapsed/1,
         get_name_auction_state/2,
         name_length/1]).

-export([assert_commitment_owner/2,
         assert_height_delta/3]).

%% Getters
-export([hash/1,
         owner_pubkey/1,
         ttl/1,
         created/1,
         auction/1,
         name_fee/1,
         name_hash/1]).

-behavior(aens_cache).
%%%===================================================================
%%% Types
%%%===================================================================
-define(LEGACY, legacy).
-define(PRECLAIM, preclaim).
-define(CLAIM_ATTEMPT, claim_attempt).

-type(auction_state() :: ?LEGACY | ?PRECLAIM | ?CLAIM_ATTEMPT).

-record(commitment,
        {id                :: aeser_id:id(),
         owner_id          :: aeser_id:id(),
         created           :: aec_blocks:height(),
         auction           :: auction_state(),
         name_fee          :: non_neg_integer(),
         name_hash         :: hash() | undefined,
         ttl               :: aec_blocks:height()
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
-define(COMMITMENT_ROMA, 1).
-define(COMMITMENT_LIMA_PRECLAIM, 2).
-define(COMMITMENT_LIMA_CLAIM, 3).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aeser_id:id(), aeser_id:id(), non_neg_integer(), aec_blocks:height()) -> commitment().
new(Id, OwnerId, DeltaTTL, BlockHeight) ->
    commitment = aeser_id:specialize_type(Id),
    account    = aeser_id:specialize_type(OwnerId),
    #commitment{id       = Id,
                owner_id = OwnerId,
                auction = ?PRECLAIM,
                created  = BlockHeight,
                name_fee = 0,
                ttl      = BlockHeight + DeltaTTL}.

-spec update(commitment(), aeser_id:id(), non_neg_integer(), aec_blocks:height(),
             non_neg_integer(), hash()) -> commitment().
update(Commitment, NewOwnerId, BidDelta, BlockHeight, NameFee, NameHash) ->
    NewOwnerIdSpec = aeser_id:create(account, NewOwnerId),
    Commitment#commitment{
      owner_id = NewOwnerIdSpec,
      auction  = ?CLAIM_ATTEMPT,
      name_fee = NameFee,
      name_hash = NameHash,
      ttl      = BlockHeight + BidDelta}.

-spec serialize(commitment()) -> serialized().
%% INFO: order of causes matters as legacy ones don't have auction field
serialize(#commitment{owner_id = OwnerId,
    created = Created,
    auction = preclaim,
    name_fee = 0,
    ttl = TTL}) ->
    aeser_chain_objects:serialize(
        ?COMMITMENT_TYPE,
        ?COMMITMENT_LIMA_PRECLAIM,
        serialization_template(?COMMITMENT_LIMA_PRECLAIM),
        [ {owner_id, OwnerId}
        , {created, Created}
        , {auction, <<"preclaim">>}
        , {name_fee, 0}
        , {ttl, TTL}]);
serialize(#commitment{owner_id = OwnerId,
                      created = Created,
                      auction = claim_attempt,
                      name_fee = NameFee,
                      name_hash = NameHash,
                      ttl = TTL}) ->
    aeser_chain_objects:serialize(
      ?COMMITMENT_TYPE,
      ?COMMITMENT_LIMA_CLAIM,
      serialization_template(?COMMITMENT_LIMA_CLAIM),
      [ {owner_id, OwnerId}
      , {created, Created}
      , {auction, <<"claim_attempt">>}
      , {name_fee, NameFee}
      , {name_hash, NameHash}
      , {ttl, TTL}]);
serialize(#commitment{owner_id = OwnerId,
    created  = Created,
    ttl      = TTL}) ->
    aeser_chain_objects:serialize(
        ?COMMITMENT_TYPE,
        ?COMMITMENT_ROMA,
        serialization_template(?COMMITMENT_ROMA),
        [ {owner_id, OwnerId}
        , {created, Created}
        , {ttl, TTL}]).

-spec deserialize(hash(), binary()) -> commitment().
deserialize(CommitmentHash, Bin) ->
    {Type, Vsn, _Rest} = aeser_chain_objects:deserialize_type_and_vsn(Bin),
    Fields = aeser_chain_objects:deserialize(Type, Vsn, serialization_template(Vsn), Bin),
    deserialize_from_fields(Vsn, CommitmentHash, Fields).

deserialize_from_fields(?COMMITMENT_ROMA, CommitmentHash,
                        [ {owner_id, OwnerId}
                        , {created, Created}
                        , {ttl, TTL}]) ->
    #commitment{id       = aeser_id:create(commitment, CommitmentHash),
                owner_id = OwnerId,
                created  = Created,
                auction  = ?LEGACY,
                name_fee = aec_governance:name_claim_fee_base(), %% base fee is equal to pre-lima fee
                ttl      = TTL};
deserialize_from_fields(?COMMITMENT_LIMA_PRECLAIM, CommitmentHash,
                        [ {owner_id, OwnerId}
                        , {created, Created}
                        , {auction, <<"preclaim">>}
                        , {name_fee, 0}
                        , {ttl, TTL}]) ->
    #commitment{id       = aeser_id:create(commitment, CommitmentHash),
                owner_id = OwnerId,
                created  = Created,
                name_fee = 0,
                auction = ?PRECLAIM,
                ttl      = TTL};
deserialize_from_fields(?COMMITMENT_LIMA_CLAIM, CommitmentHash,
                        [ {owner_id, OwnerId}
                        , {created, Created}
                        , {auction, <<"claim_attempt">>}
                        , {name_fee, NameFee}
                        , {name_hash, NameHash}
                        , {ttl, TTL}]) ->
    #commitment{id       = aeser_id:create(commitment, CommitmentHash),
                owner_id = OwnerId,
                created  = Created,
                auction = ?CLAIM_ATTEMPT,
                name_fee = NameFee,
                name_hash = NameHash,
                ttl      = TTL}.

serialization_template(?COMMITMENT_ROMA) ->
    [ {owner_id, id}
    , {created, int}
    , {ttl, int}
    ];
serialization_template(?COMMITMENT_LIMA_PRECLAIM) ->
    [ {owner_id, id}
    , {created, int}
    , {auction, binary}
    , {name_fee, int}
    , {ttl, int}
    ];
serialization_template(?COMMITMENT_LIMA_CLAIM) ->
    [ {owner_id, id}
    , {created, int}
    , {auction, binary}
    , {name_fee, int}
    , {name_hash, binary}
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

-spec auction(commitment()) -> auction_state().
auction(#commitment{auction = AuctionState}) ->
    AuctionState.

-spec name_hash(commitment()) -> hash().
name_hash(#commitment{name_hash = NameHash}) ->
    NameHash.

-spec name_fee(commitment()) -> non_neg_integer().
name_fee(#commitment{name_fee = NameFee}) ->
    NameFee.

%%%===================================================================
%%% Auction handling
%%%===================================================================

-spec is_auction_done_when_elapsed(commitment()) -> boolean().
is_auction_done_when_elapsed(#commitment{auction = ?LEGACY}) ->
    false;
is_auction_done_when_elapsed(#commitment{auction = ?PRECLAIM}) ->
    false;
is_auction_done_when_elapsed(#commitment{auction = ?CLAIM_ATTEMPT}) ->
    true.

-spec get_name_auction_state(commitment(), binary()) ->
                             no_auction | auction_ready | auction_ongoing.
get_name_auction_state(#commitment{auction = Auction}, Name) ->
    Length = name_length(Name),
    %% If the time for the next bid is higher than 1 block, then we have an auction
    BidTimeout = aec_governance:name_claim_bid_timeout(Length),
    case {Auction, BidTimeout > 1} of
        {?LEGACY, _} ->
            no_auction;
        {?PRECLAIM, false} ->
            no_auction;
        {?PRECLAIM, true} ->
            auction_ready;
        {?CLAIM_ATTEMPT, _} ->
            auction_ongoing
    end.


-spec name_length(binary()) -> non_neg_integer().
name_length(PlainName) ->
    %% Works only for one namespace; improve when we have more
    %% No reason to regex now
    size(PlainName) - size(hd(aec_governance:name_registrars())) - 1.


%%%===================================================================
%%% Assertions
%%%===================================================================

assert_commitment_owner(Commitment, Pubkey) ->
    OwnerId = aens_commitments:owner_pubkey(Commitment),
    AuctionState = aens_commitments:auction(Commitment),
    case {AuctionState, OwnerId =:= Pubkey} of
        {?LEGACY, true} -> ok;
        {?LEGACY, false} -> commitment_not_owned;
        {?PRECLAIM, true}  -> ok;
        {?PRECLAIM, false} -> commitment_not_owned;
        {?CLAIM_ATTEMPT, true} -> commitment_already_owned;
        {?CLAIM_ATTEMPT, false} -> ok
    end.

assert_height_delta(Commitment, PreclaimDelta, Height) ->
    AuctionState = aens_commitments:auction(Commitment),
    case AuctionState of
        ?LEGACY -> assert_preclaim_delta(Commitment, PreclaimDelta, Height);
        ?PRECLAIM -> assert_preclaim_delta(Commitment, PreclaimDelta, Height);
        ?CLAIM_ATTEMPT ->  assert_bid_delta(Commitment, Height)
    end.

assert_preclaim_delta(Commitment, PreclaimDelta, Height) ->
    case aens_commitments:created(Commitment) + PreclaimDelta =< Height of
        true  -> ok;
        false -> commitment_delta_too_small
    end.

assert_bid_delta(Commitment, Height) ->
    case aens_commitments:ttl(Commitment) >= Height of
        true  -> ok;
        false -> too_late_for_bid
    end.
