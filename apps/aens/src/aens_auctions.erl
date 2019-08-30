%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for names auction objects
%%%    Idea: whenever a new claim comes in within auction period,
%%%          then - if namefee too low, just charge for transaction, but not the fee
%%%               - if namefee exceeds previous bid with right margin
%%%                 then we end up here and create a new auction object,
%%%                 the previous highest bidder should get its namefee unlocked and
%%%                 the new one get it locked.
%%%          Invariant: we can only start claiming names when name is not claimed
%%%          hence, we cannot start an auction before the name is available
%%%          We delete all auction objects after the auction, therefore no
%%%          risk ??? that previous name holder get returned name fee when new auction starts.
%%% @end
%%%=============================================================================

-module(aens_auctions).

%% API
-export([new/5,
         serialize/1,
         deserialize/2,
         deserialize_from_fields/3,
         serialization_type/0,
         serialization_template/1
        ]).

%% Getters
-export([name_hash/1,
         bidder_pubkey/1,
         ttl/1,
         name_fee/1,
         started/1]).

-behavior(aens_cache).
%%%===================================================================
%%% Types
%%%===================================================================
-record(auction,
        {id        :: aeser_id:id(),
         started   :: aec_blocks:height(),
         bidder_id :: aeser_id:id(),      %% should there be a preclaim for second bidder?? No.
                                          %% Preclaim owner can start auction by a claim, nobody else
                                          %% Having a second preclaim for same name won't restart auction
         bid       :: non_neg_integer(),  %% the name_fee provided
         ttl       :: aec_blocks:height()
         }).

-opaque auction() :: #auction{}.

-type id() :: aeser_id:id().
-type pubkey() :: aec_keys:pubkey().
-type serialized() :: binary().

-export_type([id/0,
              auction/0,
              serialized/0]).

-define(AUCTION_TYPE, name_auction).
-define(AUCTION_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aens_hash:name_hash(), aec_keys:pubkey(), non_neg_integer(), non_neg_integer(), aec_blocks:height()) -> auction().
new(NameHash, BidderId, NameFee, DeltaTTL, BlockHeight) ->
    #auction{id        = aeser_id:create(name, NameHash),
             bidder_id = aeser_id:create(account, BidderId),
             started   = BlockHeight,
             bid       = NameFee,
             ttl       = BlockHeight + DeltaTTL}.

-spec serialize(auction()) -> serialized().
serialize(#auction{id        = Id,
                   bidder_id = BidderId,
                   bid       = NameFee,
                   started  = Started,
                   ttl      = TTL}) ->
    aeser_chain_objects:serialize(
      ?AUCTION_TYPE,
      ?AUCTION_VSN,
      serialization_template(?AUCTION_VSN),
      [ {bidder_id, BidderId}
      , {bid, NameFee}
      , {started, Started}
      , {ttl, TTL}]).

-spec deserialize(aens_hash:name_hash(), binary()) -> auction().
deserialize(NameHash, Bin) ->
    Fields = aeser_chain_objects:deserialize(
                  ?AUCTION_TYPE,
                  ?AUCTION_VSN,
                  serialization_template(?AUCTION_VSN),
                  Bin),
    deserialize_from_fields(?AUCTION_VSN, NameHash, Fields).

deserialize_from_fields(?AUCTION_VSN, NameHash,
    [ {bidder_id, BidderId}
    , {bid, NameFee}
    , {started, Started}
    , {ttl, TTL} ]) ->
    #auction{id        = aeser_id:create(name, NameHash),
             bidder_id = BidderId,
             bid       = NameFee,
             started   = Started,
             ttl       = TTL}.

serialization_template(?AUCTION_VSN) ->
    [ {bidder_id, id}
    , {bid, int}
    , {started, int}
    , {ttl, int}].

serialization_type() -> ?AUCTION_TYPE.
%%%===================================================================
%%% Getters
%%%===================================================================

-spec name_hash(auction()) -> aens_hash:name_hash().
name_hash(#auction{id = Id}) ->
    aeser_id:specialize(Id, name).

-spec bidder_pubkey(auction()) -> pubkey().
bidder_pubkey(#auction{bidder_id = BidderId}) ->
    aeser_id:specialize(BidderId, account).

-spec name_fee(auction()) -> non_neg_integer().
name_fee(#auction{bid = NameFee}) ->
    NameFee.

-spec ttl(auction()) -> aec_blocks:height().
ttl(#auction{ttl = TTL}) ->
    TTL.

-spec started(auction()) -> aec_blocks:height().
started(#auction{started = Started}) ->
    Started.
