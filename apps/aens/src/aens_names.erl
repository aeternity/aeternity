%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for name objects
%%% @end
%%%=============================================================================

-module(aens_names).

-include_lib("apps/aecore/include/common.hrl").
-include("aens.hrl").

%% API
-export([id/1,
         new/3,
         update/3,
         revoke/3,
         transfer/2,
         serialize/1,
         deserialize/1]).

%% Getters
-export([owner/1,
         status/1,
         expires/1,
         pointers/1,
         ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque name() :: #name{}.

-type id() :: binary().
-type serialized() :: binary().

-export_type([id/0,
              name/0,
              serialized/0]).

-define(NAME_TYPE, <<"name">>).
-define(NAME_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(name()) -> aens_hash:name_hash().
id(N) ->
    hash(N).

-spec new(aens_claim_tx:tx(), non_neg_integer(), height()) -> name().
new(ClaimTx, Expiration, BlockHeight) ->
    Expires    = BlockHeight + Expiration,
    Name       = aens_claim_tx:name(ClaimTx),
    {ok, Hash} = aens:get_name_hash(Name),
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    #name{hash    = Hash,
          owner   = aens_claim_tx:account(ClaimTx),
          expires = Expires,
          status  = claimed}.

-spec update(aens_update_tx:tx(), name(), height()) -> name().
update(UpdateTx, Name, BlockHeight) ->
    Expires = BlockHeight + aens_update_tx:ttl(UpdateTx),
    Name#name{expires  = Expires,
              ttl      = aens_update_tx:name_ttl(UpdateTx),
              pointers = aens_update_tx:pointers(UpdateTx)}.

-spec revoke(name(), non_neg_integer(), height()) -> name().
revoke(Name, Expiration, BlockHeight) ->
    Expires = BlockHeight + Expiration,
    Name#name{status  = revoked,
              expires = Expires}.

-spec transfer(aens_transfer_tx:tx(), name()) -> name().
transfer(TransferTx, Name) ->
    Name#name{owner = aens_transfer_tx:recipient_account(TransferTx)}.

-spec serialize(name()) -> binary().
serialize(#name{} = N) ->
    msgpack:pack([#{<<"type">>     => ?NAME_TYPE},
                  #{<<"vsn">>      => ?NAME_VSN},
                  #{<<"hash">>     => hash(N)},
                  #{<<"owner">>    => owner(N)},
                  #{<<"expires">>  => expires(N)},
                  #{<<"status">>   => status(N)},
                  #{<<"ttl">>      => ttl(N)},
                  #{<<"pointers">> => jsx:encode(pointers(N))}]).

-spec deserialize(binary()) -> name().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"type">>     := ?NAME_TYPE},
     #{<<"vsn">>      := ?NAME_VSN},
     #{<<"hash">>     := Hash},
     #{<<"owner">>    := Owner},
     #{<<"expires">>  := Expires},
     #{<<"status">>   := Status},
     #{<<"ttl">>      := TTL},
     #{<<"pointers">> := Pointers}] = List,
    #name{hash     = Hash,
          owner    = Owner,
          expires  = Expires,
          status   = binary_to_existing_atom(Status, utf8),
          ttl      = TTL,
          pointers = jsx:decode(Pointers)}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec owner(name()) -> pubkey().
owner(N) -> N#name.owner.

-spec status(name()) -> name_status().
status(N) -> N#name.status.

-spec expires(name()) -> height().
expires(N) -> N#name.expires.

-spec pointers(name()) -> list().
pointers(N) -> N#name.pointers.

-spec ttl(name()) -> integer().
ttl(N) -> N#name.ttl.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec hash(name()) -> aens_hash:name_hash().
hash(N) -> N#name.hash.
