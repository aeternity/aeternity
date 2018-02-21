%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for commitment objects
%%% @end
%%%=============================================================================

-module(aens_commitments).

-include_lib("apps/aecore/include/common.hrl").
-include("aens.hrl").

%% API
-export([id/1,
         new/3,
         serialize/1,
         deserialize/1]).

%% Getters
-export([expires/1,
         created/1,
         hash/1,
         owner/1]).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque commitment() :: #commitment{}.

-type id() :: binary().
-type serialized() :: binary().

-export_type([id/0,
              commitment/0,
              serialized/0]).

-define(COMMITMENT_TYPE, <<"commitment">>).
-define(COMMITMENT_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(commitment()) -> aens_hash:commitment_hash().
id(C) ->
    hash(C).

-spec new(aens_preclaim_tx:tx(), non_neg_integer(), height()) -> commitment().
new(PreclaimTx, Expiration, BlockHeight) ->
    Expires = BlockHeight + Expiration,
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    #commitment{hash    = aens_preclaim_tx:commitment(PreclaimTx),
                owner   = aens_preclaim_tx:account(PreclaimTx),
                created = BlockHeight,
                expires = Expires}.

-spec serialize(commitment()) -> binary().
serialize(#commitment{} = C) ->
    msgpack:pack([#{<<"type">>    => ?COMMITMENT_TYPE},
                  #{<<"vsn">>     => ?COMMITMENT_VSN},
                  #{<<"hash">>    => hash(C)},
                  #{<<"owner">>   => owner(C)},
                  #{<<"created">> => created(C)},
                  #{<<"expires">> => expires(C)}]).

-spec deserialize(binary()) -> commitment().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"type">>    := ?COMMITMENT_TYPE},
     #{<<"vsn">>     := ?COMMITMENT_VSN},
     #{<<"hash">>    := Hash},
     #{<<"owner">>   := Owner},
     #{<<"created">> := Created},
     #{<<"expires">> := Expires}] = List,
    #commitment{hash    = Hash,
                owner   = Owner,
                created = Created,
                expires = Expires}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec expires(commitment()) -> height().
expires(C) -> C#commitment.expires.

-spec created(commitment()) -> height().
created(C) -> C#commitment.created.

-spec hash(commitment()) -> aens_hash:commitment_hash().
hash(C) -> C#commitment.hash.

-spec owner(commitment()) -> pubkey().
owner(C) -> C#commitment.owner.
