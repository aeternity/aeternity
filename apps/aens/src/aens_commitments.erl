%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% ADT for commitment objects
%%% @end
%%%-------------------------------------------------------------------

-module(aens_commitments).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([id/1,
         new/3,
         commitment_hash/2,
         serialize/1,
         deserialize/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(commitment, {hash    :: binary(),
                     owner   :: pubkey(),
                     expires :: height()}).

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

-spec id(commitment()) -> binary().
id(C) ->
    hash(C).

-spec new(aens_preclaim_tx:preclaim_tx(), non_neg_integer(), height()) -> commitment().
new(PreclaimTx, Expiration, BlockHeight) ->
    Expires = BlockHeight + Expiration,
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    #commitment{hash    = aens_preclaim_tx:commitment(PreclaimTx),
                owner   = aens_preclaim_tx:account(PreclaimTx),
                expires = Expires}.

-spec commitment_hash(aens_names:name(), non_neg_integer()) -> binary().
commitment_hash(Name, _NameNonce) ->
    %% TODO: Implement me
    %% TODO: Maybe move name and commitment hashes to aens_hash module
    _HashName = aens_names:hash_name(Name),
    Name.

-spec serialize(commitment()) -> binary().
serialize(#commitment{} = C) ->
    msgpack:pack([#{<<"type">>    => ?COMMITMENT_TYPE},
                  #{<<"vsn">>     => ?COMMITMENT_VSN},
                  #{<<"hash">>    => hash(C)},
                  #{<<"owner">>   => owner(C)},
                  #{<<"expires">> => expires(C)}]).

-spec deserialize(binary()) -> commitment().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"type">>    := ?COMMITMENT_TYPE},
     #{<<"vsn">>     := ?COMMITMENT_VSN},
     #{<<"hash">>    := Hash},
     #{<<"owner">>   := Owner},
     #{<<"expires">> := Expires}] = List,
    #commitment{hash    = Hash,
                owner   = Owner,
                expires = Expires}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec expires(commitment()) -> height().
expires(C) -> C#commitment.expires.

-spec hash(commitment()) -> binary().
hash(C) -> C#commitment.hash.

-spec owner(commitment()) -> pubkey().
owner(C) -> C#commitment.owner.
