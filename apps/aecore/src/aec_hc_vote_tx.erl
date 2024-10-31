%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity Foundation
%%%-------------------------------------------------------------------

-module(aec_hc_vote_tx).

%% API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         voter_id/1,
         voter_pubkey/1,
         recipient_id/1,
         epoch/1,
         check/3,
         process/3,
         signers/2,
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
        ]).

-export([data/1]).

-behavior(aetx).

-include("blocks.hrl").

-define(HC_VOTE_TX_VSN, 1).
-define(HC_VOTE_TX_TYPE, hc_vote_tx).

-record(hc_vote_tx,
        { voter_id        :: aeser_id:id(),
          epoch     = 0   :: non_neg_integer(),
          data      = #{} :: #{binary() => binary()}
        }).

-opaque tx() :: #hc_vote_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{voter_id := VoterId,
      epoch    := Epoch,
      data     := Data})
  when is_integer(Epoch), Epoch >= 0,
       is_map(Data) ->
    account = aeser_id:specialize_type(VoterId),
    Tx = #hc_vote_tx{voter_id = VoterId,
                     epoch    = Epoch,
                     data     = Data},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?HC_VOTE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#hc_vote_tx{}) ->
    0.

-spec gas(tx()) -> non_neg_integer().
gas(#hc_vote_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#hc_vote_tx{}) ->
    0.

-spec nonce(tx()) -> non_neg_integer().
nonce(#hc_vote_tx{}) ->
    0.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#hc_vote_tx{} = Tx) ->
    voter_pubkey(Tx).

-spec voter_id(tx()) -> aeser_id:id().
voter_id(#hc_vote_tx{voter_id = VoterId}) ->
    VoterId.

-spec voter_pubkey(tx()) -> aec_keys:pubkey().
voter_pubkey(#hc_vote_tx{voter_id = VoterId}) ->
    aeser_id:specialize(VoterId, account).

-spec epoch(tx()) -> non_neg_integer().
epoch(#hc_vote_tx{epoch = Epoch}) ->
    Epoch.

-spec data(tx()) -> #{binary() => binary()}.
data(#hc_vote_tx{data = Data}) ->
    Data.

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#hc_vote_tx{}, Trees,_Env) ->
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#hc_vote_tx{} = Tx, _) -> {ok, [voter_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) ->
    {ok, aec_trees:trees(), aetx_env:env()} | {error, term()}.
process(#hc_vote_tx{}, _Trees, _Env) ->
    {error, hc_vote_tx_cant_be_processed}.

serialize(#hc_vote_tx{voter_id = VoterId,
                      epoch    = Epoch,
                      data     = Data} = Tx) ->
    {version(Tx),
     [ {voter_id, VoterId}
     , {epoch, Epoch}
     , {data, serialize_data(Data)}
     ]}.

deserialize(?HC_VOTE_TX_VSN,
            [ {voter_id, VoterId}
            , {epoch, Epoch}
            , {data, Data}]) ->
    %% Asserts
    account = aeser_id:specialize_type(VoterId),
    #hc_vote_tx{voter_id = VoterId,
                epoch    = Epoch,
                data     = deserialize_data(Data)}.

serialize_data(Data) ->
    lists:sort(maps:to_list(Data)).

deserialize_data(Data) ->
    maps:from_list(Data).

serialization_template(?HC_VOTE_TX_VSN) ->
    [ {voter_id, id}
    , {epoch, int}
    , {data, [{binary, binary}]}
    ].

for_client(#hc_vote_tx{voter_id = VoterId,
                       epoch    = Epoch,
                       data     = Data}) ->
    #{<<"voter_id">>    => aeser_api_encoder:encode(id_hash, VoterId),
      <<"epoch">>       => Epoch,
      <<"data">>        => Data}.

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?HC_VOTE_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_, _) ->
    true.

