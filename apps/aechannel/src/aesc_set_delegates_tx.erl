%%%=============================================================================
%%% @copyright 2021, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel set delegates transaction
%%% @end
%%%=============================================================================
-module(aesc_set_delegates_tx).

-behavior(aetx).
-behaviour(aesc_signable_transaction).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
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

% aesc_signable_transaction callbacks
-export([channel_id/1,
         channel_pubkey/1,
         state_hash/1,
         round/1]).

-include_lib("aecontract/include/hard_forks.hrl").
%%%===================================================================
%%% Types
%%%===================================================================

-define(INITIAL_VSN, 1).
-define(TX_TYPE, channel_set_delegates_tx).

-type vsn() :: non_neg_integer().

-record(?TX_TYPE, {
          channel_id             :: aeser_id:id(),
          initiator_delegate_ids :: [aeser_id:id()],
          responder_delegate_ids :: [aeser_id:id()],
          from_id                :: aeser_id:id(),
          payload                :: binary(),
          state_hash             :: binary(),
          round                  :: non_neg_integer(),
          ttl                    :: aetx:tx_ttl(),
          fee                    :: non_neg_integer(),
          nonce                  :: non_neg_integer()
         }).

-opaque tx() :: #channel_set_delegates_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id             := ChannelId,
      from_id                := FromId,
      initiator_delegate_ids := IDelegates,
      responder_delegate_ids := RDelegates,
      payload                := Payload,
      state_hash             := StateHash,
      round                  := Round,
      fee                    := Fee,
      nonce                  := Nonce} = Args) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    lists:all(fun(Id) -> account =:= aeser_id:specialize_type(Id) end,
              IDelegates ++ RDelegates),
    Tx = #channel_set_delegates_tx{
            channel_id             = ChannelId,
            from_id                = FromId,
            initiator_delegate_ids = IDelegates,
            responder_delegate_ids = RDelegates,
            payload                = Payload,
            state_hash             = StateHash,
            round                  = Round,
            ttl                    = maps:get(ttl, Args, 0),
            fee                    = Fee,
            nonce                  = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_set_delegates_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_set_delegates_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_set_delegates_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_set_delegates_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_set_delegates_tx{} = Tx) ->
    from_pubkey(Tx).

-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_set_delegates_tx{channel_id = ChannelId}) ->
    aeser_id:specialize(ChannelId, channel).

-spec state_hash(tx()) -> binary().
state_hash(#channel_set_delegates_tx{state_hash = StateHash}) -> StateHash.

-spec round(tx()) -> non_neg_integer().
round(#channel_set_delegates_tx{round = Round}) -> Round.


channel_id(#channel_set_delegates_tx{channel_id = ChannelId}) ->
    ChannelId.

from_pubkey(#channel_set_delegates_tx{from_id = FromId}) ->
    aeser_id:specialize(FromId, account).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_set_delegates_tx{payload    = Payload,
                                state_hash = StateHash,
                                round      = Round,
                                fee        = Fee,
                                nonce      = Nonce} = Tx, Trees, Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    %% the same checks with snapshot but we make sure the state_hash and round
    %% are the same as in the payload. This could be tricky as the payload
    %% could as well be empty: in the case when the transaction is based on
    %% latest on-chain state
    case aesc_utils:check_set_delegates(
           ChannelPubKey, FromPubKey, Nonce, Fee, Payload, StateHash, Round, Trees, Env) of
        ok -> {ok, Trees};
        Err -> Err
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_set_delegates_tx{payload                = Payload,
                                  initiator_delegate_ids = IDelegates,
                                  responder_delegate_ids = RDelegates,
                                  fee                    = Fee,
                                  nonce                  = Nonce} = Tx,
        Trees, Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    %% note that since the state hash and the round are already validated,
    %% the payload is not being used
    Specialize = fun(L) -> [aeser_id:specialize(Id, account) || Id <- L] end,
    aesc_utils:process_set_delegates(ChannelPubKey, FromPubKey,
                                     Specialize(IDelegates), Specialize(RDelegates),
                                     Payload,
                                     Nonce, Fee, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_set_delegates_tx{} = Tx, Trees) ->
    ChannelPubKey = channel_pubkey(Tx),
    case aec_chain:get_channel(ChannelPubKey, Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator_pubkey(Channel),
                  aesc_channels:responder_pubkey(Channel)]};
        {error, not_found} -> {error, channel_not_found}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_set_delegates_tx{channel_id             = ChannelId,
                                    from_id                = FromId,
                                    initiator_delegate_ids = IDelegates,
                                    responder_delegate_ids = RDelegates,
                                    payload                = Payload,
                                    state_hash             = StateHash,
                                    round                  = Round,
                                    ttl                    = TTL,
                                    fee                    = Fee,
                                    nonce                  = Nonce} = Tx) ->
    {version(Tx),
     [ {channel_id            , ChannelId}
     , {from_id               , FromId}
     , {initiator_delegate_ids, IDelegates}
     , {responder_delegate_ids, RDelegates}
     , {payload               , Payload}
     , {state_hash            , StateHash}
     , {round                 , Round}
     , {ttl                   , TTL}
     , {fee                   , Fee}
     , {nonce                 , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?INITIAL_VSN,
            [ {channel_id            , ChannelId}
            , {from_id               , FromId}
            , {initiator_delegate_ids, IDelegates}
            , {responder_delegate_ids, RDelegates}
            , {payload               , Payload}
            , {state_hash            , StateHash}
            , {round                 , Round}
            , {ttl                   , TTL}
            , {fee                   , Fee}
            , {nonce                 , Nonce}]) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    lists:all(fun(Id) -> account =:= aeser_id:specialize_type(Id) end,
              IDelegates ++ RDelegates),
    #channel_set_delegates_tx{channel_id             = ChannelId,
                              from_id                = FromId,
                              initiator_delegate_ids = IDelegates,
                              responder_delegate_ids = RDelegates,
                              payload                = Payload,
                              state_hash             = StateHash,
                              round                  = Round,
                              ttl                    = TTL,
                              fee                    = Fee,
                              nonce                  = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_set_delegates_tx{channel_id             = ChannelId,
                                     from_id                = FromId,
                                     initiator_delegate_ids = IDelegates,
                                     responder_delegate_ids = RDelegates,
                                     payload                = Payload,
                                     state_hash             = StateHash,
                                     round                  = Round,
                                     ttl                    = TTL,
                                     fee                    = Fee,
                                     nonce                  = Nonce}) ->
    #{<<"channel_id">>             => aeser_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>                => aeser_api_encoder:encode(id_hash, FromId),
      <<"initiator_delegate_ids">> => [aeser_api_encoder:encode(id_hash, Id)
                                       || Id <- IDelegates],
      <<"responder_delegate_ids">> => [aeser_api_encoder:encode(id_hash, Id)
                                       || Id <- RDelegates],
      <<"payload">>                => aeser_api_encoder:encode(transaction, Payload),
      <<"state_hash">>             => aeser_api_encoder:encode(state, StateHash),
      <<"round">>                  => Round,
      <<"ttl">>                    => TTL,
      <<"fee">>                    => Fee,
      <<"nonce">>                  => Nonce}.

serialization_template(?INITIAL_VSN) ->
    [ {channel_id               , id}
    , {from_id                  , id}
    , {initiator_delegate_ids   , [id]}
    , {responder_delegate_ids   , [id]}
    , {payload                  , binary}
    , {state_hash               , binary}
    , {round                    , int}
    , {ttl                      , int}
    , {fee                      , int}
    , {nonce                    , int}
    ].

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?INITIAL_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, _) when Protocol < ?IRIS_PROTOCOL_VSN -> false;
valid_at_protocol(Protocol, #channel_set_delegates_tx{payload = Payload}) ->
    aesc_utils:is_payload_valid_at_protocol(Protocol, Payload).

