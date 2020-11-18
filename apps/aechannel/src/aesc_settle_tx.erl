%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel settle transaction
%%% @end
%%%=============================================================================
-module(aesc_settle_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         entities/1,
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
         channel_pubkey/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_SETTLE_TX_VSN, 1).
-define(CHANNEL_SETTLE_TX_TYPE, channel_settle_tx).

-type vsn() :: non_neg_integer().

-record(channel_settle_tx, {
          channel_id              :: aeser_id:id(),
          from_id                 :: aeser_id:id(),
          initiator_amount_final  :: non_neg_integer(),
          responder_amount_final  :: non_neg_integer(),
          ttl                     :: aetx:tx_ttl(),
          fee                     :: non_neg_integer(),
          nonce                   :: non_neg_integer()
         }).

-opaque tx() :: #channel_settle_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id              := ChannelId,
      from_id                 := FromId,
      initiator_amount_final  := InitiatorAmount,
      responder_amount_final  := ResponderAmount,
      fee                     := Fee,
      nonce                   := Nonce} = Args) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    Tx = #channel_settle_tx{
            channel_id              = ChannelId,
            from_id                 = FromId,
            initiator_amount_final  = InitiatorAmount,
            responder_amount_final  = ResponderAmount,
            ttl                     = maps:get(ttl, Args, 0),
            fee                     = Fee,
            nonce                   = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_SETTLE_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_settle_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_settle_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_settle_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_settle_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_settle_tx{} = Tx) ->
    from_pubkey(Tx).

-spec entities(tx()) -> [aeser_id:id()].
%% origin id first
entities(#channel_settle_tx{channel_id = ChId, from_id = FromId}) ->
    %% Unfortunately, we cannot include the peer id here
    [FromId, ChId].

from_pubkey(#channel_settle_tx{from_id = FromId}) ->
    aeser_id:specialize(FromId, account).

-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_settle_tx{channel_id = ChannelId}) ->
    aeser_id:specialize(ChannelId, channel).

channel_id(#channel_settle_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_settle_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_settle_tx{initiator_amount_final = InitiatorAmount,
                           responder_amount_final = ResponderAmount} = Tx,
        Trees, Env) ->
    Instructions =
        aeprimop:channel_settle_tx_instructions(
          from_pubkey(Tx),
          channel_pubkey(Tx),
          InitiatorAmount,
          ResponderAmount,
          fee(Tx),
          nonce(Tx)),
    aeprimop:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_settle_tx{} = Tx, _) ->
    {ok, [from_pubkey(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_settle_tx{channel_id             = ChannelId,
                             from_id                = FromId,
                             initiator_amount_final = InitiatorAmount,
                             responder_amount_final = ResponderAmount,
                             ttl                    = TTL,
                             fee                    = Fee,
                             nonce                  = Nonce} = Tx) ->
    {version(Tx),
    [ {channel_id             , ChannelId}
    , {from_id                , FromId}
    , {initiator_amount_final , InitiatorAmount}
    , {responder_amount_final , ResponderAmount}
    , {ttl                    , TTL}
    , {fee                    , Fee}
    , {nonce                  , Nonce}
    ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_SETTLE_TX_VSN,
            [ {channel_id             , ChannelId}
            , {from_id                , FromId}
            , {initiator_amount_final , InitiatorAmount}
            , {responder_amount_final , ResponderAmount}
            , {ttl                    , TTL}
            , {fee                    , Fee}
            , {nonce                  , Nonce}]) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    #channel_settle_tx{channel_id             = ChannelId,
                       from_id                = FromId,
                       initiator_amount_final = InitiatorAmount,
                       responder_amount_final = ResponderAmount,
                       ttl                    = TTL,
                       fee                    = Fee,
                       nonce                  = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_settle_tx{channel_id             = ChannelId,
                              from_id                = FromId,
                              initiator_amount_final = InitiatorAmount,
                              responder_amount_final = ResponderAmount,
                              ttl                    = TTL,
                              fee                    = Fee,
                              nonce                  = Nonce}) ->
    #{<<"channel_id">>             => aeser_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>                => aeser_api_encoder:encode(id_hash, FromId),
      <<"initiator_amount_final">> => InitiatorAmount,
      <<"responder_amount_final">> => ResponderAmount,
      <<"ttl">>                    => TTL,
      <<"fee">>                    => Fee,
      <<"nonce">>                  => Nonce}.


serialization_template(?CHANNEL_SETTLE_TX_VSN) ->
    [ {channel_id             , id}
    , {from_id                , id}
    , {initiator_amount_final , int}
    , {responder_amount_final , int}
    , {ttl                    , int}
    , {fee                    , int}
    , {nonce                  , int}
    ].

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?CHANNEL_SETTLE_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_, _) ->
    true.

