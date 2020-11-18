%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel solo snapshot transaction
%%% @end
%%%=============================================================================
-module(aesc_snapshot_solo_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         channel_pubkey/1,
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
         valid_at_protocol/2,
         payload/1
        ]).

% aesc_signable_transaction callbacks
-export([channel_id/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_SNAPSHOT_SOLO_TX_VSN, 1).
-define(CHANNEL_SNAPSHOT_SOLO_TX_TYPE, channel_snapshot_solo_tx).

-type vsn() :: non_neg_integer().

-record(channel_snapshot_solo_tx, {
          channel_id :: aeser_id:id(),
          from_id    :: aeser_id:id(),
          payload    :: binary(),
          ttl        :: aetx:tx_ttl(),
          fee        :: non_neg_integer(),
          nonce      :: non_neg_integer()
         }).

-opaque tx() :: #channel_snapshot_solo_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      from_id    := FromId,
      payload    := Payload,
      fee        := Fee,
      nonce      := Nonce} = Args) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    Tx = #channel_snapshot_solo_tx{
            channel_id = ChannelId,
            from_id    = FromId,
            payload    = Payload,
            ttl        = maps:get(ttl, Args, 0),
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_SNAPSHOT_SOLO_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_snapshot_solo_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_snapshot_solo_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_snapshot_solo_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_snapshot_solo_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_snapshot_solo_tx{} = Tx) ->
    from_pubkey(Tx).

-spec entities(tx()) -> [aeser_id:id()].
%% origin id first
entities(#channel_snapshot_solo_tx{channel_id = ChId, from_id = FromId}) ->
    [FromId, ChId].

-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_snapshot_solo_tx{channel_id = ChannelId}) ->
    aeser_id:specialize(ChannelId, channel).

channel_id(#channel_snapshot_solo_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec payload(tx()) -> binary().
payload(#channel_snapshot_solo_tx{payload = Payload}) ->
    Payload.

from_pubkey(#channel_snapshot_solo_tx{from_id = FromId}) ->
    aeser_id:specialize(FromId, account).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_snapshot_solo_tx{payload    = Payload,
                                fee        = Fee,
                                nonce      = Nonce} = Tx, Trees, Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    case aesc_utils:check_solo_snapshot_payload(
           ChannelPubKey, FromPubKey, Nonce, Fee, Payload, Trees, Env) of
        ok -> {ok, Trees};
        Err -> Err
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_snapshot_solo_tx{payload    = Payload,
                                  fee        = Fee,
                                  nonce      = Nonce} = Tx,
        Trees, Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    aesc_utils:process_solo_snapshot(ChannelPubKey, FromPubKey, Nonce, Fee, Payload, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_snapshot_solo_tx{} = Tx, _) ->
    {ok, [from_pubkey(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_snapshot_solo_tx{channel_id = ChannelId,
                                    from_id    = FromId,
                                    payload    = Payload,
                                    ttl        = TTL,
                                    fee        = Fee,
                                    nonce      = Nonce} = Tx) ->
    {version(Tx),
     [ {channel_id, ChannelId}
     , {from_id   , FromId}
     , {payload   , Payload}
     , {ttl       , TTL}
     , {fee       , Fee}
     , {nonce     , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_SNAPSHOT_SOLO_TX_VSN,
            [ {channel_id, ChannelId}
            , {from_id   , FromId}
            , {payload   , Payload}
            , {ttl       , TTL}
            , {fee       , Fee}
            , {nonce     , Nonce}]) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    #channel_snapshot_solo_tx{channel_id = ChannelId,
                              from_id    = FromId,
                              payload    = Payload,
                              ttl        = TTL,
                              fee        = Fee,
                              nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_snapshot_solo_tx{channel_id = ChannelId,
                                     from_id    = FromId,
                                     payload    = Payload,
                                     ttl        = TTL,
                                     fee        = Fee,
                                     nonce      = Nonce}) ->
    #{<<"channel_id">>  => aeser_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>     => aeser_api_encoder:encode(id_hash, FromId),
      <<"payload">>     => aeser_api_encoder:encode(transaction, Payload),
      <<"ttl">>         => TTL,
      <<"fee">>         => Fee,
      <<"nonce">>       => Nonce}.

serialization_template(?CHANNEL_SNAPSHOT_SOLO_TX_VSN) ->
    [ {channel_id, id}
    , {from_id   , id}
    , {payload   , binary}
    , {ttl       , int}
    , {fee       , int}
    , {nonce     , int}
    ].

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?CHANNEL_SNAPSHOT_SOLO_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, #channel_snapshot_solo_tx{payload = Payload}) ->
    aesc_utils:is_payload_valid_at_protocol(Protocol, Payload).

