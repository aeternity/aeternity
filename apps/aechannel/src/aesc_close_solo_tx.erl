%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel close solo transaction
%%% @end
%%%=============================================================================
-module(aesc_close_solo_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/6,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_CLOSE_SOLO_TX_VSN, 1).
-define(CHANNEL_CLOSE_SOLO_TX_TYPE, channel_close_solo_tx).

-type vsn() :: non_neg_integer().

-record(channel_close_solo_tx, {
          channel_id :: aec_id:id(),
          from_id    :: aec_id:id(),
          payload    :: binary(),
          poi        :: aec_trees:poi(),
          ttl        :: aetx:tx_ttl(),
          fee        :: non_neg_integer(),
          nonce      :: non_neg_integer()
         }).

-opaque tx() :: #channel_close_solo_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      from_id    := FromId,
      payload    := Payload,
      poi        := PoI,
      fee        := Fee,
      nonce      := Nonce} = Args) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    Tx = #channel_close_solo_tx{
            channel_id = ChannelId,
            from_id    = FromId,
            payload    = Payload,
            poi        = PoI,
            ttl        = maps:get(ttl, Args, 0),
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CLOSE_SOLO_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_close_solo_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_close_solo_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_solo_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_close_solo_tx{} = Tx) ->
    from_pubkey(Tx).

channel_pubkey(#channel_close_solo_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

from_pubkey(#channel_close_solo_tx{from_id = FromPubKey}) ->
    aec_id:specialize(FromPubKey, account).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_solo_tx{payload    = Payload,
                             poi        = PoI,
                             fee        = Fee,
                             nonce      = Nonce} = Tx, _Context, Trees, _Height, _ConsensusVersion) ->
    ChannelPubKey  = channel_pubkey(Tx),
    FromPubKey = from_pubkey(Tx),
    case aesc_utils:check_solo_close_payload(ChannelPubKey, FromPubKey, Nonce, Fee,
                                        Payload, PoI, Trees) of
        ok -> {ok, Trees};
        Err -> Err
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(),
              non_neg_integer(), binary() | no_tx_hash) -> {ok, aec_trees:trees()}.
process(#channel_close_solo_tx{payload    = Payload,
                               poi        = PoI,
                               fee        = Fee,
                               nonce      = Nonce} = Tx, _Context, Trees,
        Height, _ConsensusVersion, _TxHash) ->
    ChannelPubKey  = channel_pubkey(Tx),
    FromPubKey     = from_pubkey(Tx),
    aesc_utils:process_solo_close(ChannelPubKey, FromPubKey, Nonce, Fee,
                                  Payload, PoI, Height, Trees).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_close_solo_tx{} = Tx, _) ->
    {ok, [from_pubkey(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_close_solo_tx{channel_id = ChannelId,
                                 from_id    = FromId,
                                 payload    = Payload,
                                 poi        = PoI,
                                 ttl        = TTL,
                                 fee        = Fee,
                                 nonce      = Nonce}) ->
    {version(),
     [ {channel_id, ChannelId}
     , {from_id   , FromId}
     , {payload   , Payload}
     , {poi       , aec_trees:serialize_poi(PoI)}
     , {ttl       , TTL}
     , {fee       , Fee}
     , {nonce     , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CLOSE_SOLO_TX_VSN,
            [ {channel_id, ChannelId}
            , {from_id   , FromId}
            , {payload   , Payload}
            , {poi       , PoI}
            , {ttl       , TTL}
            , {fee       , Fee}
            , {nonce     , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    #channel_close_solo_tx{channel_id = ChannelId,
                           from_id    = FromId,
                           payload    = Payload,
                           poi        = aec_trees:deserialize_poi(PoI),
                           ttl        = TTL,
                           fee        = Fee,
                           nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_solo_tx{channel_id = ChannelId,
                                  from_id    = FromId,
                                  payload    = Payload,
                                  poi        = PoI,
                                  ttl        = TTL,
                                  fee        = Fee,
                                  nonce      = Nonce}) ->
    #{<<"vsn">>         => version(),
      <<"channel_id">>  => aec_base58c:encode(id_hash, ChannelId),
      <<"from_id">>     => aec_base58c:encode(id_hash, FromId),
      <<"payload">>     => Payload,
      <<"poi">>         => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI)),
      <<"ttl">>         => TTL,
      <<"fee">>         => Fee,
      <<"nonce">>       => Nonce}.

serialization_template(?CHANNEL_CLOSE_SOLO_TX_VSN) ->
    [ {channel_id, id}
    , {from_id   , id}
    , {payload   , binary}
    , {poi       , binary}
    , {ttl       , int}
    , {fee       , int}
    , {nonce     , int}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_SOLO_TX_VSN.

