%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel force progress on-chain transaction
%%% @end
%%%=============================================================================
-module(aesc_force_progress_tx).

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

-define(CHANNEL_FORCE_PROGRESS_TX_VSN, 1).
-define(CHANNEL_FORCE_PROGRESS_TX_TYPE, channel_force_progress_tx).
-define(CHANNEL_FORCE_PROGRESS_TX_FEE, 1).

-type vsn() :: non_neg_integer().

-record(channel_force_progress_tx, {
          channel_id    :: aec_id:id(),
          from_id       :: aec_id:id(),
          payload       :: binary(),
          solo_payload  :: binary(),
          addresses     :: [aec_id:id()],
          poi           :: aec_trees:poi(),
          ttl           :: aetx:tx_ttl(),
          fee           :: non_neg_integer(),
          nonce         :: non_neg_integer()
         }).

-opaque tx() :: #channel_force_progress_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id    := ChannelId,
      from_id       := FromId,
      payload       := Payload,
      solo_payload  := SoloPayload,
      addresses     := Addresses,
      poi           := PoI,
      fee           := Fee,
      nonce         := Nonce} = Args) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    true =
        lists:all(
            fun(Id) ->
                T = aec_id:specialize_type(Id),
                T =:= account orelse T =:= contract
            end,
            Addresses),
    Tx = #channel_force_progress_tx{
            channel_id    = ChannelId,
            from_id       = FromId,
            payload       = Payload,
            solo_payload  = SoloPayload,
            addresses     = Addresses,
            poi           = PoI,
            ttl           = maps:get(ttl, Args, 0),
            fee           = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_FORCE_PROGRESS_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_force_progress_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_force_progress_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_force_progress_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_force_progress_tx{} = Tx) ->
    from_pubkey(Tx).

channel(#channel_force_progress_tx{channel_id = ChannelId}) ->
    ChannelId.

channel_hash(#channel_force_progress_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

from_id(#channel_force_progress_tx{from_id = FromId}) ->
    FromId.

from_pubkey(#channel_force_progress_tx{from_id = FromId}) ->
    aec_id:specialize(FromId, account).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_force_progress_tx{payload      = Payload,
                                 solo_payload = SoloPayload,
                                 addresses    = Addresses,
                                 poi          = PoI,
                                 fee          = Fee,
                                 nonce        = Nonce} = Tx, _Context, Trees, Height, _ConsensusVersion) ->
    ChannelId  = channel_hash(Tx),
    FromPubKey = from_pubkey(Tx),
    case aesc_utils:check_force_progress(ChannelId, FromPubKey, Nonce, Fee,
                                   Payload, SoloPayload, Addresses,
                                   PoI, Height, Trees) of
        ok -> {ok, Trees};
        Err -> Err
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(),
              non_neg_integer(), binary() | no_tx_hash) -> {ok, aec_trees:trees()}.
process(#channel_force_progress_tx{payload      = Payload,
                                   solo_payload = SoloPayload,
                                   addresses    = Addresses,
                                   poi          = PoI,
                                   fee          = Fee,
                                   nonce        = Nonce} = Tx, _Context,
        Trees, Height, _ConsensusVersion, TxHash) when is_binary(TxHash) ->
    ChannelId  = channel_hash(Tx),
    FromPubKey = from_pubkey(Tx),
    aesc_utils:process_force_progress(ChannelId, FromPubKey, Nonce, Fee,
                                      Payload, SoloPayload, Addresses,
                                      PoI, TxHash, Height, Trees).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_force_progress_tx{} = Tx, _) ->
    {ok, [from_pubkey(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_force_progress_tx{channel_id   = ChannelId,
                                     from_id      = FromId,
                                     payload      = Payload,
                                     solo_payload = SoloPayload,
                                     addresses    = Addresses,
                                     poi          = PoI,
                                     ttl          = TTL,
                                     fee          = Fee,
                                     nonce        = Nonce}) ->
    {version(),
     [ {channel_id    , ChannelId}
     , {from_id       , FromId}
     , {payload       , Payload}
     , {solo_payload  , SoloPayload}
     , {addresses     , Addresses}
     , {poi           , aec_trees:serialize_poi(PoI)}
     , {ttl           , TTL}
     , {fee           , Fee}
     , {nonce         , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_FORCE_PROGRESS_TX_VSN,
            [ {channel_id   , ChannelId}
            , {from_id      , FromId}
            , {payload      , Payload}
            , {solo_payload , SoloPayload}
            , {address      , Addresses}
            , {poi          , PoI}
            , {ttl          , TTL}
            , {fee          , Fee}
            , {nonce        , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    #channel_force_progress_tx{channel_id   = ChannelId,
                               from_id      = FromId,
                               payload      = Payload,
                               solo_payload = SoloPayload,
                               addresses    = Addresses,
                               poi          = aec_trees:deserialize_poi(PoI),
                               ttl          = TTL,
                               fee          = Fee,
                               nonce        = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_force_progress_tx{payload       = Payload,
                                      solo_payload  = SoloPayload,
                                      addresses     = Addresses,
                                      poi           = PoI,
                                      ttl           = TTL,
                                      fee           = Fee,
                                      nonce         = Nonce} = Tx) ->
    #{<<"vsn">>           => version(),
      <<"channel_id">>    => aec_base58c:encode(id_hash, channel(Tx)),
      <<"from_id">>       => aec_base58c:encode(id_hash, from_id(Tx)),
      <<"payload">>       => Payload,
      <<"solo_payload">>  => SoloPayload,
      <<"addresses">>     => [aec_base58c:encode(id_hash, Id) || Id <- Addresses],
      <<"poi">>           => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI)),
      <<"ttl">>           => TTL,
      <<"fee">>           => Fee,
      <<"nonce">>         => Nonce}.

serialization_template(?CHANNEL_FORCE_PROGRESS_TX_VSN) ->
    [ {channel_id     , id}
    , {from_id        , id}
    , {payload        , binary}
    , {solo_payload   , binary}
    , {addresses      , [id]}
    , {poi            , binary}
    , {ttl            , int}
    , {fee            , int}
    , {nonce          , int}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_FORCE_PROGRESS_TX_VSN.

