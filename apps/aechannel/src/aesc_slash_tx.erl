%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel slash transaction
%%% @end
%%%=============================================================================
-module(aesc_slash_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_SLASH_TX_VSN, 1).
-define(CHANNEL_SLASH_TX_TYPE, channel_slash_tx).
-define(CHANNEL_SLASH_TX_FEE, 0).

-type vsn() :: non_neg_integer().

-record(channel_slash_tx, {
          channel_id :: aec_id:id(),
          from       :: aec_id:id(),
          payload    :: binary(),
          poi        :: aec_trees:poi(),
          ttl        :: aetx:tx_ttl(),
          fee        :: non_neg_integer(),
          nonce      :: non_neg_integer()
         }).

-opaque tx() :: #channel_slash_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelIdBin,
      from       := FromPubKey,
      payload    := Payload,
      poi        := PoI,
      fee        := Fee,
      nonce      := Nonce} = Args) ->
    Tx = #channel_slash_tx{
            channel_id = aec_id:create(channel, ChannelIdBin),
            from       = aec_id:create(account, FromPubKey),
            payload    = Payload,
            poi        = PoI,
            ttl        = maps:get(ttl, Args, 0),
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_SLASH_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_slash_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_slash_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_slash_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_slash_tx{} = Tx) ->
    from(Tx).

from(#channel_slash_tx{from = FromId}) ->
    aec_id:specialize(FromId, account).

channel(#channel_slash_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_slash_tx{payload    = Payload,
                        poi        = PoI,
                        fee        = Fee,
                        nonce      = Nonce} = Tx, _Context, Trees, Height,  _ConsensusVersion) ->
    ChannelId  = channel(Tx),
    FromPubKey = from(Tx),
    aesc_utils:check_slash_payload(ChannelId, FromPubKey, Nonce, Fee,
                                   Payload, PoI, Height, Trees).


-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#channel_slash_tx{payload    = Payload,
                          poi        = PoI,
                          fee        = Fee,
                          nonce      = Nonce} = Tx, _Context, Trees, Height, _ConsensusVersion) ->
    ChannelId  = channel(Tx),
    FromPubKey = from(Tx),
    aesc_utils:process_slash(ChannelId, FromPubKey, Nonce, Fee,
                             Payload, PoI, Height, Trees).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_slash_tx{} = Tx, _) ->
    {ok, [from(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_slash_tx{channel_id = ChannelId,
                            from       = FromId,
                            payload    = Payload,
                            poi        = PoI,
                            ttl        = TTL,
                            fee        = Fee,
                            nonce      = Nonce}) ->
    {version(),
     [ {channel_id, ChannelId}
     , {from      , FromId}
     , {payload   , Payload}
     , {poi       , aec_trees:serialize_poi(PoI)}
     , {ttl       , TTL}
     , {fee       , Fee}
     , {nonce     , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_SLASH_TX_VSN,
            [ {channel_id, ChannelId}
            , {from      , FromId}
            , {payload   , Payload}
            , {poi       , PoI}
            , {ttl       , TTL}
            , {fee       , Fee}
            , {nonce     , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    #channel_slash_tx{channel_id = ChannelId,
                      from       = FromId,
                      payload    = Payload,
                      poi        = aec_trees:deserialize_poi(PoI),
                      ttl        = TTL,
                      fee        = Fee,
                      nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_slash_tx{payload    = Payload,
                             poi        = PoI,
                             ttl        = TTL,
                             fee        = Fee,
                             nonce      = Nonce} = Tx) ->
    #{<<"data_schema">>=> <<"ChannelSlashTxJSON">>, % swagger schema name
      <<"vsn">>        => version(),
      <<"channel_id">> => aec_base58c:encode(channel, channel(Tx)),
      <<"from">>       => aec_base58c:encode(account_pubkey, from(Tx)),
      <<"payload">>    => Payload,
      <<"poi">>        => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI)),
      <<"ttl">>        => TTL,
      <<"fee">>        => Fee,
      <<"nonce">>      => Nonce}.

serialization_template(?CHANNEL_SLASH_TX_VSN) ->
    [ {channel_id, id}
    , {from      , id}
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
    ?CHANNEL_SLASH_TX_VSN.
