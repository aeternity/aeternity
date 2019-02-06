%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel close mutual transaction
%%% @end
%%%=============================================================================
-module(aesc_close_mutual_tx).

-behavior(aetx).

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
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         initiator_amount_final/1,
         responder_amount_final/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type vsn() :: non_neg_integer().

-define(CHANNEL_CLOSE_MUTUAL_TX_VSN, 1).
-define(CHANNEL_CLOSE_MUTUAL_TX_TYPE, channel_close_mutual_tx).

-record(channel_close_mutual_tx, {
          channel_id              :: aec_id:id(),
          from_id                 :: aec_id:id(),
          initiator_amount_final  :: non_neg_integer(),
          responder_amount_final  :: non_neg_integer(),
          ttl                     :: aetx:tx_ttl(),
          fee                     :: non_neg_integer(),
          nonce                   :: non_neg_integer()
         }).

-opaque tx() :: #channel_close_mutual_tx{}.

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
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    Tx = #channel_close_mutual_tx{
            channel_id              = ChannelId,
            from_id                 = FromId,
            initiator_amount_final  = InitiatorAmount,
            responder_amount_final  = ResponderAmount,
            ttl                     = maps:get(ttl, Args, 0),
            fee                     = Fee,
            nonce                   = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CLOSE_MUTUAL_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_close_mutual_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_close_mutual_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_close_mutual_tx{ttl = Ttl}) ->
    Ttl.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_mutual_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_close_mutual_tx{from_id = FromId}) ->
    aec_id:specialize(FromId, account).

channel_pubkey(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_mutual_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#channel_close_mutual_tx{from_id = FromId} = Tx, Trees, Env) ->
    FromPubkey = aec_id:specialize(FromId, account),
    Instructions =
        aec_tx_processor:channel_close_mutual_tx_instructions(
          FromPubkey,
          channel_pubkey(Tx),
          initiator_amount_final(Tx),
          responder_amount_final(Tx),
          nonce(Tx),
          fee(Tx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}
                                        | {error, channel_not_found}.
signers(#channel_close_mutual_tx{} = Tx, Trees) ->
    case aec_chain:get_channel(channel_pubkey(Tx), Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator_pubkey(Channel),
                  aesc_channels:responder_pubkey(Channel)]};
        {error, not_found} -> {error, channel_not_found}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_close_mutual_tx{channel_id             = ChannelId,
                                   from_id                = FromId,
                                   initiator_amount_final = InitiatorAmount,
                                   responder_amount_final = ResponderAmount,
                                   ttl                    = TTL,
                                   fee                    = Fee,
                                   nonce                  = Nonce}) ->
    {version(),
     [ {channel_id              , ChannelId}
     , {from_id                 , FromId}
     , {initiator_amount_final  , InitiatorAmount}
     , {responder_amount_final  , ResponderAmount}
     , {ttl                     , TTL}
     , {fee                     , Fee}
     , {nonce                   , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CLOSE_MUTUAL_TX_VSN,
            [ {channel_id             , ChannelId}
            , {from_id                , FromId}
            , {initiator_amount_final , InitiatorAmount}
            , {responder_amount_final , ResponderAmount}
            , {ttl                    , TTL}
            , {fee                    , Fee}
            , {nonce                  , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    #channel_close_mutual_tx{channel_id             = ChannelId,
                             from_id                = FromId,
                             initiator_amount_final = InitiatorAmount,
                             responder_amount_final = ResponderAmount,
                             ttl                    = TTL,
                             fee                    = Fee,
                             nonce                  = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_mutual_tx{channel_id             = ChannelId,
                                    from_id                = FromId,
                                    initiator_amount_final = InitiatorAmount,
                                    responder_amount_final = ResponderAmount,
                                    ttl                    = TTL,
                                    fee                    = Fee,
                                    nonce                  = Nonce}) ->
    #{<<"channel_id">>              => aehttp_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>                 => aehttp_api_encoder:encode(id_hash, FromId),
      <<"initiator_amount_final">>  => InitiatorAmount,
      <<"responder_amount_final">>  => ResponderAmount,
      <<"ttl">>                     => TTL,
      <<"fee">>                     => Fee,
      <<"nonce">>                   => Nonce}.

serialization_template(?CHANNEL_CLOSE_MUTUAL_TX_VSN) ->
    [ {channel_id             , id}
    , {from_id                , id}
    , {initiator_amount_final , int}
    , {responder_amount_final , int}
    , {ttl                    , int}
    , {fee                    , int}
    , {nonce                  , int}
    ].

-spec initiator_amount_final(tx()) -> integer().
initiator_amount_final(#channel_close_mutual_tx{initiator_amount_final  = Amount}) ->
    Amount.

-spec responder_amount_final(tx()) -> integer().
responder_amount_final(#channel_close_mutual_tx{responder_amount_final  = Amount}) ->
    Amount.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_MUTUAL_TX_VSN.

