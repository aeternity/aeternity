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
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_SETTLE_TX_VSN, 1).
-define(CHANNEL_SETTLE_TX_TYPE, channel_settle_tx).

-type vsn() :: non_neg_integer().

-record(channel_settle_tx, {
          channel_id              :: aec_id:id(),
          from_id                 :: aec_id:id(),
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
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
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

from_pubkey(#channel_settle_tx{from_id = FromId}) ->
    aec_id:specialize(FromId, account).

channel_pubkey(#channel_settle_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_settle_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#channel_settle_tx{initiator_amount_final = InitiatorAmount,
                           responder_amount_final = ResponderAmount} = Tx,
        Trees, Env) ->
    Instructions =
        aec_tx_processor:channel_settle_tx_instructions(
          from_pubkey(Tx),
          channel_pubkey(Tx),
          InitiatorAmount,
          ResponderAmount,
          fee(Tx),
          nonce(Tx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

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
                             nonce                  = Nonce}) ->
    {version(),
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
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
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
    #{<<"channel_id">>             => aehttp_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>                => aehttp_api_encoder:encode(id_hash, FromId),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_channel(ChannelPubKey, FromPubKey, InitiatorAmount, ResponderAmount, Height, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelPubKey, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            Checks =
                [fun() -> aesc_utils:check_is_peer(FromPubKey, aesc_channels:peers(Channel)) end,
                 fun() -> check_solo_closed(Channel, Height) end,
                 %% check total amount
                 fun() -> check_are_funds_in_channel(ChannelPubKey,
                                InitiatorAmount + ResponderAmount, Trees) end,
                 %% check individual amounts are what is expected
                 fun() -> check_peer_amount(InitiatorAmount,
                                            aesc_channels:initiator_amount(Channel))
                 end,
                 fun() -> check_peer_amount(ResponderAmount,
                                            aesc_channels:responder_amount(Channel))
                 end],
            aeu_validation:run(Checks)
    end.

check_peer_amount(ExpectedAmt, Amt) ->
    case Amt =:= ExpectedAmt of
        true -> ok;
        false -> {error, wrong_amt}
    end.

check_solo_closed(Channel, Height) ->
    case aesc_channels:is_solo_closed(Channel, Height) of
        true  -> ok;
        false -> {error, channel_not_closed}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_SETTLE_TX_VSN.

-spec check_are_funds_in_channel(aesc_channels:pubkey(), non_neg_integer(), aec_trees:trees()) ->
                                        ok | {error, insufficient_channel_funds}.
check_are_funds_in_channel(ChannelPubKey, Amount, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    Channel      = aesc_state_tree:get(ChannelPubKey, ChannelsTree),
    case aesc_channels:channel_amount(Channel) >= Amount of
        true  -> ok;
        false -> {error, insufficient_channel_funds}
    end.

