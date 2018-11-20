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
check(#channel_settle_tx{initiator_amount_final = InitiatorAmount,
                         responder_amount_final = ResponderAmount,
                         fee                    = Fee,
                         nonce                  = Nonce} = Tx,
      Trees, Env) ->
    Height        = aetx_env:height(Env),
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Nonce, Fee) end,
         fun() -> check_channel(ChannelPubKey, FromPubKey, InitiatorAmount,
                                ResponderAmount, Height, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#channel_settle_tx{initiator_amount_final = InitiatorAmount,
                           responder_amount_final = ResponderAmount,
                           fee                    = Fee,
                           nonce                  = Nonce} = Tx,
        Trees,_Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    Channel0        = aesc_state_tree:get(ChannelPubKey, ChannelsTree0),
    InitiatorPubKey = aesc_channels:initiator_pubkey(Channel0),
    InitiatorAmount = aesc_channels:initiator_amount(Channel0), % same amt
    ResponderPubKey = aesc_channels:responder_pubkey(Channel0),
    ResponderAmount = aesc_channels:responder_amount(Channel0), % same amt

    InitiatorAccount0       = aec_accounts_trees:get(InitiatorPubKey, AccountsTree0),
    ResponderAccount0       = aec_accounts_trees:get(ResponderPubKey, AccountsTree0),
    {ok, InitiatorAccount1} = aec_accounts:earn(InitiatorAccount0, InitiatorAmount),
    {ok, ResponderAccount1} = aec_accounts:earn(ResponderAccount0, ResponderAmount),

    {InitiatorAccount3, ResponderAccount3} =
        case FromPubKey of
            InitiatorPubKey ->
                {ok, InitiatorAccount2} = aec_accounts:spend(InitiatorAccount1, Fee, Nonce),
                {InitiatorAccount2, ResponderAccount1};
            ResponderPubKey ->
                {ok, ResponderAccount2} = aec_accounts:spend(ResponderAccount1, Fee, Nonce),
                {InitiatorAccount1, ResponderAccount2}
        end,

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount3, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ResponderAccount3, AccountsTree1),

    % all coins that are not in the final balance are locked
    % they're sent to the special account that holds locked coins
    DistributedAmounts = InitiatorAmount + ResponderAmount,
    MissingCoins = aesc_channels:channel_amount(Channel0) - DistributedAmounts,
    AccountsTree3 = aec_accounts_trees:lock_coins(MissingCoins,
                                                  AccountsTree2),

    ChannelsTree1 = aesc_state_tree:delete(aesc_channels:pubkey(Channel0), ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree3),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

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

