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
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Nonce, Fee) end,
         fun() -> check_payload(ChannelId, FromPubKey, Payload, PoI, Height, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#channel_slash_tx{payload    = Payload,
                          poi        = PoI,
                          fee        = Fee,
                          nonce      = Nonce} = Tx, _Context, Trees, Height, _ConsensusVersion) ->
    ChannelId  = channel(Tx),
    FromPubKey = from(Tx),
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    FromAccount0       = aec_accounts_trees:get(FromPubKey, AccountsTree0),
    {ok, FromAccount1} = aec_accounts:spend(FromAccount0, Fee, Nonce),
    AccountsTree1           = aec_accounts_trees:enter(FromAccount1, AccountsTree0),

    {ok, _SignedTx, PayloadTx} = deserialize_from_binary(Payload),
    Channel0                   = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1                   = aesc_channels:slash(Channel0, PayloadTx, PoI, Height),
    ChannelsTree1              = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

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

-spec check_payload(aesc_channels:id(), aec_keys:pubkey(), binary(),
                    aec_trees:poi(), aec_blocks:height(), aec_trees:trees()) ->
                           ok | {error, term()}.
check_payload(ChannelId, FromPubKey, Payload, PoI, Height, Trees) ->
    case deserialize_from_binary(Payload) of
        {ok, SignedState, PayloadTx} ->
            Checks =
                [fun() -> is_peer(FromPubKey, SignedState, Trees) end,
                 fun() -> aetx_sign:verify(SignedState, Trees) end,
                 fun() -> check_channel(ChannelId, PayloadTx, PoI, Height, Trees) end,
                 fun() -> check_root_hash(PayloadTx, PoI) end],
            aeu_validation:run(Checks);
        {error, _Reason} = Error ->
            Error
    end.

-spec deserialize_from_binary(binary()) -> {ok, aetx_sign:signed_tx(), aesc_payload:tx()}
                                         | {error, bad_offchain_state_type}.
deserialize_from_binary(Payload) ->
    try
        SignedTx = aetx_sign:deserialize_from_binary(Payload),
        Tx       = aetx_sign:tx(SignedTx),
        case aesc_payload:new(Tx) of
            {ok, PayloadTx} ->
                {ok, SignedTx, PayloadTx};
            error ->
                {error, bad_offchain_state_type}
        end
    catch _:_ ->
            {error, payload_deserialization_failed}
    end.

is_peer(AccountPubKey, SignedState, Trees) ->
    Tx = aetx_sign:tx(SignedState),
    case aetx:signers(Tx, Trees) of
        {ok, Signers} ->
            case lists:member(AccountPubKey, Signers) of
                true  -> ok;
                false -> {error, account_not_peer}
            end;
        {error, _Reason}=Err -> Err
    end.

-spec check_channel(aesc_channels:id(), aesc_payload:tx(),
                    aec_trees:poi(), aec_blocks:height(), aec_trees:trees())
    -> ok | {error, atom()}.
check_channel(ChannelId, PayloadTx, PoI, Height, Trees) ->
    case ChannelId =:= aesc_payload:channel_id(PayloadTx) of
        true ->
            ChannelsTree = aec_trees:channels(Trees),
            case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
                none ->
                    {error, channel_does_not_exist};
                {value, Channel} ->
                    Checks =
                        [fun() -> check_solo_closing(Channel, Height) end,
                         fun() -> check_peers_and_amounts(PoI, Channel) end,
                         fun() -> check_round(PayloadTx, Channel) end],
                    aeu_validation:run(Checks)
            end;
        false ->
            {error, bad_state_channel_id}
    end.

check_peers_and_amounts(PoI, Channel) ->
    InitiatorPubKey   = aesc_channels:initiator(Channel),
    ResponderPubKey   = aesc_channels:responder(Channel),
    ChannelAmount     = aesc_channels:total_amount(Channel),
    case aesc_utils:accounts_in_poi([InitiatorPubKey, ResponderPubKey], PoI) of
        {error, _} = Err -> Err;
        {ok, [PoIInitiatorAcc, PoIResponderAcc]} ->
            PoIInitiatorAmt = aec_accounts:balance(PoIInitiatorAcc),
            PoIResponderAmt = aec_accounts:balance(PoIResponderAcc),
            PoIAmount      = PoIInitiatorAmt + PoIResponderAmt,
            case ChannelAmount =:= PoIAmount of
                true  -> ok;
                false -> {error, wrong_state_amount}
            end
    end.

check_solo_closing(Channel, Height) ->
    case aesc_channels:is_solo_closing(Channel, Height) of
        true  -> ok;
        false -> {error, channel_not_closing}
    end.

check_round(PayloadTx, Channel) ->
    case aesc_channels:round(Channel) < aesc_payload:round(PayloadTx) of
        true  -> ok;
        false -> {error, state_round_too_small}
    end.

check_root_hash(PayloadTx, PoI) ->
    ChannelStateHash = aesc_payload:state_hash(PayloadTx),
    PoIHash = aec_trees:poi_hash(PoI),
    case ChannelStateHash =:= PoIHash of
        true -> ok;
        false -> {error, invalid_poi_hash}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_SLASH_TX_VSN.
