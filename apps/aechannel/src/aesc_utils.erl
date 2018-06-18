%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    State Channel utility functions
%%% @end
%%%=============================================================================
-module(aesc_utils).

%% API
-export([check_active_channel_exists/4,
         get_channel/2,
         get_active_channel/2,
         accounts_in_poi/2,
         check_is_active/1,
         check_is_peer/2,
         check_are_peers/2,
         check_are_funds_in_channel/3,
         check_round_greater_than_last/2,
         check_round_at_last_last/2,
         check_state_hash_size/1,
         check_peers_and_amounts_in_poi/2,
         deserialize_payload/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_channel(aesc_channels:id(), aec_trees:trees()) ->
                         {error, term()} | ok.
get_channel(ChannelId, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Ch} ->
            {ok, Ch}
    end.

-spec get_active_channel(aesc_channels:id(), aec_trees:trees()) ->
                         {error, term()} | ok.
get_active_channel(ChannelId, Trees) ->
    case get_channel(ChannelId, Trees) of
        {error, _} = Err -> Err;
        {ok, Ch} ->
            case aesc_channels:is_active(Ch) of
                true ->
                    {ok, Ch};
                false ->
                    {error, channel_not_active}
            end
    end.

-spec check_active_channel_exists(aesc_channels:id(),
                                  aesc_offchain_tx:tx(),
                                  aec_trees:poi(),
                                  aec_trees:trees()) ->
                                         {error, term()} | ok.
check_active_channel_exists(ChannelId, PayloadTx, PoI, Trees) ->
    case get_active_channel(ChannelId, Trees) of
        {error, _} = Err -> Err;
        {ok, Ch} ->
            InitiatorPubKey = aesc_channels:initiator(Ch),
            ResponderPubKey = aesc_channels:responder(Ch),
            ChTotalAmount     = aesc_channels:total_amount(Ch),
            case accounts_in_poi([InitiatorPubKey, ResponderPubKey], PoI) of
                {error, _} = Err -> Err;
                {ok, [PoIInitiatorAcc, PoIResponderAcc]} ->
                    PoIInitiatorAmt = aec_accounts:balance(PoIInitiatorAcc),
                    PoIResponderAmt = aec_accounts:balance(PoIResponderAcc),
                    STotalAmount      = PoIInitiatorAmt + PoIResponderAmt,
                    ChannelRound      = aesc_channels:round(Ch),
                    StRound           = aesc_offchain_tx:round(PayloadTx),
                    StChanId          = aesc_offchain_tx:channel_id(PayloadTx),
                    case {ChTotalAmount =:= STotalAmount,
                          ChannelRound  =<  StRound,
                          ChannelId     =:= StChanId} of
                        {true , true , true} -> ok;
                        {_    , _    , false} -> {error, different_channel};
                        {false, _    , _    } -> {error, poi_amounts_change_channel_funds};
                        {_    , false, _    } -> {error, old_round}
                    end
          end
    end.

accounts_in_poi(Peers, PoI) ->
    Lookups = [aec_trees:lookup_poi(accounts, Pubkey, PoI) || Pubkey <- Peers],
    Accounts = [Acc || {ok, Acc} <- Lookups], % filter successful ones
    case length(Accounts) =:= length(Peers) of
        false -> {error, wrong_channel_peers};
        true ->
            {ok, Accounts}
    end.

-spec check_is_active(aesc_channels:channel()) -> ok | {error, channel_not_active}.
check_is_active(Channel) ->
    case aesc_channels:is_active(Channel) of
        true  -> ok;
        false -> {error, channel_not_active}
    end.

-spec check_round_greater_than_last(aesc_channels:channel(), non_neg_integer())
    -> ok | {error, old_round}.
check_round_greater_than_last(Channel, Round) ->
    case aesc_channels:round(Channel) < Round of
        true  -> ok;
        false -> {error, old_round}
    end.

-spec check_round_at_last_last(aesc_channels:channel(), non_neg_integer())
    -> ok | {error, old_round}.
check_round_at_last_last(Channel, Round) ->
    case aesc_channels:round(Channel) =< Round of
        true  -> ok;
        false -> {error, old_round}
    end.

-spec check_is_peer(aec_keys:pubkey(), list(aec_keys:pubkey())) -> ok | {error, account_not_peer}.
check_is_peer(PubKey, Peers) ->
    case lists:member(PubKey, Peers) of
        true  -> ok;
        false -> {error, account_not_peer}
    end.

-spec check_are_peers(list(aec_keys:pubkey()), list(aec_keys:pubkey())) -> ok | {error, account_not_peer}.
check_are_peers([], _Peers) ->
    ok;
check_are_peers([PubKey | Rest], Peers) ->
    case check_is_peer(PubKey, Peers) of
        ok    -> check_are_peers(Rest, Peers);
        Error -> Error
    end.


-spec check_are_funds_in_channel(aesc_channels:id(), non_neg_integer(), aec_trees:trees()) ->
                                        ok | {error, insufficient_channel_funds}.
check_are_funds_in_channel(ChannelId, Amount, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    Channel      = aesc_state_tree:get(ChannelId, ChannelsTree),
    case aesc_channels:total_amount(Channel) >= Amount of
        true  -> ok;
        false -> {error, insufficient_channel_funds}
    end.

-spec check_state_hash_size(binary()) -> boolean().
check_state_hash_size(Hash) ->
    byte_size(Hash) =:= 32.

-spec deserialize_payload(binary()) -> {ok, aetx_sign:signed_tx(), aesc_offchain_tx:tx()}
                                         | {ok, last_onchain}
                                         | {error, bad_offchain_state_type}.
deserialize_payload(<<>>) ->
    {ok, last_onchain};
deserialize_payload(Payload) ->
    try
        SignedTx = aetx_sign:deserialize_from_binary(Payload),
        Tx       = aetx_sign:tx(SignedTx),
        case aetx:specialize_type(Tx) of
            {channel_offchain_tx, PayloadTx} ->
                {ok, SignedTx, PayloadTx};
            _ ->
                {error, bad_offchain_state_type}
        end
    catch _:_ ->
            {error, payload_deserialization_failed}
    end.

-spec check_peers_and_amounts_in_poi(aesc_channels:channel(), aec_trees:poi())
    -> ok | {error, atom()}.
check_peers_and_amounts_in_poi(Channel, PoI) ->
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

