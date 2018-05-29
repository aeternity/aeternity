%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    State Channel utility functions
%%% @end
%%%=============================================================================
-module(aesc_utils).

%% API
-export([check_active_channel_exists/4,
         accounts_in_poi/2,
         check_is_active/1,
         check_is_peer/2,
         check_are_peers/2,
         check_are_funds_in_channel/3,
         check_round_greater_than_last/2,
         check_round_at_last_last/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_active_channel_exists(aesc_channels:id(),
                                  aesc_payload:tx(),
                                  aec_trees:poi(),
                                  aec_trees:trees()) ->
                                         {error, term()} | ok.
check_active_channel_exists(ChannelId, PayloadTx, PoI, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Ch} ->
            case aesc_channels:is_active(Ch) of
                true ->
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
                            StRound           = aesc_payload:round(PayloadTx),
                            case {ChTotalAmount     =:= STotalAmount,
                                  ChannelRound      =<  StRound} of
                                {true , true} -> ok;
                                {false, _   } -> {error, poi_amounts_change_channel_funds};
                                {_    , _   } -> {error, old_round}
                            end
                    end;
                false ->
                    {error, channel_not_active}
            end
    end.

accounts_in_poi(Peers, PoI) ->
    Lookups = [aec_trees:lookup_poi(accounts, Pubkey, PoI) || Pubkey <- Peers],
    Accounts = [Acc || {ok, Acc} <- Lookups],
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
