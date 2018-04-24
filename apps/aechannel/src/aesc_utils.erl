%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    State Channel utility functions
%%% @end
%%%=============================================================================
-module(aesc_utils).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([check_active_channel_exists/3,
         check_is_active/1,
         check_is_peer/2,
         check_are_peers/2,
         check_are_funds_in_channel/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_active_channel_exists(aesc_channels:id(),
                                  aesc_offchain_tx:tx(),
                                  aec_trees:trees()) ->
                                         {error, term()} | ok.
check_active_channel_exists(ChannelId, StateTx, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Ch} ->
            case aesc_channels:is_active(Ch) of
                true ->
                    ChInitiatorPubKey   = aesc_channels:initiator(Ch),
                    ChParticipantPubKey = aesc_channels:participant(Ch),
                    ChTotalAmount       = aesc_channels:total_amount(Ch),
                    SInitiatorPubKey    = aesc_offchain_tx:initiator(StateTx),
                    SParticipantPubKey  = aesc_offchain_tx:participant(StateTx),
                    SInitiatorAmount    = aesc_offchain_tx:initiator_amount(StateTx),
                    SParticipantAmount  = aesc_offchain_tx:participant_amount(StateTx),
                    STotalAmount        = SInitiatorAmount + SParticipantAmount,
                    case {ChInitiatorPubKey   =:= SInitiatorPubKey,
                          ChParticipantPubKey =:= SParticipantPubKey,
                          ChTotalAmount =:= STotalAmount} of
                        {true, true, true} -> ok;
                        {true, true, _   } -> {error, payload_amounts_change_channel_funds};
                        {_   , _   , _   } -> {error, wrong_channel_peers}
                    end;
                false ->
                    {error, channel_not_active}
            end
    end.

-spec check_is_active(aesc_channels:channel()) -> ok | {error, channel_not_active}.
check_is_active(Channel) ->
    case aesc_channels:is_active(Channel) of
        true  -> ok;
        false -> {error, channel_not_active}
    end.

-spec check_is_peer(pubkey(), list(pubkey())) -> ok | {error, account_not_peer}.
check_is_peer(PubKey, Peers) ->
    case lists:member(PubKey, Peers) of
        true  -> ok;
        false -> {error, account_not_peer}
    end.

-spec check_are_peers(list(pubkey()), list(pubkey())) -> ok | {error, account_not_peer}.
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
