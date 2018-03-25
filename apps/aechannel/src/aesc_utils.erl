%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    State Channel utility functions
%%% @end
%%%=============================================================================
-module(aesc_utils).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([check_active_channel_exists/4,
         check_are_peers/2,
         check_peer_has_funds/4]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_active_channel_exists(aesc_channels:id(), pubkey(), pubkey(),
                                  aec_trees:trees()) -> {error, term()} | ok.
check_active_channel_exists(ChannelId, InitiatorPubKey, ParticipantPubKey, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Ch} ->
            case aesc_channels:is_active(Ch) of
                true ->
                    ChInitiatorPubKey   = aesc_channels:initiator(Ch),
                    ChParticipantPubKey = aesc_channels:participant(Ch),
                    case {ChInitiatorPubKey   =:= InitiatorPubKey,
                          ChParticipantPubKey =:= ParticipantPubKey} of
                        {true, true} -> ok;
                        {_   , _}    -> {error, wrong_channel_peers}
                    end;
                false ->
                    {error, channel_not_active}
            end
    end.

-spec check_are_peers(list(pubkey()), list(pubkey())) -> ok | {error, accounts_not_peers}.
check_are_peers([], _Peers) ->
    ok;
check_are_peers([PubKey | Rest], Peers) ->
    case lists:member(PubKey, Peers) of
        true  -> check_are_peers(Rest, Peers);
        false -> {error, accounts_not_peers}
    end.

-spec check_peer_has_funds(pubkey(), aesc_channels:id(), non_neg_integer(), aec_trees:trees()) ->
                                  ok | {error, insufficient_channel_peer_amount}.
check_peer_has_funds(PeerPubKey, ChannelId, Amount, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    Channel      = aesc_state_tree:get(ChannelId, ChannelsTree),
    case PeerPubKey =:= aesc_channels:initiator(Channel) of
        true  -> check_sufficient_funds(aesc_channels:initiator_amount(Channel), Amount);
        false -> check_sufficient_funds(aesc_channels:participant_amount(Channel), Amount)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_sufficient_funds(Funds, Amount) ->
    case Funds >= Amount of
        true  -> ok;
        false -> {error, insufficient_channel_peer_amount}
    end.
