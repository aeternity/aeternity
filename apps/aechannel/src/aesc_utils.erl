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
         check_are_peers/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_active_channel_exists(aesc_channels:id(), pubkey(), pubkey(),
                                  aec_trees:trees()) -> {error, term()} | ok.
check_active_channel_exists(ChannelId, InitiatorPubKey, ParticipantPubKey, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    %% It assumes aesc_state_tree:lookup/2 does not return closed/expired channels
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Ch} ->
            ChInitiatorPubKey   = aesc_channels:initiator(Ch),
            ChParticipantPubKey = aesc_channels:participant(Ch),
            case {ChInitiatorPubKey   =:= InitiatorPubKey,
                  ChParticipantPubKey =:= ParticipantPubKey} of
                {true, true} -> ok;
                {_, _}       -> {error, wrong_channel_peers}
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
