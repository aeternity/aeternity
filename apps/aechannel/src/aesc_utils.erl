%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    State Channel utility functions
%%% @end
%%%=============================================================================
-module(aesc_utils).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([check_active_channel_exists/2,
         check_accounts_are_peers/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_active_channel_exists(aesc_channels:id(), aec_trees:trees()) ->
                                         {error, channel_does_not_exist} | ok.
check_active_channel_exists(ChannelId, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    %% It assumes aesc_state_tree:lookup/2 does not return closed/expired channels
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none       -> {error, channel_does_not_exist};
        {value, _} -> ok
    end.

-spec check_accounts_are_peers(list(pubkey()), aesc_channels:id(), aec_trees:trees()) ->
                                      ok | {error, accounts_not_peers}.
check_accounts_are_peers(Accounts, ChannelId, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    Channel      = aesc_state_tree:get(ChannelId, ChannelsTree),
    Peers        = aesc_channels:peers(Channel),
    check_are_peers(Accounts, Peers).

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_are_peers([], _Peers) ->
    ok;
check_are_peers([PubKey | Rest], Peers) ->
    case lists:member(PubKey, Peers) of
        true  -> check_are_peers(Rest, Peers);
        false -> {error, accounts_not_peers}
    end.
