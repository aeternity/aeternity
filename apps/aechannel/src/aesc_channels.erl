%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for channel objects
%%% @end
%%%=============================================================================
-module(aesc_channels).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([]).

%%%===================================================================
%%% Types
%%%===================================================================

-type amount() :: integer().

-record(channel, {initiator          :: pubkey(),
                  participant        :: pubkey(),
                  initiator_amount   :: amount(),
                  participant_amount :: amount()}).

-opaque channel() :: #channel{}.

-type id() :: binary().
-type serialized() :: binary().

-export_type([id/0,
              channel/0,
              serialized/0]).

-define(CHANNEL_TYPE, <<"channel">>).
-define(CHANNEL_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

