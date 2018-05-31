%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Event publish/subscribe support
%%% @end
%%%=============================================================================
-module(aec_events).

-export([publish/2,
         subscribe/1,
         unsubscribe/1]).

-import(aeu_debug, [pp/1]).

-export_type([event/0]).

-include("blocks.hrl").

-type event() :: start_mining
               | block_created %% We created (mined) a new block. It usually becomes the new top block, though not necessarily.
               | top_synced %% We received a block, pulled from a network peer, that became the new top.
               | top_changed %% We received a block, pushed by a network peer, that became the new top.
               | tx_created
               | tx_received
               | candidate_block
               | peers
               | metric
               | chain_sync
               | mempool_sync
               | oracle_query_tx_created
               | oracle_response_tx_created.

-spec publish(event(), any()) -> ok.
publish(Event, Info) ->
    Data = #{sender => self(),
             time => os:timestamp(),
             info => Info},
    gproc_ps:publish(l, Event, Data).

-spec subscribe(event()) -> true.
subscribe(Event) ->
    gproc_ps:subscribe(l, Event).

-spec unsubscribe(event()) -> true.
unsubscribe(Event) ->
    gproc_ps:unsubscribe(l, Event).

