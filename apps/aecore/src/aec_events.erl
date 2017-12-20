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

-include("common.hrl").
-include("blocks.hrl").

-type event() :: block_created
               | start_mining
               | top_changed
               | tx_created
               | tx_received
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

