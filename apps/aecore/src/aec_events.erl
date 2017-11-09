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

-export_type([event/0]).

-type event() :: block_created
               | block_received
               | tx_created
               | tx_received.

-spec publish(event(), any()) -> ok.
publish(Event, Info) ->
    gproc_ps:publish(l, Event, Info),
    ok.

-spec subscribe(event()) -> true.
subscribe(Event) ->
    gproc_ps:subscribe(l, Event).

-spec unsubscribe(event()) -> true.
unsubscribe(Event) ->
    gproc_ps:unsubscribe(l, Event).
