%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Event publish/subscribe support
%%% @end
%%%=============================================================================
-module(aec_events).

-export([publish/2,
         subscribe/1,
         ensure_subscription/1,
         unsubscribe/1]).

-import(aeu_debug, [pp/1]).

-export_type([event/0]).

-include("blocks.hrl").

-type event() :: start_mining
               | block_created
               | micro_block_created
               | start_micro_mining
               | start_micro_sleep
               | top_changed
               | block_to_publish
               | tx_created
               | tx_received
               | candidate_block
               | stratum_new_candidate
               | peers
               | metric
               | chain_sync
               | parent_top_changed
               | oracle_query_tx_created
               | oracle_response_tx_created
               | {tx_event, any()}.

-spec publish(event(), any()) -> ok.
publish(Event, Info) ->
    Data = #{sender => self(),
             time => os:timestamp(),
             info => Info},
    gproc_ps:publish(l, Event, Data).

-spec subscribe(event()) -> true.
subscribe(Event) ->
    gproc_ps:subscribe(l, Event).

-spec ensure_subscription(event()) -> true.
%% @doc Subscribes to Event. Will not crash if called more than once.
ensure_subscription(Event) ->
    try subscribe(Event)
    catch
        error:badarg ->
            %% This assertion should not be needed, since there is
            %% no other scenario that would cause subscribe/1 to fail,
            %% other than gproc not running (would also cause a badarg)
            _ = gproc:get_value({p,l,{gproc_ps_event, Event}}, self()),
            true
    end.

-spec unsubscribe(event()) -> true.
unsubscribe(Event) ->
    gproc_ps:unsubscribe(l, Event).

