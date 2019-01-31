-module(aestratum_pubsub).

%% TODO: eunit
%% TODO: type spec

-export([publish/2,
         subscribe/1,
         unsubscribe/1
        ]).

publish(new_block = Event, Info) ->
    Data =
        #{time => os:timestamp(),
          info => Info},
    gproc_ps:publish(l, Event, Data).

subscribe(Event) ->
    true = gproc_ps:subscribe(l, Event),
    ok.

unsubscribe(Event) ->
    true = gproc_ps:unsubscribe(l, Event),
    ok.

