-module(aemon_metrics).

-export([create/1]).

-export([ fork_micro/0
        , confirmation_delay/1
        ]).
-export([ gen_stats_tx/1
        , gen_stats_tx_monitoring/1
        , gen_stats_microblocks/1
        ]).
-export([ publisher_balance/1
        , publisher_post_tx/1
        ]).
-export([ queue_size/1
        , ttl_expired/1
        ]).

%% on chain forks & confirmation delay

fork_micro() ->
    update([fork, micro], 1).

confirmation_delay(Count) ->
    update([confirmation, delay], Count).

%% generation stats

gen_stats_tx(Count) ->
    update([gen_stats, tx, total], Count).

gen_stats_tx_monitoring(Count) ->
    update([gen_stats, tx, monitoring], Count).

gen_stats_microblocks(Count) ->
    update([gen_stats, microblocks, total], Count).


%% publisher

publisher_balance(Error) when is_atom(Error) ->
    update([publisher, balance, Error], 1);
publisher_balance(Balance) ->
    update([publisher, balance], Balance).

publisher_post_tx(Action) ->
    update([publisher, post_tx, Action], 1).

%% ttl

queue_size(Count) ->
    update([publisher, queue, size], Count).

ttl_expired(Count) ->
    update([publisher, queue, ttl_expired], Count).

%% on start metric creation

create(on_chain) ->
    create([forks, micro], counter),
    create([confirmation, delay], histogram),
    ok;
create(gen_stats) ->
    create([gen_stats, tx, total], histogram),
    create([gen_stats, tx, monitoring], histogram),
    create([gen_stats, microblocks, total], histogram),
    ok;
create(publisher) ->
    create([publisher, balance], gauge),
    create([publisher, post_tx, success], counter),
    create([publisher, post_tx, max_adjustment], counter),
    create([publisher, post_tx, nonce_too_low], counter),
    create([publisher, post_tx, nonce_too_high], counter),
    ok;
create(ttl) ->
    create([publisher, queue, size], histogram),
    create([publisher, queue, ttl_expired], histogram),
    ok.

%% internals

create(Name, Type) ->
    exometer:ensure(metric(Name), Type, []),
    ok.

update(Name, Value) ->
    aec_metrics:try_update(metric(Name), Value).

metric(Name) ->
    [ae, epoch, aemon | Name ].
