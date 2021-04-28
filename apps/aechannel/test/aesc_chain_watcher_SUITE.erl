%%%=============================================================================
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%%    Test utils for State Channel Chain Watcher
%%% @end
%%%=============================================================================

-module(aesc_chain_watcher_SUITE).

-export([
          all/0
        , groups/0
        , suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([
          reg_unreg/1
        , set_up_channel/1
        , deposit/1
        , flush_cache/1
        , innocent_fork/1
        , fork_touches/1
        , fork_evicts/1
        , cleanup/1
        , dummy_request_mine_blocks/1
        ]).

%% Watcher callbacks
-export([
          minimum_depth_achieved/4
        , channel_changed_on_chain/2
        , channel_closing_on_chain/2
        , channel_closed_on_chain/2
        , channel_unlocked/2
        ]).

%% Test helpers (called using `apply/3`)
-export([
          reg_watch/1
        , reg_close/2
        , close_watch/2
        , min_depth_watch/4
        ]).


-include_lib("common_test/include/ct.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(LOG(_Fmt, _Args), log(_Fmt, _Args, ?LINE, true)).
-define(LOG(_D, _Fmt, _Args), log(_Fmt, _Args, ?LINE, _D)).

-define(TIMEOUT, 3000).
-define(TYPE, {type, ?LINE}).   % unique type info for min-depth callback
-define(FORK, {fork, ?LINE}).   % fork id

-define(BIG_AMOUNT, 10000000000000000000000000000 * aec_test_utils:min_gas_price()).

all() ->
    [{group, all_tests}].

groups() ->
    [
     %% A separate watcher started for each sub-group
      {all_tests, [sequence], [ {group, admin}
                              , {group, channel_lifecycle}
                              , {group, multiple_channels}
                              , {group, cache_flushing}
                              , {group, innocent_fork}
                              , {group, fork_touches_but_no_change}
                              , {group, fork_evicts_tx}
                              , {group, pause_between_requests}]}
    , {admin, [sequence], [ reg_unreg ]}
    , {channel_lifecycle, [sequence], [ set_up_channel
                                      , deposit
                                      , cleanup ]}
    , {multiple_channels, [sequence], [ set_up_channel
                                      , set_up_channel
                                      , set_up_channel
                                      , deposit
                                      , cleanup ]}
    , {cache_flushing, [sequence], [ set_up_channel
                                   , deposit
                                   , flush_cache
                                   , deposit
                                   , cleanup ]}
    , {innocent_fork, [sequence], [ set_up_channel
                                  , deposit
                                  , innocent_fork
                                  , cleanup ]}
    , {fork_touches_but_no_change, [sequence], [ set_up_channel
                                               , deposit
                                               , fork_touches
                                               , cleanup ]}
    , {fork_evicts_tx, [sequence], [ set_up_channel
                                   , deposit
                                   , fork_evicts
                                   , cleanup ]}
    , {pause_between_requests, [sequence], [ dummy_request_mine_blocks
                                           , set_up_channel
                                           , deposit
                                           , cleanup
        ]}
    ].

suite() ->
    [].

%% ======================================================================
%% Test cases

reg_unreg(_Config) ->
    Client = spawn_link(fun() ->
                                ok = reg_watch(chid(1)),
                                receive die -> ok end
                        end),
    timer:sleep(50),
    kill_client(Client),
    ok.

cleanup(_Config) ->
    ChSetup = get_channel_setup(),
    AllClients = get_all_clients(ChSetup),
    ChIds = maps:keys(ChSetup),
    ?LOG("AllClients = ~p", [AllClients]),
    [kill_client(C) || C <- AllClients],
    lists:foreach(
      fun(ChId) ->
              case value_exists(ChId) of
                  false ->
                      ok;
                  true ->
                      ?LOG("ChId ~p still exists: ~p",
                           [ChId, [{T, ets:tab2list(T)} ||
                                      T <- aesc_chain_watcher:table_names()]]),
                      error({still_exists, ChId})
              end
      end, ChIds).

kill_client(Client) ->
    true = value_exists(Client),
    unlink(Client),
    MRef = erlang:monitor(process, Client),
    exit(Client, kill),
    receive
        {'DOWN', MRef, _, _, _} ->
            _ = sys:get_status(aesc_chain_watcher),
            _ = sys:get_status(aesc_chain_watcher),
            case value_exists(Client) of
                false ->
                    ok;
                true ->
                    ?LOG("Pid ~p still exists: ~p",
                         [Client, [{T, ets:tab2list(T)} ||
                                      T <- aesc_chain_watcher:table_names()]]),
                    error({still_exists, Client})
            end
    end.

set_up_channel(Config) ->
    SignedTx = create_tx(Config),
    TxHash = aetx_sign:hash(SignedTx),
    {ok, ChId} = aesc_utils:channel_pubkey(SignedTx),
    none = aec_chain:find_tx_with_location(TxHash),
    ok = push(SignedTx),
    {mempool, SignedTx} = aec_chain:find_tx_with_location(TxHash),
    ClientIxs = [1,2,3],
    [C1,C2,C3] = Cs = [spawn_client(N, ChId) || N <- ClientIxs],
    Watchers = [C1, C2],
    [ok,ok] = [set_reg_watch(C, ChId) || C <- Watchers],
    ok = set_reg_close(C3, ChId, 3),
    %%
    add_microblock(),
    ok = watchers_notified(
           fun({channel_changed_on_chain, I}, _C) ->
                   #{tx := CreateTx} = I,
                   ok
           end, Watchers),
    %%
    %% Set a min-depth watch. Verify that the event comes at the right height,
    %% and only to those that requested it
    %%
    ok = set_min_depth_watch(C1, ChId, TxHash, 3, MType1 = {type,?LINE}),
    verify_min_depth([{C1, MType1}], ChId, TxHash, 3, Cs),
    add_channel_setup(ChId, #{ client_ixs     => ClientIxs
                             , clients        => Cs
                             , watchers       => Watchers
                             , close_watchers => [C3] }),
    ok.

deposit(Config) ->
    foreach_channel(
      fun(ChId, Setup) ->
              deposit_(ChId, Setup, Config)
      end).

deposit_(ChId, Setup, Config) ->
    Cs = maps:get(clients, Setup),
    Watchers = maps:get(watchers, Setup),
    DepositTx = deposit_tx(ChId, Config),
    TxHash = aetx_sign:hash(DepositTx),
    push(DepositTx),
    C1 = hd(Cs),
    ok = set_min_depth_watch(C1, ChId, TxHash, MinDepth = 3, MType1 = ?TYPE),
    assert_no_events(Cs),
    add_microblock(),
    ok = watchers_notified(
           fun({channel_changed_on_chain, I}, _C) ->
                   #{tx := _SignedTx} = I,
                   ok
           end, Watchers),
    verify_min_depth([{C1,MType1}], ChId, TxHash, MinDepth, Cs -- [C1]),
    ok.

flush_cache(_Config) ->
    I = aesc_chain_watcher:get_cache_reinit_interval(),
    [ add_keyblock() || _ <- lists:seq(1, I+1) ],
    AllClients = get_all_clients(),
    assert_no_events(AllClients),
    ok.

innocent_fork(_Config) ->
    ChSetup = get_channel_setup(),
    #{clients := Cs} = maps:get(hd(maps:keys(ChSetup)), ChSetup),
    {ok, ForkBlock} = add_keyblock(),
    {ok, ForkHash} = aec_blocks:hash_internal_representation(ForkBlock),
    {ok, _} = fork_from_hash(ForkId = ?FORK, ForkHash),
    add_microblock(ForkId),
    add_microblock(ForkId),
    assert_no_events(fun() -> fork_switch(ForkId) end, Cs),
    ok.

fork_touches(Config) ->
    ChSetup = get_channel_setup(),
    ChId = hd(maps:keys(ChSetup)),
    #{ clients    := Cs
     , watchers   := Watchers } = maps:get(ChId, ChSetup),
    DepositTx = deposit_tx(ChId, Config),
    TxHash = aetx_sign:hash(DepositTx),
    {ok, ForkBlock} = add_keyblock(),
    {ok, ForkPoint} = aec_blocks:hash_internal_representation(ForkBlock),
    push(DepositTx),
    C1 = hd(Cs),
    ok = set_min_depth_watch(C1, ChId, TxHash, _MinDepth = 3, _MType = ?TYPE),
    assert_no_events(Cs),
    {ok, MicroBlock} = add_microblock(),
    {ok, MicroHash} = aec_blocks:hash_internal_representation(MicroBlock),
    ok = watchers_notified(
           fun({channel_changed_on_chain, I}, _C) ->
                   #{tx := _SignedTx} = I,
                   ok
           end, Watchers),
    {ok, _} = fork_from_hash(ForkId = ?FORK, ForkPoint),
    add_microblock(ForkId),
    clone_microblock_on_fork(MicroHash, ForkId),
    assert_no_events(fun() -> fork_switch(ForkId) end, Cs),
    ?LOG("No events detected after fork switch", []),
    ok.

fork_evicts(Config) ->
    ChSetup = get_channel_setup(),
    ChId = hd(maps:keys(ChSetup)),
    #{ clients    := Cs
     , watchers   := Watchers } = maps:get(ChId, ChSetup),
    StateHash1 = channel_state_hash(ChId), % fallback state hash
    DepositTx = deposit_tx(ChId, Config),
    TxHash = aetx_sign:hash(DepositTx),
    {ok, ForkBlock} = add_keyblock(),
    {ok, ForkPoint} = aec_blocks:hash_internal_representation(ForkBlock),
    push(DepositTx),
    C1 = hd(Cs),
    ok = set_min_depth_watch(C1, ChId, TxHash, _MinDepth = 3, _MType = ?TYPE),
    assert_no_events(Cs),
    {ok, _} = add_microblock(),
    ok = watchers_notified(
           fun({channel_changed_on_chain, I}, _C) ->
                   #{tx := _SignedTx} = I,
                   ok
           end, Watchers),
    StateHash2 = channel_state_hash(ChId), % will be evicted
    {true, _, _} = {StateHash1 =/= StateHash2, StateHash1, StateHash2},
    {ok, _} = fork_from_hash(ForkId = ?FORK, ForkPoint),
    add_microblock(ForkId),
    add_microblock(ForkId),
    fork_switch(ForkId),
    ok = watchers_notified(
           fun({channel_changed_on_chain, I}, _C) ->
                   #{channel := Chx} = I,
                   NewStateHash = aesc_channels:state_hash(Chx),
                   %% We should now see the fallback state hash
                   {StateHash1, NewStateHash} = {NewStateHash, StateHash1},
                   ok
           end, Watchers),
    %% assert_no_events(fun() -> fork_switch(ForkId) end, Cs),
    ok.

foreach_channel(F) ->
    ChSetup = get_channel_setup(),
    maps:fold(
      fun(ChId, Setup, ok) ->
              F(ChId, Setup)
      end, ok, ChSetup).

verify_min_depth(MDWs, ChId, TxHash, MinDepth, Cs) ->
    MDWPids = [ client_pid(W) || W <- MDWs],
    OtherCs = Cs -- MDWPids,
    [ assert_no_events(fun add_keyblock/0, Cs)
      || _ <- lists:seq(1, MinDepth-1) ],
    add_keyblock(),
    ok = watchers_notified(
           fun({minimum_depth_achieved, Info}, {_, T}) ->
                   {{ChId, T, TxHash}, Info} = {Info, Info},
                   ok
           end, MDWs),
    assert_no_events(OtherCs),
    ok.

dummy_request_mine_blocks(_Cfg) ->
    Client = spawn_link(fun() ->
                                ok = reg_watch(chid(1)),
                                receive die -> ok end
                        end),
    add_keyblock(),
    kill_client(Client),
    add_keyblock(),
    ok.

%% ======================================================================
%% Watcher callbacks

channel_changed_on_chain(Client, Info) ->
    event(Client, {channel_changed_on_chain, Info}).

channel_closed_on_chain(Client, Info) ->
    event(Client, {channel_closed_on_chain, Info}).

channel_closing_on_chain(Client, Info) ->
    event(Client, {channel_closing_on_chain, Info}).

channel_unlocked(Client, Info) ->
    event(Client, {channel_unlocked, Info}).

minimum_depth_achieved(Client, ChId, Req, Info) ->
    event(Client, {minimum_depth_achieved, {ChId, Req, Info}}).

event(Client, E) ->
    ?LOG("From watcher (Client: ~p): ~p", [Client, E]),
    Client ! {from_watcher, E},
    ok.

await_event(Timeout) ->
    receive
        {from_watcher, E} ->
            {ok, E}
    after Timeout ->
            timeout
    end.

%% ======================================================================
%% Helper functions

assert_no_events(F, Cs) when is_function(F, 0) ->
    F(),
    assert_no_events(Cs).

assert_no_events(Cs) ->
    Timeouts = [client_req(C, {await_event, 50}) || C <- Cs],
    case all_are_timeouts(Timeouts) of
        true ->
            true;
        false ->
            error({expected_all_timeouts, Timeouts})
    end.

all_are_timeouts(L) ->
    lists:all(fun(X) -> X == timeout end, L).

watchers_notified(Check, Cs) ->
    lists:foreach(
      fun(C) ->
              CPid = client_pid(C),
              case client_req(CPid, {await_event, ?TIMEOUT}) of
                  {ok, Evt} ->
                      Error = fun(E) ->
                                      error({notification_error, [ {client, C}
                                                                 , {event, Evt}
                                                                 , {error, E} ]})
                              end,
                      try Check(Evt, C) of
                          ok -> ok;
                          {error,_} = E ->
                              Error(E)
                      catch
                          error:E ->
                              Error(E)
                      end;
                  timeout ->
                      error({notification_error, [ {client, C}
                                                 , {error, timeout} ]})
              end
      end, Cs),
    ?LOG("Watchers notified: ~p", [Cs]),
    ok.

client_pid(C) when is_pid(C) ->
    C;
client_pid({C,_}) when is_pid(C) ->
    C.

get_all_clients() ->
    get_all_clients(get_channel_setup()).

get_all_clients(ChSetup) when is_map(ChSetup) ->
    maps:fold(
      fun(_, Setup, Acc) ->
              Acc ++ maps:get(clients, Setup, [])
      end, [], ChSetup).

get_channel_setup() ->
    aec_chain_sim:dict_get(channel_setup, #{}).

add_channel_setup(ChId, Setup) ->
    S = get_channel_setup(),
    aec_chain_sim:dict_set(channel_setup, S#{ChId => Setup}).

set_reg_watch(C, ChId) ->
    client_apply(C, ?MODULE, reg_watch, [ChId]).

set_reg_close(C, ChId, MinDepth) ->
    client_apply(C, ?MODULE, reg_close, [ChId, MinDepth]).

set_min_depth_watch(C, ChId, TxHash, MinDepth, Type) ->
    client_apply(C, ?MODULE, min_depth_watch,
                 [ChId, TxHash, MinDepth, Type]).

reg_watch(ChId) ->
    aesc_chain_watcher:register(
      ChId, ?MODULE, [aesc_chain_watcher:watch_req()]).

reg_close(ChId, MinDepth) ->
    aesc_chain_watcher:register(
      ChId, ?MODULE, [aesc_chain_watcher:close_req(MinDepth)]).

close_watch(ChId, MinDepth) ->
    aesc_chain_watcher:request(
      ChId, aesc_chain_watcher:close_req(MinDepth)).

min_depth_watch(ChId, TxHash, Depth, ReqType) ->
    aesc_chain_watcher:request(
      ChId, aesc_chain_watcher:min_depth_req(TxHash, Depth, ReqType)).

value_exists(X) ->
    Tabs = aesc_chain_watcher:table_names(),
    try lists:foldl(
          fun(Tab, Acc) ->
                  ets:foldl(
                    fun(Obj, Acc1) ->
                            case x_in_obj(Obj, X) of
                                true ->
                                    throw(true);
                                false ->
                                    Acc1
                            end
                    end, Acc, Tab)
          end, false, Tabs)
    catch
        throw:true ->
            true
    end.

x_in_obj(X, X) ->
    true;
x_in_obj([_|_] = L, X) ->
    lists:any(fun(Obj) -> x_in_obj(Obj, X) end, L);
x_in_obj(Tuple, X) when is_tuple(Tuple) ->
    x_in_obj(tuple_to_list(Tuple), X);
x_in_obj(Map, X) when is_map(Map) ->
    x_in_obj(maps:to_list(Map), X);
x_in_obj(_, _) ->
    false.

chid(1) ->
    <<"chid-1..........................">>;
chid(2) ->
    <<"chid-2..........................">>;
chid(3) ->
    <<"chid-3..........................">>;
chid(4) ->
    <<"chid-4..........................">>.

spawn_client(N, ChId) ->
    spawn(fun() ->
                  gproc:reg(client_regname(N, ChId)),
                  client_loop()
          end).

client_regname(N, ChId) ->
    {n, l, {?MODULE, client, N, ChId}}.

client_apply(Pid, M, F, A) when is_pid(Pid) ->
    client_req(Pid, {apply, M, F, A}).

client_req(Pid, Req) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {self(), Req},
    receive
        {Pid, Reply} ->
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            ?LOG("Client ~p died: ~p", [Pid, Reason]),
            error({client_died, [Pid, Reason]})
    after 5000 ->
            error(timeout)
    end.

client_loop() ->
    receive
        {From, {await_event, Timeout}} when is_pid(From) ->
            ?LOG("await_event (~p)", [Timeout]),
            Result = await_event(Timeout),
            From ! {self(), Result},
            client_loop();
        {From, Req} when is_pid(From) ->
            Reply = handle_client_req(Req),
            From ! {self(), Reply},
            client_loop()
    end.

handle_client_req(R) ->
    try handle_client_req_(R)
    catch error:Err ->
            ?LOG("CAUGHT error:~p / ~p", [Err, erlang:get_stacktrace()]),
            error(Err)
    end.

handle_client_req_({apply, M, F, A}) ->
    apply(M, F, A).

log(Fmt, Args, L, #{debug := true}) ->
    log(Fmt, Args, L, true);
log(Fmt0, Args0, L, true) ->
    Fmt = "~p at ~p: " ++ Fmt0,
    Args = [self(), L | Args0],
    lager:debug(Fmt, Args),
    ct:log(Fmt, Args);
log(_, _, _, _) ->
    ok.

create_and_sign_tx(#{mod := Mod} = TxInfo, SKs) ->
    try
        {ok, Tx} = Mod:new(maps:merge(#{fee => 50000 * aec_test_utils:min_gas_price(), nonce => 1},
                                      maps:remove(mod, TxInfo))),
        ?LOG("Tx = ~p", [Tx]),
        aec_test_utils:sign_tx(Tx, SKs)
    catch
        error:Err ->
            ?LOG("CAUGHT error:~p / ~p", [Err, erlang:get_stacktrace()]),
            error(Err)
    end.

%% We fake a state hash, but want one that's unique for each
%% channel update, so that we can match on it to ensure that the
%% right state is reported.
state_hash(BlockHash, Bin) when is_binary(BlockHash), is_binary(Bin) ->
    aec_hash:hash(state_trees, <<BlockHash/binary, Bin/binary>>).

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.

%% Tx creation - most values don't matter for the tests, but must be present
%% in order to create the transaction objects
%%
create_tx(Config) ->
    I = ?config(initiator, Config),
    R = ?config(responder, Config),
    ISK = ?config(initiator_sk, Config),
    RSK = ?config(responder_sk, Config),
    TopHash = aec_chain:top_block_hash(),
    Nonce = next_nonce(I),
    Tx0 = #{ mod              => aesc_create_tx
           , initiator_id     => I
           , initiator_amount => 10
           , responder_id     => R
           , responder_amount => 10
           , channel_reserve  => 10
           , lock_period      => 3
           , fee              => 50000 * aec_test_utils:min_gas_price()
           , state_hash       => <<>>
           , nonce            => Nonce
           , delegate_ids     => no_delegates()},
    StateHash = state_hash(TopHash, term_to_binary(Tx0)),
    create_and_sign_tx(Tx0#{state_hash => StateHash}, [RSK, ISK]).

deposit_tx(ChId, Config) ->
    Round = channel_round(ChId) + 1,
    I = ?config(initiator, Config),
    ISK = ?config(initiator_sk, Config),
    RSK = ?config(responder_sk, Config),
    TopHash = aec_chain:top_block_hash(),
    Nonce = next_nonce(I),
    Tx0 = #{ mod            => aesc_deposit_tx
           , channel_id     => aeser_id:create(channel, ChId)
           , from_id        => I
           , amount         => 10
           , fee            => 50000 * aec_test_utils:min_gas_price()
           , state_hash     => <<>>
           , round          => Round
           , nonce          => Nonce },
    StateHash = state_hash(TopHash, term_to_binary(Tx0)),
    create_and_sign_tx(Tx0#{state_hash => StateHash}, [RSK, ISK]).

channel_state_hash(ChId) ->
    case aec_chain:get_channel(ChId) of
        undefined ->
            error({unknown_channel, ChId});
        {ok, Ch} ->
            aesc_channels:state_hash(Ch)
    end.

channel_round(ChId) ->
    case aec_chain:get_channel(ChId) of
        undefined ->
            error({unknown_channel, ChId});
        {ok, Ch} ->
            aesc_channels:round(Ch)
    end.

next_nonce(Acct) ->
    aec_chain_sim:next_nonce(Acct).

%% Chain simulator requests

push(Tx) ->
    aec_chain_sim:push(Tx).

add_keyblock() ->
    aec_chain_sim:add_keyblock().

add_microblock() ->
    aec_chain_sim:add_microblock().

add_microblock(ForkId) ->
    aec_chain_sim:add_microblock(ForkId).

clone_microblock_on_fork(Hash, ForkId) ->
    aec_chain_sim:clone_microblock_on_fork(Hash, ForkId).

fork_from_hash(ForkId, FromHash) when is_binary(FromHash) ->
    aec_chain_sim:fork_from_hash(ForkId, FromHash).

fork_switch(ForkId) ->
    aec_chain_sim:fork_switch(ForkId).

start_chain_process() ->
    aec_chain_sim:start().

stop_chain_process() ->
    aec_chain_sim:stop().

%% ======================================================================
%% Test environment setup

init_per_suite(Config0) ->
    DataDir = ?config(data_dir, Config0),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config = [ {top_dir, TopDir}
             , {test_module, ?MODULE}
             , {symlink_name, "latest.watch"} | Config0],
    aecore_suite_utils:make_shortcut(Config),
    {ok, StartedApps} = application:ensure_all_started(gproc),
    ok = mnesia:start(),
    {ok, Apps1} = application:ensure_all_started(lager),
    {ok, Apps2} = application:ensure_all_started(crypto),
    {ok, Apps3} = application:ensure_all_started(enacl),
    aec_chain_sim:setup_meck(),
    [ {started_apps, StartedApps ++ [mnesia] ++ Apps1 ++ Apps2 ++ Apps3}
    | Config ].

end_per_suite(Config) ->
    aec_chain_sim:remove_meck(),
    StartedApps = proplists:get_value(started_apps, Config),
    [ok = application:stop(A) || A <- lists:reverse(StartedApps)],
    ok.

init_per_group(GrpName, Config) ->
    ?LOG("init_per_group(~p, Config)", [GrpName]),
    setup_lager(GrpName, Config),
    case GrpName of
        all_tests ->
            Config;
        _ ->
            {ok, ChainP} = start_chain_process(),
            {ok, #{pubkey := Initiator, privkey := InitiatorSK}} = aec_chain_sim:new_account(?BIG_AMOUNT),
            {ok, #{pubkey := Responder, privkey := ResponderSK}} = aec_chain_sim:new_account(?BIG_AMOUNT),
            {ok, Watcher} = start_chain_watcher(),
            %% mine a few keyblocks so we are on the right protocol
            lists:foreach(
                fun(_) -> add_keyblock() end,
                lists:seq(1, 2)),
            [ {watcher, Watcher}
            , {chain_process, ChainP}
            , {initiator, aeser_id:create(account, Initiator)}
            , {responder, aeser_id:create(account, Responder)}
            , {initiator_sk, InitiatorSK}
            , {responder_sk, ResponderSK}
            | Config]
    end.

end_per_group(GrpName, Config) ->
    ?LOG("end_per_group(~p, Config)", [GrpName]),
    case GrpName of
        all_tests ->
            ok;
        _ ->
            ok = stop_chain_watcher(Config),
            ok = stop_chain_process(),
            ok
    end.

init_per_testcase(_Test, Config) ->
    ?LOG("~p: Config = ~p", [_Test, Config]),
    case ?config(saved_config, Config) of
        {FromTestCase, SavedConfig} ->
            ?LOG("Reusing config from ~p", [FromTestCase]),
            lists:foldl(
              fun({K, V}, Acc) ->
                      lists:keystore(K, 1, Acc, {K, V})
              end, lists:keydelete(saved_config, 1, Config),
              SavedConfig);
        undefined ->
            Config
    end.

end_per_testcase(_Test, _Config) ->
    ok.

setup_lager(Grp, Config) ->
    LogDir = aecore_suite_utils:shortcut_dir(Config),
    ?LOG("LogDir = ~p", [LogDir]),
    LogFile = filename:join(LogDir, atom_to_list(Grp) ++ ".log"),
    application:stop(lager),
    application:set_env(
      lager, handlers,
      [{lager_file_backend, [{file, LogFile}, {level, debug}]}]),
    application:start(lager).

start_chain_watcher() ->
    Me = self(),
    {Parent, MRef} =
        spawn_monitor(
          fun() ->
                  {ok, W} = aesc_chain_watcher:start_link(),
                  erlang:monitor(process, W),
                  Me ! {self(), ok},
                  receive
                      {'DOWN', _, process, W, Reason} ->
                          ?LOG("Watcher died Reason = ~p", [Reason]),
                          exit(Reason);
                      {From, shutdown} ->
                          ?LOG("got shutdown request", []),
                          unlink(W),
                          exit(W, shutdown),
                          receive
                              {'DOWN', _, process, W, shutdown} ->
                                  ?LOG("watcher terminated", []),
                                  From ! {self(), ok}
                          after 5000 ->
                                  ?LOG("shutdown timeout. Msgs = ~p",
                                       [element(2, process_info(self(), messages))])
                          end
                  end
          end),
    receive
        {Parent, ok} ->
            demonitor(MRef),
            {ok, Parent};
        {'DOWN', MRef, _, _, Reason} ->
            error(Reason)
    after 5000 ->
            error(timeout)
    end.

stop_chain_watcher(Config) ->
    {_, WatcherParent} = lists:keyfind(watcher, 1, Config),
    ?LOG("WatcherParent alive? ~p", [erlang:is_process_alive(WatcherParent)]),
    WatcherParent ! {self(), shutdown},
    receive
        {WatcherParent, ok} ->
            ok
    after 5000 ->
            ?LOG("stop timeout. Parent = ~p", [process_info(WatcherParent)]),
            error(timeout)
    end.

%%
%% ======================================================================
no_delegates() ->
    case aec_hard_forks:protocol_effective_at_height(100) < ?IRIS_PROTOCOL_VSN of
        true -> [];
        false -> {[], []}
    end.

