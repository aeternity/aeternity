%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Memory pool synchronization
%%%
-module(aec_tx_pool_sync).

-behaviour(gen_server).

%% API for supervisor
-export([ start_link/0
        , stop/0
        ]).

%% Sync API
-export([ accept/2
        , connect/2
        , sync_finish/2
        , sync_get/2
        , sync_unfold/2
        ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(aec_peer, [ppp/1]).
-include("blocks.hrl").

-record(sync,
        { id           :: reference()
        , peer_id      :: binary()
        , peer_con     :: pid()
        , peer_con_ref :: undefined | reference()
        , tree         :: undefined | aeu_mp_trees:tree()
        , data         :: undefined | [aeu_mp_trees:key()]
        }).

-type sync_object() :: #sync{}.

-record(state,
        { local  = not_synced :: not_synced | {active, sync_object()}
                               | {in_sync, reference()}
        , remote = []         :: [sync_object()]
        }).

-include("aec_peer_messages.hrl").

-define(SERVER, ?MODULE).
-define(MAX_INCOMING_SYNC, 5).
-define(TX_PLACEHOLDER, []). %% RLP encodable.

-define(DEFAULT_SYNC_INTERVAL, 30 * 60 * 1000).

-type unfold_node()    :: aeu_mp_trees:unfold_node().
-type unfold_leaf()    :: aeu_mp_trees:unfold_leaf().
-type unfold_subtree() :: {subtree, aeu_mp_trees:path()}.
-type unfold_key()     :: {key, <<_:256>>}.
-type unfold()         :: unfold_node()
                        | unfold_leaf()
                        | unfold_subtree()
                        | unfold_key().

%% -- API --------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

accept(PeerId, PeerCon) ->
    gen_server:call(?SERVER, {accept, PeerId, PeerCon}).

connect(PeerId, PeerCon) ->
    gen_server:cast(?SERVER, {connect, PeerId, PeerCon}).

sync_finish(PeerId, Status) ->
    gen_server:cast(?SERVER, {finish, PeerId, Status}).

sync_get(PeerId, TxHashes) ->
    gen_server:call(?SERVER, {get, PeerId, TxHashes}).

sync_unfold(PeerId, Unfolds) ->
    gen_server:call(?SERVER, {unfold, PeerId, Unfolds}).

%% -- gen_server callbacks ---------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({accept, PeerId, PeerCon}, _From, State) ->
    handle_accept(State, PeerId, PeerCon);
handle_call({get, PeerId, TxHashes}, _From, State) ->
    Res = handle_get(State, PeerId, TxHashes),
    {reply, Res, State};
handle_call({unfold, PeerId, Unfolds}, _From, State) ->
    Res = handle_unfold(State, PeerId, Unfolds),
    {reply, Res, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({connect, PeerId, PeerCon}, State) ->
    {noreply, handle_connect(State, PeerId, PeerCon)};
handle_cast({finish, PeerId, _Status}, State) ->
    {noreply, handle_finish(State, PeerId)};
handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info({local_action, Ref, TimerRef, Action, Result}, State) ->
    erlang:cancel_timer(TimerRef),
    %% We might have already gotten the message :-(
    receive {timeout, TimerRef, _} -> ok
    after 0 -> ok end,
    {noreply, handle_local_action(State, Action, Ref, Result)};
handle_info({timeout, _TimerRef, {local_action, Ref, Worker, Action}}, State) ->
    kill_local_action(Worker, Ref, Action),
    {noreply, handle_local_action(State, Action, Ref, {error, timeout})};
handle_info({timeout, TimerRef, re_sync}, State) ->
    {noreply, handle_re_sync(State, TimerRef)};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    {noreply, handle_down(State, Ref)};
handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Local functions --------------------------------------------------------
handle_connect(State = #state{ local = not_synced }, PeerId, PeerCon) ->
    epoch_sync:info("TX-pool synchronization (outgoing) started towards ~p",
                    [ppp(PeerId)]),
    Sync = mk_sync(PeerId, PeerCon),
    do_local_action(Sync, tree),
    State#state{ local = {active, Sync} };
handle_connect(State, _PeerId, _PeerCon) ->
    State.

handle_accept(State = #state{ remote = Remotes }, PeerId, _PeerCon)
        when length(Remotes) >= ?MAX_INCOMING_SYNC ->
    epoch_sync:debug("Incoming TX-pool sync from ~p rejected, too many connections",
                     [ppp(PeerId)]),
    {reply, {error, too_many_simultaneous_sync_attempts}, State};
handle_accept(State = #state{ remote = Remotes }, PeerId, PeerCon) ->
    case lists:keyfind(PeerId, #sync.peer_id, Remotes) of
        false ->
            Sync = mk_sync(PeerId, PeerCon),
            do_local_action(Sync, tree),
            epoch_sync:info("TX-pool synchronization (incoming) started from ~p",
                            [ppp(PeerId)]),
            {reply, ok, State#state{ remote = [Sync | Remotes] }};
        #sync{} ->
            epoch_sync:debug("Incoming TX-pool sync from ~p rejected, already syncing",
                             [ppp(PeerId)]),
            {reply, {error, already_syncing_with_peer}, State}
    end.

handle_down(State = #state{ local = {active, Sync = #sync{ peer_con_ref = Ref }} }, Ref) ->
    case repair_sync(Sync) of
        {ok, NewSync} ->
            epoch_sync:debug("Peer connection died - using new one instead!", []),
            State#state{ local = {active, NewSync} };
        {error, _} ->
            epoch_sync:info("Peer connection died - TX-pool synchronization aborted!", []),
            State#state{ local = not_synced }
    end;
handle_down(State = #state{ remote = Remotes }, Ref) ->
    case lists:keyfind(Ref, #sync.peer_con_ref, Remotes) of
        false ->
            State;
        Sync = #sync{ id = Id } ->
            case repair_sync(Sync) of
                {ok, NewSync} ->
                    State#state{ remote = lists:keyreplace(Id, #sync.id, Remotes, NewSync) };
                {error, _} ->
                    epoch_sync:info("Peer connection died for remote - TX-pool sync aborted!", []),
                    State#state{ remote = lists:keydelete(Id, #sync.id, Remotes) }
            end
    end.

handle_re_sync(State = #state{ local = {in_sync, TimerRef} }, TimerRef) ->
    State#state{ local = not_synced };
handle_re_sync(State, _TimerRef) -> %% Stale TimerRef
    State.

handle_local_action(State = #state{ local = not_synced }, _Action, _Ref, _Result) ->
    %% Synchronization aborted
    State;
handle_local_action(State = #state{ local = {active, #sync{ id = Ref }} }, Action, Ref, Result) ->
    handle_local_action(State, Action, Result);
handle_local_action(State = #state{ remote = Remotes }, tree, Ref, Result) ->
    case lists:keyfind(Ref, #sync.id, Remotes) of
        false -> %% ignore stale result
            State;
        Sync = #sync{} ->
            case Result of
                {ok, Tree} ->
                    Sync1 = Sync#sync{ tree = Tree },
                    State#state{ remote = lists:keyreplace(Ref, #sync.id, Remotes, Sync1) };
                {error, _} ->
                    State#state{ remote = lists:keydelete(Ref, #sync.id, Remotes) }
            end
    end;
handle_local_action(State, _Action, _Ref, _Result) ->
    %% Ignore stale result
    State.

handle_local_action(State = #state{ local = {active, Sync} }, Action, {error, _} = Err) ->
    epoch_sync:debug("Local TX-pool sync action ~p failed (~p)", [Action, Err]),
    do_local_action(Sync, {finish, false}),
    State;
handle_local_action(State = #state{ local = {active, Sync} }, Action, Res) ->
    case {Action, Res} of
        {tree, {ok, Tree}} ->
            do_local_action(Sync, init),
            State#state{ local = {active, Sync#sync{ tree = Tree }} };
        {init, ok} ->
            RootHash = aeu_mp_trees:root_hash(Sync#sync.tree),
            do_local_action(Sync, {unfold, [{node, <<>>, RootHash}], 50}),
            State#state{ local = {active, Sync#sync{ data = [] }} };
        {unfold, {ok, tree_not_ready, OldUnfolds, Delay}} when Delay < 10000 ->
            %% Remote tree is not computed, retry with delay
            epoch_sync:debug("Remote tree temporary failure, retrying in ~p ms",
                             [Delay + 1000]),
            do_local_action(Sync, {unfold, OldUnfolds, Delay + 1000}),
            State;
        {unfold, {ok, tree_not_ready, _OldUnfolds, _Delay}} ->
            epoch_sync:debug("Remote tree permanent failure, aborting", []),
            do_local_action(Sync, {finish, false}),
            State;
        {unfold, {ok, NewUnfolds}} ->
            #sync{ data = OldGets } = Sync,
            {NewUnfolds1, NewGets} = analyze_unfolds(NewUnfolds, Sync#sync.tree),
            NewGets1 = OldGets ++ NewGets,
            case NewUnfolds1 of
                [] ->
                    %% TODO - maybe chunkify
                    epoch_sync:info("TX-pool sync requires getting ~p TXs",
                                    [length(NewGets1)]),
                    do_local_action(Sync, {get, NewGets1}),
                    State#state{ local = {active, Sync#sync{ data = [] }} };
                _ ->
                    do_local_action(Sync, {unfold, NewUnfolds1, 0}),
                    State#state{ local = {active, Sync#sync{ data = NewGets1 }} }
            end;
        {get, {ok, NTxs}} ->
            epoch_sync:info("TX-pool sync added ~p TXs", [NTxs]),
            do_local_action(Sync, {finish, true}),
            State;
        {finish, {ok, Done}} ->
            erlang:demonitor(Sync#sync.peer_con_ref, [flush]),
            case Done of
                true  ->
                    epoch_sync:info("TX-pool synchronization finished!", []),
                    TimerRef = erlang:start_timer(re_sync_timeout(), self(), re_sync),
                    State#state{ local = {in_sync, TimerRef} };
                false ->
                    epoch_sync:info("TX-pool synchronization aborted!", []),
                    State#state{ local = not_synced }
            end
    end.

handle_get(State, PeerId, TxHashes) ->
    case lists:keyfind(PeerId, #sync.peer_id, State#state.remote) of
        false ->
            {error, no_active_sync};
        #sync{} ->
            {ok, do_get(TxHashes)}
    end.

handle_unfold(State, PeerId, SerUnfolds) ->
    case lists:keyfind(PeerId, #sync.peer_id, State#state.remote) of
        false ->
            {error, no_active_sync};
        #sync{ tree = undefined } ->
            {error, tree_not_ready};
        #sync{ tree = Tree } ->
            case deserialize_unfolds(SerUnfolds) of
                {ok, Unfolds} ->
                    NewUnfolds =
                        lists:append([ unfold(Unfold, Tree) || Unfold <- Unfolds ]),
                    {ok, serialize_unfolds(NewUnfolds)};
                Err = {error, _} ->
                    Err
            end
    end.

handle_finish(State = #state{ remote = Remotes }, PeerId) ->
    epoch_sync:debug("Finish remote TX-pool sync ~p", [ppp(PeerId)]),
    State#state{ remote = lists:keydelete(PeerId, #sync.peer_id, Remotes) }.

mk_sync(PeerId, PeerCon) ->
    Ref  = erlang:monitor(process, PeerCon),
    #sync{ id = make_ref(), peer_id = PeerId
         , peer_con = PeerCon, peer_con_ref = Ref }.

repair_sync(Sync = #sync{ peer_id = PeerId }) ->
    case aec_peers:get_connection(PeerId) of
        {ok, NewPeerCon} ->
            NewRef = erlang:monitor(process, NewPeerCon),
            {ok, Sync#sync{ peer_con = NewPeerCon, peer_con_ref = NewRef }};
        _ ->
            {error, no_connection}
    end.

-define(LOCAL_ACTION_TIMEOUT, (?REQUEST_TIMEOUT + 3000)).

do_local_action(Sync, tree) ->
    do_local_action(Sync, tree, fun build_tx_mpt/0, ?LOCAL_ACTION_TIMEOUT);
do_local_action(Sync = #sync{ peer_id = PeerId }, init) ->
    Fun = fun() -> aec_peer_connection:tx_pool_sync_init(PeerId) end,
    do_local_action(Sync, init, Fun, ?LOCAL_ACTION_TIMEOUT);
do_local_action(Sync = #sync{ peer_id = PeerId }, {unfold, Unfolds, Delay}) ->
    Fun = fun() ->
              timer:sleep(Delay),
              SerUnfolds = serialize_unfolds(Unfolds),
              case aec_peer_connection:tx_pool_sync_unfold(PeerId, SerUnfolds) of
                  {ok, NewSerUnfolds} ->
                      deserialize_unfolds(NewSerUnfolds);
                  {error, <<"tree_not_ready">>} ->
                      {ok, tree_not_ready, Unfolds, Delay};
                  Err = {error, _} ->
                      Err
              end
          end,
    do_local_action(Sync, unfold, Fun, ?LOCAL_ACTION_TIMEOUT);
do_local_action(Sync = #sync{ peer_id = PeerId }, {get, TxHashes}) ->
    Fun = fun() ->
              case aec_peer_connection:tx_pool_sync_get(PeerId, TxHashes) of
                  {ok, Txs} ->
                      [ aec_tx_pool:push(Tx, tx_received) || Tx <- Txs ],
                      {ok, length(Txs)};
                  Err = {error, _} ->
                      Err
              end
          end,
    %% Actually pushing the transactions might be a slow operation,
    %% increase the timeout.
    do_local_action(Sync, get, Fun, ?LOCAL_ACTION_TIMEOUT * 10);
do_local_action(Sync = #sync{ peer_id = PeerId }, {finish, Done}) ->
    %% Try to send finish to Peer - if it fails it fails but at least
    %% we tried.
    Fun = fun() ->
              try
                  aec_peer_connection:tx_pool_sync_finish(PeerId, Done)
              catch _:_ ->
                  ok
              end,
              {ok, Done}
          end,
    do_local_action(Sync, finish, Fun, ?LOCAL_ACTION_TIMEOUT).

%% To avoid locking up main process (and deadlock in the interaction with
%% aec_peer_connection) we do all requests in a worker process. The worker
%% is linked, and we also start a timer when spawning it. The timeout is
%% 10s which should be sufficiently more than the request timeout (7s) in
%% aec_peer_connection.
do_local_action(#sync{ id = Ref }, Action, ActionFun, Timeout) ->
    Self = self(),
    Fun = fun() ->
              %% Receive the timeout message ref so
              %% we can cancel it on success...
              receive {go, SendRef} -> ok end,
              Res =
                  try
                      ActionFun()
                  catch _:Reason ->
                      {error, Reason}
                  end,
              Self ! {local_action, Ref, SendRef, Action, Res}
          end,
    Pid        = spawn_link(Fun),
    TimeoutMsg = {local_action, Ref, Pid, Action},
    SendRef    = erlang:start_timer(Timeout, self(), TimeoutMsg),
    Pid ! {go, SendRef}.

kill_local_action(Worker, Ref, Action) ->
    unlink(Worker),
    erlang:exit(Worker, kill),
    %% Flush any late arriving result
    receive {local_action, Ref, _, Action, _} -> ok
    after 0 -> ok end.

do_get(TxHashes) ->
    lists:concat([ do_get_(Tx) || Tx <- TxHashes ]).

do_get_(TxHash) ->
    try
        STx = aec_db:get_signed_tx(TxHash),
        [STx]
    catch _:_ ->
        []
    end.

-spec unfold(unfold(), aeu_mp_trees:tree()) -> [unfold()].
unfold({node, Path, Node}, Tree) ->
    aeu_mp_trees:unfold(Path, Node, Tree);
unfold({leaf, Path}, _Tree) ->
    [{leaf, Path}];
unfold({subtree, Path}, Tree) ->
    get_subtree(Path, Tree);
unfold({key, Key}, _Tree) ->
    [{key, Key}].

-spec get_subtree(aeu_mp_trees:path(), aeu_mp_trees:tree()) -> [unfold_key()].
get_subtree(Key, _Tree) when bit_size(Key) =:= ?TXS_HASH_BYTES * 8 ->
    [{key, Key}];
get_subtree(Path, Tree) ->
    get_subtree(aeu_mp_trees:iterator_from(Path, Tree), Path, bit_size(Path), []).

get_subtree(Iter, Path, S, Acc) ->
    case aeu_mp_trees:iterator_next(Iter) of
        {Key = <<Path:S/bits, _Rest/bits>>, _, NewIter}
                when byte_size(Key) =:= ?TXS_HASH_BYTES ->
            get_subtree(NewIter, Path, S, [{key, Key} | Acc]);
        _X ->
            lists:reverse(Acc)
    end.

build_tx_mpt() ->
    try
        F = fun(TxHash, Tree) ->
                aeu_mp_trees:put(TxHash, ?TX_PLACEHOLDER, Tree)
            end,
        Tree = aec_db:fold_mempool(F, aeu_mp_trees:new()),
        {ok, Tree}
    catch _:R ->
        {error, R}
    end.

-define(NODE_TAG, 0).
-define(LEAF_TAG, 1).
-define(SUBTREE_TAG, 2).
-define(KEY_TAG, 3).

-spec serialize_unfolds([unfold()]) -> [aeser_rlp:encoded()].
serialize_unfolds(Us) ->
    [ serialize_unfold(U) || U <- Us ].

-spec deserialize_unfolds([aeser_rlp:encoded()]) ->
        {ok, [unfold()]} | {error, term()}.
deserialize_unfolds(SUs) ->
    try
        {ok, [ deserialize_unfold(SU) || SU <- SUs ]}
    catch _:Reason ->
        {error, Reason}
    end.

-spec serialize_unfold(unfold()) -> aeser_rlp:encoded().
serialize_unfold({node, Path, Node}) ->
    aeser_rlp:encode(
        aeserialization:encode_fields(
            [{type, int}, {path, binary}, {node, binary}],
            [{type, ?NODE_TAG}, {path, serialize_niblets(Path)}, {node, Node}]));
serialize_unfold({leaf, Path}) ->
    aeser_rlp:encode(
        aeserialization:encode_fields(
            [{type, int}, {leaf, binary}],
            [{type, ?LEAF_TAG}, {leaf, serialize_niblets(Path)}]));
serialize_unfold({subtree, Path}) ->
    aeser_rlp:encode(
        aeserialization:encode_fields(
            [{type, int}, {subtree, binary}],
            [{type, ?SUBTREE_TAG}, {subtree, serialize_niblets(Path)}]));
serialize_unfold({key, Key}) ->
    aeser_rlp:encode(
        aeserialization:encode_fields(
            [{type, int}, {key, binary}],
            [{type, ?KEY_TAG}, {key, Key}])).

-spec deserialize_unfold(aeser_rlp:encoded()) -> unfold().
deserialize_unfold(Blob) ->
    [TypeBin | Fields] = aeser_rlp:decode(Blob),
    [{type, Type}] = aeserialization:decode_fields([{type, int}], [TypeBin]),
    deserialize_unfold(Type, Fields).

deserialize_unfold(?NODE_TAG, Flds) ->
    [{path, Path}, {node, Node}] =
        aeserialization:decode_fields([{path, binary}, {node, binary}], Flds),
    {node, deserialize_niblets(Path), Node};
deserialize_unfold(?LEAF_TAG, Flds) ->
    [{leaf, Path}] = aeserialization:decode_fields([{leaf, binary}], Flds),
    {leaf, deserialize_niblets(Path)};
deserialize_unfold(?SUBTREE_TAG, Flds) ->
    [{subtree, Path}] = aeserialization:decode_fields([{subtree, binary}], Flds),
    {subtree, deserialize_niblets(Path)};
deserialize_unfold(?KEY_TAG, Flds) ->
    [{key, Key}] = aeserialization:decode_fields([{key, binary}], Flds),
    {key, Key}.

%% Paths are not necessarily even bytes, this is required by RLP so we have to
%% pad this using parity information.
serialize_niblets(Bin) ->
    case bit_size(Bin) rem 8 of
        0 -> <<0:8, Bin/bitstring>>;
        4 -> <<1:4, Bin/bitstring>>
    end.

deserialize_niblets(<<1:4, Bin/bitstring>>) ->
    Bin;
deserialize_niblets(<<0:8, Bin/bitstring>>) ->
    Bin.

analyze_unfolds(Us, Tree) ->
    analyze_unfolds(Us, Tree, [], []).

analyze_unfolds([], _Tree, NewUs, NewGets) ->
    {lists:reverse(NewUs), lists:reverse(NewGets)};
analyze_unfolds([{key, Key} | Us], Tree, NewUs, NewGets) ->
    analyze_unfolds(Us, Tree, NewUs, [Key | NewGets]);
analyze_unfolds([{leaf, Key} | Us], Tree, NewUs, NewGets) ->
    case aeu_mp_trees:get(Key, Tree) of
        <<>> ->
            %% Key is not in local tree - i.e. TX is missing, get it.
            analyze_unfolds(Us, Tree, NewUs, [Key | NewGets]);
        ?TX_PLACEHOLDER ->
            analyze_unfolds(Us, Tree, NewUs, NewGets)
    end;
analyze_unfolds([N = {node, Path, Node} | Us], Tree, NewUs, NewGets) ->
    case aeu_mp_trees:has_node(Path, Node, Tree) of
        no ->
            analyze_unfolds(Us, Tree, [{subtree, Path} | NewUs], NewGets);
        maybe ->
            analyze_unfolds(Us, Tree, [N | NewUs], NewGets);
        yes ->
            analyze_unfolds(Us, Tree, NewUs, NewGets)
    end.

re_sync_timeout() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"sync_interval">>],
                               aecore, mempool_sync_interval, ?DEFAULT_SYNC_INTERVAL).
