-module(aec_trees_proxy).

-export([ prepare_for_par_eval/1
        , par_eval/4
        , start_monitor/4 ]).
        %% , register_clients/2 ]).

%% -export([ client_tree/3 ]).

-export([ proxy_init/1
        , proxy_get/4
        , proxy_put/4
        , proxy_iter/4 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

%% -dialyzer([{nowarn_function, spawn_monitor_worker/5}]).

%% -type tree_type() :: aec_trees:tree_type().
-type subtree_type() :: aec_trees:subtree_type().

%% the requests will show up in crash reports as "Last message in", but those reports
%% don't reveal the pid of the caller. By setting the 'from' field to self(), we can
%% match the caller pid with a transaction index. The from field is immediately overwritten
%% with {Pid, Ref}, i.e. the gen_server reply handle, upon receipt.
%%
-record(req, { op   :: get | put
             , mod  :: module()
             , type :: subtree_type() | undefined
             , f    :: atom()
             , args :: tuple()
             , from = self() :: {pid(), reference()} | pid()
             }).

-type req() :: #req{}.

-type signed_tx() :: aetx_sign:signed_tx().
-type index() :: non_neg_integer().
-type id_key() :: {subtree_type(), aeser_id:id()}.
-type indexed_txs() :: [{index(), signed_tx()}].
-type deps_map() :: #{ id_key() => gb_sets:set(index()) }.
-type claims_map() :: #{ id_key() => gb_sets:set(index()) }.

-type trees()    :: aec_trees:trees().
-type env()      :: aetx_env:env().
-type proplist() :: proplists:proplist().
-type protocol() :: non_neg_integer().

-type context() :: #{ indexed       := indexed_txs()
                    , tx_count      := non_neg_integer()
                    , max_dep_count := non_neg_integer()
                    , deps          := deps_map()
                    , claims        := claims_map() }.

-record(st, { parent        :: pid()
            , trees         :: trees()
            , env           :: aetx_env:env()
            , proxy_trees   :: aec_trees:trees()
            , dontverify    :: boolean()
            , strict        :: boolean()
            %% , ctrees        :: #{ index() => aec_trees:trees() }
            , status  = #{} :: #{ index() => done | ongoing }
            , deps    = #{} :: deps_map()
            , claims  = #{} :: claims_map()
            , clients = #{} :: #{ index() => {pid(), reference(), signed_tx()} }
            , pids    = #{} :: #{ pid() => index() }
            , pending = gb_trees:empty()  :: gb_trees:tree(index(), req())
            , valid   = []  :: [{index(), signed_tx()}]
            , invalid = []  :: [{index(), signed_tx()}]
            , events  = []  :: aetx_env:events() }).

-record(pstate, { cache :: term()
                , type  :: subtree_type()
                , pid   :: pid() }).

-include_lib("aeutils/include/aeu_proxy.hrl").
-include_lib("mnesia/src/mnesia.hrl").
-include_lib("trace_runner/include/trace_runner.hrl").

par_eval(Trees, Env, Context, Opts) ->
    {ok, {Pid, MRef}} = start_monitor(Trees, Env, Context, Opts),
    receive
        {Pid, result, Result} ->
            receive
                {'DOWN', MRef, _, _, _} ->
                    Result
            end;
        {'DOWN', MRef, _, _, Error} ->
            {error, Error}
    after 5000 ->
            {error, timeout}
    end.

-spec start_monitor(aec_trees:trees(), aetx_env:env(), context(), proplist()) ->
          {ok, {pid(), reference()}}.
-if(?OTP_RELEASE >= 23).
start_monitor(Trees, Env, #{indexed := _} = Ctxt, Opts) ->
    gen_server:start_monitor(?MODULE, {self(), Trees, Env, Ctxt}, []).
-else.
start_monitor(Trees, Env, #{indexed := _} = Ctxt, Opts) ->
    _TStore = get_tstore(),
    {ok, Pid} = gen_server:start(?MODULE, {self(), Trees, Env, Ctxt, Opts}, []),
    MRef = monitor(process, Pid),
    {ok, {Pid, MRef}}.
-endif.

%% ======================================================================
%% Gen_server side
%% ======================================================================

-spec init({pid(), trees(), env(), context(), proplist()}) -> {ok, #st{}}.
init({Parent, Trees, Env, #{ indexed := Indexed
                           , deps    := Deps
                           , claims  := Claims }, Opts}) ->
    ArgF = fun(Type) ->
                   aeu_mtrees:proxy_tree(?MODULE, #{ type => Type
                                                   , pid  => self() })
           end,
    ProxyTrees = aec_trees:proxy_trees(ArgF),
    DontVerify = proplists:get_value(dont_verify_signature, Opts, false),
    Strict = proplists:get_value(strict, Opts, false),
    Events = aetx_env:events(Env),
    S0 = #st{ parent      = Parent
            , clients     = #{}
            , deps        = Deps
            , claims      = Claims
            , proxy_trees = ProxyTrees
            , dontverify  = DontVerify
            , strict      = Strict
            , env         = Env
            , trees       = Trees
            , events      = Events },
    {ok, start_workers(Indexed, ProxyTrees, Env, DontVerify, S0)}.

handle_call({?MODULE, #req{} = Req}, {Pid, _} = From, #st{} = St) ->
    handle_req(Req#req{from = From}, Pid, St).

handle_cast(_, St) ->
    {noreply, St}.

handle_info({'DOWN', _MRef, process, Pid, Reason}, #st{ pids = Pids } = St) ->
    case maps:find(Pid, Pids) of
        {ok, Ix} ->
            ?event({'DOWN', Pid, Ix, Reason}),
            Pids1 = maps:remove(Pid, Pids),
            ?event({pids1, Pids1}, St),
            tx_down(Ix, Pid, Reason, St#st{pids = Pids1});
        error ->
            ?event({ignoring_DOWN, Pid}),
            {noreply, St}
    end;
handle_info(_, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

tx_down(_Ix, Pid, {'$caught', Type, What}, #st{strict = true} = St) ->
    throw_error(Type, What, Pid, St);
tx_down(Ix, _Pid, Reason, #st{ status = Status
                             , events = Events0 } = St) ->
    %% All valid txs must have at least some claims.
    {IsValid, Events} =
        case Reason of
            {ok, NewEvents} ->
                {true, Events0 ++ NewEvents};
            _ ->
                {false, Events0}
        end,
    St1 = log_valid(IsValid, Ix, St#st{ status = Status#{Ix => done}
                                      , events = Events }),
    case map_size(St1#st.pids) of
        0 ->
            ?event(no_workers_left),
            #st{parent = Parent, valid = Valid, invalid = Invalid, trees = Trees} = St1,
            Valid1 = [STx || {_, STx} <- lists:keysort(1, Valid)],
            Invalid1 = [STx || {_, STx} <- lists:keysort(1, Invalid)],
            Parent ! {self(), result, {ok, Valid1, Invalid1, Trees, Events}},
            {stop, normal, St1};
        _ ->
            {noreply, serve_pending(St1)}
    end.

throw_error(Type, What, Pid, #st{parent = Parent, clients = Cs} = St) ->
    [kill_worker(W) || {P,_,_} = W <- maps:values(Cs),
                       P =/= Pid],
    Parent ! {self(), result, {error, {Type, What}}},
    {stop, normal, St}.

-spec start_workers(indexed_txs(), aec_trees:trees(), aetx_env:env(), boolean(), #st{}) -> #st{}.
start_workers(IndexedTxs, ProxyTrees, Env, DontVerify, S) ->
    Protocol = get_protocol(Env),
    {Clients, Pids} =
        lists:foldl(
          fun({N, STx}, Ms) ->
                  start_worker(N, STx, ProxyTrees, Env, DontVerify, Protocol, Ms)
          end, {#{}, #{}}, IndexedTxs),
    S#st{clients = Clients, pids = Pids}.

start_worker(N, SignedTx, Trees, Env, DontVerify, CsPs) ->
    start_worker(N, SignedTx, Trees, Env, DontVerify, get_protocol(Env), CsPs).

start_worker(N, SignedTx, Trees, Env, DontVerify, Protocol, {Clients, Pids}) ->
    {Pid, MRef} = spawn_monitor(
                    worker_spawn_f(SignedTx, Trees, Env, DontVerify, Protocol)),
    {Clients#{N => {Pid, MRef, SignedTx}}, Pids#{Pid => N}}.

-spec worker_spawn_f(signed_tx(), trees(), env(), boolean(), protocol()) ->
          fun(() -> no_return()).
%% If inlined, this fun makes Dialyzer complain.
worker_spawn_f(SignedTx, Trees, Env, DontVerify, Protocol) ->
    fun() ->
            apply_one_tx(SignedTx, Trees, Env, DontVerify, Protocol)
    end.

get_protocol(Env) ->
    aetx_env:consensus_version(Env).

restart_workers(Restarts, St) ->
    lists:foldl(fun restart_worker/2, St, Restarts).

restart_worker(Ix, #st{ clients     = Cs0
                      , pids        = Pids0
                      , env         = Env
                      , proxy_trees = ProxyTrees
                      , dontverify  = DontVerify } = S) ->
    case maps:find(Ix, Cs0) of
        {ok, {Pid, _, SignedTx} = Worker} ->
            ?event({restarting_worker, Ix, Pid}),
            kill_worker(Worker),
            {Cs, Pids} = start_worker(
                           Ix, SignedTx, ProxyTrees, Env, DontVerify,
                           {Cs0, maps:remove(Pid, Pids0)}),
            S#st{clients = Cs, pids = Pids};
        error ->
            S
    end.

kill_worker({Pid, MRef, _}) ->
    erlang:demonitor(MRef),
    exit(Pid, kill).

-spec prepare_for_par_eval([signed_tx()]) -> context().
prepare_for_par_eval(SignedTxs) ->
    {IndexedTxs, {TxCnt, {MaxDepCnt, Deps, Claims}}} =
        lists:mapfoldl(
          fun(SignedTx, {N, Acc}) ->
                  N1 = N+1,
                  { {N1, SignedTx},
                    {N1, log_initial_deps(SignedTx, N, Acc)} }
          end, {0, {0, #{}, #{}}}, SignedTxs),
    #{ indexed       => IndexedTxs
     , tx_count      => TxCnt
     , max_dep_count => MaxDepCnt
     , deps          => Deps
     , claims        => Claims }.

log_initial_deps(SignedTx, N, {Max0, Deps0, Claims0}) ->
    {Mod, Tx} = aetx:specialize_callback(aetx_sign:tx(SignedTx)),
    Ids = Mod:entities(Tx),
    {NewMax, Deps} =
        lists:foldl(
          fun(Id, {Max, Depsx}) ->
                  Type = id_to_type(Id),
                  {Count, Depsx1} = update_dep(Type, Id, N, Depsx),
                  {max(Count, Max), Depsx1}
          end, {Max0, Deps0}, Ids),
    Claims = Claims0#{ N => gb_sets:from_list(Ids) },
    {NewMax, Deps, Claims}.

update_dep(Type, Id, Ix, Deps) ->
    Key = {Type, Id},
    case maps:find(Key, Deps) of
        {ok, Set} ->
            Set1 = gb_sets:add_element(Ix, Set),
            {gb_sets:size(Set1), Deps#{Key => Set1}};
        error ->
            {1, Deps#{Key => gb_sets:singleton(Ix)}}
    end.

%% We map dependencies keyed on {Type, Id} where Id is whatever it is the client
%% looks up. In the case of txs, the entities are proper ids, but we can't necessarily
%% derive a proper id from the lookup key used for accessing a tree.
id_to_type(Id) ->
    {Tag, _} = aeser_id:specialize(Id),
    case Tag of
        account    -> accounts;
        name       -> ns;
        commitment -> ns;
        oracle     -> oracles;
        contract   -> contracts;
        channel    -> channels
    end.

-spec apply_one_tx(signed_tx(), trees(), env(), boolean(), protocol()) -> no_return().
apply_one_tx(SignedTx, Trees, Env, DontVerify, Protocol) ->
    try aec_trees:apply_one_tx(SignedTx, Trees, Env, DontVerify, Protocol) of
        {ok, _Trees1, Env1} ->
            exit({ok, aetx_env:events(Env1)});
        {error, _} = Err ->
            exit(Err)
    catch
        Type:What:ST ->
            ?event({'CAUGHT', Type, What, ST}),
            exit({'$caught', Type, What})
    end.

-spec log_valid(boolean(), index(), #st{}) -> #st{}.
log_valid(IsValid, Ix, #st{valid = Valid, invalid = Invalid, clients = Cs} = St) ->
    {_, _, SignedTx} = maps:get(Ix, Cs),
    case IsValid of
        true->
            St#st{valid = [{Ix, SignedTx} | Valid]};
        false ->
            St#st{invalid = [{Ix, SignedTx} | Invalid]}
    end.

maybe_add_pend(Mode, Ix, Req, #st{pending = Pend} = St) ->
    case Mode of
        new   -> St#st{pending = gb_trees:insert(Ix, Req, Pend)};
        retry -> St
    end.

maybe_remove_pend(Mode, Ix, #st{pending = Pend} = St) ->
    case Mode of
        retry -> St#st{pending = gb_trees:delete_any(Ix, Pend)};
        new   -> St
    end.

serve_pending(#st{ pending = Pend } = St) ->
    I = gb_trees:iterator(Pend),
    serve_pending(gb_trees:next(I), St).

serve_pending(none, St) ->
    St;
serve_pending({Ix, Req, I}, #st{pending = Pend, status = Status} = St) ->
    case is_done(Ix, Status) of
        true ->
            St1 = St#st{pending = gb_trees:delete_any(Ix, Pend)},
            serve_pending(gb_trees:next(I), St1);
        false ->
            St1 = try_serve_req(Req, Ix, St, retry),
            serve_pending(gb_trees:next(I), St1)
    end.

handle_req(Req, Pid, #st{pids = Pids} = St) ->
    case maps:find(Pid, Pids) of
        {ok, Ix} ->
            %% All replies are explicit
            {noreply, try_serve_req(Req, Ix, St, new)};
        error ->
            %% ... except this one
            {reply, {'$fail', unknown_pid}, St}
    end.

try_serve_req(#req{op = Op, type = Type, f = F, args = Args, from = From} = Req, Ix, St, Mode) ->
    try check_deps(Req, Ix, St) of
        {reply, Reply, St1} ->
            reply(From, Ix, Reply),
            maybe_remove_pend(Mode, Ix, St1);
        {ok, St1} ->
            {Res, St2} = perform_req(Op, F, Args, Type, St1),
            reply(From, Ix, Res),
            maybe_remove_pend(Mode, Ix, St2);
        {wait, St1} ->
            maybe_add_pend(Mode, Ix, Req, St1);
        {restart, _, St1} ->
            ?event({restarting_worker, Ix, Mode}),
            restart_worker(Ix, St1)
    catch
        throw:{error, _} = Error ->
            reply(From, Ix, Error),
            St
        %% {error, _} = Err ->
        %%     gen_server:reply(From, {'$fail', Err}),
        %%     maybe_remove_pend(Mode, Ix, St)
    end.

reply(From, _Ix, Reply) ->
    gen_server:reply(From, Reply).

perform_req(get, F, Args, Type, St) ->
    ?event({req, get, F, Args, Type}),
    {perform_get(F, Args, Type, St), St};
perform_req(put, F, Args, Type, St) ->
    ?event({req, put, F, Args, Type}),
    Tree = perform_put(F, Args, Type, St),
    {ok, set_mtree(Type, Tree, St)}.

perform_get(lookup, {Key}, Type, St) ->
    aeu_mtrees:lookup(Key, get_mtree(Type, St));
perform_get(read_only_subtree, {Key}, Type, St) ->
    aeu_mtrees:read_only_subtree(Key, get_mtree(Type, St));
perform_get(root_hash, {}, Type, St) ->
    aeu_mtrees:root_hash(get_mtree(Type, St)).

perform_put(delete, {Key}, Type, St) ->
    aeu_mtrees:delete(Key, get_mtree(Type, St));
perform_put(enter, {Key, Value}, Type, St) ->
    aeu_mtrees:enter(Key, Value, get_mtree(Type, St));
perform_put(insert, {Key, Value}, Type, St) ->
    aeu_mtrees:insert(Key, Value, get_mtree(Type, St)).

get_mtree(Type, #st{trees = Trees}) ->
    aec_trees:get_mtree(Type, Trees).

set_mtree(Type, Tree, #st{trees = Trees} = St) ->
    St#st{trees = aec_trees:set_mtree(Type, Tree, Trees)}.

deps_ids(root_hash, _, Type, _Trees) ->
    [{Type, root_hash}];
deps_ids(delete, {Key}, Type, _Trees) ->
    [{Type, Key}];
deps_ids(get, {Key}, Type, _Trees) ->
    [{Type, Key}];
deps_ids(lookup, {Key}, Type, _Trees) ->
    [{Type, Key}];
deps_ids(enter, {Key, _}, Type, _Trees) ->
    [{Type, Key}];
deps_ids(insert, {Key, _}, Type, _Trees) ->
    [{Type, Key}];
deps_ids(read_only_subtree, {Key}, Type, Trees) ->
    %% Is there any way we can answer this without actually returning the tree?
    Tree = aec_trees:get_mtree(Type, Trees),
    case aeu_mtrees:read_only_subtree(Key, Tree) of
        {ok, Subtree} ->
            {[ {Type, K} || {K, _} <- aeu_mtrees:to_list(Subtree) ], {ok, Subtree}};
        {error, _} = Error ->
            throw(Error)
    end.

check_deps(#req{type = Type, f = F, args = Args} = Req, Ix, #st{trees = Trees} = St) ->
    ?event({check_deps, Req}),
    case deps_ids(F, Args, Type, Trees) of
        {Ids, CachedResult} ->
            %% e.g. for read_only_subtree
            case check_deps_ids(Ids, Ix, St) of
                {ok, St1} ->
                    {reply, CachedResult, St1};
                Other ->
                    Other
            end;
        Ids when is_list(Ids) ->
            check_deps_ids(Ids, Ix, St)
    end.

check_deps_ids(Ids, Ix, St) ->
    check_deps_ids(Ids, Ix, St, []).

check_deps_ids([], _Ix, St, Restarts) ->
    {ok, restart_workers(Restarts, St)};
check_deps_ids([Id|Ids], Ix, St, Restarts0) ->
    case check_deps_for_id(Id, Ix, St) of
        {ok, Restart, St1} ->
            check_deps_ids(Ids, Ix, St1, Restarts0 ++ Restart);
        {wait, [], St1} ->
            {wait, restart_workers(Restarts0, St1)};
        {restart, _, _} = Restart ->
            %% We ignore the accumulated restart candidates and restart requester instead
            Restart
    end.

check_deps_for_id(Id, Ix, #st{ deps    = Deps
                             , claims  = Claims
                             , status  = Status } = S) ->
    {Action, Restart, Deps1, Claims1} = check_claim(Id, Ix, Deps, Claims, Status),
    {Action, Restart, S#st{ deps   = Deps1
                          , claims = Claims1
                          , status = Status#{Ix => ongoing} }}.

check_claim(Id, Ix, Deps, Claims, Status) ->
    case maps:find(Id, Deps) of
        {ok, IdDeps} ->
            case prune_deps(IdDeps, Status) of
                empty ->
                    {ok, [], Deps#{Id => gb_sets:singleton(Ix)}, add_claim(Ix, Id, Claims)};
                {Ix, IdDeps1} ->
                    {ok, [], Deps#{Id => IdDeps1}, Claims};
                {HdIx, IdDeps1} when HdIx < Ix ->
                    Action = wait_or_restart(Ix, Status),
                    {Action, [], Deps#{Id => gb_sets:add_element(Ix, IdDeps1)}, Claims};
                {HdIx, IdDeps1} when HdIx > Ix ->
                    Restart = maybe_restart_rest(IdDeps1, Status),
                    {ok, Restart, Deps#{Id => gb_sets:add_element(Ix, IdDeps1)},
                     add_claim(Ix, Id, Claims)}
            end;
        error ->
            {ok, [], Deps#{Id => gb_sets:singleton(Ix)}, add_claim(Ix, Id, Claims)}
    end.

add_claim(Ix, Id, Claims) ->
    maps:update_with(Ix, fun(Set) ->
                                 gb_sets:add_element(Id, Set)
                         end, gb_sets:singleton(Id), Claims).

wait_or_restart(Ix, Status) ->
    case is_ongoing(Ix, Status) of
        true ->
            restart;
        false ->
            wait
    end.

maybe_restart_rest(Deps, Status) ->
    gb_sets:fold(fun(Ix, Acc) ->
                         maybe_restart_(Ix, Status, Acc)
                 end, [], Deps).

maybe_restart_(Ix, Status, Acc) ->
    case is_ongoing(Ix, Status) of
        true ->
            [Ix | Acc];
        false ->
            Acc
    end.

is_ongoing(Ix, Status) ->
    ongoing == maps:get(Ix, Status, undefined).

is_done(Ix, Status) ->
    done == maps:get(Ix, Status, undefined).

prune_deps(Deps, Status) ->
    case gb_sets:is_empty(Deps) of
        true ->
            empty;
        false ->
            {H, T} = gb_sets:take_smallest(Deps),
            case is_done(H, Status) of
                true ->
                    prune_deps(T, Status);
                false ->
                    {H, Deps}
            end
    end.    

get_tstore() ->
    case get(mnesia_activity_state) of
        undefined ->
            undefined;
        {_, _, non_transaction} ->
            undefined;
        {_, _, #tidstore{store = Ets}} ->
            check_store(Ets),
            {ets, Ets}
    end.

check_store(Ets) ->
    ets:tab2list(Ets).

%% Proxy callbacks

proxy_init(#{ type := Type
            , pid  := Pid }) ->
    #proxy_mp_tree{ mod = ?MODULE
                  , state = #pstate{ cache = #{}
                                   , type  = Type
                                   , pid   = Pid } }.

proxy_get(F, Mod, Args, P) ->
    call(#req{op = get, mod = Mod, f = F, args = Args}, P).

proxy_put(F, Mod, Args, P) ->
    ok = call(#req{op = put, mod = Mod, f = F, args = Args}, P),
    P.

proxy_iter(_F, _Mod, _Args, _I) ->
    error(nyi).

call(#req{} = Req, #proxy_mp_tree{state = #pstate{ pid  = Proxy
                                                 , type = Type}}) ->
    case gen_server:call(Proxy, {?MODULE, Req#req{type = Type}}) of
        {'$fail', Error} ->
            error(Error);
        Reply ->
            Reply
    end.

-dialyzer({nowarn_function, lager_pr/1}).
lager_pr(St) ->
    lager:pr(St#st{trees=hidden, proxy_trees=hidden}, ?MODULE).

%% For trace-based debugging
event(_L, _E, _S) -> ok.
