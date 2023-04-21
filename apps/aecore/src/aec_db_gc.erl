-module(aec_db_gc).

%% Implementation of Garbage Collection of Account state.
%% To run periodically, the GC needs to be `enabled` (config param),
%% and node should be synced (although it's not strict requirement - if not synced,
%% GC will run a lot more during syncing without much benefit (and slow down syncing).
%% It makes sense to wait until the DB is not updated so rapidly).
%%
%% If key block `interval` (config parameter) is hit, GC starts collecting
%% of the reachable nodes from most recent generations (`history` config parameter).
%% Note that for production use, the `interval` from config is slightly deviated to
%% avoid the situation where most of the nodes run GC at the same time.
%% (Since we assume users will probably keep the `interval` parameter the same.)

%% If #data.enabled and #data.synced are true, #data.hashes indicates the GC status:
%% - undefined    - not the right time to run GC
%% - is pid       - we are scanning reachable nodes
%% - is reference - waiting for signal to execute the `swap_nodes` phase
%%
%% GC scan is performed on the background.
%% The synchronous part of GC is:
%% - writing of the reachable nodes from cache to disk
%% - subsequent restart of the node
%% - removing all old nodes and copying only reachable ones

-behaviour(gen_server).

%% API
-export([start_link/0,
         cleanup/0]).

-export([ height_of_last_gc/0
        , state_at_height_still_reachable/1
        , info/0
        , info/1
        ]).

-export([maybe_garbage_collect/1]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-export([config/0]).

-ifdef(TEST).
-export([install_test_env/0]).
-endif.

-type tree_name() :: aec_trees:tree_name().

-type pid_ref() :: {pid(), reference()}.

-record(scanner,
        { tree   :: tree_name()
        , height :: non_neg_integer()
        , pid    :: pid_ref() }).

-record(st,
        {enabled        :: boolean(),                     % do we garbage collect?
         history        :: non_neg_integer(),             % how many block state back from top to keep
         min_height     :: undefined | non_neg_integer(), % if hash_not_found error, try GC from this height
         depth          :: non_neg_integer(),             % how far below the top to perform scans (0: fork resistance height)
         during_sync    :: boolean(),                     % run GC from the beginning
         synced         :: boolean(),                     % we only run GC if chain is synced
         last_switch = 0 :: non_neg_integer(),            % height at last switch
         last_complete_scan = 0 :: non_neg_integer(),     % height of last complete scan
         trees = []     :: [tree_name()],                 % State trees being GC:ed
         scanners = []  :: [#scanner{}]                   % ongoing scans
        }).

-include_lib("aecore/include/aec_db.hrl").

-define(DEFAULT_HISTORY, 500).

-define(TIMED(Expr), timer:tc(fun () -> Expr end)).
-define(LOG(Fmt), lager:info(Fmt, [])).         % io:format(Fmt"~n")
-define(LOG(Fmt, Args), lager:info(Fmt, Args)). % io:format(Fmt"~n", Args)
-define(LOGERR(Fmt, Args), lager:error(Fmt, Args)). % io:format("ERROR:"Fmt"~n", Args)

%% -define(PROTECT(Expr, OkFun), ?PROTECT(Expr, OkFun, fun signal_scanning_failed/1)).
-define(PROTECT(Expr, OkFun, ErrHeightFun),
        (try Expr of
             Res -> (OkFun)(Res)
         catch
             error:{hash_not_present_in_db_at_height, ErrHeight, MissingHash, Stack} ->
                 ?LOGERR("Scanning at height ~p failed due to a missing hash ~p at: ~p",
                         [ErrHeight, MissingHash, Stack]),
                 (ErrHeightFun)(ErrHeight)
         end)).

-define(JOBS_QUEUE, gc_scan).
-define(SCAN_SLICE, 100).

%%%===================================================================
%%% API
%%%===================================================================


%% We don't support reconfiguration of config parameters when the GC is already up,
%% doesn't seem to have practical utility.
start_link() ->
    #{<<"enabled">> := _, <<"trees">> := _, <<"history">> := _} = Config = config(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

cleanup() ->
    erase_cached_enabled_status().

-spec maybe_garbage_collect(aec_headers:header()) -> ok | nop.
maybe_garbage_collect(Header) ->
    case get_cached_enabled_status() of
        true ->
            gen_server:call(
              ?MODULE, {maybe_garbage_collect, aec_headers:height(Header), Header});
        false ->
            nop
    end.

%% If GC is disabled, or there hasn't yet been a GC, this function returns 0.
-spec height_of_last_gc() -> non_neg_integer().
height_of_last_gc() ->
    case get_cached_enabled_status() of
        true ->
            aec_db:ensure_activity(async_dirty, fun aec_db:read_last_gc_switch/0);
        false ->
            0
    end.

state_at_height_still_reachable(Height) ->
    case get_cached_enabled_status() of
        true ->
            #{last_gc := LastGC} = info([last_gc]),
            LastGC =< Height;
        false ->
            true
    end.

info() ->
    info(info_keys()).

info(Keys) when is_list(Keys) ->
    case Keys -- info_keys() of
        [] ->
            gen_server:call(?MODULE, {info, Keys});
        Invalid ->
            error({invalid_info_keys, Invalid})
    end.

info_keys() ->
    [enabled, history, depth, last_gc, last_scan, active_sweeps, during_sync, trees].

-ifdef(TEST).
install_test_env() ->
    cache_enabled_status(false).
-endif.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Change of configuration parameters requires restart of the node.
init(#{ <<"enabled">>     := Enabled
      , <<"trees">>       := Trees
      , <<"during_sync">> := DuringSync
      , <<"history">>     := History
      , <<"minimum_height">> := MinHeight
      , <<"depth">>       := Depth
      } = Cfg) when is_integer(History), History > 0 ->
    lager:debug("Cfg = ~p", [Cfg]),
    cache_enabled_status(Enabled),
    {LastSwitch, LastScan} =
        case Enabled of
            true ->
                aec_events:subscribe(chain_sync),
                aec_db:ensure_activity(
                  async_dirty,
                  fun() ->
                          {aec_db:read_last_gc_switch(),
                           aec_db:read_last_gc_scan()}
                  end);
            false ->
                {0, 0}
        end,
    lager:debug("LastSwitch = ~p", [LastSwitch]),
    Scanners = maybe_restart_scanners(LastSwitch, LastScan, Trees),
    %% TODO: Make min_height configurable
    Data = #st{enabled      = Enabled,
               during_sync  = DuringSync,
               trees        = Trees,
               history      = History,
               min_height   = MinHeight,
               depth        = Depth,
               last_switch  = LastSwitch,
               last_complete_scan = LastScan,
               scanners     = Scanners,
               synced       = false},
    {ok, Data}.

maybe_restart_scanners(LastSwitch, LastScan, Trees) ->
    if LastSwitch > 0, LastScan < LastSwitch ->
            [start_scanner(T, LastSwitch) || T <- Trees];
       true ->
            []
    end.

%% once the chain is synced, there's no way to "unsync"
handle_info({_, chain_sync, #{info := {chain_sync_done, _}}}, St) ->
    case aec_sync:sync_progress() of
        {false, _, _} ->
            {noreply, St#st{synced = true}};
        _ ->
            {noreply, St}
    end;

handle_info({'DOWN', MRef, process, Pid, Reason}, #st{scanners = Scanners} = St) ->
    case lists:keyfind({Pid, MRef}, #scanner.pid, Scanners) of
        false ->
            {noreply, St};
        #scanner{tree = Tree, height = Height} = S ->
            lager:error("GC Scanner process for ~p died: ~p", [Tree, Reason]),
            signal_scanning_failed(Height),
            Scanners1 = Scanners -- [S],
            NewSt = if Reason =/= normal ->
                            NewS = start_scanner(Tree, Height),
                            St#st{scanners = [NewS | Scanners1]};
                       true ->
                            St#st{scanners = Scanners1}
                    end,
            {noreply, NewSt}
    end;

handle_info(_, St) ->
    {noreply, St}.

handle_call({maybe_garbage_collect, TopHeight, Header}, _From,
            #st{enabled = true, synced = Synced, during_sync = DuringSync,
                history = History, min_height = MinHeight, depth = Depth,
                trees = Trees, scanners = [], last_switch = Last} = St) ->
    SafeHeight = safe_height(TopHeight, Depth),
    if (Synced orelse DuringSync), SafeHeight >= MinHeight, SafeHeight >= Last + History ->
            lager:debug("WILL collect at height ~p (Top = ~p). St = ~p",
                        [SafeHeight, TopHeight, lager:pr(St, ?MODULE)]),
            %% Double-check that the GC hasn't been requested on a microblock.
            %% This would be a bug, since aec_conductor should only ask for keyblocks.
            case aec_headers:type(Header) of
                key ->
                    ?PROTECT(perform_switch(Trees, SafeHeight),
                             fun(Scanners1) ->
                                     {reply, ok, St#st{ scanners = Scanners1
                                                      , last_switch = SafeHeight}}
                             end,
                             signal_switching_failed_and_reply(St, nop));
                micro ->
                    lager:warning("GC called on microblock - ignoring", []),
                    {reply, nop, St}
            end;
       true ->
            {reply, nop, St}
    end;
handle_call({maybe_garbage_collect, _, _}, _From, St) ->
    {reply, nop, St};
handle_call({info, Keys}, _, St) ->
    {reply, info_(Keys, St), St}.

%% the initial scan failed due to hash_not_present_in_db, reschedule it for later
handle_cast({scanning_failed, ErrHeight}, St) ->
    {noreply, St#st{min_height = ErrHeight}};

handle_cast({scan_complete, Name, Height}, #st{scanners = Scanners} = St) ->
    Scanners1 = lists:keydelete(Name, #scanner.tree, Scanners),
    case Scanners1 of
        [] ->
            aec_db:write_last_gc_scan(Height),
            aec_events:publish(gc, scans_complete),
            {noreply, St#st{last_complete_scan = Height}};
        _ ->
            {noreply, St#st{scanners = Scanners1}}
    end;

handle_cast(_, St) ->
    {noreply, St}.

terminate(_Reason, _St) -> ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

safe_height(TopHeight, 0) ->
    case aec_resilience:fork_resistance_active() of
        no -> TopHeight;
        {yes, FRHeight} -> TopHeight - FRHeight - 1
    end;
safe_height(TopHeight, Depth) when Depth > 0 ->
    TopHeight - Depth.

info_(Keys, St) ->
    maps:from_list(
      [{K, info_item(K, St)} || K <- Keys]).

info_item(history, #st{history = H}) ->
    H;
info_item(last_gc, #st{last_switch = L}) ->
    L;
info_item(last_scan, #st{last_complete_scan = L}) ->
    L;
info_item(active_sweeps, #st{scanners = Scanners}) ->
    [#{tree => T, pid => P, height => H} ||
        #scanner{tree = T, height = H, pid = P} <- Scanners];
info_item(during_sync, #st{during_sync = Flag}) ->
    Flag;
info_item(trees, #st{trees = Trees}) ->
    Trees;
info_item(depth, #st{depth = D}) ->
    D;
info_item(enabled, #st{enabled = Bool}) ->
    Bool.

perform_switch(Trees, Height) ->
    clear_secondary_tables(Trees),
    ok = aec_db:ensure_transaction(
           fun() ->
                   switch_tables(Trees),
                   aec_db:write_last_gc_switch(Height)
           end),
    aec_events:publish(gc, {gc_switch, Height}),
    [start_scanner(T, Height) || T <- Trees].

clear_secondary_tables(Trees) ->
    [clear_secondary(T) || T <- Trees].

clear_secondary(Name) ->
    Secondary = aec_db:secondary_state_tab(Name),
    lager:debug("Clearing secondary for ~p (~p)", [Name, Secondary]),
    aec_db:clear_table(Secondary).

switch_tables(Trees) ->
    [switch(T) || T <- Trees].

switch(Name) ->
    Secondary = aec_db:secondary_state_tab(Name),
    lager:debug("Making ~p the primary for ~p", [Secondary, Name]),
    aec_db:make_primary_state_tab(Name, Secondary),
    ok.

start_scanner(Name, Height) ->
    Parent = self(),
    S = spawn_monitor(fun() ->
                              scan_tree(Name, Height, Parent)
                      end),
    #scanner{tree = Name, height = Height, pid = S}.

scan_tree(Name, Height, Parent) ->
    T0 = erlang:system_time(millisecond),
    {ok, Count} = collect_reachable_hashes_fullscan(Name, Height),
    T1 = erlang:system_time(millisecond),
    lager:info("GC fullscan done for ~p, Height = ~p, Count = ~p, Time = ~p ms",
               [Name, Height, Count, T1 - T0]),
    gen_server:cast(Parent, {scan_complete, Name, Height}),
    ok.

collect_reachable_hashes_fullscan(Tree, Height) ->
    case get_mpt(Tree, Height) of
        empty ->
            lager:debug("Tree ~p empty - skipping scan", [Tree]),
            {ok, 0};
        MPT ->
            lager:debug("GC fullscan for ~p at height ~p of accounts with hash ~w",
                 [Tree, Height, aeu_mp_trees:root_hash(MPT)]),
            {ok, visit_reachable(MPT)}
    end.

visit_reachable(MPT) ->
    {_, Count} = aeu_mp_trees:visit_reachable_hashes(MPT, init_acc(0), fun visit_count/3),
    Count.

visit_count(_, _, {Ref, N}) ->
    Acc1 = maybe_ask_jobs({Ref, N+1}),
    {continue, Acc1}.

init_acc(N) ->
    case jobs:ask(?JOBS_QUEUE) of
        {ok, NewRef} ->
            {NewRef, N};
        {error, Reason} ->
            lager:error("Jobs return error: ~p", [Reason]),
            signal_scanning_failed(0),
            error(jobs_error)
    end.

maybe_ask_jobs({OldRef, N} = Acc) ->
    case N rem ?SCAN_SLICE of
        0 ->
            jobs:done(OldRef),
            init_acc(N);
        _ ->
            Acc
    end.

-spec get_mpt(tree_name(), non_neg_integer()) -> aeu_mp_trees:tree() | empty.
get_mpt(Tree, Height) ->
    {ok, Hash} = aec_chain_state:get_key_block_hash_at_height(Height),
    get_mpt_from_hash(Tree, Hash).

get_mpt_from_hash(Tree, Hash) ->
    {value, Trees} = aec_db:find_block_state(Hash, _DirtyBackend = true),
    case tree_hash(Tree, Trees) of
        {error, empty} ->
            empty;
        {ok, RootHash} ->
            aeu_mp_trees:new(RootHash, db(Tree, Trees))
    end.

tree_hash(Tree, Trees) ->
    case Tree of
        accounts      -> aec_trees:accounts_hash(Trees);
        calls         -> aec_trees:calls_hash(Trees);
        contracts     -> aec_trees:contracts_hash(Trees);
        oracles       -> aec_trees:oracles_hash(Trees);
        oracles_cache -> aec_trees:oracles_cache_hash(Trees);
        channels      -> aec_trees:channels_hash(Trees);
        ns            -> aec_trees:ns_hash(Trees);
        ns_cache      -> aec_trees:ns_cache_hash(Trees)
    end.

db(Tree, Trees) ->
    {ok, DB} = case Tree of
                   accounts ->
                       aec_accounts_trees:db(aec_trees:accounts(Trees));
                   calls ->
                       aect_call_state_tree:db(aec_trees:calls(Trees));
                   contracts ->
                       aect_state_tree:db(aec_trees:contracts(Trees));
                   oracles ->
                       aeo_state_tree:oracles_db(aec_trees:oracles(Trees));
                   oracles_cache ->
                       aeo_state_tree:cache_db(aec_trees:oracles(Trees));
                   channels ->
                       aesc_state_tree:db(aec_trees:channels(Trees));
                   ns ->
                       aens_state_tree:ns_db(aec_trees:ns(Trees));
                   ns_cache ->
                       aens_state_tree:cache_db(aec_trees:ns(Trees))
               end,
    DB.

signal_scanning_failed(ErrHeight) ->
    gen_server:cast(?MODULE, {scanning_failed, ErrHeight}).

signal_switching_failed_and_reply(St, Reply) ->
    fun (ErrHeight) ->
            signal_scanning_failed(ErrHeight),
            {reply, Reply, St}
    end.

cache_enabled_status(Bool) when is_boolean(Bool) ->
    persistent_term:put({?MODULE, gc_enabled}, Bool).

get_cached_enabled_status() ->
    persistent_term:get({?MODULE, gc_enabled}).

erase_cached_enabled_status() ->
    persistent_term:erase({?MODULE, gc_enabled}).

config() ->
    Trees = get_trees(),
    %% In the LC below, we rely on the schema to provide default values.
    M = maps:from_list(
          [{Key, ok(aeu_env:find_config([<<"chain">>, <<"garbage_collection">>, Key],
                                        [user_config, schema_default]))} ||
              Key <- [<<"enabled">>,
                      <<"history">>,
                      <<"minimum_height">>,
                      <<"depth">>,
                      <<"during_sync">>]]),
    M#{<<"trees">> => Trees}.

ok({ok, Value}) -> Value.

get_trees() ->
    {ok, Trees} = aeu_env:find_config([<<"chain">>, <<"garbage_collection">>, <<"trees">>],
                                      [user_config, schema_default]),
    expand_trees(Trees).

expand_trees(Trees) ->
    Map = aec_trees:tree_map(),
    binaries_to_atoms(Trees, Map).

binaries_to_atoms(Bins, TreeMap) ->
    binaries_to_atoms(Bins, TreeMap, {[], []}).

binaries_to_atoms([], _, {Good, Bad}) ->
    case Bad of
        [] ->
            lists:reverse(Good);
        _ ->
            error({invalid_tree_names, Bad})
    end;
binaries_to_atoms([B|Bins], TreeMap, {Good, Bad}) ->
    Acc1 = try binary_to_existing_atom(B, utf8) of
               A ->
                   case maps:find(A, TreeMap) of
                       {ok, Subs} ->
                           {[A|Subs] ++ Good, Bad};
                       error ->
                           {Good, [B|Bad]}
                   end
           catch
               error:_ ->
                   {Good, [B|Bad]}
           end,
    binaries_to_atoms(Bins, TreeMap, Acc1).
