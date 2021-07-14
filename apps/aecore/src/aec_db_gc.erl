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
%%
%% If uses wishes to keep GC off by default (as is in default config) but invoke it manually,
%% that is possible with calling of aec_db_gc:run() or aec_db_gc:run(HistoryLength).

-behaviour(gen_statem).

%% API
-export([start_link/0,
         start_link/3,
         run/0,
         run/1,
         stop/0]).

%% for internal use only
-export([maybe_swap_nodes/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-export([config/0]).

-record(data,
        {enabled    :: boolean(),                         % do we garbage collect?
         interval   :: non_neg_integer(),                 % how often (every `interval` blocks) GC runs
         history    :: non_neg_integer(),                 % how many block state back from top to keep
         min_height :: undefined | non_neg_integer(),     % if hash_not_found error, try GC from this height
         synced     :: boolean(),                         % we only run GC (repeatedly) if chain is synced
         height     :: undefined | non_neg_integer(),     % latest height of MPT hashes stored in tab
         hashes     :: undefined | pid() | reference()}). % hashes tab (or process filling the tab 1st time)

-include_lib("aecore/include/aec_db.hrl").

-define(DEFAULT_HISTORY, 500).
-define(DEFAULT_INTERVAL, 50000).

%% interval from config +- random offset to avoid situation where large
%% majority of nodes become unresponsive due to node restart invoked by GC
-define(INTERVAL_VARIANCE_PERCENT, 10).
-define(INTERVAL_VARIANCE, true). % comment this out for development only!

-define(GCED_TABLE_NAME, aec_account_state_gced).
-define(TABLE_NAME, aec_account_state).

-define(TIMED(Expr), timer:tc(fun () -> Expr end)).
-define(LOG(Fmt), lager:info(Fmt, [])).         % io:format(Fmt"~n")
-define(LOG(Fmt, Args), lager:info(Fmt, Args)). % io:format(Fmt"~n", Args)
-define(LOGERR(Fmt, Args), lager:error(Fmt, Args)). % io:format("ERROR:"Fmt"~n", Args)

-define(PROTECT(Expr, OkFun), ?PROTECT(Expr, OkFun, fun signal_scanning_failed/1)).
-define(PROTECT(Expr, OkFun, ErrHeightFun),
        (try Expr of
             Res -> (OkFun)(Res)
         catch
             error:{hash_not_present_in_db_at_height, ErrHeight, MissingHash, Stack} ->
                 ?LOGERR("Scanning at height ~p failed due to a missing hash ~p at: ~p",
                         [ErrHeight, MissingHash, Stack]),
                 (ErrHeightFun)(ErrHeight)
         end)).

%%%===================================================================
%%% API
%%%===================================================================

%% We don't support reconfiguration of config parameters when the GC is already up,
%% doesn't seem to have practical utility.
start_link() ->
    #{enabled := Enabled, interval := Interval, history := History} = config(),
    start_link(Enabled, Interval, History).

start_link(Enabled, Interval, History) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Enabled, Interval, History], []).

run() ->
    #{history := History} = config(),
    run(History).

run(History) when is_integer(History), History > 0 ->
    gen_statem:call(?MODULE, {run, History}).

stop() ->
    gen_statem:stop(?MODULE).

%% called from aec_db on startup
maybe_swap_nodes() ->
    maybe_swap_nodes(?GCED_TABLE_NAME, ?TABLE_NAME).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% Change of configuration parameters requires restart of the node.
init([Enabled, Interval, History])
  when is_integer(Interval), Interval > 0, is_integer(History), History > 0 ->
    Interval1 =
        if Enabled ->
                aec_events:subscribe(top_changed),
                aec_events:subscribe(chain_sync),
                interval(Interval);
           true ->
                Interval
        end,
    Data = #data{enabled    = Enabled,
                 interval   = Interval1,
                 history    = History,
                 min_height = undefined,
                 synced     = false,
                 height     = undefined,
                 hashes     = undefined},
    {ok, idle, Data}.


handle_event({call, From}, {run, History}, idle, #data{enabled = false} = Data) ->
    aec_events:subscribe(top_changed),
    Data1 = Data#data{synced = true, enabled = true, interval = 1, history = History},
    {keep_state, Data1, {reply, From, {ok, run_next_scan_height(Data1)}}};

handle_event({call, From}, {run, History}, idle, #data{history = History, hashes = Hashes} = Data)
  when is_pid(Hashes) -> % initial scanner pid already runs with the same history length, keep it
    {keep_state, Data, {reply, From, {ok, run_next_scan_height(Data)}}};
handle_event({call, From}, {run, History}, idle, #data{hashes = Hashes} = Data) ->
    is_pid(Hashes) andalso exit(Hashes, kill),
    Data1 = Data#data{synced = true, interval = 1, history = History,
                      height = undefined, hashes = undefined},
    {keep_state, Data1, {reply, From, {ok, run_next_scan_height(Data1)}}};

handle_event({call, From}, {run, History}, ready, #data{history = History} = Data) -> % same history length, keep it
    {keep_state, Data, {reply, From, {ok, run_next_scan_height(Data)}}};
handle_event({call, From}, {run, History}, ready, #data{} = Data) ->
    Data1 = Data#data{interval = 1, history = History,
                      height = undefined, hashes = undefined},
    {next_state, idle, Data1, {reply, From, {ok, run_next_scan_height(Data1)}}};

%% once the chain is synced, there's no way to "unsync"
handle_event(info, {_, chain_sync, #{info := {chain_sync_done, _}}}, idle,
             #data{enabled = true} = Data) ->
    catch aec_events:unsubscribe(chain_sync),
    {keep_state, Data#data{synced = true}};

%% starting collection when the *interval* matches, and don't have a GC state (hashes = undefined)
%% OR some MPT was missing previosly so we try again later
handle_event(info, {_, top_changed, #{info := #{height := Height}}}, idle,
             #data{interval = Interval, history = History,
                   enabled = true, synced = true, min_height = MinHeight,
                   height = undefined, hashes = undefined} = Data)
  when ((Height rem Interval) == 0 andalso MinHeight == undefined) orelse
       (is_integer(MinHeight) andalso Height - History > MinHeight) ->
    Parent = self(),
    Pid = spawn_link(
            fun () ->
                    FromHeight = max(Height - History, 0),
                    ?PROTECT(?TIMED(collect_reachable_hashes(FromHeight, Height)),
                             fun ({Time, {ok, Hashes}}) ->
                                     ets:give_away(Hashes, Parent, {{FromHeight, Height}, Time})

                             end)
            end),
    {keep_state, Data#data{height = Height, hashes = Pid}};

%% the initial scan failed due to hash_not_present_in_db, reschedule it for later
handle_event(info, {scanning_failed, ErrHeight}, idle,
             #data{enabled = true, hashes = Pid} = Data)
  when is_pid(Pid) ->
    {keep_state, Data#data{height = undefined,
                           hashes = undefined,
                           min_height = ErrHeight}};
%% later incremental scan failed due to hash_not_present_in_db, reschedule it for later
handle_event(info, {scanning_failed, ErrHeight}, ready, #data{enabled = true} = Data) ->
    {next_state, idle, Data#data{height = undefined,
                                 hashes = undefined,
                                 min_height = ErrHeight}};

%% happens when scanning process is killed after it transfers hashes table, we ignore it
%% (when we manually invoke GC via 'run')
handle_event(info, {'ETS-TRANSFER', _, _, _}, idle,
             #data{enabled = true, hashes = undefined} = Data) ->
    {keep_state, Data};

%% received GC state from the phase above
handle_event(info, {'ETS-TRANSFER', Hashes, _, {{FromHeight, ToHeight}, Time}}, idle,
             #data{enabled = true, hashes = Pid} = Data)
  when is_pid(Pid) ->
    ?LOG("Scanning of ~p reachable hashes in range <~p, ~p> took ~p seconds",
         [ets:info(Hashes, size), FromHeight, ToHeight, Time / 1000000]),
    {next_state, ready, Data#data{hashes = Hashes}};

%% with valid GC state (reachable hashes in ETS cache), follow up on keeping that cache
%% synchronized with Merkle-Patricia Trie on disk keeping the latest changes in accounts
handle_event(info, {_, top_changed, #{info := #{block_type := key, height := Height}}}, ready,
             #data{enabled = true, synced = true, height = LastHeight, hashes = Hashes} = Data)
  when is_reference(Hashes) ->
    if Height > LastHeight ->
            ?PROTECT(range_collect_reachable_hashes(Height, Data),
                     fun ({ok, _}) -> {keep_state, Data#data{height = Height}} end,
                     signal_scanning_failed_keep_state(Data));
       true ->
            %% in case previous key block was a fork, we can receive top_changed event
            %% with the same or lower height as seen last time
            ?PROTECT(collect_reachable_hashes_delta(Height, Hashes),
                     fun ({ok, _}) -> {keep_state, Data} end,
                     (signal_scanning_failed_keep_state(Data)))
    end;

%% with valid GC state, if we are on key block boundary, we can
%% clear the table and insert reachable hashes back
handle_event({call, _From}, maybe_garbage_collect, ready,
             #data{enabled = true, synced = true, hashes = Hashes} = Data)
  when Hashes /= undefined, not is_pid(Hashes) ->
    Header = aec_chain:top_header(),
    case aec_headers:type(Header) of
        key ->
            Height  = aec_headers:height(Header),
            ?PROTECT(range_collect_reachable_hashes(Height, Data),
                     %% we exit here as GCEd table is swapped at startup
                     fun ({ok, _}) -> store_cache_and_restart(Hashes, ?GCED_TABLE_NAME) end,
                     signal_scanning_failed_keep_state(Data));
        micro ->
            {keep_state, Data}
    end;
handle_event({call, From}, maybe_garbage_collect, _, Data) ->
    {keep_state, Data, {reply, From, nop}};

handle_event(_, _, _, Data) ->
    {keep_state, Data}.


terminate(_Reason, _State, _Data) -> void.

code_change(_, State, Data, _) -> {ok, State, Data}.

callback_mode() -> handle_event_function.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% From - To (inclusive)
collect_reachable_hashes(FromHeight, ToHeight) when FromHeight < ToHeight ->
    {ok, Hashes} = collect_reachable_hashes_fullscan(FromHeight),     % table is created here
    {ok, Hashes} = range_collect_reachable_hashes(FromHeight, ToHeight, Hashes), % and reused
    {ok, Hashes}.

collect_reachable_hashes_fullscan(Height) ->
    Tab = ets:new(gc_reachable_hashes, [public]),
    MPT = get_mpt(Height),
    ?LOG("GC fullscan at height ~p of accounts with hash ~w",
         [Height, aeu_mp_trees:root_hash(MPT)]),
    {ok, aeu_mp_trees:visit_reachable_hashes(MPT, Tab, fun store_hash/3)}.

%% assumes Height - 1, Height - 2, ... down to Height - History
%% are in Hashes from previous runs
collect_reachable_hashes_delta(Height, Hashes) ->
    MPT = get_mpt(Height),
    ?LOG("GC diffscan at height ~p of accounts with hash ~w",
         [Height, aeu_mp_trees:root_hash(MPT)]),
    {ok, aeu_mp_trees:visit_reachable_hashes(MPT, Hashes, fun store_unseen_hash/3)}.

range_collect_reachable_hashes(ToHeight, #data{height = LastHeight, hashes = Hashes}) ->
    range_collect_reachable_hashes(LastHeight, ToHeight, Hashes).
range_collect_reachable_hashes(LastHeight, ToHeight, Hashes) ->
    [collect_reachable_hashes_delta(H, Hashes) || H <- lists:seq(LastHeight + 1, ToHeight)],
    {ok, Hashes}.

store_cache_and_restart(Hashes, GCedTab) ->
    {atomic, ok} = create_accounts_table(GCedTab),
    {ok, _Count} = store_cache(Hashes, GCedTab),
    supervisor:terminate_child(aec_conductor_sup, aec_conductor),
    init:restart(),
    sys:suspend(self(), infinity).

create_accounts_table(Name) ->
    Rec = ?TABLE_NAME,
    Fields = record_info(fields, ?TABLE_NAME),
    mnesia:create_table(Name, aec_db:tab(aec_db:backend_mode(), Rec, Fields, [{record_name, Rec}])).


iter(Fun, ets, Tab) ->
    ets:foldl(fun ({Hash, Node}, ok) -> Fun(Hash, Node), ok end, ok, Tab);
iter(Fun, mnesia, Tab) ->
    mnesia:foldl(fun (X, ok) -> Fun(element(2, X), element(3, X)), ok end, ok, Tab).


write_nodes(SrcMod, SrcTab, TgtTab) ->
    ?TIMED(aec_db:ensure_transaction(
             fun () ->
                     iter(fun (Hash, Node) ->
                                  aec_db:write_accounts_node(TgtTab, Hash, Node)
                          end, SrcMod, SrcTab)
             end, sync_transaction)).

store_cache(SrcHashes, TgtTab) ->
    NodesCount = ets:info(SrcHashes, size),
    ?LOG("Writing ~p reachable account nodes to table ~p ...", [NodesCount, TgtTab]),
    {WriteTime, ok} = write_nodes(ets, SrcHashes, TgtTab),
    ?LOG("Writing reachable account nodes took ~p seconds", [WriteTime / 1000000]),
    DBCount = length(mnesia:dirty_select(TgtTab, [{'_', [], [1]}])),
    ?LOG("GC cache has ~p aec_account_state records", [DBCount]),
    {ok, NodesCount}.

maybe_swap_nodes(SrcTab, TgtTab) ->
    try mnesia:dirty_first(SrcTab) of % table exists
        H when is_binary(H) ->        % and is non empty
            swap_nodes(SrcTab, TgtTab);
        '$end_of_table' ->
            mnesia:delete_table(SrcTab)
    catch
        exit:{aborted,{no_exists,[_]}} ->
            ok
    end.

swap_nodes(SrcTab, TgtTab) ->
    ?LOG("Clearing table ~p ...", [TgtTab]),
    {atomic, ok} = mnesia:clear_table(TgtTab),
    ?LOG("Writing garbage collected accounts ..."),
    {WriteTime, ok} = write_nodes(mnesia, SrcTab, TgtTab),
    ?LOG("Writing garbage collected accounts took ~p seconds", [WriteTime / 1000000]),
    DBCount = length(mnesia:dirty_select(TgtTab, [{'_', [], [1]}])),
    ?LOG("DB has ~p aec_account_state records", [DBCount]),
    ?LOG("Removing garbage collected table ~p ...", [SrcTab]),
    mnesia:delete_table(SrcTab),
    ok.

-spec get_mpt(non_neg_integer()) -> aeu_mp_trees:tree().
get_mpt(Height) ->
    {ok, Hash} = aec_chain_state:get_key_block_hash_at_height(Height),
    try get_mpt_from_hash(Hash) of
        MPT -> MPT
    catch
        error:{hash_not_present_in_db, MissingHash} ->
            Stacktrace = erlang:get_stacktrace(),
            error({hash_not_present_in_db_at_height, Height, MissingHash, Stacktrace})
    end.

get_mpt_from_hash(Hash) ->
    {ok, Trees} = aec_chain:get_block_state(Hash),
    AccountTree = aec_trees:accounts(Trees),
    {ok, RootHash} = aec_accounts_trees:root_hash(AccountTree),
    {ok, DB}       = aec_accounts_trees:db(AccountTree),
    aeu_mp_trees:new(RootHash, DB).


store_hash(Hash, Node, Tab) ->
    ets:insert_new(Tab, {Hash, Node}),
    {continue, Tab}.
store_unseen_hash(Hash, Node, Tab) ->
    case ets:lookup(Tab, Hash) of
        [_] -> stop;
        []  -> store_hash(Hash, Node, Tab)
    end.

signal_scanning_failed(ErrHeight) ->
    ?MODULE ! {scanning_failed, ErrHeight}.

signal_scanning_failed_keep_state(Data) ->
    fun (ErrHeight) ->
            signal_scanning_failed(ErrHeight),
            {keep_state, Data}
    end.

run_next_scan_height(#data{min_height = undefined}) ->
    aec_headers:height(aec_chain:top_header()) + 1;
run_next_scan_height(#data{min_height = MinHeight, history = History})
  when is_integer(MinHeight) ->
    MinHeight + History + 1.

-ifdef(TEST).
interval(ConfigInterval) -> ConfigInterval. % for common test
-else.

-ifdef(INTERVAL_VARIANCE).
interval(ConfigInterval) ->
    case trunc(ConfigInterval * ?INTERVAL_VARIANCE_PERCENT / 100.0) of
        0 ->
            ConfigInterval;
        Variance when Variance > 0 ->
            Delta = rand:uniform(Variance) - (Variance div 2),
            max(3, ConfigInterval + Delta)
    end.
-else.
interval(ConfigInterval) -> ConfigInterval.
-endif.

-endif. %% ifdef TEST

config() ->
    maps:from_list(
      [{binary_to_atom(Key, utf8),
        aeu_env:user_config([<<"chain">>, <<"garbage_collection">>, Key], Default)} ||
          {Key, Default} <- [{<<"enabled">>, false},
                             {<<"interval">>, ?DEFAULT_INTERVAL},
                             {<<"history">>, ?DEFAULT_HISTORY}]]).
