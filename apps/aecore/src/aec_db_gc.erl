-module(aec_db_gc).

%% Implementation of Garbage Collection of Account state.
%% To run, the GC needs to be `enabled` (config param), and node synced.
%% If key block `interval` (config parameter) is hit, GC starts collecting
%% of the reachable nodes from most recent generations (`history` config parameter).
%%
%% If #data.enabled and #data.synced are true, #data.hashes indicates the GC status:
%% - undefined    - not the right time to run GC
%% - is pid       - we are scanning reachable nodes
%% - is reference - waiting for signal to execute the `swap_nodes` phase
%%
%% GC scan is performed on the background. The only synchronous part of GC is `swap_nodes` phase.
%% (initiated by aec_conductor on key block boundary)
%%
%% More details can be found here: https://github.com/aeternity/papers/blob/master/Garbage%20collector%20for%20account%20state%20data.pdf

-behaviour(gen_statem).

%% API
-export([start_link/0,
         start_link/3,
         maybe_garbage_collect/0,
         stop/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-record(data,
        {enabled  :: boolean(),                         % do we garbage collect?
         interval :: non_neg_integer(),                 % how often (every `interval` blocks) GC runs
         history  :: non_neg_integer(),                 % how many block state back from top to keep
         synced   :: boolean(),                         % we only run GC if chain is synced
         height   :: undefined | non_neg_integer(),     % latest height of MPT hashes stored in tab
         hashes   :: undefined | pid() | reference()}). % hashes tab (or process filling the tab 1st time)

-include_lib("aecore/include/aec_db.hrl").

-define(DEFAULT_INTERVAL, 50000).
-define(DEFAULT_HISTORY, 500).

-define(TIMED(Expr), timer:tc(fun () -> Expr end)).
-define(LOG(Fmt), lager:info(Fmt, [])).         % io:format(Fmt"~n")
-define(LOG(Fmt, Args), lager:info(Fmt, Args)). % io:format(Fmt"~n", Args)

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    #{enabled := Enabled, interval := Interval, history := History} = config(),
    start_link(Enabled, Interval, History).

start_link(Enabled, Interval, History) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Enabled, Interval, History], []).


%% To avoid starting of the GC process just for EUNIT
-ifdef(EUNIT).
maybe_garbage_collect() -> nop.
-else.

%% This should be called when there are no processes modifying the block state
%% (e.g. aec_conductor on specific places)
maybe_garbage_collect() ->
    gen_statem:call(?MODULE, maybe_garbage_collect).
-endif.

stop() ->
    gen_statem:stop(?MODULE).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% Change of configuration parameters requires restart of the node.
init([Enabled, Interval, History])
  when is_integer(Interval), Interval > 0, is_integer(History), History > 0 ->
    if Enabled ->
            aec_events:subscribe(top_changed),
            aec_events:subscribe(chain_sync);
       true ->
            ok
    end,
    Data = #data{enabled  = Enabled,
                 interval = Interval,
                 history  = History,
                 synced   = false,
                 height   = undefined,
                 hashes   = undefined},
    {ok, idle, Data}.


%% once the chain is synced, there's no way to "unsync"
handle_event(info, {_, chain_sync, #{info := {chain_sync_done, _}}}, idle,
             #data{enabled = true} = Data) ->
    aec_events:unsubscribe(chain_sync),
    {keep_state, Data#data{synced = true}};

%% starting collection when the *interval* matches, and don't have a GC state (hashes = undefined)
handle_event(info, {_, top_changed, #{info := #{height := Height}}}, idle,
             #data{interval = Interval, history = History,
                   enabled = true, synced = true,
                   height = undefined, hashes = undefined} = Data)
  when Height rem Interval == 0 ->
    Top = self(),
    Pid = spawn_link(
            fun () ->
                    FromHeight = max(Height - History, 0),
                    {Time, {ok, Hashes}} = ?TIMED(collect_reachable_hashes(FromHeight, Height)),
                    ets:give_away(Hashes, Top, {{FromHeight, Height}, Time})
            end),
    {keep_state, Data#data{height = Height, hashes = Pid}};

%% received GC state from the phase above
handle_event(info, {'ETS-TRANSFER', Hashes, _, {{FromHeight, ToHeight}, Time}}, idle,
             #data{enabled = true, hashes = Pid} = Data)
  when is_pid(Pid) ->
    ?LOG("scanning of ~p reachable hashes in range <~p, ~p> took ~p seconds",
         [ets:info(Hashes, size), FromHeight, ToHeight, Time / 1000000]),
    {next_state, ready, Data#data{hashes = Hashes}};

%% with valid GC state (reachable hashes in ETS cache), follow up on keeping that cache
%% synchronized with Merkle-Patricia Trie on disk keeping the latest changes in accounts
handle_event(info, {_, top_changed, #{info := #{block_type := key, height := Height}}}, ready,
             #data{enabled = true, synced = true, height = LastHeight, hashes = Hashes} = Data)
  when is_reference(Hashes), Height > LastHeight ->
    {ok, _} = range_collect_reachable_hashes(Height, Data),
    {keep_state, Data#data{height = Height}};

%% with valid GC state, if we are on key block boundary, we can
%% clear the table and insert reachable hashes back
handle_event({call, From}, maybe_garbage_collect, ready,
             #data{enabled = true, synced = true, hashes = Hashes} = Data)
  when Hashes /= undefined, not is_pid(Hashes) ->
    Header = aec_chain:top_header(),
    case aec_headers:type(Header) of
        key ->
            Height  = aec_headers:height(Header),
            {ok, _} = range_collect_reachable_hashes(Height, Data),
            {ok, N} = swap_nodes(Hashes),
            ets:tab2file(Hashes, "/tmp/gc-state-" ++ integer_to_list(Height) ++ ".bin"),
            ets:delete(Hashes),
            {next_state, idle, Data#data{height = undefined, hashes = undefined},
             {reply, From, {ok, N}}};
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
    {ok, Hashes} = collect_reachable_hashes_fullscan(FromHeight),
    {ok, Hashes} = range_collect_reachable_hashes(FromHeight, ToHeight, Hashes),
    {ok, Hashes}.

collect_reachable_hashes_fullscan(Height) ->
    Tab = ets:new(gc_reachable_hashes, [public]), %% TODO: remove public
    MPT = get_mpt(Height),
    ?LOG("scanning accounts full tree with root hash ~w at height = ~p~n",
         [aeu_mp_trees:root_hash(MPT), Height]),
    {ok, aeu_mp_trees:visit_reachable_hashes(MPT, Tab, fun store_hash/3)}.

%% assumes Height - 1, Height - 2, ... down to Height - History
%% are in Hashes from previous runs
collect_reachable_hashes_delta(Height, Hashes) ->
    MPT = get_mpt(Height),
    ?LOG("scanning accounts changes with root hash ~w at height = ~p~n",
         [aeu_mp_trees:root_hash(MPT), Height]),
    {ok, aeu_mp_trees:visit_reachable_hashes(MPT, Hashes, fun store_unseen_hash/3)}.

range_collect_reachable_hashes(ToHeight, #data{height = LastHeight, hashes = Hashes}) ->
    range_collect_reachable_hashes(LastHeight, ToHeight, Hashes).
range_collect_reachable_hashes(LastHeight, ToHeight, Hashes) ->
    [collect_reachable_hashes_delta(H, Hashes) || H <- lists:seq(LastHeight + 1, ToHeight)],
    {ok, Hashes}.


%% the actual GC: clear whole table + insert reachable nodes
swap_nodes(Hashes) ->
    NodesCount = ets:info(Hashes, size),
    {ClearTime, {atomic, ok}} = ?TIMED(mnesia:clear_table(aec_account_state)),
    ?LOG("clearing accounts table took ~p seconds", [ClearTime / 1000000]),
    ?LOG("writing ~p reachable account state nodes...", [NodesCount]),
    {WriteTime, ok} =
        ?TIMED(aec_db:ensure_transaction(
                 fun () ->
                         ets:foldl(
                           fun ({Hash, Node}, ok) ->
                                   aec_db:write_accounts_node(Hash, Node)
                           end, ok, Hashes)
                 end, [], sync_transaction)),
    ?LOG("writing reachable account state nodes took ~p seconds", [WriteTime / 1000000]),
    {atomic, DBCount} = mnesia:transaction(fun () -> length(mnesia:all_keys(aec_account_state)) end),
    ?LOG("DB has ~p aec_account_state records", [DBCount]),
    {ok, NodesCount}.


-spec get_mpt(non_neg_integer()) -> aeu_mp_trees:tree().
get_mpt(Height) ->
    {ok, Hash0} = aec_chain_state:get_key_block_hash_at_height(Height),
    {ok, Trees} = aec_chain:get_block_state(Hash0),
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

config() ->
    maps:from_list(
      [{binary_to_atom(Key, utf8),
        aeu_env:user_config([<<"chain">>, <<"garbage_collection">>, Key], Default)} ||
          {Key, Default} <- [{<<"enabled">>, false},
                             {<<"interval">>, ?DEFAULT_INTERVAL},
                             {<<"history">>, ?DEFAULT_HISTORY}]]).
