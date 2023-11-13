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
        , enable/0
        , disable/0
        ]).

-export([maybe_garbage_collect/3]).

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
-type block_hash() :: aec_blocks:block_header_hash().

-record(scanner,
        { tree   :: tree_name()
        , block_hash :: block_hash()
        , height :: non_neg_integer()
        , pid    :: pid_ref()
        , attempt = 1 :: non_neg_integer() }).

%% message types
-record(scanner_done, { tree, block_hash, height }).
-record(maybe_garbage_collect, {
                                 height :: non_neg_integer()
                               , header :: aec_headers:header()
                               , block_hash :: block_hash()
                               , top_change :: boolean()
                               }).

-type scan() :: #{ height := non_neg_integer()
                 , hashes := [block_hash()] }.

-record(st,
        {enabled        :: boolean(),                     % do we garbage collect?
         history        :: non_neg_integer(),             % how many block state back from top to keep
         min_height     :: undefined | non_neg_integer(), % do not GC before this height
         during_sync    :: boolean(),                     % run GC from the beginning
         synced         :: boolean(),                     % we only run GC if chain is synced
         last_switch = 0 :: non_neg_integer(),            % height at last switch
         current_scan   :: undefined | scan(),            % ongoing scan
         last_complete_scan :: scan(),                    % height of last complete scan
         trees = []     :: [tree_name()],                 % State trees being GC:ed
         scanners = []  :: [#scanner{}]                   % ongoing scans
        }).

-include_lib("aecore/include/aec_db.hrl").

-define(DBG(Fmt, Args), ae_db_gc:debug(Fmt, Args)).
-define(INFO(Fmt, Args), ae_db_gc:info(Fmt, Args)). % io:format(Fmt"~n", Args)
-define(ERR(Fmt, Args), ae_db_gc:error(Fmt, Args)). % io:format("ERROR:"Fmt"~n", Args)

%% -define(PROTECT(Expr, OkFun), ?PROTECT(Expr, OkFun, fun signal_scanning_failed/1)).
-define(PROTECT(Expr, OkFun, ErrHeightFun),
        (try Expr of
             Res -> (OkFun)(Res)
         catch
             error:{hash_not_present_in_db_at_height, ErrHeight, MissingHash, Stack} ->
                 ?ERR("Scanning at height ~p failed due to a missing hash ~p at: ~p",
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

-spec maybe_garbage_collect(Header, Hash, TopChange) -> ok | nop
              when Header :: aec_headers: header()
                 , Hash   :: aec_blocks:block_header_hash()
                 , TopChange :: boolean().
maybe_garbage_collect(Header, Hash, TopChange) when is_boolean(TopChange) ->
    case get_cached_enabled_status() of
        true ->
            gen_server:call(
              ?MODULE, #maybe_garbage_collect{
                          height = aec_headers:height(Header)
                        , header = Header
                        , block_hash = Hash
                        , top_change = TopChange});
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
    [enabled, history, last_gc, last_scan, active_sweeps, during_sync, trees].

enable() ->
    gen_server:call(?MODULE, {set_enabled, true}).

disable() ->
    gen_server:call(?MODULE, {set_enabled, false}).

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
      } = Cfg) when is_integer(History), History > 0 ->
    ?DBG("Cfg = ~p", [Cfg]),
    cache_enabled_status(Enabled),
    {LastSwitch, LastScan} =
        case Enabled of
            true ->
                aec_events:subscribe(chain_sync),
                aec_db:ensure_activity(
                  async_dirty,
                  fun() ->
                          {last_gc_switch(),
                           last_gc_scan()}
                  end);
            false ->
                {0, undefined}
        end,
    ?DBG("LastSwitch = ~p", [LastSwitch]),
    {Scanners, CurScan} = maybe_restart_scanners(LastSwitch, LastScan, Trees),
    %% TODO: Make min_height configurable
    Data = #st{enabled      = Enabled,
               during_sync  = DuringSync,
               trees        = Trees,
               history      = History,
               min_height   = MinHeight,
               last_switch  = LastSwitch,
               current_scan = CurScan,
               last_complete_scan = LastScan,
               scanners     = Scanners,
               synced       = false},
    {ok, Data}.

last_gc_switch() ->
    case aec_db:read_last_gc_switch(undefined) of
        undefined ->
            case aec_chain:top_height() of
                undefined -> 0;
                TopHeight -> TopHeight
            end;
        Height ->
            Height
    end.

last_gc_scan() ->
    %% Used to be stored only as the height of the last complete scanner
    case aec_db:read_last_gc_scan() of
        error ->
            undefined;
        {ok, Height} when is_integer(Height) ->
            case aec_chain_state:get_key_block_hash_at_height(Height) of
                error ->
                    undefined;
                {ok, Hash} ->
                    #{ height => Height
                     , hashes => [Hash] }
            end;
        {ok, #{height := _, hashes := _} = Last} ->
            Last
    end.


maybe_restart_scanners(LastSwitch, LastScan, Trees) ->
    case scan_incomplete_at_switch_height(LastSwitch, LastScan) of
        false ->
            {[], undefined};
        {true, Hashes} ->
            ?INFO("Scan incomplete at height ~p; restarting scan of ~p siblings",
                  [LastSwitch, length(Hashes)]),
            [?INFO("Sibling hash: ~s", [enc_keyblock_hash(H)]) || H <- Hashes],
            {[start_scanner(T, LastSwitch, Hash)
              || T <- Trees,
                 Hash <- Hashes],
             #{height => LastSwitch, hashes => []}}
    end.

scan_incomplete_at_switch_height(0, _) ->
    false;
scan_incomplete_at_switch_height(H, #{height := H, hashes := Hs}) ->
    case siblings_at_height(H) -- Hs of
        [] -> false;
        [_|_] = Unscanned ->
            {true, Unscanned}
    end;
scan_incomplete_at_switch_height(LastSwitch, _) ->
    {true, siblings_at_height(LastSwitch)}.

siblings_at_height(H) ->
    aec_db:ensure_activity(
      async_dirty,
      fun() ->
              aec_chain_state:key_block_hashes_at_height(H)
      end).

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
        #scanner{tree = Tree, height = Height, block_hash = BHash, attempt = Attempt} = S ->
            Scanners1 = Scanners -- [S],
            NewSt = if Reason =/= normal ->
                            ?ERR("GC Scanner process for ~p died: ~p", [Tree, Reason]),
                            if Attempt == 1 ->
                                    NewS = start_scanner(Tree, Height, BHash, Attempt + 1),
                                    St#st{scanners = [NewS | Scanners1]};
                               true ->
                                    disable_gc(St#st{scanners = Scanners1})
                            end;
                       true ->
                            St#st{scanners = Scanners1}
                    end,
            {noreply, NewSt}
    end;

handle_info(_, St) ->
    {noreply, St}.

handle_call(#maybe_garbage_collect{ height = Height
                                  , top_change = false
                                  , block_hash = Hash }, _From,
            #st{ enabled = true, last_switch = Height
               , trees = Trees, scanners = Scanners } = St) ->
    %% A sibling appears at the last_switch height. We need to scan this block
    %% to make sure that all the live state is promoted.
    %%
    ?INFO("Sibling at scan height ~p. Hash: ~s", [Height, enc_keyblock_hash(Hash)]),
    NewScanners = [start_scanner(T, Height, Hash) || T <- Trees],
    %%
    %% The return value here isn't optimal. I guess what we're saying is that
    %% we didn't perform a switch.
    CurScan = case St#st.current_scan of
                  undefined -> #{height => Height, hashes => []};
                  #{height := Height} = Existing ->
                      %% Invariant: Height must not be different from above
                      Existing
              end,
    {reply, nop, St#st{ scanners = Scanners ++ NewScanners
                      , current_scan = CurScan }};
handle_call(#maybe_garbage_collect{ height = TopHeight
                                  , top_change = true   %% new top block
                                  , header = Header
                                  , block_hash = Hash }, _From,
            #st{enabled = true, synced = Synced, during_sync = DuringSync,
                history = History, min_height = MinHeight,
                trees = Trees, scanners = [], last_switch = Last} = St) ->
    if (Synced orelse DuringSync), TopHeight >= MinHeight, TopHeight >= Last + History ->
            ?DBG("WILL collect at height ~p. St = ~p",
                 [TopHeight, lager:pr(St, ?MODULE)]),
            %% Double-check that the GC hasn't been requested on a microblock.
            %% This would be a bug, since aec_conductor should only ask for keyblocks.
            case aec_headers:type(Header) of
                key ->
                    ?PROTECT(perform_switch(Trees, TopHeight, Hash),
                             fun(Scanners1) ->
                                     {reply, ok, St#st{ scanners = Scanners1
                                                      , current_scan = #{height => TopHeight,
                                                                         hashes => []}
                                                      , last_switch = TopHeight}}
                             end,
                             signal_switching_failed_and_reply(St, nop));
                micro ->
                    lager:warning("GC called on microblock - ignoring", []),
                    {reply, nop, St}
            end;
       true ->
            {reply, nop, St}
    end;
handle_call(#maybe_garbage_collect{}, _From, St) ->
    {reply, nop, St};
handle_call({set_enabled, Bool}, _From, #st{enabled = Enabled} = St) ->
    {reply, Enabled, set_gc_enabled(Bool, St)};
handle_call({info, Keys}, _, St) ->
    {reply, info_(Keys, St), St}.

handle_cast({scanning_failed, _ErrHeight}, St) ->
    %% TODO: This used to set min_height to ErrHeight. This seems wrong, but
    %% perhaps there is something else we want to do here.
    {noreply, St};

handle_cast(#scanner_done{tree = Name, height = Height, block_hash = Hash},
            #st{scanners = Scanners} = St) ->
    Scanners1 = 
        [X || #scanner{height = He, block_hash = Ha, tree = T} = X <- Scanners,
              {He, Ha, T} =/= {Height, Hash, Name}],
    St1 = update_current_scan(Hash, Scanners1, St#st{scanners = Scanners1}),
    case Scanners1 of
        [] ->
            ?INFO("All scanners done at height ~p", [Height]),
            #st{current_scan = Current} = St1,
            aec_db:write_last_gc_scan(Current),
            aec_events:publish(gc, scans_complete),
            {noreply, St1#st{ last_complete_scan = Current
                            , current_scan = undefined}};
        _ ->
            {noreply, St1}
    end;

handle_cast(_, St) ->
    {noreply, St}.

terminate(_Reason, _St) -> ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

disable_gc(S) -> set_gc_enabled(false, S).
%% enable_gc(S)  -> set_gc_enabled(true, S).

set_gc_enabled(Bool, #st{enabled = Bool} = S) -> S;
set_gc_enabled(Bool, #st{} = S ) when is_boolean(Bool) ->
    ?INFO("Changing GC enabled state to ~p", [Bool]),
    S#st{enabled = Bool}.

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
info_item(enabled, #st{enabled = Bool}) ->
    Bool.

perform_switch(Trees, Height, Hash) ->
    ?INFO("GC switch at height ~p, Hash: ~s",
          [Height, aeser_api_encoder:encode(key_block_hash, Hash)]),
    clear_secondary_tables(Trees),
    ok = aec_db:ensure_transaction(
           fun() ->
                   switch_tables(Trees),
                   aec_db:write_last_gc_switch(Height)
           end),
    aec_events:publish(gc, {gc_switch, Height}),
    [start_scanner(T, Height, Hash) || T <- Trees].

clear_secondary_tables(Trees) ->
    [clear_secondary(T) || T <- Trees].

clear_secondary(Name) ->
    Secondary = aec_db:secondary_state_tab(Name),
    ?DBG("Clearing secondary for ~p (~p)", [Name, Secondary]),
    aec_db:clear_table(Secondary).

switch_tables(Trees) ->
    [switch(T) || T <- Trees].

switch(Name) ->
    Secondary = aec_db:secondary_state_tab(Name),
    ?DBG("Making ~p the primary for ~p", [Secondary, Name]),
    aec_db:make_primary_state_tab(Name, Secondary),
    ok.

start_scanner(Name, Height, Hash) ->
    start_scanner(Name, Height, Hash, 1).

start_scanner(Name, Height, Hash, Attempt) ->
    Parent = self(),
    S = spawn_monitor(fun() ->
                              scan_tree(Name, Height, Hash, Parent)
                      end),
    #scanner{ tree = Name
            , height = Height
            , block_hash = Hash
            , pid = S
            , attempt = Attempt}.

scan_tree(Name, Height, Hash, Parent) when is_atom(Name)
                                         , is_integer(Height)
                                         , is_binary(Hash)
                                         , is_pid(Parent) ->
    T0 = erlang:system_time(millisecond),
    {ok, Count} = collect_reachable_hashes_fullscan(Name, Height, Hash),
    T1 = erlang:system_time(millisecond),
    ?INFO("GC scan done at ~p (~-13w), Count: ~-9w, Time (ms): ~-8w, hash: ~s",
          [Height, Name, Count, T1 - T0, pp_hash(Hash)]),
    gen_server:cast(Parent, #scanner_done{ tree = Name
                                         , height = Height
                                         , block_hash = Hash }),
    ok.

pp_hash(Hash) when is_binary(Hash) ->
    Enc = enc_keyblock_hash(Hash),
    Start = binary:part(Enc, 0, 8),
    End = binary:part(Enc, byte_size(Enc), -4),
    <<Start/binary, "...", End/binary>>.

enc_keyblock_hash(Hash) ->
    aeser_api_encoder:encode(key_block_hash, Hash).

collect_reachable_hashes_fullscan(Tree, Height, Hash) ->
    case get_mpt(Tree, Hash) of
        empty ->
            ?DBG("Tree ~p empty at height ~p - skipping scan", [Tree, Height]),
            {ok, 0};
        MPT ->
            ?DBG("GC fullscan for ~p at height ~p from root hash ~w",
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
            ?ERR("Jobs return error: ~p", [Reason]),
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

update_current_scan(Hash, Scanners, #st{current_scan = #{hashes := Hashes} = Scan} = St) ->
    case [X || #scanner{block_hash = H} = X <- Scanners,
               H =:= Hash] of
        [] ->
            case lists:member(Hash, Hashes) of
                true ->
                    St;
                false ->
                    St#st{current_scan = Scan#{hashes := Hashes ++ [Hash]}}
            end;
        [_|_] ->
            St
    end.

%% The following, commented, functions were intended to be used to
%% "top up" the state tree, by re-evaluating the transactions of blocks
%% otherwise ignored by the GC scanner (e.g. if the GC is trailing behind
%% the top. We might still want them for state tree repair, if we decide
%% that there is still a need for it.

%% top_up_to_height(Height, ToHeight) ->
%%     lager:debug("Height = ~p, ToHeight = ~p", [Height, ToHeight]),
%%     top_up_to_height(Height, ToHeight, _StartHeight = Height).

%% top_up_to_height(Height, ToHeight, StartHeight) when Height =< ToHeight ->
%%     GenRes = aec_db:ensure_activity(
%%                async_dirty,
%%                fun() ->
%%                        aec_chain:get_generation_by_height(Height, forward)
%%                end),
%%     case GenRes of
%%         {ok, #{key_block := KB, micro_blocks := MBs}} ->
%%             case Height =:= StartHeight of
%%                 true ->
%%                     eval_microblocks(MBs);
%%                 false ->
%%                     eval_keyblock(KB),
%%                     eval_microblocks(MBs)
%%             end,
%%             top_up_to_height(Height + 1, ToHeight, StartHeight);
%%         error ->
%%             error(generation_not_found)
%%     end;
%% top_up_to_height(_, _, _) ->
%%     ok.

%% eval_keyblock(KeyBlock) ->
%%     Res = aec_chain_state:repair_block_state(KeyBlock),
%%     lager:debug("Height = ~p; Res = ~p", [aec_blocks:height(KeyBlock), Res]),
%%     Res.

%% eval_microblocks(MicroBlocks) ->
%%     {L,_} = lists:mapfoldl(fun(B,N) -> {{N, B}, N+1} end, 1, MicroBlocks),
%%     [eval_microblock(B) || B <- L].

%% eval_microblock({N, MicroBlock}) ->
%%     Res = aec_chain_state:repair_block_state(MicroBlock),
%%     lager:debug("Height = ~p, N = ~p, Res = ~p",
%%                 [aec_blocks:height(MicroBlock), N, Res]),
%%     Res.

-spec get_mpt(tree_name(), block_hash()) -> aeu_mp_trees:tree() | empty.
get_mpt(Tree, Hash) ->
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
