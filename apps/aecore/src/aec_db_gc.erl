-module(aec_db_gc).

-behaviour(gen_statem).

%% API
-export([start_link/0,
         start_link/4,
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
-define(DEFAULT_CHUNK_SIZE, 10000).

-define(TIMED(Expr), timer:tc(fun () -> Expr end)).
-define(LOG(Fmt), lager:info(Fmt, [])).         % io:format(Fmt"~n")
-define(LOG(Fmt, Args), lager:info(Fmt, Args)). % io:format(Fmt"~n", Args)

%% %%%===================================================================
%% %%% API
%% %%%===================================================================

start_link() ->
    #{ enabled := Enabled
     , interval := Interval
     , history := History
     , chunk_size := ChunkSize } = config(),
    start_link(Enabled, Interval, History, ChunkSize).

start_link(Enabled, Interval, History, ChunkSize) ->
    Params = [Enabled, Interval, History, ChunkSize],
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Params, []).

-ifdef(EUNIT).
maybe_garbage_collect() -> nop.
-else.
%% this should be called when there are no processes modifying the block state
%% (e.g. aec_conductor on specific places)
maybe_garbage_collect() ->
    gen_statem:call(?MODULE, maybe_garbage_collect).
-endif.

stop() ->
    gen_statem:stop(?MODULE).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([Enabled, Interval, History, ChunkSize]) ->
    if Enabled ->
            aec_events:subscribe(top_changed),
            aec_events:subscribe(chain_sync);
       true ->
            ok
    end,
    Data = #data{ enabled    = Enabled
                , interval   = Interval
                , history    = History
                , chunk_size = ChunkSize
                , synced     = false
                , height     = undefined
                , hashes     = undefined },
    {ok, idle, Data}.

%% once the chain is synced, there's no way to "unsync"
handle_event(info, {_, chain_sync, #{info := {chain_sync_done, _}}}, idle,
             #data{enabled = true} = Data) ->
    aec_events:unsubscribe(chain_sync),
    {keep_state, Data#data{synced = true}};
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
handle_event(info, {'ETS-TRANSFER', Hashes, _, {{FromHeight, ToHeight}, Time}}, idle,
             #data{enabled = true, hashes = Pid} = Data)
  when is_pid(Pid) ->
    ?LOG("scanning of ~p reachable hashes in range <~p, ~p> took ~p seconds",
         [ets:info(Hashes, size), FromHeight, ToHeight, Time / 1000000]),
    {next_state, ready, Data#data{hashes = Hashes}};

handle_event(info, {_, top_changed, #{info := #{block_type := key, height := Height}}}, ready,
             #data{enabled = true, synced = true, height = LastHeight, hashes = Hashes} = Data)
  when is_reference(Hashes), Height > LastHeight ->
    {Time, {ok, _}} = ?TIMED(range_collect_reachable_hashes(Height, Data)),
    lager:debug("Updating reachable hashes after top changed took ~p seconds", [Time / 1000000]),
    {keep_state, Data#data{height = Height}};

handle_event({call, From}, maybe_garbage_collect, ready,
             #data{enabled = true, synced = true, hashes = Hashes} = Data)
  when Hashes /= undefined, not is_pid(Hashes) ->
    #data{height = Height, chunk_size = ChunkSize} = Data,
    Header = aec_chain:top_header(),
    HeaderHeight = aec_headers:height(Header),
    HeaderType = aec_headers:type(Header),
    lager:debug("Maybe running GC at height = ~p, with top header type = ~p and top height = ~p",
                [Height, HeaderType, HeaderHeight]),
    case HeaderType of
        key ->
            {Time, {ok, _}} = ?TIMED(range_collect_reachable_hashes(HeaderHeight, Data)),
            lager:debug("Updating reachable hashes before GC took ~p seconds", [Time / 1000000]),
            case partially_delete_nodes(ChunkSize, Hashes) of
              done ->
                {keep_state, Data#data{height=undefined, hashes=undefined}, {reply, From, ok}};
              ok ->
                % we keep garbage collecting until we are done
                spawn(fun() -> maybe_garbage_collect() end),
                {keep_state, Data, {reply, From, ok}}
            end;
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

partially_delete_nodes(Count, Hashes) ->
    Objects = ets:tab2list(Hashes),
    {WriteTime, Res} = ?TIMED(aec_db:delete_partial_accounts_nodes(Count, Objects)),
    ?LOG("GC partially deleting ~p unreachable account state nodes took ~p seconds", [Count, WriteTime / 1000000]),
    Res.

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
  Defaults = [ {<<"enabled">>, false}
             , {<<"interval">>, ?DEFAULT_INTERVAL}
             , {<<"history">>, ?DEFAULT_HISTORY}
             , {<<"chunk_size">>, ?DEFAULT_CHUNK_SIZE}
             ]],
  Config = lists:map(
             fun({Key, Default}) ->
                 Key1 = binary_to_atom(Key, utf8),
                 Value = aeu_env:user_config([<<"chain">>, <<"garbage_collection">>, Key], Default),
                 {Key1, Value}
             end, Defaults),
  maps:from_list(Config).
