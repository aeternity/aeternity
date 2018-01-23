-module(aec_db).

-export([check_db/0,           % called from setup hook
         load_database/0,      % called in aecore app start phase
         tables/1]).           % for e.g. test database setup

-export([transaction/1,
         write/2,
         delete/2,
         read/2]).

%% Mimicking the aec_persistence API used by aec_conductor_chain
-export([get_chain/0,
         write_block/1,
         write_header/1,
         write_block_state/2,
         write_top_block/1,
         write_top_header/1,
         get_block/1,
         get_header/1,
         get_top_block/0,
         get_top_header/0,
         get_block_state/1]).

-include("common.hrl").
-include("blocks.hrl").

%% - transactions
%% - headers
%% - block  [transaction_ids]
%% - oracle_state
%% - account_state
%% - name_service_state
%% - one per state tree

-record(aec_blocks             , {key, value}).
-record(aec_headers            , {key, value}).
-record(aec_chain_state        , {key, value}).
-record(aec_block_state        , {key, value}).
-record(aec_oracle_state       , {key, value}).
-record(aec_account_state      , {key, value}).
-record(aec_name_service_state , {key, value}).

-define(TAB(Record), {Record, set(Mode, record_info(fields, Record))}).

tables(Mode) ->
    [?TAB(aec_blocks)
   , ?TAB(aec_headers)
   , ?TAB(aec_chain_state)
   , ?TAB(aec_block_state)
   , ?TAB(aec_oracle_state)
   , ?TAB(aec_account_state)
   , ?TAB(aec_name_service_state)
    ].


transaction(Fun) when is_function(Fun, 0) ->
    mnesia:activity(transaction, Fun).

read(Tab, Key) ->
    mnesia:read(Tab, Key).

write(Tab, Obj) ->
    mnesia:write(Tab, Obj, write).

delete(Tab, Key) ->
    mnesia:delete(Tab, Key, write).

%% old-style chain_state initialization API

%% start a transaction if there isn't already one
-define(t(Expr), case get(mnesia_activity_state) of undefined ->
                         transaction(fun() -> Expr end);
                     _ -> Expr
                 end).

get_chain() ->
    ?t(mnesia:select(
         aec_headers, [{ #aec_headers{value = '$1', _ = '_'},
                         [], ['$1'] }], read)
       ++ mnesia:select(
            aec_blocks, [{ #aec_blocks{value = '$1', _ = '_'},
                           [], ['$1'] }], read)).

write_block(Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    ?t(mnesia:write(#aec_blocks{key = Hash, value = Block})).

write_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    ?t(mnesia:write(#aec_headers{key = Hash, value = Header})).

get_block(Hash) ->
    ?t(begin
           [#aec_blocks{value = Block}] =
               mnesia:read(aec_blocks, Hash),
           Block
       end).

get_header(Hash) ->
    ?t(begin
           [#aec_headers{value = Header}] =
               mnesia:read(aec_headers, Hash),
           Header
       end).

write_block_state(Hash, Trees) ->
    ?t(mnesia:write(#aec_block_state{key = Hash, value = Trees})).

write_top_block(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = top_block_hash, value = Hash})).

write_top_header(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = top_header_hash, value = Hash})).

get_top_block() ->
    get_chain_state_value(top_block_hash).

get_top_header() ->
    get_chain_state_value(top_header_hash).

get_block_state(Hash) ->
    ?t(begin
           [#aec_block_state{value = Trees}] =
               mnesia:read(aec_block_state, Hash),
           Trees
       end).

get_chain_state_value(Key) ->
    ?t(case mnesia:read(aec_chain_state, Key) of
           [#aec_chain_state{value = Value}] ->
               Value;
           _ ->
               undefined
       end).

%% start phase hook to load the database (maybe import from persistence)

load_database() ->
    wait_for_tables(),
    maybe_import_data().

wait_for_tables() ->
    Tabs = mnesia:system_info(tables) -- [schema],
    case wait_for_tables(Tabs, 0, _TimePeriods = 5, _MaxWait = 60) of
        ok -> ok;
        {timeout, Mins, NotLoaded} ->
            lager:error("Tables not loaded after ~p minutes: ~p",
                        [Mins, NotLoaded]),
            init:stop()
    end.

wait_for_tables(Tabs, Sofar, Period, Max) when Sofar < Max ->
    case mnesia:wait_for_tables(Tabs, timer:minutes(Period)) of
        ok ->
            ok;
        {timeout, NotLoaded} ->
            lager:warning("Tables not loaded after ~p minutes: ~p",
                          [Period, NotLoaded]),
            wait_for_tables(NotLoaded, Sofar + Period, Period, Max)
    end;
wait_for_tables(Tabs, Sofar, _, _) ->
    {timeout, Sofar, Tabs}.

maybe_import_data() ->
    case aec_persistence:get_chain() of
        [] ->
            ok;
        Chain ->
            Hash = aec_persistence:get_top_block(),
            TopState = aec_persistence:get_block_state(Hash),
            InitTrees = [{Hash, TopState}],
            ChainState = aec_chain_state:new_from_persistence(Chain, InitTrees),
            transaction(fun() -> persist_chain(ChainState) end),
            aec_persistence:remove_files(),
            ok
    end.

persist_chain(ChainState) ->
    Trees = aec_chain_state:get_state_trees_for_persistence(ChainState),
    aec_chain_state:fold_blocks(
      fun(Hash, Block, _) ->
              mnesia:write(#aec_blocks{key = Hash, value = Block}),
              ok
      end, ok, ChainState),
    aec_chain_state:fold_headers(
      fun(Hash, Hdr, _) ->
              mnesia:write(#aec_headers{key = Hash, value = Hdr}),
              ok
      end, ok, ChainState),
    lists:foreach(
      fun({Hash, BTrees}) ->
              mnesia:write(#aec_block_state{key = Hash, value = BTrees})
      end, Trees),
    TopHeaderHash = aec_chain_state:top_header_hash(ChainState),
    TopBlockHash  = aec_chain_state:top_block_hash(ChainState),
    mnesia:write(#aec_chain_state{key = top_header_hash,
                                  value = TopHeaderHash}),
    mnesia:write(#aec_chain_state{key = top_block_hash,
                                  value = TopBlockHash}).

%% Initialization routines

check_db() ->
    try
        Mode = case application:get_env(aecore, persist, false) of
                   true  -> disc;
                   false -> ram
               end,
        ensure_schema_storage_mode(Mode),
        ok = application:ensure_started(mnesia),
        ensure_mnesia_tables(Mode),
        ok
    catch
        error:Reason ->
            lager:error("CAUGHT error:~p / ~p",
                        [Reason, erlang:get_stacktrace()]),
            error(Reason)
    end.

ensure_mnesia_tables(Mode) ->
    Tabs = mnesia:system_info(tables),
    [{atomic,ok} = mnesia:create_table(T, Spec)
     || {T, Spec} <- tables(Mode),
        not lists:member(T, Tabs)],
    ok.


set(Mode, Attrs) ->
    [copies(Mode), {type, set}, {attributes, Attrs}].

copies(disc) -> {disc_copies, [node()]};
copies(ram ) -> {ram_copies , [node()]}.

ensure_schema_storage_mode(ram) ->
    ok = mnesia:delete_schema([node()]);
ensure_schema_storage_mode(disc) ->
    case mnesia:create_schema([node()]) of
        {error, {_, {already_exists, _}}} -> ok;
        ok -> ok
    end.
