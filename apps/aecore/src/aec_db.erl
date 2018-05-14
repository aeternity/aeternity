%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API to the epoch db
%%% @end
%%%-------------------------------------------------------------------
-module(aec_db).

-export([check_db/0,                    % called from setup hook
         initialize_db/1,               % assumes mnesia started
         load_database/0,               % called in aecore app start phase
         tables/1,                      % for e.g. test database setup
         clear_db/0,                    % mostly for test purposes
         persisted_valid_genesis_block/0
        ]).

-export([transaction/1,
         ensure_transaction/1,
         write/2,
         delete/2,
         read/2]).

%% Mimicking the aec_persistence API used by aec_conductor_chain
-export([has_block/1,
         write_block/1,
         write_block_state/4,
         write_genesis_hash/1,
         write_top_block_hash/1,
         write_top_block_height/1,
         find_block/1,
         find_header/1,
         find_headers_at_height/1,
         get_block/1,
         get_block_tx_hashes/1,
         get_header/1,
         get_genesis_hash/0,
         get_signed_tx/1,
         get_top_block_hash/0,
         get_top_block_height/0,
         get_block_state/1
        ]).

%% Location of chain transactions
-export([ add_tx_location/2
        , add_tx/1
        , add_tx_hash_to_mempool/1
        , is_in_tx_pool/1
        , find_transaction_in_main_chain_or_mempool/1 %% NOTE: Legacy tx hash
        , find_tx_location/1
        , remove_tx_from_mempool/1
        , remove_tx_location/1
        ]).

%% Only to be used from aec_tx_pool:init/1
-export([ fold_mempool/2
        ]).


%% MP trees backend
-export([ find_accounts_node/1
        , find_calls_node/1
        , find_channels_node/1
        , find_contracts_node/1
        , find_ns_node/1
        , find_ns_cache_node/1
        , find_oracles_node/1
        , find_oracles_cache_node/1
        , write_accounts_node/2
        , write_calls_node/2
        , write_channels_node/2
        , write_contracts_node/2
        , write_ns_node/2
        , write_ns_cache_node/2
        , write_oracles_node/2
        , write_oracles_cache_node/2
        ]).

-export([ find_block_state/1
        , find_block_difficulty/1
        , find_block_fork_id/1
        , find_block_state_and_data/1
        ]).

%% API for finding transactions related to account key
-export([transactions_by_account/3]).

%% indexing callbacks
-export([ ix_acct2tx/3
        , ix_tx2stx/3
        ]).

-include("common.hrl").
-include("blocks.hrl").
-include("aec_db.hrl").

%% - transactions
%% - headers
%% - block  [transaction_ids]
%% - oracle_state
%% - oracle_cache
%% - account_state
%% - channel_state
%% - name_service_state
%% - name_service_cache
%% - one per state tree

-define(TAB(Record),
        {Record, tab(Mode, Record, record_info(fields, Record), [])}).
-define(TAB(Record, Extra),
        {Record, tab(Mode, Record, record_info(fields, Record), Extra)}).

%% start a transaction if there isn't already one
-define(t(Expr), case get(mnesia_activity_state) of undefined ->
                         transaction(fun() -> Expr end);
                     _ -> Expr
                 end).

-define(TX_IN_MEMPOOL, []).

tables(Mode) ->
    [?TAB(aec_blocks)
   , ?TAB(aec_headers, [{index, [height]}])
   , ?TAB(aec_chain_state)
   , ?TAB(aec_contract_state)
   , ?TAB(aec_call_state)
   , ?TAB(aec_block_state)
   , ?TAB(aec_oracle_cache)
   , ?TAB(aec_oracle_state)
   , ?TAB(aec_account_state)
   , ?TAB(aec_channel_state)
   , ?TAB(aec_name_service_cache)
   , ?TAB(aec_name_service_state)
   , ?TAB(aec_signed_tx, [{index, [{acct2tx}, {tx2stx}]}])
   , ?TAB(aec_tx_location)
   , ?TAB(aec_tx_pool)
    ].

tab(Mode, Record, Attributes, Extra) ->
    [ tab_copies(Mode)
    , {type, tab_type(Record)}
    , {attributes, Attributes}
    , {user_properties, [{vsn, tab_vsn(Record)}]}
      | Extra].

tab_vsn(_) -> 1.

tab_type(_) -> set.

tab_copies(disc) -> {rocksdb_copies, [node()]};
tab_copies(ram ) -> {ram_copies , [node()]}.

clear_db() ->
    ?t([clear_table(T) || {T, _} <- tables(ram)]).

clear_table(Tab) ->
    ?t(begin
           Keys = mnesia:all_keys(Tab),
           [delete(Tab, K) || K <- Keys],
           ok
       end).

persisted_valid_genesis_block() ->
    case application:get_env(aecore, persist, false) of
        false ->
            true;
        true ->
            {ok, ExpectedGH} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
            case aec_db:get_genesis_hash() of
                undefined ->
                    lager:info("Loaded empty persisted chain"),
                    true;
                ExpectedGH ->
                    true;
                LoadedGH ->
                    lager:warning("Expected genesis block hash ~p, persisted genesis block hash ~p",
                                  [ExpectedGH, LoadedGH]),
                    false
            end
    end.

transaction(Fun) when is_function(Fun, 0) ->
    mnesia:activity(transaction, Fun).

ensure_transaction(Fun) when is_function(Fun, 0) ->
    %% TODO: actually, some non-transactions also have an activity state
    case get(mnesia_activity_state) of undefined ->
            transaction(Fun);
        _ -> Fun()
    end.


read(Tab, Key) ->
    mnesia:read(Tab, Key).

write(Tab, Obj) ->
    mnesia:write(Tab, Obj, write).

delete(Tab, Key) ->
    mnesia:delete(Tab, Key, write).

write_block(Block) ->
    Header = aec_blocks:to_header(Block),
    Height = aec_headers:height(Header),
    Txs    = aec_blocks:txs(Block),
    {ok, Hash} = aec_headers:hash_header(Header),
    ?t(begin
           TxHashes = [begin
                           STxHash = aetx_sign:hash(STx),
                           write_signed_tx(STxHash, STx),
                           STxHash
                       end
                       || STx <- Txs],
           mnesia:write(#aec_blocks{key = Hash,
                                    txs = TxHashes}),
           mnesia:write(#aec_headers{key = Hash,
                                     value = Header,
                                     height = Height})
       end).

get_block(Hash) ->
    ?t(begin
           [#aec_headers{value = Header}] =
               mnesia:read(aec_headers, Hash),
           [#aec_blocks{txs = TxHashes}] =
               mnesia:read(aec_blocks, Hash),
           Txs = [begin
                      [#aec_signed_tx{value = STx}] =
                          mnesia:read(aec_signed_tx, TxHash),
                      STx
                  end || TxHash <- TxHashes],
           aec_blocks:from_header_and_txs(Header, Txs)
       end).

get_block_tx_hashes(Hash) ->
    ?t(begin
           [#aec_blocks{txs = TxHashes}] =
               mnesia:read(aec_blocks, Hash),
           TxHashes
       end).

get_header(Hash) ->
    ?t(begin
           [#aec_headers{value = Header}] =
               mnesia:read(aec_headers, Hash),
           Header
       end).

has_block(Hash) ->
    case ?t(mnesia:read(aec_headers, Hash)) of
        [] -> false;
        [_] -> true
    end.

find_block(Hash) ->
    ?t(case mnesia:read(aec_blocks, Hash) of
           [#aec_blocks{txs = TxHashes}] ->
               Txs = [begin
                          [#aec_signed_tx{value = STx}] =
                              mnesia:read(aec_signed_tx, TxHash),
                          STx
                      end || TxHash <- TxHashes],
               [#aec_headers{value = Header}] = mnesia:read(aec_headers, Hash),
               {value, aec_blocks:from_header_and_txs(Header, Txs)};
           [] -> none
       end).

find_header(Hash) ->
    case ?t(mnesia:read(aec_headers, Hash)) of
        [#aec_headers{value = Header}] -> {value, Header};
        [] -> none
    end.

find_headers_at_height(Height) when is_integer(Height), Height >= 0 ->
    ?t([H || #aec_headers{value = H}
                 <- mnesia:index_read(aec_headers, Height, height)]).

write_block_state(Hash, Trees, AccDifficulty, ForkId) ->
    ?t(mnesia:write(#aec_block_state{key = Hash, value = Trees,
                                     difficulty = AccDifficulty,
                                     fork_id = ForkId
                                    })).

write_accounts_node(Hash, Node) ->
    ?t(mnesia:write(#aec_account_state{key = Hash, value = Node})).

write_calls_node(Hash, Node) ->
    ?t(mnesia:write(#aec_call_state{key = Hash, value = Node})).

write_channels_node(Hash, Node) ->
    ?t(mnesia:write(#aec_channel_state{key = Hash, value = Node})).

write_contracts_node(Hash, Node) ->
    ?t(mnesia:write(#aec_contract_state{key = Hash, value = Node})).

write_ns_node(Hash, Node) ->
    ?t(mnesia:write(#aec_name_service_state{key = Hash, value = Node})).

write_ns_cache_node(Hash, Node) ->
    ?t(mnesia:write(#aec_name_service_cache{key = Hash, value = Node})).

write_oracles_node(Hash, Node) ->
    ?t(mnesia:write(#aec_oracle_state{key = Hash, value = Node})).

write_oracles_cache_node(Hash, Node) ->
    ?t(mnesia:write(#aec_oracle_cache{key = Hash, value = Node})).

write_genesis_hash(Hash) when is_binary(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = genesis_hash, value = Hash})).

write_top_block_hash(Hash) when is_binary(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = top_block_hash, value = Hash})).

write_top_block_height(Height) when is_integer(Height) ->
    ?t(mnesia:write(#aec_chain_state{key = top_block_height, value = Height})).

get_genesis_hash() ->
    get_chain_state_value(genesis_hash).

get_top_block_hash() ->
    get_chain_state_value(top_block_hash).

get_top_block_height() ->
    get_chain_state_value(top_block_height).

get_block_state(Hash) ->
    ?t(begin
           [#aec_block_state{value = Trees}] =
               mnesia:read(aec_block_state, Hash),
           Trees
       end).

find_block_state(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{value = Trees}] -> {value, Trees};
        [] -> none
    end.

find_block_difficulty(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{difficulty = D}] -> {value, D};
        [] -> none
    end.

find_block_fork_id(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{fork_id = F}] -> {value, F};
        [] -> none
    end.

find_block_state_and_data(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{value = Trees, difficulty = D, fork_id = F}] ->
            {value, Trees, D, F};
        [] -> none
    end.

find_oracles_node(Hash) ->
    case ?t(mnesia:read(aec_oracle_state, Hash)) of
        [#aec_oracle_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_oracles_cache_node(Hash) ->
    case ?t(mnesia:read(aec_oracle_cache, Hash)) of
        [#aec_oracle_cache{value = Node}] -> {value, Node};
        [] -> none
    end.

find_calls_node(Hash) ->
    case ?t(mnesia:read(aec_call_state, Hash)) of
        [#aec_call_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_channels_node(Hash) ->
    case ?t(mnesia:read(aec_channel_state, Hash)) of
        [#aec_channel_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_contracts_node(Hash) ->
    case ?t(mnesia:read(aec_contract_state, Hash)) of
        [#aec_contract_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_ns_node(Hash) ->
    case ?t(mnesia:read(aec_name_service_state, Hash)) of
        [#aec_name_service_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_ns_cache_node(Hash) ->
    case ?t(mnesia:read(aec_name_service_cache, Hash)) of
        [#aec_name_service_cache{value = Node}] -> {value, Node};
        [] -> none
    end.

find_accounts_node(Hash) ->
    case ?t(mnesia:read(aec_account_state, Hash)) of
        [#aec_account_state{value = Node}] -> {value, Node};
        [] -> none
    end.

get_chain_state_value(Key) ->
    ?t(case mnesia:read(aec_chain_state, Key) of
           [#aec_chain_state{value = Value}] ->
               Value;
           _ ->
               undefined
       end).

write_signed_tx(Hash, STx) ->
    ?t(write(aec_signed_tx, #aec_signed_tx{key = Hash,
                                           value = STx})).

get_signed_tx(Hash) ->
    [#aec_signed_tx{value = STx}] = ?t(read(aec_signed_tx, Hash)),
    STx.

add_tx_location(STxHash, BlockHash) when is_binary(STxHash),
                                         is_binary(BlockHash) ->
    Obj = #aec_tx_location{key = STxHash, value = BlockHash},
    ?t(write(aec_tx_location, Obj)).

remove_tx_location(TxHash) when is_binary(TxHash) ->
    ?t(delete(aec_tx_location, TxHash)).

find_tx_location(STxHash) ->
    ?t(case mnesia:read(aec_tx_location, STxHash) of
           [] ->
               case mnesia:read(aec_tx_pool, STxHash) of
                   [] -> none;
                   [_] -> mempool
               end;
           [#aec_tx_location{value = BlockHash}] -> BlockHash
       end).

add_tx(STx) ->
    Hash = aetx_sign:hash(STx),
    ?t(case mnesia:read(aec_signed_tx, Hash) of
           [_] -> {error, already_exists};
           [] ->
               Obj = #aec_signed_tx{key = Hash, value = STx},
               write(aec_signed_tx, Obj),
               add_tx_hash_to_mempool(Hash),
               {ok, Hash}
       end).

add_tx_hash_to_mempool(TxHash) when is_binary(TxHash) ->
    Obj = #aec_tx_pool{key = TxHash, value = []},
    ?t(write(aec_tx_pool, Obj)).

is_in_tx_pool(TxHash) ->
    ?t(mnesia:read(aec_tx_pool, TxHash)) =/= [].

remove_tx_from_mempool(TxHash) when is_binary(TxHash) ->
    ?t(delete(aec_tx_pool, TxHash)).

fold_mempool(FunIn, InitAcc) ->
    Fun = fun(#aec_tx_pool{key = Hash}, Acc) ->
                  FunIn(Hash, Acc)
          end,
    ?t(mnesia:foldl(Fun, InitAcc, aec_tx_pool)).

-dialyzer({nowarn_function, find_transaction_in_main_chain_or_mempool/1}). %% For mnesia patches.

-spec find_transaction_in_main_chain_or_mempool(binary()) ->
                                                       'none'
                                                     | {'mempool', [aetx_sign:signed_tx()]}
                                                     | {binary(), aetx_sign:signed_tx()}.
find_transaction_in_main_chain_or_mempool(Hash) ->
    ?t(pick_location(mnesia:index_read(aec_signed_tx, Hash, {tx2stx}), [])).

-dialyzer({nowarn_function, pick_location/2}). %% For mnesia patches.
pick_location([], Acc) ->
    case [STx || #aec_signed_tx{key = STxHash, value = STx} <- Acc,
                 is_in_tx_pool(STxHash)] of
        [] -> none;
        MempoolTxs -> {mempool, MempoolTxs}
    end;
pick_location([#aec_signed_tx{key = STxHash, value = STx} = DBTx|Left], Acc) ->
    case mnesia:read(aec_tx_location, STxHash) of
        [] -> pick_location(Left, [DBTx|Acc]);
        [#aec_tx_location{value = BlockHash}] -> {BlockHash, STx}
    end.

-dialyzer({nowarn_function, transactions_by_account/3}). %% For mnesia patches.
transactions_by_account(AcctPubKey, Filter, false =_ShowPending) ->
    ?t([T || #aec_signed_tx{value = T, key = STxHash}
                 <- mnesia:index_read(aec_signed_tx, AcctPubKey, {acct2tx}),
             mnesia:read(aec_tx_location, STxHash) =/= [],
             Filter(T)
       ]);
transactions_by_account(AcctPubKey, Filter, true =_ShowPending) ->
    ?t([T || #aec_signed_tx{key = STxHash, value = T}
                 <- mnesia:index_read(aec_signed_tx, AcctPubKey, {acct2tx}),
             (mnesia:read(aec_tx_location, STxHash) =/= [])
                 orelse is_in_tx_pool(STxHash),
             Filter(T)
       ]).

%% start phase hook to load the database

load_database() ->
    lager:debug("load_database()", []),
    try
        wait_for_tables()
    catch
        error:E ->
            erlang:error({E, erlang:get_stacktrace()});
        exit:E ->
            exit({E, erlang:get_stacktrace()})
    end.

wait_for_tables() ->
    Tabs = mnesia:system_info(tables) -- [schema],
    lager:debug("wait_for_tables (~p)", [Tabs]),
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

%% Index callbacks

ix_acct2tx(aec_signed_tx, _Ix, #aec_signed_tx{value = SignedTx}) ->
    try aetx_sign:tx(SignedTx) of
        Tx ->
            aetx:accounts(Tx)
    catch
        error:_ ->
            []
    end.

ix_tx2stx(aec_signed_tx, _Ix, #aec_signed_tx{value = SignedTx}) ->
    try aetx_sign:tx(SignedTx) of
        Tx -> [aetx:hash(Tx)]
    catch
        error:_ ->
            []
    end.

%% Initialization routines

check_db() ->
    try
        Mode = case application:get_env(aecore, persist, false) of
                   true  -> disc;
                   false -> ram
               end,
        Storage = ensure_schema_storage_mode(Mode),
        ok = application:ensure_started(mnesia),
        initialize_db(Mode, Storage)
    catch
        error:Reason ->
            lager:error("CAUGHT error:~p / ~p",
                        [Reason, erlang:get_stacktrace()]),
            error(Reason)
    end.

%% Test interface
initialize_db(ram) ->
    initialize_db(ram, ok).

initialize_db(Mode, Storage) ->
    add_backend_plugins(Mode),
    run_hooks('$aec_db_add_plugins', Mode),
    add_index_plugins(),
    run_hooks('$aec_db_add_index_plugins', Mode),
    ensure_mnesia_tables(Mode, Storage),
    ok.

run_hooks(Hook, Mode) ->
    [M:F(Mode) || {_App, {M,F}} <- setup:find_env_vars(Hook)].

fold_hooks(Hook, Acc0) ->
    lists:foldl(
      fun({_App, {M,F}}, Acc) ->
              M:F(Acc)
      end, Acc0, setup:find_env_vars(Hook)).

add_backend_plugins(disc) ->
    mnesia_rocksdb:register();
add_backend_plugins(_) ->
    ok.

add_index_plugins() ->
    mnesia_schema:add_index_plugin({acct2tx}, aec_db, ix_acct2tx),
    mnesia_schema:add_index_plugin({tx2stx}, aec_db, ix_tx2stx).

ensure_mnesia_tables(Mode, Storage) ->
    Tables = tables(Mode),
    case Storage of
        existing_schema ->
            case check_mnesia_tables(Tables, []) of
                [] -> ok;
                Errors ->
                    lager:error("Database check failed: ~p", [Errors]),
                    error({table_check, Errors})
            end;
        ok ->
            [{atomic,ok} = mnesia:create_table(T, Spec)
             || {T, Spec} <- Tables],
            run_hooks('$aec_db_create_tables', Mode),
            ok
    end.

check_mnesia_tables([{Table, Spec}|Left], Acc) ->
    NewAcc = try mnesia:table_info(Table, user_properties) of
                 [{vsn, Vsn}] ->
                     case proplists:get_value(user_properties, Spec) of
                         [{vsn, Vsn}] -> Acc;
                         [{vsn, Old}] -> [{vsn_fail, Table,
                                           [{expected, Vsn},
                                            {got, Old}]}
                                          |Acc]
                     end;
                 Other -> [{missing_version, Table, Other}|Acc]
             catch _:_ -> [{missing_table, Table}|Acc]
             end,
    check_mnesia_tables(Left, NewAcc);
check_mnesia_tables([], Acc) ->
    fold_hooks('$aec_db_check_tables', Acc).

ensure_schema_storage_mode(ram) ->
    case disc_db_exists() of
        {true, Dir} ->
            lager:warning("Will not use existing Mnesia db (~s)", [Dir]),
            set_dummy_mnesia_dir(Dir);
        false ->
            ok
    end;
ensure_schema_storage_mode(disc) ->
    case mnesia:create_schema([node()]) of
        {error, {_, {already_exists, _}}} -> existing_schema;
        ok -> ok
    end.

disc_db_exists() ->
    Dir = default_dir(),
    case f_exists(Dir) of
        true ->
            {true, Dir};
        false ->
            false
    end.

f_exists(F) ->
    case file:read_link_info(F) of
        {ok, _} -> true;
        _ -> false
    end.

set_dummy_mnesia_dir(Dir) ->
    TS = erlang:system_time(millisecond),
    NewDir = find_nonexisting(filename:absname(Dir), TS),
    application:set_env(mnesia, dir, NewDir).

find_nonexisting(Dir, N) ->
    Path = Dir ++ "-" ++ integer_to_list(N),
    case f_exists(Path) of
        true ->
            find_nonexisting(Dir, N+1);
        false ->
            Path
    end.

default_dir() ->
    case application:get_env(mnesia, dir) of
        undefined ->
            %% This is is how mnesia produces the default. The result will
            %% be the same as long as the current working directory doesn't
            %% change between now and when mnesia starts.
            filename:absname(lists:concat(["Mnesia.", node()]));
        {ok, Dir} ->
            Dir
    end.
