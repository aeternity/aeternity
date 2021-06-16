%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API to the Aeternity node db
%%% @end
%%%-------------------------------------------------------------------
-module(aec_db).

-export([check_db/0,                    % called from setup hook
         initialize_db/1,               % assumes mnesia started
         load_database/0,               % called in aecore app start phase
         tables/1,                      % for e.g. test database setup
         clear_db/0,                    % mostly for test purposes
         tab_copies/1,                  % for create_tables hooks
         check_table/3,                 % for check_tables hooks
         tab/4,
         persisted_valid_genesis_block/0,
         prepare_mnesia_bypass/0
        ]).

-export([ensure_transaction/1,
         ensure_transaction/2,
         ensure_transaction/3,
         ensure_activity/2]).

%% Mimicking the aec_persistence API used by aec_conductor_chain
-export([has_block/1,
         write_block/1,
         write_block/2,
         write_block_state/6,
         write_discovered_pof/2,
         write_genesis_hash/1,
         write_top_block_node/2,
         write_finalized_height/1,
         write_signal_count/2,
         find_block/1,
         find_block_tx_hashes/1,
         find_discovered_pof/1,
         find_header/1,
         dirty_find_header/1,
         find_headers_at_height/1,
         find_key_headers_and_hash_at_height/1,
         find_headers_and_hash_at_height/1,
         find_key_block/1,
         find_signed_tx/1,
         find_signal_count/1,
         get_block/1,
         get_header/1,
         get_genesis_hash/0,
         get_signed_tx/1,
         dirty_get_signed_tx/1,
         get_top_block_hash/0,
         get_top_block_node/0,
         get_finalized_height/0,
         dirty_get_finalized_height/0,
         get_block_state/1,
         get_block_state/2,
         get_block_state_partial/3,
         get_block_from_micro_header/2
        ]).

%% Location of chain transactions
-export([ add_tx_location/2
        , add_tx/1
        , add_tx_hash_to_mempool/1
        , is_in_tx_pool/1
        , find_tx_location/1
        , find_tx_with_location/1
        , remove_tx_from_mempool/1
        , remove_tx_location/1
        , gc_tx/1
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
        , dirty_find_accounts_node/1
        , dirty_find_calls_node/1
        , dirty_find_channels_node/1
        , dirty_find_contracts_node/1
        , dirty_find_ns_node/1
        , dirty_find_ns_cache_node/1
        , dirty_find_oracles_node/1
        , dirty_find_oracles_cache_node/1
        , write_accounts_node/2
        , write_accounts_node/3
        , write_calls_node/2
        , write_channels_node/2
        , write_contracts_node/2
        , write_ns_node/2
        , write_ns_cache_node/2
        , write_oracles_node/2
        , write_oracles_cache_node/2
        ]).

-export([ find_block_state/1
        , find_block_state/2
        , find_block_state_partial/3
        , find_block_difficulty/1
        , find_block_fees/1
        , find_block_fork_id/1
        , find_block_fraud_status/1
        , find_block_state_and_data/1
        , find_block_state_and_data/2
        ]).

-export([ write_peer/1
        , delete_peer/1
        , read_all_peers/0
        ]).

%% Fork tracking and selection
-export([ find_chain_end_hashes/0
        , mark_chain_end_hash/1
        , unmark_chain_end_hash/1
        , start_chain_migration/1
        , finish_chain_migration/1
        , chain_migration_status/1
        ]).

%% For Hyperchains
%% TODO: Until the DB gets refactored we need to share tables in order to not regress on performance
%%       Context: Every table is backed right now by a separate full blown DB -.-
-export([ write_hc_staking_contract_address/1
        , find_hc_staking_contract_address/0
        , delete_hc_staking_contract_address/0
        ]).

%%
%% for testing
-export([backend_mode/0]).

-include("blocks.hrl").
-include("aec_db.hrl").

-include_lib("aehyperchains/include/aehc_parent_db.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").

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
%% - untrusted peers

-define(BYPASS, {?MODULE, mnesia_bypass}).

-define(TAB(Record),
        {Record, tab(Mode, Record, record_info(fields, Record), [])}).
-define(TAB(Record, Extra),
        {Record, tab(Mode, Record, record_info(fields, Record), Extra)}).

%% start a transaction if there isn't already one
-define(t(Expr), ensure_transaction(fun() -> Expr end)).
-define(t(Expr, ErrorKeys), ensure_transaction(fun() -> Expr end, ErrorKeys)).

-define(TX_IN_MEMPOOL, []).
-define(PERSIST, true).

tables() -> tables(ram).

%% WARNING: We are migrating away from mnesia - currently some
%%          backends are bypasing the mnesia transaction manager and issuing
%%          a custom commit to the backend - as a results indexes are not
%%          updated automatically - If you add another index then you need
%%          to update the custom bypass logic
tables(Mode) when Mode==ram; Mode==disc ->
    tables(expand_mode(Mode));
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
   , ?TAB(aec_signed_tx)
   , ?TAB(aec_tx_location)
   , ?TAB(aec_tx_pool)
   , ?TAB(aec_discovered_pof)
   , ?TAB(aec_signal_count)
   , ?TAB(aec_peers)
   , ?TAB(hc_db_pogf)
   , ?TAB(hc_db_commitment_header)
   , ?TAB(hc_db_parent_block_header)
   , ?TAB(hc_db_parent_block_state)
   , ?TAB(hc_db_parent_state)
   , ?TAB(hc_db_delegate_state)
    ].

tab(Mode0, Record, Attributes, Extra) ->
    Mode = expand_mode(Mode0),
    UserProps = maps:get(user_properties, Mode, []),
    UserProps1 = [{vsn, tab_vsn(Record)} | UserProps],
    [ tab_copies(Mode)
    , {type, tab_type(Record)}
    , {attributes, Attributes}
    , {user_properties, UserProps1}
    | Extra
    ].

tab_vsn(_) -> 1.

tab_type(_) -> set.

tab_copies(Mode) when Mode == ram; Mode == disc ->
    tab_copies(expand_mode(Mode));
tab_copies(#{alias := Alias}) -> {Alias, [node()]}.

clear_db() ->
    ?t(begin
           lists:map(
             fun({T, _}) ->
                     Keys = mnesia:all_keys(T),
                     [mnesia:delete({T, K}) || K <- Keys]
             end, tables())
       end,
       [T || {T, _} <- tables()]).

persisted_valid_genesis_block() ->
    case application:get_env(aecore, persist, ?PERSIST) of
        false ->
            true;
        true ->
            ExpectedGH = aec_consensus:get_genesis_hash(),
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

backend_mode() ->
    case application:get_env(aecore, persist, ?PERSIST) of
        false -> expand_mode(ram);
        true  -> expand_mode(disc)
    end.

disc_backend_mode() ->
    {ok, BackendExpr} =
        aeu_env:find_config([<<"chain">>, <<"db_backend">>], [user_config,
                                                              schema_default,
                                                              {value, <<"mnesia">>}]),
    Backend = select_backend(BackendExpr),
    backend_mode(Backend, #{persist => true}).

%% The `db_backend` config option can have the format
%% "<backend>" | "<platform>:<backend> | "<platform>:<backend> [ | <p1:b1> ]*"
%% The alternatives are matched in order, and the first match is chosen.
%% The <platform> pattern may be "*", which matches any platform.
%%
select_backend(Expr) ->
    {Family, Name} = os:type(),
    FamilyB = atom_to_binary(Family, utf8),
    NameB = atom_to_binary(Name, utf8),
    Alts0 = re:split(Expr, <<"\\h*\\|\\h*">>, [{return,binary}]),
    Alts = [re:split(A, <<"\\h*:\\h*">>, [{return,binary}])
            || A <- Alts0],
    match_alts(Alts, FamilyB, NameB).

%% Platform is determined by `os:type() -> {OSFamily, OSName}`
%% We match on EITHER `OSFamily` or `OSName`, even though you're normally
%% not supposed to match on the name. This is so that we can distinguish
%% MacOS (`{unix,darwin}`) from e.g. linux (usually `{unix,linux}`).
%%
%% It's generally better to match on the OS family.
match_alts([[X,B]|_]      , F, N) when X=:=F; X=:=N -> B;
match_alts([[<<"*">>,B]|_], _, _) -> B;
match_alts([[B]|_]        , _, _) -> B;
match_alts([_|T]          , F, N) -> match_alts(T, F, N);
match_alts([]             , _, _) -> erlang:error(no_matching_backend).

backend_mode(_ , #{persist := false} = M) ->
        M#{ module => mnesia
          , alias => ram_copies
          };
backend_mode(<<"rocksdb">>, #{persist := true } = M) ->
        M#{ module => mnesia_rocksdb
          , alias => rocksdb_copies
          , user_properties => [ {rocksdb_opts,
                                  [ {on_write_error, error}
                                    %% This refers to the process managing the table as well.
                                  , {on_write_error_store, aec_db_error_store}
                                  ]}
                               ]
          };
backend_mode(<<"leveled">>, #{persist := true } = M) ->
        M#{ module => mnesia_leveled
          , alias => leveled_copies
          };
backend_mode(<<"mnesia">>, #{persist := true } = M) ->
        M#{ module => mnesia
          , alias => disc_copies
          }.

ensure_transaction(Fun) when is_function(Fun, 0) ->
    ensure_transaction(Fun, []).

ensure_transaction(Fun, ErrorKeys) when is_function(Fun, 0) ->
    ensure_transaction(Fun, ErrorKeys, transaction).

ensure_transaction(Fun, ErrorKeys, TxType) when is_function(Fun, 0) ->
    %% TODO: actually, some non-transactions also have an activity state
    case get(mnesia_activity_state) of
        undefined ->
            try_activity(TxType, Fun, ErrorKeys);
        {_, _, non_transaction} ->
            %% Transaction inside a dirty context; rely on mnesia to handle it
            try_activity(TxType, Fun, ErrorKeys);
        _ ->
            %% We are already in a transaction, thus no custom retry is
            %% attempted via `try_activity/3` since this only works outside of a
            %% transaction.
            Fun()
    end.

ensure_activity(transaction, Fun) when is_function(Fun, 0) ->
    ensure_transaction(Fun);
ensure_activity(PreferedType, Fun) when is_function(Fun, 0) ->
    case get(mnesia_activity_state) of
        undefined ->
            mnesia:activity(PreferedType, Fun);
        _ ->
            Fun()
    end.

read(Tab, Key) ->
    mnesia:read(Tab, Key).

write_block(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, Hash} = aec_headers:hash_header(Header),
    write_block(Block, Hash).

write_block(Block, Hash) ->
    Header = aec_headers:strip_extra(aec_blocks:to_header(Block)),
    Height = aec_headers:height(Header),

    case aec_blocks:type(Block) of
        key ->
            Headers = #aec_headers{ key = Hash
                                  , value = Header
                                  , height = Height },
            ?t(begin
                   mnesia:write(Headers),
                   mnesia:write(#aec_chain_state{key = {key_header, Height, Hash}, value = Header})
               end,
               [{aec_headers, Hash}]);
        micro ->
            Txs = aec_blocks:txs(Block),
            SignedTxs = [#aec_signed_tx{key = aetx_sign:hash(STx), value = STx} || STx <- Txs],
            ?t(begin
                   TxHashes = [begin
                                   mnesia:write(SignedTx),
                                   SignedTx#aec_signed_tx.key
                               end || SignedTx <- SignedTxs],
                   Block1 = #aec_blocks{ key = Hash
                                       , txs = TxHashes
                                       , pof = aec_blocks:pof(Block) },
                   Headers = #aec_headers{ key = Hash
                                         , value = Header
                                         , height = Height },
                   mnesia:write(Block1),
                   mnesia:write(Headers)
               end,
               [ {aec_blocks, Hash}
               , {aec_headers, Hash}
               | [{aec_signed_tx, H} || #aec_signed_tx{key = H} <- SignedTxs] ])
    end.

-spec get_block(binary()) -> aec_blocks:block().
get_block(Hash) ->
    ?t(begin
           [#aec_headers{value = DBHeader}] =
               mnesia:read(aec_headers, Hash),
           Header = aec_headers:from_db_header(DBHeader),
           case aec_headers:type(Header) of
               key ->
                   aec_blocks:new_key_from_header(Header);
               micro ->
                   [#aec_blocks{txs = TxHashes, pof = PoF}] =
                       mnesia:read(aec_blocks, Hash),
                   Txs = [begin
                              [#aec_signed_tx{value = DBSTx}] =
                                  mnesia:read(aec_signed_tx, TxHash),
                              aetx_sign:from_db_format(DBSTx)
                          end || TxHash <- TxHashes],
                   aec_blocks:new_micro_from_header(Header, Txs, PoF)
           end
       end).

find_block_tx_hashes(Hash) ->
    ?t(case mnesia:read(aec_blocks, Hash) of
           [#aec_blocks{txs = TxHashes}] -> {value, TxHashes};
           [] -> none
       end).

get_header(Hash) ->
    ?t(begin
           [#aec_headers{value = DBHeader}] =
               mnesia:read(aec_headers, Hash),
           aec_headers:from_db_header(DBHeader)
       end).

has_block(Hash) ->
    case ?t(mnesia:read(aec_headers, Hash)) of
        [] -> false;
        [_] -> true
    end.

-spec find_block(binary()) -> 'none' | {'value', aec_blocks:block()}.
find_block(Hash) ->
    ?t(case mnesia:read(aec_headers, Hash) of
           [#aec_headers{value = DBHeader}] ->
               Header = aec_headers:from_db_header(DBHeader),
               case aec_headers:type(Header) of
                   key   -> {value, aec_blocks:new_key_from_header(Header)};
                   micro ->
                       [#aec_blocks{txs = TxHashes, pof = PoF}]
                           = mnesia:read(aec_blocks, Hash),
                       Txs = [begin
                                  [#aec_signed_tx{value = DBSTx}] =
                                      mnesia:read(aec_signed_tx, TxHash),
                                  aetx_sign:from_db_format(DBSTx)
                              end || TxHash <- TxHashes],
                       {value, aec_blocks:new_micro_from_header(Header, Txs, PoF)}
               end;
           [] -> none
       end).

-spec get_block_from_micro_header(binary(), aec_headers:micro_header()) -> aec_blocks:micro_block().
get_block_from_micro_header(Hash, MicroHeader) ->
    aec_headers:assert_micro_header(MicroHeader),
    ?t(begin
        [#aec_blocks{txs = TxHashes, pof = PoF}] = mnesia:read(aec_blocks, Hash),
        Txs = [begin
                  [#aec_signed_tx{value = DBSTx}] =
                      mnesia:read(aec_signed_tx, TxHash),
                  aetx_sign:from_db_format(DBSTx)
              end || TxHash <- TxHashes],
        aec_blocks:new_micro_from_header(MicroHeader, Txs, PoF)
       end).

-spec find_key_block(binary()) -> 'none' | {'value', aec_blocks:key_block()}.
find_key_block(Hash) ->
    ?t(case mnesia:read(aec_headers, Hash) of
           [#aec_headers{value = DBHeader}] ->
               Header = aec_headers:from_db_header(DBHeader),
               case aec_headers:type(Header) of
                   key   -> {value, aec_blocks:new_key_from_header(Header)};
                   micro -> none
               end;
           [] -> none
       end).

-spec find_header(binary()) -> 'none' | {'value', aec_headers:header()}.
find_header(Hash) ->
    case ?t(mnesia:read(aec_headers, Hash)) of
        [#aec_headers{value = DBHeader}] -> {value, aec_headers:from_db_header(DBHeader)};
        [] -> none
    end.

%% Dirty dirty reads bypass mnesia entirely for some backends
%% This yields observable performance improvements for the rocksdb backend
-define(dirty_dirty_read(TABLE, KEY),
    begin
        case persistent_term:get(?BYPASS, no_bypass) of
            {rocksdb, #{TABLE := Ref}} ->
                %% When bypass is possible then talk to the DB directly
                case rocksdb:get(Ref, mnesia_rocksdb:encode_key(KEY), []) of
                    {ok, EncVal} ->
                        %% We don't use the key from this object anymore - we don't restore it
                        [mnesia_rocksdb:decode_val(EncVal)];
                    _ ->
                        []
                end;
            _ ->
                %% Otherwise let mnesia handle it
                mnesia:dirty_read(TABLE, KEY)
        end
    end).

-spec dirty_find_header(binary()) -> 'none' | {'value', aec_headers:header()}.
dirty_find_header(Hash) ->
    case ?dirty_dirty_read(aec_headers, Hash) of
        [#aec_headers{value = DBHeader}] -> {value, aec_headers:from_db_header(DBHeader)};
        [] -> none
    end.

-spec find_headers_at_height(pos_integer()) -> [aec_headers:header()].
find_headers_at_height(Height) when is_integer(Height), Height >= 0 ->
    ?t([aec_headers:from_db_header(H) || #aec_headers{value = H}
                 <- mnesia:index_read(aec_headers, Height, height)]).

-spec find_headers_and_hash_at_height(pos_integer()) ->
                                             [{binary(), aec_headers:header()}].
find_headers_and_hash_at_height(Height) when is_integer(Height), Height >= 0 ->
    ?t([{K, aec_headers:from_db_header(H)} || #aec_headers{key = K, value = H}
                 <- mnesia:index_read(aec_headers, Height, #aec_headers.height)]).

%% When benchmarked on an cloud SSD it is faster then mnesia:index_read followed by filter
-spec find_key_headers_and_hash_at_height(pos_integer()) -> [{binary(), aec_headers:key_header()}].
find_key_headers_and_hash_at_height(Height) when is_integer(Height), Height >= 0 ->
    case persistent_term:get({?MODULE, chain_migration, key_headers}, done) of
        in_progress ->
            R = mnesia:dirty_select(aec_headers, [{ #aec_headers{key = '_',
                                                                 value = '$1',
                                                                 height = Height}
                                                  , [{'=:=', {element, 1, '$1'}, key_header}]
                                                  , ['$_']}]),
            [{Hash, aec_headers:from_db_header(Header)}
             || #aec_headers{key = Hash, value = Header} <- R];
        done ->
            case persistent_term:get(?BYPASS, nobypass) of
                {rocksdb, #{aec_chain_state := H}} ->
                    rocks_iterate_from_to(
                        fun({_, _, Hash}, V, Acc) ->
                            {cont,
                             [{Hash,
                               aec_headers:from_db_header(element(3, binary_to_term(V)))
                              } | Acc]}
                        end,
                        [],
                        H,
                        {key_header, Height, '_'},
                        {key_header, Height+1, '_'}
                    );
                nobypass ->
                    R = mnesia:dirty_select(aec_chain_state,
                                            [{#aec_chain_state{key = {key_header, Height, '_'},
                                                               value = '_'}
                                            , []
                                            , ['$_']}]),
                    [{Hash, aec_headers:from_db_header(Header)}
                     || #aec_chain_state{key = {key_header, _, Hash}, value = Header} <- R]
            end
    end.

-spec rocks_iterate_from_to( fun((tuple(), binary(), Acc) -> {cont, Acc} | stop)
                           , Acc
                           , term()
                           , tuple()
                           , tuple()) -> Acc.
rocks_iterate_from_to(Fun, Acc, Handle, From, To) ->
    {ok, I} = rocksdb:iterator(
        Handle,
        [{iterate_upper_bound, sext:prefix(To)}]
    ),
    try
        rocks_iterate_from_to_loop(
            I,
            sext:prefix(To),
            rocksdb:iterator_move(I, sext:prefix(From)),
            Fun,
            Acc
        )
    after
        rocksdb:iterator_close(I)
    end.

rocks_iterate_from_to_loop(I, ToBin, {ok, K, V}, Fun, Acc) when ToBin > K ->
    case Fun(sext:decode(K), V, Acc) of
        {cont, Acc2} ->
            rocks_iterate_from_to_loop(
                I,
                ToBin,
                rocksdb:iterator_move(I, next),
                Fun,
                Acc2
            )%;  % Uncomment when used
        %stop -> Acc
    end;
rocks_iterate_from_to_loop(_I, _, _, _, Acc) -> Acc.

find_discovered_pof(Hash) ->
    case ?t(read(aec_discovered_pof, Hash)) of
        [#aec_discovered_pof{value = PoF}] -> {value, PoF};
        [] -> none
    end.

write_discovered_pof(Hash, PoF) ->
    ?t(mnesia:write(#aec_discovered_pof{key = Hash, value = PoF}),
      [{aec_discovered_pof, Hash}]).

write_block_state(Hash, Trees, AccDifficulty, ForkId, Fees, Fraud) ->
    ?t(begin
           Trees1 = aec_trees:serialize_for_db(aec_trees:commit_to_db(Trees)),
           BlockState = #aec_block_state{ key = Hash
                                        , value = Trees1
                                        , difficulty = AccDifficulty
                                        , fork_id = ForkId
                                        , fees = Fees
                                        , fraud = Fraud },
           mnesia:write(BlockState)
       end,
       [{aec_block_state, Hash}]).

write_accounts_node(Hash, Node) ->
    ?t(mnesia:write(#aec_account_state{key = Hash, value = Node}),
       [{aec_account_state, Hash}]).

write_accounts_node(Table, Hash, Node) ->
    ?t(mnesia:write(Table, #aec_account_state{key = Hash, value = Node}, write),
       [{aec_account_state, Hash}]).

write_calls_node(Hash, Node) ->
    ?t(mnesia:write(#aec_call_state{key = Hash, value = Node}),
       [{aec_call_state, Hash}]).

write_channels_node(Hash, Node) ->
    ?t(mnesia:write(#aec_channel_state{key = Hash, value = Node}),
       [{aec_channel_state, Hash}]).

write_contracts_node(Hash, Node) ->
    ?t(mnesia:write(#aec_contract_state{key = Hash, value = Node}),
       [{aec_contract_state, Hash}]).

write_ns_node(Hash, Node) ->
    ?t(mnesia:write(#aec_name_service_state{key = Hash, value = Node}),
       [{aec_name_service_state, Hash}]).

write_ns_cache_node(Hash, Node) ->
    ?t(mnesia:write(#aec_name_service_cache{key = Hash, value = Node}),
       [{aec_name_service_cache, Hash}]).

write_oracles_node(Hash, Node) ->
    ?t(mnesia:write(#aec_oracle_state{key = Hash, value = Node}),
       [{aec_oracle_state, Hash}]).

write_oracles_cache_node(Hash, Node) ->
    ?t(mnesia:write(#aec_oracle_cache{key = Hash, value = Node}),
       [{aec_oracle_cache, Hash}]).

write_genesis_hash(Hash) when is_binary(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = genesis_hash, value = Hash}),
       [{aec_chain_state, genesis_hash}]).

write_top_block_node(Hash, Hdr) when is_binary(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = top_block_node, value = #{ hash => Hash
                                                                    , header => Hdr} }),
       [{aec_chain_state, top_block_node}]).

write_finalized_height(0) ->
    lager:debug("clearing finalized height", []),
    ?t(mnesia:delete(aec_chain_state, finalized_height, write));
write_finalized_height(Height) when is_integer(Height), Height > 0 ->
    lager:debug("Height = ~p", [Height]),
    ?t(mnesia:write(#aec_chain_state{key = finalized_height, value = Height}),
       [{aec_chain_state, finalized_height}]).

mark_chain_end_hash(Hash) when is_binary(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = {end_block_hash, Hash}, value = []}),
       [{aec_chain_state, {end_block_hash, Hash}}]).

unmark_chain_end_hash(Hash) when is_binary(Hash) ->
    ?t(mnesia:delete(aec_chain_state, {end_block_hash, Hash}, write)).

find_chain_end_hashes() ->
    mnesia:dirty_select(aec_chain_state, [{ #aec_chain_state{key = {end_block_hash, '$1'}, _ = '_'}, [], ['$1'] }]).

start_chain_migration(Key) ->
    %% Writes occur before the error store is initialized - if error keys are present then this will crash
    %% Fortunately this write will be correctly handled by the rocksdb bypass logic
    ?t(mnesia:write(#aec_chain_state{key = {chain_migration_lock, Key}, value = lock})),
    persistent_term:put({?MODULE, chain_migration, Key}, in_progress).

finish_chain_migration(Key) ->
    ?t(mnesia:delete(aec_chain_state, {chain_migration_lock, Key}, write)),
    persistent_term:erase({?MODULE, chain_migration, Key}).

-spec chain_migration_status(atom()) -> in_progress | done.
chain_migration_status(Key) ->
    case ?t(mnesia:read(aec_chain_state, {chain_migration_lock, Key})) of
        [#aec_chain_state{}] -> in_progress;
        _ -> done
    end.

write_hc_staking_contract_address(Address) when is_binary(Address) ->
    ?t(mnesia:write(#aec_chain_state{key = hc_staking_contract_address, value = Address})).

find_hc_staking_contract_address() ->
    case ?t(mnesia:read(aec_chain_state, hc_staking_contract_address)) of
        [#aec_chain_state{value = Address}] -> {value, Address};
        [] -> none
    end.

delete_hc_staking_contract_address() ->
    ?t(mnesia:delete(aec_chain_state, hc_staking_contract_address, write)).

write_signal_count(Hash, Count) when is_binary(Hash), is_integer(Count) ->
    ?t(mnesia:write(#aec_signal_count{key = Hash, value = Count}),
       [{aec_signal_count, Hash}]).

get_genesis_hash() ->
    get_chain_state_value(genesis_hash).

get_top_block_hash() ->
    case get_chain_state_value(top_block_node) of
        #{hash := Hash} ->
            Hash;
        undefined ->
            undefined
    end.

get_top_block_node() ->
    get_chain_state_value(top_block_node).

%% Some migration code: Ideally, top_block_node is there, and we're done.
%% If not, we should find top_block_hash. Fetch the corresponding
%% header, delete the obsolete top_block_hash and put in the new top_block_node.
convert_top_block_entry() ->
    ?t(convert_top_block_entry_()).

convert_top_block_entry_() ->
    case get_chain_state_value(top_block_node) of
        undefined ->
            case get_chain_state_value(top_block_hash) of
                undefined ->
                    ok;
                Hash ->
                    case find_header(Hash) of
                        {value, Header} ->
                            delete_chain_state_value(top_block_hash),
                            write_top_block_node(Hash, Header),
                            #{ hash => Hash
                             , header => Header };
                        none ->
                            weird
                    end
            end;
        _Node ->
            ok
    end.

get_finalized_height() ->
    get_chain_state_value(finalized_height).

dirty_get_finalized_height() ->
    dirty_get_chain_state_value(finalized_height).

get_block_state(Hash) ->
    get_block_state(Hash, false).

get_block_state(Hash, DirtyBackend) ->
    DeserializeFun =
        fun(Trees) ->
           aec_trees:deserialize_from_db(Trees, DirtyBackend)
        end,
    get_block_state_(Hash, DeserializeFun).

get_block_state_partial(Hash, DirtyBackend, Elements) ->
    DeserializeFun =
        fun(Trees) ->
            aec_trees:deserialize_from_db_partial(Trees, DirtyBackend,
                                                  Elements)
        end,
    get_block_state_(Hash, DeserializeFun).

get_block_state_(Hash, DeserializeFun) ->
    ?t(begin
           [#aec_block_state{value = Trees}] =
               mnesia:read(aec_block_state, Hash),
           DeserializeFun(Trees)
       end).

find_block_state(Hash) ->
    find_block_state(Hash, false).

find_block_state(Hash, DirtyBackend) ->
    DeserializeFun =
        fun(Trees) ->
            aec_trees:deserialize_from_db(Trees, DirtyBackend)
        end,
    find_block_state_(Hash, DeserializeFun).

find_block_state_partial(Hash, DirtyBackend, Elements) ->
    DeserializeFun =
        fun(Trees) ->
            aec_trees:deserialize_from_db_partial(Trees, DirtyBackend,
                                                  Elements)
        end,
    find_block_state_(Hash, DeserializeFun).

find_block_state_(Hash, DeserializeFun) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{value = Trees}] ->
            {value, DeserializeFun(Trees)};
        [] -> none
    end.

find_block_difficulty(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{difficulty = D}] -> {value, D};
        [] -> none
    end.

find_block_fees(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{fees = F}] -> {value, F};
        [] -> none
    end.

find_block_fork_id(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{fork_id = F}] -> {value, F};
        [] -> none
    end.

find_block_fraud_status(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{fraud = FS}] -> {value, FS};
        [] -> none
    end.

find_block_state_and_data(Hash) ->
    find_block_state_and_data(Hash, false).

find_block_state_and_data(Hash, DirtyBackend) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{value = Trees, difficulty = D,
                          fork_id = FId, fees = Fees,
                          fraud = Fraud}] ->
            {value, aec_trees:deserialize_from_db(Trees, DirtyBackend), D, FId, Fees, Fraud};
        [] -> none
    end.

find_oracles_node(Hash) ->
    case ?t(mnesia:read(aec_oracle_state, Hash)) of
        [#aec_oracle_state{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_oracles_node(Hash) ->
    case ?dirty_dirty_read(aec_oracle_state, Hash) of
        [#aec_oracle_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_oracles_cache_node(Hash) ->
    case ?t(mnesia:read(aec_oracle_cache, Hash)) of
        [#aec_oracle_cache{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_oracles_cache_node(Hash) ->
    case ?dirty_dirty_read(aec_oracle_cache, Hash) of
        [#aec_oracle_cache{value = Node}] -> {value, Node};
        [] -> none
    end.

find_calls_node(Hash) ->
    case ?t(mnesia:read(aec_call_state, Hash)) of
        [#aec_call_state{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_calls_node(Hash) ->
    case ?dirty_dirty_read(aec_call_state, Hash) of
        [#aec_call_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_channels_node(Hash) ->
    case ?t(mnesia:read(aec_channel_state, Hash)) of
        [#aec_channel_state{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_channels_node(Hash) ->
    case ?dirty_dirty_read(aec_channel_state, Hash) of
        [#aec_channel_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_contracts_node(Hash) ->
    case ?t(mnesia:read(aec_contract_state, Hash)) of
        [#aec_contract_state{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_contracts_node(Hash) ->
    case ?dirty_dirty_read(aec_contract_state, Hash) of
        [#aec_contract_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_ns_node(Hash) ->
    case ?t(mnesia:read(aec_name_service_state, Hash)) of
        [#aec_name_service_state{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_ns_node(Hash) ->
    case ?dirty_dirty_read(aec_name_service_state, Hash) of
        [#aec_name_service_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_ns_cache_node(Hash) ->
    case ?t(mnesia:read(aec_name_service_cache, Hash)) of
        [#aec_name_service_cache{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_ns_cache_node(Hash) ->
    case ?dirty_dirty_read(aec_name_service_cache, Hash) of
        [#aec_name_service_cache{value = Node}] -> {value, Node};
        [] -> none
    end.

find_accounts_node(Hash) ->
    case ?t(mnesia:read(aec_account_state, Hash)) of
        [#aec_account_state{value = Node}] -> {value, Node};
        [] -> none
    end.

dirty_find_accounts_node(Hash) ->
    case ?dirty_dirty_read(aec_account_state, Hash) of
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

delete_chain_state_value(Key) ->
    ?t(mnesia:delete({aec_chain_state, Key}),
       [{aec_chain_state, Key}]).

dirty_get_chain_state_value(Key) ->
    case ?dirty_dirty_read(aec_chain_state, Key) of
        [#aec_chain_state{value = Value}] ->
            Value;
        [] ->
            undefined
    end.

gc_tx(TxHash) ->
    ?t(case find_tx_location(TxHash) of
           BlockHash when is_binary(BlockHash) ->
               {error, BlockHash};
           mempool ->
               mnesia:delete({aec_tx_pool, TxHash});
           none ->
               ok;
           not_found ->
               {error, tx_not_found}
       end,
      [{aec_tx_pool, TxHash}]).

get_signed_tx(Hash) ->
    [#aec_signed_tx{value = DBSTx}] = ?t(read(aec_signed_tx, Hash)),
    aetx_sign:from_db_format(DBSTx).

dirty_get_signed_tx(Hash) ->
    case ?dirty_dirty_read(aec_signed_tx, Hash) of
        [#aec_signed_tx{value = DBSTx}] -> {ok, aetx_sign:from_db_format(DBSTx)};
        [] -> none
    end.

find_signed_tx(Hash) ->
    case ?t(read(aec_signed_tx, Hash)) of
        []                            -> none;
        [#aec_signed_tx{value = DBSTx}] -> {value, aetx_sign:from_db_format(DBSTx)}
    end.

find_signal_count(Hash) ->
    case ?t(read(aec_signal_count, Hash)) of
        [#aec_signal_count{value = Count}] ->
            {value, Count};
        [] ->
            none
    end.

add_tx_location(STxHash, BlockHash) when is_binary(STxHash),
                                         is_binary(BlockHash) ->
    ?t(mnesia:write(#aec_tx_location{key = STxHash, value = BlockHash}),
       [{aec_tx_location, STxHash}]).

remove_tx_location(TxHash) when is_binary(TxHash) ->
    ?t(mnesia:delete({aec_tx_location, TxHash}),
       [{aec_tx_location, TxHash}]).

find_tx_location(STxHash) ->
    ?t(case mnesia:read(aec_tx_location, STxHash) of
           [] ->
               case mnesia:read(aec_tx_pool, STxHash) of
                   [] -> case mnesia:read(aec_signed_tx, STxHash) of
                             [] -> not_found;
                             [_] -> none
                         end;
                   [_] -> mempool
               end;
           [#aec_tx_location{value = BlockHash}] -> BlockHash
       end).

-spec find_tx_with_location(binary()) ->
                                   'none'
                                 | {'mempool', aetx_sign:signed_tx()}
                                 | {binary(), aetx_sign:signed_tx()}.
find_tx_with_location(STxHash) ->
    ?t(case mnesia:read(aec_signed_tx, STxHash) of
           [#aec_signed_tx{value = DBSTx}] ->
               case mnesia:read(aec_tx_location, STxHash) of
                   [] ->
                       case mnesia:read(aec_tx_pool, STxHash) of
                           [] -> none;
                           [_] -> {mempool, aetx_sign:from_db_format(DBSTx)}
                       end;
                   [#aec_tx_location{value = BlockHash}] ->
                       {BlockHash, aetx_sign:from_db_format(DBSTx)}
               end;
           [] -> none
       end).

add_tx(STx) ->
    Hash = aetx_sign:hash(STx),
    ?t(case mnesia:read(aec_signed_tx, Hash) of
           [#aec_signed_tx{value = STx}] ->
              case mnesia:read(aec_tx_location, Hash) of
                   [] -> %% the transaction had either been GCed or is in mempool
                      add_tx_hash_to_mempool(Hash), %% ensure in mempool
                      {ok, Hash};
                   [_] -> {error, already_exists}
              end;
           [] ->
               Obj = #aec_signed_tx{key = Hash, value = STx},
               mnesia:write(Obj),
               add_tx_hash_to_mempool(Hash),
               {ok, Hash}
       end,
      [{aec_signed_tx, Hash}]).

add_tx_hash_to_mempool(TxHash) when is_binary(TxHash) ->
    ?t(mnesia:write(#aec_tx_pool{key = TxHash, value = ?TX_IN_MEMPOOL}),
      [{aec_tx_pool, TxHash}]).

is_in_tx_pool(TxHash) ->
    ?t(mnesia:read(aec_tx_pool, TxHash)) =/= ?TX_IN_MEMPOOL.

remove_tx_from_mempool(TxHash) when is_binary(TxHash) ->
    ?t(mnesia:delete({aec_tx_pool, TxHash}),
       [{aec_tx_pool, TxHash}]).

fold_mempool(FunIn, InitAcc) ->
    Fun = fun(#aec_tx_pool{key = Hash}, Acc) ->
                  FunIn(Hash, Acc)
          end,
    ?t(mnesia:foldl(Fun, InitAcc, aec_tx_pool)).

%% start phase hook to load the database

load_database() ->
    lager:debug("load_database()", []),
    wait_for_tables(),
    convert_top_block_entry(),
    aec_db_gc:maybe_swap_nodes(),
    prepare_mnesia_bypass().

wait_for_tables() ->
    Tabs = mnesia:system_info(tables) -- [schema],
    lager:debug("wait_for_tables (~p)", [Tabs]),
    wait_for_tables(Tabs, 0, _TimePeriods = 5, _MaxWait = 60).

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
    lager:error("Tables not loaded after ~p minutes: ~p", [Sofar, Tabs]),
    %% This is serious and user intervention needed. Stop the system instead
    %% of keeping retrying, but also raise an error for the crash log.
    init:stop(),
    erlang:error({tables_not_loaded, Tabs}).

%% Prepare bypass
prepare_mnesia_bypass() ->
    P = mnesia:table_info(aec_account_state, all),
    TNames = [N || {N,_} <- tables()],
    %% Check whether we can bypass mnesia in some cases
    case proplists:get_value(rocksdb_copies, P, []) of
        [] -> persistent_term:erase(?BYPASS); %% TODO: add leveled backend here
        [_] ->
            %% Ask the backend for the handle
            {HIndexRef, set} = mnesia_rocksdb:get_ref(undefined, {aec_headers, index, {4, ordered}}),
            Tabs = maps:from_list([{aec_headers_index, HIndexRef}] ++ [
                begin
                    {Ref, set} = mnesia_rocksdb:get_ref(undefined, N),
                    %% This is a sanity check designed to ensure that we crash the node when someone adds a new index
                    %% If you add a new index and we still didn't move away to a proper DB then add your new index here
                    %% and modify do_activity!
                    case N of
                        aec_headers -> {index, set, [{{4,ordered},_}]} = mnesia_lib:val({N, index_info});
                        _ -> {index, set, []} = mnesia_lib:val({N, index_info})
                    end,
                    {N, Ref}
                end || N <- TNames]),
            persistent_term:put(?BYPASS, {rocksdb, Tabs})
    end.

%% Initialization routines

check_db() ->
    try
        %% We need to start lager here in case something goes wrong - otherwise instead of nice
        %% debugging logs we will get an erl_crash.dump with a truncated stack trace.
        lager:start(),
        Mode = backend_mode(),
        Storage = ensure_schema_storage_mode(Mode),
        ok = application:ensure_started(mnesia),
        ok = assert_schema_node_name(Mode),
        initialize_db(Mode, Storage)
    ?_catch_(error, Reason, StackTrace)
        error_logger:error_msg("CAUGHT error:~p / ~p~n", [Reason, StackTrace]),
        erlang:error(Reason)
    end.

%% Test interface
initialize_db(ram) ->
    initialize_db(expand_mode(ram), ok).

initialize_db(Mode, Storage) ->
    add_backend_plugins(Mode),
    run_hooks('$aec_db_add_plugins', Mode),
    add_index_plugins(),
    run_hooks('$aec_db_add_index_plugins', Mode),
    ensure_mnesia_tables(Mode, Storage),
    ok.

expand_mode(ram)  -> backend_mode(<<"mnesia">>, #{persist => false});
expand_mode(disc) -> disc_backend_mode();
expand_mode(M) when is_map(M) -> M.

run_hooks(Hook, Mode) ->
    [M:F(Mode) || {_App, {M,F}} <- setup:find_env_vars(Hook)].

fold_hooks(Hook, Acc0) ->
    lists:foldl(
      fun({_App, {M,F}}, Acc) ->
              M:F(Acc)
      end, Acc0, setup:find_env_vars(Hook)).

add_backend_plugins(#{module := Mod, alias := Alias}) when Mod =/= mnesia ->
    Mod:register(Alias);
add_backend_plugins(_) ->
    ok.

add_index_plugins() ->
    ok.

ensure_mnesia_tables(Mode, Storage) ->
    Tables = tables(Mode),
    case Storage of
        existing_schema ->
            handle_table_errors(Tables, Mode, check_mnesia_tables(Tables, []));
        ok ->
            [{atomic,ok} = mnesia:create_table(T, Spec) || {T, Spec} <- Tables],
            run_hooks('$aec_db_create_tables', Mode),
            ok
    end.

handle_table_errors(_Tables, _Mode, []) ->
    ok;
handle_table_errors(Tables, Mode, [{missing_table, aec_signal_count = Table} | Tl]) ->
    %% The table is new in node version 5.1.0.
    new_table_migration(Table, Tables),
    handle_table_errors(Tables, Mode, Tl);
handle_table_errors(Tables, Mode, [{missing_table, aec_peers = Table} | Tl]) ->
    %% The table is new in node version 5.6.0.
    new_table_migration(Table, Tables),
    handle_table_errors(Tables, Mode, Tl);
handle_table_errors(Tables, Mode, [{missing_table, aesc_state_cache_v2} | Tl]) ->
    aesc_db:create_tables(Mode),
    handle_table_errors(Tables, Mode, Tl);
handle_table_errors(Tables, Mode, [{missing_table, Table} | Tl]) when Table == hc_db_pogf;
                                                                      Table == hc_db_commitment_header;
                                                                      Table == hc_db_parent_block_header;
                                                                      Table == hc_db_parent_block_state;
                                                                      Table == hc_db_parent_state;
                                                                      Table == hc_db_delegate_state ->
    %% The table is new in Hyperchains node version
    new_table_migration(Table, Tables),
    handle_table_errors(Tables, Mode, Tl);
handle_table_errors(Tables, Mode, [{callback, {Mod, Fun, Args}} | Tl]) ->
    apply(Mod, Fun, Args),
    handle_table_errors(Tables, Mode, Tl);
handle_table_errors(_Tables, _Mode, Errors) ->
    erlang:error({table_check, Errors}).

check_mnesia_tables([{Table, Spec}|Left], Acc) ->
    NewAcc = check_table(Table, Spec, Acc),
    check_mnesia_tables(Left, NewAcc);
check_mnesia_tables([], Acc) ->
    fold_hooks('$aec_db_check_tables', Acc).

check_table(Table, Spec, Acc) ->
    try
        UserProps = mnesia:table_info(Table, user_properties),
        UserPropsSpec = proplists:get_value(user_properties, Spec, []),
        Vsn = proplists:get_value(vsn, UserProps),
        VsnSpec = proplists:get_value(vsn, UserPropsSpec),
        if
            Vsn =:= undefined ->
                [{missing_version, Table, UserProps}|Acc];
            VsnSpec =:= undefined ->
                [{missing_version_in_spec, Table, UserProps}|Acc];
            Vsn =:= VsnSpec ->
                Acc;
            true ->
                %% old version and spec are not equal
                {vsn, Old} = VsnSpec,
                {vsn, New} = Vsn,
                [{vsn_fail, Table, [{expected, New}, {got, Old}]}|Acc]
        end
    catch
        _:_ ->
            [{missing_table, Table}|Acc]
    end.

assert_schema_node_name(#{persist := false}) ->
    ok;
assert_schema_node_name(#{persist := true}) ->
    [DbOwnerNode] = mnesia:table_info(schema, all_nodes),
    case DbOwnerNode =:= node() of
        true -> ok;
        false ->
            {ok,_DbDir} = aeu_env:find_config([<<"chain">>, <<"db_path">>],
                                              [user_config, schema_default]),
            error_logger:error_msg("Database cannot be loaded. "
                                   "It was created for the node ~p, and current node "
                                   "is ~p (these must not differ!).",
                        [DbOwnerNode, node()]),
            exit(wrong_db_owner_node)
    end.

ensure_schema_storage_mode(#{persist := false}) ->
    case disc_db_exists() of
        {true, Dir} ->
            lager:warning("Will not use existing Mnesia db (~s)", [Dir]),
            set_dummy_mnesia_dir(Dir);
        false ->
            ok
    end;
ensure_schema_storage_mode(#{persist := true}) ->
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

try_activity(Type, Fun, ErrorKeys) ->
    %% If no configuration is found, we only retry once.
    MaxRetries = aeu_env:user_config_or_env([<<"chain">>, <<"db_write_max_retries">>],
                                            aecore, db_write_max_retries, 1),
    try_activity(Type, Fun, ErrorKeys, MaxRetries).

%% @doc Run function in an mnesia activity context. If retries is 0 or less, the
%% operation will not be retried upon failure. If retries is greater than 0, the
%% operation will be retried upon failure and the retry counter will be
%% decremented.
%% This function must not be called from inside another transaction because this
%% will mess with Mnesia's internal retry mechanism.
try_activity(Type, Fun, ErrorKeys, Retries) when Retries =< 0 ->
    handle_activity_result(do_activity(Type, Fun), ErrorKeys);
try_activity(Type, Fun, ErrorKeys, Retries) ->
    try
        handle_activity_result(do_activity(Type, Fun), ErrorKeys)
    catch
        exit:Reason ->
            lager:warning("Mnesia activity Type=~p exit with Reason=~p, retrying", [Type, Reason]),
            try_activity(Type, Fun, ErrorKeys, Retries - 1)
    end.

handle_activity_result(Res, ErrorKeys) ->
    case aec_db_error_store:check(ErrorKeys) of
        [] ->
            Res;
        [{_, Err, _} | _] ->
            %% For simplicity reasons we only report the first error, since that
            %% is enough to indicate that something went wrong on the storage
            %% layer.
            exit(Err)
    end.

do_activity(transaction, Fun) ->
    try
    mnesia:activity(transaction, fun() ->
        R = Fun(), %% Get the result of the transaction
        %% Normally mnesia would commit this transaction but we may speed up things
        %% a lot by bypassing mnesia_tm in some cases
        case persistent_term:get(?BYPASS, no_bypass) of
            {rocksdb, Handles} ->
                %% Ok we may try to do a custom bypass
                {mnesia, _, {tidstore, TStore, _, _}} = get(mnesia_activity_state),
                %% Find our what we need to actually commit
                try ets:foldl(fun walk_tstore/2, #{}, TStore) of
                    M when map_size(M) =:= 0 ->
                        %% If there is nothing to commit
                        %% aborting the transaction will be faster than going through commit protocols and checkpointing in mnesia
                        throw({bypass, R});
                    M ->
                        %% This might be paralleized but by doing so we can get an inconsistent DB in case of IO failures
                        %% Right now commit things sequentially to N separate databases - this won't be a problem after we have the new DB in place
                        %% As the node might add custom mnesia tables we check which tables we can bypass
                        {Found, NotFound} = lists:foldl(
                            fun ({Table, Batch}, {AccFound, AccNotFound}) ->
                                case maps:find(Table, Handles) of
                                    error -> {AccFound, [Table|AccNotFound]}; %% Not bypassable table
                                    {ok, H} ->
                                        %% Commit the entire batch of data
                                        case aec_db_lib:rocksdb_write(H, Batch, []) of %% {sync, true} ?
                                            ok -> {[Table|AccFound], AccNotFound};
                                            {error, Err} ->
                                                %% In case of IO errors abort immediately - we still might leave the DB in an inconsistent state
                                                %% Fortunately by writing the state trees first the inconsistency won't be fatal :)
                                                lager:debug("DB transaction commit failed: ~p\n", [{Table, Err}]),
                                                throw({error, {io_error, {Table, Err}}})
                                        end
                                end
                            end, {[], []}, safe_batch_sort(maps:to_list(M))), %% Order matters! We first write the state trees and only then other metadata
                            case NotFound of
                                [] -> throw({bypass, R}); %% Great we bypassed all tables and we may exit early :)
                                _ ->
                                    %% When not all data was bypassed then log what was not bypassed
                                    lager:debug("Missed tables in mnesia bypass: ~p\n", [NotFound]),
                                    %% Fixup the transaction store as we have still data left to write :P
                                    [begin
                                         Batch = maps:get(Table, M),
                                         lists:foreach(
                                             fun ({put, Key, _}) -> ets:delete(TStore, {Table, mnesia_rocksdb:decode_key(Key)});
                                                 ({delete, Key}) -> ets:delete(TStore, {Table, mnesia_rocksdb:decode_key(Key)})
                                             end, Batch)
                                     end || Table <- Found],
                                    R
                            end
                catch
                    throw:bypass_abort -> R
                end;
            no_bypass -> R
        end
    end)
    catch
        exit:{aborted, {throw, {bypass, R}}} -> R;
        exit:{aborted, {throw, {error, {io_error, _}} = Err}} -> Err
    end;
do_activity(Type, Fun) ->
    mnesia:activity(Type, Fun).

walk_tstore({{locks, _, _}, _}, Acc) -> Acc;
walk_tstore({nodes, _}, Acc) -> Acc;
walk_tstore({{aec_headers, Key}, Val, write}, Acc) ->
    O = maps:get(aec_headers, Acc, []),
    I = maps:get(aec_headers_index, Acc, []),
    Height = element(4, Val),
    Acc#{ aec_headers       => [{put, mnesia_rocksdb:encode_key(Key), mnesia_rocksdb:encode_val(setelement(2, Val, []))} | O]
        , aec_headers_index => [{put, mnesia_rocksdb:encode_key({Height, Key}), mnesia_rocksdb:encode_val({[]})} | I]
        };
walk_tstore({{Table, Key}, Val, write}, Acc) ->
    Acc#{Table => [{put, mnesia_rocksdb:encode_key(Key), mnesia_rocksdb:encode_val(setelement(2, Val, []))} | maps:get(Table, Acc, [])]};
walk_tstore({{aec_headers, _}, _, delete}, _Acc) ->
    %% We never delete headers...
    lager:debug("Unimplemented delete of aec_headers due to index - reverting back to an ordinary mnesia commit\n", []),
    throw(bypass_abort);
walk_tstore({{Table, Key}, _, delete}, Acc) ->
    Acc#{Table => [{delete, mnesia_rocksdb:encode_key(Key)} | maps:get(Table, Acc, [])]}.

%% Until we migrate away to a proper DB we need to be careful
%% This is the safest ordering when inserting things to our databases
%% This ensures that state trees are written first and only then we proceed to write other metadata
safe_batch_sort(L) -> lists:sort(fun safe_batch_cmp/2, L).
safe_batch_cmp({aec_account_state, _}, _) -> true;
safe_batch_cmp({aec_contract_state, _}, _) -> true;
safe_batch_cmp({aec_call_state, _}, _) -> true;
safe_batch_cmp({aec_oracle_cache, _}, _) -> true;
safe_batch_cmp({aec_oracle_state, _}, _) -> true;
safe_batch_cmp({aec_channel_state, _}, _) -> true;
safe_batch_cmp({aec_name_service_cache, _}, _) -> true;
safe_batch_cmp({aec_name_service_state, _}, _) -> true;
safe_batch_cmp(_, {aec_headers, _}) -> false;
safe_batch_cmp(_, _) -> false.

write_peer(Peer) ->
    PeerId = aec_peer:id(Peer),
    ?t(mnesia:write(#aec_peers{key = PeerId, value = Peer}),
      [{aec_peers, PeerId}]).

delete_peer(Peer) ->
    PeerId = aec_peer:id(Peer),
    ?t(mnesia:delete({aec_peers, PeerId})).

%% to be used only in aec_sync:init to load persisted peeers
read_all_peers() ->
    Fun = fun(#aec_peers{value = Peer}, Acc) ->
              [Peer | Acc]
          end,
    ?t(mnesia:foldl(Fun, [], aec_peers)).

new_table_migration(Table, Tables) ->
    {Table, Spec} = lists:keyfind(Table, 1, Tables),
    {atomic, ok} = mnesia:create_table(Table, Spec).
