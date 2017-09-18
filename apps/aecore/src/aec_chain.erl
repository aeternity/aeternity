%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Service holding the longest chain of block headers and blocks.
%%%
%%% The longest chain is determined according to the amount of work
%%% done on the chain.
%%%
%%% @TODO Unit testing of unhappy paths.
%%% @TODO Persistence.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain).

-behaviour(gen_server).

%% API
-export([start_link/1,
         stop/0]).
-export([top/0,
         top_header/0,
         top_block/0,
         get_header_by_hash/1,
         get_block_by_hash/1,
         get_header_by_height/1,
         get_block_by_height/1,
         insert_header/1,
         write_block/1,
         get_work_at_top/0,
         get_work_by_hash_and_at_top/1,
         has_more_work/1,
         force_insert_headers/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("common.hrl").
-include("blocks.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CALL_TIMEOUT, infinity). %% For synchronous persistence and for forced chain (fork).

-define(IS_HEIGHT(H), %% For guard.
        (is_integer(H) andalso (H >= ?GENESIS_HEIGHT))
       ).
-define(IS_HEIGHT_AFTER_GENESIS(H), %% For guard.
        (is_integer(H) andalso (H > ?GENESIS_HEIGHT))
       ).

-define(IS_WORK(W), (is_float(W) andalso (W > 0.0))). %% For guard.

-define(IS_CHAIN_HEADER(H), %% For guard.
        (is_record(H, chain_header)
         andalso is_record(element(#chain_header.h, H), header)
         andalso ?IS_WORK(element(#chain_header.td, H))
        )
       ).

-define(TOP_HEADER, top_header).

-record(top_state,
        {top_header :: headers_db_value(),
         top_header_db :: top_header_db(),
         top_block :: aec_blocks:block_deserialized_from_network()}). %% Without state trees.
-record(state, {top :: #top_state{},
                headers_db :: headers_db(),
                blocks_db :: blocks_db()}).

-record(chain_header, {
          %% Assumption: The internal representation of the block
          %% header is the same as the block header deserialized from
          %% the network, e.g. does not contain extra information.
          h :: header(),
          %% Total difficulty i.e. amount of work done from genesis to
          %% this block header.
          td :: work()}).

-type work() :: float(). %% TODO: Move to PoW-related module.

-type chain_header() :: #chain_header{}.

-type header_chain() :: header_chain_from_genesis()
                      | header_chain_without_genesis().
-type header_chain_from_genesis() :: [header(), ...]. %% First element is genesis.
-type header_chain_without_genesis() :: [header(), ...]. %% First element has lowest height.

-type top_header_db_key() :: ?TOP_HEADER. %% Only one key.
-type top_header_db_value() :: block_header_hash().
-type top_header_db() :: dict:dict(top_header_db_key(), top_header_db_value()).

-type headers_db_key() :: block_header_hash().
-type headers_db_value() :: chain_header().
-type headers_db() :: dict:dict(headers_db_key(), headers_db_value()).

-type blocks_db_key() :: block_header_hash().
-type blocks_db_value() ::
        aec_blocks:block_serialized_for_network(). %% Without state trees.
-type blocks_db() :: dict:dict(blocks_db_key(), blocks_db_value()).

-type db_handle() :: dict:dict().
-type db_key() :: binary().
-type db_value() :: binary().

%%%===================================================================
%%% API
%%%===================================================================

start_link(GenesisBlock) ->
    Args = [GenesisBlock],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

%% Returns the highest known block in the chain with its state trees.
%%
%% The highest known block may be lower than the highest block header
%% in the chain as returned by `top_header/0`.
-spec top() -> {ok, block()}.
top() ->
    {ok, BlockWithoutStateTrees = #block{}} = top_block(),
    BlockWithStateTrees = BlockWithoutStateTrees, %% TODO: Enrich block with state trees.
    {ok, BlockWithStateTrees}.

%% Returns the highest block header in the chain.
-spec top_header() -> do_top_header_reply().
top_header() ->
    %% TODO Store top header in ETS table so not to require server state.
    gen_server:call(?SERVER, {top_header},
                    ?DEFAULT_CALL_TIMEOUT).

%% Returns the highest known block in the chain.
%%
%% The highest known block may be lower than the highest block header
%% in the chain as returned by `top_header/0`.
-spec top_block() -> do_top_block_reply().
top_block() ->
    %% TODO Store top block in ETS table so not to require server state.
    gen_server:call(?SERVER, {top_block},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_header_by_hash(block_header_hash()) -> do_get_header_by_hash_reply().
get_header_by_hash(HeaderHash) ->
    gen_server:call(?SERVER, {get_header_by_hash, HeaderHash},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_block_by_hash(block_header_hash()) -> do_get_block_by_hash_reply().
get_block_by_hash(HeaderHash) ->
    gen_server:call(?SERVER, {get_block_by_hash, HeaderHash},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_header_by_height(height()) -> do_get_header_by_height_reply().
get_header_by_height(Height) ->
    gen_server:call(?SERVER, {get_header_by_height, Height},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_block_by_height(height()) -> do_get_block_by_height_reply().
get_block_by_height(Height) ->
    gen_server:call(?SERVER, {get_block_by_height, Height},
                    ?DEFAULT_CALL_TIMEOUT).

%% Insert in the chain the specified header if it is a successor of
%% the top header in the chain.
-spec insert_header(header()) -> do_insert_header_reply_ok() |
                                 do_insert_header_reply_error().
insert_header(Header) ->
    gen_server:call(?SERVER, {insert_header, Header},
                    ?DEFAULT_CALL_TIMEOUT).

%% Store the specified block if its header is in the chain.
-spec write_block(do_write_block_argument()) -> do_write_block_reply_ok() |
                                                do_write_block_reply_error().
write_block(Block) ->
    gen_server:call(?SERVER, {write_block, Block},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_work_at_top() -> do_get_work_at_top_reply().
get_work_at_top() ->
    gen_server:call(?SERVER, {get_work_at_top},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_work_by_hash_and_at_top(block_header_hash()) ->
                                         do_get_work_by_hash_and_at_top_reply().
get_work_by_hash_and_at_top(HeaderHash) ->
    gen_server:call(?SERVER, {get_work_by_hash_and_at_top, HeaderHash},
                    ?DEFAULT_CALL_TIMEOUT).

-spec has_more_work(header_chain()) ->
                           {ok, {boolean(), {{{top_chain_work, work()},
                                              {alt_chain_work, work()}},
                                             {top_header, header()}}}} |
                           {error, Reason} when
      Reason :: {no_common_ancestor, {top_header, header()}}
              | {different_genesis, {genesis_header, header()}}.
has_more_work(HeaderChain = [LowerHeader | _]) ->
    %% This function does not guarantee that the specified list of
    %% headers is a chain, i.e. it does not check fields height and
    %% previous hash in each header in relation to the previous
    %% header.
    case aec_headers:height(LowerHeader) of
        ?GENESIS_HEIGHT ->
            %% The specified chain is a chain complete from genesis.
            %% Check that the genesis is the expected one.
            {ok, GenesisHeader} = get_header_by_height(0),
            if
                LowerHeader =/= GenesisHeader ->
                    {error, {different_genesis, {genesis_header,
                                                 GenesisHeader}}};
                true ->
                    %% The specified chain does not require retrieving
                    %% any ancestor for computing the total work in
                    %% it.
                    {ok, AltChainWork} = work_in_header_chain(HeaderChain),
                    %% Retrieve the total work in the longest chain.
                    {ok, {TopChainWork, {top_header, TopHeader}}} =
                        get_work_at_top(),
                    {ok, {'work_op_>'(AltChainWork, TopChainWork),
                          {{{top_chain_work, TopChainWork},
                            {alt_chain_work, AltChainWork}},
                           {top_header, TopHeader}}}}
            end;
        LowerHeaderHeight when ?IS_HEIGHT_AFTER_GENESIS(LowerHeaderHeight) ->
            %% The specified chain does not start from genesis.
            %% Attempt to retrieve work of higher missing ancestor and
            %% work of top, so to compare work of top against work of
            %% specified chain.
            LowerHeaderPreviousHash = aec_headers:prev_hash(LowerHeader),
            case get_work_by_hash_and_at_top(LowerHeaderPreviousHash) of
                {error, {header_not_found, {top_header, TopHeader}}} ->
                    {error, {no_common_ancestor, {top_header, TopHeader}}};
                {ok, {{{work_at_hash, WorkBeforeLowerHeader},
                       {work_at_top, TopChainWork}},
                      {top_header, TopHeader}}} ->
                    {ok, WorkFromLowerHeader} =
                        work_in_header_chain(HeaderChain),
                    AltChainWork =
                        'work_op_+'(WorkBeforeLowerHeader, WorkFromLowerHeader),
                    {ok, {'work_op_>'(AltChainWork, TopChainWork),
                          {{{top_chain_work, TopChainWork},
                            {alt_chain_work, AltChainWork}},
                           {top_header, TopHeader}}}}
            end
    end.

%% Insert the specified sequence of headers in the chain, changing the
%% top header in the chain.
-spec force_insert_headers(header_chain()) ->
                                  do_force_insert_headers_reply_ok() |
                                  do_force_insert_headers_reply_error().
force_insert_headers(HeaderChain) ->
    gen_server:call(?SERVER, {force_insert_headers, HeaderChain},
                    ?DEFAULT_CALL_TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args = [GenesisBlock]) ->
    process_flag(trap_exit, true),

    %% Identify state of databases as before the start of this process.
    %% TODO Consider persisted state.
    TopHeaderDb = dict:new(),
    HeadersDb = dict:new(),
    BlocksDb = dict:new(),

    %% Compute initial state of databases.
    {ok, {NewTopHeaderDb, NewHeadersDb, NewBlocksDb}} =
        do_init([GenesisBlock], TopHeaderDb, HeadersDb, BlocksDb),

    %% Compute initial state of process.
    {ok, TopHeaderHash} = top_header_db_get(NewTopHeaderDb, ?TOP_HEADER),
    {ok, TopHeader} = headers_db_get(NewHeadersDb, TopHeaderHash),
    {ok, TopBlock} =
        do_find_highest_block_from_header_hash(TopHeaderHash,
                                               NewHeadersDb, NewBlocksDb),
    TopState = #top_state{top_header = TopHeader,
                          top_header_db = NewTopHeaderDb,
                          top_block = TopBlock},
    State = #state{top = TopState,
                   headers_db = NewHeadersDb,
                   blocks_db = NewBlocksDb},
    {ok, State}.

handle_call({top_header}, _From, State) ->
    Reply = do_top_header(State#state.top#top_state.top_header),
    {reply, Reply, State};
handle_call({top_block}, _From, State) ->
    Reply = do_top_block(State#state.top#top_state.top_block),
    {reply, Reply, State};
handle_call({get_header_by_hash,
             HeaderHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>},
            _From, State) ->
    Reply = do_get_header_by_hash(HeaderHash,
                                  State#state.top#top_state.top_header,
                                  State#state.headers_db),
    {reply, Reply, State};
handle_call({get_block_by_hash,
             HeaderHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>},
            _From, State) ->
    Reply = do_get_block_by_hash(HeaderHash,
                                 State#state.top#top_state.top_header,
                                 State#state.blocks_db),
    {reply, Reply, State};
handle_call({get_header_by_height, Height}, _From, State)
  when ?IS_HEIGHT(Height) ->
    Reply = do_get_header_by_height(Height,
                                    State#state.top#top_state.top_header,
                                    State#state.headers_db),
    {reply, Reply, State};
handle_call({get_block_by_height, Height}, _From, State)
  when ?IS_HEIGHT(Height) ->
    Reply = do_get_block_by_height(Height,
                                   State#state.top#top_state.top_header,
                                   State#state.headers_db,
                                   State#state.blocks_db),
    {reply, Reply, State};
handle_call({insert_header, Header}, _From, State) ->
    case
        do_insert_header(Header,
                         State#state.top#top_state.top_header,
                         State#state.top#top_state.top_header_db,
                         State#state.headers_db)
    of
        {error, _Reason} = Reply ->
            {reply, Reply, State};
        {ok, {Reply, {NewTopHeader, NewTopHeaderDb, NewHeadersDb}}} ->
            NewState =
                State#state{
                  top =
                      State#state.top#top_state{
                                    top_header = NewTopHeader,
                                    top_header_db = NewTopHeaderDb},
                  headers_db = NewHeadersDb},
            {reply, Reply, NewState}
    end;
handle_call({write_block, Block}, _From, State) ->
    case
        do_write_block(Block,
                       State#state.top#top_state.top_header,
                       State#state.top#top_state.top_block,
                       State#state.headers_db,
                       State#state.blocks_db)
    of
        {error, _Reason} = Reply ->
            {reply, Reply, State};
        {ok, {Reply, {NewTopBlock, NewBlocksDb}}} ->
            NewState =
                State#state{
                  top =
                      State#state.top#top_state{top_block = NewTopBlock},
                  blocks_db = NewBlocksDb},
            {reply, Reply, NewState}
    end;
handle_call({get_work_at_top}, _From, State) ->
    Reply = do_get_work_at_top(State#state.top#top_state.top_header),
    {reply, Reply, State};
handle_call({get_work_by_hash_and_at_top,
             HeaderHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>},
            _From, State) ->
    Reply = do_get_work_by_hash_and_at_top(HeaderHash,
                                           State#state.top#top_state.top_header,
                                           State#state.headers_db),
    {reply, Reply, State};
handle_call({force_insert_headers, HeaderChain = [_|_]}, _From, State) ->
    case
        do_force_insert_headers(HeaderChain,
                                State#state.top#top_state.top_header,
                                State#state.top#top_state.top_block,
                                State#state.top#top_state.top_header_db,
                                State#state.headers_db,
                                State#state.blocks_db)
    of
        {error, _Reason} = Reply ->
            {reply, Reply, State};
        {ok, {Reply, {NewTopHeader, NewTopBlock,
                      NewTopHeaderDb, NewHeadersDb, NewBlocksDb}}} ->
            NewState =
                State#state{
                  top =
                      State#state.top#top_state{
                                    top_header = NewTopHeader,
                                    top_header_db = NewTopHeaderDb,
                                    top_block = NewTopBlock},
                  headers_db = NewHeadersDb,
                  blocks_db = NewBlocksDb},
            {reply, Reply, NewState}
    end;
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% TODO Close databases.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec header_difficulty(header()) -> work().
header_difficulty(Header) ->
    aec_headers:linear_difficulty(Header).

-spec 'work_op_>'(work(), work()) -> boolean().
'work_op_>'(A, B) when ?IS_WORK(A), ?IS_WORK(B) ->
    A > B.

-spec 'work_op_+'(work(), work()) -> work().
'work_op_+'(A, B) when ?IS_WORK(A), ?IS_WORK(B) ->
    A + B.

-spec work_in_header_chain(header_chain()) -> {ok, work()}.
work_in_header_chain([Header]) ->
    case header_difficulty(Header) of
        W when ?IS_WORK(W) ->
            {ok, W}
    end;
work_in_header_chain([LowerHeader | OtherHeaders]) ->
    LowerHeaderWork = header_difficulty(LowerHeader),
    {ok, OtherHeadersWork} = work_in_header_chain(OtherHeaders),
    {ok, 'work_op_+'(LowerHeaderWork, OtherHeadersWork)}.

-spec header_chain_with_work(work(), header_chain_without_genesis()) ->
                                    [chain_header(), ...].
header_chain_with_work(WorkBeforeLowerHeader, HeaderChain) ->
    {_, ReversedHeaderChainWithWork} =
        lists:foldl(
          fun(H, {W, RHCW}) ->
                  D = header_difficulty(H),
                  HH = #chain_header{h = H, td = 'work_op_+'(W, D)},
                  {HH#chain_header.td, [HH | RHCW]}
          end,
          {WorkBeforeLowerHeader, []},
          HeaderChain),
    lists:reverse(ReversedHeaderChainWithWork).

-type is_header_chain_return_error() ::
        {error, Reason::{inconsistent_previous_hash, term()} |
                        {height_inconsistent_with_previous_height, term()}
        }.
-spec is_header_chain(header_chain()) -> ok | is_header_chain_return_error().
is_header_chain([LowestHeader | OtherHeaders]) ->
    {ok, LowestHeaderHash} =
        aec_headers:hash_internal_representation(LowestHeader),
    is_header_chain_1(LowestHeaderHash, aec_headers:height(LowestHeader),
                      OtherHeaders).

is_header_chain_1(<<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>, PreviousHeight, [])
  when ?IS_HEIGHT(PreviousHeight) ->
    ok;
is_header_chain_1(PreviousHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
                  PreviousHeight,
                  [LowestHeader | OtherHeaders])
  when ?IS_HEIGHT(PreviousHeight) ->
    LowestHeaderPrevHash = aec_headers:prev_hash(LowestHeader),
    ExpectedHeight = 1 + PreviousHeight,
    LowestHeaderHeight = aec_headers:height(LowestHeader),
    if
        LowestHeaderPrevHash =/= PreviousHash ->
            {error, {inconsistent_previous_hash,
                     {{actual_previous_hash, LowestHeaderPrevHash},
                      {expected_previous_hash, PreviousHash}}}};
        LowestHeaderHeight =/= ExpectedHeight ->
            {error, {height_inconsistent_with_previous_height,
                     {{actual_height, LowestHeaderHeight},
                      {expected_height, {ExpectedHeight}},
                      {previous_hash, LowestHeaderPrevHash}}}};
        true ->
            {ok, LowestHeaderHash} =
                aec_headers:hash_internal_representation(LowestHeader),
            is_header_chain_1(
              LowestHeaderHash, aec_headers:height(LowestHeader),
              OtherHeaders)
    end.

-spec is_header_chain_included(header_chain(), chain_header(), headers_db()) ->
                                      boolean().
is_header_chain_included(HeaderChain, TopHeader, HeadersDb) ->
    {ok, HeaderChainHash} =
        aec_headers:hash_internal_representation(lists:last(HeaderChain)),
    case do_find_header_hash_in_chain(HeaderChainHash, TopHeader, HeadersDb) of
        ok ->
            true;
        {error, not_found} ->
            false
    end.

chain_header_serialize(H) when ?IS_CHAIN_HEADER(H) ->
    term_to_binary(H).

chain_header_deserialize(B) when is_binary(B) ->
    case binary_to_term(B) of H when ?IS_CHAIN_HEADER(H) -> H end.

-type db_get_return(Value) :: {ok, Value} | {error, not_found}.
-spec db_get(db_handle(), db_key()) -> db_get_return(db_value()).
db_get(Db, K) ->
    case dict:find(K, Db) of
        {ok, V} ->
            {ok, V};
        error ->
            {error, not_found}
    end.

-type db_put_return() :: {ok, NewDb::db_handle()}.
-spec db_put(db_handle(), db_key(), db_value()) -> db_put_return().
db_put(Db, K, V) ->
    {ok, dict:store(K, V, Db)}.

-type db_delete_return() :: {ok, NewDb::db_handle()}.
-spec db_delete(db_handle(), db_key()) -> db_delete_return().
db_delete(Db, K) ->
    {ok, dict:erase(K, Db)}.

-spec top_header_db_get(db_handle(), top_header_db_key()) ->
                               db_get_return(top_header_db_value()).
top_header_db_get(Db, K = ?TOP_HEADER) ->
    db_get(Db, atom_to_binary(K, latin1)).

-spec top_header_db_put(db_handle(), top_header_db_key(), top_header_db_value()
                       ) -> db_put_return().
top_header_db_put(Db, K = ?TOP_HEADER, V) ->
    db_put(Db, atom_to_binary(K, latin1), V).

-spec headers_db_get(db_handle(), headers_db_key()) ->
                            db_get_return(headers_db_value()).
headers_db_get(Db, K) ->
    case db_get(Db, K) of
        {error, not_found} = Err ->
            Err;
        {ok, V} ->
            {ok, chain_header_deserialize(V)}
    end.

-spec headers_db_put(db_handle(), headers_db_key(), headers_db_value()) ->
                            db_put_return().
headers_db_put(Db, K, V) ->
    db_put(Db, K, chain_header_serialize(V)).

-spec headers_db_delete(db_handle(), headers_db_key()) ->
                               db_delete_return().
headers_db_delete(Db, K) ->
    db_delete(Db, K).

-spec blocks_db_get(db_handle(), blocks_db_key()) ->
                           db_get_return(blocks_db_value()).
blocks_db_get(Db, K) ->
    db_get(Db, K).

-spec blocks_db_put(db_handle(), blocks_db_key(), blocks_db_value()) ->
                           db_put_return().
blocks_db_put(Db, K, V) ->
    db_put(Db, K, V).

-spec blocks_db_delete(db_handle(), blocks_db_key()) ->
                              db_delete_return().
blocks_db_delete(Db, K) ->
    db_delete(Db, K).

do_init([GenesisBlock], TopHeaderDb, HeadersDb, BlocksDb) ->
    %% Hardcode expectations on specified genesis block.
    ok = do_init_check_genesis_block(GenesisBlock),

    {ok, SerializedGenesisBlock} =
        aec_blocks:serialize_for_network(GenesisBlock),
    GenesisHeader = aec_blocks:to_header(GenesisBlock),
    {ok, GenesisHeaderHash} =
        aec_headers:hash_internal_representation(GenesisHeader),

    %% Initialize databases.
    {ok, {NewTopHeaderDb, NewHeadersDb, NewBlocksDb}} =
        do_init_initialize_dbs(
          GenesisHeaderHash, GenesisHeader, SerializedGenesisBlock,
          TopHeaderDb, HeadersDb, BlocksDb),
    %% Hardcode expectation on initialized databases.
    ok = do_init_check_dbs(
           GenesisHeaderHash, GenesisHeader, SerializedGenesisBlock,
           NewTopHeaderDb, NewHeadersDb, NewBlocksDb),

    {ok, {NewTopHeaderDb, NewHeadersDb, NewBlocksDb}}.

do_init_check_genesis_block(GenesisBlock) ->
    ?GENESIS_HEIGHT = aec_blocks:height(GenesisBlock),
    <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>> = aec_blocks:prev_hash(GenesisBlock),
    Difficulty = header_difficulty(aec_blocks:to_header(GenesisBlock)),
    {true, _} = {?IS_WORK(Difficulty), Difficulty},
    ok.

do_init_initialize_dbs(GenesisHeaderHash, GenesisHeader, SerializedGenesisBlock,
                       TopHeaderDb, _HeadersDb, _BlocksDb) ->
    {NewTopHeaderDb, NewHeadersDb, NewBlocksDb} =
        case top_header_db_get(TopHeaderDb, ?TOP_HEADER) of
            %% TODO Handle `{ok, TopHeaderHash}`.
            {error, not_found} ->
                %% Re-initialize the databases considering the
                %% specified genesis block as the only element of the
                %% chain.
                %%
                %% Discard top header database. There should be at
                %% most one, key and that is absent, so the database
                %% shall be empty. Discard it anyway.
                TopHeaderDb1 = dict:new(),
                %% Discard headers database.
                HeadersDb1 = dict:new(),
                %% Discard blocks database.
                BlocksDb1 = dict:new(),
                {ok, TopHeaderDb2} =
                    top_header_db_put(TopHeaderDb1,
                                      ?TOP_HEADER, GenesisHeaderHash),
                {ok, HeadersDb2} =
                    headers_db_put(
                      HeadersDb1,
                      GenesisHeaderHash,
                      #chain_header{
                         h = GenesisHeader,
                         td = header_difficulty(GenesisHeader)}),
                {ok, BlocksDb2} =
                    blocks_db_put(BlocksDb1,
                                  GenesisHeaderHash, SerializedGenesisBlock),
                {TopHeaderDb2, HeadersDb2, BlocksDb2}
        end,
    {ok, {NewTopHeaderDb, NewHeadersDb, NewBlocksDb}}.

do_init_check_dbs(GenesisHeaderHash, GenesisHeader, SerializedGenesisBlock,
                  TopHeaderDb, HeadersDb, BlocksDb) ->
    %% Hardcode expectation on initialized databases: the chain leads
    %% to the specified genesis block.
    {ok, TopHeaderHash} = top_header_db_get(TopHeaderDb, ?TOP_HEADER),
    {ok, GenesisHeader} =
        do_find_genesis_header_from_header_hash(TopHeaderHash, HeadersDb),
    %% Hardcode expectation on initialized databases: the specified
    %% genesis block and its header are in the databases.
    {ok, #chain_header{h = GenesisHeader}} =
        headers_db_get(HeadersDb, GenesisHeaderHash),
    {ok, SerializedGenesisBlock} = blocks_db_get(BlocksDb, GenesisHeaderHash),
    ok.

do_find_genesis_header_from_header_hash(HeaderHash, HeadersDb) ->
    {ok, #chain_header{h = Header}} = headers_db_get(HeadersDb, HeaderHash),
    case aec_headers:height(Header) of
        ?GENESIS_HEIGHT ->
            {ok, Header};
        Height when ?IS_HEIGHT_AFTER_GENESIS(Height) ->
            do_find_genesis_header_from_header_hash(
              aec_headers:prev_hash(Header), Height - 1, HeadersDb)
    end.

do_find_genesis_header_from_header_hash(HeaderHash, Height = ?GENESIS_HEIGHT,
                                        HeadersDb) ->
    {ok, #chain_header{h = Header}} = headers_db_get(HeadersDb, HeaderHash),
    {Height, _} = {aec_headers:height(Header), Header},
    {ok, Header};
do_find_genesis_header_from_header_hash(HeaderHash, Height, HeadersDb) ->
    {ok, #chain_header{h = Header}} = headers_db_get(HeadersDb, HeaderHash),
    {Height, _} = {aec_headers:height(Header), Header},
    do_find_genesis_header_from_header_hash(
      aec_headers:prev_hash(Header), Height - 1, HeadersDb).

do_find_highest_block_from_header_hash(HeaderHash, HeadersDb, BlocksDb) ->
    case blocks_db_get(BlocksDb, HeaderHash) of
        {ok, SerializedBlock} ->
            {ok, _Block} =
                aec_blocks:deserialize_from_network(SerializedBlock);
        {error, not_found} ->
            {ok, #chain_header{h = Header}} =
                headers_db_get(HeadersDb, HeaderHash),
            %% Assumption: The specified header hash leads to genesis.
            %% Assumption: At least the genesis block is present.
            do_find_highest_block_from_header_hash(
              aec_headers:prev_hash(Header), HeadersDb, BlocksDb)
    end.

-type do_top_header_reply() :: {ok, header()}.
-spec do_top_header(chain_header()) -> do_top_header_reply().
do_top_header(TopHeader) ->
    {ok, TopHeader#chain_header.h}.

-type do_top_block_reply() ::
        {ok, aec_blocks:block_deserialized_from_network()}.
-spec do_top_block(aec_blocks:block_deserialized_from_network()) ->
                          do_top_block_reply().
do_top_block(TopBlock) ->
    {ok, TopBlock}.

-type do_get_header_by_hash_reply() ::
        {ok, header()} |
        {error, Reason::{header_not_found, {top_header, header()}}}.
-spec do_get_header_by_hash(block_header_hash(),
                            chain_header(), headers_db()) ->
                                   do_get_header_by_hash_reply().
do_get_header_by_hash(HeaderHash, TopHeader, HeadersDb) ->
    case headers_db_get(HeadersDb, HeaderHash) of
        {ok, #chain_header{h = Header}} ->
            {ok, Header};
        {error, not_found} ->
            {error, {header_not_found, {top_header, TopHeader#chain_header.h}}}
    end.

-type do_get_block_by_hash_reply() ::
        {ok, header()} |
        {error, Reason::{block_not_found, {top_header, header()}}}.
-spec do_get_block_by_hash(block_header_hash(), chain_header(), blocks_db()) ->
                                  do_get_block_by_hash_reply().
do_get_block_by_hash(HeaderHash, TopHeader, BlocksDb) ->
    case blocks_db_get(BlocksDb, HeaderHash) of
        {ok, SerializedBlock} ->
            {ok, _Block} = aec_blocks:deserialize_from_network(SerializedBlock);
        {error, not_found} ->
            {error, {block_not_found, {top_header, TopHeader#chain_header.h}}}
    end.

-type do_get_header_by_height_reply() ::
        {ok, header()} |
        {error, Reason::{chain_too_short, {{chain_height, height()},
                                           {top_header, header()}}}}.
-spec do_get_header_by_height(height(), chain_header(), headers_db()) ->
                                     do_get_header_by_height_reply().
do_get_header_by_height(Height, TopHeader, HeadersDb) ->
    ChainHeight = aec_headers:height(TopHeader#chain_header.h),
    if
        Height > ChainHeight ->
            {error, {chain_too_short, {{chain_height, ChainHeight},
                                       {top_header, TopHeader#chain_header.h}
                                      }
                    }};
        Height =:= ChainHeight ->
            {ok, TopHeader#chain_header.h};
        Height < ChainHeight ->
            do_get_past_header(ChainHeight - Height, TopHeader, HeadersDb)
    end.

do_get_past_header(Distance, CurrentHeader, HeadersDb)
  when is_integer(Distance), Distance > 0 ->
    PreviousHeaderHash = aec_headers:prev_hash(CurrentHeader#chain_header.h),
    {ok, PreviousHeader} = %% If not found, database is corrupt: fail.
        headers_db_get(HeadersDb, PreviousHeaderHash),
    case Distance of
        1 ->
            {ok, PreviousHeader#chain_header.h};
        _ ->
            do_get_past_header(Distance - 1, PreviousHeader, HeadersDb)
    end.

-type do_get_block_by_height_reply() ::
        {ok, block()} |
        {error, Reason::{chain_too_short, {{chain_height, height()},
                                           {top_header, header()}}} |
                        {block_not_found, {top_header, header()}}
        }.
-spec do_get_block_by_height(height(),
                             chain_header(), headers_db(), blocks_db()) ->
                                    do_get_block_by_height_reply().
do_get_block_by_height(Height, TopHeader, HeadersDb, BlocksDb) ->
    case do_get_header_by_height(Height, TopHeader, HeadersDb) of
        {error, {chain_too_short, _}} = Err ->
            Err;
        {ok, Header} ->
            {ok, HeaderHash} = aec_headers:hash_internal_representation(Header),
            case blocks_db_get(BlocksDb, HeaderHash) of
                {ok, SerializedBlock} ->
                    {ok, _Block} =
                        aec_blocks:deserialize_from_network(SerializedBlock);
                {error, not_found} ->
                    {error, {block_not_found, {top_header,
                                               TopHeader#chain_header.h}}}
            end
    end.

-type do_insert_header_reply_ok() :: ok.
-type do_insert_header_reply_error() ::
        {error, Reason::{previous_hash_is_not_top, {top_header, header()}} |
                        {height_inconsistent_with_previous_hash, {top_header,
                                                                  header()}}
        }.
-spec do_insert_header(header(),
                       chain_header(), top_header_db(), headers_db()) ->
                              do_insert_header_reply_error() |
                              {ok, {do_insert_header_reply_ok(), NewState}} when
      NewState :: {chain_header(), top_header_db(), headers_db()}.
do_insert_header(Header, TopHeader, TopHeaderDb, HeadersDb) ->
    HeaderDifficulty = header_difficulty(Header),
    %% Hardcode expectation on block header difficulty: a valid
    %% internal representation of block header will always comply.
    %% Check this explicitly in this manner in order to speed up
    %% investigation in case of failure in unit tests, and not to
    %% cause Dialyzer warnings.
    {true, _} = {?IS_WORK(HeaderDifficulty), {bad_difficulty,
                                              HeaderDifficulty}},
    {ok, TopHeaderHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>} =
        aec_headers:hash_internal_representation(TopHeader#chain_header.h),
    HeaderPreviousHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>> =
        aec_headers:prev_hash(Header),
    TopHeaderHeight = aec_headers:height(TopHeader#chain_header.h),
    ExpectedHeaderHeight = 1 + TopHeaderHeight,
    HeaderHeight = aec_headers:height(Header),
    if
        HeaderPreviousHash =/= TopHeaderHash ->
            _Reply =
                {error, {previous_hash_is_not_top, {top_header,
                                                    TopHeader#chain_header.h}}};
        HeaderHeight =/= ExpectedHeaderHeight ->
            _Reply =
                {error, {height_inconsistent_with_previous_hash,
                         {top_header, TopHeader#chain_header.h}}};
        true ->
            NewTopHeader =
                #chain_header{
                   h = Header,
                   td = 'work_op_+'(TopHeader#chain_header.td,
                                    HeaderDifficulty)},
            %% Ensure header is stored, then update top. In this order
            %% so that, if execution stops after storing header, the
            %% top header hash still refers to a a chain.
            {ok, HeaderHash} = aec_headers:hash_internal_representation(Header),
            %% As header is a successor of the current top, it should
            %% not be stored yet.  So store header without first
            %% checking that it is not yet stored.
            {ok, NewHeadersDb} =
                headers_db_put(HeadersDb, HeaderHash, NewTopHeader),
            {ok, NewTopHeaderDb} =
                top_header_db_put(TopHeaderDb, ?TOP_HEADER, HeaderHash),
            {ok, {_Reply = ok,
                  {NewTopHeader, NewTopHeaderDb, NewHeadersDb}}}
    end.

-type do_write_block_argument() ::
        block() | aec_blocks:block_deserialized_from_network().
-type do_write_block_reply_ok() :: ok.
-type do_write_block_reply_error() ::
        {error, Reason::{header_not_in_chain, {top_header, header()}} |
                        {block_already_stored, aec_blocks:block_serialized_for_network()}
        }.
-spec do_write_block(do_write_block_argument(),
                     chain_header(),
                     aec_blocks:block_deserialized_from_network(),
                     headers_db(), blocks_db()) ->
                            do_write_block_reply_error() |
                            {ok, {do_write_block_reply_ok(), NewState}} when
      NewState :: {aec_blocks:block_deserialized_from_network(), blocks_db()}.
do_write_block(Block, TopHeader, TopBlock, HeadersDb, BlocksDb) ->
    Header = aec_blocks:to_header(Block),
    {ok, HeaderHash} = aec_headers:hash_internal_representation(Header),
    case do_find_header_hash_in_chain(HeaderHash, TopHeader, HeadersDb) of
        {error, not_found} ->
            {error, {header_not_in_chain, {top_header,
                                           TopHeader#chain_header.h}}};
        ok ->
            case blocks_db_get(BlocksDb, HeaderHash) of
                {ok, SerializedBlock} ->
                    {error, {block_already_stored, SerializedBlock}};
                {error, not_found} ->
                    %% Store block.
                    {ok, SerializedBlock} =
                        aec_blocks:serialize_for_network(Block),
                    {ok, NewBlocksDb} =
                        blocks_db_put(BlocksDb, HeaderHash, SerializedBlock),

                    %% Determine new top block.
                    %%
                    %% There is no need to check the validity of the
                    %% height of the block, because the block
                    %% corresponds to a header already in the
                    %% chain. The consistency of the height of the
                    %% header was checked when inserting the header.
                    BlockHeight = aec_headers:height(Header),
                    TopBlockHeight = aec_blocks:height(TopBlock),
                    NewTopBlock =
                        if
                            BlockHeight > TopBlockHeight ->
                                %% Prefer the deserialized block to
                                %% the initial one, in order not to
                                %% keep track of state trees.
                                {ok, DeserializedBlock} =
                                    aec_blocks:deserialize_from_network(
                                      SerializedBlock),
                                DeserializedBlock;
                            true ->
                                TopBlock
                        end,
                    {ok, {_Reply = ok, {NewTopBlock, NewBlocksDb}}}
            end
    end.

do_find_header_hash_in_chain(HeaderHashToFind, TopHeader, HeadersDb) ->
    {ok, TopHeaderHash} =
        aec_headers:hash_internal_representation(TopHeader#chain_header.h),
    TopHeaderHeight = aec_headers:height(TopHeader#chain_header.h),
    if
        HeaderHashToFind =:= TopHeaderHash ->
            ok;
        TopHeaderHeight =:= ?GENESIS_HEIGHT ->
            {error, not_found};
        true ->
            do_find_header_hash_in_chain_1(
              HeaderHashToFind,
              aec_headers:prev_hash(TopHeader#chain_header.h),
              HeadersDb)
    end.

do_find_header_hash_in_chain_1(HeaderHashToFind, HeaderHashToFind, _) ->
    ok;
do_find_header_hash_in_chain_1(HeaderHashToFind, HeaderHash, HeadersDb) ->
    {ok, #chain_header{h = Header}} = headers_db_get(HeadersDb, HeaderHash),
    case aec_headers:height(Header) of
        ?GENESIS_HEIGHT ->
            {error, not_found};
        Height when ?IS_HEIGHT_AFTER_GENESIS(Height) ->
            do_find_header_hash_in_chain_1(
              HeaderHashToFind, aec_headers:prev_hash(Header), HeadersDb)
    end.

-type do_get_work_at_top_reply() ::
        {ok, {WorkAtTop::work(), {top_header, header()}}}.
-spec do_get_work_at_top(chain_header()) -> do_get_work_at_top_reply().
do_get_work_at_top(TopHeader) ->
    {ok, {TopHeader#chain_header.td, {top_header, TopHeader#chain_header.h}}}.

-type do_get_work_by_hash_and_at_top_reply() ::
        {ok, {ResultInfo::{{work_at_hash, work()},
                           {work_at_top, work()}},
              ResultContext::{top_header, header()}}} |
        {error, Reason::{header_not_found, {top_header, header()}}}.
-spec do_get_work_by_hash_and_at_top(block_header_hash(),
                                     chain_header(), headers_db()
                                    ) -> do_get_work_by_hash_and_at_top_reply().
do_get_work_by_hash_and_at_top(HeaderHash, TopHeader, HeadersDb) ->
    case headers_db_get(HeadersDb, HeaderHash) of
        {error, not_found} ->
            {error, {header_not_found, {top_header, TopHeader#chain_header.h}}};
        {ok, #chain_header{td = WorkAtHash}} ->
            {ok, {{{work_at_hash, WorkAtHash},
                   {work_at_top, TopHeader#chain_header.td}},
                  {top_header, TopHeader#chain_header.h}}}
    end.

-type do_force_insert_headers_reply_ok() :: {ok, {{old_top_header, header()},
                                                  {new_top_header, header()}}}.
-type do_force_insert_headers_reply_error() ::
        is_header_chain_return_error() |
        {error,
         Reason::{header_chain_already_included, term()} |
                 {no_common_ancestor, term()} |
                 {chain_does_not_have_more_work, {{{top_chain_work, work()},
                                                   {alt_chain_work, work()}},
                                                  {top_header, header()}}}
        }.
-spec do_force_insert_headers(
        header_chain(),
        chain_header(), aec_blocks:block_deserialized_from_network(),
        top_header_db(), headers_db(), blocks_db()
       ) -> do_force_insert_headers_reply_error() |
            {ok, {do_force_insert_headers_reply_ok(), NewState}} when
      NewState :: {chain_header(), aec_blocks:block_deserialized_from_network(),
                   top_header_db(), headers_db(), blocks_db()}.
do_force_insert_headers(HeaderChain,
                        TopHeader, TopBlock,
                        TopHeaderDb, HeadersDb, BlocksDb) ->
    case is_header_chain(HeaderChain) of
        {error, _Reason} = Err ->
            _Reply = Err;
        ok ->
            case is_header_chain_included(HeaderChain, TopHeader, HeadersDb) of
                true ->
                    _Reply =
                        {error, {header_chain_already_included,
                                 {top_header, TopHeader#chain_header.h}}};
                false -> %% At least part of the chain is not included.
                    case
                        do_find_highest_common_ancestor(HeaderChain,
                                                        TopHeader, HeadersDb)
                    of
                        {error, no_common_ancestor} ->
                            _Reply =
                                {error,
                                 {no_common_ancestor,
                                  {top_header, TopHeader#chain_header.h}}};
                        {ok, {{highest_common_ancestor,
                               HighestCommonAncestor},
                              {not_included_header_chain,
                               RelevantHeaderChainPortion =
                                   [LowestRelevantHeader | _]}}} ->
                            %% Hardcode expectation that lowest header
                            %% of relevant portion of the specified
                            %% header chain is not genesis, as it has
                            %% at least a predecessor (the highest
                            %% common ancestor).
                            case aec_headers:height(LowestRelevantHeader) of
                                H when ?IS_HEIGHT_AFTER_GENESIS(H) ->
                                    do_force_insert_headers_1(
                                      RelevantHeaderChainPortion,
                                      HighestCommonAncestor,
                                      TopHeader, TopBlock,
                                      TopHeaderDb, HeadersDb, BlocksDb)
                            end
                    end
            end
    end.

-spec do_force_insert_headers_1(
        header_chain_without_genesis(),
        chain_header(),
        chain_header(), aec_blocks:block_deserialized_from_network(),
        top_header_db(), headers_db(), blocks_db()
       ) -> {error, Reason::{chain_does_not_have_more_work, term()}} |
            {ok, {do_force_insert_headers_reply_ok(), NewState}} when
      NewState :: {chain_header(), aec_blocks:block_deserialized_from_network(),
                   top_header_db(), headers_db(), blocks_db()}.
do_force_insert_headers_1(HeaderChain = [_|_], %% Above common ancestor.
                          CommonAncestor,
                          OldTopHeader, _TopBlock,
                          TopHeaderDb, HeadersDb, BlocksDb) ->
    HeaderChainWithWork =
        header_chain_with_work(CommonAncestor#chain_header.td, HeaderChain),
    NewTopHeader = lists:last(HeaderChainWithWork),
    case 'work_op_>'(NewTopHeader#chain_header.td,
                     OldTopHeader#chain_header.td) of
        false ->
            _Reply =
                {error, {chain_does_not_have_more_work,
                         {{{top_chain_work, OldTopHeader#chain_header.td},
                           {alt_chain_work, NewTopHeader#chain_header.td}},
                          {top_header, OldTopHeader#chain_header.h}}}};
        true ->
            {ok, NewTopHeaderHash} =
                aec_headers:hash_internal_representation(
                  NewTopHeader#chain_header.h),
            {ok, CommonAncestorHash} =
                aec_headers:hash_internal_representation(
                  CommonAncestor#chain_header.h),
            %% Ensure headers above common ancestor are stored, then
            %% update top, then delete unused blocks (in old chain
            %% above highest common ancestor), then delete unused
            %% headers (in old chain above highest common
            %% ancestor). In this order so that, if execution stops,
            %% the top header hash still refers to a chain, and each
            %% block has its header stored.
            %%
            %% As the headers belong to a distinct chain than the
            %% current top, they should not be stored yet.  So store
            %% each header without first checking that it is not yet
            %% stored.
            HeadersDbWithNewHeaders =
                lists:foldl(
                  fun(H, HsDb) ->
                          {ok, HH} =
                              aec_headers:hash_internal_representation(
                                H#chain_header.h),
                          {ok, NewHsDb} = headers_db_put(HsDb, HH, H),
                          NewHsDb
                  end,
                  HeadersDb,
                  HeaderChainWithWork),
            {ok, NewTopHeaderDb} =
                top_header_db_put(TopHeaderDb, ?TOP_HEADER, NewTopHeaderHash),
            {ok, {NewHeadersDb, NewBlocksDb}} =
                do_delete_headers_and_blocks_from_header_to_header_hash(
                  OldTopHeader, %% To delete.
                  CommonAncestorHash, %% Not to delete.
                  HeadersDbWithNewHeaders, BlocksDb),
            {ok, NewTopBlock} =
                do_find_highest_block_from_header_hash(NewTopHeaderHash,
                                                       NewHeadersDb, NewBlocksDb),
            {ok, {_Reply = {ok, {{old_top_header, OldTopHeader#chain_header.h},
                                 {new_top_header, NewTopHeader#chain_header.h}}},
                  {NewTopHeader, NewTopBlock,
                   NewTopHeaderDb, NewHeadersDb, NewBlocksDb}}}
    end.

do_delete_headers_and_blocks_from_header_to_header_hash(
  HighIncludedHeader, LowExcludedHeaderHash,
  HeadersDb, BlocksDb) ->
    {ok, HighIncludedHeaderHash} =
        aec_headers:hash_internal_representation(
          HighIncludedHeader#chain_header.h),
    {ok, NewBlocksDb} = blocks_db_delete(BlocksDb, HighIncludedHeaderHash),
    {ok, NewHeadersDb} = headers_db_delete(HeadersDb, HighIncludedHeaderHash),
    case aec_headers:prev_hash(HighIncludedHeader#chain_header.h) of
        LowExcludedHeaderHash ->
            {ok, {NewHeadersDb, NewBlocksDb}};
        PrevHeaderHash ->
            {ok, PrevHeader} = headers_db_get(HeadersDb, PrevHeaderHash),
            do_delete_headers_and_blocks_from_header_to_header_hash(
              PrevHeader, LowExcludedHeaderHash,
              NewHeadersDb, NewBlocksDb)
    end.

-spec do_find_highest_common_ancestor(
        header_chain(), %% At least part of the first chain must be not included...
        chain_header(), headers_db() %% ... in the second chain.
       ) -> {ok, {{highest_common_ancestor, chain_header()},
                  {not_included_header_chain, header_chain_without_genesis()}}
            } |
            {error, no_common_ancestor}.
do_find_highest_common_ancestor(HC, TopHeader, HeadersDb) ->
    %% The highest common ancestor cannot be higher than the minimum
    %% height of the two chains.
    [HighestHCHeader | ReversedHCRest] = lists:reverse(HC),
    HCHeight = aec_headers:height(HighestHCHeader),
    TopHeight = aec_headers:height(TopHeader#chain_header.h),
    if
        HCHeight =< TopHeight ->
            %% Hardcode expectation on at least part of the chain
            %% being not included.  Compare block headers at highest
            %% common height.
            {ok, H} = do_get_header_by_height(HCHeight, TopHeader, HeadersDb),
            case
                {aec_headers:hash_internal_representation(HighestHCHeader),
                 aec_headers:hash_internal_representation(H)}
            of
                {{ok, HighestHCHeaderHash}, {ok, HH}}
                  when HighestHCHeaderHash =/= HH ->
                    ok
            end,
            do_find_highest_common_ancestor_1(
              {aec_headers:prev_hash(HighestHCHeader),
               aec_headers:height(HighestHCHeader),
               ReversedHCRest},
              {aec_headers:prev_hash(H), aec_headers:height(H), HeadersDb},
              _NotIncludedHCAcc0 = [HighestHCHeader]);
        HCHeight > TopHeight ->
            %% Being higher, of course at least part of the chain is
            %% not included.
            {ok, TopHeaderHash} = aec_headers:hash_internal_representation(
                                    TopHeader#chain_header.h),
            do_find_highest_common_ancestor_2(
              {aec_headers:prev_hash(HighestHCHeader),
               aec_headers:height(HighestHCHeader),
               ReversedHCRest},
              {TopHeaderHash,
               1 + aec_headers:height(TopHeader#chain_header.h),
               HeadersDb},
              _NotIncludedHCAcc0 = [HighestHCHeader])
    end.

%% The first specified chain may be without genesis. The second
%% specified chain arrives at genesis. The two specified chains may
%% refer to distinct genesis block headers.
-spec do_find_highest_common_ancestor_1(
        FirstChain::term(), SecondChain::term(),
        NotIncludedHeaderChainAccumulator::header_chain()) -> term().
do_find_highest_common_ancestor_1({_, H, []}, {_, H = ?GENESIS_HEIGHT, _}, _) ->
    %% Distinct genesis block headers.
    {error, no_common_ancestor};
do_find_highest_common_ancestor_1({HCPrevHash, H, []}, {HCPrevHash, H, HeadersDb}, NotIncludedHC)
  when ?IS_HEIGHT_AFTER_GENESIS(H) ->
    %% The first chain is without genesis.
    %%
    %% The highest common ancestor is the header that is not in the
    %% first chain, but that is the previous header of the lowest
    %% header in the first chain.
    {ok, Header2} = headers_db_get(HeadersDb, HCPrevHash),
    {ok, {{highest_common_ancestor, Header2},
          {not_included_header_chain, NotIncludedHC}}};
do_find_highest_common_ancestor_1({_, H, []}, {_, H, _}, _)
  when ?IS_HEIGHT_AFTER_GENESIS(H) ->
    %% The first chain is without genesis.
    %%
    %% No known common ancestors - not even the previous header of the
    %% lowest header in the first chain.
    {error, no_common_ancestor};
do_find_highest_common_ancestor_1({HCPrevHash, H, [HighestHCHeader | _]}, {HCPrevHash, H, HeadersDb}, NotIncludedHC)
  when ?IS_HEIGHT_AFTER_GENESIS(H) ->
    %% The highest common ancestor is the identified header, in the
    %% first chain.
    %%
    %% Hardcode expectation that the ancestor is the same in both chains.
    {ok, Header2 = #chain_header{h = HighestHCHeader}} =
        headers_db_get(HeadersDb, HCPrevHash),
    {ok, {{highest_common_ancestor, Header2},
          {not_included_header_chain, NotIncludedHC}}};
do_find_highest_common_ancestor_1({_, H, [HighestHCHeader | HCRest]}, {PrevHash, H, HeadersDb}, NotIncludedHC)
  when ?IS_HEIGHT_AFTER_GENESIS(H) ->
    %% Still looking for the highest common ancestor.  Go down both
    %% chains.
    {ok, #chain_header{h = Header2}} = headers_db_get(HeadersDb, PrevHash),
    do_find_highest_common_ancestor_1(
      {aec_headers:prev_hash(HighestHCHeader),
       aec_headers:height(HighestHCHeader),
       HCRest},
      {aec_headers:prev_hash(Header2), aec_headers:height(Header2), HeadersDb},
      [HighestHCHeader | NotIncludedHC]).

-spec do_find_highest_common_ancestor_2(
        FirstChain::term(), SecondChain::term(),
        NotIncludedHeaderChainAccumulator::header_chain()) -> term().
do_find_highest_common_ancestor_2({_, H1, [HighestHCHeader | HCRest]}, C2 = {_, H2, _}, NotIncludedHC)
  when ?IS_HEIGHT(H1), ?IS_HEIGHT(H2), H1 > H2 ->
    %% Still higher on first chain than on second chain.  Go down
    %% first chain.
    do_find_highest_common_ancestor_2(
      {aec_headers:prev_hash(HighestHCHeader),
       aec_headers:height(HighestHCHeader),
       HCRest},
      C2,
      [HighestHCHeader | NotIncludedHC]
     );
do_find_highest_common_ancestor_2({_, H1, []}, {_, H2, _}, _)
  when ?IS_HEIGHT(H1), ?IS_HEIGHT(H2), H1 > H2 ->
    %% No known common ancestors.
    {error, no_common_ancestor};
do_find_highest_common_ancestor_2(C1 = {_, H, _}, C2 = {_, H, _}, NotIncludedHC)
  when ?IS_HEIGHT(H) ->
    %% The two chains have a block at the same height.  Reuse other function.
    do_find_highest_common_ancestor_1(C1, C2, NotIncludedHC).
