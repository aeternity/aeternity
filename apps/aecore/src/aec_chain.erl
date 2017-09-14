%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Service holding the chain of block headers and blocks.
%%%
%%% @TODO Unit testing of unhappy paths.
%%% @TODO Forced chain (fork).
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
         get_work_at_top/0]).
%% TODO `force_insert_headers`: Insert the specified sequence of headers in the chain, potentially changing the top header in the chain.

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
    {ok, SerializedTopBlock} = blocks_db_get(NewBlocksDb, TopHeaderHash),
    {ok, TopBlock} = aec_blocks:deserialize_from_network(SerializedTopBlock),
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

-spec blocks_db_get(db_handle(), blocks_db_key()) ->
                           db_get_return(blocks_db_value()).
blocks_db_get(Db, K) ->
    db_get(Db, K).

-spec blocks_db_put(db_handle(), blocks_db_key(), blocks_db_value()) ->
                           db_put_return().
blocks_db_put(Db, K, V) ->
    db_put(Db, K, V).

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
                   td = TopHeader#chain_header.td + HeaderDifficulty},
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
