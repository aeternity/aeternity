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
         write_block/1]).
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

-define(TOP_HEADER, top_header).

-record(top_state,
        {top_header :: header(),
         top_header_db :: top_header_db(),
         top_block :: aec_blocks:block_deserialized_from_network()}). %% Without state trees.
-record(state, {top :: #top_state{},
                headers_db :: headers_db(),
                blocks_db :: blocks_db()}).

-type top_header_db() :: dict:dict(
                           ?TOP_HEADER, %% Only one key.
                           block_header_hash()).
-type headers_db() :: dict:dict(
                        block_header_hash(),
                        aec_headers:block_header_serialized_for_network()).
-type blocks_db() :: dict:dict(
                       block_header_hash(),
                       aec_blocks:block_serialized_for_network()). %% Without state trees.

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
-spec top_header() -> {ok, header()}.
top_header() ->
    %% TODO Store top header in ETS table so not to require server state.
    gen_server:call(?SERVER, {top_header},
                    ?DEFAULT_CALL_TIMEOUT).

%% Returns the highest known block in the chain.
%%
%% The highest known block may be lower than the highest block header
%% in the chain as returned by `top_header/0`.
-spec top_block() -> {ok, aec_blocks:block_deserialized_from_network()}.
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
    {ok, SerializedTopHeader} = headers_db_get(NewHeadersDb, TopHeaderHash),
    {ok, TopHeader} = aec_headers:deserialize_from_network(SerializedTopHeader),
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
    Reply = {ok, State#state.top#top_state.top_header},
    {reply, Reply, State};
handle_call({top_block}, _From, State) ->
    Reply = {ok, State#state.top#top_state.top_block},
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
        {ok, {Reply, {NewTopHeaderDb, NewHeadersDb}}} ->
            NewState =
                State#state{
                  top =
                      State#state.top#top_state{top_header = Header,
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

db_get(Db, K) ->
    case dict:find(K, Db) of
        {ok, V} ->
            {ok, V};
        error ->
            {error, not_found}
    end.

db_put(Db, K, V) ->
    {ok, _NewDb = dict:store(K, V, Db)}.

top_header_db_get(Db, K = ?TOP_HEADER) ->
    db_get(Db, K).

top_header_db_put(Db, K = ?TOP_HEADER, V) ->
    db_put(Db, K, V).

headers_db_get(Db, K) ->
    db_get(Db, K).

headers_db_put(Db, K, V) ->
    db_put(Db, K, V).

blocks_db_get(Db, K) ->
    db_get(Db, K).

blocks_db_put(Db, K, V) ->
    db_put(Db, K, V).

do_init([GenesisBlock], TopHeaderDb, _HeadersDb, _BlocksDb) ->
    %% Hardcode expectations on specified genesis block.
    ?GENESIS_HEIGHT = aec_blocks:height(GenesisBlock),
    <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>> = aec_blocks:prev_hash(GenesisBlock),

    %% Compute genesis serializations and hash.
    {ok, SerializedGenesisBlock} =
        aec_blocks:serialize_for_network(GenesisBlock),
    {ok, SerializedGenesisHeader} =
        aec_headers:serialize_for_network(
          aec_blocks:to_header(GenesisBlock)),
    {ok, GenesisHeaderHash} =
        aec_headers:hash_network_serialization(
          SerializedGenesisHeader),

    %% Initialize databases.
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
                    headers_db_put(HeadersDb1,
                                   GenesisHeaderHash, SerializedGenesisHeader),
                {ok, BlocksDb2} =
                    blocks_db_put(BlocksDb1,
                                  GenesisHeaderHash, SerializedGenesisBlock),
                {TopHeaderDb2, HeadersDb2, BlocksDb2}
        end,

    %% Hardcode expectation on initialized databases: the chain leads
    %% to the specified genesis block.
    {ok, TopHeaderHash} = top_header_db_get(NewTopHeaderDb, ?TOP_HEADER),
    {ok, SerializedGenesisHeader} =
        do_find_genesis_header_from_header_hash(TopHeaderHash, NewHeadersDb),
    %% Hardcode expectation on initialized databases: the specified
    %% genesis block and its header are in the databases.
    {ok, SerializedGenesisHeader} =
        headers_db_get(NewHeadersDb, GenesisHeaderHash),
    {ok, SerializedGenesisBlock} =
        blocks_db_get(NewBlocksDb, GenesisHeaderHash),

    {ok, {NewTopHeaderDb, NewHeadersDb, NewBlocksDb}}.

do_find_genesis_header_from_header_hash(HeaderHash, HeadersDb) ->
    {ok, SerializedHeader} = headers_db_get(HeadersDb, HeaderHash),
    {ok, Header} = aec_headers:deserialize_from_network(SerializedHeader),
    case aec_headers:height(Header) of
        ?GENESIS_HEIGHT ->
            {ok, SerializedHeader};
        Height when ?IS_HEIGHT_AFTER_GENESIS(Height) ->
            do_find_genesis_header_from_header_hash(
              aec_headers:prev_hash(Header), Height - 1, HeadersDb)
    end.

do_find_genesis_header_from_header_hash(HeaderHash, Height = ?GENESIS_HEIGHT,
                                        HeadersDb) ->
    {ok, SerializedHeader} = headers_db_get(HeadersDb, HeaderHash),
    {ok, Header} = aec_headers:deserialize_from_network(SerializedHeader),
    {Height, _} = {aec_headers:height(Header), Header},
    {ok, SerializedHeader};
do_find_genesis_header_from_header_hash(HeaderHash, Height, HeadersDb) ->
    {ok, SerializedHeader} = headers_db_get(HeadersDb, HeaderHash),
    {ok, Header} = aec_headers:deserialize_from_network(SerializedHeader),
    {Height, _} = {aec_headers:height(Header), Header},
    do_find_genesis_header_from_header_hash(
      aec_headers:prev_hash(Header), Height - 1, HeadersDb).

-type do_get_header_by_hash_reply() ::
        {ok, header()} |
        {error, Reason::{header_not_found, {top_header, header()}}}.
-spec do_get_header_by_hash(block_header_hash(), header(), headers_db()) ->
                                   do_get_header_by_hash_reply().
do_get_header_by_hash(HeaderHash, TopHeader, HeadersDb) ->
    case headers_db_get(HeadersDb, HeaderHash) of
        {ok, SerializedHeader} ->
            {ok, _Header} =
                aec_headers:deserialize_from_network(SerializedHeader);
        {error, not_found} ->
            {error, {header_not_found, {top_header, TopHeader}}}
    end.

-type do_get_block_by_hash_reply() ::
        {ok, header()} |
        {error, Reason::{block_not_found, {top_header, header()}}}.
-spec do_get_block_by_hash(block_header_hash(), header(), blocks_db()) ->
                                  do_get_block_by_hash_reply().
do_get_block_by_hash(HeaderHash, TopHeader, BlocksDb) ->
    case blocks_db_get(BlocksDb, HeaderHash) of
        {ok, SerializedBlock} ->
            {ok, _Block} = aec_blocks:deserialize_from_network(SerializedBlock);
        {error, not_found} ->
            {error, {block_not_found, {top_header, TopHeader}}}
    end.

-type do_get_header_by_height_reply() ::
        {ok, header()} |
        {error, Reason::{chain_too_short, {{chain_height, height()},
                                           {top_header, header()}}}}.
-spec do_get_header_by_height(height(), header(), headers_db()) ->
                                     do_get_header_by_height_reply().
do_get_header_by_height(Height, TopHeader, HeadersDb) ->
    ChainHeight = aec_headers:height(TopHeader),
    if
        Height > ChainHeight ->
            {error, {chain_too_short, {{chain_height, ChainHeight},
                                       {top_header, TopHeader}
                                      }
                    }};
        Height =:= ChainHeight ->
            {ok, TopHeader};
        Height < ChainHeight ->
            do_get_past_header(ChainHeight - Height, TopHeader, HeadersDb)
    end.

do_get_past_header(Distance, CurrentHeader, HeadersDb)
  when is_integer(Distance), Distance > 0 ->
    PreviousHeaderHash = aec_headers:prev_hash(CurrentHeader),
    {ok, SerializedPreviousHeader} = %% If not found, database is corrupt: fail.
        headers_db_get(HeadersDb, PreviousHeaderHash),
    {ok, PreviousHeader} =
        aec_headers:deserialize_from_network(SerializedPreviousHeader),
    case Distance of
        1 ->
            {ok, PreviousHeader};
        _ ->
            do_get_past_header(Distance - 1, PreviousHeader, HeadersDb)
    end.

-type do_get_block_by_height_reply() ::
        {ok, block()} |
        {error, Reason::{chain_too_short, {{chain_height, height()},
                                           {top_header, header()}}} |
                        {block_not_found, {top_header, header()}}
        }.
-spec do_get_block_by_height(height(), header(), headers_db(), blocks_db()) ->
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
                    {error, {block_not_found, {top_header, TopHeader}}}
            end
    end.

-type do_insert_header_reply_ok() :: ok.
-type do_insert_header_reply_error() ::
        {error, Reason::{previous_hash_is_not_top, {top_header, header()}} |
                        {height_inconsistent_with_previous_hash, {top_header,
                                                                  header()}}
        }.
-spec do_insert_header(header(), header(), top_header_db(), headers_db()) ->
                              do_insert_header_reply_error() |
                              {ok, {do_insert_header_reply_ok(), NewState}} when
      NewState :: {top_header_db(), headers_db()}.
do_insert_header(Header, TopHeader, TopHeaderDb, HeadersDb) ->
    {ok, TopHeaderHash} = aec_headers:hash_internal_representation(TopHeader),
    case aec_headers:prev_hash(Header) of
        TopHeaderHash ->
            TopHeaderHeight = aec_headers:height(TopHeader),
            HeaderHeight = aec_headers:height(Header),
            case {HeaderHeight, 1 + TopHeaderHeight} of
                {HH, HH} ->
                    %% Ensure header is stored, then update top. In
                    %% this order so that, if execution stops after
                    %% storing header, the top header hash still
                    %% refers to a a chain.
                    {ok, SerializedHeader} =
                        aec_headers:serialize_for_network(Header),
                    {ok, HeaderHash} =
                        aec_headers:hash_network_serialization(SerializedHeader),
                    %% As header is a successor of the current top, it
                    %% should not be stored yet.  So store header
                    %% without first checking that it is not yet
                    %% stored.
                    {ok, NewHeadersDb} =
                        headers_db_put(HeadersDb, HeaderHash, SerializedHeader),
                    {ok, NewTopHeaderDb} =
                        top_header_db_put(TopHeaderDb, ?TOP_HEADER, HeaderHash),
                    {ok, {_Reply = ok, {NewTopHeaderDb, NewHeadersDb}}};
                {HeaderHeight, _} when ?IS_HEIGHT(HeaderHeight) ->
                    {error, {height_inconsistent_with_previous_hash,
                             {top_header, TopHeader}}}
            end;
        <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>> ->
            _Reply =
                {error, {previous_hash_is_not_top, {top_header, TopHeader}}}
    end.

-type do_write_block_argument() ::
        block() | aec_blocks:block_deserialized_from_network().
-type do_write_block_reply_ok() :: ok.
-type do_write_block_reply_error() ::
        {error, Reason::{header_not_in_chain, {top_header, header()}} |
                        {block_already_stored, aec_blocks:block_serialized_for_network()}
        }.
-spec do_write_block(do_write_block_argument(),
                     header(), aec_blocks:block_deserialized_from_network(),
                     headers_db(), blocks_db()) ->
                            do_write_block_reply_error() |
                            {ok, {do_write_block_reply_ok(), NewState}} when
      NewState :: {aec_blocks:block_deserialized_from_network(), blocks_db()}.
do_write_block(Block, TopHeader, TopBlock, HeadersDb, BlocksDb) ->
    Header = aec_blocks:to_header(Block),
    {ok, SerializedHeader} = aec_headers:serialize_for_network(Header),
    {ok, HeaderHash} = aec_headers:hash_network_serialization(
                         SerializedHeader),
    case do_find_header_hash_in_chain(HeaderHash, TopHeader, HeadersDb) of
        {error, not_found} ->
            {error, {header_not_in_chain, {top_header, TopHeader}}};
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
    {ok, TopHeaderHash} = aec_headers:hash_internal_representation(TopHeader),
    TopHeaderHeight = aec_headers:height(TopHeader),
    if
        HeaderHashToFind =:= TopHeaderHash ->
            ok;
        TopHeaderHeight =:= ?GENESIS_HEIGHT ->
            {error, not_found};
        true ->
            do_find_header_hash_in_chain_1(
              HeaderHashToFind, aec_headers:prev_hash(TopHeader), HeadersDb)
    end.

do_find_header_hash_in_chain_1(HeaderHashToFind, HeaderHashToFind, _) ->
    ok;
do_find_header_hash_in_chain_1(HeaderHashToFind, HeaderHash, HeadersDb) ->
    {ok, SerializedHeader} = headers_db_get(HeadersDb, HeaderHash),
    {ok, Header} = aec_headers:deserialize_from_network(SerializedHeader),
    case aec_headers:height(Header) of
        ?GENESIS_HEIGHT ->
            {error, not_found};
        Height when ?IS_HEIGHT_AFTER_GENESIS(Height) ->
            do_find_header_hash_in_chain_1(
              HeaderHashToFind, aec_headers:prev_hash(Header), HeadersDb)
    end.
