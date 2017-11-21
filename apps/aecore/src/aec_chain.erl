%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Service holding the longest chain of block headers and blocks.
%%%
%%% The longest chain is determined according to the amount of work
%%% done on the chain, i.e. according to the total difficulty of the
%%% highest header in the chain (the so-called "top" header).  The
%%% total difficulty of a header is the sum of the difficulty of the
%%% header and the total difficulty of the previous header.
%%%
%%% The difficulty of a header is a linear representation of the
%%% expected average amount of work required for mining the header,
%%% and is derived from the target threshold in the header.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain).

-export([start_link/0,
         start_link/1,
         stop/0]).

%% API
-export([common_ancestor/2,
         get_block_by_hash/1,
         get_block_by_height/1,
         get_header_by_hash/1,
         get_header_by_height/1,
         get_total_difficulty/0,
         get_transactions_between/2,
         insert_header/1,
         top/0,
         top_block_hash/0,
         top_header/0,
         write_block/1 %% TODO: rename
	]).

-include("common.hrl").
-include("blocks.hrl").

-define(CHAIN_SERVER, aec_chain_server).
-define(DEFAULT_CALL_TIMEOUT, infinity).

-type work() :: float(). %% TODO: Move to PoW-related module.

-type top_header_reply() :: {ok, header()}.
-type get_header_by_hash_reply() ::
        {ok, header()} |
        {error, Reason::{header_not_found, {top_header, header()}}}.

-type get_block_by_hash_reply() ::
        {ok, block()} |
        {error, Reason::{block_not_found, {top_header, header()}}}.
-type get_header_by_height_reply() ::
        {ok, header()} | {error, atom()}.
-type get_block_by_height_reply() ::
        {ok, block()} | {error, atom()}.
-type insert_header_reply_ok() :: ok.
-type insert_header_reply_error() ::
        {error, Reason::{previous_hash_is_not_top, {top_header, header()}} |
                        {height_inconsistent_with_previous_hash, {top_header,
                                                                  header()}}
        }.
-type write_block_argument() ::
        block() | aec_blocks:block_deserialized_from_network().
-type write_block_reply_ok() :: ok.
-type write_block_reply_error() ::
        {error, Reason::{header_not_in_chain, {top_header, header()}} |
                        {block_already_stored, aec_blocks:block_serialized_for_network()}
        }.
-type get_total_difficulty_reply() ::
        {ok, {WorkAtTop::work(), {top_header, header()}}}.

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    case aec_persistence:get_chain() of
        [] ->
            GB = aec_block_genesis:genesis_block(),
            aec_chain_server:start_link(GB);
        Chain ->
            %% TODO: This needs protection against not finding the block.
            Hash = aec_persistence:get_top_block(),
            TopState = aec_persistence:get_block_state(Hash),
            aec_chain_server:start_link({Chain, Hash, TopState})
    end.

start_link(GenesisBlock) ->
    aec_chain_server:start_link(GenesisBlock).

stop() ->
    aec_chain_server:stop().

%% Returns the highest known block in the chain with its state trees
%%
%% The highest known block may be lower than the highest block header
%% in the chain as returned by `top_header/0`.
-spec top() -> {ok, block()}.
top() -> gen_server:call(?CHAIN_SERVER, top, ?DEFAULT_CALL_TIMEOUT).

-spec top_block_hash() -> binary() | 'undefined'.
top_block_hash() ->
    gen_server:call(?CHAIN_SERVER, top_block_hash, ?DEFAULT_CALL_TIMEOUT).

%% Returns the highest block header in the chain.
-spec top_header() -> top_header_reply().
top_header() -> gen_server:call(?CHAIN_SERVER, top_header,
				?DEFAULT_CALL_TIMEOUT).


-spec get_header_by_hash(block_header_hash()) -> get_header_by_hash_reply().
get_header_by_hash(HeaderHash) ->
    gen_server:call(?CHAIN_SERVER, {get_header, HeaderHash},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_block_by_hash(block_header_hash()) -> get_block_by_hash_reply().
get_block_by_hash(HeaderHash) ->
    gen_server:call(?CHAIN_SERVER, {get_block, HeaderHash},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_header_by_height(height()) -> get_header_by_height_reply().
get_header_by_height(Height) ->
    gen_server:call(?CHAIN_SERVER, {get_header_by_height, Height},
                    ?DEFAULT_CALL_TIMEOUT).

-spec get_block_by_height(height()) -> get_block_by_height_reply().
get_block_by_height(Height) ->
    gen_server:call(?CHAIN_SERVER, {get_block_by_height, Height},
                    ?DEFAULT_CALL_TIMEOUT).

%% Insert in the chain the specified header if it is a successor of
%% the top header in the chain.
-spec insert_header(header()) -> insert_header_reply_ok() |
                                 insert_header_reply_error().
insert_header(Header) ->
    gen_server:call(?CHAIN_SERVER, {insert_header, Header},
                    ?DEFAULT_CALL_TIMEOUT).

%% Store the specified block if its header is in the chain.
-spec write_block(write_block_argument()) -> write_block_reply_ok() |
                                                write_block_reply_error().
write_block(Block) ->
    gen_server:call(?CHAIN_SERVER, {write_block, Block}, ?DEFAULT_CALL_TIMEOUT).

%% Returns the amount of work done on the chain i.e. the total
%% difficulty of the top header.
-spec get_total_difficulty() -> get_total_difficulty_reply().
get_total_difficulty() ->
    {ok, Top} = top_header(),
    {ok, {gen_server:call(?CHAIN_SERVER, difficulty,
                          ?DEFAULT_CALL_TIMEOUT),
          {top_header, Top}}}.

common_ancestor(Hash1, Hash2) ->
    gen_server:call(?CHAIN_SERVER,
                    {common_ancestor, Hash1, Hash2}).

get_transactions_between(Hash1, Root) ->
    try get_transactions_between(Hash1, Root, []) of
        Transactions ->
            {ok, Transactions}
    catch throw:Error -> Error
    end.

get_transactions_between(Hash, Hash, Transactions) ->
    Transactions;
get_transactions_between(Hash, Root, Transactions) ->
    case aec_headers:hash_header(aec_block_genesis:genesis_header()) of
        %% Current block is genesis
        Hash -> Transactions;
        %% Block is not genesis
        _ ->
            case get_block_by_hash(Hash) of
                {ok, #block{prev_hash = Parent,
                            txs = BlockTransactions}} ->
                    get_transactions_between(Parent, Root,
                                             BlockTransactions ++ Transactions);
                {error,_} -> throw({error, {block_off_chain, Hash}})
            end
    end.

