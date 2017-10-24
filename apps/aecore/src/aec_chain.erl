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

-export([start_link/1,
         stop/0]).

%% API
-export([top/0,
         top_header/0,
         get_header_by_hash/1,
         get_block_by_hash/1,
         get_header_by_height/1,
         get_block_by_height/1,
         insert_header/1,
         write_block/1,
         get_total_difficulty/0
	]).

-export([export_chain/1,
         import_chain/1]).

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
        {ok, header()} |
        {error, Reason::{chain_too_short, {{chain_height, height()},
                                           {top_header, header()}}}}.
-type get_block_by_height_reply() ::
        {ok, block()} |
        {error, Reason::{chain_too_short, {{chain_height, height()},
                                           {top_header, header()}}} |
                        {block_not_found, {top_header, header()}}
        }.
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


export_chain(OutFile) ->
    Headers = all_headers(),
    Blocks = all_blocks(Headers),
    file:write_file(
      OutFile, term_to_binary({{0,1}, Headers, Blocks}, [compressed])).

import_chain(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            try binary_to_term(Bin) of
                {{0,1}, Headers, Blocks} ->
                    io:fwrite("Headers = ~p~n"
                              "Blocks = ~p~n",
                              [Headers, Blocks]),
                    lists:foreach(
                      fun(H) ->
                              ok = insert_header(H)
                      end, Headers),
                    lists:foreach(
                      fun(B) ->
                              ok = write_block(B)
                      end, Blocks)
            catch
                error:Reason ->
                    {error, Reason}
            end;
        Error ->
            Error
    end.


all_headers() ->
    all_headers(top_header(), []).

all_headers({ok, H}, Acc) ->
    case aec_headers:height(H) of
        0 ->
            Acc;
        _ ->
            all_headers(
              get_header_by_hash(
                aec_headers:prev_hash(H)), [H|Acc])
    end;
all_headers({error, E}, _) ->
    erlang:error(E).

all_blocks(Hdrs) ->
    lists:foldr(
      fun(H, Acc) ->
              {ok, HH} = aec_headers:hash_header(H),
              {ok, B} = aec_chain:get_block_by_hash(HH),
              [B|Acc]
      end, [], Hdrs).
