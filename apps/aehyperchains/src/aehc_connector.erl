%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_connector).

-export([connector/0]).

-export([send_tx/1, get_block_by_hash/1, get_top_block/0]).
-export([tx/2, block/3]).
-export([publish_block/1, subscribe_block/0]).

-type connector() :: atom().

-callback send_tx(binary()) -> binary().

-callback get_top_block() -> block().

-callback get_block_by_hash(binary()) -> block().

%%%===================================================================
%%%  Parent chain simplified proto
%%%===================================================================

-record(tx, { sender_id :: binary(), payload :: binary() }).

-type tx() :: #tx{}.

-record(block, { hash :: binary(), prev_hash :: binary(), txs :: [tx()] }).

-type block() :: #block{}.

-export_type([tx/0, block/0]).

-spec tx(Sender::binary(), Payload::binary()) -> tx().
tx(SenderId, Payload) when
      is_binary(SenderId), is_binary(Payload) ->
    #tx{ sender_id = SenderId, payload = Payload }.

-spec block(Hash::binary(), PrevHash::binary(), Txs::[tx()]) -> block().
block(Hash, PrevHash, Txs) when
      is_binary(Hash), is_binary(PrevHash), is_list(Txs) ->
    #block{ hash = Hash, prev_hash = PrevHash, txs = Txs }.

%%%===================================================================
%%%  Parent chain interface
%%%===================================================================

-spec send_tx(Payload::binary()) -> ok | {error, {term(), term()}}.
send_tx(Payload) ->
    Con = connector(),
    try
        ok = Con:send_tx(Payload)
    catch E:R ->
            {error, {E, R}}
    end.

-spec get_top_block() -> {ok, block()} | {error, {term(), term()}}.
get_top_block() ->
    Con = connector(),
    try
        Res = Con:get_top_block(), true = is_record(Res, block),
        {ok, Res}
    catch E:R ->
        {error, {E, R}}
    end.

-spec get_block_by_hash(binary()) -> {ok, block()} | {error, {term(), term()}}.
get_block_by_hash(Hash) ->
    Con = connector(),
    try
        Res = Con:get_block_by_hash(Hash), true = is_record(Res, block),
        {ok, Res}
    catch E:R ->
            {error, {E, R}}
    end.

%%%===================================================================
%%%  Parent chain events
%%%===================================================================

-spec subscribe_block() -> true.
subscribe_block() ->
    aec_events:subscribe({parent_chain, block}).

-spec publish_block(block()) -> ok.
publish_block(Block) ->
    aec_events:publish({parent_chain, block}, {block_created, Block}).

%%%===================================================================
%%%  Proto accessors
%%%===================================================================

%% TODO: BlockHash, etc..

-spec connector() -> connector().
connector() ->
    Con = aehc_app:get_connector_id(),
    binary_to_existing_atom(Con, utf8).
