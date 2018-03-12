%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Merkle trees of transactions.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_txs_trees).

%% API
-export([from_txs/1,
         add_txs/3,
         pad_empty/1,
         root_hash/1
        ]).

-export_type([txs_tree/0,
              root_hash/0]).

-include("blocks.hrl").

-type key() :: aec_hash:hash().
-type value() :: aetx_sign:binary_signed_tx(). %% Deterministic.
-opaque txs_tree() :: aeu_mtrees:mtree(key(), value()).

%% ?TXS_HASH_BYTES*8 bits binary
-type root_hash() :: <<_:256>>.

-type root_hash_result() :: {ok, root_hash()} | {error, empty}.

%%%===================================================================
%%% API
%%%===================================================================

-spec from_txs([aetx_sign:signed_tx(), ...] | []) -> txs_tree().

from_txs([]) ->
    %% NG-INFO: its fine to have empty transaction list (key-block)
    aeu_mtrees:empty();
from_txs(Txs = [_|_]) ->
    from_txs(Txs, 0, aeu_mtrees:empty()).

-spec add_txs([aetx_sign:signed_tx()], non_neg_integer(), txs_tree()) ->
        txs_tree().
add_txs(Txs, StartPos, TxsTree) ->
    from_txs(Txs, StartPos, TxsTree).

-spec root_hash(txs_tree()) -> root_hash_result().
root_hash(TxsTree) ->
    aeu_mtrees:root_hash(TxsTree).

-spec pad_empty(root_hash_result()) -> root_hash().
pad_empty({ok, H = <<_:?TXS_HASH_BYTES/unit:8>>}) -> H;
pad_empty({error, empty}) -> <<0:?TXS_HASH_BYTES/unit:8>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

from_txs([],_Position, Tree) ->
    Tree;
from_txs([SignedTx|Left], Position, Tree) ->
    Key = binary:encode_unsigned(Position),
    Val = aetx_sign:serialize_to_binary(SignedTx),
    from_txs(Left, Position + 1, aeu_mtrees:enter(Key, Val, Tree)).
