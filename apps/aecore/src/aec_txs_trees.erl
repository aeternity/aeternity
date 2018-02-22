%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Merkle trees of transactions.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_txs_trees).

%% API
-export([from_txs/1,
         root_hash/1]).

-export_type([txs_tree/0,
              root_hash/0]).

-include("common.hrl").
-include("blocks.hrl").

-type key() :: aec_hash:hash().
-type value() :: aetx_sign:binary_signed_tx(). %% Deterministic.
-opaque txs_tree() :: aeu_mtrees:mtree(key(), value()).

%% ?TXS_HASH_BYTES*8 bits binary
-type root_hash() :: <<_:256>>.

%%%===================================================================
%%% API
%%%===================================================================

-spec from_txs([aetx_sign:signed_tx(), ...]) -> txs_tree().
from_txs(Txs = [_|_]) ->
    lists:foldl(fun enter_signed_tx/2, empty(), Txs).

-spec root_hash(txs_tree()) -> {ok, root_hash()}.
root_hash(TxsTree) ->
    {ok, <<_:?TXS_HASH_BYTES/unit:8>>} = aeu_mtrees:root_hash(TxsTree).

%%%===================================================================
%%% Internal functions
%%%===================================================================

empty() ->
    aeu_mtrees:empty().

enter(K, V, T) ->
    aeu_mtrees:enter(K, V, T).

enter_signed_tx(SignedTx, TxsTree) ->
    V = aetx_sign:serialize_to_binary(SignedTx),
    K = aec_hash:hash(signed_tx, V),
    enter(K, V, TxsTree).
