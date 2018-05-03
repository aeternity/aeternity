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

-spec from_txs([aetx_sign:signed_tx(), ...] | []) -> txs_tree().

from_txs([]) ->
    %% NG-INFO: its fine to have empty transaction list
    %%          - it happens for key blocks
    %%          - it happens for microblocks during slow time
    %%            (when microblock has to be triggered to comply with governance,
    %%             but there are no transactions)
    aeu_mtrees:empty();
from_txs(Txs = [_|_]) ->
    from_txs(Txs, 0, aeu_mtrees:empty()).

-spec root_hash(txs_tree()) -> {ok, root_hash()}.
root_hash(TxsTree) ->
    RT = <<_:?TXS_HASH_BYTES/unit:8>> = aeu_mtrees:pad_empty(aeu_mtrees:root_hash(TxsTree)),
    {ok, RT}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

from_txs([],_Position, Tree) ->
    Tree;
from_txs([SignedTx|Left], Position, Tree) ->
    Key = binary:encode_unsigned(Position),
    Val = aetx_sign:serialize_to_binary(SignedTx),
    from_txs(Left, Position + 1, aeu_mtrees:enter(Key, Val, Tree)).
