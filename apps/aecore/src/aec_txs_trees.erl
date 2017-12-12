%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Merkle trees of transactions.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_txs_trees).

%% API
-export([new/1,
         root_hash/1]).

-export_type([txs_tree/0,
              root_hash/0]).

-include("common.hrl").
-include("blocks.hrl").

-type key() :: aec_sha256:hash(value()).
-type value() :: aec_tx_sign:deterministic_signed_tx_binary().
-opaque txs_tree() :: aeu_mtrees:mtree(key(), value()).
-type root_hash() :: <<_:(?TXS_HASH_BYTES*8)>>.

%%%===================================================================
%%% API
%%%===================================================================

-spec new([aec_tx_sign:signed_tx(), ...]) -> {ok, txs_tree()}.
new(Txs = [_|_]) ->
    {ok, EmptyTree} = new(),
    TxsTree =
        lists:foldl(
          fun(SignedTx, TreeIn) ->
                  {ok, TreeOut} = put_signed_tx(SignedTx, TreeIn),
                  TreeOut
          end,
          EmptyTree,
          Txs),
    {ok, TxsTree}.

-spec root_hash(txs_tree()) -> {ok, root_hash()}.
root_hash(TxsTree) ->
    {ok, <<_:?TXS_HASH_BYTES/unit:8>>} = aeu_mtrees:root_hash(TxsTree).

%%%===================================================================
%%% Internal functions
%%%===================================================================

new() ->
    aeu_mtrees:new().

put(K, V, T) ->
    aeu_mtrees:put(K, V, T).

put_signed_tx(SignedTx, TxsTree) ->
    V = aec_tx_sign:serialize_to_binary(SignedTx),
    K = aec_sha256:hash(V),
    {ok, _NewTxsTree} = put(K, V, TxsTree).
