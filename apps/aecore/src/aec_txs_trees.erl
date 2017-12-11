-module(aec_txs_trees).

-export([new/1,
         root_hash/1]).

%% TODO: Avoid creation of Merkle tree for empty list of transactions
new(Txs) ->
    {ok, EmptyTree} = aec_trees:new_merkle_tree(),
    TxsTree =
        lists:foldl(
          fun(SignedTx, TreeIn) ->
                  {ok, TreeOut} = put_signed_tx(SignedTx, TreeIn),
                  TreeOut
          end,
          EmptyTree,
          Txs),
    {ok, TxsTree}.

put_signed_tx(SignedTx, TxsTree) ->
    V = aec_tx_sign:serialize_to_binary(SignedTx),
    K = aec_sha256:hash(V),
    {ok, _NewTxsTree} =
        aec_trees:put(K, V, TxsTree).

root_hash(TxsTree) ->
    aec_trees:root_hash(TxsTree).
