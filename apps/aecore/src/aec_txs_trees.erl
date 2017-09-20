-module(aec_txs_trees).

-export([new/1,
         root_hash/1]).

new(Txs = [_|_]) ->
    {ok, EmptyTree} = aec_trees:new(),
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
    V = aec_tx_sign:serialize(SignedTx),
    K = aec_sha256:hash(V),
    {ok, _NewTxsTree} =
        aec_trees:put(K, V, TxsTree).

root_hash(TxsTree) ->
    aec_trees:root_hash(TxsTree).
