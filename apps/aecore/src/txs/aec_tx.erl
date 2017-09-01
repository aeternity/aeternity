-module(aec_tx).

-include("trees.hrl").

-callback new(Args :: map(), Trees :: trees()) ->
    {ok, {Tx :: term(), Proofs :: list()}}.

-callback run(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()}.
