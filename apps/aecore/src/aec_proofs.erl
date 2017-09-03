-module(aec_proofs).

%% API
-export([prove/2]).

prove(tx, trees) ->
    %% TODO: Prove tx
    ok;
prove(_Tx, _Trees) ->
    {error, failed}.
