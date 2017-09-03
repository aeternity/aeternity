-module(aec_keys).

%% API
-export([pubkey/0,
         sign/1]).

-include("common.hrl").
-include("txs.hrl").

pubkey() ->
    {ok, <<>>}.

sign(Tx) ->
    %% TODO: Return signed tx
    %% If only one signature needed for a tx, signatures list will contain only one item
    {ok, #signed_tx{data = Tx, signatures = []}}.
