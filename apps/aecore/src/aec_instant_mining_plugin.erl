%%%
%%% Utility module for instantly mining blocks in tests and when dev mode is enabled
%%%

-module(aec_instant_mining_plugin).
-export([ instant_tx_confirm_hook/1
        , instant_tx_confirm_enabled/0 ]).

%% Executed after a transaction was successfully inserted to the tx pool
%% This is a placeholder for dev mode
instant_tx_confirm_hook(_) ->
    ok.

instant_tx_confirm_enabled() ->
    true.
