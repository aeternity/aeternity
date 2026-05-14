%%%-------------------------------------------------------------------
%%% @doc Maps AE network ids (`<<"ae_mainnet">>', `<<"ae_uat">>', etc.)
%%% to numeric chain ids for the JSON-RPC layer.
%%%
%%% The numeric ids reserved below are placeholders pending sign-off
%%% from the protocol team; the canonical place to register them is
%%% chainlist.org. Do not assume these values are final.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_chain_id).

-export([to_numeric/1]).

%% @doc Map an AE network id to its numeric counterpart. Unknown
%% network ids fall back to a sentinel value (0) rather than crashing,
%% so a misconfigured node still responds rather than 500-ing.
-spec to_numeric(binary()) -> non_neg_integer().
to_numeric(<<"ae_mainnet">>) -> 1247;
to_numeric(<<"ae_uat">>)     -> 1248;
to_numeric(<<"ae_dev1">>)    -> 9991;
to_numeric(<<"ae_dev2">>)    -> 9992;
to_numeric(<<"ae_dev3">>)    -> 9993;
to_numeric(_Unknown)         -> 0.
