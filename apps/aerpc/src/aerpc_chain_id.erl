%%%-------------------------------------------------------------------
%%% @doc Numeric chain id for the JSON-RPC layer.
%%%
%%% An operator-set `http > rpc > chain_id' wins over everything else.
%%% That override is the supported way to run this endpoint today,
%%% because the built-in per-network numbers below are **placeholders
%%% only**: registering a chain id publicly (chainlist.org, EIP-155) is
%%% æternity's call to make, not ours, and nothing here should be read
%%% as a claim on those numbers.
%%%
%%% They stay as defaults rather than becoming a hard error so a node
%%% started without the key still answers `eth_chainId' instead of
%%% 500-ing, and so an unknown network id degrades to 0 rather than
%%% crashing the dispatcher.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_chain_id).

-export([to_numeric/1, current/0, configured/0]).

%% @doc Numeric chain id for the network this node is on.
-spec current() -> non_neg_integer().
current() ->
    %% Check the override before asking for the network id, so a node
    %% with an explicit chain_id never depends on aec_governance here.
    case configured() of
        undefined -> default_for(aec_governance:get_network_id());
        N         -> N
    end.

%% @doc The operator override, if one is set. `undefined' means "fall
%% back to the placeholder table". Exposed separately so a caller can
%% tell a configured id from a default that happens to match.
-spec configured() -> non_neg_integer() | undefined.
configured() ->
    case application:get_env(aerpc, chain_id, undefined) of
        N when is_integer(N), N >= 0 -> N;
        _Other                       -> undefined
    end.

%% @doc Map an AE network id to its numeric counterpart, honouring the
%% operator override first. Unknown network ids fall back to a sentinel
%% value (0) rather than crashing, so a misconfigured node still
%% responds rather than 500-ing.
-spec to_numeric(binary()) -> non_neg_integer().
to_numeric(NetworkId) ->
    case configured() of
        undefined -> default_for(NetworkId);
        N         -> N
    end.

%% Placeholders. See the module doc before citing any of these.
default_for(<<"ae_mainnet">>) -> 1247;
default_for(<<"ae_uat">>)     -> 1248;
default_for(<<"ae_dev1">>)    -> 9991;
default_for(<<"ae_dev2">>)    -> 9992;
default_for(<<"ae_dev3">>)    -> 9993;
default_for(_Unknown)         -> 0.
