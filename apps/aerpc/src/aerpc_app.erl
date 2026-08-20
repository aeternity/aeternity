%%%-------------------------------------------------------------------
%%% @doc Application callback and operator-config mapping for `aerpc'.
%%%
%%% `check_env/0' is registered as a `setup' hook at priority 110 in
%%% `aecore.app.src', next to `aehttp_app:check_env/0', so the `aerpc'
%%% application env reflects `aeternity.yaml' before the first request
%%% is served. Both keys are read at request time rather than cached at
%%% boot, so this ordering has slack.
%%%
%%% Nothing here decides whether the endpoint is reachable at all --
%%% that is `http > endpoints > rpc', resolved by `aehttp_app' with
%%% every other endpoint group.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_app).
-behaviour(application).

-export([
          start/2
        , stop/1
        , check_env/0
        ]).

start(_StartType, _StartArgs) ->
    aerpc_sup:start_link().

stop(_State) ->
    ok.

%% @doc Map `http > rpc > *' onto the `aerpc' application env. Keys the
%% operator did not set are left alone, so the in-code defaults
%% (`aehttp_rpc_handler:?DEFAULT_MAX_BATCH', the placeholder chain-id
%% table) remain the fallback.
-spec check_env() -> ok.
check_env() ->
    aeu_env:check_env(
      aerpc,
      [{[<<"http">>, <<"rpc">>, <<"max_batch_size">>],
        {set_env, max_batch_size}},
       {[<<"http">>, <<"rpc">>, <<"chain_id">>],
        {set_env, chain_id}},
       {[<<"http">>, <<"rpc">>, <<"log_index">>],
        {set_env, log_index}},
       {[<<"http">>, <<"rpc">>, <<"log_retention_blocks">>],
        {set_env, log_retention_blocks}},
       {[<<"http">>, <<"rpc">>, <<"max_filters">>],
        {set_env, max_filters}},
       {[<<"http">>, <<"rpc">>, <<"filter_ttl_seconds">>],
        {set_env, filter_ttl_seconds}}]).
