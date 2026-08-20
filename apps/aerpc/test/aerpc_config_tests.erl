%%%-------------------------------------------------------------------
%%% @doc Operator-config coverage for the JSON-RPC endpoint: that
%%% `http > rpc > *' actually reaches the `aerpc' application env, and
%%% that `http > endpoints > rpc' actually decides whether the routes
%%% are mounted.
%%%
%%% Both are checked against a real config file through the real schema
%%% and the real `check_env/0' functions, because the failure mode this
%%% guards against -- a key that validates fine and is then read from a
%%% path nothing writes -- is invisible to a schema test alone.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_config_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

rpc_config_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"'http > rpc' reaches the aerpc application env",
       fun rpc_settings_reach_app_env/0},
      {"'http > endpoints > rpc' enables the route group",
       fun rpc_endpoint_group_enabled/0},
      {"the route group is off unless the operator asks for it",
       fun rpc_endpoint_group_is_opt_in/0}]}.

%% ===================================================================
%% Cases
%% ===================================================================

rpc_settings_reach_app_env() ->
    install(rpc_config()),
    application:unset_env(aerpc, max_batch_size),
    application:unset_env(aerpc, chain_id),
    ok = aerpc_app:check_env(),
    %% The fixture sets 32 / 987654; anything else means the config path
    %% in aerpc_app:check_env/0 does not match the schema path.
    ?assertEqual(32,     aerpc:max_batch_size()),
    ?assertEqual(987654, aerpc_chain_id:configured()),
    ?assertEqual(987654, aerpc_chain_id:current()).

rpc_endpoint_group_enabled() ->
    install(rpc_config()),
    ok = aehttp_app:check_env(),
    ?assert(lists:member(<<"rpc">>, enabled_groups())).

rpc_endpoint_group_is_opt_in() ->
    %% A config that says nothing about the endpoint must not mount it:
    %% eth_call and eth_estimateGas run through dry-run, so this surface
    %% is opt-in the same way the 'dry-run' group is.
    install(plain_config()),
    ok = aehttp_app:check_env(),
    ?assertNot(lists:member(<<"rpc">>, enabled_groups())).

%% ===================================================================
%% Fixtures / helpers
%% ===================================================================

%% The config fixtures live with every other one, under aeutils.
config_file(Name) ->
    DataDir = "aeutils/test/data/",
    Dir = case filelib:is_dir(filename:join("apps", DataDir)) of
              true  -> "apps/";
              false -> "../../lib/"
          end,
    filename:join([Dir, DataDir, Name]).

rpc_config()   -> config_file("epoch_rpc.yaml").
plain_config() -> config_file("epoch_no_peers.yaml").

install(File) ->
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(File),
    mock_user_config(UserMap, UserConfig).

%% Same shape as aeu_env_tests:mock_user_config/2 -- setup:get_env/2 and
%% /3 must agree, or user_config/0 sees an {ok, _} wrapper where the
%% bare value belongs.
mock_user_config(UserMap, UserConfig) ->
    forget_user_config(),
    F = fun (aeutils, '$user_config') -> {ok, UserConfig};
            (aeutils, '$user_map')    -> {ok, UserMap};
            (A, K)                    -> meck:passthrough([A, K])
        end,
    ok = meck:expect(setup, get_env, F),
    ok = meck:expect(setup, get_env,
                     fun(A, K, Default) ->
                             case F(A, K) of
                                 {ok, V}   -> V;
                                 undefined -> Default
                             end
                     end),
    ok.

forget_user_config() ->
    %% aeu_env caches the parsed config in persistent_term; a stale entry
    %% there would shadow everything mocked above.
    ok = aeu_env:invalidate_config_cache(),
    ok = application:unset_env(aeutils, '$user_map'),
    ok = application:unset_env(aeutils, '$user_config').

enabled_groups() ->
    application:get_env(aehttp, enabled_endpoint_groups, []).

setup() ->
    ok = meck:new(setup, [passthrough]),
    application:ensure_all_started(jesse),
    application:ensure_all_started(yamerl),
    application:ensure_all_started(jsx),
    #{max_batch_size => application:get_env(aerpc, max_batch_size),
      chain_id       => application:get_env(aerpc, chain_id),
      groups         => application:get_env(aehttp, enabled_endpoint_groups)}.

teardown(Saved) ->
    forget_user_config(),
    restore(aerpc,  max_batch_size, maps:get(max_batch_size, Saved)),
    restore(aerpc,  chain_id,       maps:get(chain_id, Saved)),
    restore(aehttp, enabled_endpoint_groups, maps:get(groups, Saved)),
    ok = meck:unload(setup),
    application:stop(jsx),
    application:stop(yamerl),
    application:stop(jesse),
    ok.

restore(App, Key, undefined)   -> application:unset_env(App, Key);
restore(App, Key, {ok, Value}) -> application:set_env(App, Key, Value).

-endif.
