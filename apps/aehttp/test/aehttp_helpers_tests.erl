-module(aehttp_helpers_tests).

%% Tests for the parts of aehttp_helpers that do not need a running node.

-include_lib("eunit/include/eunit.hrl").

%% The 503 responses are built here rather than in the dispatch modules so that
%% the external, internal and Rosetta APIs cannot drift apart. The shape matters
%% to cowboy: header names must be lowercase binaries, and Retry-After must be a
%% binary delay-seconds value, not an integer.
service_unavailable_test_() ->
    [ {"queue rejection is reported as a temporary overload",
       fun() ->
           {Code, Headers, Body} = aehttp_helpers:service_unavailable(overload),
           ?assertEqual(503, Code),
           ?assertEqual(#{reason => <<"Temporary overload">>}, Body),
           ?assertEqual([{<<"retry-after">>, <<"3">>}], Headers)
       end}
    , {"an unstable node gets the more conservative back-off hint",
       fun() ->
           {Code, Headers, Body} = aehttp_helpers:service_unavailable(not_stable),
           ?assertEqual(503, Code),
           ?assertEqual(#{reason => <<"Not yet started">>}, Body),
           ?assertEqual([{<<"retry-after">>, <<"30">>}], Headers)
       end}
    , {"every 503 carries a Retry-After clients can parse",
       fun() ->
           lists:foreach(
             fun(Reason) ->
                 {503, Headers, _} = aehttp_helpers:service_unavailable(Reason),
                 {_, Value} = lists:keyfind(<<"retry-after">>, 1, Headers),
                 ?assert(is_binary(Value)),
                 ?assert(binary_to_integer(Value) > 0)
             end, [overload, not_stable])
       end}
    ].

%% The helper only pays off if the dispatch modules actually call it. Each of
%% them catches the same two errors, and nothing above notices if one goes back
%% to building its own 503 - the drift the helper exists to prevent is exactly
%% what the tests above cannot see. So drive the real handle_request/3 with the
%% mode check and the request queue stubbed out, and assert all three APIs shed
%% load identically.
dispatch_shedding_test_() ->
    {foreach,
     fun() ->
         meck:new(app_ctrl, [non_strict]),
         meck:new(aec_jobs_queues, [non_strict])
     end,
     fun(_) ->
         meck:unload(aec_jobs_queues),
         meck:unload(app_ctrl)
     end,
     [ {"a rejected request is shed as an overload by every API",
        fun() ->
            meck:expect(app_ctrl, await_stable_mode, fun(_) -> {ok, stable} end),
            meck:expect(aec_jobs_queues, run,
                        fun(_Queue, _F) -> erlang:error({rejected, counter}) end),
            [ assert_shed(Mod, OpId, <<"3">>, <<"Temporary overload">>)
              || {Mod, OpId} <- dispatchers() ]
        end}
     , {"an unstable node is shed as not-yet-started by every API",
        fun() ->
            meck:expect(app_ctrl, await_stable_mode, fun(_) -> {timeout, starting} end),
            %% The queue would have admitted this one: in all three modules
            %% when_stable/1 wraps the queue, so the mode check sheds first.
            meck:expect(aec_jobs_queues, run, fun(_Queue, F) -> F() end),
            [ assert_shed(Mod, OpId, <<"30">>, <<"Not yet started">>)
              || {Mod, OpId} <- dispatchers() ]
        end}
     ]}.

%% One read operation per dispatch module. It is never run - it only has to be
%% an operation the module's own queue/1 recognises.
dispatchers() ->
    [ {aehttp_dispatch_ext    , 'GetTopBlock'}
    , {aehttp_dispatch_int    , 'GetNetworkStatus'}
    , {aehttp_dispatch_rosetta, networkList}
    ].

assert_shed(Mod, OpId, RetryAfter, Reason) ->
    Actual   = Mod:handle_request(OpId, #{}, #{}),
    Expected = {503, [{<<"retry-after">>, RetryAfter}], #{reason => Reason}},
    %% Tagged with the module so a failure names the API that drifted.
    ?assertEqual({Mod, Expected}, {Mod, Actual}),
    %% aehttp_api_handler:to_headers/1 hands this list to cowboy as a map, so
    %% the conversion has to be lossless or the back-off never reaches a client.
    {503, Headers, _} = Actual,
    ?assertEqual(#{<<"retry-after">> => RetryAfter}, maps:from_list(Headers)).

%%%===================================================================
%%% Dry-run reported gas-price floor (http.gas_price.min_relay_gas_price)
%%%===================================================================

-define(OBSERVED_GAS_PRICE, 1000000).       %% what aec_dry_run meters a call_req at
-define(OBSERVED_GAS_USED,  73421).
-define(MIN_RELAY,          500000000000).  %% 500 x the 1e9 baseline clients use

%% Only the reported price, only on the public profile. Everything else -- above
%% all gas_used, which clients derive a gas limit from -- comes through untouched.
min_relay_gas_price_test_() ->
    {foreach,
     fun() -> application:unset_env(aehttp, gas_price) end,
     fun(_) -> application:unset_env(aehttp, gas_price) end,
     [ {"off by default: no config, no change",
        fun() ->
            %% the one config read the dry-run path now shares with the endpoint
            ?assertEqual(undefined, aehttp_logic:min_relay_gas_price()),
            ?assertEqual(results(?OBSERVED_GAS_PRICE),
                         aehttp_helpers:floor_dry_run_gas_prices(public, results(?OBSERVED_GAS_PRICE)))
        end}
     , {"set: the public profile reports the floor instead of the metered price",
        fun() ->
            set_min_relay(?MIN_RELAY),
            [#{call_obj := CallObj}] =
                aehttp_helpers:floor_dry_run_gas_prices(public, results(?OBSERVED_GAS_PRICE)),
            ?assertEqual(?MIN_RELAY, maps:get(<<"gas_price">>, CallObj)),
            %% the whole point of the constraint: gas_used is never scaled
            ?assertEqual(?OBSERVED_GAS_USED, maps:get(<<"gas_used">>, CallObj))
        end}
     , {"it is a floor, not a replacement: a pricier call keeps its own price",
        fun() ->
            set_min_relay(?MIN_RELAY),
            Higher = ?MIN_RELAY * 2,
            [#{call_obj := CallObj}] =
                aehttp_helpers:floor_dry_run_gas_prices(public, results(Higher)),
            ?assertEqual(Higher, maps:get(<<"gas_price">>, CallObj))
        end}
     , {"every other profile keeps the metered price, set or not",
        fun() ->
            set_min_relay(?MIN_RELAY),
            %% internal is ae_mdw's default via aec_dry_run:dry_run/4, replay is
            %% Rosetta, includability is the pool's real inclusion check.
            lists:foreach(
              fun(Profile) ->
                  ?assertEqual({Profile, results(?OBSERVED_GAS_PRICE)},
                               {Profile, aehttp_helpers:floor_dry_run_gas_prices(
                                           Profile, results(?OBSERVED_GAS_PRICE))})
              end, [internal, replay, includability, undefined])
        end}
     , {"a zero or non-integer value is off, not a floor of zero",
        fun() ->
            lists:foreach(
              fun(Value) ->
                  set_min_relay(Value),
                  ?assertEqual({Value, results(?OBSERVED_GAS_PRICE)},
                               {Value, aehttp_helpers:floor_dry_run_gas_prices(
                                         public, results(?OBSERVED_GAS_PRICE))})
              end, [0, -1, undefined, <<"500">>])
        end}
     , {"results carrying no call object pass through untouched",
        fun() ->
            set_min_relay(?MIN_RELAY),
            %% a spend, and a failed contract call: neither has a gas price
            NoCallObj = [ #{type => <<"spend">>, result => <<"ok">>}
                        , #{type => <<"contract_call">>, result => <<"error">>,
                            reason => <<"Error: out_of_gas">>} ],
            ?assertEqual(NoCallObj,
                         aehttp_helpers:floor_dry_run_gas_prices(public, NoCallObj))
        end}
     ]}.

%% The unit tests above cannot see whether do_dry_run/1 actually applies the
%% floor -- which is the only thing that reaches a client. So drive the real
%% dispatch fun with aec_dry_run stubbed and read the response body.
min_relay_gas_price_dispatch_test_() ->
    {foreach,
     fun() ->
         application:unset_env(aehttp, gas_price),
         meck:new(aec_dry_run, [passthrough]),
         meck:expect(aec_dry_run, dry_run,
                     fun(_Top, _Accounts, _Txs, _Opts) ->
                         {ok, {[{contract_call_tx, {ok, call_obj(?OBSERVED_GAS_PRICE)}}], []}}
                     end)
     end,
     fun(_) ->
         meck:unload(aec_dry_run),
         application:unset_env(aehttp, gas_price)
     end,
     [ {"the external endpoint (public) serves the floor",
        fun() ->
            set_min_relay(?MIN_RELAY),
            ?assertEqual({?MIN_RELAY, ?OBSERVED_GAS_USED}, dispatch_gas(public))
        end}
     , {"the internal endpoint serves the metered price",
        fun() ->
            set_min_relay(?MIN_RELAY),
            ?assertEqual({?OBSERVED_GAS_PRICE, ?OBSERVED_GAS_USED}, dispatch_gas(internal))
        end}
     , {"with the key unset the external endpoint is unchanged",
        fun() ->
            ?assertEqual({?OBSERVED_GAS_PRICE, ?OBSERVED_GAS_USED}, dispatch_gas(public))
        end}
     ]}.

%% Run aehttp_helpers:do_dry_run(Profile) the way process_request/3 does and
%% return the {gas_price, gas_used} the client would see.
dispatch_gas(Profile) ->
    State = #{ top => aeser_api_encoder:encode(key_block_hash, <<0:256>>)
             , txs => [#{<<"call_req">> => #{}}]
             , accounts => []
             , tx_events => false },
    Fun = aehttp_helpers:do_dry_run(Profile),
    {ok, {200, [], #{results := [#{call_obj := CallObj}]}}} = Fun(#{}, State),
    {maps:get(<<"gas_price">>, CallObj), maps:get(<<"gas_used">>, CallObj)}.

set_min_relay(Value) ->
    application:set_env(aehttp, gas_price, [{min_relay_gas_price, Value}]).

%% One serialized dry-run result, the shape dry_run_results/1 produces.
results(GasPrice) ->
    [#{ type => <<"contract_call">>
      , result => <<"ok">>
      , call_obj => aect_call:serialize_for_client(call_obj(GasPrice)) }].

call_obj(GasPrice) ->
    Call = aect_call:new(aeser_id:create(account, <<1:256>>), 1,
                         aeser_id:create(contract, <<2:256>>), 10, GasPrice),
    aect_call:set_gas_used(?OBSERVED_GAS_USED, Call).
