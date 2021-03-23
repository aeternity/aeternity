-module(aehc_app).
-behavior(application).

-export([
          start/2
        , start_phase/3
        , prep_stop/1
        , stop/1
        , check_env/0
        ]).

start(_StartType, _StartArgs) ->
    Res = aehc_sup:start_link(),
    %% NOTE: In order to start and the common sync procedure HC performs:
    %% - fetching the parent chain data via network interface (connector);
    %% - processing thq queue of parent chain blocks via acknowledgement interface
    aehc_utils:hc_enabled() andalso begin aehc_utils:confirm_commitment(), pop() end,
    Res.

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.

check_env() ->
    check_env([{[<<"chain">>, <<"hyperchains">>, <<"enabled">>], {set_env, enabled}}]),
    aehc_utils:hc_enabled() andalso lager:info("Hyperchains is enabled"),
    ok.

check_env(Spec) ->
    lists:foreach(
      fun({K, F}) ->
              case aeu_env:user_config(K) of
                  undefined -> ignore;
                  {ok, V}   -> set_env(F, V)
              end
      end, Spec).

set_env({set_env, K}, V) when is_atom(K) ->
    io:fwrite("setenv K=~p, V=~p~n", [K, V]),
    application:set_env(aehyperchains, K, V);
set_env(F, V) when is_function(F, 1) ->
    F(V).


pop() ->
    Res = aehc_parent_mng:pop(), lager:info("~nPop: ~p~n",[Res]), (Res == empty) orelse pop().
