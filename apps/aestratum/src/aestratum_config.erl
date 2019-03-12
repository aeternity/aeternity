-module(aestratum_config).
-export([extra_nonce_bytes/0,
         initial_target/0,
         max_target/0]).

-export([set_initial_target/1,
         set_max_target/1]).

-define(EXTRA_NONCE_BYTES, 1).
-define(INITIAL_TARGET, 0).
-define(MAX_TARGET, 16#ffff000000000000000000000000000000000000000000000000000000000000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extra_nonce_bytes() ->
    value(<<"extra_nonce_bytes">>).

initial_target() ->
    value(<<"initial_target">>).

max_target() ->
    value(<<"max_target">>).


set_initial_target(NewVal)
  when is_integer(NewVal), NewVal >= ?INITIAL_TARGET, NewVal =< ?MAX_TARGET ->
    set_value(<<"initial_target">>, NewVal).

set_max_target(NewVal)
  when is_integer(NewVal), NewVal >= ?INITIAL_TARGET, NewVal =< ?MAX_TARGET ->
    set_value(<<"max_target">>, NewVal).


value(K) when is_binary(K) ->
    {ok, X} = aeu_env:user_map([<<"stratum">>, K]),
    X.

set_value(K, V) when is_binary(K) ->
    aeu_env:store([#{<<"stratum">> => #{K => V}}]).
