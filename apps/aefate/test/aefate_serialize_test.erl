%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate serialization
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_serialize_test).

-include_lib("eunit/include/eunit.hrl").

serialize_integer_test() ->
    <<0>> = aefa_encoding:serialize(0),
    <<2>> = aefa_encoding:serialize(1),
    <<126>> = aefa_encoding:serialize(63),
    <<111, 0>> = aefa_encoding:serialize(64),
    <<111,130,255,255>> = aefa_encoding:serialize(65535 + 64),
    <<111,184,129,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>> = aefa_encoding:serialize(1 bsl 1024 + 64).

serialize_deserialize_test_() ->
    [{lists:flatten(io_lib:format("~p", [X])),
      fun() ->
          ?assertEqual(X,
                       aefa_encoding:deserialize(aefa_encoding:serialize(X)))
      end}
     || X <- sources()].

sources() ->
    [0,1, true, false, {tuple, {}}, "", #{},
     {address, <<0,1,2,3,4,5,6,7,8,9,
                 0,1,2,3,4,5,6,7,8,9,
                 0,1,2,3,4,5,6,7,8,9,
                 1,2>>},
     <<"Hello">>,
     <<"0123456789012345678901234567890123456789"
       "0123456789012345678901234567890123456789"
       "0123456789012345678901234567890123456789"
       "0123456789012345678901234567890123456789">>, %% Magic concat 80 char string.
     {tuple, {true, 42}},
     {tuple, list_to_tuple(lists:seq(1, 65))},
     #{ 1 => true, 2 => false},
     #{ "foo" => {tuple, {42, true}}},
     [1,2,3],
     lists:seq(1, 65),
     {variant, 0, {42}},
     {variant, 3, {true, [1,2,3], "foo"}}
    ].
