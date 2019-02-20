%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate serialization
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_serialize_test).

-include_lib("eunit/include/eunit.hrl").

serialize_integer_test() ->
    <<0>> = aefa_encoding:serialize(aefa_data:make_integer(0)),
    <<2>> = aefa_encoding:serialize(aefa_data:make_integer(1)),
    <<126>> = aefa_encoding:serialize(aefa_data:make_integer(63)),
    <<111, 0>> = aefa_encoding:serialize(aefa_data:make_integer(64)),
    <<111,130,255,255>> = aefa_encoding:serialize(aefa_data:make_integer(65535 + 64)),
    <<111,184,129,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>> =
        aefa_encoding:serialize(aefa_data:make_integer(1 bsl 1024 + 64)).

serialize_deserialize_test_() ->
    [{lists:flatten(io_lib:format("~p", [X])),
      fun() ->
          ?assertEqual(X,
                       aefa_encoding:deserialize(aefa_encoding:serialize(X)))
      end}
     || X <- sources()].

make_int_list(N) -> [aefa_data:make_integer(I) || I <- lists:seq(1, N)].

sources() ->
    FortyTwo = aefa_data:make_integer(42),
    Unit = aefa_data:make_unit(),
    True = aefa_data:make_boolean(true),
    False = aefa_data:make_boolean(false),
    Nil = aefa_data:make_list([]),
    EmptyString = aefa_data:make_string(""),
    EmptyMap = aefa_data:make_map(#{}),
    [aefa_data:make_integer(0),
     aefa_data:make_integer(1),
     True, False, Unit, Nil, EmptyString, EmptyMap,
     aefa_data:make_list([True]),
     aefa_data:make_address(
       <<0,1,2,3,4,5,6,7,8,9,
         0,1,2,3,4,5,6,7,8,9,
         0,1,2,3,4,5,6,7,8,9,
         1,2>>),
     aefa_data:make_string(<<"Hello">>),
     aefa_data:make_string(
       <<"0123456789012345678901234567890123456789"
         "0123456789012345678901234567890123456789"
         "0123456789012345678901234567890123456789"
         "0123456789012345678901234567890123456789">>), %% Magic concat 80 char string.
     aefa_data:make_tuple({True, FortyTwo}),
     aefa_data:make_tuple(list_to_tuple(make_int_list(65))),
     aefa_data:make_map(#{ aefa_data:make_integer(1) => True, aefa_data:make_integer(2) => False}),
     aefa_data:make_map(#{ aefa_data:make_string(<<"foo">>) => aefa_data:make_tuple({FortyTwo, True})}),
     aefa_data:make_list(make_int_list(3)),
     aefa_data:make_integer(-65),
     aefa_data:make_integer(65),
     aefa_data:make_integer(-32432847932847928374983),
     aefa_data:make_list(make_int_list(65)),
     aefa_data:make_variant(0, {FortyTwo}),
     aefa_data:make_variant(0, {}),
     aefa_data:make_list([aefa_data:make_variant(0, {})]),
     aefa_data:make_variant(255, {}),
     aefa_data:make_variant(3, {aefa_data:make_boolean(true),
                                aefa_data:make_list(make_int_list(3)),
                                aefa_data:make_string(<<"foo">>)})

    ].
