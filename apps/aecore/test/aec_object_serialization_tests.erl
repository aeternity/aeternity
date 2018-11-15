%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_object_serialization
%%% @end
%%%-------------------------------------------------------------------
-module(aec_object_serialization_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_TAG, account).
-define(DEFAULT_VERSION, 1).

basic_test() ->
    Template = [{foo, int}, {bar, binary}],
    Values   = [{foo, 1},   {bar, <<2>>}],
    ?assertEqual(Values, deserialize(Template, serialize(Template, Values))).

basic_fail_test() ->
    Template = [{foo, int}, {bar, binary}],
    Values   = [{foo, 1},   {bar, 1}],
    ?assertError({illegal_field, _, _, _, _, _}, serialize(Template, Values)).

list_test() ->
    Template = [{foo, [int]}, {bar, [binary]}, {baz, [int]}],
    Values   = [{foo, [1]},   {bar, [<<2>>, <<2>>]}, {baz, []}],
    ?assertEqual(Values, deserialize(Template, serialize(Template, Values))).

list_fail_test() ->
    Template = [{foo, [int]}, {bar, [binary]}],
    Values   = [{foo, [1]},   {bar, [2, <<2>>]}],
    ?assertError({illegal_field, _, _, _, _, _}, serialize(Template, Values)).

deep_list_test() ->
    Template = [{foo, [[int]]}, {bar, [[[[[binary]]]]]}],
    Values   = [{foo, [[1]]},   {bar, [[[[[<<2>>]]]]]}],
    ?assertEqual(Values, deserialize(Template, serialize(Template, Values))).

deep_list_fail_test() ->
    Template = [{foo, [[int]]}, {bar, [[[[[binary]]]]]}],
    Values   = [{foo, [[1]]},   {bar, [[[[[2]]]]]}],
    ?assertError({illegal_field, _, _, _, _, _}, serialize(Template, Values)).

array_test() ->
    Template = [{foo, {int, binary}},  {bar, [{int, int}]}, {baz, {int}}],
    Values   = [{foo, {1, <<"foo">>}}, {bar, [{1, 2}, {3, 4}, {5, 6}]}, {baz, {1}}],
    ?assertEqual(Values, deserialize(Template, serialize(Template, Values))).

array_fail_test() ->
    Template = [{foo, {int, binary}},  {bar, [{int, int}]}, {baz, {int}}],
    Values   = [{foo, {1, <<"foo">>}}, {bar, [{1, 2}, {3, 4}, {5, 6}]}, {baz, {1, 1}}],
    ?assertError({illegal_field, _, _, _, _, _}, serialize(Template, Values)).

deep_array_test() ->
    Template = [{foo, {{int, binary}}},  {bar, [{{int}, int}]}, {baz, {{int}}}],
    Values   = [{foo, {{1, <<"foo">>}}}, {bar, [{{1}, 2}, {{3}, 4}, {{5}, 6}]}, {baz, {{1}}}],
    ?assertEqual(Values, deserialize(Template, serialize(Template, Values))).

deep_array_fail_test() ->
    Template = [{foo, {{int, binary}}},  {bar, [{{int}, int}]}, {baz, {{binary}}}],
    Values   = [{foo, {{1, <<"foo">>}}}, {bar, [{{1}, 2}, {{3}, 4}, {{5}, 6}]}, {baz, {{1}}}],
    ?assertError({illegal_field, _, _, _, _, _}, serialize(Template, Values)).

tag_fail_test() ->
    Template = [{foo, int}, {bar, binary}],
    Values   = [{foo, 1},   {bar, <<2>>}],
    ?assertError({illegal_serialization, _, _, _, _, _, _},
                 deserialize(Template, serialize(Template, Values), signed_tx, ?DEFAULT_VERSION)).

vsn_fail_test() ->
    Template = [{foo, int}, {bar, binary}],
    Values   = [{foo, 1},   {bar, <<2>>}],
    ?assertError({illegal_serialization, _, _, _, _, _, _},
                 deserialize(Template, serialize(Template, Values), ?DEFAULT_TAG, 2)).

deserialize(Template, Bin) ->
    deserialize(Template, Bin, ?DEFAULT_TAG, ?DEFAULT_VERSION).

deserialize(Template, Bin, Tag, Vsn) ->
    aec_object_serialization:deserialize(Tag, Vsn, Template, Bin).

serialize(Template, Bin) ->
    serialize(Template, Bin, ?DEFAULT_TAG, ?DEFAULT_VERSION).

serialize(Template, Bin, Tag, Vsn) ->
    aec_object_serialization:serialize(Tag, Vsn, Template, Bin).
