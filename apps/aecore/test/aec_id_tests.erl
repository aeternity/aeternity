%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Tests for aec_id
%%% @end
%%%-------------------------------------------------------------------
-module(aec_id_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    lists:append(
      [
       [ {"Create and specialize " ++ atom_to_list(Type), test_create(Type)}
       , {"Encoding/decoding " ++ atom_to_list(Type), test_encoding(Type)}
       ]
       || Type <- all_tags()
      ]).

%%%=============================================================================
%%% Basic tests

test_create(Type) ->
    fun() ->
            Bin = id_bin(),
            Id  = aeser_id:create(Type, Bin),
            ?assertEqual({Type, Bin}, aeser_id:specialize(Id)),
            ?assertEqual(Bin, aeser_id:specialize(Id, Type)),
            ?assertEqual(Type, aeser_id:specialize_type(Id)),
            ?assertError(_, aeser_id:specialize(Id, foo))
    end.

test_encoding(Type) ->
    fun() ->
            Bin = id_bin(),
            Id  = aeser_id:create(Type, Bin),
            ?assertEqual(Id, aeser_id:decode(aeser_id:encode(Id)))
    end.

%%%=============================================================================
%%% Tools

id_bin() ->
    <<12345:32/unit:8>>.

all_tags() ->
    [ account
    , name
    , commitment
    , contract
    , oracle
    , channel
    ].
