-module(aestratum_lqueue_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_lqueue).

-compile({no_auto_import,[get/1]}).

lqueue_test_() ->
    [new(badarg),
     new(valid),
     is_empty(badarg),
     is_empty(valid),
     is_full(badarg),
     is_full(valid),
     max_len(badarg),
     max_len(valid),
     len(badarg),
     len(valid),
     member(badarg),
     member(valid),
     keymember(badarg),
     keymember(valid),
     find(badarg_lqueue),
     find(badarg_pred),
     find(valid),
     keyfind(badarg),
     keyfind(valid),
     keyreplace(badarg),
     keyreplace(valid),
     from_and_to_list(badarg),
     from_and_to_list(valid),
     in(badarg),
     in(valid),
     out(badarg),
     out(valid),
     get(badarg),
     get(valid),
     get_r(badarg),
     get_r(valid)].

new(badarg) ->
    L = [0, -1, atom, [], <<>>, 0.9],
    [?_assertException(error, badarg, ?TEST_MODULE:new(I)) || I <- L];
new(valid) ->
    L = [1, 123, 1000],
    [?_assertEqual([], ?TEST_MODULE:to_list(?TEST_MODULE:new(I))) || I <- L].

is_empty(badarg) ->
    L = [{}, [], not_lqueue],
    [?_assertException(error, badarg, ?TEST_MODULE:is_empty(I)) || I <- L];
is_empty(valid) ->
    Q = ?TEST_MODULE:new(2),
    Q1 = ?TEST_MODULE:in({item, 123}, Q),
    [?_assertEqual(true, ?TEST_MODULE:is_empty(Q)),
     ?_assertEqual(false, ?TEST_MODULE:is_empty(Q1))].

is_full(badarg) ->
    L = [foo, <<>>, [[],[]]],
    [?_assertException(error, badarg, ?TEST_MODULE:is_full(I)) || I <- L];
is_full(valid) ->
    Q = ?TEST_MODULE:new(2),
    Q1 = ?TEST_MODULE:in(foo, Q),
    Q2 = ?TEST_MODULE:in(bar, Q1),
    Q3 = ?TEST_MODULE:in(baz, Q2),
    [?_assertEqual(false, ?TEST_MODULE:is_full(Q)),
     ?_assertEqual(false, ?TEST_MODULE:is_full(Q1)),
     ?_assertEqual(true, ?TEST_MODULE:is_full(Q2)),
     ?_assertEqual(true, ?TEST_MODULE:is_full(Q3))].

max_len(badarg) ->
    L = [foo, [[]], <<"not_lqueue">>],
    [?_assertException(error, badarg, ?TEST_MODULE:max_len(I)) || I <- L];
max_len(valid) ->
    L = [1, 2, 10, 1000],
    [?_assertEqual(I, ?TEST_MODULE:max_len(?TEST_MODULE:new(I))) || I <- L].

len(badarg) ->
    L = [10, 0.123, {foo, bar}],
    [?_assertException(error, badarg, ?TEST_MODULE:len(I)) || I <- L];
len(valid) ->
    Q = ?TEST_MODULE:new(3),
    Q1 = ?TEST_MODULE:in({a, 1}, Q),
    Q2 = ?TEST_MODULE:in({b, 2}, Q1),
    Q3 = ?TEST_MODULE:in({c, 3}, Q2),
    Q4 = ?TEST_MODULE:in({d, 4}, Q3),
    [?_assertEqual(0, ?TEST_MODULE:len(Q)),
     ?_assertEqual(1, ?TEST_MODULE:len(Q1)),
     ?_assertEqual(2, ?TEST_MODULE:len(Q2)),
     ?_assertEqual(3, ?TEST_MODULE:len(Q3)),
     ?_assertEqual(3, ?TEST_MODULE:len(Q4))].

member(badarg) ->
    L = [<<>>, {}, foo],
    [?_assertException(error, badarg, ?TEST_MODULE:member(foo, I)) || I <- L];
member(valid) ->
    Q = ?TEST_MODULE:new(2),
    Q1 = ?TEST_MODULE:in(foo, Q),
    Q2 = ?TEST_MODULE:in(bar, Q1),
    Q3 = ?TEST_MODULE:in(baz, Q2),
    [?_assertEqual(false, ?TEST_MODULE:member(foo, Q)),
     ?_assertEqual(true, ?TEST_MODULE:member(foo, Q1)),
     ?_assertEqual(true, ?TEST_MODULE:member(foo, Q2)),
     ?_assertEqual(false, ?TEST_MODULE:member(foo, Q3)),
     ?_assertEqual(true, ?TEST_MODULE:member(bar, Q3)),
     ?_assertEqual(true, ?TEST_MODULE:member(baz, Q3))].

keymember(badarg) ->
    L = [[], [<<"A">>]],
    [?_assertException(error, badarg, ?TEST_MODULE:keymember(foo, I)) || I <- L];
keymember(valid) ->
    Q = ?TEST_MODULE:new(2),
    Q1 = ?TEST_MODULE:in({foo, 1}, Q),
    Q2 = ?TEST_MODULE:in({bar, 10}, Q1),
    Q3 = ?TEST_MODULE:in({baz, 100}, Q2),
    [?_assertEqual(false, ?TEST_MODULE:keymember(foo, Q)),
     ?_assertEqual(true, ?TEST_MODULE:keymember(foo, Q1)),
     ?_assertEqual(true, ?TEST_MODULE:keymember(foo, Q2)),
     ?_assertEqual(false, ?TEST_MODULE:keymember(foo, Q3)),
     ?_assertEqual(true, ?TEST_MODULE:keymember(bar, Q3)),
     ?_assertEqual(true, ?TEST_MODULE:keymember(baz, Q3))].

find(badarg_lqueue) ->
    L = [1.23, {0, 0, [], []}],
    F = fun(X) when X > 0 -> true;
           (_) -> false
        end,
    [?_assertException(error, badarg, ?TEST_MODULE:find(F, I)) || I <- L];
find(badarg_pred) ->
    Q = ?TEST_MODULE:new(2),
    LF = [fun(A, B) -> A + B end, atom, {foo, bar}, fun() -> ok end],
    [?_assertException(error, badarg, ?TEST_MODULE:find(F, Q)) || F <- LF];
find(valid) ->
    Q = ?TEST_MODULE:new(2),       %% []
    Q1 = ?TEST_MODULE:in(100, Q),  %% [100]
    Q2 = ?TEST_MODULE:in(200, Q1), %% [100, 200]
    Q3 = ?TEST_MODULE:in(300, Q2), %% [200, 300]
    Q4 = ?TEST_MODULE:in(400, Q3), %% [300, 400]
    F1 = fun(V) when V >= 100 -> true;
            (_) -> false
         end,
    F2 = fun(V) when V > 1000 -> true;
            (_) -> false
         end,
    [?_assertEqual(false, ?TEST_MODULE:find(F1, Q)),
     ?_assertEqual(false, ?TEST_MODULE:find(F2, Q)),
     ?_assertEqual({value, 100}, ?TEST_MODULE:find(F1, Q1)),
     ?_assertEqual(false, ?TEST_MODULE:find(F2, Q1)),
     ?_assertEqual({value, 100}, ?TEST_MODULE:find(F1, Q2)),
     ?_assertEqual(false, ?TEST_MODULE:find(F2, Q2)),
     ?_assertEqual({value, 200}, ?TEST_MODULE:find(F1, Q3)),
     ?_assertEqual(false, ?TEST_MODULE:find(F2, Q3)),
     ?_assertEqual({value, 300}, ?TEST_MODULE:find(F1, Q4)),
     ?_assertEqual(false, ?TEST_MODULE:find(F2, Q4))].

keyfind(badarg) ->
    L = [{1, 2}, [<<>>], 100],
    [?_assertException(error, badarg, ?TEST_MODULE:keyfind(foo, I)) || I <- L];
keyfind(valid) ->
    Q = ?TEST_MODULE:new(3),
    Q1 = ?TEST_MODULE:in({a, 1}, Q),
    Q2 = ?TEST_MODULE:in({b, 2}, Q1),
    Q3 = ?TEST_MODULE:in({c, 3}, Q2),
    Q4 = ?TEST_MODULE:in({d, 4}, Q3),
    [?_assertEqual(false, ?TEST_MODULE:keyfind(a, Q)),
     ?_assertEqual(false, ?TEST_MODULE:keyfind(x, Q)),
     ?_assertEqual({a, 1}, ?TEST_MODULE:keyfind(a, Q1)),
     ?_assertEqual(false, ?TEST_MODULE:keyfind(x, Q1)),
     ?_assertEqual(false, ?TEST_MODULE:keyfind(c, Q2)),
     ?_assertEqual({c, 3}, ?TEST_MODULE:keyfind(c, Q3)),
     ?_assertEqual(false, ?TEST_MODULE:keyfind(a, Q4)),
     ?_assertEqual({d, 4}, ?TEST_MODULE:keyfind(d, Q4))].

keyreplace(badarg) ->
    L = [{foo, bar, baz}, {1, 1, [], []}, <<"foo">>],
    [?_assertException(error, badarg, ?TEST_MODULE:keyreplace(foo, 10, I)) || I <- L];
keyreplace(valid) ->
    Q = ?TEST_MODULE:new(4),
    Q1 = ?TEST_MODULE:in({a, 1}, Q),
    Q2 = ?TEST_MODULE:in({b, 2}, Q1),
    Q3 = ?TEST_MODULE:in({c, 3}, Q2),
    Q4 = ?TEST_MODULE:in({c, 4}, Q3),
    [?_assertEqual([], ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(a, 2, Q))),
     ?_assertEqual([{a, 1}], ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(b, 10, Q1))),
     ?_assertEqual([{a, 2}], ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(a, 2, Q1))),
     ?_assertEqual([{a, 1}, {b, 2}], ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(b, 2, Q2))),
     ?_assertEqual([{a, 1}, {b, 2}], ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(x, 10, Q2))),
     ?_assertEqual([{a, 10}, {b, 2}], ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(a, 10, Q2))),
     ?_assertEqual([{a, 1}, {b, 20}], ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(b, 20, Q2))),
     ?_assertEqual([{a, 1}, {b, 2}, {c, 30}],
                   ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(c, 30, Q3))),
     ?_assertEqual([{a, 10}, {b, 2}, {c, 3}],
                   ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(a, 10, Q3))),
     ?_assertEqual([{a, 1}, {b, 20}, {c, 3}],
                   ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(b, 20, Q3))),
     ?_assertEqual([{a, 1}, {b, 2}, {c, 3}, {c, 4}],
                   ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(x, 100, Q4))),
     ?_assertEqual([{a, 1}, {b, 2}, {c, 40}, {c, 40}],
                   ?TEST_MODULE:to_list(?TEST_MODULE:keyreplace(c, 40, Q4)))].

from_and_to_list(badarg) ->
    L1 = [{{}, 0}, {atom, 10}, {[123], 0}, {[1, 2, 3], 2}, {[], []}],
    L2 = [12.34, {}, not_lqueue],
    [?_assertException(error, badarg, ?TEST_MODULE:from_list(IL, IM)) || {IL, IM} <- L1] ++
    [?_assertException(error, badarg, ?TEST_MODULE:to_list(I)) || I <- L2];
from_and_to_list(valid) ->
    [?_assertEqual([], ?TEST_MODULE:to_list(?TEST_MODULE:from_list([], 1))),
     ?_assertEqual([1], ?TEST_MODULE:to_list(?TEST_MODULE:from_list([1], 1))),
     ?_assertEqual([1, 2], ?TEST_MODULE:to_list(?TEST_MODULE:from_list([1, 2], 2))),
     ?_assertEqual([1, 2, 3], ?TEST_MODULE:to_list(?TEST_MODULE:from_list([1, 2, 3], 100)))].

in(badarg) ->
    L = [atom, [], {}, <<>>],
    [?_assertException(error, badarg, ?TEST_MODULE:in(foo, I)) || I <- L];
in(valid) ->
    Q1 = ?TEST_MODULE:new(1),
    Q1_1 = ?TEST_MODULE:in({a, 1}, Q1),
    Q1_2 = ?TEST_MODULE:in({b, 2}, Q1_1),
    Q1_3 = ?TEST_MODULE:in({c, 3}, Q1_2),
    Q1_4 = ?TEST_MODULE:in({d, 4}, Q1_3),
    Q2 = ?TEST_MODULE:new(2),
    Q2_1 = ?TEST_MODULE:in({a, 1}, Q2),
    Q2_2 = ?TEST_MODULE:in({b, 2}, Q2_1),
    Q2_3 = ?TEST_MODULE:in({c, 3}, Q2_2),
    Q2_4 = ?TEST_MODULE:in({d, 4}, Q2_3),
    [?_assertEqual([], ?TEST_MODULE:to_list(Q1)),
     ?_assertEqual([{a, 1}], ?TEST_MODULE:to_list(Q1_1)),
     ?_assertEqual([{b, 2}], ?TEST_MODULE:to_list(Q1_2)),
     ?_assertEqual([{c, 3}], ?TEST_MODULE:to_list(Q1_3)),
     ?_assertEqual([{d, 4}], ?TEST_MODULE:to_list(Q1_4)),
     ?_assertEqual([], ?TEST_MODULE:to_list(Q2)),
     ?_assertEqual([{a, 1}], ?TEST_MODULE:to_list(Q2_1)),
     ?_assertEqual([{a, 1}, {b, 2}], ?TEST_MODULE:to_list(Q2_2)),
     ?_assertEqual([{b, 2}, {c, 3}], ?TEST_MODULE:to_list(Q2_3)),
     ?_assertEqual([{c, 3}, {d, 4}], ?TEST_MODULE:to_list(Q2_4))].

out(badarg) ->
    L = [<<"some binary">>, {foo}, [bar]],
    [?_assertException(error, badarg, ?TEST_MODULE:out(I)) || I <- L];
out(valid) ->
    Q1 = ?TEST_MODULE:new(3),             %% []
    Q1_1 = ?TEST_MODULE:in({a, 1}, Q1),   %% [{a, 1}]
    Q1_2 = ?TEST_MODULE:in({b, 2}, Q1_1), %% [{a, 1}, {b, 2}]
    Q1_3 = ?TEST_MODULE:in({c, 3}, Q1_2), %% [{a, 1}, {b, 2}, {c, 3}]
    Q1_4 = ?TEST_MODULE:in({d, 4}, Q1_3), %% [{b, 2}, {c, 3}, {d, 4}]
    Q2 = ?TEST_MODULE:new(2),
    Q2_1 = ?TEST_MODULE:in({a, 1}, Q2),
    Q2_2 = ?TEST_MODULE:in({b, 2}, Q2_1),
    Q2_3 = ?TEST_MODULE:in({c, 3}, Q2_2),
    Q2_4 = ?TEST_MODULE:in({d, 4}, Q2_3),
    {{value, _}, Q2_5} = ?TEST_MODULE:out(Q2_4),
    {{value, _}, Q2_6} = ?TEST_MODULE:out(Q2_5),
    [?_assertMatch({empty, _}, ?TEST_MODULE:out(Q1)),
     ?_assertMatch({{value, {a, 1}}, _}, ?TEST_MODULE:out(Q1_1)),
     ?_assertMatch({{value, {a, 1}}, _}, ?TEST_MODULE:out(Q1_2)),
     ?_assertMatch({{value, {a, 1}}, _}, ?TEST_MODULE:out(Q1_3)),
     ?_assertMatch({{value, {b, 2}}, _}, ?TEST_MODULE:out(Q1_4)),
     ?_assertEqual([], ?TEST_MODULE:to_list(Q2_6)),
     ?_assertMatch({empty, _}, ?TEST_MODULE:out(Q2_6))].

get(badarg) ->
    L = [<<>>, {}, atom],
    [?_assertException(error, badarg, ?TEST_MODULE:get(I)) || I <- L];
get(valid) ->
    Q = ?TEST_MODULE:new(3),
    Q1 = ?TEST_MODULE:in({a, 1}, Q),
    Q2 = ?TEST_MODULE:in({b, 2}, Q1),
    Q3 = ?TEST_MODULE:in({c, 3}, Q2),
    Q4 = ?TEST_MODULE:in({d, 4}, Q3),
    [?_assertEqual({error, empty}, ?TEST_MODULE:get(Q)),
     ?_assertEqual({ok, {a, 1}}, ?TEST_MODULE:get(Q1)),
     ?_assertEqual({ok, {a, 1}}, ?TEST_MODULE:get(Q2)),
     ?_assertEqual({ok, {a, 1}}, ?TEST_MODULE:get(Q3)),
     ?_assertEqual({ok, {b, 2}}, ?TEST_MODULE:get(Q4))].

get_r(badarg) ->
    L = [{10, 1, [x], []}, [lqueue]],
    [?_assertException(error, badarg, ?TEST_MODULE:get_r(I)) || I <- L];
get_r(valid) ->
    Q = ?TEST_MODULE:new(3),
    Q1 = ?TEST_MODULE:in({a, 1}, Q),
    Q2 = ?TEST_MODULE:in({b, 2}, Q1),
    Q3 = ?TEST_MODULE:in({c, 3}, Q2),
    Q4 = ?TEST_MODULE:in({d, 4}, Q3),
    [?_assertEqual({error, empty}, ?TEST_MODULE:get_r(Q)),
     ?_assertEqual({ok, {a, 1}}, ?TEST_MODULE:get_r(Q1)),
     ?_assertEqual({ok, {b, 2}}, ?TEST_MODULE:get_r(Q2)),
     ?_assertEqual({ok, {c, 3}}, ?TEST_MODULE:get_r(Q3)),
     ?_assertEqual({ok, {d, 4}}, ?TEST_MODULE:get_r(Q4))].

