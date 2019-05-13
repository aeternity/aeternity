-module(aestratum_user_register_tests).

-include_lib("eunit/include/eunit.hrl").

-compile({no_auto_import, [size/1]}).

-define(TEST_MODULE, aestratum_user_register).
-define(ENV_MODULE, aestratum_env).

-define(TEST_ACCOUNT1, <<"ak_123o45ABCiANzqxxxxUUrrrJuDuxU61zCGr9LJCwtTUg34567">>).
-define(TEST_ACCOUNT2, <<"ak_123o45ABCiANzqxxxxUUrrrJuDuxU61zCGr9LJCwtTUg34568">>).
-define(TEST_ACCOUNT3, <<"ak_123o45ABCiANzqxxxxUUrrrJuDuxU61zCGr9LJCwtTUg34569">>).

-define(TEST_WORKER1, <<"worker1">>).
-define(TEST_WORKER2, <<"worker2">>).
-define(TEST_WORKER3, <<"worker3">>).

user_register_test_() ->
    {foreach,
     fun() ->
             {ok, _Pid} = aestratum_user_register:start_link(),
             ?ENV_MODULE:set(#{max_workers => 10}),
             ok
     end,
     fun(_) ->
             ok = aestratum_user_register:stop()
     end,
     [fun(_) -> add(exception) end,
      fun(_) -> add(valid) end,
      fun(_) -> add_worker(exception) end,
      fun(_) -> add_worker(valid_existent_account_worker_count_exhausted) end,
      fun(_) -> add_worker(valid_existent_account_existent_worker) end,
      fun(_) -> add_worker(valid_existent_account_nonexistent_worker) end,
      fun(_) -> del(exception) end,
      fun(_) -> del(valid_nonexistent) end,
      fun(_) -> del(valid_existent_by_account) end,
      fun(_) -> del(valid_existent_by_conn_pid) end,
      fun(_) -> member(exception) end,
      fun(_) -> member(valid_nonexistent) end,
      fun(_) -> member(valid_existent_by_account) end,
      fun(_) -> member(valid_existent_by_conn_pid) end,
      fun(_) -> member2(exception) end,
      fun(_) -> member2(valid_nonexistent_account) end,
      fun(_) -> member2(valid_nonexistent_conn_pid) end,
      fun(_) -> member2(valid_existent) end,
      fun(_) -> find(exception) end,
      fun(_) -> find(valid_nonexistent) end,
      fun(_) -> find(valid_existent_by_account) end,
      fun(_) -> find(valid_existent_by_conn_pid) end,
      fun(_) -> notify(no_users) end,
      fun(_) -> notify(single_user) end,
      fun(_) -> notify(multiple_users) end,
      fun(_) -> conn_pids(no_users) end,
      fun(_) -> conn_pids(single_user) end,
      fun(_) -> conn_pids(multiple_users) end,
      fun(_) -> size(empty) end,
      fun(_) -> size(single_user) end,
      fun(_) -> size(multiple_users) end,
      {<<"complex check">>, fun complex_check/0}]}.


add(exception) ->
    T = <<"add - exception">>,
    L = [{atom, atom, atom}, {0, 123, not_pid}, {{x, y}, {foo, bar}, 1.987},
         {<<"foo">>, 999, <<>>}, {<<"foo">>, "aaaa", new_pid()}],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:add(A, W, P))}
     || {A, W, P} <- L];
add(valid) ->
    T = <<"add - valid">>,
    L = [{?TEST_ACCOUNT1, ?TEST_WORKER1, new_pid()}],
    [{T, ?_assertEqual(ok, ?TEST_MODULE:add(A, W, P))} || {A, W, P} <- L].

add_worker(exception) ->
    T = <<"add_worker - exception">>,
    L = [{"", foo}, {{foo, bar}, 100}, {10.0, 11}],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:add_worker(A, W))}
     || {A, W} <- L];
add_worker(valid_existent_account_worker_count_exhausted) ->
    T = <<"add_worker - valid_existent_account_worker_count_exhausted">>,
    ?ENV_MODULE:set(#{max_workers => 1}),
    {?TEST_ACCOUNT1, ?TEST_WORKER1, _ConnPid} = prep_single_user(),
    [{T, ?_assertEqual({error, worker_count_exhausted},
                       ?TEST_MODULE:add_worker(?TEST_ACCOUNT1, ?TEST_WORKER2))}];
add_worker(valid_existent_account_existent_worker) ->
    T = <<"add_worker - valid_existent_account_existent_worker">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, _ConnPid} = prep_single_user(),
    [{T, ?_assertEqual({error, worker_already_present},
                       ?TEST_MODULE:add_worker(?TEST_ACCOUNT1, ?TEST_WORKER1))}];
add_worker(valid_existent_account_nonexistent_worker) ->
    T = <<"add_worker - valid_existent_account_nonexistent_worker">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, _ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(ok, ?TEST_MODULE:add_worker(?TEST_ACCOUNT1, ?TEST_WORKER2))}].

del(exception) ->
    T = <<"del - exception">>,
    L = [1, {foo, bar}, 6.321, "abcd", atom],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:del(I))} || I <- L];
del(valid_nonexistent) ->
    T = <<"del - valid nonexistent">>,
    L = [<<"user_x">>, ?TEST_ACCOUNT1, new_pid()],
    [{T, ?_assertEqual({error, not_found}, ?TEST_MODULE:del(I))} || I <- L];
del(valid_existent_by_account) ->
    T = <<"del - valid existent by account">>,
    {Account, _Worker, _ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(ok, ?TEST_MODULE:del(Account))}];
del(valid_existent_by_conn_pid) ->
    T = <<"del - valid existent by conn PID">>,
    {_Account, _Worker, ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(ok, ?TEST_MODULE:del(ConnPid))}].

member(exception) ->
    T = <<"member - exception">>,
    L = ["foo", bar, [], atom],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:member(I))} || I <- L];
member(valid_nonexistent) ->
    T = <<"member - valid_nonexistent">>,
    L = [<<"foo">>, ?TEST_ACCOUNT1, new_pid()],
    [{T, ?_assertEqual(false, ?TEST_MODULE:member(I))} || I <- L];
member(valid_existent_by_account) ->
    T = <<"member - valid_existent_by_account">>,
    {Account, _Worker, _ConnPid} = prep_single_user(),
    [{T, ?_assert(?TEST_MODULE:member(Account))}];
member(valid_existent_by_conn_pid) ->
    T = <<"member - valid_existent_by_conn_pid">>,
    {_Account, _Worker, ConnPid} = prep_single_user(),
    [{T, ?_assert(?TEST_MODULE:member(ConnPid))}].

member2(exception) ->
    T = <<"member2 - exception">>,
    L = [{foo, bar}, {<<>>, 100}, {"test", new_pid()}],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:member(A, P))} || {A, P} <- L];
member2(valid_nonexistent) ->
    T = <<"member2 - valid_nonexistent">>,
    L = [{?TEST_ACCOUNT1, new_pid()}, {<<"foo">>, new_pid}],
    [{T, ?_assertEqual(false, ?TEST_MODULE:member(A, P))} || {A, P} <- L];
member2(valid_nonexistent_account) ->
    T = <<"member2 - valid_nonexistent_account">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(neither, ?TEST_MODULE:member(?TEST_ACCOUNT2, ConnPid))}];
member2(valid_nonexistent_conn_pid) ->
    T = <<"member2 - valid_nonexistent_conn_pid">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, _ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(account_only, ?TEST_MODULE:member(?TEST_ACCOUNT1, new_pid()))}];
member2(valid_existent) ->
    T = <<"member2 - valid_existent">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(both, ?TEST_MODULE:member(?TEST_ACCOUNT1, ConnPid))}].

find(exception) ->
    T = <<"find - exception">>,
    L = [1, {foo, bar}, 542.321, atom, "some_string"],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:find(I))} || I <- L];
find(valid_nonexistent) ->
    T = <<"find - valid_nonexistent">>,
    L = [<<"foo">>, ?TEST_ACCOUNT1, new_pid()],
    [{T, ?_assertEqual({error, not_found}, ?TEST_MODULE:find(I))} || I <- L];
find(valid_existent_by_account) ->
    T = <<"find - valid_existent_by_account">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid} = prep_single_user(),
    [{T, ?LET({ok, V}, ?TEST_MODULE:find(?TEST_ACCOUNT1),
              ?_assert(maps:get(conn_pid, V) =:= ConnPid))}];
find(valid_existent_by_conn_pid) ->
    T = <<"find - valid_existent_by_conn_pid">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid} = prep_single_user(),
    [{T, ?LET({ok, V}, ?TEST_MODULE:find(ConnPid),
              ?_assert(maps:get(account, V) =:= ?TEST_ACCOUNT1))}].

notify(no_users) ->
    T = <<"notify - no_users">>,
    [{T, ?_assertEqual(ok, ?TEST_MODULE:notify(<<"message">>))}];
notify(single_user) ->
    T = <<"notify - single_user">>,
    Msg = <<"test message">>,
    {_User, _Worker, ConnPid} = prep_single_receiver(Msg),
    ok = ?TEST_MODULE:notify(Msg),
    [{T, ?_assertEqual(Msg, receiver_result(self(), ConnPid))}];
notify(multiple_users) ->
    T = <<"notify - multiple_users">>,
    Msg = #{msg => test_msg},
    [{?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid1},
     {?TEST_ACCOUNT2, ?TEST_WORKER2, ConnPid2},
     {?TEST_ACCOUNT3, ?TEST_WORKER3, ConnPid3}] = prep_multiple_receivers(Msg),
    ok = ?TEST_MODULE:notify(Msg),
    [{T, ?_assertEqual(Msg, receiver_result(self(), ConnPid1))},
     {T, ?_assertEqual(Msg, receiver_result(self(), ConnPid2))},
     {T, ?_assertEqual(Msg, receiver_result(self(), ConnPid3))}].

conn_pids(no_users) ->
    T = <<"conn_pids - no_users">>,
    [{T, ?_assertEqual([], ?TEST_MODULE:conn_pids())}];
conn_pids(single_user) ->
    T = <<"conn_pids - single_user">>,
    {?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid} = prep_single_user(),
    [{T, ?LET(Pids, ?TEST_MODULE:conn_pids(), ?_assert(Pids =:= [ConnPid]))}];
conn_pids(multiple_users) ->
    T = <<"conn_pids - multiple_users">>,
    [{?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid1},
     {?TEST_ACCOUNT2, ?TEST_WORKER2, ConnPid2},
     {?TEST_ACCOUNT3, ?TEST_WORKER3, ConnPid3}] = prep_multiple_users(),
    [{T, ?LET(Pids, ?TEST_MODULE:conn_pids(),
              ?_assertEqual({true, true, true},
                            {lists:member(ConnPid1, Pids),
                             lists:member(ConnPid2, Pids),
                             lists:member(ConnPid3, Pids)}))}].

size(empty) ->
    T = <<"size - empty">>,
    [{T, ?_assertEqual(0, ?TEST_MODULE:size())}];
size(single_user) ->
    T = <<"size - single_user">>,
    prep_single_user(),
    [{T, ?_assertEqual(1, ?TEST_MODULE:size())}];
size(multiple_users) ->
    T = <<"size - multiple_users">>,
    Us = prep_multiple_users(),
    [{T, ?_assertEqual(length(Us), ?TEST_MODULE:size())}].

complex_check() ->
    [{?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid1},
     {?TEST_ACCOUNT2, ?TEST_WORKER2, ConnPid2},
     {?TEST_ACCOUNT3, ?TEST_WORKER3, _ConnPid3}] = prep_multiple_users(),
    ?assertEqual(3, ?TEST_MODULE:size()),
    ?assertEqual(ok, ?TEST_MODULE:add(<<"foo">>, <<"bar">>, new_pid())),
    ?assertEqual(4, ?TEST_MODULE:size()),

    ?assertEqual({error, not_found}, ?TEST_MODULE:del(<<"bar">>)),
    ?assertEqual(ok, ?TEST_MODULE:del(<<"foo">>)),
    ?assertEqual(3, ?TEST_MODULE:size()),

    ?LET({ok, V}, ?TEST_MODULE:find(?TEST_ACCOUNT1),
         ?assert(maps:get(conn_pid, V) =:= ConnPid1)),
    ?assertEqual(ok, ?TEST_MODULE:del(ConnPid1)),
    ?assertEqual({error, not_found}, ?TEST_MODULE:find(?TEST_ACCOUNT1)),

    ?LET({ok, V}, ?TEST_MODULE:find(ConnPid2),
         ?assert(maps:get(account, V) =:= ?TEST_ACCOUNT2)),
    ?assertEqual(ok, ?TEST_MODULE:del(?TEST_ACCOUNT2)),
    ?assertEqual({error, not_found}, ?TEST_MODULE:find(ConnPid2)),

    ?assertEqual(1, ?TEST_MODULE:size()).

prep_single_user() ->
    ConnPid = new_pid(),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid),
    1 = ?TEST_MODULE:size(),
    {?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid}.

prep_single_receiver(Msg) ->
    ConnPid = new_receiver(Msg),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid),
    1 = ?TEST_MODULE:size(),
    {?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid}.

prep_multiple_users() ->
    ConnPid1 = new_pid(),
    ConnPid2 = new_pid(),
    ConnPid3 = new_pid(),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid1),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT2, ?TEST_WORKER2, ConnPid2),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT3, ?TEST_WORKER3, ConnPid3),
    3 = ?TEST_MODULE:size(),
    [{?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid1},
     {?TEST_ACCOUNT2, ?TEST_WORKER2, ConnPid2},
     {?TEST_ACCOUNT3, ?TEST_WORKER3, ConnPid3}].

prep_multiple_receivers(Msg) ->
    ConnPid1 = new_receiver(Msg),
    ConnPid2 = new_receiver(Msg),
    ConnPid3 = new_receiver(Msg),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid1),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT2, ?TEST_WORKER2, ConnPid2),
    ok = ?TEST_MODULE:add(?TEST_ACCOUNT3, ?TEST_WORKER3, ConnPid3),
    3 = ?TEST_MODULE:size(),
    [{?TEST_ACCOUNT1, ?TEST_WORKER1, ConnPid1},
     {?TEST_ACCOUNT2, ?TEST_WORKER2, ConnPid2},
     {?TEST_ACCOUNT3, ?TEST_WORKER3, ConnPid3}].

%% This is just to get a PID. The process itself is not important.
new_pid() ->
    spawn(fun() -> ok end).

new_receiver(Msg) ->
    spawn(fun() ->
                  Res =
                    receive
                        Msg    -> Msg;
                        _Other -> unexpected_msg
                    after
                        5000   -> receiver_msg_timeout
                    end,
                  receive
                      {get_result, From} -> From ! Res
                  end
          end).

receiver_result(From, ConnPid) ->
    ConnPid ! {get_result, From},
    receive Res -> Res end.

