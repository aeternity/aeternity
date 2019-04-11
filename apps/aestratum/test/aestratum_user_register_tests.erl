-module(aestratum_user_register_tests).

-include_lib("eunit/include/eunit.hrl").

-compile({no_auto_import, [size/1]}).

-define(TEST_MODULE, aestratum_user_register).

-define(TEST_USER1, <<"ak_123o45ABCiANzqxxxxUUrrrJuDuxU61zCGr9LJCwtTUg34567">>).
-define(TEST_USER2, <<"ak_123o45ABCiANzqxxxxUUrrrJuDuxU61zCGr9LJCwtTUg34568">>).
-define(TEST_USER3, <<"ak_123o45ABCiANzqxxxxUUrrrJuDuxU61zCGr9LJCwtTUg34569">>).

user_register_test_() ->
    {foreach,
     fun() ->
             {ok, _Pid} = aestratum_user_register:start_link(),
             ok
     end,
     fun(_) ->
             ok = aestratum_user_register:stop()
     end,
     [fun(_) -> add(exception) end,
      fun(_) -> add(valid) end,
      fun(_) -> del(exception) end,
      fun(_) -> del(valid_nonexistent) end,
      fun(_) -> del(valid_existent_by_user) end,
      fun(_) -> del(valid_existent_by_conn_pid) end,
      fun(_) -> member(exception) end,
      fun(_) -> member(valid_nonexistent) end,
      fun(_) -> member(valid_existent_by_user) end,
      fun(_) -> member(valid_existent_by_conn_pid) end,
      fun(_) -> find(exception) end,
      fun(_) -> find(valid_nonexistent) end,
      fun(_) -> find(valid_existent_by_user) end,
      fun(_) -> find(valid_existent_by_conn_pid) end,
      fun(_) -> notify(no_users) end,
      fun(_) -> notify(single_user) end,
      fun(_) -> notify(multiple_users) end,
      fun(_) -> size(empty) end,
      fun(_) -> size(single_user) end,
      fun(_) -> size(multiple_users) end,
      {<<"complex check">>, fun complex_check/0}]}.


add(exception) ->
    T = <<"add - exception">>,
    L = [{atom, atom}, {123, not_pid}, {{foo, bar}, 1.987},
         {<<"foo">>, 999}, {"aaaa", new_pid()}],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:add(U, P))}
     || {U, P} <- L];
add(valid) ->
    T = <<"add - valid">>,
    L = [{?TEST_USER1, new_pid()}],
    [{T, ?_assertEqual(ok, ?TEST_MODULE:add(U, P))} || {U, P} <- L].

del(exception) ->
    T = <<"del - exception">>,
    L = [1, {foo, bar}, 6.321, "abcd", atom],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:del(I))} || I <- L];
del(valid_nonexistent) ->
    T = <<"del - valid nonexistent">>,
    L = [<<"user_x">>, ?TEST_USER1, new_pid()],
    [{T, ?_assertEqual({error, not_found}, ?TEST_MODULE:del(I))} || I <- L];
del(valid_existent_by_user) ->
    T = <<"del - valid existent by user">>,
    {User, _ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(ok, ?TEST_MODULE:del(User))}];
del(valid_existent_by_conn_pid) ->
    T = <<"del - valid existent by conn PID">>,
    {_User, ConnPid} = prep_single_user(),
    [{T, ?_assertEqual(ok, ?TEST_MODULE:del(ConnPid))}].

member(exception) ->
    T = <<"member - exception">>,
    L = ["foo", bar, [], atom],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:member(I))} || I <- L];
member(valid_nonexistent) ->
    T = <<"member - valid_nonexistent">>,
    L = [<<"foo">>, ?TEST_USER1, new_pid()],
    [{T, ?_assertEqual(false, ?TEST_MODULE:member(I))} || I <- L];
member(valid_existent_by_user) ->
    T = <<"member - valid_existent_by_user">>,
    {User, _ConnPid} = prep_single_user(),
    [{T, ?_assert(?TEST_MODULE:member(User))}];
member(valid_existent_by_conn_pid) ->
    T = <<"member - valid_existent_by_conn_pid">>,
    {_User, ConnPid} = prep_single_user(),
    [{T, ?_assert(?TEST_MODULE:member(ConnPid))}].

find(exception) ->
    T = <<"find - exception">>,
    L = [1, {foo, bar}, 542.321, atom, "some_string"],
    [{T, ?_assertException(error, function_clause, ?TEST_MODULE:find(I))} || I <- L];
find(valid_nonexistent) ->
    T = <<"find - valid nonexistent">>,
    L = [<<"foo">>, ?TEST_USER1, new_pid()],
    [{T, ?_assertEqual({error, not_found}, ?TEST_MODULE:find(I))} || I <- L];
find(valid_existent_by_user) ->
    T = <<"find - valid existent by user">>,
    {User, ConnPid} = prep_single_user(),
    [{T, ?LET({ok, V}, ?TEST_MODULE:find(User),
              ?_assert(maps:get(conn_pid, V) =:= ConnPid))}];
find(valid_existent_by_conn_pid) ->
    T = <<"find - valid existent by conn PID">>,
    {User, ConnPid} = prep_single_user(),
    [{T, ?LET({ok, V}, ?TEST_MODULE:find(ConnPid),
              ?_assert(maps:get(user, V) =:= User))}].

notify(no_users) ->
    T = <<"notify - no users">>,
    [{T, ?_assertEqual(ok, ?TEST_MODULE:notify(<<"message">>))}];
notify(single_user) ->
    T = <<"notify - single user">>,
    Msg = <<"test message">>,
    {_User, ConnPid} = prep_single_receiver(Msg),
    ok = ?TEST_MODULE:notify(Msg),
    [{T, ?_assertEqual(Msg, receiver_result(self(), ConnPid))}];
notify(multiple_users) ->
    T = <<"notify - multiple users">>,
    Msg = #{msg => test_msg},
    [{?TEST_USER1, ConnPid1},
     {?TEST_USER2, ConnPid2},
     {?TEST_USER3, ConnPid3}] = prep_multiple_receivers(Msg),
    ok = ?TEST_MODULE:notify(Msg),
    [{T, ?_assertEqual(Msg, receiver_result(self(), ConnPid1))},
     {T, ?_assertEqual(Msg, receiver_result(self(), ConnPid2))},
     {T, ?_assertEqual(Msg, receiver_result(self(), ConnPid3))}].

size(empty) ->
    T = <<"size - empty">>,
    [{T, ?_assertEqual(0, ?TEST_MODULE:size())}];
size(single_user) ->
    T = <<"size - single user">>,
    prep_single_user(),
    [{T, ?_assertEqual(1, ?TEST_MODULE:size())}];
size(multiple_users) ->
    T = <<"size - multiple users">>,
    Us = prep_multiple_users(),
    [{T, ?_assertEqual(length(Us), ?TEST_MODULE:size())}].

complex_check() ->
    [{?TEST_USER1, ConnPid1},
     {?TEST_USER2, ConnPid2},
     {?TEST_USER3, _ConnPid3}] = prep_multiple_users(),
    ?assertEqual(3, ?TEST_MODULE:size()),
    ?assertEqual(ok, ?TEST_MODULE:add(<<"foo">>, new_pid())),
    ?assertEqual(4, ?TEST_MODULE:size()),

    ?assertEqual({error, not_found}, ?TEST_MODULE:del(<<"bar">>)),
    ?assertEqual(ok, ?TEST_MODULE:del(<<"foo">>)),
    ?assertEqual(3, ?TEST_MODULE:size()),

    ?LET({ok, V}, ?TEST_MODULE:find(?TEST_USER1),
         ?assert(maps:get(conn_pid, V) =:= ConnPid1)),
    ?assertEqual(ok, ?TEST_MODULE:del(ConnPid1)),
    ?assertEqual({error, not_found}, ?TEST_MODULE:find(?TEST_USER1)),

    ?LET({ok, V}, ?TEST_MODULE:find(ConnPid2),
         ?assert(maps:get(user, V) =:= ?TEST_USER2)),
    ?assertEqual(ok, ?TEST_MODULE:del(?TEST_USER2)),
    ?assertEqual({error, not_found}, ?TEST_MODULE:find(ConnPid2)),

    ?assertEqual(1, ?TEST_MODULE:size()).

prep_single_user() ->
    ConnPid = new_pid(),
    ok = ?TEST_MODULE:add(?TEST_USER1, ConnPid),
    1 = ?TEST_MODULE:size(),
    {?TEST_USER1, ConnPid}.

prep_single_receiver(Msg) ->
    ConnPid = new_receiver(Msg),
    ok = ?TEST_MODULE:add(?TEST_USER1, ConnPid),
    1 = ?TEST_MODULE:size(),
    {?TEST_USER1, ConnPid}.

prep_multiple_users() ->
    ConnPid1 = new_pid(),
    ConnPid2 = new_pid(),
    ConnPid3 = new_pid(),
    ok = ?TEST_MODULE:add(?TEST_USER1, ConnPid1),
    ok = ?TEST_MODULE:add(?TEST_USER2, ConnPid2),
    ok = ?TEST_MODULE:add(?TEST_USER3, ConnPid3),
    3 = ?TEST_MODULE:size(),
    [{?TEST_USER1, ConnPid1}, {?TEST_USER2, ConnPid2}, {?TEST_USER3, ConnPid3}].

prep_multiple_receivers(Msg) ->
    ConnPid1 = new_receiver(Msg),
    ConnPid2 = new_receiver(Msg),
    ConnPid3 = new_receiver(Msg),
    ok = ?TEST_MODULE:add(?TEST_USER1, ConnPid1),
    ok = ?TEST_MODULE:add(?TEST_USER2, ConnPid2),
    ok = ?TEST_MODULE:add(?TEST_USER3, ConnPid3),
    3 = ?TEST_MODULE:size(),
    [{?TEST_USER1, ConnPid1}, {?TEST_USER2, ConnPid2}, {?TEST_USER3, ConnPid3}].

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

