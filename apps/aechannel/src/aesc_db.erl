-module(aesc_db).
-export([ create_tables/1    %% (Mode) -> ok
        , check_tables/1     %% (Acc)  -> Acc1
        ]).

create_tables(Mode) ->
    Specs = lists:flatten([M:table_specs(Mode) || {missing_table, M} <- check_tables([])]),
    [{atomic, ok} = mnesia:create_table(Tab, Spec) || {Tab, Spec} <- Specs].

check_tables(Acc) ->
    Errors = lists:foldl(
      fun(M, Acc1) ->
              M:check_tables(Acc1)
      end, Acc, modules()),

    [ok = M:migrate(From, To) || {vsn_fail, M, [{expected, From}, {got, To}]} = Error <- Errors, filter_migrations(Error)],
    [Error || Error <- Errors, not filter_migrations(Error)].

modules() ->
    [aesc_state_cache].

filter_migrations({vsn_fail, _, _}) -> true;
filter_migrations(_) -> false.
