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

    %% Run migrations for each migration error. This will be later moved to aec_db.
    [  ok = M:migrate(From, To)
    || {vsn_fail, M, [{expected, From}, {got, To}]} = Error <- Errors
    ,  is_migration_error(Error)],
    [Error || Error <- Errors, not is_migration_error(Error)].

modules() ->
    [aesc_state_cache].

is_migration_error({vsn_fail, _, _}) -> true;
is_migration_error(_) -> false.
