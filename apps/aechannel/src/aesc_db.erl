-module(aesc_db).
-export([ create_tables/1    %% (Mode) -> ok
        , check_tables/1     %% (Acc)  -> Acc1
        ]).

create_tables(Mode) ->
    Specs = lists:flatten([M:table_specs(Mode) || M <- modules()]),
    [{atomic, ok} = mnesia:create_table(Tab, Spec)
     || {Tab, Spec} <- Specs].

check_tables(Acc) ->
    lists:foldl(
      fun(M, Acc1) ->
              M:check_tables(Acc1)
      end, Acc, modules()).

modules() ->
    [aesc_state_cache].
