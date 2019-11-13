-module(aesc_db).
-export([ create_tables/1    %% (Mode) -> ok
        , check_tables/1     %% (Acc)  -> Acc1
        ]).

create_tables(Mode) ->
    AllSpecs = all_specs(Mode),
    Specs = lists:flatten([proplists:lookup(Table, AllSpecs) || {missing_table, Table} <- check_tables([])]),
    [{atomic, ok} = mnesia:create_table(Tab, Spec) || {Tab, Spec} <- Specs].

check_tables(Acc) ->
    lists:foldl(
      fun(M, Acc1) ->
              M:check_tables(Acc1)
      end, Acc, modules()).

modules() ->
    [aesc_state_cache].

all_specs(Mode) ->
    lists:flatten([M:table_specs(Mode) || M <- modules()]).
