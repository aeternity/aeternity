-module(aesc_db).
-export([create_tables/1]).    %% (Mode) -> ok


create_tables(Mode) ->
    Specs = lists:flatten([M:table_specs(Mode) || M <- modules()]),
    [{atomic, ok} = mnesia:create_table(Tab, Spec)
     || {Tab, Spec} <- Specs].

modules() ->
    [aesc_state_cache].
