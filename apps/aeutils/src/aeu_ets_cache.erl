%%%=============================================================================
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%%    Helpers for caching data in ETS
%%% @end
%%%=============================================================================
-module(aeu_ets_cache).

-export([get/3,
         reinit/3]).

-spec get(atom(), term(), fun(() -> term())) -> term().
get(TableName, EtsKey, ComputeFun) ->
    try
        [{EtsKey, Result}] = ets:lookup(TableName, EtsKey),
        Result
    catch
        _:_ ->
            case ets:whereis(TableName) of
                undefined ->
                    ets:new(TableName, [named_table, {read_concurrency, true}, public]);
                _ -> pass
            end,
            Res = ComputeFun(),
            ets:insert(TableName, {EtsKey, Res}),
            Res
    end.

-spec reinit(atom(), term(), fun(() -> term())) -> term().
reinit(TableName, EtsKey, ComputeFun) ->
    ets:delete(TableName),
    get(TableName, EtsKey, ComputeFun).
