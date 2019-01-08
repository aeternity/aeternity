-module(aesc_window).

-export([new/0, new/1,
         change_keep/2,
         add/2,
         pop/1,
         keyfind/3,
         keymember/3,
         info_find/3,
         to_list/1]).

-export_type([window/0]).

-define(KEEP, 10).

-type size()   :: non_neg_integer().
-type entry()  :: any().

-record(w, { n = 0         :: non_neg_integer()
           , keep = ?KEEP  :: size()
           , a = []        :: [entry()]
           , b = []        :: [entry()]
           }).

-type window() :: #w{}.

-spec new() -> window().
new() ->
    #w{}.

-spec new(size()) -> window().
new(Sz) when is_integer(Sz), Sz >= 0 ->
    #w{keep = Sz}.

-spec change_keep(size(), window()) -> window().
change_keep(Keep, #w{} = W) when is_integer(Keep), Keep >= 0 ->
    W#w{keep = Keep}.

-spec add(entry(), window()) -> window().
add(Item, #w{n = N, a = A, keep = Keep} = W) when N < Keep ->
    W#w{n = N+1, a = [Item|A]};
add(Item, #w{a = A} = W) ->
    W#w{n = 1, a = [Item], b = A}.

-spec pop(window()) -> {entry(), window()} | error.
pop(#w{a = [], b = []}) ->
    error;
pop(#w{a = [], b = [H|T], n = N} = W) ->
    {H, W#w{n = N-1, b = T}};
pop(#w{a = [H|T], n = N} = W) ->
    {H, W#w{n = N-1, a = T}}.

-spec to_list(window()) -> [entry()].
to_list(#w{a = A, b = B}) ->
    A ++ B.

%% Like lists:keyfind/3. Finds the most recent match (if any),
%% since items are essentially stored in LIFO fashion.
-spec keyfind(any(), non_neg_integer(), window()) -> false | entry().
keyfind(K, Pos, #w{a = A, b = B}) ->
    case lists:keyfind(K, Pos, A) of
        false ->
            lists:keyfind(K, Pos, B);
        Other ->
            Other
    end.

-spec keymember(any(), non_neg_integer(), window()) -> boolean().
keymember(K, Pos, #w{a = A, b = B}) ->
    lists:keymember(K, Pos, A)
        orelse lists:keymember(K, Pos, B).

%% Like keyfind/3, but instead of `Key', A list of `{Key, Value}' pairs is
%% matched against map values in position `Pos' (entries where the `Pos'th
%% element is not a map are skipped). If a value in the `KVL' is `undefined',
%% this will match either the value `undefined' or the key being missing.
%%
-spec info_find([{any(), any()}], non_neg_integer(), window()) ->
                       false | entry().
info_find(KVL, Pos, #w{a = A, b = B}) when is_list(KVL) ->
    case info_find_(KVL, Pos, A) of
        false ->
            info_find_(KVL, Pos, B);
        Other ->
            Other
    end.

info_find_(KVL, Pos, [H|T]) when is_map(element(Pos, H)) ->
    case match_info(KVL, element(Pos, H)) of
        true ->
            H;
        false ->
            info_find_(KVL, Pos, T)
    end;
info_find_(KVL, Pos, [_|T]) ->
    info_find_(KVL, Pos, T);
info_find_(_, _, []) ->
    false.

match_info([{K, V}|T], Map) when is_map(Map) ->
    case maps:get(K, Map, undefined) of
        V -> match_info(T, Map);
        _ -> false
    end;
match_info([], _) ->
    true;
match_info(_, _) ->
    false.
