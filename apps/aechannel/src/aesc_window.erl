-module(aesc_window).

-export([new/0, new/1,
         change_keep/2,
         add/2,
         pop/1,
         size/1,
         keyfind/3,
         keymember/3,
         info_find/3,
         to_list/1]).

-export([record_fields/1]).

-export_type([window/0]).

-define(KEEP, 10).

-type size()   :: non_neg_integer().
-type entry()  :: any().

%% This is a bounded buffer, optimized for performance.
%% A counter, `na`, keeps track of the number of elements
%% in the `a` list (new items are prepended to `a`). When `na` exceeds `keep`,
%% the contents of `a` are shifted to `b`, and any previous contents of `b`
%% are discarded. This way, adding to the buffer is always O(1).
%%
%% The total size of the buffer can be `> keep`, but no more than `2*keep`
%% (except temporarily as a result of calling `change_keep/2`.)
%% The size can be read via `size/1` and amounts to `na + nb`.
%% Strictly speaking, we only really need to keep track of `na` to know when to
%% shift, but keeping track of the total size seems like the decent thing to do.
%%
-record(w, { na = 0        :: non_neg_integer()
           , nb = 0        :: non_neg_integer()
           , keep = ?KEEP  :: size()
           , a = []        :: [entry()]
           , b = []        :: [entry()]
           }).

-type window() :: #w{}.

%% ==================================================================
%% Tracing support
record_fields(w) -> record_info(fields, w);
record_fields(_) -> no.
%% ==================================================================


-spec new() -> window().
new() ->
    #w{}.

-spec new(size()) -> window().
new(Sz) when is_integer(Sz), Sz >= 0 ->
    #w{keep = Sz}.

%% When changing `keep`, we do not modify (e.g. truncate) the data set.
%% This is for performance reasons, and because we don't strive to keep
%% the exact size anyway: `keep` is an approximate number.
-spec change_keep(size(), window()) -> window().
change_keep(Keep, #w{} = W) when is_integer(Keep), Keep >= 0 ->
    W#w{keep = Keep}.

-spec add(entry(), window()) -> window().
add(Item, #w{na = N, a = A, keep = Keep} = W) when N < Keep ->
    W#w{na = N+1, a = [Item|A]};
add(Item, #w{na = PrevNa, a = A} = W) ->
    W#w{na = 1, a = [Item], nb = PrevNa, b = A}.

-spec pop(window()) -> {entry(), window()} | error.
pop(#w{a = [], b = []}) ->
    error;
pop(#w{a = [], b = [H|T], nb = N} = W) ->
    {H, W#w{nb = N-1, b = T}};
pop(#w{a = [H|T], na = N} = W) ->
    {H, W#w{na = N-1, a = T}}.

-spec size(window()) -> non_neg_integer().
size(#w{na = Na, nb = Nb}) -> Na + Nb.

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
