-module(aestratum_lqueue).

-export([new/1,
         is_empty/1,
         is_full/1,
         max_len/1,
         len/1,
         member/2,
         keymember/2,
         find/2,
         keyfind/2,
         keyreplace/3
        ]).

-export([to_list/1,
         from_list/2
        ]).

-export([in/2,
         out/1,
         get/1,
         get_r/1
        ]).

-export_type([lqueue/0,
              length/0,
              max_length/0,
              item/0,
              key/0,
              value/0
             ]).

-define(VALID_TYPES(L, M, R, F),
        (is_integer(L) and is_integer(M) and is_list(R) and is_list(F))).

-define(VALID_LENGTH(L, M, R, F),
        ((length(R) + length(F)) =:= L) and (M > 0)).

-define(IS_LQUEUE(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L =< M)).

-define(IS_LQUEUE_FULL(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L =:= M)).

-define(IS_LQUEUE_EMPTY(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L =:= 0)).

-define(IS_LQUEUE_NOT_FULL(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L < M)).

-define(IS_LQUEUE_NOT_EMPTY(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L > 0) and (L =< M)).

-opaque lqueue()   :: {length(), max_length(), rear_list(), front_list()}.

-type length()     :: non_neg_integer().

-type max_length() :: pos_integer().

-type rear_list()  :: [item()].

-type front_list() :: [item()].

-type item()       :: term().

-type key()        :: term().

-type value()      :: term().

%% API.

-spec new(max_length()) -> lqueue().
new(M) when is_integer(M) and (M > 0) ->
    {0, M, [], []};
new(M) ->
    erlang:error(badarg, [M]).

-spec is_empty(lqueue()) -> boolean().
is_empty({L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    true;
is_empty({L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    false;
is_empty(LQ) ->
    erlang:error(badarg, [LQ]).

-spec is_full(lqueue()) -> boolean().
is_full({M, M, R, F}) when ?IS_LQUEUE_FULL(M, M, R, F) ->
    true;
is_full({L, M, R, F}) when ?IS_LQUEUE_NOT_FULL(L, M, R, F) ->
    false;
is_full(LQ) ->
    erlang:error(badarg, [LQ]).

-spec max_len(lqueue()) -> max_length().
max_len({L, M, R, F}) when ?IS_LQUEUE(L, M, R, F) ->
    M;
max_len(LQ) ->
    erlang:error(badarg, [LQ]).

-spec len(lqueue()) -> length().
len({L, M, R, F}) when ?IS_LQUEUE(L, M, R, F) ->
    L;
len(LQ) ->
    erlang:error(badarg, [LQ]).

-spec member(item(), lqueue()) -> boolean().
member(_X, {L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    false;
member(X, {L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    lists:member(X, F) orelse lists:member(X, R);
member(X, LQ) ->
    erlang:error(badarg, [X, LQ]).

-spec keymember(key(), lqueue()) -> boolean().
keymember(_K, {L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    false;
keymember(K, {L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    lists:keymember(K, 1, R) orelse lists:keymember(K, 1, F);
keymember(K, LQ) ->
    erlang:error(badarg, [K, LQ]).

%% Search starts at the front of the queue.
-spec find(fun((item()) -> boolean()), lqueue()) -> {value, item()} | false.
find(Pred, {L, M, R, F}) when
      ?IS_LQUEUE_EMPTY(L, M, R, F) and is_function(Pred, 1) ->
    false;
find(Pred, {L, M, R, F}) when
      ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) and is_function(Pred, 1) ->
    %% Erlang 21 has lists:search/2
    case lists_search(Pred, F) of
        {value, _X} = Res -> Res;
        false -> lists_search(Pred, lists:reverse(R))
    end;
find(Pred, LQ) ->
    erlang:error(badarg, [Pred, LQ]).

-spec keyfind(key(), lqueue()) -> {key(), value()} | false.
keyfind(_K, {L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    false;
keyfind(K, {L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    case lists:keyfind(K, 1, F) of
        {K, _V} = Res -> Res;
        false -> lists:keyfind(K, 1, lists:reverse(R))
    end;
keyfind(K, LQ) ->
    erlang:error(badarg, [K, LQ]).

-spec keyreplace(key(), value(), lqueue()) -> lqueue().
keyreplace(_K, _V, {L, M, R, F} = LQ) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    LQ;
keyreplace(K, V, {L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    {L, M, replace_all(K, V, R), replace_all(K, V, F)};
keyreplace(K, V, LQ) ->
    erlang:error(badarg, [K, V, LQ]).

-spec to_list(lqueue()) -> [item()].
to_list({L, M, R, F}) when ?IS_LQUEUE(L, M, R, F) ->
    F ++ lists:reverse(R, []);
to_list(LQ) ->
    erlang:error(badarg, [LQ]).

-spec from_list([item()], max_length()) -> lqueue().
from_list([], M) when is_integer(M) and (M > 0) ->
    {0, M, [], []};
from_list(L, M) when
      is_integer(M) and (M > 0) and is_list(L) and (M >= length(L)) ->
    {length(L), M, lists:reverse(L), []};
from_list(L, M) ->
    erlang:error(badarg, [L, M]).

-spec in(item(), lqueue()) -> lqueue().
in(X, {L, M, R, F}) when ?IS_LQUEUE_NOT_FULL(L, M, R, F) ->
    {L + 1, M, [X | R], F};
in(X, {L, M, R, [_H | T] = F}) when ?IS_LQUEUE_FULL(L, M, R, F) ->
    {M, M, [X | R], T};
in(X, {L, M, R, [] = F}) when ?IS_LQUEUE_FULL(L, M, R, F) ->
    in(X, {M, M, [], lists:reverse(R)});
in(X, LQ) ->
    erlang:error(badarg, [X, LQ]).

-spec out(lqueue()) -> {{value, item()}, lqueue()} | {empty, lqueue()}.
out({L, M, R, F} = LQ) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    {empty, LQ};
out({L, M, R, [H | T] = F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    {{value, H}, {L - 1, M, R, T}};
out({L, M, R, [] = F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    out({L, M, [], lists:reverse(R)});
out(LQ) ->
    erlang:error(badarg, [LQ]).

-spec get(lqueue()) -> {ok, item()} | {error, empty}.
get({L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    {error, empty};
get({L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    {ok, get(R, F)};
get(LQ) ->
    erlang:error(badarg, [LQ]).

-spec get_r(lqueue()) -> {ok, item()} | {error, empty}.
get_r({L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    {error, empty};
get_r({L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    {ok, get_r(R, F)};
get_r(LQ) ->
    erlang:error(badarg, [LQ]).

%% Internal functions.

lists_search(Pred, [H | T]) ->
    case Pred(H) of
        true  -> {value, H};
        false -> lists_search(Pred, T)
    end;
lists_search(_Pred, []) ->
    false.

replace_all(K, V, L) ->
    lists:map(fun({K1, _}) when K1 =:= K -> {K, V};
                 (Other) -> Other
              end, L).

get(_, [FH | _])  -> FH;
get([RH], [])     -> RH;
get([_ | RT], []) -> lists:last(RT).

get_r([RH | _], _)  -> RH;
get_r([], [FH])     -> FH;
get_r([], [_ | FT]) -> lists:last(FT).

