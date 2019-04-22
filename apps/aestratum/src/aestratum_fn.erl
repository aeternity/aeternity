-module(aestratum_fn).

%% FN/OK/VAL/ERR/LST handlers
-export([id/1,
         ok/0, ok/1,
         err/0, err/1,
         const/1, fail/1,
         is_ok/1,
         ok_err/1, ok_err/2,
         ok_val_err/1, ok_val_err/2,
         ok_or/2, ok_or_else/2, lazy_ok/3,
         tag_val_err/3,
         hd_or/2, hd_or_else/2, lazy_hd/3,
         key_val/2, val/2, val/3]).

id(X) -> X.

is_ok({ok, _}) -> true;
is_ok(_) -> false.

ok() -> fun (X) -> ok(X) end.
const(V) -> fun (_) -> V end.
err() -> error.

ok(X) -> {ok, X}.
err(X) -> {error, X}.
fail(X) -> error(X).

ok_err(ok) -> ok;
ok_err(Other) -> error(Other).

ok_err(ok, _) -> ok;
ok_err(Other, ErrTag) -> error({ErrTag, Other}).

ok_val_err({ok, Val}) -> Val;
ok_val_err(Other) -> error(Other).

ok_val_err({ok, Val}, _ErrTag) -> Val;
ok_val_err(Other, ErrTag) -> error({ErrTag, Other}).

ok_or({ok, _} = OK, _) -> OK;
ok_or(_, Default)      -> Default.

ok_or_else({ok, _} = OK, _) -> OK;
ok_or_else(_, DefaultFn)    -> DefaultFn().

lazy_ok({ok, X}, T, _)  -> T(X);
lazy_ok(Other, _, F) -> F(Other).

tag_val_err({Tag, Val}, Tag, _ErrTag) -> Val;
tag_val_err(Other, _Tag, ErrTag) -> error({ErrTag, Other}).

hd_or([X | _], _)  -> X;
hd_or([], Default) -> Default.

hd_or_else([X | _], _)  -> X;
hd_or_else([], DefaultFn) -> DefaultFn().

lazy_hd([X | _], T, _)  -> T(X);
lazy_hd([], _, F) -> F().

key_val({K, Default}, #{} = M) when is_atom(K) ->
    {K, val(K, M, Default)};
key_val(K, #{} = M) when is_atom(K) ->
    {K, val(K, M)}.

val(K, M) ->
    maps:get(aestratum_conv:bin(K), M).
val(K, M, Default) ->
    maps:get(aestratum_conv:bin(K), M, Default).
