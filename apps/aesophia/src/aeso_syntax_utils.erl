%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Sophia syntax utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(aeso_syntax_utils).

-export([used_ids/1, used_types/1]).

%% Var set combinators
none()           -> [].
one(X)           -> [X].
union_map(F, Xs) -> lists:umerge(lists:map(F, Xs)).
minus(Xs, Ys)    -> Xs -- Ys.

%% Compute names used by a definition or expression.
used_ids(Es) when is_list(Es) ->
    union_map(fun used_ids/1, Es);
used_ids({bind, A, B}) ->
    minus(used_ids(B), used_ids(A));
%% Declarations
used_ids({contract, _, _, Decls})    -> used_ids(Decls);
used_ids({type_decl, _, _, _})       -> none();
used_ids({type_def, _, _, _, _})     -> none();
used_ids({fun_decl, _, _, _})        -> none();
used_ids({letval, _, _, _, E})       -> used_ids(E);
used_ids({letfun, _, _, Args, _, E}) -> used_ids({bind, Args, E});
used_ids({letrec, _, Decls})         -> used_ids(Decls);
%% Args
used_ids({arg, _, X, _}) -> used_ids(X);
%% Constants
used_ids({int, _, _})    -> none();
used_ids({bool, _, _})   -> none();
used_ids({hash, _, _})   -> none();
used_ids({unit, _})      -> none();
used_ids({string, _, _}) -> none();
used_ids({char, _, _})   -> none();
%% Expressions
used_ids({lam, _, Args, E})  -> used_ids({bind, Args, E});
used_ids({'if', _, A, B, C}) -> used_ids([A, B, C]);
used_ids({switch, _, E, Bs}) -> used_ids([E, Bs]);
used_ids({app, _, E, Es})    -> used_ids([E | Es]);
used_ids({proj, _, E, _})    -> used_ids(E);
used_ids({tuple, _, Es})     -> used_ids(Es);
used_ids({list, _, Es})      -> used_ids(Es);
used_ids({typed, _, E, _})   -> used_ids(E);
used_ids({record, _, Fs})    -> used_ids(Fs);
used_ids({record, _, E, Fs}) -> used_ids([E, Fs]);
used_ids({map, _, E, Fs})    -> used_ids([E, Fs]);
used_ids({map, _, KVs})      -> used_ids([ [K, V] || {K, V} <- KVs ]);
used_ids({map_get, _, M, K}) -> used_ids([M, K]);
used_ids({block, _, Ss})     -> used_ids_s(Ss);
used_ids({Op, _}) when is_atom(Op) -> none();
used_ids({id, _, X})   -> [X];
used_ids({qid, _, _})  -> none();
used_ids({con, _, _})  -> none();
used_ids({qcon, _, _}) -> none();
%% Switch branches
used_ids({'case', _, P, E}) -> used_ids({bind, P, E});
%% Fields
used_ids({field, _, LV, E})    -> used_ids([LV, E]);
used_ids({field, _, LV, X, E}) -> used_ids([LV, {bind, X, E}]);
used_ids({proj, _, _})         -> none();
used_ids({map_get, _, E})      -> used_ids(E).

%% Statements
used_ids_s([])       -> none();
used_ids_s([S | Ss]) ->
    used_ids([S, {bind, bound_ids(S), {block, [], Ss}}]).

bound_ids({letval, _, X, _, _})    -> one(X);
bound_ids({letfun, _, X, _, _, _}) -> one(X);
bound_ids({letrec, _, Decls})      -> union_map(fun bound_ids/1, Decls);
bound_ids(_)                       -> none().

