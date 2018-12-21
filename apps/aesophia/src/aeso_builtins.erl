%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Compiler builtin functions for Aeterinty Sophia language.
%%% @end
%%% Created : 20 Dec 2018
%%%
%%%-------------------------------------------------------------------

-module(aeso_builtins).

-export([ builtin_function/1
        , check_event_type/1
        , used_builtins/1 ]).

-import(aeso_ast_to_icode, [prim_call/5]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aeso_icode.hrl").

used_builtins(#funcall{ function = #var_ref{ name = {builtin, Builtin} }, args = Args }) ->
    lists:umerge(dep_closure([Builtin]), used_builtins(Args));
used_builtins([H|T]) ->
  lists:umerge(used_builtins(H), used_builtins(T));
used_builtins(T) when is_tuple(T) ->
  used_builtins(tuple_to_list(T));
used_builtins(M) when is_map(M) ->
  used_builtins(maps:to_list(M));
used_builtins(_) -> [].

builtin_deps(Builtin) ->
    lists:usort(builtin_deps1(Builtin)).

builtin_deps1({map_lookup_default, Type}) -> [{map_lookup, Type}];
builtin_deps1({map_get, Type})            -> [{map_lookup, Type}];
builtin_deps1(map_member)                 -> [{map_lookup, word}];
builtin_deps1({map_upd, Type})            -> [{map_get, Type}, map_put];
builtin_deps1({map_upd_default, Type})    -> [{map_lookup_default, Type}, map_put];
builtin_deps1(map_from_list)              -> [map_put];
builtin_deps1(str_equal)                  -> [str_equal_p];
builtin_deps1(string_concat)              -> [string_concat_inner1, string_concat_inner2];
builtin_deps1(int_to_str)                 -> [int_to_str_, int_digits];
builtin_deps1(addr_to_str)                -> [base58_int, string_concat];
builtin_deps1(base58_int)                 -> [base58_int_encode, base58_int_pad, string_reverse, string_concat];
builtin_deps1(base58_int_encode)          -> [base58_int_encode_, base58_tab];
builtin_deps1(string_reverse)             -> [string_reverse_];
builtin_deps1(_)                          -> [].

dep_closure(Deps) ->
    case lists:umerge(lists:map(fun builtin_deps/1, Deps)) of
        []    -> Deps;
        Deps1 -> lists:umerge(Deps, dep_closure(Deps1))
    end.

%% Helper functions/macros
v(X) when is_atom(X) -> v(atom_to_list(X));
v(X) when is_list(X) -> #var_ref{name = X}.

option_none()  -> {tuple, [{integer, 0}]}.
option_some(X) -> {tuple, [{integer, 1}, X]}.

-define(call(Fun, Args), #funcall{ function = #var_ref{ name = {builtin, Fun} }, args = Args }).
-define(I(X), {integer, X}).
-define(V(X), v(X)).
-define(A(Op), aeb_opcodes:mnemonic(Op)).
-define(LET(Var, Expr, Body), {switch, Expr, [{v(Var), Body}]}).
-define(DEREF(Var, Ptr, Body), {switch, v(Ptr), [{{tuple, [v(Var)]}, Body}]}).
-define(NXT(Ptr), op('+', Ptr, 32)).
-define(NEG(A), op('/', A, {unop, '-', {integer, 1}})).
-define(BYTE(Ix, Word), op('byte', Ix, Word)).

-define(EQ(A, B), op('==', A, B)).
-define(LT(A, B), op('<', A, B)).
-define(GT(A, B), op('>', A, B)).
-define(ADD(A, B), op('+', A, B)).
-define(SUB(A, B), op('-', A, B)).
-define(MUL(A, B), op('*', A, B)).
-define(DIV(A, B), op('div', A, B)).
-define(MOD(A, B), op('mod', A, B)).
-define(EXP(A, B), op('^', A, B)).
-define(AND(A, B), op('&&', A, B)).

-define(BSL(X, B), ?MUL(X, ?EXP(2, ?MUL(B, 8)))).
-define(BSR(X, B), ?DIV(X, ?EXP(2, ?MUL(B, 8)))).

op(Op, A, B) -> {binop, Op, operand(A), operand(B)}.

operand(A) when is_atom(A) -> v(A);
operand(I) when is_integer(I) -> {integer, I};
operand(T) -> T.

str_to_icode(String) when is_list(String) ->
    str_to_icode(list_to_binary(String));
str_to_icode(BinStr) ->
    Cpts = [size(BinStr) | aeso_memory:binary_to_words(BinStr)],
    #tuple{ cpts = [ #integer{value = X} || X <- Cpts ] }.

check_event_type(Icode) ->
    case maps:get(event_type, Icode) of
        {variant_t, Cons} ->
            check_event_type(Cons, Icode);
        _ ->
            error({event_should_be_variant_type})
    end.

check_event_type(Evts, Icode) ->
    [ check_event_type(Name, T, Icode)
      || {constr_t, _, {con, _, Name}, Types} <- Evts, T <- Types ].

check_event_type(EvtName, Type, Icode) ->
    io:format("~p: ~p??\n", [EvtName, Type]),
    io:format("=> ~p\n", [aeso_ast_to_icode:ast_typerep(Type, Icode)]),
    VMType =
        try
           aeso_ast_to_icode:ast_typerep(Type, Icode)
        catch _:_ ->
            error({EvtName, could_not_resolve_type, Type})
        end,
    case aeso_syntax:get_ann(indexed, Type, false) of
        true when VMType == word    -> ok;
        false when VMType == string -> ok;
        true  -> error({EvtName, indexed_field_should_be_word, is, VMType});
        false -> error({EvtName, payload_should_be_string, is, VMType})
    end.

%% Event primitive (dependent on Event type)
%%
%% We need to switch on the event and prepare the correct #event for icode_to_asm
%% NOTE: we assume all errors are already checked!
builtin_function(Builtin = {event, EventT}) ->
    A         = fun(X) -> aeb_opcodes:mnemonic(X) end,
    VIx       = fun(Ix) -> v(lists:concat(["v", Ix])) end,
    ArgPats   = fun(Ts) -> [ VIx(Ix) || Ix <- lists:seq(0, length(Ts) - 1) ] end,
    IsIndexed = fun(T) -> aeso_syntax:get_ann(indexed, T, false) end,
    Payload = %% Should put data ptr, length on stack.
        fun([]) ->  {inline_asm, [A(?PUSH1), 0, A(?PUSH1), 0]};
           ([V]) -> {seq, [V, {inline_asm, [A(?DUP1), A(?MLOAD),                  %% length, ptr
                                            A(?SWAP1), A(?PUSH1), 32, A(?ADD)]}]} %% ptr+32, length
        end,
    Clause =
        fun(_Tag, {con, _, Con}, Types) ->
            Indexed   = [ Var || {Var, Type} <- lists:zip(ArgPats(Types), Types),
                                 IsIndexed(Type) ],
            EvtIndex  = {unop, 'sha3', str_to_icode(Con)},
            {event, lists:reverse(Indexed) ++ [EvtIndex], Payload(ArgPats(Types) -- Indexed)}
        end,
    Pat = fun(Tag, Types) -> {tuple, [{integer, Tag} | ArgPats(Types)]} end,

    {variant_t, Cons} = EventT,
    Tags              = lists:seq(0, length(Cons) - 1),

    {{builtin, Builtin}, [private],
        [{"e", event}],
        {switch, v(e),
            [{Pat(Tag, Types), Clause(Tag, Con, Types)}
             || {Tag, {constr_t, _, Con, Types}} <- lists:zip(Tags, Cons) ]},
        {tuple, []}};

%% Abort primitive.
builtin_function(abort) ->
    A = fun(X) -> aeb_opcodes:mnemonic(X) end,
    {{builtin, abort}, [private],
     [{"s", string}],
     {inline_asm, [A(?PUSH1),0,  %% Push a dummy 0 for the first arg
                   A(?REVERT)]}, %% Stack: 0,Ptr
     {tuple,[]}};

%% Map primitives
builtin_function(Builtin = {map_lookup, Type}) ->
    Ret = aeso_icode:option_typerep(Type),
    {{builtin, Builtin}, [private],
        [{"m", word}, {"k", word}],
            prim_call(?PRIM_CALL_MAP_GET, #integer{value = 0},
                      [#var_ref{name = "m"}, #var_ref{name = "k"}],
                      [word, word], Ret),
     Ret};

builtin_function(Builtin = map_put) ->
    %% We don't need the types for put.
    {{builtin, Builtin}, [private],
        [{"m", word}, {"k", word}, {"v", word}],
        prim_call(?PRIM_CALL_MAP_PUT, #integer{value = 0},
                  [v(m), v(k), v(v)],
                  [word, word, word], word),
     word};

builtin_function(Builtin = map_delete) ->
    {{builtin, Builtin}, [private],
        [{"m", word}, {"k", word}],
        prim_call(?PRIM_CALL_MAP_DELETE, #integer{value = 0},
                  [v(m), v(k)],
                  [word, word], word),
     word};

builtin_function(Builtin = map_size) ->
    Name = {builtin, Builtin},
    {Name, [private], [{"m", word}],
        prim_call(?PRIM_CALL_MAP_SIZE, #integer{value = 0},
                  [v(m)], [word], word),
        word};

%% Map builtins
builtin_function(Builtin = {map_get, Type}) ->
    %% function map_get(m, k) =
    %%   switch(map_lookup(m, k))
    %%     Some(v) => v
    {{builtin, Builtin}, [private],
        [{"m", word}, {"k", word}],
            {switch, ?call({map_lookup, Type}, [v(m), v(k)]),
                [{option_some(v(v)), v(v)}]},
     Type};

builtin_function(Builtin = {map_lookup_default, Type}) ->
    %% function map_lookup_default(m, k, default) =
    %%   switch(map_lookup(m, k))
    %%     None    => default
    %%     Some(v) => v
    {{builtin, Builtin}, [private],
        [{"m", word}, {"k", word}, {"default", Type}],
            {switch, ?call({map_lookup, Type}, [v(m), v(k)]),
                [{option_none(),     v(default)},
                 {option_some(v(v)), v(v)}]},
     Type};

builtin_function(Builtin = map_member) ->
    %% function map_member(m, k) : bool =
    %%   switch(Map.lookup(m, k))
    %%     None => false
    %%     _    => true
    {{builtin, Builtin}, [private],
        [{"m", word}, {"k", word}],
            {switch, ?call({map_lookup, word}, [v(m), v(k)]),
                [{option_none(), {integer, 0}},
                 {{var_ref, "_"}, {integer, 1}}]},
     word};

builtin_function(Builtin = {map_upd, Type}) ->
    %% function map_upd(map, key, fun) =
    %%   map_put(map, key, fun(map_get(map, key)))
    {{builtin, Builtin}, [private],
     [{"map", word}, {"key", word}, {"valfun", word}],
     ?call(map_put,
        [v(map), v(key),
         #funcall{ function = v(valfun),
                   args     = [?call({map_get, Type}, [v(map), v(key)])] }]),
     word};

builtin_function(Builtin = {map_upd_default, Type}) ->
    %% function map_upd(map, key, val, fun) =
    %%   map_put(map, key, fun(map_lookup_default(map, key, val)))
    {{builtin, Builtin}, [private],
     [{"map", word}, {"key", word}, {"val", word}, {"valfun", word}],
     ?call(map_put,
        [v(map), v(key),
         #funcall{ function = v(valfun),
                   args     = [?call({map_lookup_default, Type}, [v(map), v(key), v(val)])] }]),
     word};

builtin_function(Builtin = map_from_list) ->
    %% function map_from_list(xs, acc) =
    %%   switch(xs)
    %%     [] => acc
    %%     (k, v) :: xs => map_from_list(xs, acc { [k] = v })
    {{builtin, Builtin}, [private],
     [{"xs", {list, {tuple, [word, word]}}}, {"acc", word}],
     {switch, v(xs),
        [{{list, []}, v(acc)},
         {{binop, '::', {tuple, [v(k), v(v)]}, v(ys)},
          ?call(map_from_list,
            [v(ys), ?call(map_put, [v(acc), v(k), v(v)])])}]},
     word};

%% list_concat
%%
%% Concatenates two lists.
builtin_function(list_concat) ->
    {{builtin, list_concat}, [private],
     [{"l1", {list, word}}, {"l2", {list, word}}],
     {switch, v(l1),
        [{{list, []}, v(l2)},
         {{binop, '::', v(hd), v(tl)},
          {binop, '::', v(hd), ?call(list_concat, [v(tl), v(l2)])}}
        ]
     },
     word};

builtin_function(string_length) ->
    %% function length(str) =
    %%   switch(str)
    %%      {n} -> n  // (ab)use the representation
    {{builtin, string_length}, [private],
        [{"s", string}],
        ?DEREF(n, s, ?V(n)),
     word};

%% str_concat - concatenate two strings
%%
%% Unless the second string is the empty string, a new string is created at the
%% top of the Heap and the address to it is returned. The tricky bit is when
%% the words from the second string has to be shifted to fit next to the first
%% string.
builtin_function(string_concat) ->
    {{builtin, string_concat}, [private],
     [{"s1", string}, {"s2", string}],
     ?DEREF(n1, s1,
     ?DEREF(n2, s2,
        {ifte, ?EQ(n2, 0),
            ?V(s1), %% Second string is empty return first string
            ?LET(ret, {inline_asm, [?A(?MSIZE)]},
                {seq, [?ADD(n1, n2), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]}, %% Store total len
                       ?call(string_concat_inner1, [?V(n1), ?NXT(s1), ?V(n2), ?NXT(s2)]),
                       {inline_asm, [?A(?POP)]}, %% Discard fun ret val
                       ?V(ret)                   %% Put the actual return value
                      ]})}
     )),
     word};

builtin_function(string_concat_inner1) ->
    %% Copy all whole words from the first string, and set up for word fusion
    %% Special case when the length of the first string is divisible by 32.
    {{builtin, string_concat_inner1}, [private],
     [{"n1", word}, {"p1", pointer}, {"n2", word}, {"p2", pointer}],
     ?DEREF(w1, p1,
        {ifte, ?GT(n1, 32),
            {seq, [?V(w1), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                   ?call(string_concat_inner1, [?SUB(n1, 32), ?NXT(p1), ?V(n2), ?V(p2)])]},
            {ifte, ?EQ(n1, 0),
                ?call(string_concat_inner2, [?I(32), ?I(0), ?V(n2), ?V(p2)]),
                ?call(string_concat_inner2, [?SUB(32, n1), ?V(w1), ?V(n2), ?V(p2)])}
        }),
     word};

builtin_function(string_concat_inner2) ->
    %% Current "work in progess" word 'x', has 'o' bytes that are "free" - fill them from
    %% words of the second string.
    {{builtin, string_concat_inner2}, [private],
     [{"o", word}, {"x", word}, {"n2", word}, {"p2", pointer}],
     {ifte, ?LT(n2, 1),
        {seq, [?V(x), {inline_asm, [?A(?MSIZE), ?A(?MSTORE), ?A(?MSIZE)]}]}, %% Use MSIZE as dummy return value
        ?DEREF(w2, p2,
            {ifte, ?GT(n2, o),
                {seq, [?ADD(x, ?BSR(w2, ?SUB(32, o))),
                       {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                       ?call(string_concat_inner2,
                             [?V(o), ?BSL(w2, o), ?SUB(n2, 32), ?NXT(p2)])
                      ]},
                {seq, [?ADD(x, ?BSR(w2, ?SUB(32, o))),
                       {inline_asm, [?A(?MSIZE), ?A(?MSTORE), ?A(?MSIZE)]}]} %% Use MSIZE as dummy return value
            })
     },
     word};

builtin_function(str_equal_p) ->
    %% function str_equal_p(n, p1, p2) =
    %%   if(n =< 0) true
    %%   else
    %%      let w1 = *p1
    %%      let w2 = *p2
    %%      w1 == w2 && str_equal_p(n - 32, p1 + 32, p2 + 32)
    {{builtin, str_equal_p}, [private],
        [{"n", word}, {"p1", pointer}, {"p2", pointer}],
        {ifte, ?LT(n, 1),
            ?I(1),
            ?DEREF(w1, p1,
            ?DEREF(w2, p2,
                ?AND(?EQ(w1, w2),
                     ?call(str_equal_p, [?SUB(n, 32), ?NXT(p1), ?NXT(p2)]))))},
     word};

builtin_function(str_equal) ->
    %% function str_equal(s1, s2) =
    %%   let n1 = length(s1)
    %%   let n2 = length(s2)
    %%   n1 == n2 && str_equal_p(n1, s1 + 32, s2 + 32)
    {{builtin, str_equal}, [private],
        [{"s1", string}, {"s2", string}],
        ?DEREF(n1, s1,
        ?DEREF(n2, s2,
            ?AND(?EQ(n1, n2), ?call(str_equal_p, [?V(n1), ?NXT(s1), ?NXT(s2)]))
        )),
        word};

builtin_function(int_to_str) ->
    {{builtin, int_to_str}, [private],
        [{"i0", word}],
        {switch, {ifte, ?LT(i0, 0),
                    {tuple, [?I(2), ?NEG(i0), ?BSL(45, 31)]},
                    {tuple, [?I(1), ?V(i0), ?I(0)]}},
        [{{tuple, [v(off), v(i), v(x)]},
        ?LET(ret, {inline_asm, [?A(?MSIZE)]},
        ?LET(n,   ?call(int_digits, [?DIV(i, 10), ?I(0)]),
        ?LET(fac, ?EXP(10, n),
            {seq, [?ADD(n, off), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]}, %% Store str len
                   ?call(int_to_str_,
                        [?MOD(i, fac), ?ADD(x, ?BSL(?ADD(48, ?DIV(i, fac)), ?SUB(32, off))), ?DIV(fac, 10), ?V(off)]),
                   {inline_asm, [?A(?POP)]}, ?V(ret)]}
        )))}]},
        word};

builtin_function(int_to_str_) ->
    {{builtin, int_to_str_}, [private],
        [{"x", word}, {"y", word}, {"fac", word}, {"n", word}],
        {ifte, ?EQ(fac, 0),
            {seq, [?V(y), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]}, ?V(n)]},
            {ifte, ?EQ(?MOD(n, 32), 0),
                 %% We've filled a word, write it and start on new word
                 {seq, [?V(y), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                        ?call(int_to_str_,
                              [?MOD(x, fac), ?BSL(?ADD(48, ?DIV(x, fac)), 31),
                               ?DIV(fac, 10), ?I(1)])]},
                 ?call(int_to_str_,
                       [?MOD(x, fac), ?ADD(y, ?BSL(?ADD(48, ?DIV(x, fac)), ?SUB(31, n))),
                        ?DIV(fac, 10), ?ADD(n, 1)])}
        },
        word};

builtin_function(int_digits) ->
    {{builtin, int_digits}, [private],
        [{"x", word}, {"n", word}],
        {ifte, ?EQ(x, 0), ?V(n), ?call(int_digits, [?DIV(x, 10), ?ADD(n, 1)])},
        word};

builtin_function(base58_tab) ->
    Fst32 = 22252025330403739761829862310514590177935513752035045390683118730099851483225,
    Lst26 = 40880219588527126470443504235291962205031881694701834176631306799289575931904,
    {{builtin, base58_tab}, [private],
        [{"ix", word}],
        {ifte, ?LT(ix, 32),
            ?BYTE(ix, Fst32),
            ?BYTE(?SUB(ix, 32), Lst26)
        }, word};

builtin_function(base58_int) ->
    {{builtin, base58_int}, [private],
        [{"w", word}],
        ?LET(str0, ?call(base58_int_encode, [?V(w)]),
        ?LET(str1, ?call(base58_int_pad, [?V(w), ?I(0), ?I(0)]),
        ?LET(str2, ?call(string_concat, [?V(str0), ?V(str1)]),
            ?call(string_reverse, [?V(str2)])
        ))),
        word};

builtin_function(string_reverse) ->
    {{builtin, string_reverse}, [private],
        [{"s", string}],
        ?DEREF(n, s,
        ?LET(ret, {inline_asm, [?A(?MSIZE)]},
            {seq, [?V(n), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                   ?call(string_reverse_, [?NXT(s), ?I(0), ?I(31), ?SUB(?V(n), 1)]),
                   {inline_asm, [?A(?POP)]}, ?V(ret)]})),
        word};

builtin_function(string_reverse_) ->
    {{builtin, string_reverse_}, [private],
        [{"p", pointer}, {"x", word}, {"i1", word}, {"i2", word}],
        {ifte, ?LT(i2, 0),
            {seq, [?V(x), {inline_asm, [?A(?MSIZE), ?A(?MSTORE), ?A(?MSIZE)]}]},
            ?LET(p1, ?ADD(p, ?MUL(?DIV(i2, 32), 32)),
            ?DEREF(w, p1,
            ?LET(b, ?BYTE(?MOD(i2, 32), w),
            {ifte, ?LT(i1, 0),
                {seq, [?V(x), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                       ?call(string_reverse_,
                             [?V(p), ?BSL(b, 31), ?I(30), ?SUB(i2, 1)])]},
                ?call(string_reverse_,
                      [?V(p), ?ADD(x, ?BSL(b, i1)), ?SUB(i1, 1), ?SUB(i2, 1)])})))},
        word};

builtin_function(base58_int_pad) ->
    {{builtin, base58_int_pad}, [private],
        [{"w", word}, {"i", word}, {"x", word}],
        {ifte, ?GT(?BYTE(i, w), 0),
            {seq, [?V(i), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                   ?V(x), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                   {inline_asm, [?A(?PUSH1), 64, ?A(?MSIZE), ?A(?SUB)]}]},
            ?call(base58_int_pad, [?V(w), ?ADD(i, 1),
                                   ?ADD(x, ?BSL(49, ?SUB(31, i)))])},
        word};

builtin_function(base58_int_encode) ->
    {{builtin, base58_int_encode}, [private],
        [{"w", word}],
        ?LET(ret, {inline_asm, [?A(?MSIZE), ?A(?PUSH1), 0, ?A(?MSIZE), ?A(?MSTORE)]}, %% write placeholder
        ?LET(n, ?call(base58_int_encode_, [?V(w), ?I(0), ?I(0), ?I(31)]),
            {seq, [?V(ret), {inline_asm, [?A(?DUP2), ?A(?SWAP1), ?A(?MSTORE)]},
                   ?V(ret)]})),
        word};

builtin_function(base58_int_encode_) ->
    {{builtin, base58_int_encode_}, [private],
        [{"w", word}, {"x", word}, {"n", word}, {"i", word}],
        {ifte, ?EQ(w, 0),
            {seq, [?V(x), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]}, ?V(n)]},
            {ifte, ?LT(i, 0),
                {seq, [?V(x), {inline_asm, [?A(?MSIZE), ?A(?MSTORE)]},
                       ?call(base58_int_encode_,
                             [?DIV(w, 58), ?BSL(?call(base58_tab, [?MOD(w, 58)]), 31),
                              ?ADD(n, 1), ?I(30)])]},
                ?call(base58_int_encode_,
                    [?DIV(w, 58), ?ADD(x, ?BSL(?call(base58_tab, [?MOD(w, 58)]), i)),
                     ?ADD(n, 1), ?SUB(i, 1)])}},
        word};


builtin_function(addr_to_str) ->
    {{builtin, addr_to_str}, [private],
        [{"a", word}],
        ?call(base58_int, [?V(a)]),
        word}.

