%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Type checker for Sophia.
%%% @end
%%%-------------------------------------------------------------------
-module(aeso_ast_infer_types).

-export([infer/1, infer_constant/1]).

%% Environment containing language primitives
-spec global_env() -> [{string(), aeso_syntax:type()}].
global_env() ->
    Ann     = [{origin, system}],
    Int     = {id, Ann, "int"},
    Bool    = {id, Ann, "bool"},
    String  = {id, Ann, "string"},
    Address = {id, Ann, "address"},
    State   = {id, Ann, "state"},
    Hash    = {id, Ann, "hash"},
    Oracle  = fun(Q, R) -> {app_t, Ann, {id, Ann, "oracle"}, [Q, R]} end,
    Query   = fun(Q, R) -> {app_t, Ann, {id, Ann, "oracle_query"}, [Q, R]} end,
    Unit    = {tuple_t, Ann, []},
    List    = fun(T) -> {app_t, Ann, {id, Ann, "list"}, [T]} end,
    Option  = fun(T) -> {app_t, Ann, {id, Ann, "option"}, [T]} end,
    Map     = fun(A, B) -> {app_t, Ann, {id, Ann, "map"}, [A, B]} end,
    Pair    = fun(A, B) -> {tuple_t, Ann, [A, B]} end,
    Fun     = fun(Ts, T) -> {type_sig, Ts, T} end,
    Fun1    = fun(S, T) -> Fun([S], T) end,
    TVar    = fun(X) -> {tvar, Ann, "'" ++ X} end,
    Signature = {id, Ann, "signature"},
    TTL       = Int,
    Fee       = Int,
    [A, B, Q, R, K, V] = lists:map(TVar, ["a", "b", "q", "r", "k", "v"]),
     %% Option constructors
    [{"None", Option(A)},
     {"Some", Fun1(A, Option(A))},
     %% Placeholder for inter-contract calls until we get proper type checking
     %% of contracts.
     {"raw_call", Fun([Address, String, Int, Int, A], B)},
     %% Spend transaction. Also not the proper version.
     {"raw_spend", Fun([Address, Int], Unit)},
     %% Environment variables
     %% {["Contract", "owner"],   Int},    %% Not in EVM?
     {["Contract", "address"],      Address},
     {["Contract", "balance"],      Int},
     {["Call",     "origin"],       Address},
     {["Call",     "caller"],       Address},
     {["Call",     "value"],        Int},
     {["Call",     "gas_price"],    Int},
     {["Chain",    "balance"],      Fun1(Address, Int)},
     {["Chain",    "block_hash"],   Fun1(Int, Int)},
     {["Chain",    "coinbase"],     Address},
     {["Chain",    "timestamp"],    Int},
     {["Chain",    "block_height"], Int},
     {["Chain",    "difficulty"],   Int},
     {["Chain",    "gas_limit"],    Int},
     %% State
     {"state", State},
     {"put",   Fun1(State, Unit)},
     %% Oracles
     {["Oracle", "register"],     Fun([Address, Signature, Fee, Fee, TTL], Oracle(Q, R))},
     {["Oracle", "query_fee"],    Fun([Oracle(Q, R)], Fee)},
     {["Oracle", "query"],        Fun([Oracle(Q, R), Q, Fee, TTL, TTL], Query(Q, R))},
     {["Oracle", "get_question"], Fun([Oracle(Q, R), Query(Q, R)], Q)},
     {["Oracle", "respond"],      Fun([Oracle(Q, R), Query(Q, R), Signature, R], Unit)},
     {["Oracle", "extend"],       Fun([Oracle(Q, R), Signature, Fee, TTL], Unit)},
     {["Oracle", "get_answer"],   Fun([Oracle(Q, R), Query(Q, R)], option_t(Ann, R))},
     %% Name service
     {["AENS", "resolve"],  Fun([String, String], option_t(Ann, A))},
     {["AENS", "preclaim"], Fun([Address, Hash, Signature], Unit)},
     {["AENS", "claim"],    Fun([Address, String, Int, Signature], Unit)},
     {["AENS", "transfer"], Fun([Address, Address, Hash, Signature], Unit)},
     {["AENS", "revoke"],   Fun([Address, Hash, Signature], Unit)},
     %% Maps
     {["Map", "from_list"], Fun1(List(Pair(K, V)), Map(K, V))},
     {["Map", "to_list"],   Fun1(Map(K, V), List(Pair(K, V)))},
     {["Map", "lookup"],    Fun([K, Map(K, V)], Option(V))},
     {["Map", "delete"],    Fun([K, Map(K, V)], Map(K, V))},
     {["Map", "member"],    Fun([K, Map(K, V)], Bool)},
     {["Map", "size"],      Fun1(Map(K, V), Int)}
    ].

option_t(As, T) -> {app_t, As, {id, As, "option"}, [T]}.
map_t(As, K, V) -> {app_t, As, {id, As, "map"}, [K, V]}.

-spec infer(aeso_syntax:ast()) -> aeso_syntax:ast().
infer([{contract, Attribs, ConName, Code}|Rest]) ->
    %% do type inference on each contract independently.
    [{contract, Attribs, ConName, infer_contract(Code)}|infer(Rest)];
infer([]) ->
    [].

infer_contract(Defs0) ->
    Defs = desugar(Defs0),
    create_record_types(Defs),
    C = unfold_record_types(infer_contract(global_env(), Defs)),
    destroy_record_types(),
    C.

infer_constant({letval, Attrs,_Pattern, Type, E}) ->
    create_record_types([]),
    ets:new(type_vars, [set, named_table, public]),
    {typed, _, _, PatType} =
        infer_expr(global_env(), {typed, Attrs, E, arg_type(Type)}),
    T = instantiate(PatType),
    ets:delete(type_vars),
    destroy_record_types(),
    T.

%% infer_contract takes a proplist mapping global names to types, and
%% a list of definitions.
infer_contract(Env, Defs) ->
    Kind = fun({type_def, _, _, _, _})  -> type;
              ({letfun, _, _, _, _, _}) -> function;
              ({fun_decl, _, _, _})     -> prototype
           end,
    Get = fun(K) -> [ Def || Def <- Defs, Kind(Def) == K ] end,
    %% TODO: handle type defs
    {Env1, TypeDefs} = check_typedefs(Env, Get(type)),
    ProtoSigs = [ check_fundecl(Env1, Decl) || Decl <- Get(prototype) ],
    Env2      = ProtoSigs ++ Env1,
    Functions = Get(function),
    FunMap    = maps:from_list([ {Fun, Def} || Def = {letfun, _, {id, _, Fun}, _, _, _} <- Functions ]),
    DepGraph  = maps:map(fun(_, Def) -> aeso_syntax_utils:used_ids(Def) end, FunMap),
    SCCs      = aeso_utils:scc(DepGraph),
    io:format("Dependency sorted functions:\n  ~p\n", [SCCs]),
    TypeDefs ++ check_sccs(Env2, FunMap, SCCs, []).

check_typedefs(Env, Defs) ->
    create_type_errors(),
    GetName  = fun({type_def, _, {id, _, Name}, _, _}) -> Name end,
    TypeMap  = maps:from_list([ {GetName(Def), Def} || Def <- Defs ]),
    DepGraph = maps:map(fun(_, Def) -> aeso_syntax_utils:used_types(Def) end, TypeMap),
    SCCs     = aeso_utils:scc(DepGraph),
    io:format("Dependency sorted types:\n  ~p\n", [SCCs]),
    Env1     = check_typedef_sccs(Env, TypeMap, SCCs),
    destroy_and_report_type_errors(),
    SCCNames = fun({cyclic, Xs}) -> Xs; ({acyclic, X}) -> [X] end,
    {Env1, [ Def || SCC <- SCCs, Name <- SCCNames(SCC),
                    Def <- [maps:get(Name, TypeMap, undefined)], Def /= undefined ]}.

check_typedef_sccs(Env, _TypeMap, []) -> Env;
check_typedef_sccs(Env, TypeMap, [{acyclic, Name} | SCCs]) ->
    case maps:get(Name, TypeMap, undefined) of
        undefined -> check_typedef_sccs(Env, TypeMap, SCCs);    %% Builtin type
        {type_def, Ann, D, Xs, Def} ->
            case Def of
                {alias_t, _}  -> check_typedef_sccs(Env, TypeMap, SCCs); %% TODO: check these
                {record_t, _} -> check_typedef_sccs(Env, TypeMap, SCCs); %%       and these
                {variant_t, Cons} ->
                    Target   = {app_t, Ann, D, Xs},
                    ConType  = fun([]) -> Target; (Args) -> {type_sig, Args, Target} end,
                    ConTypes = [ begin
                                    {constr_t, _, {con, _, Con}, Args} = ConDef,
                                    {Con, ConType(Args)}
                                 end || ConDef <- Cons ],
                    check_repeated_constructors([ {Con, ConType(Args)} || {constr_t, _, Con, Args} <- Cons ]),
                    [ check_constructor_overlap(Env, Con, Target) || {constr_t, _, Con, _} <- Cons ],
                    check_typedef_sccs(ConTypes ++ Env, TypeMap, SCCs)
            end
    end;
check_typedef_sccs(Env, TypeMap, [{cyclic, Names} | SCCs]) ->
    Id = fun(X) -> {type_def, _, D, _, _} = maps:get(X, TypeMap), D end,
    type_error({recursive_types_not_implemented, lists:map(Id, Names)}),
    check_typedef_sccs(Env, TypeMap, SCCs).

check_constructor_overlap(Env, Con = {con, _, Name}, NewType) ->
    case proplists:get_value(Name, Env) of
        undefined -> ok;
        Type ->
            OldType = case Type of {type_sig, _, T} -> T;
                                   _ -> Type end,
            OldCon  = {con, aeso_syntax:get_ann(OldType), Name},    %% TODO: we don't have the location of the old constructor here
            type_error({repeated_constructor, [{OldCon, OldType}, {Con, NewType}]})
    end.

check_repeated_constructors(Cons) ->
    Names      = [ Name || {{con, _, Name}, _} <- Cons ],
    Duplicated = lists:usort(Names -- lists:usort(Names)),
    Fail       = fun(Name) ->
                    type_error({repeated_constructor, [ CT || CT = {{con, _, C}, _} <- Cons, C == Name ]})
                 end,
    [ Fail(Dup) || Dup <- Duplicated ],
    ok.

check_sccs(_, _, [], Acc) -> lists:reverse(Acc);
check_sccs(Env, Funs, [{acyclic, X} | SCCs], Acc) ->
    case maps:get(X, Funs, undefined) of
        undefined ->    %% Previously defined function
            check_sccs(Env, Funs, SCCs, Acc);
        Def ->
            {TypeSig, Def1} = infer_nonrec(Env, Def),
            Env1 = [TypeSig | Env],
            check_sccs(Env1, Funs, SCCs, [Def1 | Acc])
    end;
check_sccs(Env, Funs, [{cyclic, Xs} | SCCs], Acc) ->
    Defs = [ maps:get(X, Funs) || X <- Xs ],
    {TypeSigs, {letrec, _, Defs1}} = infer_letrec(Env, {letrec, [], Defs}),
    Env1 = TypeSigs ++ Env,
    check_sccs(Env1, Funs, SCCs, Defs1 ++ Acc).

check_fundecl(_Env, {fun_decl, _Attrib, {id, _NameAttrib, Name}, {fun_t, _, Args, Ret}}) ->
    {Name, {type_sig, Args, Ret}};  %% TODO: actually check that the type makes sense!
check_fundecl(_, {fun_decl, _Attrib, {id, _, Name}, Type}) ->
    error({fundecl_must_have_funtype, Name, Type}).

infer_nonrec(Env, LetFun) ->
    ets:new(type_vars, [set, named_table, public]),
    create_type_errors(),
    create_field_constraints(),
    NewLetFun = infer_letfun(Env, LetFun),
    solve_field_constraints(),
    Result = {TypeSig, _} = instantiate(NewLetFun),
    destroy_and_report_type_errors(),
    destroy_field_constraints(),
    ets:delete(type_vars),
    print_typesig(TypeSig),
    Result.

typesig_to_fun_t({type_sig, Args, Res}) -> {fun_t, [], Args, Res}.

infer_letrec(Env, {letrec, Attrs, Defs}) ->
    ets:new(type_vars, [set, named_table, public]),
    create_type_errors(),
    create_field_constraints(),
    Env1 = [{Name, fresh_uvar(A)}
                 || {letfun, _, {id, A, Name}, _, _, _} <- Defs],
    ExtendEnv = Env1 ++ Env,
    Inferred =
        [ begin
            Res    = {{Name, TypeSig}, _} = infer_letfun(ExtendEnv, LF),
            Got    = proplists:get_value(Name, Env1),
            Expect = typesig_to_fun_t(TypeSig),
            unify(Got, Expect, {check_typesig, Name, Got, Expect}),
            solve_field_constraints(),
            io:format("Checked ~s : ~s\n", [Name, pp(dereference_deep(Got))]),
            Res
          end || LF <- Defs ],
    destroy_and_report_unsolved_field_constraints(),
    destroy_and_report_type_errors(),
    TypeSigs = instantiate([Sig || {Sig, _} <- Inferred]),
    NewDefs  = instantiate([D || {_, D} <- Inferred]),
    [print_typesig(S) || S <- TypeSigs],
    ets:delete(type_vars),
    {TypeSigs, {letrec, Attrs, NewDefs}}.

infer_letfun(Env, {letfun, Attrib, {id, NameAttrib, Name}, Args, What, Body}) ->
    ArgTypes  = [{ArgName, arg_type(T)} || {arg, _, {id, _, ArgName}, T} <- Args],
    ExpectedType = arg_type(What),
    NewBody={typed, _, _, ResultType} = check_expr(ArgTypes ++ Env, Body, ExpectedType),
    NewArgs = [{arg, A1, {id, A2, ArgName}, T}
               || {{ArgName, T}, {arg, A1, {id, A2, ArgName}, _}} <- lists:zip(ArgTypes, Args)],
    TypeSig = {type_sig, [T || {arg, _, _, T} <- NewArgs], ResultType},
    {{Name, TypeSig},
     {letfun, Attrib, {id, NameAttrib, Name}, NewArgs, ResultType, NewBody}}.

print_typesig({Name, TypeSig}) ->
    io:format("Inferred ~s : ~s\n", [Name, pp(TypeSig)]).

arg_type({id, Attrs, "_"}) ->
    fresh_uvar(Attrs);
arg_type({app_t, Attrs, Name, Args}) ->
    {app_t, Attrs, Name, [arg_type(T) || T <- Args]};
arg_type(T) ->
    T.

lookup_name(Env, As, Name) ->
    lookup_name(Env, As, Name, []).

lookup_name(Env, As, Name, Options) ->
    case proplists:get_value(Name, Env) of
        undefined ->
            Id = case Name of
                    [C | _] when is_integer(C) -> {id, As, Name};
                    [X | _] when is_list(X)    -> {qid, As, Name}
                 end,
            type_error({unbound_variable, Id}),
            fresh_uvar(As);
        {type_sig, _, _} = Type ->
            freshen_type(typesig_to_fun_t(Type));
        Type ->
            case proplists:get_value(freshen, Options, false) of
                true  -> freshen_type(Type);
                false -> Type
            end
    end.

check_expr(Env, Expr, Type) ->
    E = {typed, _, _, Type1} = infer_expr(Env, Expr),
    unify(Type1, Type, {check_expr, Expr, Type1, Type}),
    E.

infer_expr(_Env, Body={bool, As, _}) ->
    {typed, As, Body, {id, As, "bool"}};
infer_expr(_Env, Body={int, As, _}) ->
    {typed, As, Body, {id, As, "int"}};
infer_expr(_Env, Body={string, As, _}) ->
    {typed, As, Body, {id, As, "string"}};
infer_expr(_Env, Body={hash, As, _}) ->
    {typed, As, Body, {id, As, "address"}};
infer_expr(_Env, Body={id, As, "_"}) ->
    {typed, As, Body, fresh_uvar(As)};
infer_expr(Env, Body={id, As, Name}) ->
    Type = lookup_name(Env, As, Name),
    {typed, As, Body, Type};
infer_expr(Env, Body={qid, As, Name}) ->
    Type = lookup_name(Env, As, Name),
    {typed, As, Body, Type};
infer_expr(Env, Body={con, As, Name}) ->
    Type = lookup_name(Env, As, Name, [freshen]),
    {typed, As, Body, Type};
infer_expr(Env, {unit, As}) ->
    infer_expr(Env, {tuple, As, []});
infer_expr(Env, {tuple, As, Cpts}) ->
    NewCpts = [infer_expr(Env, C) || C <- Cpts],
    CptTypes = [T || {typed, _, _, T} <- NewCpts],
    {typed, As, {tuple, As, NewCpts}, {tuple_t, As, CptTypes}};
infer_expr(Env, {list, As, Elems}) ->
    ElemType = fresh_uvar(As),
    NewElems = [check_expr(Env, X, ElemType) || X <- Elems],
    {typed, As, {list, As, NewElems}, {app_t, As, {id, As, "list"}, [ElemType]}};
infer_expr(Env, {typed, As, Body, Type}) ->
    {typed, _, NewBody, NewType} = check_expr(Env, Body, Type),
    {typed, As, NewBody, NewType};
infer_expr(Env, {app, As, Fun, Args}) ->
    case aeso_syntax:get_ann(format, As) of
        infix ->
            infer_op(Env, As, Fun, Args, fun infer_infix/1);
        prefix ->
            infer_op(Env, As, Fun, Args, fun infer_prefix/1);
        _ ->
            NewFun={typed, _, _, FunType} = infer_expr(Env, Fun),
            NewArgs = [infer_expr(Env, A) || A <- Args],
            ArgTypes = [T || {typed, _, _, T} <- NewArgs],
            ResultType = fresh_uvar(As),
            unify(FunType, {fun_t, [], ArgTypes, ResultType}, {infer_app, Fun, Args, FunType, ArgTypes}),
            {typed, As, {app, As, NewFun, NewArgs}, dereference(ResultType)}
    end;
infer_expr(Env, {'if', Attrs, Cond, Then, Else}) ->
    NewCond = check_expr(Env, Cond, {id, Attrs, "bool"}),
    NewThen = {typed, _, _, ThenType} = infer_expr(Env, Then),
    NewElse = {typed, _, _, ElseType} = infer_expr(Env, Else),
    unify(ThenType, ElseType, {if_branches, Then, ThenType, Else, ElseType}),
    {typed, Attrs, {'if', Attrs, NewCond, NewThen, NewElse}, ThenType};
infer_expr(Env, {switch, Attrs, Expr, Cases}) ->
    NewExpr = {typed, _, _, ExprType} = infer_expr(Env, Expr),
    SwitchType = fresh_uvar(Attrs),
    NewCases = [infer_case(Env, As, Pattern, ExprType, Branch, SwitchType)
                || {'case', As, Pattern, Branch} <- Cases],
    {typed, Attrs, {switch, Attrs, NewExpr, NewCases}, SwitchType};
infer_expr(Env, {record, Attrs, Fields}) ->
    RecordType = fresh_uvar(Attrs),
    NewFields = [{field, A, FieldName, infer_expr(Env, Expr)}
                 || {field, A, FieldName, Expr} <- Fields],
    constrain([case LV of
                [{proj, _, FieldName}] -> {RecordType, FieldName, T, Fld}
               end || {Fld, {field, _, LV, {typed, _, _, T}}} <- lists:zip(Fields, NewFields)]),
    {typed, Attrs, {record, Attrs, NewFields}, RecordType};
infer_expr(Env, {record, Attrs, Record, Update}) ->
    NewRecord = {typed, _, _, RecordType} = infer_expr(Env, Record),
    NewUpdate = [ check_record_update(Env, RecordType, Fld) || Fld <- Update ],
    {typed, Attrs, {record, Attrs, NewRecord, NewUpdate}, RecordType};
infer_expr(Env, {proj, Attrs, Record, FieldName}) ->
    NewRecord = {typed, _, _, RecordType} = infer_expr(Env, Record),
    FieldType = fresh_uvar(Attrs),
    constrain({RecordType, FieldName, FieldType, {proj, Attrs, Record, FieldName}}),
    {typed, Attrs, {proj, Attrs, NewRecord, FieldName}, FieldType};
%% Maps
infer_expr(Env, {map_get, Attrs, Map, Key}) ->  %% map lookup
    KeyType = fresh_uvar(Attrs),
    ValType = fresh_uvar(Attrs),
    MapType = map_t(Attrs, KeyType, ValType),
    Map1 = check_expr(Env, Map, MapType),
    Key1 = check_expr(Env, Key, KeyType),
    {typed, Attrs, {map_get, Attrs, Map1, Key1}, ValType};
infer_expr(Env, {map, Attrs, KVs}) ->   %% map construction
    KeyType = fresh_uvar(Attrs),
    ValType = fresh_uvar(Attrs),
    KVs1 = [ {check_expr(Env, K, KeyType), check_expr(Env, V, ValType)}
             || {K, V} <- KVs ],
    {typed, Attrs, {map, Attrs, KVs1}, map_t(Attrs, KeyType, ValType)};
infer_expr(Env, {map, Attrs, Map, Updates}) -> %% map update
    KeyType  = fresh_uvar(Attrs),
    ValType  = fresh_uvar(Attrs),
    MapType  = map_t(Attrs, KeyType, ValType),
    Map1     = check_expr(Env, Map, MapType),
    Updates1 = [ check_map_update(Env, Upd, KeyType, ValType) || Upd <- Updates ],
    {typed, Attrs, {map, Attrs, Map1, Updates1}, MapType};
infer_expr(Env, {block, Attrs, Stmts}) ->
    BlockType = fresh_uvar(Attrs),
    NewStmts = infer_block(Env, Attrs, Stmts, BlockType),
    {typed, Attrs, {block, Attrs, NewStmts}, BlockType};
infer_expr(Env, {lam, Attrs, Args, Body}) ->
    ArgTypes = [fresh_uvar(As) || {arg, As, _, _} <- Args],
    ArgPatterns = [{typed, As, Pat, T} || {arg, As, Pat, T} <- Args],
    ResultType = fresh_uvar(Attrs),
    {'case', _, {typed, _, {tuple, _, NewArgPatterns}, _}, NewBody} =
        infer_case(Env, Attrs, {tuple, Attrs, ArgPatterns}, {tuple_t, Attrs, ArgTypes}, Body, ResultType),
    NewArgs = [{arg, As, NewPat, NewT} || {typed, As, NewPat, NewT} <- NewArgPatterns],
    {typed, Attrs, {lam, Attrs, NewArgs, NewBody}, {fun_t, Attrs, ArgTypes, ResultType}}.

check_map_update(Env, {field, Ann, [{map_get, Ann1, Key}], Val}, KeyType, ValType) ->
    Key1 = check_expr(Env, Key, KeyType),
    Val1 = check_expr(Env, Val, ValType),
    {field, Ann, [{map_get, Ann1, Key1}], Val1};
check_map_update(Env, {field, Ann, [{map_get, Ann1, Key}], Id, Val}, KeyType, ValType) ->
    FunType = {fun_t, Ann, [ValType], ValType},
    Key1    = check_expr(Env, Key, KeyType),
    Fun     = check_expr(Env, {lam, Ann1, [{arg, Ann1, Id, ValType}], Val}, FunType),
    {field_upd, Ann, [{map_get, Ann1, Key1}], Fun};
check_map_update(_, {field, Ann, Flds, _}, _, _) ->
    error({nested_map_updates_not_implemented, Ann, Flds}).

check_record_update(Env, RecordType, Fld) ->
    [field, Ann, LV = [{proj, Ann1, FieldName}] | Val] = tuple_to_list(Fld),
    FldType = fresh_uvar(Ann),
    Fld1 = case Val of
            [Expr] ->
                {field, Ann, LV, check_expr(Env, Expr, FldType)};
            [Id, Expr] ->
                Fun     = {lam, Ann1, [{arg, Ann1, Id, FldType}], Expr},
                FunType = {fun_t, Ann1, [FldType], FldType},
                {field_upd, Ann, LV, check_expr(Env, Fun, FunType)}
        end,
    constrain([{RecordType, FieldName, FldType, Fld}]),
    Fld1.

infer_op(Env, As, Op, Args, InferOp) ->
    TypedArgs = [infer_expr(Env, A) || A <- Args],
    ArgTypes = [T || {typed, _, _, T} <- TypedArgs],
    Inferred = {fun_t, _, OperandTypes, ResultType} = InferOp(Op),
    unify(ArgTypes, OperandTypes, {infer_app, Op, Args, Inferred, ArgTypes}),
    {typed, As, {app, As, Op, TypedArgs}, ResultType}.

infer_case(Env, Attrs, Pattern, ExprType, Branch, SwitchType) ->
    Vars = free_vars(Pattern),
    Names = [N || {id, _, N} <- Vars, N /= "_"],
    case Names -- lists:usort(Names) of
        [] -> ok;
        Nonlinear -> type_error({non_linear_pattern, Pattern, lists:usort(Nonlinear)})
    end,
    NewEnv = [{Name, fresh_uvar(Attr)} || {id, Attr, Name} <- Vars] ++ Env,
    NewPattern = {typed, _, _, PatType} = infer_expr(NewEnv, Pattern),
    NewBranch  = check_expr(NewEnv, Branch, SwitchType),
    unify(PatType, ExprType, {case_pat, Pattern, PatType, ExprType}),
    {'case', Attrs, NewPattern, NewBranch}.

%% NewStmts = infer_block(Env, Attrs, Stmts, BlockType)
infer_block(_Env, Attrs, [], BlockType) ->
    error({impossible, empty_block, Attrs, BlockType});
infer_block(Env, Attrs, [Def={letfun, _, _, _, _, _}|Rest], BlockType) ->
    NewDef = infer_letfun(Env, Def),
    [NewDef|infer_block(Env, Attrs, Rest, BlockType)];
infer_block(Env, Attrs, [Def={letrec, _, _}|Rest], BlockType) ->
    NewDef = infer_letrec(Env, Def),
    [NewDef|infer_block(Env, Attrs, Rest, BlockType)];
infer_block(Env, _, [{letval, Attrs, Pattern, Type, E}|Rest], BlockType) ->
    NewE = {typed, _, _, PatType} = infer_expr(Env, {typed, Attrs, E, arg_type(Type)}),
    {'case', _, NewPattern, {typed, _, {block, _, NewRest}, _}} =
        infer_case(Env, Attrs, Pattern, PatType, {block, Attrs, Rest}, BlockType),
    [{letval, Attrs, NewPattern, Type, NewE}|NewRest];
infer_block(Env, _, [E], BlockType) ->
    [check_expr(Env, E, BlockType)];
infer_block(Env, Attrs, [E|Rest], BlockType) ->
    [infer_expr(Env, E)|infer_block(Env, Attrs, Rest, BlockType)].

infer_infix({BoolOp, As})
  when BoolOp =:= '&&'; BoolOp =:= '||' ->
    Bool = {id, As, "bool"},
    {fun_t, As, [Bool,Bool], Bool};
infer_infix({IntOp, As})
  when IntOp == '+';    IntOp == '-';   IntOp == '*'; IntOp == '/';
       IntOp == 'band'; IntOp == 'bor'; IntOp == 'bxor' ->
    Int = {id, As, "int"},
    {fun_t, As, [Int, Int], Int};
infer_infix({RelOp, As})
  when RelOp == '=='; RelOp == '!=';
       RelOp == '<';  RelOp == '>';
       RelOp == '<='; RelOp == '=<'; RelOp == '>=' ->
    T = fresh_uvar(As),     %% allow any type here, check in ast_to_icode that we have comparison for it
    Bool = {id, As, "bool"},
    {fun_t, As, [T, T], Bool};
infer_infix({'::', As}) ->
    ElemType = fresh_uvar(As),
    ListType = {app_t, As, {id, As, "list"}, [ElemType]},
    {fun_t, As, [ElemType, ListType], ListType}.

infer_prefix({'!',As}) ->
    Bool = {id, As, "bool"},
    {fun_t, As, [Bool], Bool};
infer_prefix({IntOp,As})
  when IntOp =:= '-'; IntOp =:= 'bnot' ->
    Int = {id, As, "int"},
    {fun_t, As, [Int], Int}.

free_vars({int, _, _}) ->
    [];
free_vars({string, _, _}) ->
    [];
free_vars({bool, _, _}) ->
    [];
free_vars(Id={id, _, _}) ->
    [Id];
free_vars({con, _, _}) ->
    [];
free_vars({tuple, _, Cpts}) ->
    free_vars(Cpts);
free_vars({list, _, Elems}) ->
    free_vars(Elems);
free_vars({app, _, {'::', _}, Args}) ->
    free_vars(Args);
free_vars({app, _, {con, _, _}, Args}) ->
    free_vars(Args);
free_vars({record, _, Fields}) ->
    free_vars([E || {field, _, _, E} <- Fields]);
free_vars({typed, _, A, _}) ->
    free_vars(A);
free_vars(L) when is_list(L) ->
    [V || Elem <- L,
          V <- free_vars(Elem)].

%% Record types

create_record_types(Defs) ->
    %% A map from type names to definitions
    ets:new(record_types, [public, named_table, set]),
    %% A relation from field names to types
    ets:new(record_fields, [public, named_table, bag]),
    [begin
         ets:insert(record_types, {Name, Args, Fields}),
         [ets:insert(record_fields, {FieldName, FieldType, {app_t, Attrs, Id, Args}})
          || {field_t, _, {id, _, FieldName}, FieldType} <- Fields]
     end
     || {type_def, Attrs, Id={id, _, Name}, Args, {record_t, Fields}} <- Defs].

destroy_record_types() ->
    ets:delete(record_types),
    ets:delete(record_fields).

create_field_constraints() ->
    %% A relation from uvars to constraints
    ets:new(field_constraints, [public, named_table, bag]).

destroy_field_constraints() ->
    ets:delete(field_constraints).

constrain(FieldConstraints) ->
    ets:insert(field_constraints, FieldConstraints).

solve_field_constraints() ->
    solve_field_constraints(ets:tab2list(field_constraints)).

solve_field_constraints(Constraints) ->
    %% First look for record fields that appear in only one type definition
    IsAmbiguous = fun({RecordType, Field={id, _Attrs, FieldName}, FieldType, When}) ->
        case ets:lookup(record_fields, FieldName) of
            [] ->
                type_error({undefined_field, Field}),
                false;
            [{FieldName, FldType, RecType}] ->
                create_freshen_tvars(),
                FreshFldType = freshen(FldType),
                FreshRecType = freshen(RecType),
                destroy_freshen_tvars(),
                unify(FreshFldType, FieldType, {field_constraint, FreshFldType, FieldType, When}),
                unify(FreshRecType, RecordType, {record_constraint, FreshRecType, RecordType, When}),
                false;
            _ ->
                %% ambiguity--need cleverer strategy
                true
         end end,
    AmbiguousConstraints = lists:filter(IsAmbiguous, Constraints),
    solve_ambiguous_field_constraints(AmbiguousConstraints).

solve_ambiguous_field_constraints(Constraints) ->
    Unknown = solve_known_record_types(Constraints),
    if Unknown == [] -> ok;
       length(Unknown) < length(Constraints) ->
            %% progress! Keep trying.
            solve_ambiguous_field_constraints(Unknown);
       true ->
            case solve_unknown_record_types(Unknown) of
                true -> %% Progress!
                    solve_ambiguous_field_constraints(Unknown);
                _ -> ok %% No progress. Report errors later.
            end
    end.

solve_unknown_record_types(Unknown) ->
    UVars = lists:usort([UVar || {UVar = {uvar, _, _}, _, _, _} <- Unknown]),
    Solutions = [solve_for_uvar(UVar, [Field || {U, Field, _, _} <- Unknown,
                                                U == UVar])
                 || UVar <- UVars],
    case lists:member(true, Solutions) of
        true  -> true;
        false -> Solutions
    end.

solve_known_record_types(Constraints) ->
    DerefConstraints =
        [{dereference(RecordType), FieldName, FieldType, When}
         || {RecordType, FieldName, FieldType, When} <- Constraints],
    SolvedConstraints =
        [begin
             RecId = {id, Attrs, RecName} = record_type_name(RecType),
             case ets:lookup(record_types, RecName) of
                 [] ->
                     type_error({not_a_record_type, RecId, When}),
                     not_solved;
                 [{RecName, Formals, Fields}] ->
                     FieldTypes = [{Name, Type} || {field_t, _, {id, _, Name}, Type} <- Fields],
                     {id, _, FieldString} = FieldName,
                     case proplists:get_value(FieldString, FieldTypes) of
                         undefined ->
                             type_error({missing_field, FieldName, RecName}),
                             not_solved;
                         FldType ->
                             create_freshen_tvars(),
                             FreshFldType = freshen(FldType),
                             FreshRecType = freshen({app_t, Attrs, RecId, Formals}),
                             destroy_freshen_tvars(),
                             unify(FreshFldType, FieldType, {todo, {solve_field_constraint_field, When}}),
                             unify(FreshRecType, RecType, {todo, {solve_field_constraint_record, When}}),
                             {RecType, FieldName, FieldType, When}
                     end
             end
         end
         || {RecType, FieldName, FieldType, When} <- DerefConstraints,
            case RecType of
                {uvar, _, _} -> false;
                _          -> true
            end],
    DerefConstraints--SolvedConstraints.

destroy_and_report_unsolved_field_constraints() ->
    Unsolved = ets:tab2list(field_constraints),
    Unknown  = solve_known_record_types(Unsolved),
    if Unknown == [] -> ok;
       true ->
            case solve_unknown_record_types(Unknown) of
                true   -> ok;
                Errors -> [ type_error(Err) || Err <- Errors ]
            end
    end,
    destroy_field_constraints(),
    ok.

record_type_name({app_t, _Attrs, RecId={id, _, _}, _Args}) ->
    RecId;
record_type_name(RecId={id, _, _}) ->
    RecId.

solve_for_uvar(UVar = {uvar, Attrs, _}, Fields) ->
    %% Does this set of fields uniquely identify a record type?
    FieldNames = [ Name || {id, _, Name} <- Fields ],
    UniqueFields = lists:usort(FieldNames),
    Candidates = [RecName || {_, _, {app_t, _, {id, _, RecName}, _}} <- ets:lookup(record_fields, hd(FieldNames))],
    TypesAndFields = [case ets:lookup(record_types, RecName) of
                          [{RecName, _, RecFields}] ->
                              {RecName, [Field || {field_t, _, {id, _, Field}, _} <- RecFields]};
                          [] ->
                              error({no_definition_for, RecName, in, Candidates})
                      end
                      || RecName <- Candidates],
    Solutions = lists:sort([RecName || {RecName, RecFields} <- TypesAndFields,
                                       UniqueFields -- RecFields == []]),
    case Solutions of
        [] ->
            {no_records_with_all_fields, Fields};
        [RecName] ->
            [{RecName, Formals, _}] = ets:lookup(record_types, RecName),
            create_freshen_tvars(),
            FreshRecType = freshen({app_t, Attrs, {id, Attrs, RecName}, Formals}),
            destroy_freshen_tvars(),
            unify(UVar, FreshRecType, {solve_rec_type, UVar, Fields}),
            true;
        StillPossible ->
            {ambiguous_record, Fields, StillPossible}
    end.

%% During type inference, record types are represented by their
%% names. But, before we pass the typed program to the code generator,
%% we replace record types annotating expressions with their
%% definition. This enables the code generator to see the fields.
unfold_record_types({typed, Attr, E, Type}) ->
    {typed, Attr, unfold_record_types(E), unfold_record_types_in_type(Type)};
unfold_record_types({arg, Attr, Id, Type}) ->
    {arg, Attr, Id, unfold_record_types_in_type(Type)};
unfold_record_types({type_sig, Args, Ret}) ->
    {type_sig, unfold_record_types_in_type(Args), unfold_record_types_in_type(Ret)};
unfold_record_types({type_def, Ann, Name, Args, Def}) ->
    {type_def, Ann, Name, Args, unfold_record_types_in_type(Def)};
unfold_record_types({letfun, Ann, Name, Args, Type, Body}) ->
    {letfun, Ann, Name, unfold_record_types(Args), unfold_record_types_in_type(Type), unfold_record_types(Body)};
unfold_record_types(T) when is_tuple(T) ->
    list_to_tuple(unfold_record_types(tuple_to_list(T)));
unfold_record_types([H|T]) ->
    [unfold_record_types(H)|unfold_record_types(T)];
unfold_record_types(X) ->
    X.

unfold_record_types_in_type({app_t, Ann, Id = {id, _, RecName}, Args}) ->
    case ets:lookup(record_types, RecName) of
        [{RecName, Formals, Fields}] when length(Formals) == length(Args) ->
            {record_t,
             unfold_record_types_in_type(
               subst_tvars(lists:zip(Formals, Args), Fields))};
        _ ->
            %% Not a record type, or ill-formed record type.
            {app_t, Ann, Id, unfold_record_types_in_type(Args)}
    end;
unfold_record_types_in_type(Type={id, _, RecName}) ->
    %% Like the case above, but for record types without parameters.
    case ets:lookup(record_types, RecName) of
        [{RecName, [], Fields}] ->
            {record_t,
             unfold_record_types_in_type(Fields)};
        _ ->
            %% Not a record type, or ill-formed record type
            Type
    end;
unfold_record_types_in_type({field_t, Attr, Name, Type}) ->
    {field_t, Attr, Name, unfold_record_types_in_type(Type)};
unfold_record_types_in_type(T) when is_tuple(T) ->
    list_to_tuple(unfold_record_types_in_type(tuple_to_list(T)));
unfold_record_types_in_type([H|T]) ->
    [unfold_record_types_in_type(H)|unfold_record_types_in_type(T)];
unfold_record_types_in_type(X) ->
    X.


subst_tvars(Env, Type) ->
    subst_tvars1([{V, T} || {{tvar, _, V}, T} <- Env], Type).

subst_tvars1(Env, T={tvar, _, Name}) ->
    proplists:get_value(Name, Env, T);
subst_tvars1(Env, [H|T]) ->
    [subst_tvars1(Env, H)|subst_tvars1(Env, T)];
subst_tvars1(Env, Type) when is_tuple(Type) ->
    list_to_tuple(subst_tvars1(Env, tuple_to_list(Type)));
subst_tvars1(_Env, X) ->
    X.

%% Unification

unify({id, _, "_"}, _, _When) ->
  true;
unify(_, {id, _, "_"}, _When) ->
  true;
unify(T1, T2, When) ->
  unify1(dereference(T1), dereference(T2), When).

unify1({uvar, _, R}, {uvar, _, R}, _When) ->
    true;
unify1({uvar, A, R}, T, When) ->
    case occurs_check(R, T) of
        true ->
            cannot_unify({uvar, A, R}, T, When),
            false;
        false ->
            ets:insert(type_vars, {R, T}),
            true
    end;
unify1(T, {uvar, A, R}, When) ->
    unify1({uvar, A, R}, T, When);
unify1({tvar, _, X}, {tvar, _, X}, _When) -> true; %% Rigid type variables
unify1([A|B], [C|D], When) ->
    unify(A, C, When) andalso unify(B, D, When);
unify1(X, X, _When) ->
    true;
unify1({id, _, Name}, {id, _, Name}, _When) ->
    true;
unify1({fun_t, _, Args1, Result1}, {fun_t, _, Args2, Result2}, When) ->
    unify(Args1, Args2, When) andalso unify(Result1, Result2, When);
unify1({app_t, _, {id, _, F}, Args1}, {app_t, _, {id, _, F}, Args2}, When)
  when length(Args1) == length(Args2) ->
    unify(Args1, Args2, When);
unify1({tuple_t, _, As}, {tuple_t, _, Bs}, When)
  when length(As) == length(Bs) ->
    unify(As, Bs, When);
%% The grammar is a bit inconsistent about whether types without
%% arguments are represented as applications to an empty list of
%% parameters or not. We therefore allow them to unify.
unify1({app_t, _, T, []}, B, When) ->
    unify(T, B, When);
unify1(A, {app_t, _, T, []}, When) ->
    unify(A, T, When);
unify1(A, B, When) ->
    cannot_unify(A, B, When),
    false.

dereference(T = {uvar, _, R}) ->
    case ets:lookup(type_vars, R) of
        [] ->
            T;
        [{R, Type}] ->
            dereference(Type)
    end;
dereference(T) ->
    T.

dereference_deep(Type) ->
    case dereference(Type) of
        Tup when is_tuple(Tup) ->
            list_to_tuple(dereference_deep(tuple_to_list(Tup)));
        [H | T] -> [dereference_deep(H) | dereference_deep(T)];
        T -> T
    end.

occurs_check(R, T) ->
    occurs_check1(R, dereference(T)).

occurs_check1(R, {uvar, _, R1}) -> R == R1;
occurs_check1(_, {id, _, _}) -> false;
occurs_check1(_, {tvar, _, _}) -> false;
occurs_check1(R, {fun_t, _, Args, Res}) ->
    occurs_check(R, [Res | Args]);
occurs_check1(R, {app_t, _, T, Ts}) ->
    occurs_check(R, [T | Ts]);
occurs_check1(R, {tuple_t, _, Ts}) ->
    occurs_check(R, Ts);
occurs_check1(R, [H | T]) ->
    occurs_check(R, H) orelse occurs_check(R, T);
occurs_check1(_, []) -> false.

fresh_uvar(Attrs) ->
    {uvar, Attrs, make_ref()}.

create_freshen_tvars() ->
    ets:new(freshen_tvars, [set, public, named_table]).

destroy_freshen_tvars() ->
    ets:delete(freshen_tvars).

freshen_type(Type) ->
    create_freshen_tvars(),
    Type1 = freshen(Type),
    destroy_freshen_tvars(),
    Type1.

freshen({tvar, As, Name}) ->
    NewT = case ets:lookup(freshen_tvars, Name) of
               [] ->
                   fresh_uvar(As);
               [{Name, T}] ->
                   T
           end,
    ets:insert(freshen_tvars, {Name, NewT}),
    NewT;
freshen(T) when is_tuple(T) ->
    list_to_tuple(freshen(tuple_to_list(T)));
freshen([A|B]) ->
    [freshen(A)|freshen(B)];
freshen(X) ->
    X.

%% Dereferences all uvars and replaces the uninstantiated ones with a
%% succession of tvars.
instantiate(E) ->
    instantiate1(dereference(E)).

instantiate1({uvar, Attr, R}) ->
    Next = proplists:get_value(next, ets:lookup(type_vars, next), 1),
    TVar = {tvar, Attr, "'" ++ integer_to_list(Next)},
    ets:insert(type_vars, [{next, Next + 1}, {R, TVar}]),
    TVar;
instantiate1(T) when is_tuple(T) ->
    list_to_tuple(instantiate1(tuple_to_list(T)));
instantiate1([A|B]) ->
    [instantiate(A)|instantiate(B)];
instantiate1(X) ->
    X.

%% Save unification failures for error messages.

cannot_unify(A, B, When) ->
    type_error({cannot_unify, A, B, When}).

type_error(Err) ->
    ets:insert(type_errors, Err).

create_type_errors() ->
    ets:new(type_errors, [bag, named_table, public]).

destroy_and_report_type_errors() ->
    Errors = ets:tab2list(type_errors),
    [ io:format("~s", [pp_error(Err)]) || Err <- Errors ],
    ets:delete(type_errors),
    [ error(type_errors) || Errors /= [] ].

pp_error({cannot_unify, A, B, When}) ->
    io_lib:format("Cannot unify ~s\n"
                  "         and ~s\n"
                  "~s", [pp(instantiate(A)), pp(instantiate(B)), pp_when(When)]);
pp_error({unbound_variable, Id}) ->
    io_lib:format("Unbound variable ~s at ~s\n", [pp(Id), pp_loc(Id)]);
pp_error({not_a_record_type, Type, Why}) ->
    io_lib:format("~s\n~s\n", [pp_type("Not a record type: ", Type), pp_why_record(Why)]);
pp_error({non_linear_pattern, Pattern, Nonlinear}) ->
    Plural = [ $s || length(Nonlinear) > 1 ],
    io_lib:format("Repeated name~s ~s in pattern\n~s (at ~s)\n",
                  [Plural, string:join(Nonlinear, ", "), pp_expr("  ", Pattern), pp_loc(Pattern)]);
pp_error({ambiguous_record, Fields, Candidates}) ->
    S = [ "s" || length(Fields) > 1 ],
    io_lib:format("Ambiguous record type with field~s ~s (at ~s) could be one of\n  ~s\n",
                  [S, string:join([ pp(F) || F <- Fields ], ", "),
                   pp_loc(hd(Fields)),
                   string:join([ C || C <- Candidates ], ", ")]);
pp_error({missing_field, Field, Rec}) ->
    io_lib:format("Record type ~s does not have field ~s (at ~s)\n", [Rec, pp(Field), pp_loc(Field)]);
pp_error({no_records_with_all_fields, Fields}) ->
    S = [ "s" || length(Fields) > 1 ],
    io_lib:format("No record type with field~s ~s (at ~s)\n",
                  [S, string:join([ pp(F) || F <- Fields ], ", "),
                   pp_loc(hd(Fields))]);
pp_error({recursive_types_not_implemented, Types}) ->
    S = if length(Types) > 1 -> "s are mutually";
           true              -> " is" end,
    io_lib:format("The following type~s recursive, which is not yet supported:\n~s",
                    [S, [io_lib:format("  - ~s (at ~s)\n", [pp(T), pp_loc(T)]) || T <- Types]]);
pp_error({repeated_constructor, Cs}) ->
    io_lib:format("Variant types must have distinct constructor names\n~s",
                  [[ io_lib:format("~s  (at ~s)\n", [pp_typed("  - ", C, T), pp_loc(C)]) || {C, T} <- Cs ]]);
pp_error(Err) ->
    io_lib:format("Unknown error: ~p\n", [Err]).

pp_when({todo, What}) -> io_lib:format("[TODO] ~p\n", [What]);
pp_when({check_typesig, Name, Inferred, Given}) ->
    io_lib:format("when checking the definition of ~s\n"
                  "  inferred type: ~s\n"
                  "  given type:    ~s\n",
        [Name, pp(instantiate(Inferred)), pp(instantiate(Given))]);
pp_when({infer_app, Fun, Args, Inferred0, ArgTypes0}) ->
    Inferred = instantiate(Inferred0),
    ArgTypes = instantiate(ArgTypes0),
    io_lib:format("when checking the application at ~s of\n"
                  "~s\n"
                  "to arguments\n~s",
                  [pp_loc(Fun),
                   pp_typed("  ", Fun, Inferred),
                   [ [pp_typed("  ", Arg, ArgT), "\n"]
                      || {Arg, ArgT} <- lists:zip(Args, ArgTypes) ] ]);
pp_when({field_constraint, FieldType0, InferredType0, Fld}) ->
    FieldType    = instantiate(FieldType0),
    InferredType = instantiate(InferredType0),
    case Fld of
        {field, _Ann, LV, Id, E} ->
            io_lib:format("when checking the assignment of the field\n~s (at ~s)\nto the old value ~s and the new value\n~s\n",
                [pp_typed("  ", {lvalue, [], LV}, FieldType),
                 pp_loc(Fld),
                 pp(Id),
                 pp_typed("  ", E, InferredType)]);
        {field, _Ann, LV, E} ->
            io_lib:format("when checking the assignment of the field\n~s (at ~s)\nto the value\n~s\n",
                [pp_typed("  ", {lvalue, [], LV}, FieldType),
                 pp_loc(Fld),
                 pp_typed("  ", E, InferredType)]);
        {proj, _Ann, _Rec, _Fld} ->
            io_lib:format("when checking the record projection at ~s\n~s\nagainst the expected type\n~s\n",
                [pp_loc(Fld),
                 pp_typed("  ", Fld, FieldType),
                 pp_type("  ", InferredType)])
    end;
pp_when({record_constraint, RecType0, InferredType0, Fld}) ->
    RecType      = instantiate(RecType0),
    InferredType = instantiate(InferredType0),
    case Fld of
        {field, _Ann, _LV, _Id, _E} ->
            io_lib:format("when checking that the record type\n~s\n~s\n"
                          "matches the expected type\n~s\n",
                [pp_type("  ", RecType),
                 pp_why_record(Fld),
                 pp_type("  ", InferredType)]);
        {field, _Ann, _LV, _E} ->
            io_lib:format("when checking that the record type\n~s\n~s\n"
                          "matches the expected type\n~s\n",
                [pp_type("  ", RecType),
                 pp_why_record(Fld),
                 pp_type("  ", InferredType)]);
        {proj, _Ann, Rec, _FldName} ->
            io_lib:format("when checking that the expression\n~s (at ~s)\nhas type\n~s\n~s\n",
                [pp_typed("  ", Rec, InferredType),
                 pp_loc(Rec),
                 pp_type("  ", RecType),
                 pp_why_record(Fld)])
    end;
pp_when({if_branches, Then, ThenType0, Else, ElseType0}) ->
    {ThenType, ElseType} = instantiate({ThenType0, ElseType0}),
    Branches = [ {Then, ThenType} | [ {B, ElseType} || B <- if_branches(Else) ] ],
    io_lib:format("when comparing the types of the if-branches\n"
                  "~s\n", [ [ io_lib:format("~s (at ~s)\n", [pp_typed("  - ", B, BType), pp_loc(B)])
                                || {B, BType} <- Branches ] ]);
pp_when({case_pat, Pat, PatType0, ExprType0}) ->
    {PatType, ExprType} = instantiate({PatType0, ExprType0}),
    io_lib:format("when checking the type of the pattern at ~s\n~s\n"
                  "against the expected type\n~s\n",
                  [pp_loc(Pat), pp_typed("  ", Pat, PatType),
                   pp_type("  ", ExprType)]);
pp_when({check_expr, Expr, Inferred0, Expected0}) ->
    {Inferred, Expected} = instantiate({Inferred0, Expected0}),
    io_lib:format("when checking the type of the expression at ~s\n~s\n"
                  "against the expected type\n~s\n",
                  [pp_loc(Expr), pp_typed("  ", Expr, Inferred),
                   pp_type("  ", Expected)]);
pp_when(unknown) -> "".

pp_why_record(Fld = {field, _Ann, LV, _Id, _E}) ->
    io_lib:format("arising from an assignment of the field ~s (at ~s)",
        [pp_expr("", {lvalue, [], LV}),
         pp_loc(Fld)]);
pp_why_record(Fld = {field, _Ann, LV, _E}) ->
    io_lib:format("arising from an assignment of the field ~s (at ~s)",
        [pp_expr("", {lvalue, [], LV}),
         pp_loc(Fld)]);
pp_why_record({proj, _Ann, Rec, FldName}) ->
    io_lib:format("arising from the projection of the field ~s (at ~s)",
        [pp(FldName),
         pp_loc(Rec)]).


if_branches(If = {'if', Ann, _, Then, Else}) ->
    case proplists:get_value(format, Ann) of
        elif -> [Then | if_branches(Else)];
        _    -> [If]
    end;
if_branches(E) -> [E].

pp_typed(Label, E, T = {type_sig, _, _}) -> pp_typed(Label, E, typesig_to_fun_t(T));
pp_typed(Label, {typed, _, Expr, _}, Type) ->
    pp_typed(Label, Expr, Type);
pp_typed(Label, Expr, Type) ->
    pp_expr(Label, {typed, [], Expr, Type}).

pp_expr(Label, Expr) ->
    prettypr:format(prettypr:beside(prettypr:text(Label), aeso_pretty:expr(Expr, [show_generated]))).

pp_type(Label, Type) ->
    prettypr:format(prettypr:beside(prettypr:text(Label), aeso_pretty:type(Type, [show_generated]))).

line_number(T)   -> aeso_syntax:get_ann(line, T, 0).
column_number(T) -> aeso_syntax:get_ann(col, T, 0).

loc(T) ->
    {line_number(T), column_number(T)}.

pp_loc(T) ->
    {Line, Col} = loc(T),
    io_lib:format("line ~p, column ~p", [Line, Col]).

pp({type_sig, As, B}) ->
    ["(", pp(As), ") => ", pp(B)];
pp([]) ->
    "";
pp([T]) ->
    pp(T);
pp([T|Ts]) ->
    [pp(T), ", "|pp(Ts)];
pp({id, _, Name}) ->
    Name;
pp({qid, _, Name}) ->
    string:join(Name, ".");
pp({con, _, Name}) ->
    Name;
pp({uvar, _, Ref}) ->
    ["?" | lists:sublist(base58:binary_to_base58(term_to_binary(Ref)), 43, 3)];
pp({tvar, _, Name}) ->
    Name;
pp({tuple_t, _, Cpts}) ->
    ["(", pp(Cpts), ")"];
pp({app_t, _, T, []}) ->
    pp(T);
pp({app_t, _, {id, _, Name}, Args}) ->
    [Name, "(", pp(Args), ")"];
pp({fun_t, _, As, B}) ->
    ["(", pp(As), ") => ", pp(B)].

%% -- Pre-type checking desugaring -------------------------------------------

%% Desugars nested record/map updates as follows:
%%  { x.y = v1, x.z @ z = f(z) } becomes { x @ __x = __x { y = v1, z @ z = f(z) } }
%%  { [k1].x = v1, [k2].y = v2 } becomes { [k1] @ __x = __x { x = v1 }, [k2] @ __x = __x { y = v2 } }
%% There's no comparison of k1 and k2 to group the updates if they are equal.
desugar({record, Ann, Rec, Updates}) ->
    {record, Ann, Rec, desugar_updates(Updates)};
desugar({map, Ann, Map, Updates}) ->
    {map, Ann, Map, desugar_updates(Updates)};
desugar([H|T]) ->
  [desugar(H) | desugar(T)];
desugar(T) when is_tuple(T) ->
  list_to_tuple(desugar(tuple_to_list(T)));
desugar(X) -> X.

desugar_updates([]) -> [];
desugar_updates([Upd | Updates]) ->
    {Key, MakeField, Rest} = update_key(Upd),
    {More, Updates1}       = updates_key(Key, Updates),
    %% Check conflicts
    case length([ [] || [] <- [Rest | More] ]) of
        N when N > 1 -> error({conflicting_updates_for_field, Upd, Key});
        _ -> ok
    end,
    [MakeField(lists:append([Rest | More])) | desugar_updates(Updates1)].

%% TODO: refactor representation to make this not horrible
update_key(Fld = {field, _, [Elim], _}) ->
    {elim_key(Elim), fun(_) -> Fld end, []};
update_key(Fld = {field, _, [Elim], _, _}) ->
    {elim_key(Elim), fun(_) -> Fld end, []};
update_key({field, Ann, [P = {proj, _, {id, _, Name}} | Rest], Value}) ->
    {Name, fun(Flds) -> {field, Ann, [P], {id, [], "__x"},
                            desugar(map_or_record(Ann, {id, [], "__x"}, Flds))}
           end, [{field, Ann, Rest, Value}]};
update_key({field, Ann, [P = {proj, _, {id, _, Name}} | Rest], Id, Value}) ->
    {Name, fun(Flds) -> {field, Ann, [P], {id, [], "__x"},
                            desugar(map_or_record(Ann, {id, [], "__x"}, Flds))}
           end, [{field, Ann, Rest, Id, Value}]};
update_key({field, Ann, [K = {map_get, _, _} | Rest], Value}) ->
    {map_key, fun(Flds) -> {field, Ann, [K], {id, [], "__x"},
                            desugar(map_or_record(Ann, {id, [], "__x"}, Flds))}
              end, [{field, Ann, Rest, Value}]};
update_key({field, Ann, [K = {map_get, _, _} | Rest], Id, Value}) ->
    {map_key, fun(Flds) -> {field, Ann, [K], {id, [], "__x"},
                            desugar(map_or_record(Ann, {id, [], "__x"}, Flds))}
              end, [{field, Ann, Rest, Id, Value}]}.

map_or_record(Ann, Val, Flds = [Fld | _]) ->
    Kind = case element(3, Fld) of
             [{proj, _, _}    | _] -> record;
             [{map_get, _, _} | _] -> map
           end,
    {Kind, Ann, Val, Flds}.

elim_key({proj, _, {id, _, Name}}) -> Name;
elim_key({map_get, _, _})          -> map_key.  %% no grouping on map keys (yet)

updates_key(map_key, Updates) -> {[], Updates};
updates_key(Name, Updates) ->
    Xs = [ {Upd, Name1 == Name, Rest}
           || Upd <- Updates,
              {Name1, _, Rest} <- [update_key(Upd)] ],
    Updates1 = [ Upd  || {Upd, false, _} <- Xs ],
    More     = [ Rest || {_, true, Rest} <- Xs ],
    {More, Updates1}.
