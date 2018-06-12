%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Type checker for Sophia.
%%% @end
%%%-------------------------------------------------------------------
-module(aeso_ast_infer_types).

-export([infer/1]).

%% Environment containing language primitives
-spec global_env() -> [{string(), aeso_syntax:type()}].
global_env() ->
    Ann     = [{origin, system}],
    Int     = {id, Ann, "int"},
    String  = {id, Ann, "string"},
    Address = {id, Ann, "address"},
    State   = {id, Ann, "state"},
    Oracle  = fun(Q, R) -> {app_t, Ann, {id, Ann, "oracle"}, [Q, R]} end,
    Query   = fun(Q, R) -> {app_t, Ann, {id, Ann, "oracle_query"}, [Q, R]} end,
    Unit    = {tuple_t, Ann, []},
    List    = fun(T) -> {app_t, Ann, {id, Ann, "list"}, [T]} end,
    Map     = fun(A, B) -> {app_t, Ann, {id, Ann, "map"}, [A, B]} end,
    Pair    = fun(A, B) -> {tuple_t, Ann, [A, B]} end,
    Fun     = fun(Ts, T) -> {type_sig, Ts, T} end,
    Fun1    = fun(S, T) -> Fun([S], T) end,
    TVar    = fun(X) -> {tvar, Ann, "'" ++ X} end,
    Signature = Int,
    TTL       = Int,
    Fee       = Int,
    [A, B, Q, R] = lists:map(TVar, ["a", "b", "q", "r"]),
     %% Placeholder for inter-contract calls until we get proper type checking
     %% of contracts.
    [{"raw_call", Fun([Address, String, Int, Int, A], B)},
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
     {["Oracle", "query_fee"],    Fun([Oracle(Q, R)], Int)},
     {["Oracle", "query"],        Fun([Oracle(Q, R), Q, Fee, TTL, TTL], Query(Q, R))},
     {["Oracle", "get_question"], Fun([Oracle(Q, R), Query(Q, R)], Q)},
     {["Oracle", "respond"],      Fun([Oracle(Q, R), Query(Q, R), Signature, R], Unit)},
     {["Oracle", "extend"],       Fun([Oracle(Q, R), Signature, Fee, TTL], Unit)},
     {["Oracle", "get_answer"],   Fun([Oracle(Q, R), Query(Q, R)], option_t(Ann, R))},
     %% Maps
     {["Map", "from_list"], Fun1(List(Pair(A, B)), Map(A, B))}
    ].

option_t(As, T) -> {app_t, As, {id, As, "option"}, [T]}.
map_t(As, K, V) -> {app_t, As, {id, As, "map"}, [K, V]}.

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

%% infer_contract takes a proplist mapping global names to types, and
%% a list of definitions.
infer_contract(Env, Defs) ->
    Kind = fun({type_def, _, _, _, _})  -> type;
              ({letfun, _, _, _, _, _}) -> function;
              ({fun_decl, _, _, _})     -> prototype
           end,
    Get = fun(K) -> [ Def || Def <- Defs, Kind(Def) == K ] end,
    %% TODO: handle type defs
    TypeDefs   = Get(type),
    ProtoSigs = [ check_fundecl(Env, Decl) || Decl <- Get(prototype) ],
    Env1      = ProtoSigs ++ Env,
    {_, {letrec, _, Funs}} = infer_letrec(Env1, {letrec, [], Get(function)}),
    TypeDefs ++ Funs.

check_fundecl(_Env, {fun_decl, _Attrib, {id, _NameAttrib, Name}, {fun_t, _, Args, Ret}}) ->
    {Name, {type_sig, Args, Ret}};  %% TODO: actually check that the type makes sense!
check_fundecl(_, {fun_decl, _Attrib, {id, _, Name}, Type}) ->
    error({fundecl_must_have_funtype, Name, Type}).

%% infer_nonrec(Env, LetFun) ->
%%     ets:new(type_vars, [set, named_table, public]),
%%     create_unification_errors(),
%%     create_field_constraints(),
%%     NewLetFun = infer_letfun(Env, LetFun),
%%     solve_field_constraints(),
%%     Result = {TypeSig, _} = instantiate(NewLetFun),
%%     destroy_and_report_unification_errors(),
%%     destroy_field_constraints(),
%%     ets:delete(type_vars),
%%     print_typesig(TypeSig),
%%     Result.

typesig_to_fun_t({type_sig, Args, Res}) -> {fun_t, [], Args, Res}.

infer_letrec(Env, {letrec, Attrs, Defs}) ->
    ets:new(type_vars, [set, named_table, public]),
    create_unification_errors(),
    create_field_constraints(),
    Env1 = [{Name, fresh_uvar(A)}
		 || {letfun, _, {id, A, Name}, _, _, _} <- Defs],
    ExtendEnv = Env1 ++ Env,
    Inferred = [infer_letfun(ExtendEnv, LF) || LF <- Defs],
    [ unify(proplists:get_value(Name, Env1), typesig_to_fun_t(TypeSig))
        || {{Name, TypeSig}, _} <- Inferred ],
    solve_field_constraints(),
    TypeSigs = instantiate([Sig || {Sig, _} <- Inferred]),
    NewDefs = instantiate([D || {_, D} <- Inferred]),
    destroy_and_report_unification_errors(),
    [print_typesig(S) || S <- TypeSigs],
    destroy_field_constraints(),
    ets:delete(type_vars),
    {TypeSigs, {letrec, Attrs, NewDefs}}.

infer_letfun(Env, {letfun, Attrib, {id, NameAttrib, Name}, Args, What, Body}) ->
    ArgTypes  = [{ArgName, arg_type(T)} || {arg, _, {id, _, ArgName}, T} <- Args],
    NewBody={typed, _, _, ResultType} = infer_expr(ArgTypes ++ Env, Body),
    unify(ResultType, arg_type(What)),
    NewArgs = [{arg, A1, {id, A2, ArgName}, T}
	       || {{ArgName, T}, {arg, A1, {id, A2, ArgName}, _}} <- lists:zip(ArgTypes, Args)],
    TypeSig = {type_sig, [T || {arg, _, _, T} <- NewArgs], ResultType},
    {{Name, TypeSig},
     {letfun, Attrib, {id, NameAttrib, Name}, NewArgs, ResultType, NewBody}}.

print_typesig({Name, TypeSig}) ->
    io:format("Inferred ~p : ~s\n", [Name, pp(TypeSig)]).

arg_type({id, Attrs, "_"}) ->
    fresh_uvar(Attrs);
arg_type({app_t, Attrs, Name, Args}) ->
    {app_t, Attrs, Name, [arg_type(T) || T <- Args]};
arg_type(T) ->
    T.

lookup_name(Env, As, Name) ->
    case proplists:get_value(Name, Env) of
	undefined ->
	    io:format("Unbound variable: ~p\n", [Name]),
	    error({unbound_variable, Name});
	{type_sig, ArgTypes, ReturnType} ->
	    ets:new(freshen_tvars, [set, public, named_table]),
	    Type = freshen({fun_t, As, ArgTypes, ReturnType}),
	    ets:delete(freshen_tvars),
            Type;
	Type ->
            Type
    end.

check_expr(Env, Expr, Type) ->
    E = {typed, _, _, Type1} = infer_expr(Env, Expr),
    unify(Type1, Type),
    E.

infer_expr(_Env, Body={bool, As, _}) ->
    {typed, As, Body, {id, As, "bool"}};
infer_expr(_Env, Body={int, As, _}) ->
    {typed, As, Body, {id, As, "int"}};
infer_expr(_Env, Body={string, As, _}) ->
    {typed, As, Body, {id, As, "string"}};
infer_expr(_Env, Body={id, As, "_"}) ->
    {typed, As, Body, fresh_uvar(As)};
infer_expr(Env, Body={id, As, Name}) ->
    Type = lookup_name(Env, As, Name),
    {typed, As, Body, Type};
infer_expr(Env, Body={qid, As, Name}) ->
    Type = lookup_name(Env, As, Name),
    {typed, As, Body, Type};
infer_expr(Env, {unit, As}) ->
    infer_expr(Env, {tuple, As, []});
infer_expr(Env, {tuple, As, Cpts}) ->
    NewCpts = [infer_expr(Env, C) || C <- Cpts],
    CptTypes = [T || {typed, _, _, T} <- NewCpts],
    {typed, As, {tuple, As, NewCpts}, {tuple_t, As, CptTypes}};
infer_expr(Env, {list, As, Elems}) ->
    NewElems = [infer_expr(Env, X) || X <- Elems],
    ElemType = fresh_uvar(As),
    [unify(ElemType, T) || {typed, _, _, T} <- NewElems],
    {typed, As, {list, As, NewElems}, {app_t, As, {id, As, "list"}, [ElemType]}};
%% TODO: not hardwired!
infer_expr(_Env, E = {con, As, "None"}) ->
    ElemType = fresh_uvar(As),
    {typed, As, E, option_t(As, ElemType)};
infer_expr(_Env, E = {con, As, "Some"}) ->
    ElemType = fresh_uvar(As),
    {typed, As, E, {fun_t, As, [ElemType], option_t(As, ElemType)}};
infer_expr(Env, {typed, As, Body, Type}) ->
    AnnotType = case Type of
		    {id, Attrs, "_"} ->
			fresh_uvar(Attrs);
		    _ ->
			Type
		end,
    {typed, _, NewBody, NewType} = infer_expr(Env, Body),
    unify(NewType, AnnotType),
    {typed, As, NewBody, AnnotType};
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
            unify(FunType, {fun_t, [], ArgTypes, ResultType}),
            {typed, As, {app, As, NewFun, NewArgs}, dereference(ResultType)}
    end;
infer_expr(Env, {'if', Attrs, Cond, Then, Else}) ->
    NewCond={typed, _, _, CondType} = infer_expr(Env, Cond),
    unify(CondType, {id, Attrs, "bool"}),
    NewThen = {typed, _, _, ThenType} = infer_expr(Env, Then),
    NewElse = check_expr(Env, Else, ThenType),
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
                [{proj, _, FieldName}] -> {RecordType, FieldName, T}
	       end || {field, _, LV, {typed, _, _, T}} <- NewFields]),
    {typed, Attrs, {record, Attrs, NewFields}, RecordType};
infer_expr(Env, {record, Attrs, Record, Update}) ->
    NewRecord = {typed, _, _, RecordType} = infer_expr(Env, Record),
    NewUpdate = [ check_record_update(Env, RecordType, Fld) || Fld <- Update ],
    {typed, Attrs, {record, Attrs, NewRecord, NewUpdate}, RecordType};
infer_expr(Env, {proj, Attrs, Record, FieldName}) ->
    NewRecord = {typed, _, _, RecordType} = infer_expr(Env, Record),
    FieldType = fresh_uvar(Attrs),
    constrain({RecordType, FieldName, FieldType}),
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
    constrain([{RecordType, FieldName, FldType}]),
    Fld1.

infer_op(Env, As, Op, Args, InferOp) ->
    TypedArgs = [infer_expr(Env, A) || A <- Args],
    ArgTypes = [T || {typed, _, _, T} <- TypedArgs],
    {fun_t, _, OperandTypes, ResultType} = InferOp(Op),
    unify(ArgTypes, OperandTypes),
    {typed, As, {app, As, Op, TypedArgs}, ResultType}.

infer_case(Env, Attrs, Pattern, ExprType, Branch, SwitchType) ->
    Line = line_number(Attrs),
    Vars = free_vars(Pattern),
    Names = [N || {id, _, N} <- Vars, N /= "_"],
    case Names -- lists:usort(Names) of
	[] -> ok;
	Nonlinear ->
	    Plural = case lists:usort(Nonlinear) of
			 [_] -> "";
			 _   -> "s"
		     end,
	    io:format("Repeated name~s in pattern on line ~p: ~s\n",
		      [Plural, Line, [[N, " "] || N <- lists:usort(Nonlinear)]]),
	    error({non_linear_pattern, Pattern})
    end,
    NewEnv = [{Name, fresh_uvar(Attr)} || {id, Attr, Name} <- Vars] ++ Env,
    NewPattern = {typed, _, _, PatType} = infer_expr(NewEnv, Pattern),
    NewBranch = {typed, _, _, BranchType} = infer_expr(NewEnv, Branch),
    unify(PatType, ExprType),
    unify(BranchType, SwitchType),
    {'case', Attrs, NewPattern, NewBranch}.

%% NewStmts = infer_block(Env, Attrs, Stmts, BlockType)
infer_block(_Env, Attrs, [], BlockType) ->
    %% DANG! A block with no value. Interpret it as unit.
    unify({tuple_t, Attrs, []}, BlockType),
    [];
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
    NewE = {typed, _, _, Type} = infer_expr(Env, E),
    unify(Type, BlockType),
    [NewE];
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
    Int = {id, As, "int"},
    Bool = {id, As, "bool"},
    {fun_t, As, [Int, Int], Bool};
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
	  || {field_t, _, _, {id, _, FieldName}, FieldType} <- Fields]
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
    [case ets:lookup(record_fields, FieldName) of
	 [] ->
	     Line = line_number(Attrs),
	     io:format("Undefined record field ~s on line ~p\n", [FieldName, Line]),
	     error({undefined_field, FieldName});
	 [{FieldName, FldType, RecType}] ->
	     ets:new(freshen_tvars, [set, public, named_table]),
	     FreshFldType = freshen(FldType),
	     FreshRecType = freshen(RecType),
	     ets:delete(freshen_tvars),
	     unify(FreshFldType, FieldType),
	     unify(FreshRecType, RecordType),
	     true;
	 _ ->
	     %% ambiguity--need cleverer strategy
	     false
     end
     || {RecordType, {id, Attrs, FieldName}, FieldType} <- Constraints],
    solve_ambiguous_field_constraints(Constraints).

solve_ambiguous_field_constraints(Constraints) ->
    Unknown = solve_known_record_types(Constraints),
    if Unknown == [] -> ok;
       length(Unknown) < length(Constraints) ->
	    %% progress! Keep trying.
	    solve_ambiguous_field_constraints(Unknown);
       true ->
	    %% If there is a SMALLEST record type that solves all the
	    %% field constraints for a variable, choose that
	    %% type. This should enable, in particular, record
	    %% expressions that list all the fields of a record, even
	    %% if there is a record type with a superset of the
	    %% fields.
	    UVars = lists:usort([UVar || {UVar, _, _} <- Unknown]),
	    Solutions = [solve_for_uvar(UVar, [Field || {U, {id, _, Field}, _} <- Unknown,
						        U == UVar])
			 || UVar <- UVars],
	    case lists:member(true, Solutions) of
		true ->
		    %% Progress!
		    solve_ambiguous_field_constraints(Unknown);
		false ->
		    [io:format("Record\n  with fields ~s\n  on line ~p\n  could be any of ~s\n",
			       [[[F, " "] || F <- Fields],
				Line,
				[[T, " "] || T <- Types]
			       ])
		     || {ambiguous_record, Line, Fields, Types} <- Solutions],
		    [io:format("No record type has all the fields ~s(on line ~p)\n",
			       [[[F, " "] || F <- Fs], Line])
		     || {no_records_with_all_fields, Line, Fs} <- Solutions],
		    error({ambiguous_constraints, Solutions})
	    end
    end.

solve_known_record_types(Constraints) ->
    DerefConstraints =
	[{dereference(RecordType), FieldName, FieldType}
	 || {RecordType, FieldName, FieldType} <- Constraints],
    SolvedConstraints =
	[begin
	     RecId = {id, Attrs, RecName} = record_type_name(RecType),
	     case ets:lookup(record_types, RecName) of
		 [] ->
		     io:format("Undefined record type: ~s\n", [RecName]),
		     error({undefined_record_type, RecName});
		 [{RecName, Formals, Fields}] ->
		     FieldTypes = [{Name, Type} || {field_t, _, _, {id, _, Name}, Type} <- Fields],
		     {id, _, FieldString} = FieldName,
                     Line = line_number(FieldName),
		     case proplists:get_value(FieldString, FieldTypes) of
			 undefined ->
			     io:format("Field ~s of record ~s does not exist (line ~p)\n",
				       [FieldString, RecName, Line]),
			     error({missing_field, FieldString, RecName});
			 FldType ->
			     ets:new(freshen_tvars, [set, public, named_table]),
			     FreshFldType = freshen(FldType),
			     FreshRecType = freshen({app_t, Attrs, RecId, Formals}),
			     ets:delete(freshen_tvars),
			     unify(FreshFldType, FieldType),
			     unify(FreshRecType, RecType),
			     {RecType, FieldName, FieldType}
		     end
	     end
	 end
	 || {RecType, FieldName, FieldType} <- DerefConstraints,
	    case RecType of
		{uvar, _, _} -> false;
		_          -> true
	    end],
    DerefConstraints--SolvedConstraints.

record_type_name({app_t, _Attrs, RecId={id, _, _}, _Args}) ->
    RecId;
record_type_name(RecId={id, _, _}) ->
    RecId.

solve_for_uvar(UVar, Fields) ->
    %% Does this set of fields uniquely identify a record type?
    UniqueFields = lists:usort(Fields),
    Candidates = [RecName || {_, _, {app_t, _, {id, _, RecName}, _}} <- ets:lookup(record_fields, hd(Fields))],
    TypesAndFields = [case ets:lookup(record_types, RecName) of
			  [{RecName, _, RecFields}] ->
			      {RecName, [Field || {field_t, _, _, {id, _, Field}, _} <- RecFields]};
			  [] ->
			      error({no_definition_for, RecName, in, Candidates})
		      end
		      || RecName <- Candidates],
    SortByMissing = lists:sort([{length(RecFields -- UniqueFields), RecName}
				|| {RecName, RecFields} <- TypesAndFields,
			           UniqueFields -- RecFields == []]),
    {uvar, Attrs, _} = UVar,
    Line = line_number(UVar),
    case lowest_scores(SortByMissing) of
	[] ->
	    {no_records_with_all_fields, Line, UniqueFields};
	[RecName] ->
	    [{RecName, Formals, _}] = ets:lookup(record_types, RecName),
	    ets:new(freshen_tvars, [set, public, named_table]),
	    FreshRecType = freshen({app_t, Attrs, {id, Attrs, RecName}, Formals}),
	    ets:delete(freshen_tvars),
	    unify(UVar, FreshRecType),
	    true;
	StillPossible ->
	    {ambiguous_record, Line, UniqueFields, StillPossible}
    end.

lowest_scores([{M, X}, {N, Y}|More]) ->
    if M < N  -> [X];
       M == N -> [X|lowest_scores([{N, Y}|More])]
    end;
lowest_scores([{_M, X}]) ->
    [X];
lowest_scores([]) ->
    [].

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
unfold_record_types(T) when is_tuple(T) ->
    list_to_tuple(unfold_record_types(tuple_to_list(T)));
unfold_record_types([H|T]) ->
    [unfold_record_types(H)|unfold_record_types(T)];
unfold_record_types(X) ->
    X.

unfold_record_types_in_type(Type={app_t, _, {id, _, RecName}, Args}) ->
    case ets:lookup(record_types, RecName) of
	[{RecName, Formals, Fields}] when length(Formals) == length(Args) ->
	    {record_t,
	     unfold_record_types_in_type(
	       subst_tvars(lists:zip(Formals, Args), Fields))};
	_ ->
	    %% Not a record type, or ill-formed record type.
	    Type
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
unfold_record_types_in_type({field_t, Attr, Mut, Name, Type}) ->
    {field_t, Attr, Mut, Name, unfold_record_types_in_type(Type)};
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

unify({id, _, "_"}, _) ->
  true;
unify(_, {id, _, "_"}) ->
  true;
unify(T1, T2) ->
  unify1(dereference(T1), dereference(T2)).

unify1({uvar, _, R}, {uvar, _, R}) ->
    true;
unify1({uvar, A, R}, T) ->
    case occurs_check(R, T) of
	true ->
	    cannot_unify({uvar, A, R}, T),
	    false;
	false ->
	    ets:insert(type_vars, {R, T}),
	    true
    end;
unify1(T, {uvar, A, R}) ->
    unify1({uvar, A, R}, T);
unify1({tvar, _, X}, {tvar, _, X}) -> true; %% Rigid type variables
unify1([A|B], [C|D]) ->
    unify(A, C) andalso unify(B, D);
unify1(X, X) ->
    true;
unify1({id, _, Name}, {id, _, Name}) ->
    true;
unify1({fun_t, _, Args1, Result1}, {fun_t, _, Args2, Result2}) ->
    unify(Args1, Args2) andalso unify(Result1, Result2);
unify1({app_t, _, {id, _, F}, Args1}, {app_t, _, {id, _, F}, Args2})
  when length(Args1) == length(Args2) ->
    unify(Args1, Args2);
unify1({tuple_t, _, As}, {tuple_t, _, Bs})
  when length(As) == length(Bs) ->
    unify(As, Bs);
%% The grammar is a bit inconsistent about whether types without
%% arguments are represented as applications to an empty list of
%% parameters or not. We therefore allow them to unify.
unify1({app_t, _, T, []}, B) ->
    unify(T, B);
unify1(A, {app_t, _, T, []}) ->
    unify(A, T);
unify1(A, B) ->
    cannot_unify(A, B),
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

cannot_unify(A, B) ->
    ets:insert(unification_errors, {A, B}).

create_unification_errors() ->
    ets:new(unification_errors, [bag, named_table, public]).

destroy_and_report_unification_errors() ->
    [io:format("Cannot unify ~s (from line ~p)\n"
	       "         and ~s (from line ~p)\n",
	       [pp(instantiate(A)), line_number(A),
		pp(instantiate(B)), line_number(B)])
     || {A, B} <- ets:tab2list(unification_errors)],
    ets:delete(unification_errors).

line_number(T) ->
    aeso_syntax:get_ann(line, T, 0).

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
pp({con, _, Name}) ->
    Name;
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
        N when N > 1 -> error({conflicting_updates_for_field, element(2, Upd), Key});
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
