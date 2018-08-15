%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Compiler from Aeterinty Sophia language to the Aeternity VM, aevm.
%%% @end
%%% Created : 21 Dec 2017
%%%
%%%-------------------------------------------------------------------
-module(aeso_ast_to_icode).

-export([ast_typerep/1,
         convert/2]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aeso_icode.hrl").

-spec convert(aeso_syntax:ast(), list()) -> aeso_icode:icode().
convert(Tree, Options) ->
    TypedTree = aeso_ast_infer_types:infer(Tree),
    [io:format("Typed tree:\n~s\n",[prettypr:format(aeso_pretty:decls(TypedTree, [show_generated]))]) || lists:member(pp_typed,Options)],
    %% [io:format("Typed tree:\n~p\n",[TypedTree]) || lists:member(pp_typed,Options)],
    code(TypedTree, aeso_icode:new(Options)).

code([{contract, _Attribs, {con, _, Name}, Code}|Rest], Icode) ->
    NewIcode = contract_to_icode(Code,
                                 aeso_icode:set_name(Name, Icode)),
    code(Rest, NewIcode);
code([], Icode) ->
    add_default_init_function(add_builtins(Icode)).

%% Create default init function (only if state is unit).
add_default_init_function(Icode = #{functions := Funs, state_type := State}) ->
    case lists:keymember("init", 1, Funs) of
        true -> Icode;
        false when State /= {tuple, []} -> error(missing_init_function);
        false ->
            DefaultInit = {"init", [], {tuple, []}, {tuple, []}},
            Icode#{ functions => [DefaultInit | Funs] }
    end.

-spec contract_to_icode(aeso_syntax:ast(), aeso_icode:icode()) ->
                               aeso_icode:icode().
contract_to_icode([{type_def, _Attrib, {id, _, Name}, Args, Def} | Rest],
                  Icode = #{ types := Types, constructors := Constructors }) ->
    TypeDef = make_type_def(Args, Def, Icode),
    NewConstructors =
        case Def of
            {variant_t, Cons} ->
                Tags = lists:seq(0, length(Cons) - 1),
                GetName = fun({constr_t, _, {con, _, C}, _}) -> C end,
                maps:from_list([ {GetName(Con), Tag} || {Tag, Con} <- lists:zip(Tags, Cons) ]);
            _ -> #{}
        end,
    Icode1 = Icode#{ types := Types#{ Name => TypeDef },
                     constructors := maps:merge(Constructors, NewConstructors) },
    Icode2 = case Name of
                "state" when Args == [] -> Icode1#{ state_type => ast_typerep(Def, Icode) };
                "state"                 -> error(state_type_cannot_be_parameterized);
                _                       -> Icode1
             end,
    contract_to_icode(Rest, Icode2);
contract_to_icode([{letfun,_Attrib, Name, Args, _What, Body={typed,_,_,T}}|Rest], Icode) ->
    %% TODO: Handle types
    FunName = ast_id(Name),
    %% TODO: push funname to env
    FunArgs = ast_args(Args, [], Icode),
    %% TODO: push args to env
    FunBody = ast_body(Body, Icode),
    TypeRep = ast_typerep(T, Icode),
    NewIcode = ast_fun_to_icode(FunName, FunArgs, FunBody, TypeRep, Icode),
    contract_to_icode(Rest, NewIcode);
contract_to_icode([{letrec,_,Defs}|Rest], Icode) ->
    %% OBS! This code ignores the letrec structure of the source,
    %% because the back end treats ALL declarations as recursive! We
    %% need to decide whether to (a) modify the back end to respect
    %% the letrec structure, or (b) (preferably) modify the front end
    %% just to parse a list of (mutually recursive) definitions.
    contract_to_icode(Defs++Rest, Icode);
contract_to_icode([], Icode) -> Icode;
contract_to_icode(Code, Icode) ->
    io:format("Unhandled code ~p~n",[Code]),
    Icode.

ast_id({id, _, Id}) -> Id.

ast_args([{arg, _, Name, Type}|Rest], Acc, Icode) ->
    ast_args(Rest, [{ast_id(Name), ast_type(Type, Icode)}| Acc], Icode);
ast_args([], Acc, _Icode) -> lists:reverse(Acc).

ast_type(T, Icode) ->
    ast_typerep(T, Icode).

-define(id_app(Fun, Args, ArgTypes, OutType),
    {app, _, {typed, _, {id, _, Fun}, {fun_t, _, ArgTypes, OutType}}, Args}).

-define(qid_app(Fun, Args, ArgTypes, OutType),
    {app, _, {typed, _, {qid, _, Fun}, {fun_t, _, ArgTypes, OutType}}, Args}).

-define(oracle_t(Q, R), {app_t, _, {id, _, "oracle"}, [Q, R]}).
-define(query_t(Q, R),  {app_t, _, {id, _, "oracle_query"}, [Q, R]}).
-define(option_t(A),    {app_t, _, {id, _, "option"}, [A]}).
-define(map_t(K, V),    {app_t, _, {id, _, "map"}, [K, V]}).

ast_body(?id_app("raw_call", [To, Fun, Gas, Value, {typed, _, Arg, ArgT}], _, OutT), Icode) ->
    %% TODO: temp hack before we have contract calls properly in the type checker
    {Args, ArgTypes} =
        case Arg of %% Hack: pack unary argument in tuple
            %% Already a tuple.
            {tuple, _,_Elems} -> {Arg, ArgT};
            _ -> {{tuple, [], [Arg]}, {tuple_t, [], [ArgT]}}
        end,
    #prim_call_contract{ gas      = ast_body(Gas, Icode),
                         address  = ast_body(To, Icode),
                         value    = ast_body(Value, Icode),
                         arg      = #tuple{cpts = [ast_body(X, Icode) || X <- [Fun , Args]]},
                         arg_type = {tuple, [string , ast_typerep(ArgTypes, Icode)]},
                         out_type = ast_typerep(OutT, Icode) };
ast_body(?id_app("raw_spend", [To, Amount], _, _), Icode) ->
    prim_call(?PRIM_CALL_SPEND, ast_body(Amount, Icode), [ast_body(To, Icode)], [word], {tuple, []});

%% Chain environment
ast_body(?qid_app(["Chain", "balance"], [Address], _, _), Icode) ->
    #prim_balance{ address = ast_body(Address, Icode) };
ast_body(?qid_app(["Chain", "block_hash"], [Height], _, _), Icode) ->
    #prim_block_hash{ height = ast_body(Height, Icode) };
ast_body({qid, _, ["Contract", "address"]}, _Icode)      -> prim_contract_address;
ast_body({qid, _, ["Contract", "balance"]}, _Icode)      -> #prim_balance{ address = prim_contract_address };
ast_body({qid, _, ["Call",     "origin"]}, _Icode)       -> prim_call_origin;
ast_body({qid, _, ["Call",     "caller"]}, _Icode)       -> prim_caller;
ast_body({qid, _, ["Call",     "value"]}, _Icode)        -> prim_call_value;
ast_body({qid, _, ["Call",     "gas_price"]}, _Icode)    -> prim_gas_price;
ast_body({qid, _, ["Chain",    "coinbase"]}, _Icode)     -> prim_coinbase;
ast_body({qid, _, ["Chain",    "timestamp"]}, _Icode)    -> prim_timestamp;
ast_body({qid, _, ["Chain",    "block_height"]}, _Icode) -> prim_block_height;
ast_body({qid, _, ["Chain",    "difficulty"]}, _Icode)   -> prim_difficulty;
ast_body({qid, _, ["Chain",    "gas_limit"]}, _Icode)    -> prim_gas_limit;
%% TODO: eta expand!
ast_body({qid, _, ["Chain", "balance"]}, _Icode) ->
    error({underapplied_primitive, 'Chain.balance'});
ast_body({qid, _, ["Chain", "block_hash"]}, _Icode) ->
    error({underapplied_primitive, 'Chain.block_hash'});
ast_body({id, _, "raw_call"}, _Icode) ->
    error({underapplied_primitive, raw_call});
ast_body({id, _, "raw_spend"}, _Icode) ->
    error({underapplied_primitive, raw_spend});

%% State
ast_body({id, _, "state"}, _Icode) -> prim_state;
ast_body(?id_app("put", [NewState], _, _), Icode) ->
    #prim_put{ state = ast_body(NewState, Icode) };
ast_body({id, _, "put"}, _Icode) ->
    error({underapplied_primitive, put});   %% TODO: eta

%% Oracles
ast_body(?qid_app(["Oracle", "register"], [Acct, Sign, QFee, TTL], _, ?oracle_t(QType, RType)), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_REGISTER, #integer{value = 0},
              [ast_body(Acct, Icode), ast_body(Sign, Icode), ast_body(QFee, Icode), ast_body(TTL, Icode),
               ast_type_value(QType, Icode), ast_type_value(RType, Icode)],
              [word, word, word, word, typerep, typerep], word);

ast_body(?qid_app(["Oracle", "query_fee"], [Oracle], _, _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_QUERY_FEE, #integer{value = 0},
              [ast_body(Oracle, Icode)], [word], word);

ast_body(?qid_app(["Oracle", "query"], [Oracle, Q, QFee, QTTL, RTTL], [_, QType, _, _, _], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_QUERY, ast_body(QFee, Icode),
              [ast_body(Oracle, Icode), ast_body(Q, Icode), ast_body(QTTL, Icode), ast_body(RTTL, Icode)],
              [word, ast_type(QType, Icode), word, word], word);

ast_body(?qid_app(["Oracle", "extend"], [Oracle, Sign, TTL], _, _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_EXTEND, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Sign, Icode), ast_body(TTL, Icode)],
              [word, word, word], {tuple, []});

ast_body(?qid_app(["Oracle", "respond"], [Oracle, Query, Sign, R], [_, _, _, RType], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_RESPOND, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Query, Icode), ast_body(Sign, Icode), ast_body(R, Icode)],
              [word, word, word, ast_type(RType, Icode)], {tuple, []});

ast_body(?qid_app(["Oracle", "get_question"], [Oracle, Q], [_, ?query_t(QType, _)], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_GET_QUESTION, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Q, Icode)], [word, word], ast_type(QType, Icode));

ast_body(?qid_app(["Oracle", "get_answer"], [Oracle, Q], [_, ?query_t(_, RType)], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_GET_ANSWER, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Q, Icode)], [word, word], {option, ast_type(RType, Icode)});

ast_body({qid, _, ["Oracle", "register"]}, _Icode)     -> error({underapplied_primitive, 'Oracle.register'});
ast_body({qid, _, ["Oracle", "query"]}, _Icode)        -> error({underapplied_primitive, 'Oracle.query'});
ast_body({qid, _, ["Oracle", "extend"]}, _Icode)       -> error({underapplied_primitive, 'Oracle.extend'});
ast_body({qid, _, ["Oracle", "respond"]}, _Icode)      -> error({underapplied_primitive, 'Oracle.respond'});
ast_body({qid, _, ["Oracle", "query_fee"]}, _Icode)    -> error({underapplied_primitive, 'Oracle.query_fee'});
ast_body({qid, _, ["Oracle", "get_answer"]}, _Icode)   -> error({underapplied_primitive, 'Oracle.get_answer'});
ast_body({qid, _, ["Oracle", "get_question"]}, _Icode) -> error({underapplied_primitive, 'Oracle.get_question'});

%% Name service
ast_body(?qid_app(["AENS", "resolve"], [Name, Key], _, ?option_t(Type)), Icode) ->
    case is_monomorphic(Type) of
        true ->
            case ast_type(Type, Icode) of
                T when T == word; T == string -> ok;
                _ -> error({invalid_result_type, 'AENS.resolve', Type})
            end,
            prim_call(?PRIM_CALL_AENS_RESOLVE, #integer{value = 0},
                      [ast_body(Name, Icode), ast_body(Key, Icode), ast_type_value(Type, Icode)],
                      [string, string, typerep], {option, ast_type(Type, Icode)});
        false ->
            error({unresolved_result_type, 'AENS.resolve', Type})
    end;

ast_body(?qid_app(["AENS", "preclaim"], [Addr, CHash, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_PRECLAIM, #integer{value = 0},
              [ast_body(Addr, Icode), ast_body(CHash, Icode), ast_body(Sign, Icode)],
              [word, word, word], {tuple, []});

ast_body(?qid_app(["AENS", "claim"], [Addr, Name, Salt, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_CLAIM, #integer{value = 0},
              [ast_body(Addr, Icode), ast_body(Name, Icode), ast_body(Salt, Icode), ast_body(Sign, Icode)],
              [word, string, word, word], {tuple, []});

ast_body(?qid_app(["AENS", "transfer"], [FromAddr, ToAddr, NameHash, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_TRANSFER, #integer{value = 0},
              [ast_body(FromAddr, Icode), ast_body(ToAddr, Icode), ast_body(NameHash, Icode), ast_body(Sign, Icode)],
              [word, word, word, word], {tuple, []});

ast_body(?qid_app(["AENS", "revoke"], [Addr, NameHash, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_REVOKE, #integer{value = 0},
              [ast_body(Addr, Icode), ast_body(NameHash, Icode), ast_body(Sign, Icode)],
              [word, word, word], {tuple, []});

ast_body({qid, _, ["AENS", "resolve"]}, _Icode)  -> error({underapplied_primitive, 'AENS.resolve'});
ast_body({qid, _, ["AENS", "preclaim"]}, _Icode) -> error({underapplied_primitive, 'AENS.preclaim'});
ast_body({qid, _, ["AENS", "claim"]}, _Icode)    -> error({underapplied_primitive, 'AENS.claim'});
ast_body({qid, _, ["AENS", "transfer"]}, _Icode) -> error({underapplied_primitive, 'AENS.transfer'});
ast_body({qid, _, ["AENS", "revoke"]}, _Icode)   -> error({underapplied_primitive, 'AENS.revoke'});

%% Maps

%% -- map lookup  m[k]
ast_body({map_get, _, Map, {typed, Ann, Key, KeyType}}, Icode) ->
    Fun = {map_get, key_type(Ann, KeyType, Icode)},
    #funcall{ function = #var_ref{name = {builtin, Fun}},
              args     = [ast_body(Map, Icode), ast_body(Key, Icode)] };

%% -- lookup functions
ast_body(?qid_app(["Map", "lookup"], [{typed, Ann, Key, KeyType}, Map], _, _), Icode) ->
    Fun = {map_lookup, key_type(Ann, KeyType, Icode)},
    #funcall{ function = #var_ref{name = {builtin, Fun}},
              args     = [ast_body(Map, Icode), ast_body(Key, Icode)] };
ast_body(?qid_app(["Map", "member"], [{typed, Ann, Key, KeyType}, Map], _, _), Icode) ->
    Fun = {map_member, key_type(Ann, KeyType, Icode)},
    #funcall{ function = #var_ref{name = {builtin, Fun}},
              args     = [ast_body(Map, Icode), ast_body(Key, Icode)] };
ast_body(?qid_app(["Map", "size"], [Map], _, _), Icode) ->
    #funcall{ function = #var_ref{name = {builtin, map_size}},
              args     = [ast_body(Map, Icode), {integer, 0}] };
ast_body(?qid_app(["Map", "delete"], [{typed, Ann, Key, KeyType}, Map], _, _), Icode) ->
    Fun = {map_del, key_type(Ann, KeyType, Icode)},
    #funcall{ function = #var_ref{name = {builtin, Fun}},
              args     = [ast_body(Map, Icode), ast_body(Key, Icode)] };

%% -- map conversion to/from list
ast_body(?qid_app(["Map", "from_list"], [List], _, _), Icode) -> ast_body(List, Icode);
ast_body(?qid_app(["Map", "to_list"],   [Map], _, _), Icode)  -> ast_body(Map, Icode);

ast_body({qid, _, ["Map", "from_list"]}, _Icode) -> error({underapplied_primitive, 'Map.from_list'});
ast_body({qid, _, ["Map", "to_list"]}, _Icode)   -> error({underapplied_primitive, 'Map.to_list'});
ast_body({qid, _, ["Map", "lookup"]}, _Icode)    -> error({underapplied_primitive, 'Map.to_list'});
ast_body({qid, _, ["Map", "member"]}, _Icode)    -> error({underapplied_primitive, 'Map.to_list'});

%% -- map construction { k1 = v1, k2 = v2 }
ast_body({map, Ann, KVs}, Icode) ->
    ast_body({list, Ann, [{tuple, Ann, [K, V]} || {K, V} <- KVs]}, Icode);

%% -- map update       m { [k] = v } or m { [k] @ x = f(x) }
ast_body({map, _, Map, []}, Icode) -> ast_body(Map, Icode);
ast_body({map, _, Map, [Upd]}, Icode) ->
    {Fun, Args} = case Upd of
            {field, _, [{map_get, _, {typed, Ann, Key, KeyType}}], Val} ->
                {{map_put, key_type(Ann, KeyType, Icode)}, [ast_body(Key, Icode), ast_body(Val, Icode)]};
            {field_upd, _, [{map_get, _, {typed, Ann, Key, KeyType}}], ValFun} ->
                {{map_upd, key_type(Ann, KeyType, Icode)}, [ast_body(Key, Icode), ast_body(ValFun, Icode)]}
        end,
    #funcall{ function = #var_ref{name = {builtin, Fun}},
              args     = [ast_body(Map, Icode) | Args] };
ast_body({map, Ann, Map, [Upd | Upds]}, Icode) ->
    ast_body({map, Ann, {map, Ann, Map, [Upd]}, Upds}, Icode);

%% Other terms
ast_body({id, _, Name}, _Icode) ->
    %% TODO Look up id in env
    #var_ref{name = Name};
ast_body({bool, _, Bool}, _Icode) ->		%BOOL as ints
    Value = if Bool -> 1 ; true -> 0 end,
    #integer{value = Value};
ast_body({int, _, Value}, _Icode) ->
    #integer{value = Value};
ast_body({hash, _, Hash}, _Icode) ->
    <<Value:32/unit:8>> = Hash,
    #integer{value = Value};
ast_body({string,_,Bin}, _Icode) ->
    Cpts = [size(Bin)|aeso_data:binary_to_words(Bin)],
    #tuple{cpts = [#integer{value=X} || X <- Cpts]};
ast_body({tuple,_,Args}, Icode) ->
    #tuple{cpts = [ast_body(A, Icode) || A <- Args]};
ast_body({list,_,Args}, Icode) ->
    #list{elems = [ast_body(A, Icode) || A <- Args]};
%% Hardwired option types. TODO: Remove when we have arbitrary variant types.
ast_body({con, _, "None"}, _Icode) ->
    #list{elems = []};
ast_body({app, _, {typed, _, {con, _, "Some"}, _}, [Elem]}, Icode) ->
    #tuple{cpts = [ast_body(Elem, Icode)]};
ast_body({typed, _, {con, _, "Some"}, {fun_t, _, [A], _}}, Icode) ->
    #lambda{ args = [#arg{name = "x", type = ast_type(A, Icode)}]
           , body = #tuple{cpts = [#var_ref{name = "x"}]} };
ast_body({con, _, Name}, Icode) ->
    Tag = aeso_icode:get_constructor_tag(Name, Icode),
    #tuple{cpts = [#integer{value = Tag}]};
ast_body({app, _, {typed, _, {con, _, Name}, _}, Args}, Icode) ->
    Tag = aeso_icode:get_constructor_tag(Name, Icode),
    #tuple{cpts = [#integer{value = Tag} | [ ast_body(Arg, Icode) || Arg <- Args ]]};
ast_body({app,As,Fun,Args}, Icode) ->
    case aeso_syntax:get_ann(format, As) of
        infix  ->
            {Op, _} = Fun,
            [A, B]  = Args,
            ast_binop(Op, As, A, B, Icode);
        prefix ->
            {Op, _} = Fun,
            [A]     = Args,
            #unop{op = Op, rand = ast_body(A, Icode)};
        _ ->
            #funcall{function=ast_body(Fun, Icode),
	             args=[ast_body(A, Icode) || A <- Args]}
    end;
ast_body({'if',_,Dec,Then,Else}, Icode) ->
    #ifte{decision = ast_body(Dec, Icode)
	 ,then     = ast_body(Then, Icode)
	 ,else     = ast_body(Else, Icode)};
ast_body({switch,_,A,Cases}, Icode) ->
    %% let's assume the parser has already ensured that only valid
    %% patterns appear in cases.
    #switch{expr=ast_body(A, Icode),
	    cases=[{ast_body(Pat, Icode),ast_body(Body, Icode)}
		   || {'case',_,Pat,Body} <- Cases]};
ast_body({block,As,[{letval,_,Pat,_,E}|Rest]}, Icode) ->
    #switch{expr=ast_body(E, Icode),
	    cases=[{ast_body(Pat, Icode),ast_body({block,As,Rest}, Icode)}]};
ast_body({block,_,[]}, _Icode) ->
    #tuple{cpts=[]};
ast_body({block,_,[E]}, Icode) ->
    ast_body(E, Icode);
ast_body({block,As,[E|Rest]}, Icode) ->
    #switch{expr=ast_body(E, Icode),
	    cases=[{#var_ref{name="_"},ast_body({block,As,Rest}, Icode)}]};
ast_body({lam,_,Args,Body}, Icode) ->
    #lambda{args=[#arg{name = ast_id(P), type = ast_type(T, Icode)} || {arg,_,P,T} <- Args],
	    body=ast_body(Body, Icode)};
ast_body({typed,_,{record,Attrs,Fields},{record_t,DefFields}}, Icode) ->
    %% Compile as a tuple with the fields in the order they appear in the definition.
    NamedField = fun({field, _, [{proj, _, {id, _, Name}}], E}) -> {Name, E} end,
    NamedFields = lists:map(NamedField, Fields),
    #tuple{cpts =
	       [case proplists:get_value(Name,NamedFields) of
		    undefined ->
                        io:format("~p not in ~p\n", [Name, NamedFields]),
			Line = aeso_syntax:get_ann(line, Attrs),
			#missing_field{format = "Missing field in record: ~s (on line ~p)\n",
				       args = [Name,Line]};
		    E ->
			ast_body(E, Icode)
		end
		|| {field_t,_,{id,_,Name},_} <- DefFields]};
ast_body({typed,_,{record,Attrs,_Fields},T}, _Icode) ->
    error({record_has_bad_type,Attrs,T});
ast_body({proj,_,{typed,_,Record,{record_t,Fields}},{id,_,FieldName}}, Icode) ->
    [Index] = [I
	       || {I,{field_t,_,{id,_,Name},_}} <-
		      lists:zip(lists:seq(1,length(Fields)),Fields),
		  Name==FieldName],
    #binop{op = '!', left = #integer{value = 32*(Index-1)}, right = ast_body(Record, Icode)};
ast_body({record, Attrs, {typed, _, Record, RecType={record_t, Fields}}, Update}, Icode) ->
    UpdatedName = fun({field, _,     [{proj, _, {id, _, Name}}], _}) -> Name;
                     ({field_upd, _, [{proj, _, {id, _, Name}}], _}) -> Name
                  end,
    UpdatedNames = lists:map(UpdatedName, Update),
    Rec = {typed, Attrs, {id, Attrs, "_record"}, RecType},
    CompileUpdate =
        fun(Fld={field, _, _, _}) -> Fld;
           ({field_upd, Ann, LV=[{proj, Ann1, P}], Fun}) ->
            {field, Ann, LV, {app, Ann, Fun, [{proj, Ann1, Rec, P}]}}
        end,

    #switch{expr=ast_body(Record, Icode),
	    cases=[{#var_ref{name = "_record"},
		    ast_body({typed, Attrs,
			      {record, Attrs,
			       lists:map(CompileUpdate, Update) ++
				[{field, Attrs, [{proj, Attrs, {id, Attrs, Name}}],
				     {proj, Attrs, Rec, {id, Attrs, Name}}}
				    || {field_t, _, {id, _, Name}, _} <- Fields,
				       not lists:member(Name, UpdatedNames)]},
			      RecType}, Icode)}
		   ]};
ast_body({typed, _, Body, _}, Icode) ->
    ast_body(Body, Icode).

ast_binop(Op, Ann, {typed, _, A, Type}, B, Icode)
    when Op == '=='; Op == '!=';
         Op == '<';  Op == '>';
         Op == '<='; Op == '=<'; Op == '>=' ->
    Monomorphic = is_monomorphic(Type),
    case ast_typerep(Type, Icode) of
        _ when not Monomorphic ->
            error({cant_compare_polymorphic_type, Ann, Op, Type});
        word   -> #binop{op = Op, left = ast_body(A, Icode), right = ast_body(B, Icode)};
        string ->
            Neg = case Op of
                    '==' -> fun(X) -> X end;
                    '!=' -> fun(X) -> #unop{ op = '!', rand = X } end;
                    _    -> error({cant_compare, Ann, Op, Type})
                  end,
            Neg(#funcall{ function = #var_ref{name = {builtin, str_equal}},
                          args     = [ast_body(A, Icode), ast_body(B, Icode)] });
        _ -> error({cant_compare, Ann, Op, Type})
    end;
ast_binop(Op, _, A, B, Icode) ->
    #binop{op = Op, left = ast_body(A, Icode), right = ast_body(B, Icode)}.

is_monomorphic({tvar, _, _}) -> false;
is_monomorphic([H|T]) ->
  is_monomorphic(H) andalso is_monomorphic(T);
is_monomorphic(T) when is_tuple(T) ->
  is_monomorphic(tuple_to_list(T));
is_monomorphic(_) -> true.

%% Implemented as a contract call to the contract with address 0.
prim_call(Prim, Amount, Args, ArgTypes, OutType) ->
    #prim_call_contract{ gas      = #integer{ value = 0 },
                         address  = #integer{ value = ?PRIM_CALLS_CONTRACT },
                         value    = Amount,
                         arg      = #tuple{cpts = [#integer{ value = Prim } | Args]},
                         arg_type = {tuple, [word | ArgTypes]},
                         out_type = OutType }.

make_type_def(Args, Def, Icode = #{ type_vars := TypeEnv }) ->
    TVars = [ X || {tvar, _, X} <- Args ],
    fun(Types) ->
        TypeEnv1 = maps:from_list(lists:zip(TVars, Types)),
        ast_typerep(Def, Icode#{ type_vars := maps:merge(TypeEnv, TypeEnv1) })
    end.

-spec ast_typerep(aeso_syntax:type()) -> aeso_sophia:type().
ast_typerep(Type) -> ast_typerep(Type, aeso_icode:new([])).

ast_typerep({id, _, Name}, Icode) ->
    lookup_type_id(Name, [], Icode);
ast_typerep({app_t, _, {id, _, Name}, Args}, Icode) ->
    ArgReps = [ ast_typerep(Arg, Icode) || Arg <- Args ],
    lookup_type_id(Name, ArgReps, Icode);
ast_typerep({tvar,_,A}, #{ type_vars := TypeVars }) ->
    case maps:get(A, TypeVars, undefined) of
        undefined -> word; %% We serialize type variables just as addresses in the originating VM.
        Type      -> Type
    end;
ast_typerep({tuple_t,_,Cpts}, Icode) ->
    {tuple, [ast_typerep(C, Icode) || C<-Cpts]};
ast_typerep({record_t,Fields}, Icode) ->
    {tuple, [ begin
                {field_t, _, _, T} = Field,
                ast_typerep(T, Icode)
              end || Field <- Fields]};
ast_typerep({fun_t,_,_,_}, _Icode) ->
    function;
ast_typerep({alias_t, T}, Icode) -> ast_typerep(T, Icode);
ast_typerep({variant_t, Cons}, Icode) ->
    {variant, [ begin
                  {constr_t, _, _, Args} = Con,
                  [ ast_typerep(Arg, Icode) || Arg <- Args ]
                end || Con <- Cons ]}.

lookup_type_id(Name, Args, #{ types := Types }) ->
    case maps:get(Name, Types, undefined) of
        undefined -> error({undefined_type, Name});
        TDef      -> TDef(Args)
    end.

ast_type_value(T, Icode) ->
    type_value(ast_type(T, Icode)).

type_value(word)   ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_WORD_TAG }] };
type_value(string) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_STRING_TAG }] };
type_value({list, A}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_LIST_TAG }, type_value(A)] };
type_value({option, A}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_OPTION_TAG }, type_value(A)] };
type_value({tuple, As}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_TUPLE_TAG },
                    #list{ elems = [ type_value(A) || A <- As ] }] };
type_value({variant, Cs}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_VARIANT_TAG },
                    #list{ elems = [ #list{ elems = [ type_value(A) || A <- As ] } || As <- Cs ] }] }.

ast_fun_to_icode(Name, Args, Body, TypeRep, #{functions := Funs} = Icode) ->
    NewFuns = [{Name, Args, Body, TypeRep}| Funs],
    aeso_icode:set_functions(NewFuns, Icode).

%% -------------------------------------------------------------------
%% Builtins
%% -------------------------------------------------------------------

key_type(Ann, Type, Icode) ->
    Monomorphic = is_monomorphic(Type),
    case ast_typerep(Type, Icode) of
        _ when not Monomorphic ->
            error({polymorphic_key_type, Ann, Type});
        word   -> word;
        string -> string;
        _      -> error({unsupported_key_type, Ann, Type})
    end.

builtin_deps({map_get, Type})      -> [{map_lookup, Type}];
builtin_deps({map_member, Type})   -> [{map_lookup, Type}];
builtin_deps({map_lookup, string}) -> [str_equal];
builtin_deps({map_del, string})    -> [str_equal];
builtin_deps({map_put, string})    -> [str_equal];
builtin_deps({map_upd, string})    -> [str_equal];
builtin_deps(str_equal)            -> [str_equal_p];
builtin_deps(_) -> [].

dep_closure(Deps) ->
    case lists:umerge(lists:map(fun builtin_deps/1, Deps)) of
        []    -> Deps;
        Deps1 -> lists:umerge(Deps, dep_closure(Deps1))
    end.

used_builtins(#funcall{ function = #var_ref{ name = {builtin, Builtin} } }) ->
    dep_closure([Builtin]);
used_builtins([H|T]) ->
  lists:umerge(used_builtins(H), used_builtins(T));
used_builtins(T) when is_tuple(T) ->
  used_builtins(tuple_to_list(T));
used_builtins(M) when is_map(M) ->
  used_builtins(maps:to_list(M));
used_builtins(_) -> [].

builtin_eq(word, A, B)   -> {binop, '==', A, B};
builtin_eq(string, A, B) -> {funcall, {var_ref, {builtin, str_equal}}, [A, B]}.

builtin_function(Builtin = {map_get, Type}) ->
    %% function map_get(m, k) =
    %%   switch(map_lookup(m, k))
    %%     Some(v) => v
    {{builtin, Builtin},
        [{"m", aeso_icode:map_typerep(Type, word)}, {"k", Type}],
            {switch, {funcall, {var_ref, {builtin, {map_lookup, Type}}},
                     [{var_ref, "m"}, {var_ref, "k"}]},
                [{{tuple, [{var_ref, "v"}]}, {var_ref, "v"}}]},
     word};

builtin_function(Builtin = {map_member, Type}) ->
    %% function map_member(m, k) : bool =
    %%   switch(Map.lookup(m, k))
    %%     None => false
    %%     _    => true
    {{builtin, Builtin},
        [{"m", aeso_icode:map_typerep(Type, word)}, {"k", Type}],
            {switch, {funcall, {var_ref, {builtin, {map_lookup, Type}}},
                     [{var_ref, "m"}, {var_ref, "k"}]},
                [{{list, []}, {integer, 0}},
                 {{var_ref, "_"}, {integer, 1}}]},
     word};

builtin_function(Builtin = {map_lookup, Type}) ->
    %% function map_lookup(map, key) =
    %%   switch(map)
    %%     [] => None
    %%     (k, val) :: m =>
    %%       if(k == key)
    %%          Some(val)
    %%      else
    %%          map_lookup(m, key)
    Eq = fun(A, B) -> builtin_eq(Type, A, B) end,
    Name = {builtin, Builtin},
    {Name,
     [{"map", aeso_icode:map_typerep(Type, word)}, {"key", Type}],
     {switch, {var_ref, "map"},
              [{{list, []}, {list, []}},
               {{binop, '::',
                  {tuple, [{var_ref, "k"}, {var_ref, "val"}]},
                  {var_ref, "m"}},
                {ifte, Eq({var_ref, "k"}, {var_ref, "key"}),
                    {tuple, [{var_ref, "val"}]},
                    {funcall, {var_ref, Name},
                             [{var_ref, "m"}, {var_ref, "key"}]}}}]},
    {option, word}};

builtin_function(Builtin = {map_put, Type}) ->
    %% function map_put(map, key, val) =
    %%   switch(map)
    %%     [] => [(key, val)]
    %%     (k, v) :: m =>
    %%       if(k == key)
    %%          (k, val) :: m
    %%      else
    %%          (k, v) :: map_put(m, key, val)  // note reallocates (k, v) pair
    Eq = fun(A, B) -> builtin_eq(Type, A, B) end,
    Name = {builtin, Builtin},
    {Name,
     [{"map", aeso_icode:map_typerep(Type, word)}, {"key", Type}, {"val", word}],
     {switch,
         {var_ref, "map"},
         [{{list, []}, {list, [{tuple, [{var_ref, "key"}, {var_ref, "val"}]}]}},
          {{binop, '::',
               {tuple, [{var_ref, "k"}, {var_ref, "v"}]},
               {var_ref, "m"}},
           {ifte,
               Eq({var_ref, "k"}, {var_ref, "key"}),
               {binop, '::',
                   {tuple, [{var_ref, "k"}, {var_ref, "val"}]},
                   {var_ref, "m"}},
               {binop, '::',
                   {tuple, [{var_ref, "k"}, {var_ref, "v"}]},
                   {funcall,
                       {var_ref, Name},
                       [{var_ref, "m"},
                        {var_ref, "key"},
                        {var_ref, "val"}]}}}}]},
     aeso_icode:map_typerep(Type, word)};

builtin_function(Builtin = {map_del, Type}) ->
    %% function map_del(map, key) =
    %%   switch(map)
    %%     [] => []
    %%     (k, v) :: m =>
    %%       if(k == key)
    %%          m
    %%      else
    %%          (k, v) :: map_del(m, key)  // note reallocates (k, v) pair
    Eq = fun(A, B) -> builtin_eq(Type, A, B) end,
    Name = {builtin, Builtin},
    {Name,
     [{"map", aeso_icode:map_typerep(Type, word)}, {"key", word}],
     {switch,
         {var_ref, "map"},
         [{{list, []}, {list, []}},
          {{binop, '::',
               {tuple, [{var_ref, "k"}, {var_ref, "v"}]},
               {var_ref, "m"}},
           {ifte,
               Eq({var_ref, "k"}, {var_ref, "key"}),
               {var_ref, "m"},
               {binop, '::',
                   {tuple, [{var_ref, "k"}, {var_ref, "v"}]},
                   {funcall,
                       {var_ref, Name},
                       [{var_ref, "m"},
                        {var_ref, "key"}]}}}}]},
     aeso_icode:map_typerep(Type, word)};

builtin_function(Builtin = {map_upd, Type}) ->
    %% function map_upd(map, key, fun) =
    %%   switch(map)
    %%     (k, v) :: m =>
    %%       if(k == key)
    %%          (k, fun(v)) :: m
    %%      else
    %%          (k, v) :: map_upd(m, key, fun)  // note reallocates (k, v) pair
    Eq = fun(A, B) -> builtin_eq(Type, A, B) end,
    Name = {builtin, Builtin},
    {Name,
     [{"map", aeso_icode:map_typerep(Type, word)}, {"key", word}, {"fun", word}],
     {switch,
         {var_ref, "map"},
         [{{binop, '::',
               {tuple, [{var_ref, "k"}, {var_ref, "v"}]},
               {var_ref, "m"}},
           {ifte,
               Eq({var_ref, "k"}, {var_ref, "key"}),
               {binop, '::',
                   {tuple, [{var_ref, "k"}, {funcall, {var_ref, "fun"}, [{var_ref, "v"}]}]},
                   {var_ref, "m"}},
               {binop, '::',
                   {tuple, [{var_ref, "k"}, {var_ref, "v"}]},
                   {funcall,
                       {var_ref, Name},
                       [{var_ref, "m"},
                        {var_ref, "key"},
                        {var_ref, "fun"}]}}}}]},
     aeso_icode:map_typerep(Type, word)};

builtin_function(map_size) ->
    %% function size(map, acc) =
    %%   switch(map)
    %%     []        => acc
    %%     _ :: map' => size(map', acc + 1)
    Name = {builtin, map_size},
    {Name,
        [{"map", {list, word}}, {"acc", word}],
        {switch, {var_ref, "map"},
                [{{list, []}, {var_ref, "acc"}},
                 {{binop, '::', {var_ref, "_"}, {var_ref, "map'"}},
                  {funcall, {var_ref, Name},
                           [{var_ref, "map'"},
                            {binop, '+', {var_ref, "acc"}, {integer, 1}}]}}]},
        word};

builtin_function(str_equal_p) ->
    %% function str_equal_p(n, p1, p2) =
    %%   if(n =< 0) true
    %%   else
    %%      let w1 = *p1
    %%      let w2 = *p2
    %%      w1 == w2 && str_equal_p(n - 32, p1 + 32, p2 + 32)
    V = fun(X) -> {var_ref, atom_to_list(X)} end,
    LetWord = fun(W, P, Body) -> {switch, V(P), [{{tuple, [V(W)]}, Body}]} end,
    Name = {builtin, str_equal_p},
    {Name,
        [{"n", word}, {"p1", pointer}, {"p2", pointer}],
        {ifte, {binop, '<', V(n), {integer, 1}},
            {integer, 1},
            LetWord(w1, p1,
            LetWord(w2, p2,
                {binop, '&&', {binop, '==', V(w1), V(w2)},
                    {funcall, {var_ref, Name},
                        [{binop, '-', V(n), {integer, 32}},
                         {binop, '+', V(p1), {integer, 32}},
                         {binop, '+', V(p2), {integer, 32}}]}}))},
     word};

builtin_function(str_equal) ->
    %% function str_equal(s1, s2) =
    %%   let n1 = length(s1)
    %%   let n2 = length(s2)
    %%   n1 == n2 && str_equal_p(n1, s1 + 32, s2 + 32)
    V = fun(X) -> {var_ref, atom_to_list(X)} end,
    LetLen = fun(N, S, Body) -> {switch, V(S), [{{tuple, [V(N)]}, Body}]} end,
    {{builtin, str_equal},
        [{"s1", string}, {"s2", string}],
        LetLen(n1, s1,
        LetLen(n2, s2,
            {binop, '&&', {binop, '==', V(n1), V(n2)},
                {funcall, {var_ref, {builtin, str_equal_p}},
                    [V(n1), {binop, '+', V(s1), {integer, 32}},
                            {binop, '+', V(s2), {integer, 32}}]}})),
        word}.

add_builtins(Icode = #{functions := Funs}) ->
    Builtins = used_builtins(Funs),
    Icode#{functions := [ builtin_function(B) || B <- Builtins ] ++ Funs}.
