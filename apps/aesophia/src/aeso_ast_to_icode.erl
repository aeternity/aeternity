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

-export([ast_typerep/1, type_value/1,
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
            Type  = {tuple, [typerep, {tuple, []}]},
            Value = #tuple{ cpts = [type_value({tuple, []}), {tuple, []}] },
            DefaultInit = {"init", [], [], Value, Type},
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
contract_to_icode([{letfun, Attrib, Name, Args, _What, Body={typed,_,_,T}}|Rest], Icode) ->
    FunAttrs = [ stateful || proplists:get_value(stateful, Attrib, false) ] ++
               [ private  || proplists:get_value(private, Attrib, false) orelse
                             proplists:get_value(internal, Attrib, false) ],
    %% TODO: Handle types
    FunName = ast_id(Name),
    %% TODO: push funname to env
    FunArgs = ast_args(Args, [], Icode),
    %% TODO: push args to env
    {FunBody, TypeRep} =
        case FunName of
            "init" ->
                %% Pair the initial state with a typerep for the state (TODO: until we have the state type in some contract metadata)
                #{ state_type := StateType } = Icode,
                {#tuple{ cpts = [type_value(StateType), ast_body(Body, Icode)] },
                 {tuple, [typerep, ast_typerep(T, Icode)]}};
            _ -> {ast_body(Body, Icode), ast_typerep(T, Icode)}
        end,
    NewIcode = ast_fun_to_icode(FunName, FunAttrs, FunArgs, FunBody, TypeRep, Icode),
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
    {app, _, {typed, _, {id, _, Fun}, {fun_t, _, _, ArgTypes, OutType}}, Args}).

-define(qid_app(Fun, Args, ArgTypes, OutType),
    {app, _, {typed, _, {qid, _, Fun}, {fun_t, _, _, ArgTypes, OutType}}, Args}).

-define(oracle_t(Q, R), {app_t, _, {id, _, "oracle"}, [Q, R]}).
-define(query_t(Q, R),  {app_t, _, {id, _, "oracle_query"}, [Q, R]}).
-define(option_t(A),    {app_t, _, {id, _, "option"}, [A]}).
-define(map_t(K, V),    {app_t, _, {id, _, "map"}, [K, V]}).

ast_body(?qid_app(["Chain","spend"], [To, Amount], _, _), Icode) ->
    prim_call(?PRIM_CALL_SPEND, ast_body(Amount, Icode), [ast_body(To, Icode)], [word], {tuple, []});

%% Chain environment
ast_body(?qid_app(["Chain", "balance"], [Address], _, _), Icode) ->
    #prim_balance{ address = ast_body(Address, Icode) };
ast_body(?qid_app(["Chain", "block_hash"], [Height], _, _), Icode) ->
    #prim_block_hash{ height = ast_body(Height, Icode) };
ast_body(?qid_app(["Call", "gas_left"], [], _, _), _Icode) ->
    prim_gas_left;
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
ast_body({qid, _, ["Chain", "spend"]}, _Icode) ->
    error({underapplied_primitive, 'Chain.spend'});

%% State
ast_body({id, _, "state"}, _Icode) -> prim_state;
ast_body(?id_app("put", [NewState], _, _), Icode) ->
    #prim_put{ state = ast_body(NewState, Icode) };
ast_body({id, _, "put"}, _Icode) ->
    error({underapplied_primitive, put});   %% TODO: eta

%% Abort
ast_body(?id_app("abort", [String], _, _), Icode) ->
    #funcall{ function = #var_ref{ name = {builtin, abort} },
              args     = [ast_body(String, Icode)] };

%% Oracles
ast_body(?qid_app(["Oracle", "register"], [Acct, Sign, QFee, TTL], _, ?oracle_t(QType, RType)), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_REGISTER, #integer{value = 0},
              [ast_body(Acct, Icode), ast_body(Sign, Icode), ast_body(QFee, Icode), ast_body(TTL, Icode),
               ast_type_value(QType, Icode), ast_type_value(RType, Icode)],
              [word, sign_t(), word, ttl_t(Icode), typerep, typerep], word);

ast_body(?qid_app(["Oracle", "query_fee"], [Oracle], _, _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_QUERY_FEE, #integer{value = 0},
              [ast_body(Oracle, Icode)], [word], word);

ast_body(?qid_app(["Oracle", "query"], [Oracle, Q, QFee, QTTL, RTTL], [_, QType, _, _, _], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_QUERY, ast_body(QFee, Icode),
              [ast_body(Oracle, Icode), ast_body(Q, Icode), ast_body(QTTL, Icode), ast_body(RTTL, Icode)],
              [word, ast_type(QType, Icode), ttl_t(Icode), ttl_t(Icode)], word);

ast_body(?qid_app(["Oracle", "extend"], [Oracle, Sign, TTL], _, _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_EXTEND, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Sign, Icode), ast_body(TTL, Icode)],
              [word, sign_t(), ttl_t(Icode)], {tuple, []});

ast_body(?qid_app(["Oracle", "respond"], [Oracle, Query, Sign, R], [_, _, _, RType], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_RESPOND, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Query, Icode), ast_body(Sign, Icode), ast_body(R, Icode)],
              [word, word, sign_t(), ast_type(RType, Icode)], {tuple, []});

ast_body(?qid_app(["Oracle", "get_question"], [Oracle, Q], [_, ?query_t(QType, _)], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_GET_QUESTION, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Q, Icode)], [word, word], ast_type(QType, Icode));

ast_body(?qid_app(["Oracle", "get_answer"], [Oracle, Q], [_, ?query_t(_, RType)], _), Icode) ->
    prim_call(?PRIM_CALL_ORACLE_GET_ANSWER, #integer{value = 0},
              [ast_body(Oracle, Icode), ast_body(Q, Icode)], [word, word], aeso_icode:option_typerep(ast_type(RType, Icode)));

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
                      [string, string, typerep], aeso_icode:option_typerep(ast_type(Type, Icode)));
        false ->
            error({unresolved_result_type, 'AENS.resolve', Type})
    end;

ast_body(?qid_app(["AENS", "preclaim"], [Addr, CHash, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_PRECLAIM, #integer{value = 0},
              [ast_body(Addr, Icode), ast_body(CHash, Icode), ast_body(Sign, Icode)],
              [word, word, sign_t()], {tuple, []});

ast_body(?qid_app(["AENS", "claim"], [Addr, Name, Salt, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_CLAIM, #integer{value = 0},
              [ast_body(Addr, Icode), ast_body(Name, Icode), ast_body(Salt, Icode), ast_body(Sign, Icode)],
              [word, string, word, sign_t()], {tuple, []});

ast_body(?qid_app(["AENS", "transfer"], [FromAddr, ToAddr, NameHash, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_TRANSFER, #integer{value = 0},
              [ast_body(FromAddr, Icode), ast_body(ToAddr, Icode), ast_body(NameHash, Icode), ast_body(Sign, Icode)],
              [word, word, word, sign_t()], {tuple, []});

ast_body(?qid_app(["AENS", "revoke"], [Addr, NameHash, Sign], _, _), Icode) ->
    prim_call(?PRIM_CALL_AENS_REVOKE, #integer{value = 0},
              [ast_body(Addr, Icode), ast_body(NameHash, Icode), ast_body(Sign, Icode)],
              [word, word, sign_t()], {tuple, []});

ast_body({qid, _, ["AENS", "resolve"]}, _Icode)  -> error({underapplied_primitive, 'AENS.resolve'});
ast_body({qid, _, ["AENS", "preclaim"]}, _Icode) -> error({underapplied_primitive, 'AENS.preclaim'});
ast_body({qid, _, ["AENS", "claim"]}, _Icode)    -> error({underapplied_primitive, 'AENS.claim'});
ast_body({qid, _, ["AENS", "transfer"]}, _Icode) -> error({underapplied_primitive, 'AENS.transfer'});
ast_body({qid, _, ["AENS", "revoke"]}, _Icode)   -> error({underapplied_primitive, 'AENS.revoke'});

%% Maps

%% -- map lookup  m[k]
ast_body({map_get, _, Map, Key}, Icode) ->
    {_, ValType} = check_monomorphic_map(Map, Icode),
    Fun = {map_get, ast_typerep(ValType, Icode)},
    builtin_call(Fun, [ast_body(Map, Icode), ast_body(Key, Icode)]);

%% -- lookup functions
ast_body(?qid_app(["Map", "lookup"], [Key, Map], _, _), Icode) ->
    map_get(Key, Map, Icode);
ast_body(?qid_app(["Map", "lookup_default"], [Key, Map, Val], _, _), Icode) ->
    {_, ValType} = check_monomorphic_map(Map, Icode),
    Fun = {map_lookup_default, ast_typerep(ValType, Icode)},
    builtin_call(Fun, [ast_body(Map, Icode), ast_body(Key, Icode), ast_body(Val, Icode)]);
ast_body(?qid_app(["Map", "member"], [Key, Map], _, _), Icode) ->
    builtin_call(map_member, [ast_body(Map, Icode), ast_body(Key, Icode)]);
ast_body(?qid_app(["Map", "size"], [Map], _, _), Icode) ->
    builtin_call(map_size, [ast_body(Map, Icode)]);
ast_body(?qid_app(["Map", "delete"], [Key, Map], _, _), Icode) ->
    map_del(Key, Map, Icode);

%% -- map conversion to/from list
ast_body(App = ?qid_app(["Map", "from_list"], [List], _, MapType), Icode) ->
    Ann = aeso_syntax:get_ann(App),
    {KeyType, ValType} = check_monomorphic_map(Ann, MapType, Icode),
    builtin_call(map_from_list, [ast_body(List, Icode), map_empty(KeyType, ValType, Icode)]);

ast_body(?qid_app(["Map", "to_list"], [Map], _, _), Icode) ->
    map_tolist(Map, Icode);

ast_body({qid, _, ["Map", "from_list"]}, _Icode) -> error({underapplied_primitive, 'Map.from_list'});
%% ast_body({qid, _, ["Map", "to_list"]}, _Icode)   -> error({underapplied_primitive, 'Map.to_list'});
ast_body({qid, _, ["Map", "lookup"]}, _Icode)    -> error({underapplied_primitive, 'Map.lookup'});
ast_body({qid, _, ["Map", "lookup_default"]}, _Icode)    -> error({underapplied_primitive, 'Map.lookup_default'});
ast_body({qid, _, ["Map", "member"]}, _Icode)    -> error({underapplied_primitive, 'Map.member'});

%% -- map construction { k1 = v1, k2 = v2 }
ast_body({typed, Ann, {map, _, KVs}, MapType}, Icode) ->
    {KeyType, ValType} = check_monomorphic_map(Ann, MapType, Icode),
    lists:foldr(fun({K, V}, Map) ->
                    builtin_call(map_put, [Map, ast_body(K, Icode), ast_body(V, Icode)])
                end, map_empty(KeyType, ValType, Icode), KVs);

%% -- map update       m { [k] = v } or m { [k] @ x = f(x) }
ast_body({map, _, Map, []}, Icode) -> ast_body(Map, Icode);
ast_body({map, _, Map, [Upd]}, Icode) ->
    case Upd of
        {field, _, [{map_get, _, Key}], Val} ->
            map_put(Key, Val, Map, Icode);
        {field_upd, _, [{map_get, _, Key}], ValFun} ->
            map_upd(Key, ValFun, Map, Icode)
    end;
ast_body({map, Ann, Map, [Upd | Upds]}, Icode) ->
    ast_body({map, Ann, {map, Ann, Map, [Upd]}, Upds}, Icode);

%% Strings
%% -- String length
ast_body(?qid_app(["String", "length"], [String], _, _), Icode) ->
    #funcall{ function = #var_ref{ name = {builtin, string_length} },
              args     = [ast_body(String, Icode)] };

%% -- String concat
ast_body(?qid_app(["String", "concat"], [String1, String2], _, _), Icode) ->
    #funcall{ function = #var_ref{ name = {builtin, string_concat} },
              args     = [ast_body(String1, Icode), ast_body(String2, Icode)] };

%% Other terms
ast_body({id, _, Name}, _Icode) ->
    %% TODO Look up id in env
    #var_ref{name = Name};
ast_body({bool, _, Bool}, _Icode) ->        %BOOL as ints
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
%% Typed contract calls
ast_body({proj, _, {typed, _, Addr, {con, _, _}}, {id, _, "address"}}, Icode) ->
    ast_body(Addr, Icode);  %% Values of contract types _are_ addresses.
ast_body({app, _, {typed, _, {proj, _, {typed, _, Addr, {con, _, Contract}}, {id, _, FunName}},
                             {fun_t, _, NamedT, ArgsT, OutT}}, Args0}, Icode) ->
    NamedArgs = [Arg || Arg = {named_arg, _, _, _} <- Args0],
    Args      = Args0 -- NamedArgs,
    ArgOpts   = [ {Name, ast_body(Value, Icode)}   || {named_arg,   _, {id, _, Name}, Value} <- NamedArgs ],
    Defaults  = [ {Name, ast_body(Default, Icode)} || {named_arg_t, _, {id, _, Name}, _, Default} <- NamedT ],
    %% TODO: eta expand
    [ error({underapplied_contract_call, string:join([Contract, FunName], ".")})
        || length(Args) /= length(ArgsT) ],
    ArgsI = [ ast_body(Arg, Icode) || Arg <- Args ],
    ArgType = ast_typerep({tuple_t, [], ArgsT}),
    Gas    = proplists:get_value("gas",   ArgOpts ++ Defaults),
    Value  = proplists:get_value("value", ArgOpts ++ Defaults),
    Fun    = ast_body({string, [], list_to_binary(FunName)}, Icode),
    #prim_call_contract{
        address  = ast_body(Addr, Icode),
        gas      = Gas,
        value    = Value,
        arg      = #tuple{cpts = [Fun, #tuple{ cpts = ArgsI }]},
        arg_type = {tuple, [string, ArgType]},
        out_type = ast_typerep(OutT, Icode) };
ast_body({proj, _, {typed, _, _, {con, _, Contract}}, {id, _, FunName}}, _Icode) ->
    error({underapplied_contract_call, string:join([Contract, FunName], ".")});

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

check_monomorphic_map({typed, Ann, _, MapType}, Icode) ->
    check_monomorphic_map(Ann, MapType, Icode).

check_monomorphic_map(Ann, Type = ?map_t(KeyType, ValType), Icode) ->
    case is_monomorphic(KeyType) of
        true  ->
            case aeso_data:has_maps(ast_type(KeyType, Icode)) of
                false -> {KeyType, ValType};
                true  -> error({cant_use_map_as_map_keys, Ann, Type})
            end;
        false -> error({cant_compile_map_with_polymorphic_keys, Ann, Type})
    end.

map_empty(KeyType, ValType, Icode) ->
    prim_call(?PRIM_CALL_MAP_EMPTY, #integer{value = 0},
              [ast_type_value(KeyType, Icode),
               ast_type_value(ValType, Icode)],
              [typerep, typerep], word).

map_get(Key, Map = {typed, Ann, _, MapType}, Icode) ->
    {_KeyType, ValType} = check_monomorphic_map(Ann, MapType, Icode),
    builtin_call({map_lookup, ast_type(ValType, Icode)}, [ast_body(Map, Icode), ast_body(Key, Icode)]).

map_put(Key, Val, Map, Icode) ->
    builtin_call(map_put, [ast_body(Map, Icode), ast_body(Key, Icode), ast_body(Val, Icode)]).

map_del(Key, Map, Icode) ->
    prim_call(?PRIM_CALL_MAP_DELETE, #integer{value = 0},
              [ast_body(Map, Icode), ast_body(Key, Icode)],
              [word, word], word).

map_tolist(Map, Icode) ->
    {KeyType, ValType} = check_monomorphic_map(Map, Icode),
    prim_call(?PRIM_CALL_MAP_TOLIST, #integer{value = 0},
              [ast_body(Map, Icode)],
              [word], {list, {tuple, [ast_type(KeyType, Icode), ast_type(ValType, Icode)]}}).

map_upd(Key, ValFun, Map = {typed, Ann, _, MapType}, Icode) ->
    {_, ValType} = check_monomorphic_map(Ann, MapType, Icode),
    FunName = {map_upd, ast_type(ValType, Icode)},
    Args    = [ast_body(Map, Icode), ast_body(Key, Icode), ast_body(ValFun, Icode)],
    builtin_call(FunName, Args).

is_monomorphic({tvar, _, _}) -> false;
is_monomorphic([H|T]) ->
  is_monomorphic(H) andalso is_monomorphic(T);
is_monomorphic(T) when is_tuple(T) ->
  is_monomorphic(tuple_to_list(T));
is_monomorphic(_) -> true.

%% Implemented as a contract call to the contract with address 0.
prim_call(Prim, Amount, Args, ArgTypes, OutType) ->
    #prim_call_contract{ gas      = prim_gas_left,
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
ast_typerep({qid, _, Name}, Icode) ->
    lookup_type_id(Name, [], Icode);
ast_typerep({con, _, _}, _) ->
    word;   %% Contract type
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
ast_typerep({fun_t,_,_,_,_}, _Icode) ->
    function;
ast_typerep({alias_t, T}, Icode) -> ast_typerep(T, Icode);
ast_typerep({variant_t, Cons}, Icode) ->
    {variant, [ begin
                  {constr_t, _, _, Args} = Con,
                  [ ast_typerep(Arg, Icode) || Arg <- Args ]
                end || Con <- Cons ]}.

ttl_t(Icode) ->
    ast_typerep({qid, [], ["Chain", "ttl"]}, Icode).

sign_t() ->
    {tuple, [word, word]}.

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
type_value(typerep) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_TYPEREP_TAG }] };
type_value({list, A}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_LIST_TAG }, type_value(A)] };
type_value({tuple, As}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_TUPLE_TAG },
                    #list{ elems = [ type_value(A) || A <- As ] }] };
type_value({variant, Cs}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_VARIANT_TAG },
                    #list{ elems = [ #list{ elems = [ type_value(A) || A <- As ] } || As <- Cs ] }] };
type_value({map, K, V}) ->
    #tuple{ cpts = [#integer{ value = ?TYPEREP_MAP_TAG },
                    type_value(K), type_value(V)] }.

%% As abort is a built-in in the future it will be illegal to for
%% users to define abort. For the time being strip away all user
%% defined abort functions.

ast_fun_to_icode("abort", _Atts, _Args, _Body, _TypeRep, Icode) ->
    %% Strip away all user defined abort functions.
    Icode;
ast_fun_to_icode(Name, Attrs, Args, Body, TypeRep, #{functions := Funs} = Icode) ->
    NewFuns = [{Name, Attrs, Args, Body, TypeRep}| Funs],
    aeso_icode:set_functions(NewFuns, Icode).

%% -------------------------------------------------------------------
%% Builtins
%% -------------------------------------------------------------------

builtin_call(Builtin, Args) ->
    #funcall{ function = #var_ref{ name = {builtin, Builtin} },
              args = Args }.

builtin_deps(Builtin) ->
    lists:usort(builtin_deps1(Builtin)).

builtin_deps1({map_lookup_default, Type}) -> [{map_lookup, Type}];
builtin_deps1({map_get, Type})            -> [{map_lookup, Type}];
builtin_deps1(map_member)                 -> [{map_lookup, word}];
builtin_deps1({map_upd, Type})            -> [{map_lookup, Type}, map_put];
builtin_deps1(map_from_list)              -> [map_put];
builtin_deps1(str_equal)                  -> [str_equal_p];
builtin_deps1(string_concat)              -> [string_concat_inner1, string_concat_inner2];
builtin_deps1(_)                          -> [].

dep_closure(Deps) ->
    case lists:umerge(lists:map(fun builtin_deps/1, Deps)) of
        []    -> Deps;
        Deps1 -> lists:umerge(Deps, dep_closure(Deps1))
    end.

used_builtins(#funcall{ function = #var_ref{ name = {builtin, Builtin} }, args = Args }) ->
    lists:umerge(dep_closure([Builtin]), used_builtins(Args));
used_builtins([H|T]) ->
  lists:umerge(used_builtins(H), used_builtins(T));
used_builtins(T) when is_tuple(T) ->
  used_builtins(tuple_to_list(T));
used_builtins(M) when is_map(M) ->
  used_builtins(maps:to_list(M));
used_builtins(_) -> [].

option_none()  -> {tuple, [{integer, 0}]}.
option_some(X) -> {tuple, [{integer, 1}, X]}.

v(X) when is_atom(X) -> v(atom_to_list(X));
v(X) when is_list(X) -> #var_ref{name = X}.

%% Abort primitive.
builtin_function(abort) ->
    A = fun(X) -> aeb_opcodes:mnemonic(X) end,
    {{builtin, abort}, [private],
     [{"s", string}],
     %% length, address, REVERT
     {inline_asm, [A(?DUP1),
                   A(?MLOAD),A(?SWAP1),         %Load size and swap
                   A(?PUSH1),32,A(?ADD),        %Load address of string binary
                   A(?REVERT)]},
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
            {switch, builtin_call({map_lookup, Type}, [v(m), v(k)]),
                [{option_some(v(v)), v(v)}]},
     Type};

builtin_function(Builtin = {map_lookup_default, Type}) ->
    %% function map_lookup_default(m, k, default) =
    %%   switch(map_lookup(m, k))
    %%     None    => default
    %%     Some(v) => v
    {{builtin, Builtin}, [private],
        [{"m", word}, {"k", word}, {"default", Type}],
            {switch, builtin_call({map_lookup, Type}, [v(m), v(k)]),
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
            {switch, builtin_call({map_lookup, word}, [v(m), v(k)]),
                [{option_none(), {integer, 0}},
                 {{var_ref, "_"}, {integer, 1}}]},
     word};

builtin_function(Builtin = {map_upd, Type}) ->
    %% function map_upd(map, key, fun) =
    %%   map_put(map, key, fun(map_get(map, key)))
    {{builtin, Builtin}, [private],
     [{"map", word}, {"key", word}, {"valfun", word}],
     builtin_call(map_put,
        [v(map), v(key),
         #funcall{ function = v(valfun),
                   args     = [builtin_call({map_get, Type}, [v(map), v(key)])] }]),
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
          builtin_call(map_from_list,
            [v(ys), builtin_call(map_put, [v(acc), v(k), v(v)])])}]},
     word};

builtin_function(string_length) ->
    %% function length(str) =
    %%   switch(str)
    %%      {n} -> n  // (ab)use the representation
    {{builtin, string_length}, [private],
     [{"s", string}],
     {switch, v(s), [{{tuple, [v(n)]}, v(n)}]},
     word};

%% str_concat - concatenate two strings
%%
%% Unless the second string is the empty string, a new string is created at the
%% top of the Heap and the address to it is returned. The tricky bit is when
%% the words from the second string has to be shifted to fit next to the first
%% string.
builtin_function(string_concat) ->
    I = fun(X) -> {integer, X} end,
    LetLen = fun(N, S, Body) -> {switch, v(S), [{{tuple, [v(N)]}, Body}]} end,
    Let = fun(N, E, Body) -> {switch, E, [{v(N), Body}]} end,
    StepPtr = fun(P) -> {binop, '+', v(P), I(32)} end,
    A = fun(X) -> aeb_opcodes:mnemonic(X) end,
    {{builtin, string_concat}, [private],
     [{"s1", string}, {"s2", string}],
     LetLen(n1, s1,
     LetLen(n2, s2,
        {ifte, {binop, '==', v(n2), I(0)},
            v(s1), %% Second string is empty return first string
            Let(ret, {inline_asm, [A(?MSIZE)]},
                {seq, [{binop, '+', v(n1), v(n2)},
                       {inline_asm, [A(?MSIZE), A(?MSTORE)]}, %% Store total len
                       {funcall, {var_ref, {builtin, string_concat_inner1}},
                            [v(n1), StepPtr(s1), v(n2), StepPtr(s2)]},
                       {inline_asm, [A(?POP)]}, %% Discard fun ret val
                       v(ret)                   %% Put the actual return value
                      ]})})),
     word};

builtin_function(string_concat_inner1) ->
    I = fun(X) -> {integer, X} end,
    Name = {builtin, string_concat_inner1},
    LetWord = fun(W, P, Body) -> {switch, v(P), [{{tuple, [v(W)]}, Body}]} end,
    A = fun(X) -> aeb_opcodes:mnemonic(X) end,
    %% Copy all whole words from the first string, and set up for word fusion
    %% Special case when the length of the first string is divisible by 32.
    {Name, [private],
     [{"n1", word}, {"p1", pointer}, {"n2", word}, {"p2", pointer}],
     LetWord(w1, p1,
        {ifte, {binop, '>', v(n1), I(32)},
            {seq, [v(w1), {inline_asm, [A(?MSIZE), A(?MSTORE)]},
                   {funcall, {var_ref, Name}, [{binop, '-', v(n1), I(32)},
                                               {binop, '+', v(p1), I(32)},
                                               v(n2), v(p2)]}]},
            {ifte, {binop, '==', v(n1), I(0)},
                {funcall, {var_ref, {builtin, string_concat_inner2}},
                          [I(32), I(0), v(n2), v(p2)]},
                {funcall, {var_ref, {builtin, string_concat_inner2}},
                          [{binop, '-', I(32), v(n1)}, v(w1), v(n2), v(p2)]}}
        }),
     word};

builtin_function(string_concat_inner2) ->
    I = fun(X) -> {integer, X} end,
    LetWord = fun(W, P, Body) -> {switch, v(P), [{{tuple, [v(W)]}, Body}]} end,
    BSR = fun(X, Bytes) -> {binop, 'div', X, {binop, '^', I(2), {binop, '*', Bytes, I(8)}}} end,
    BSL = fun(X, Bytes) -> {binop, '*', X, {binop, '^', I(2), {binop, '*', Bytes, I(8)}}} end,
    A = fun(X) -> aeb_opcodes:mnemonic(X) end,
    Name = {builtin, string_concat_inner2},
    %% Current "work in progess" word 'x', has 'o' bytes that are "free" - fill them from
    %% words of the second string.
    {Name, [private],
     [{"o", word}, {"x", word}, {"n2", word}, {"p2", pointer}],
     {ifte, {binop, '<', v(n2), I(1)},
        {seq, [v(x), {inline_asm, [A(?MSIZE), A(?MSTORE), A(?MSIZE)]}]}, %% Use MSIZE as dummy return value
        LetWord(w2, p2,
            {ifte, {binop, '>', v(n2), v(o)},
                {seq, [{binop, '+', v(x), BSR(v(w2), {binop, '-', I(32), v(o)})},
                       {inline_asm, [A(?MSIZE), A(?MSTORE)]},
                       {funcall, {var_ref, Name},
                                [v(o), BSL(v(w2), v(o)), {binop, '-', v(n2), I(32)}, {binop, '+', v(p2), I(32)}]}
                      ]},
                {seq, [{binop, '+', v(x), BSR(v(w2), {binop, '-', I(32), v(o)})},
                       {inline_asm, [A(?MSIZE), A(?MSTORE), A(?MSIZE)]}]} %% Use MSIZE as dummy return value
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
    LetWord = fun(W, P, Body) -> {switch, v(P), [{{tuple, [v(W)]}, Body}]} end,
    Name = {builtin, str_equal_p},
    {Name, [private],
        [{"n", word}, {"p1", pointer}, {"p2", pointer}],
        {ifte, {binop, '<', v(n), {integer, 1}},
            {integer, 1},
            LetWord(w1, p1,
            LetWord(w2, p2,
                {binop, '&&', {binop, '==', v(w1), v(w2)},
                    {funcall, {var_ref, Name},
                        [{binop, '-', v(n), {integer, 32}},
                         {binop, '+', v(p1), {integer, 32}},
                         {binop, '+', v(p2), {integer, 32}}]}}))},
     word};

builtin_function(str_equal) ->
    %% function str_equal(s1, s2) =
    %%   let n1 = length(s1)
    %%   let n2 = length(s2)
    %%   n1 == n2 && str_equal_p(n1, s1 + 32, s2 + 32)
    LetLen = fun(N, S, Body) -> {switch, v(S), [{{tuple, [v(N)]}, Body}]} end,
    {{builtin, str_equal}, [private],
        [{"s1", string}, {"s2", string}],
        LetLen(n1, s1,
        LetLen(n2, s2,
            {binop, '&&', {binop, '==', v(n1), v(n2)},
                {funcall, {var_ref, {builtin, str_equal_p}},
                    [v(n1), {binop, '+', v(s1), {integer, 32}},
                            {binop, '+', v(s2), {integer, 32}}]}})),
        word}.

add_builtins(Icode = #{functions := Funs}) ->
    Builtins = used_builtins(Funs),
    Icode#{functions := [ builtin_function(B) || B <- Builtins ] ++ Funs}.
