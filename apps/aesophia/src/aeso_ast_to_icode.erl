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

-export([convert/2]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aeso_icode.hrl").

convert(Tree, Options) ->
    TypedTree = aeso_ast_infer_types:infer(Tree),
    [io:format("Typed tree:\n~s\n",[prettypr:format(aeso_pretty:decls(TypedTree))]) || lists:member(pp_typed,Options)],
    %% [io:format("Typed tree:\n~p\n",[TypedTree]) || lists:member(pp_typed,Options)],
    code(TypedTree,
%%    code(Tree,
	 #{ functions => []
	  , env => []
          , state_type => {tuple, []}   %% Default to unit type for state
	  , options => Options}).

code([{contract, _Attribs, {con, _, Name}, Code}|Rest], Icode) ->
    NewIcode = contract_to_icode(Code, set_name(Name, Icode)),
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

contract_to_icode([{type_def, _Attrib, {id, _, "state"}, _, TypeDef}|Rest], Icode) ->
    StateType =
        case TypeDef of
            {alias_t, T}   -> ast_typerep(T);
            {record_t, _}  -> ast_typerep(TypeDef);
            {variant_t, _} -> error({not_supported, variant_state_types, TypeDef})
        end,
    contract_to_icode(Rest, Icode#{ state_type => StateType});
contract_to_icode([{type_def, _Attrib, _, _, _}|Rest], Icode) ->
    %% TODO: Handle types
    contract_to_icode(Rest, Icode);
contract_to_icode([{letfun,_Attrib, Name, Args, _What, Body={typed,_,_,T}}|Rest], Icode) ->
    %% TODO: Handle types
    FunName = ast_id(Name),
    %% TODO: push funname to env
    FunArgs = ast_args(Args, []),
    %% TODO: push args to env
    FunBody = ast_body(Body),
    TypeRep = ast_typerep(T),
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

ast_args([{arg, _, Name, Type}|Rest], Acc) ->
    ast_args(Rest, [{ast_id(Name), ast_type(Type)}| Acc]);
ast_args([], Acc) -> lists:reverse(Acc).

ast_type(T) ->
    ast_typerep(T).

-define(id_app(Fun, Args, ArgTypes, OutType),
    {app, _, {typed, _, {id, _, Fun}, {fun_t, _, ArgTypes, OutType}}, Args}).

-define(qid_app(Fun, Args, ArgTypes, OutType),
    {app, _, {typed, _, {qid, _, Fun}, {fun_t, _, ArgTypes, OutType}}, Args}).

-define(oracle_t(Q, R), {app_t, _, {id, _, "oracle"}, [Q, R]}).
-define(query_t(Q, R),  {app_t, _, {id, _, "oracle_query"}, [Q, R]}).
-define(option_t(A),    {app_t, _, {id, _, "option"}, [A]}).
-define(map_t(K, V),    {app_t, _, {id, _, "map"}, [K, V]}).

ast_body(?id_app("raw_call", [To, Fun, Gas, Value, {typed, _, Arg, ArgT}], _, OutT)) ->
    %% TODO: temp hack before we have contract calls properly in the type checker
    {Args, ArgTypes} =
        case Arg of %% Hack: unpack tuples
            {tuple, _, Elems} ->
                {tuple_t, _, Ts} = ArgT,
                {Elems, Ts};
            _ -> {[Arg], [ArgT]}
        end,
    #prim_call_contract{ gas      = ast_body(Gas),
                         address  = ast_body(To),
                         value    = ast_body(Value),
                         arg      = #tuple{cpts = [ast_body(X) || X <- [Fun | Args]]},
                         arg_type = {tuple, [string | lists:map(fun ast_typerep/1, ArgTypes)]},
                         out_type = ast_typerep(OutT) };
ast_body(?id_app("raw_spend", [To, Amount], _, _)) ->
    prim_call(?PRIM_CALL_SPEND, ast_body(Amount), [ast_body(To)], [word], {tuple, []});

%% Chain environment
ast_body(?qid_app(["Chain", "balance"], [Address], _, _)) ->
    #prim_balance{ address = ast_body(Address) };
ast_body(?qid_app(["Chain", "block_hash"], [Height], _, _)) ->
    #prim_block_hash{ height = ast_body(Height) };
ast_body({qid, _, ["Contract", "address"]})      -> prim_contract_address;
ast_body({qid, _, ["Contract", "balance"]})      -> #prim_balance{ address = prim_contract_address };
ast_body({qid, _, ["Call",     "origin"]})       -> prim_call_origin;
ast_body({qid, _, ["Call",     "caller"]})       -> prim_caller;
ast_body({qid, _, ["Call",     "value"]})        -> prim_call_value;
ast_body({qid, _, ["Call",     "gas_price"]})    -> prim_gas_price;
ast_body({qid, _, ["Chain",    "coinbase"]})     -> prim_coinbase;
ast_body({qid, _, ["Chain",    "timestamp"]})    -> prim_timestamp;
ast_body({qid, _, ["Chain",    "block_height"]}) -> prim_block_height;
ast_body({qid, _, ["Chain",    "difficulty"]})   -> prim_difficulty;
ast_body({qid, _, ["Chain",    "gas_limit"]})    -> prim_gas_limit;
%% TODO: eta expand!
ast_body({qid, _, ["Chain", "balance"]}) ->
    error({underapplied_primitive, 'Chain.balance'});
ast_body({qid, _, ["Chain", "block_hash"]}) ->
    error({underapplied_primitive, 'Chain.block_hash'});
ast_body({id, _, "raw_call"}) ->
    error({underapplied_primitive, raw_call});
ast_body({id, _, "raw_spend"}) ->
    error({underapplied_primitive, raw_spend});

%% State
ast_body({id, _, "state"}) -> prim_state;
ast_body(?id_app("put", [NewState], _, _)) ->
    #prim_put{ state = ast_body(NewState) };
ast_body({id, _, "put"}) ->
    error({underapplied_primitive, put});   %% TODO: eta

%% Oracles
ast_body(?qid_app(["Oracle", "register"], [Acct, Sign, Fee, QFee, TTL], _, ?oracle_t(QType, RType))) ->
    prim_call(?PRIM_CALL_ORACLE_REGISTER, ast_body(Fee),
              [ast_body(Acct), ast_body(Sign), ast_body(QFee), ast_body(TTL),
               ast_type_value(QType), ast_type_value(RType)],
              [word, word, word, word, typerep, typerep], word);

ast_body(?qid_app(["Oracle", "query_fee"], [Oracle], _, _)) ->
    prim_call(?PRIM_CALL_ORACLE_QUERY_FEE, #integer{value = 0},
              [ast_body(Oracle)], [word], word);

ast_body(?qid_app(["Oracle", "query"], [Oracle, Q, Fee, QTTL, RTTL], [_, QType, _, _, _], _)) ->
    prim_call(?PRIM_CALL_ORACLE_QUERY, ast_body(Fee),
              [ast_body(Oracle), ast_body(Q), ast_body(QTTL), ast_body(RTTL)],
              [word, ast_type(QType), word, word], word);

ast_body(?qid_app(["Oracle", "extend"], [Oracle, Sign, Fee, TTL], _, _)) ->
    prim_call(?PRIM_CALL_ORACLE_EXTEND, #integer{value = 0},
              [ast_body(Oracle), ast_body(Sign), ast_body(Fee), ast_body(TTL)],
              [word, word, word, word], {tuple, []});

ast_body(?qid_app(["Oracle", "respond"], [Oracle, Query, Sign, R], [_, _, _, RType], _)) ->
    prim_call(?PRIM_CALL_ORACLE_RESPOND, #integer{value = 0},
              [ast_body(Oracle), ast_body(Query), ast_body(Sign), ast_body(R)],
              [word, word, word, ast_type(RType)], {tuple, []});

ast_body(?qid_app(["Oracle", "get_question"], [Oracle, Q], [_, ?query_t(QType, _)], _)) ->
    prim_call(?PRIM_CALL_ORACLE_GET_QUESTION, #integer{value = 0},
              [ast_body(Oracle), ast_body(Q)], [word, word], ast_type(QType));

ast_body(?qid_app(["Oracle", "get_answer"], [Oracle, Q], [_, ?query_t(_, RType)], _)) ->
    prim_call(?PRIM_CALL_ORACLE_GET_ANSWER, #integer{value = 0},
              [ast_body(Oracle), ast_body(Q)], [word, word], {option, ast_type(RType)});

ast_body({qid, _, ["Oracle", "register"]})     -> error({underapplied_primitive, 'Oracle.register'});
ast_body({qid, _, ["Oracle", "query"]})        -> error({underapplied_primitive, 'Oracle.query'});
ast_body({qid, _, ["Oracle", "extend"]})       -> error({underapplied_primitive, 'Oracle.extend'});
ast_body({qid, _, ["Oracle", "respond"]})      -> error({underapplied_primitive, 'Oracle.respond'});
ast_body({qid, _, ["Oracle", "query_fee"]})    -> error({underapplied_primitive, 'Oracle.query_fee'});
ast_body({qid, _, ["Oracle", "get_answer"]})   -> error({underapplied_primitive, 'Oracle.get_answer'});
ast_body({qid, _, ["Oracle", "get_question"]}) -> error({underapplied_primitive, 'Oracle.get_question'});

%% Maps
ast_body({map_get, _, Map, {typed, _, Key, KeyType}}) ->
    Fun = {map_get, key_type(KeyType)},
    #funcall{ function = #var_ref{name = {builtin, Fun}},
              args     = [ast_body(Map), ast_body(Key)] };
ast_body(?qid_app(["Map", "from_list"], [List], _, _)) ->
    ast_body(List);
ast_body({qid, _, ["Map", "from_list"]}) ->
    error({underapplied_primitive, 'Map.from_list'});

ast_body({map, Ann, KVs}) ->
    ast_body({list, Ann, [{tuple, Ann, [K, V]} || {K, V} <- KVs]});

ast_body({map, _, Map, []}) -> ast_body(Map);
ast_body({map, _, Map, [Upd]}) ->
    %% TODO: nested updates (desugar those earlier?)
    {Fun, Args} = case Upd of
            {field, _, [{map_get, _, {typed, _, Key, KeyType}}], Val} ->
                {{map_put, key_type(KeyType)}, [ast_body(Key), ast_body(Val)]};
            {field_upd, _, [{map_get, _, {typed, _, Key, KeyType}}], ValFun} ->
                {{map_upd, key_type(KeyType)}, [ast_body(Key), ast_body(ValFun)]}
        end,
    #funcall{ function = #var_ref{name = {builtin, Fun}},
              args     = [ast_body(Map) | Args] };

ast_body({map, Ann, Map, [Upd | Upds]}) ->
    ast_body({map, Ann, {map, Ann, Map, [Upd]}, Upds});

%% Other terms
ast_body({id, _, Name}) ->
    %% TODO Look up id in env
    #var_ref{name = Name};
ast_body({bool, _, Bool}) ->		%BOOL as ints
    Value = if Bool -> 1 ; true -> 0 end,
    #integer{value = Value};
ast_body({int, _, Value}) ->
    #integer{value = Value};
ast_body({string,_,Bin}) ->
    Cpts = [size(Bin)|aeso_data:binary_to_words(Bin)],
    #tuple{cpts = [#integer{value=X} || X <- Cpts]};
ast_body({tuple,_,Args}) ->
    #tuple{cpts = [ast_body(A) || A <- Args]};
ast_body({list,_,Args}) ->
    #list{elems = [ast_body(A) || A <- Args]};
%% Hardwired option types. TODO: Remove when we have arbitrary variant types.
ast_body({con, _, "None"}) ->
    #list{elems = []};
ast_body({app, _, {typed, _, {con, _, "Some"}, _}, [Elem]}) ->
    #tuple{cpts = [ast_body(Elem)]};
ast_body({typed, _, {con, _, "Some"}, {fun_t, _, [A], _}}) ->
    #lambda{ args = [#arg{name = "x", type = ast_type(A)}]
           , body = #tuple{cpts = [#var_ref{name = "x"}]} };
ast_body({app,As,Fun,Args}) ->
    case aeso_syntax:get_ann(format, As) of
        infix  ->
            {Op, _} = Fun,
            [A, B]  = Args,
            #binop{op = Op, left = ast_body(A), right = ast_body(B)};
        prefix ->
            {Op, _} = Fun,
            [A]     = Args,
            #unop{op = Op, rand = ast_body(A)};
        _ ->
            #funcall{function=ast_body(Fun),
	             args=[ast_body(A) || A <- Args]}
    end;
ast_body({'if',_,Dec,Then,Else}) ->
    #ifte{decision = ast_body(Dec)
	 ,then     = ast_body(Then)
	 ,else     = ast_body(Else)};
ast_body({switch,_,A,Cases}) ->
    %% let's assume the parser has already ensured that only valid
    %% patterns appear in cases.
    #switch{expr=ast_body(A),
	    cases=[{ast_body(Pat),ast_body(Body)}
		   || {'case',_,Pat,Body} <- Cases]};
ast_body({block,As,[{letval,_,Pat,_,E}|Rest]}) ->
    #switch{expr=ast_body(E),
	    cases=[{ast_body(Pat),ast_body({block,As,Rest})}]};
ast_body({block,_,[]}) ->
    #tuple{cpts=[]};
ast_body({block,_,[E]}) ->
    ast_body(E);
ast_body({block,As,[E|Rest]}) ->
    #switch{expr=ast_body(E),
	    cases=[{#var_ref{name="_"},ast_body({block,As,Rest})}]};
ast_body({lam,_,Args,Body}) ->
    #lambda{args=[#arg{name = ast_id(P), type = ast_type(T)} || {arg,_,P,T} <- Args],
	    body=ast_body(Body)};
ast_body({typed,_,{record,Attrs,Fields},{record_t,DefFields}}) ->
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
			ast_body(E)
		end
		|| {field_t,_,_,{id,_,Name},_} <- DefFields]};
ast_body({typed,_,{record,Attrs,_Fields},T}) ->
    error({record_has_bad_type,Attrs,T});
ast_body({proj,_,{typed,_,Record,{record_t,Fields}},{id,_,FieldName}}) ->
    [Index] = [I
	       || {I,{field_t,_,_,{id,_,Name},_}} <-
		      lists:zip(lists:seq(1,length(Fields)),Fields),
		  Name==FieldName],
    #binop{op = '!', left = #integer{value = 32*(Index-1)}, right = ast_body(Record)};
ast_body({record, Attrs, {typed, _, Record, RecType={record_t, Fields}}, Update}) ->
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

    #switch{expr=ast_body(Record),
	    cases=[{#var_ref{name = "_record"},
		    ast_body({typed, Attrs,
			      {record, Attrs,
			       lists:map(CompileUpdate, Update) ++
				[{field, Attrs, [{proj, Attrs, {id, Attrs, Name}}],
				     {proj, Attrs, Rec, {id, Attrs, Name}}}
				    || {field_t, _, _, {id, _, Name}, _} <- Fields,
				       not lists:member(Name, UpdatedNames)]},
			      RecType})}
		   ]};
ast_body({typed, _, Body, _}) ->
    ast_body(Body).

%% Implemented as a contract call to the contract with address 0.
prim_call(Prim, Amount, Args, ArgTypes, OutType) ->
    #prim_call_contract{ gas      = #integer{ value = 0 },
                         address  = #integer{ value = 0 },
                         value    = Amount,
                         arg      = #tuple{cpts = [#integer{ value = Prim } | Args]},
                         arg_type = {tuple, [word | ArgTypes]},
                         out_type = OutType }.

ast_typerep({id,_,"bool"}) ->		%BOOL as ints
    word;
ast_typerep({id,_,"int"}) ->
    word;
ast_typerep({id,_,"string"}) ->
    string;
ast_typerep({id,_,"address"}) ->
    word;
ast_typerep(?oracle_t(_, _)) ->
    word;
ast_typerep(?query_t(_, _)) ->
    word;
ast_typerep(?option_t(A)) ->
    {option, ast_typerep(A)};   %% For now, but needs to be generalised to arbitrary datatypes later.
ast_typerep(?map_t(K, V)) ->
    map_typerep(ast_typerep(K), ast_typerep(V));
ast_typerep({tvar,_,_}) ->
    %% We serialize type variables just as addresses in the originating VM.
    word;
ast_typerep({tuple_t,_,Cpts}) ->
    {tuple, [ast_typerep(C) || C<-Cpts]};
ast_typerep({record_t,Fields}) ->
    {tuple, [ast_typerep(T) || {field_t,_,_,_,T} <- Fields]};
ast_typerep({app_t,_,{id,_,"list"},[Elem]}) ->
    {list, ast_typerep(Elem)};
ast_typerep({fun_t,_,_,_}) ->
    function.

map_typerep(K, V) ->
    {list, {tuple, [K, V]}}.  %% Lists of key-value pairs for now

ast_type_value(T) ->
    type_value(ast_type(T)).

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
                    #list{ elems = [ type_value(A) || A <- As ] }] }.

ast_fun_to_icode(Name, Args, Body, TypeRep, #{functions := Funs} = Icode) ->
    NewFuns = [{Name, Args, Body, TypeRep}| Funs],
    set_functions(NewFuns, Icode).

%% -------------------------------------------------------------------
%% Icode
%% -------------------------------------------------------------------
set_name(Name, Icode) ->
    maps:put(contract_name, Name, Icode).

%% get_name(#{contract_name := Name}) -> Name;
%% get_name(_) -> error(name_not_defined).

set_functions(NewFuns, Icode) ->
    maps:put(functions, NewFuns, Icode).

%% -------------------------------------------------------------------
%% Builtins
%% -------------------------------------------------------------------

key_type(Type) ->
    case ast_typerep(Type) of
        word   -> word;
        string -> string;
        _      -> error({unsupported_key_type, Type})
    end.

builtin_deps({map_get, string}) -> [str_equal];
builtin_deps({map_put, string}) -> [str_equal];
builtin_deps({map_upd, string}) -> [str_equal];
builtin_deps(_) -> [].

used_builtins(#funcall{ function = #var_ref{ name = {builtin, Builtin} } }) ->
    lists:umerge([Builtin], builtin_deps(Builtin));
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
    Eq = fun(A, B) -> builtin_eq(Type, A, B) end,
    Name = {builtin, Builtin},
    {Name,
        [{"m", map_typerep(Type, word)}, {"k", Type}],
        {switch, {var_ref, "m"},
            [{{binop, '::',
                {tuple, [{var_ref, "k'"}, {var_ref, "v"}]},
                {var_ref, "m'"}},
              {ifte, Eq({var_ref, "k"}, {var_ref, "k'"}),
                {var_ref, "v"},
                {funcall, {var_ref, Name},
                    [{var_ref, "m'"}, {var_ref, "k"}]}}}]},
        word};

builtin_function(Builtin = {map_put, Type}) ->
    Eq = fun(A, B) -> builtin_eq(Type, A, B) end,
    Name = {builtin, Builtin},
    {Name,
     [{"map", {list, {tuple, [word, word]}}}, {"key", word}, {"val", word}],
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
     {list, {tuple, [Type, word]}}};

builtin_function(Builtin = {map_upd, Type}) ->
    Eq = fun(A, B) -> builtin_eq(Type, A, B) end,
    Name = {builtin, Builtin},
    {Name,
     [{"map", {list, {tuple, [word, word]}}}, {"key", word}, {"fun", word}],
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
     {list, {tuple, [Type, word]}}};

builtin_function(str_equal) ->
    V = fun(X) -> {var_ref, atom_to_list(X)} end,
    P = fun(A, B) -> {tuple, [A, B]} end,
    {{builtin, str_equal},
        [{"s1", string}, {"s2", string}],
        {switch, P(V(s1), V(s2)),
        [{P(P(V(n1), V(w1)), P(V(n2), V(w2))),
            {binop, '&&',
                {binop, '==', V(n1), V(n2)},
                {binop, '==', V(w1), V(w2)}}}]},  %% TODO: only compares first 32 bytes!
        word}.

add_builtins(Icode = #{functions := Funs}) ->
    Builtins = used_builtins(Funs),
    Icode#{functions := [ builtin_function(B) || B <- Builtins ] ++ Funs}.

