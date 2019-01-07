%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Compiler from Aeterinty Sophia language to the Aeternity VM, aevm.
%%% @end
%%% Created : 12 Dec 2017
%%%-------------------------------------------------------------------
-module(aeso_compiler).

-export([ file/1
        , file/2
        , from_string/2
        , check_call/2
        , create_calldata/3
        , version/0
        , sophia_type_to_typerep/1
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aeso_icode.hrl").


-type option() :: pp_sophia_code | pp_ast | pp_icode | pp_assembler |
                  pp_bytecode.
-type options() :: [option()].

-export_type([ option/0
             , options/0
             ]).

-define(COMPILER_VERSION_1, 1).
-define(COMPILER_VERSION_2, 2).

-define(COMPILER_VERSION, ?COMPILER_VERSION_2).

-spec version() -> pos_integer().
version() ->
    ?COMPILER_VERSION.

-spec file(string()) -> map().
file(Filename) ->
    file(Filename, []).

-spec file(string(), options()) -> map().
file(Filename, Options) ->
    C = read_contract(Filename),
    from_string(C, Options).

-spec from_string(string(), options()) -> map().
from_string(ContractString, Options) ->
    Ast = parse(ContractString, Options),
    ok = pp_sophia_code(Ast, Options),
    ok = pp_ast(Ast, Options),
    TypedAst = aeso_ast_infer_types:infer(Ast, Options),
    %% pp_types is handled inside aeso_ast_infer_types.
    ok = pp_typed_ast(TypedAst, Options),
    ICode = to_icode(TypedAst, Options),
    TypeInfo = extract_type_info(ICode),
    ok = pp_icode(ICode, Options),
    Assembler =  assemble(ICode, Options),
    ok = pp_assembler(Assembler, Options),
    ByteCodeList = to_bytecode(Assembler, Options),
    ByteCode = << << B:8 >> || B <- ByteCodeList >>,
    ok = pp_bytecode(ByteCode, Options),
    #{byte_code => ByteCode, type_info => TypeInfo,
      contract_source => ContractString,
      compiler_version => version()}.

-define(CALL_NAME, "__call").

%% Takes a string containing a contract with a declaration/prototype of a
%% function (foo, say) and a function __call() = foo(args) calling this
%% function. Returns the name of the called functions, typereps and Erlang
%% terms for the arguments.
-spec check_call(string(), options()) -> {ok, string(), {[Type], Type | any}, [term()]} | {error, term()}
    when Type :: term().
check_call(ContractString, Options) ->
    Ast = parse(ContractString, Options),
    ok = pp_sophia_code(Ast, Options),
    ok = pp_ast(Ast, Options),
    TypedAst = aeso_ast_infer_types:infer(Ast, [permissive_address_literals]),
    {ok, {FunName, {fun_t, _, _, ArgTypes, RetType}}} = get_call_type(TypedAst),
    ok = pp_typed_ast(TypedAst, Options),
    Icode = to_icode(TypedAst, Options),
    ArgVMTypes = [ aeso_ast_to_icode:ast_typerep(T, Icode) || T <- ArgTypes ],
    RetVMType  = case RetType of
                    {id, _, "_"} -> any;
                    _            -> aeso_ast_to_icode:ast_typerep(RetType, Icode)
                end,
    ok = pp_icode(Icode, Options),
    #{ functions := Funs } = Icode,
    ArgIcode = get_arg_icode(Funs),
    try [ icode_to_term(T, Arg) || {T, Arg} <- lists:zip(ArgVMTypes, ArgIcode) ] of
        ArgTerms ->
            {ok, FunName, {ArgVMTypes, RetVMType}, ArgTerms}
    catch throw:Err ->
        {error, Err}
    end.

-spec create_calldata(map(), string(), string()) ->
                             {ok, aeso_sophia:heap(), aeso_sophia:type(), aeso_sophia:type()}
                                 | {error, argument_syntax_error}.
create_calldata(Contract, "", CallCode) when is_map(Contract) ->
    case check_call(CallCode, []) of
        {ok, FunName, {ArgTypes, RetType}, Args} ->
            aeso_abi:create_calldata(Contract, FunName, Args, ArgTypes, RetType);
        {error, _} = Err -> Err
    end;
create_calldata(Contract, Function, Argument) when is_map(Contract) ->
    %% Slightly hacky shortcut to let you get away without writing the full
    %% call contract code.
    %% Function should be "foo : type", and
    %% Argument should be "Arg1, Arg2, .., ArgN" (no parens)
    case string:lexemes(Function, ": ") of
                     %% If function is a single word fallback to old calldata generation
        [FunName] -> aeso_abi:old_create_calldata(Contract, FunName, Argument);
        [FunName | _] ->
            Args    = lists:map(fun($\n) -> 32; (X) -> X end, Argument),    %% newline to space
            CallContract = lists:flatten(
                [ "contract Call =\n"
                , "  function ", Function, "\n"
                , "  function __call() = ", FunName, "(", Args, ")"
                ]),
            create_calldata(Contract, "", CallContract)
    end.


get_arg_icode(Funs) ->
    [Args] = [ Args || {?CALL_NAME, _, _, {funcall, _, Args}, _} <- Funs ],
    Args.

get_call_type([{contract, _, _, Defs}]) ->
    case [ {FunName, FunType}
          || {letfun, _, {id, _, ?CALL_NAME}, [], _Ret,
                {typed, _,
                    {app, _,
                        {typed, _, {id, _, FunName}, FunType}, _}, _}} <- Defs ] of
        [Call] -> {ok, Call};
        []     -> {error, missing_call_function}
    end;
get_call_type([_ | Contracts]) ->
    %% The __call should be in the final contract
    get_call_type(Contracts).

%% Translate an icode value (error if not value) to an Erlang term that can be
%% consumed by aeso_heap:to_binary().
icode_to_term(word, {integer, N}) -> N;
icode_to_term(string, {tuple, [{integer, Len} | Words]}) ->
    <<Str:Len/binary, _/binary>> = << <<W:256>> || {integer, W} <- Words >>,
    Str;
icode_to_term({list, T}, {list, Vs}) ->
    [ icode_to_term(T, V) || V <- Vs ];
icode_to_term({tuple, Ts}, {tuple, Vs}) ->
    list_to_tuple(icodes_to_terms(Ts, Vs));
icode_to_term({variant, Cs}, {tuple, [{integer, Tag} | Args]}) ->
    Ts = lists:nth(Tag + 1, Cs),
    {variant, Tag, icodes_to_terms(Ts, Args)};
icode_to_term(T = {map, KT, VT}, M) ->
    %% Maps are compiled to builtin and primop calls, so this gets a little hairy
    case M of
        {funcall, {var_ref, {builtin, map_put}}, [M1, K, V]} ->
            Map = icode_to_term(T, M1),
            Key = icode_to_term(KT, K),
            Val = icode_to_term(VT, V),
            Map#{ Key => Val };
        #prim_call_contract{ address = {integer, 0},
                             arg = {tuple, [{integer, ?PRIM_CALL_MAP_EMPTY}, _, _]} } ->
            #{};
        _ -> throw({todo, M})
    end;
icode_to_term(typerep, _) ->
    throw({todo, typerep});
icode_to_term(T, V) ->
    throw({not_a_value, T, V}).

icodes_to_terms(Ts, Vs) ->
    [ icode_to_term(T, V) || {T, V} <- lists:zip(Ts, Vs) ].

parse(C,_Options) ->
    parse_string(C).

to_icode(TypedAst, Options) ->
    aeso_ast_to_icode:convert_typed(TypedAst, Options).

assemble(Icode, Options) ->
    aeso_icode_to_asm:convert(Icode, Options).


to_bytecode(['COMMENT',_|Rest],_Options) ->
    to_bytecode(Rest,_Options);
to_bytecode([Op|Rest], Options) ->
    [aeb_opcodes:m_to_op(Op)|to_bytecode(Rest, Options)];
to_bytecode([], _) -> [].

extract_type_info(#{functions := Functions} =_Icode) ->
    TypeInfo = [aeso_abi:function_type_info(list_to_binary(Name), Args, TypeRep)
                || {Name, Attrs, Args,_Body, TypeRep} <- Functions,
                   not is_tuple(Name),
                   not lists:member(private, Attrs)
               ],
    lists:sort(TypeInfo).

pp_sophia_code(C, Opts)->  pp(C, Opts, pp_sophia_code, fun(Code) ->
                                io:format("~s\n", [prettypr:format(aeso_pretty:decls(Code))])
                            end).
pp_ast(C, Opts)      ->  pp(C, Opts, pp_ast, fun aeso_ast:pp/1).
pp_typed_ast(C, Opts)->  pp(C, Opts, pp_typed_ast, fun aeso_ast:pp_typed/1).
pp_icode(C, Opts)    ->  pp(C, Opts, pp_icode, fun aeso_icode:pp/1).
pp_assembler(C, Opts)->  pp(C, Opts, pp_assembler, fun aeb_asm:pp/1).
pp_bytecode(C, Opts) ->  pp(C, Opts, pp_bytecode, fun aeb_disassemble:pp/1).

pp(Code, Options, Option, PPFun) ->
    case proplists:lookup(Option, Options) of
        {Option, true} ->
            PPFun(Code);
        none ->
            ok
    end.


%% -------------------------------------------------------------------
%% TODO: Tempoary parser hook below...

sophia_type_to_typerep(String) ->
    {ok, Ast} = aeso_parser:type(String),
    try aeso_ast_to_icode:ast_typerep(Ast) of
        Type -> {ok, Type}
    catch _:_ -> {error, bad_type}
    end.

parse_string(Text) ->
    %% Try and return something sensible here!
    case aeso_parser:string(Text) of
        %% Yay, it worked!
        {ok, Contract} -> Contract;
        %% Scan errors.
        {error, {Pos, scan_error}} ->
            parse_error(Pos, "scan error");
        {error, {Pos, scan_error_no_state}} ->
            parse_error(Pos, "scan error");
        %% Parse errors.
        {error, {Pos, parse_error, Error}} ->
            parse_error(Pos, Error);
        {error, {Pos, ambiguous_parse, As}} ->
            ErrorString = io_lib:format("Ambiguous ~p", [As]),
            parse_error(Pos, ErrorString)
    end.

parse_error({Line,Pos}, ErrorString) ->
    Error = io_lib:format("line ~p, column ~p: ~s", [Line,Pos,ErrorString]),
    error({parse_errors,[Error]}).

read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aes"]))),
    binary_to_list(Bin).

contract_path() ->
    "apps/aesophia/test/contracts".
