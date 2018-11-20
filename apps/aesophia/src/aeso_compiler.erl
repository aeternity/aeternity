%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Compiler from Aeterinty Sophia language to the Aeternity VM, aevm.
%%% @end
%%% Created : 12 Dec 2017
%%% aec_conductor:stop_mining().
%%% aeso_compiler:file( identity, [pp_ast,pp_icode,pp_assembler,pp_bytecode, pp_sophia_code]).
%%%-------------------------------------------------------------------
-module(aeso_compiler).

-export([ deserialize/1
        , file/1
        , file/2
        , from_string/2
        , check_call/2
        , version/0
        ]).

-ifdef(TEST).

-export([ serialize/3
        ]).

-endif.

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aeso_icode.hrl").


-type option() :: pp_sophia_code | pp_ast | pp_icode | pp_assembler |
                  pp_bytecode.
-type options() :: [option()].

-type wrapped_code() :: #{ source_hash := aec_hash:hash()
                         , type_info   := [binary()]
                         , byte_code   := binary()
                         }.

-export_type([ option/0
             , options/0
             , wrapped_code/0
             ]).

-define(COMPILER_VERSION, 1).

-spec version() -> pos_integer().
version() ->
    ?COMPILER_VERSION.

-spec file(string()) -> binary().
file(Filename) ->
    file(Filename, []).

-spec file(string(), options()) -> binary().
file(Filename, Options) ->
    C = read_contract(Filename),
    from_string(C, Options).

-spec from_string(string(), options()) -> binary().
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
    serialize(ByteCode, TypeInfo, ContractString).

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
%% consumed by aeso_data:to_binary().
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

serialize(ByteCode, TypeInfo, ContractString) ->
    ContractBin = list_to_binary(ContractString),
    Version = version(),
    Fields = [ {source_hash, aec_hash:hash(sophia_source_code, ContractBin)}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode}
             ],
    aec_object_serialization:serialize(compiler_sophia,
                                       Version,
                                       serialization_template(Version),
                                       Fields
                                      ).

-spec deserialize(binary()) -> wrapped_code().
deserialize(Binary) ->
    case aec_object_serialization:deserialize_type_and_vsn(Binary) of
        {compiler_sophia = Type, Vsn,_Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            ] = aec_object_serialization:deserialize(Type, Vsn, Template, Binary),
            #{ source_hash => Hash
             , type_info => TypeInfo
             , byte_code => ByteCode
             };
        Other ->
            error({illegal_code_object, Other})
    end.

serialization_template(?COMPILER_VERSION) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary}].


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

parse_string(Text) ->
    case aeso_parser:string(Text) of
        {ok, Contract} -> Contract;
        Err = {error, {_Line, aeso_scan,_Reason}} ->
            error(Err);
        Err = {error, {_Line, aeso_parser,_Reason}} ->
            error(Err)
    end.

read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aes"]))),
    binary_to_list(Bin).

contract_path() ->
    "apps/aesophia/test/contracts".
