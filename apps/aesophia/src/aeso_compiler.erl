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

-export([ arg_typerep_from_function/2
        , deserialize/1
        , file/1
        , file/2
        , from_string/2
        , function_type_hash/3
        , typereps_from_type_hash/2
        , version/0
        ]).

-ifdef(TEST).

-export([ serialize/3
        ]).

-endif.


-type option() :: pp_sophia_code | pp_ast | pp_icode | pp_assembler |
                  pp_bytecode.
-type options() :: [option()].

-type wrapped_code() :: #{ source_hash := aec_hash:hash()
                         , type_info   := [binary()]
                         , byte_code   := binary()
                         }.
-type function_name() :: binary(). %% String
-type typerep() :: aeso_sophia:type().
-type type_info() :: [{ FunctionName :: function_name()
                      , ArgType      :: typerep()
                      , OutType      :: typerep()}].

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
    TypedAst = aeso_ast_infer_types:infer(Ast),
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
    TypeInfo = [function_type_info(Name, Args, TypeRep)
                || {Name, Attrs, Args,_Body, TypeRep} <- Functions,
                   not is_tuple(Name),
                   not lists:member(private, Attrs)
               ],
    lists:sort(TypeInfo).

function_type_info(Name, Args, OutType) ->
    ArgType = {tuple, [T || {_, T} <- Args]},
    { list_to_binary(Name)
    , function_type_hash(Name, ArgType, OutType)
    , aeso_data:to_binary(ArgType)
    , aeso_data:to_binary(OutType)
    }.

function_type_hash(Name, ArgType, OutType) ->
    aec_hash:hash(sophia_type_hash,
                  iolist_to_binary([ Name
                                   , aeso_data:to_binary(ArgType)
                                   , aeso_data:to_binary(OutType)
                                   ])).


-spec arg_typerep_from_function(Function :: binary(), type_info()) ->
           {'ok', typerep()} | {'error', 'bad_type_data' | 'unknown_function'}.
arg_typerep_from_function(Function, TypeInfo) ->
    case lists:keyfind(Function, 1, TypeInfo) of
        {Function,_TypeHash, ArgTypeBin,_OutTypeBin} ->
            case aeso_data:from_binary(typerep, ArgTypeBin) of
                {ok, ArgType} -> {ok, ArgType};
                {error,_} -> {error, bad_type_data}
            end;
        false ->
            {error, unknown_function}
    end.

-spec typereps_from_type_hash(TypeHash :: binary(), type_info()) ->
           {'ok', typerep()} | {'error', 'bad_type_data' | 'unknown_function'}.
typereps_from_type_hash(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 2, TypeInfo) of
        {_Function, TypeHash, ArgTypeBin, OutTypeBin} ->
            case {aeso_data:from_binary(typerep, ArgTypeBin),
                  aeso_data:from_binary(typerep, OutTypeBin)} of
                {{ok, ArgType}, {ok, OutType}} -> {ok, ArgType, OutType};
                {_, _} -> {error, bad_type_data}
            end;
        false ->
            {error, unknown_function}
    end.

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
    , {type_info, [{binary, binary, binary, binary}]} %% {Function, type hash, arg type, out type}
    , {byte_code, binary}].


pp_sophia_code(C, Opts)->  pp(C, Opts, pp_sophia_code, fun(Code) ->
                                io:format("~s\n", [prettypr:format(aeso_pretty:decls(Code))])
                            end).
pp_ast(C, Opts)      ->  pp(C, Opts, pp_ast, fun aeso_ast:pp/1).
pp_typed_ast(C, Opts)->  pp(C, Opts, pp_typed, fun aeso_ast:pp_typed/1).
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
        Err = {error, {Line, aeso_scan, Reason}} ->
            io:format("Lexical error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err);
        Err = {error, {Line, aeso_parser, Reason}} ->
            io:format("Parse error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err)
    end.

read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aes"]))),
    binary_to_list(Bin).

contract_path() ->
    "apps/aesophia/test/contracts".
