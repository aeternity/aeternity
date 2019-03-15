%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Sophia contracts and data.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_sophia).

-include("aecontract.hrl").
-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-export([ compile/2
        , decode_data/2
        , encode_call_data/3
        , serialize/1
        , serialize/2
        , deserialize/1
        , is_legal_serialization_at_height/2
        ]).

-type wrapped_code() :: #{ source_hash := aec_hash:hash()
                         , type_info   := [{binary(), binary(), binary(), binary()}]
                         , byte_code   := binary()
                         , compiler_version := binary()
                         , contract_vsn := integer()
                         }.

-export_type([ wrapped_code/0 ]).

-define(SOPHIA_CONTRACT_VSN_1, 1).
-define(SOPHIA_CONTRACT_VSN_2, 2).

%% After hard fork Minerva, we accept ?SOPHIA_CONTRACT_VSN_2 in serialization
%% Therefore, switch the present version to ?SOPHIA_CONTRACT_VSN_2 when release
%% after hard fork.
-define(SOPHIA_CONTRACT_VSN, ?SOPHIA_CONTRACT_VSN_1).


-spec compile(binary(), binary()) -> {ok, binary()} | {error, binary()}.
compile(ContractAsBinString, OptionsAsBinString) ->
    ContractText = binary_to_list(ContractAsBinString),
    Options = parse_options(OptionsAsBinString),
    try aeso_compiler:from_string(ContractText, Options) of
        {ok, Map} -> {ok, serialize(Map)};
        {error, _} = Err -> Err
    catch
        %% General programming errors in the compiler.
        error:Error ->
            Where = hd(erlang:get_stacktrace()),
            ErrorString = io_lib:format("Error: ~p in\n   ~p", [Error,Where]),
            {error, list_to_binary(ErrorString)}
    end.

parse_options(OptionsBinString) ->
    parse_options(OptionsBinString, []).

parse_options(<<" ", Rest/binary>>, Acc) ->
    parse_options(Rest, Acc);
parse_options(<<"pp_sophia_code", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_sophia_code | Acc]);
parse_options(<<"pp_ast", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_ast | Acc]);
parse_options(<<"pp_icode", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_icode | Acc]);
parse_options(<<"pp_assembler", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_assembler | Acc]);
parse_options(<<"pp_bytecode", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_bytecode | Acc]);
parse_options(<<_:8, Rest/binary>>, Acc) ->
    %% TODO: give nice error instead of just ignoring stray chars.
    parse_options(Rest, Acc);
parse_options(<<>>, Acc) -> Acc.

is_legal_serialization_at_height(?SOPHIA_CONTRACT_VSN_1, _Height) ->
    true;
is_legal_serialization_at_height(?SOPHIA_CONTRACT_VSN_2, Height) ->
    aec_hard_forks:protocol_effective_at_height(Height) >= ?MINERVA_PROTOCOL_VSN.

serialize(Map) ->
    serialize(Map, ?SOPHIA_CONTRACT_VSN).

serialize(#{byte_code := ByteCode, type_info := TypeInfo,
            contract_source := ContractString, compiler_version := Version}, SophiaContractVersion) ->
    ContractBin = list_to_binary(ContractString),
    BinVersion = if is_integer(Version) -> integer_to_binary(Version);
                    is_binary(Version) -> Version
                 end,
    Fields = [ {source_hash, aec_hash:hash(sophia_source_code, ContractBin)}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode} ] ++
             [ {compiler_version, BinVersion} || SophiaContractVersion > ?SOPHIA_CONTRACT_VSN_1 ],
             %% Add version here in release when Minerva height has been reached
    aeser_chain_objects:serialize(compiler_sophia,
                                       SophiaContractVersion,
                                       serialization_template(SophiaContractVersion),
                                       Fields
                                      ).

-spec deserialize(binary()) -> wrapped_code().
deserialize(Binary) ->
    case aeser_chain_objects:deserialize_type_and_vsn(Binary) of
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_1 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            #{ source_hash => Hash
             , type_info => TypeInfo
             , byte_code => ByteCode
             , contract_vsn => Vsn
             };
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_2 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            , {compiler_version, CompilerVersion}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            #{ source_hash => Hash
             , type_info => TypeInfo
             , byte_code => ByteCode
             , compiler_version => CompilerVersion
             , contract_vsn => Vsn
             };
        Other ->
            error({illegal_code_object, Other})
    end.

serialization_template(?SOPHIA_CONTRACT_VSN_1) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary}];
serialization_template(?SOPHIA_CONTRACT_VSN_2) ->
    serialization_template(?SOPHIA_CONTRACT_VSN_1) ++
        [ {compiler_version, binary}].

-spec encode_call_data(binary(), binary(), [binary()]) ->
                              {ok, binary()} | {error, binary()}.
encode_call_data(Code, Function, Arguments) ->
    try aeso_compiler:create_calldata(binary_to_list(Code), binary_to_list(Function),
                                      lists:map(fun binary_to_list/1, Arguments)) of
        {error, _} = Err -> Err;
        {ok, Data,_DataType,_OutType} when is_binary(Data) ->
            {ok, Data}
    catch _T:_E ->
        {error, <<"bad argument">>}
    end.


decode_data(Type, Data) ->
    case get_type(Type) of
        {ok, SophiaType} ->
            try aeso_heap:from_binary(SophiaType, Data) of
                {ok, Term} ->
                    try prepare_for_json(SophiaType, Term) of
                        R -> {ok, R}
                    catch throw:R -> R
                    end;
                {error, _} -> {error, <<"bad type/data">>}
            catch _T:_E ->    {error, <<"bad argument">>}
            end;
        {error, _} = E -> E
    end.

get_type(BinaryString) ->
    String = unicode:characters_to_list(BinaryString, utf8),
    case aeso_compiler:sophia_type_to_typerep(String) of
        {ok, _Type} = R -> R;
        {error, ErrorAtom} ->
            {error, unicode:characters_to_binary(atom_to_list(ErrorAtom))}
    end.


prepare_for_json(word, Integer) when is_integer(Integer) ->
    #{ <<"type">> => <<"word">>,
       <<"value">> => Integer};
prepare_for_json(string, String) when is_binary(String) ->
    #{ <<"type">> => <<"string">>,
       <<"value">> => String};
prepare_for_json({option, _T}, none) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => <<"None">>};
prepare_for_json({option, T}, {some, E}) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => prepare_for_json(T,E) };
prepare_for_json({tuple, Ts}, Es) ->
    #{ <<"type">> => <<"tuple">>,
       <<"value">> => [prepare_for_json(T,E)
                       || {T,E} <-
                              lists:zip(Ts, tuple_to_list(Es))] };
prepare_for_json({list, T}, Es) ->
    #{ <<"type">> => <<"list">>,
       <<"value">> => [prepare_for_json(T,E) || E <- Es]};
prepare_for_json(T = {variant, Cons}, R = {variant, Tag, Args}) when is_integer(Tag), Tag < length(Cons) ->
    Ts = lists:nth(Tag + 1, Cons),
    case length(Ts) == length(Args) of
        true ->
            #{ <<"type">> => <<"variant">>
             , <<"value">> => [Tag | [prepare_for_json(ArgT, Arg)
                                      || {ArgT, Arg} <- lists:zip(Ts, Args)]] };
        false ->
            String = io_lib:format("Type: ~p Res:~p", [T,R]),
            Error = << <<B>> || B <- "Invalid Sophia type: " ++ lists:flatten(String) >>,
            throw({error, Error})
    end;
prepare_for_json({map, KeyT, ValT}, Map) when is_map(Map) ->
    #{ <<"type">> => <<"map">>,
       <<"value">> => [ #{ <<"key">> => prepare_for_json(KeyT, K),
                           <<"val">> => prepare_for_json(ValT, V) }
                        || {K, V} <- maps:to_list(Map) ] };
prepare_for_json(T, R) ->
    String = io_lib:format("Type: ~p Res:~p", [T,R]),
    Error = << <<B>> || B <- "Invalid Sophia type: " ++ lists:flatten(String) >>,
    throw({error, Error}).

