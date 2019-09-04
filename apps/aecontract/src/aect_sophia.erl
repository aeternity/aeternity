%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Sophia contracts and data.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_sophia).

-include("../include/aecontract.hrl").
-include("../../aecore/include/blocks.hrl").
-include("../../aecontract/include/hard_forks.hrl").
-include("../../aecontract/src/aect_sophia.hrl").

-export([ serialize/2
        , deserialize/1
        , is_legal_serialization_at_height/2
        , prepare_for_json/2
        ]).

-type wrapped_code() :: #{ source_hash := aec_hash:hash()
                         , type_info   := [{binary(), binary(), binary(), binary()}]
                         , byte_code   := binary()
                         , compiler_version => binary()
                         , contract_vsn := integer()
                         , payable     => boolean()
                         }.

-export_type([ wrapped_code/0 ]).

is_legal_serialization_at_height(?SOPHIA_CONTRACT_VSN_1, Height) ->
    aec_hard_forks:protocol_effective_at_height(Height) =< ?FORTUNA_PROTOCOL_VSN;
is_legal_serialization_at_height(?SOPHIA_CONTRACT_VSN_2, Height) ->
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    Protocol >= ?MINERVA_PROTOCOL_VSN andalso Protocol =< ?FORTUNA_PROTOCOL_VSN;
is_legal_serialization_at_height(?SOPHIA_CONTRACT_VSN_3, Height) ->
    aec_hard_forks:protocol_effective_at_height(Height) >= ?LIMA_PROTOCOL_VSN.

serialize(CodeMap, SophiaContractVsn) ->
    case CodeMap of
        #{ source_hash := _ } ->
            serialize_(CodeMap, SophiaContractVsn);
        #{ contract_source := ContractString } ->
            SourceHash = aec_hash:hash(sophia_source_code, list_to_binary(ContractString)),
            serialize_(CodeMap#{ source_hash => SourceHash }, SophiaContractVsn)
    end.

serialize_(CompiledCode = #{ byte_code := ByteCode
                           , type_info := TypeInfo
                           , source_hash := SourceHash }, SophiaContractVersion) ->
    Version    = maps:get(version, CompiledCode, <<"unknown">>),
    BinVersion = if is_integer(Version) -> integer_to_binary(Version);
                    is_binary(Version) -> Version
                 end,
    Fields = [ {source_hash, SourceHash}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode} ] ++
             [ {compiler_version, BinVersion}
               || SophiaContractVersion > ?SOPHIA_CONTRACT_VSN_1 ] ++
             [ {payable, maps:get(payable, CompiledCode)}
               || SophiaContractVersion > ?SOPHIA_CONTRACT_VSN_2 ],
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
             , payable => true
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
             , payable => true
             };
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_3 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            , {compiler_version, CompilerVersion}
            , {payable, Payable}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            #{ source_hash => Hash
             , type_info => TypeInfo
             , byte_code => ByteCode
             , compiler_version => CompilerVersion
             , contract_vsn => Vsn
             , payable => Payable
             };
        Other ->
            error({illegal_code_object, Other})
    end.

serialization_template(?SOPHIA_CONTRACT_VSN_1) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary} ];
serialization_template(?SOPHIA_CONTRACT_VSN_2) ->
    serialization_template(?SOPHIA_CONTRACT_VSN_1) ++
        [ {compiler_version, binary} ];
serialization_template(?SOPHIA_CONTRACT_VSN_3) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, bool, binary, binary}]} %% {type hash, name, payable, arg type, out type}
    , {byte_code, binary}
    , {compiler_version, binary}
    , {payable, bool} ].

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
