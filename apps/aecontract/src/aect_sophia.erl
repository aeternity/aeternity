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
        , is_legal_serialization_at_protocol/2
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

is_legal_serialization_at_protocol(?SOPHIA_CONTRACT_VSN_1, Protocol) ->
    Protocol =< ?FORTUNA_PROTOCOL_VSN;
is_legal_serialization_at_protocol(?SOPHIA_CONTRACT_VSN_2, Protocol) ->
    Protocol >= ?MINERVA_PROTOCOL_VSN andalso Protocol =< ?FORTUNA_PROTOCOL_VSN;
is_legal_serialization_at_protocol(?SOPHIA_CONTRACT_VSN_3, Protocol) ->
    Protocol >= ?LIMA_PROTOCOL_VSN.

-spec serialize(map(), non_neg_integer()) -> binary().
serialize(CodeMap, SophiaContractVsn) ->
    aeser_contract_code:serialize(CodeMap, SophiaContractVsn).

-spec deserialize(binary()) -> wrapped_code().
deserialize(Binary) ->
    aeser_contract_code:deserialize(Binary).

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
