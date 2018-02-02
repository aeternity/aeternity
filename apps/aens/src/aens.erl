%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Resolve registered names
%%% @end
%%%-------------------------------------------------------------------

-module(aens).

%% API
-export([resolve/3,
         get_name_entry/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(LABEL_SEPARATOR, <<".">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec resolve(atom(), binary(), aens_state_tree:tree()) -> {ok, binary()} | {error, atom()}.
resolve(Type, Binary, NSTree) ->
    case lists:reverse(binary:split(Binary, ?LABEL_SEPARATOR, [global, trim])) of %% Use trim to allow closing dot
        [Binary] ->
            aec_base58c:safe_decode(Type, Binary);
        [RegistrarNamespace|_Namespace] ->
            %% When we have more registrars, pick faster and still gov friendly structure
            %% If we allow to purchase registrars, this will live in regular tree
            case [RN || RN <- aec_governance:name_registrars(), RN =:= RegistrarNamespace] of
                [] ->
                    {error, registrar_unknown};
                _ ->
                    case get_name(Binary, NSTree) of
                        {ok, #{<<"pointers">> := Pointers}} ->
                            case proplists:get_value(atom_to_binary(Type, utf8), Pointers) of
                                undefined -> {error, type_not_found};
                                Val -> {ok, aec_base58c:safe_decode(Type, Val)}
                            end;
                        {error, _} = Err ->
                            Err
                    end
            end
    end.

-spec get_name_entry(binary(), aens_state_tree:tree()) -> {ok, map()} | {error, name_not_found}.
get_name_entry(Name, NSTree) ->
    case get_name(Name, NSTree) of
        {ok, #{<<"name">>     := Name,
               <<"name_ttl">> := TTL,
               <<"pointers">> := Pointers}} ->
            {ok, #{<<"name">>     => Name,
                   <<"name_ttl">> => TTL,
                   <<"pointers">> => jsx:encode(Pointers)}};
        {error, name_not_found} = Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_name(Name, NSTree) ->
    NameHash = aens_hash:name_hash(Name),
    case aens_state_tree:lookup_name(NameHash, NSTree) of
        {value, N} ->
            {ok, #{<<"name">>     => Name,
                   <<"name_ttl">> => aens_names:ttl(N),
                   <<"pointers">> => aens_names:pointers(N)}};
        none ->
            {error, name_not_found}
    end.
