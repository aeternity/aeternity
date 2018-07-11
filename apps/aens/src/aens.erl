%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System API
%%% @end
%%%-------------------------------------------------------------------

-module(aens).

%% API
-export([resolve/3,
         resolve_from_hash/3,
         get_commitment_hash/2,
         get_name_entry/2,
         get_name_hash/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(LABEL_SEPARATOR, <<".">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec resolve(atom(), binary(), aens_state_tree:tree()) -> {ok, binary()} | {error, atom()}.
resolve(Type, Binary, NSTree) ->
    case is_name(Binary) of
        false ->
            aec_base58c:safe_decode(Type, Binary);
        true ->
            case get_name(Binary, NSTree) of
                {ok, #{<<"pointers">> := Pointers}} ->
                    case proplists:get_value(atom_to_binary(Type, utf8), Pointers) of
                        undefined -> {error, type_not_found};
                        Val       -> aec_base58c:safe_decode(Type, Val)
                    end;
                {error, _Reason} = Error ->
                    Error
            end
    end.

resolve_from_hash(Type, NameHash, NSTree) ->
    case aens_state_tree:lookup_name(NameHash, NSTree) of
        {value, Name} ->
            Pointers = aens_names:pointers(Name),
            case proplists:get_value(atom_to_binary(Type, utf8), Pointers) of
                undefined -> {error, type_not_found};
                Val       -> aec_base58c:safe_decode(Type, Val)
            end;
        none ->
            {error, hash_not_found}
    end.

-spec get_commitment_hash(binary(), integer()) -> {ok, aens_hash:commitment_hash()} |
                                                  {error, atom()}.
get_commitment_hash(Name, Salt) when is_binary(Name) andalso is_integer(Salt) ->
    case aens_utils:to_ascii(Name) of
        {ok, NameAscii} -> {ok, aens_hash:commitment_hash(NameAscii, Salt)};
        {error, _} = E  -> E
    end.

-spec get_name_entry(binary(), aens_state_tree:tree()) -> {ok, map()} | {error, atom()}.
get_name_entry(Name, NSTree) ->
    case get_name(Name, NSTree) of
        {ok, #{<<"name">>     := Name,
               <<"hash">>     := Hash,
               <<"name_ttl">> := TTL,
               <<"pointers">> := Pointers}} ->
            {ok, #{<<"name">>     => Name,
                   <<"hash">>     => Hash,
                   <<"name_ttl">> => TTL,
                   <<"pointers">> => jsx:encode(Pointers)}};
        {error, _} = E ->
            E
    end.

-spec get_name_hash(binary()) -> {ok, binary()} |
                                 {error, atom()}.
get_name_hash(Name) when is_binary(Name) ->
    case aens_utils:to_ascii(Name) of
        {ok, NameAscii} -> {ok, aens_hash:name_hash(NameAscii)};
        {error, _} = E  -> E
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_name(Binary) ->
    length(binary:split(Binary, ?LABEL_SEPARATOR)) > 1.

get_name(Name, NSTree) ->
    case get_name_hash(Name) of
        {ok, NameHash} ->
            case aens_state_tree:lookup_name(NameHash, NSTree) of
                {value, N} ->
                    case aens_utils:is_revoked(N) of
                        true ->
                            {error, name_revoked};
                        false ->
                            {ok, #{<<"name">>     => Name,
                                   <<"hash">>     => NameHash,
                                   <<"name_ttl">> => aens_names:expires(N),
                                   <<"pointers">> => aens_names:pointers(N)}}
                    end;
                none ->
                    {error, name_not_found}
            end;
        {error, _} = E ->
            E
    end.
