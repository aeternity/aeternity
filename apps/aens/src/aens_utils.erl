%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System utility functions
%%% @end
%%%=============================================================================

-module(aens_utils).

-include("aens.hrl").

%% API
-export([check_name_claimed_and_owned/3,
         name_parts/1,
         check_split_name/1,
         name_to_ascii/1,
         to_ascii/1,
         ascii_encode/1,
         ensure_name_length/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_name_claimed_and_owned(binary(), aec_keys:pubkey(), aec_trees:trees()) -> ok | {error, term()}.
check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) ->
    NamesTree = aec_trees:ns(Trees),
    case aens_state_tree:lookup_name(NameHash, NamesTree) of
        {value, Name} ->
            Checks =
                [fun() -> check_name_owner(Name, AccountPubKey) end,
                 fun() -> check_claimed_status(Name) end],
            aeu_validation:run(Checks);
        none ->
            {error, name_does_not_exist}
    end.

-spec to_ascii(binary()) -> {ok, binary()} | {error, term()}.
to_ascii(Name) when is_binary(Name)->
    case check_split_name(Name) of
        {ok, _, _} ->
            name_to_ascii(Name);
        {error, _} = E ->
            E
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_name_owner(Name, AccountPubKey) ->
    case aens_names:owner_pubkey(Name) =:= AccountPubKey of
        true  -> ok;
        false -> {error, name_not_owned}
    end.

check_claimed_status(Name) ->
    case aens_names:status(Name) of
        claimed -> ok;
        revoked -> {error, name_revoked}
    end.

name_parts(Name) ->
    binary:split(Name, ?LABEL_SEPARATOR, [global, trim]).

check_split_name(Name) ->
    case name_parts(Name) of
        [_] ->
            {error, no_registrar};
        [_|_] = NameParts ->
            RegistrarNS = lists:last(NameParts),
            case lists:member(RegistrarNS, aec_governance:possible_name_registrars()) of
                true ->
                    PartsLen = length(NameParts),
                    NameKind = if PartsLen == 2 -> name; true -> subname end,
                    {ok, NameKind, NameParts};
                false ->
                    {error, registrar_unknown}
            end
    end.

name_to_ascii(Name) when is_binary(Name) ->
    case ascii_encode(Name) of
        {ok, NameAscii} ->
            %% idna:to_ascii(".aet") returns just "aet"
            case length(string:split(NameAscii, ".", all)) =:= 1 of
                true  -> {error, no_label_in_registrar};
                false -> {ok, list_to_binary(NameAscii)}
            end;
        {error, _} = Error ->
            Error
    end.

ascii_encode(Name) ->
    NameUnicodeList = unicode:characters_to_list(Name, utf8),
    try idna:encode(NameUnicodeList, [{uts46, true}, {std3_rules, true}]) of
        NameAscii -> {ok, NameAscii}
    catch
        exit:{bad_label, Reason} ->
            {error, {bad_label, Reason}};
        exit:{invalid_codepoint, Cp} ->
            {error, {invalid_codepoint, Cp}}
    end.

ensure_name_length(Name, ErrorTag) ->
    length(unicode:characters_to_list(Name)) =< ?MAX_NAME_LENGTH
        orelse error({ErrorTag, Name}).
