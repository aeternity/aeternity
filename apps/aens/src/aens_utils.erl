%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System utility functions
%%% @end
%%%=============================================================================

-module(aens_utils).

%% API
-export([check_name_claimed_and_owned/3,
         name_parts/1, name_join/1,
         name_domain/1,
         to_ascii/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(LABEL_SEPARATOR, <<".">>).

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
    case validate_name(Name) of
        ok ->
            name_to_ascii(Name);
        {error, _} = E ->
            E
    end.

-spec name_parts(binary()) -> [binary()].
name_parts(Name) ->
    binary:split(Name, ?LABEL_SEPARATOR, [global, trim]).

%% inverse of name_parts
-spec name_join([binary()]) -> binary().
name_join(List) when is_list(List) ->
    iolist_to_binary(lists:join(?LABEL_SEPARATOR, List)).

-spec name_domain(binary()) -> {ok, binary()} | {error, invalid_name}.
name_domain(Name) ->
    case lists:reverse(name_parts(Name)) of
        [Domain | _] -> {ok, Domain};
        _ -> {error, invalid_name}
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

validate_name(Name) ->
    case name_parts(Name) of
        [_Label, RegistrarNS] ->
            case lists:member(RegistrarNS, aec_governance:possible_name_registrars()) of
                true  -> ok;
                false -> {error, registrar_unknown}
            end;
        [_Name] ->
            {error, no_registrar};
        [_Label | _Namespaces] ->
            {error, multiple_namespaces}
    end.

name_to_ascii(Name) when is_binary(Name) ->
    NameUnicodeList = unicode:characters_to_list(Name, utf8),
    try idna:encode(NameUnicodeList, [{uts46, true}, {std3_rules, true}]) of
        NameAscii ->
            %% idna:to_ascii(".chain") returns just "chain"
            case length(string:split(NameAscii, ?LABEL_SEPARATOR, all)) =:= 1 of
                true  -> {error, no_label_in_registrar};
                false -> {ok, list_to_binary(NameAscii)}
            end
    catch
        exit:{bad_label, Reason} ->
            {error, {bad_label, Reason}};
        exit:{invalid_codepoint, Cp} ->
            {error, {invalid_codepoint, Cp}}
    end.
