%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System utility functions
%%% @end
%%%=============================================================================

-module(aens_utils).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([check_name_claimed_and_owned/3,
         is_revoked/1,
         to_ascii/1,
         from_ascii/1,
         validate_name/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(LABEL_SEPARATOR, <<".">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_name_claimed_and_owned(binary(), pubkey(), aec_trees:trees()) -> ok | {error, term()}.
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

-spec is_revoked(aens_names:name()) -> boolean().
is_revoked(Name) ->
    case aens_names:status(Name) of
        revoked -> true;
        claimed -> false
    end.

-spec to_ascii(binary()) -> {ok, binary()} | {error, term()}.
to_ascii(Name) when is_binary(Name)->
    case validate_name(Name) of
        ok ->
            NameUnicodeList = unicode:characters_to_list(Name, utf8),
            AsciiName       = idna:to_ascii(NameUnicodeList),
            %% TODO: Validation should be a part of idna library.
            %% IDNA validation should be applied on name in both Unicode and ASCII forms.
            case validate_name_ascii(AsciiName) of
                ok             -> {ok, list_to_binary(AsciiName)};
                {error, _} = E -> E
            end;
        {error, _} = E ->
            E
    end.

-spec from_ascii(binary()) -> binary().
from_ascii(NameAscii) when is_binary(NameAscii) ->
    NameAsciiList = unicode:characters_to_list(NameAscii, utf8),
    UnicodeName   = idna:from_ascii(NameAsciiList),
    list_to_binary(UnicodeName).

validate_name(Name) ->
    case binary:split(Name, ?LABEL_SEPARATOR, [global, trim]) of
        [_Label, RegistrarNS] ->
            case [RN || RN <- aec_governance:name_registrars(), RN =:= RegistrarNS] of
                [] -> {error, registrar_unknown};
                _  -> ok
            end;
        [_Name] ->
            {error, no_registrar};
        [_Label | _Namespaces] ->
            {error, multiple_namespaces}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_name_owner(Name, AccountPubKey) ->
    case aens_names:owner(Name) =:= AccountPubKey of
        true  -> ok;
        false -> {error, name_not_owned}
    end.

check_claimed_status(Name) ->
    case is_revoked(Name) of
        true  -> {error, name_revoked};
        false -> ok
    end.

validate_name_ascii(NameAscii) ->
    case length(NameAscii) > 253 of
        true  -> {error, name_too_long};
        false ->
            Labels = string:split(NameAscii, ".", all),
            %% idna:to_ascii(".aet") returns just "aet"
            case length(Labels) of
                1  -> {error, no_label_in_registrar};
                _N -> validate_labels(Labels)
            end
    end.

validate_labels([]) ->
    ok;
validate_labels([Label | Rest]) ->
    case length(Label) > 0 andalso length(Label) =< 63 of
        true  -> validate_labels(Rest);
        false -> {error, bad_label_length}
    end.
