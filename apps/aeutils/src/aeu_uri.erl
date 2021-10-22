%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc URI utilities
%%% @end
%%%=============================================================================

-module(aeu_uri).

-export([scheme_validation_fun/1, encode/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% Scheme validation function meant to be used with http_uri:parse/2
%%
%% The specified valid schemes shall be lowercase.
scheme_validation_fun(ValidSchemeStrings) ->
    fun(SchemeStr) ->
            SchemeStrLower = http_util:to_lower(SchemeStr),
            case lists:member(SchemeStrLower, ValidSchemeStrings) of
                true ->
                    valid;
                false ->
                    {error, {bad_scheme, SchemeStr}}
            end
    end.

%% Taken from the deprecated http_uri:encode/1 implementation.
%% NOTE: For use only on path parts of URIs
%% The suggested replacement uri_string module doesn't contain an equivalent
%% function apparently to discourage people from using it to replace previously
%% incorrect uses of this on entire URLs. 
-spec encode(string()) -> string().
encode(URI) when is_list(URI) ->
    Reserved = reserved(), 
    lists:append([uri_encode(Char, Reserved) || Char <- URI]);
encode(URI) when is_binary(URI) ->
    Reserved = reserved(),
    << <<(uri_encode_binary(Char, Reserved))/binary>> || <<Char>> <= URI >>.

uri_encode(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
    true ->
        [ $% | integer_to_list(Char, 16)];
    false ->
        [Char]
    end.

uri_encode_binary(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
        true ->
            << $%, (integer_to_binary(Char, 16))/binary >>;
        false ->
            <<Char>>
    end.

reserved() ->
    sets:from_list([$;, $:, $@, $&, $=, $+, $,, $/, $?,
            $#, $[, $], $<, $>, $\", ${, $}, $|, %"
                   $\\, $', $^, $%, $ ]).