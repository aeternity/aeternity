%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc URI utilities
%%% @end
%%%=============================================================================

-module(aeu_uri).

-export([scheme_validation_fun/1]).

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
