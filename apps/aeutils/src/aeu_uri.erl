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
scheme_validation_fun(ValidSchemeStrings) ->
    fun(SchemeStr) ->
            case lists:member(SchemeStr, ValidSchemeStrings) of
                true ->
                    valid;
                false ->
                    {error, {bad_scheme, SchemeStr}}
            end
    end.
