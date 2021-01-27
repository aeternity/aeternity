-module(aec_plugin).

-export([
          register/1
        , get_module/1
        , get_module/2
        ]).

tags() ->
    [aec_tx_pool].

valid_registry(R) when is_map(R) ->
    lists:all(
        fun({K, M}) ->
            case (is_atom(M)
                andalso
                lists:member(K, tags())) of
                true ->
                    module_is_loaded(M);
                false ->
                    false
            end
        end, maps:to_list(R)).

module_is_loaded(M) ->
    case code:ensure_loaded(M) of
        {module, _} -> true;
        {error, E} ->
            lager:error("Module ~p not loaded: ~p", [M, E]),
            false
    end.

register(Map) when is_map(Map) ->
    case valid_registry(Map) of
        true ->
            set_registry(Map);
        false ->
            error(invalid_registry)
    end.

get_module(Tag) ->
    get_module(Tag, undefined).

get_module(Tag, Default) ->
    case get_registry() of
        #{Tag := Module} ->
            Module;
        _ ->
            Default
    end.


%% Pre-OTP 21 ==================================================
-ifndef(OTP_RELEASE).

set_registry(R) ->
    application:set_env(aecore, {?MODULE, registry}, R).

get_registry() ->
    application:get_env(aecore, {?MODULE, registry}, undefined).

%% OTP 21 and later ============================================
-else.

%% We want to use persistent_term for storing the registry, since
%% fetching it will have the same overhead as a remote function call.
%% The functionality was introduced in OTP 21.2. Also in OTP 21, the
%% predefined macro ?OTP_RELEASE was introduced. Let's assume that anyone
%% using OTP 21 is using at least 21.2.

set_registry(R) ->
    persistent_term:put({?MODULE, registry}, R).

get_registry() ->
    persistent_term:get({?MODULE, registry}, undefined).

-endif.

