%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Module wrapping user provided state passwords in order to avoid accidental
%%%    leakage of sensitive data in debug logs and crash dumps.
%%%    The sensitive data is wrapped in a getter function - any attempt to print out
%%%    the data will result in printing the atom representing the function.
%%% @end
%%%=============================================================================
-module(aesc_state_password_wrapper).

-export([ init/1
        , get/1
        ]).

-opaque wrapper() :: fun(() -> {ok, string()} | error).
-export_type([wrapper/0]).

-spec init({ok, string()} | error) -> wrapper().
init(MaybeStatePassword) -> fun() -> MaybeStatePassword end.

-spec get(wrapper()) -> {ok, string()} | error.
get(StatePasswordWrapper) -> StatePasswordWrapper().
