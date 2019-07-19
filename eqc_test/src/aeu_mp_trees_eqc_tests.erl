-module(aeu_mp_trees_eqc_tests).
-include_lib("eunit/include/eunit.hrl").

quickcheck_test_() ->
    aeeqc_eunit:props_mod_test_repr(?MODULE_STRING, fun testing_time_ms/1).

testing_time_ms(PropName) when is_atom(PropName) ->
    aeeqc_eunit:testing_time_multiplier() * 500.
