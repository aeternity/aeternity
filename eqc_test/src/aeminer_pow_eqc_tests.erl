-module(aeminer_pow_eqc_tests).
-include_lib("eunit/include/eunit.hrl").

quickcheck_test_() ->
    aeeqc_eunit:props_mod_test_repr(?MODULE_STRING).
