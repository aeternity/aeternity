-module(aeso_abi_tests).

-include_lib("eunit/include/eunit.hrl").

encode_call_with_integer_test() ->
    [32, 96, 42, 4, "main"] =
        aeso_test_utils:dump_words(
            aeso_abi:create_calldata("", "main", "42")).
