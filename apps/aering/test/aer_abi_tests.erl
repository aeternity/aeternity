-module(aer_abi_tests).

-include_lib("eunit/include/eunit.hrl").

encode_call_with_integer_test_() -> 
    <<0:256, 42:256>> = aer_abi:create_calldata("", "main", [42]).
