-module(dummy_test).

-include_lib("eunit/include/eunit.hrl").

test_added_only_to_include_test_dir_in_git() ->
    ?assertEqual(1, 1).
