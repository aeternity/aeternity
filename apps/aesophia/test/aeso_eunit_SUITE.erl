-module(aeso_eunit_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, eunit}].

groups() ->
    [{eunit, [], [aeso_scan_tests, aeso_parser_tests, aeso_compiler_tests]}].

aeso_scan_tests(_Config)   -> ok = eunit:test(aeso_scan_tests).
aeso_parser_tests(_Config) -> ok = eunit:test(aeso_parser_tests).
aeso_compiler_tests(_Config) -> ok = eunit:test(aeso_compiler_tests).
