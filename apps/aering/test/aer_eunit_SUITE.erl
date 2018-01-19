-module(aer_eunit_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, eunit}].

groups() ->
    [{eunit, [aer_scan_tests, aer_parser_tests]}].

aer_scan_tests(_Config)   -> ok = eunit:test(aer_scan_tests).
aer_parser_tests(_Config) -> ok = eunit:test(aer_parser_tests).
