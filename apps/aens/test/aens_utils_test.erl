%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Unit tests for AE Naming System utils
%%% @end
%%%=============================================================================

-module(aens_utils_test).

-include_lib("eunit/include/eunit.hrl").

to_ascii_test() ->
    ?assertEqual({error, no_registrar},
                 aens_utils:to_ascii(<<"abcdefg">>)),
    ?assertEqual({error, multiple_namespaces},
                 aens_utils:to_ascii(<<"abcd.efgh.aet">>)),
    ?assertEqual({error, registrar_unknown},
                 aens_utils:to_ascii(<<"abcd.aettt">>)),
    LongName = list_to_binary(string:lowercase(base58:binary_to_base58(crypto:strong_rand_bytes(1000)))),
    ?assertEqual({error, no_label_in_registrar},
                 aens_utils:to_ascii(<<".aet">>)),
    ?assertMatch({error, {bad_label, {too_long, _}}},
                 aens_utils:to_ascii(<<LongName/binary, ".aet">>)),
    ?assertEqual({error, {invalid_codepoint, 65533}},
                 aens_utils:to_ascii(<<"�姆.aet"/utf8>>)),
    ?assertEqual({ok, <<"xn--8wsa062gba0028bca.aet">>},
                 aens_utils:to_ascii(<<"詹姆斯詹姆斯.aet"/utf8>>)).
