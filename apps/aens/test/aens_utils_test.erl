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
                 aens_utils:to_ascii(<<"abcd.efgh.crypto">>)),
    ?assertEqual({error, registrar_unknown},
                 aens_utils:to_ascii(<<"abcd.cryptott">>)),
    LongName = list_to_binary(string:lowercase(base58:binary_to_base58(crypto:strong_rand_bytes(1000)))),
    ?assertEqual({error, no_label_in_registrar},
                 aens_utils:to_ascii(<<".crypto">>)),
    ?assertMatch({error, {bad_label, {too_long, _}}},
                 aens_utils:to_ascii(<<LongName/binary, ".crypto">>)),
    ?assertEqual({error, {invalid_codepoint, 65533}},
                 aens_utils:to_ascii(<<"�姆.crypto"/utf8>>)),
    ?assertEqual({ok, <<"xn--8wsa062gba0028bca.crypto">>},
                 aens_utils:to_ascii(<<"詹姆斯詹姆斯.crypto"/utf8>>)).
