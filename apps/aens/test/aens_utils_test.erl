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
    LongName = base64:encode(crypto:strong_rand_bytes(1000)),
    ?assertEqual({error, name_too_long},
                 aens_utils:to_ascii(<<LongName/binary, ".aet">>)),
    ?assertEqual({error, no_label_in_registrar},
                 aens_utils:to_ascii(<<".aet">>)),
    LongLabel = base64:encode(crypto:strong_rand_bytes(80)),
    ?assertEqual({error, bad_label_length},
                 aens_utils:to_ascii(<<LongLabel/binary, ".aet">>)),
    ?assertEqual({ok, <<"xn--8wsa062gba0028bca.test">>},
                 aens_utils:to_ascii(<<"詹姆斯詹姆斯.test"/utf8>>)).
