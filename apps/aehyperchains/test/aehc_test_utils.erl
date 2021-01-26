-module(aehc_test_utils).

-export([ hc_from_genesis/0
        , hc_consensus/0
        , cuckoo_pow_consensus/0
        , cuckoo_pow_from_genesis/0
        , enable_hc_from_genesis/0
        , enable_pow_cuckoo_from_genesis/0
        ]).

-include_lib("eunit/include/eunit.hrl").

hc_consensus() -> #{<<"name">> => <<"hyperchains">>}.
cuckoo_pow_consensus() -> #{<<"name">> => <<"pow_cuckoo">>}.

hc_from_genesis() -> #{<<"0">> => hc_consensus()}.
cuckoo_pow_from_genesis() -> #{<<"0">> => cuckoo_pow_consensus()}.

enable_hc_from_genesis() ->
    application:set_env(aecore, consensus, hc_from_genesis()),
    aec_consensus:set_consensus(),
    ?assertEqual(ok, aec_consensus:check_env()),
    ok.

enable_pow_cuckoo_from_genesis() ->
    application:set_env(aecore, consensus, cuckoo_pow_from_genesis()),
    aec_consensus:set_consensus(),
    ?assertEqual(ok, aec_consensus:check_env()),
    ok.

