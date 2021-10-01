%%% File        : aeu_crypto_tests.erl
%%% Author      : Hans Svensson
%%% Description : Checks that the used crypto-components (enacl) behave as expected
%%% Created     : 19 Sep 2021 by Hans Svensson
-module(aeu_crypto_tests).

-compile([export_all, nowarn_export_all]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

enacl_test_() ->
    {setup,
     fun() -> ok = application:ensure_started(crypto) end,
     fun(_) -> ok end,
     [{"Enacl signing",
       fun() ->
           #{public := Pub, secret := Priv} = enacl:sign_keypair(),
           Message = crypto:strong_rand_bytes(64),
           Signature = enacl:sign_detached(Message, Priv),
           ?assert(enacl:sign_verify_detached(Signature, Message, Pub)),

           StaticPub  = <<246,140,72,238,41,186,66,239,124,218,138,115,69,158,69,171,163,194,140,186,39,209,164,226,12,98,24,194,134,0,122,204>>,
           StaticPriv = <<131,67,249,129,201,243,37,202,91,210,80,199,52,52,248,19,247,218,216,121,41,137,152,133,247,188,147,10,2,61,223,96,246,140,72,238,41,186,66,239,124,218,138,115,69,158,69,171,163,194,140,186,39,209,164,226,12,98,24,194,134,0,122,204>>,
           StaticMsg  = <<209,83,231,149,39,195,28,78,228,55,21,148,180,98,87,8,176,8,175,18,181,105,200,188,123,154,95,91,109,89,147,137,145,230,53,77,96,13,68,61,248,101,245,98,64,22,92,173,78,177,104,177,11,47,163,229,73,92,20,94,153,154,115,234>>,
           StaticSig  = <<116,35,216,74,185,30,21,169,166,182,3,68,98,132,160,55,102,8,87,113,155,65,189,172,216,201,231,11,161,5,246,90,115,178,93,48,246,73,194,52,76,153,142,49,13,196,160,201,109,88,243,55,209,60,98,230,73,104,133,113,222,140,223,13>>,
           ?assertEqual(StaticSig, enacl:sign_detached(StaticMsg, StaticPriv)),

           ?assert(enacl:sign_verify_detached(StaticSig, StaticMsg, StaticPub))
       end},

      {"Enacl secretbox - nonce size",
       fun() ->
           ?assertEqual(24, enacl:secretbox_NONCEBYTES())
       end},

      {"Enacl secretbox",
       fun() ->
           StaticKey   = <<53,149,187,150,182,81,186,196,227,13,250,204,249,57,11,188,181,180,175,110,216,227,159,48,130,68,106,28,245,136,187,211>>,
           StaticNonce = <<179,182,115,60,11,184,182,240,220,125,156,232,11,197,177,220,85,95,59,41,127,60,49,160>>,
           Message     = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
           StaticBox   = <<134,6,79,169,32,80,168,70,37,172,70,94,16,244,105,153,143,4,249,31,152,211,79,38,60,212,83,74,104,88,102,135,73,171,187,174,230,35,71,67,50,133,145,126,180,190,105,181,121,186,165,127,132,102,211,250,75,110,106,174,52,148,98,253,240,181,144,62,141,121,141,79,233,184,236,22,161,76>>,
           Box         = enacl:secretbox(Message, StaticNonce, StaticKey),
           ?assertEqual(byte_size(StaticNonce), enacl:secretbox_NONCEBYTES()),
           ?assertEqual(Box, StaticBox),
           ?assertMatch({ok, Message}, enacl:secretbox_open(StaticBox, StaticNonce, StaticKey)),
           ok
       end},

      {"Enacl verify_32",
       fun() ->
           A = crypto:strong_rand_bytes(32),
           B = crypto:strong_rand_bytes(32),
           ?assert(enacl:verify_32(A, A)),
           ?assert(enacl:verify_32(B, B)),
           ?assertNot(enacl:verify_32(A, B)),
           ?assertNot(enacl:verify_32(B, A))
       end},

      {"Enacl hash (blake2b)",
       fun() ->
           StaticData   = <<89,199,79,141,149,221,0,175,13,88,31,208,216,157,234,13,9,170,129,154,135,218,205,203,103,30,239,207,246,240,202,233>>,
           StaticHash32 = <<149,207,37,128,131,108,217,217,85,9,146,49,223,17,139,45,33,83,42,109,201,238,20,21,69,85,230,253,22,32,213,205>>,
           Hash32 = enacl:generichash(32, StaticData),
           ?assertEqual(StaticHash32, Hash32)
       end},

      {"Enacl curve 25519 - signkey to curve",
       fun() ->
           StaticSignPub  = <<33,57,103,47,202,253,251,23,27,211,97,108,28,158,52,73,45,197,33,22,131,49,244,229,52,132,228,137,70,220,64,114>>,
           StaticSignPriv = <<237,151,168,246,216,111,89,12,210,127,132,82,104,60,230,40,29,53,81,244,37,6,131,133,115,168,95,181,134,95,100,78,33,57,103,47,202,253,251,23,27,211,97,108,28,158,52,73,45,197,33,22,131,49,244,229,52,132,228,137,70,220,64,114>>,
           StaticPub      = <<102,16,55,173,201,137,188,198,144,107,127,69,151,98,216,48,103,236,169,211,168,207,118,58,10,107,38,163,147,31,27,44>>,
           StaticPriv     = <<48,94,153,51,21,66,116,24,163,46,116,33,235,80,122,211,3,93,56,193,103,55,225,237,225,227,92,249,207,18,20,71>>,
           Pub  = enacl:crypto_sign_ed25519_public_to_curve25519(StaticSignPub),
           Priv = enacl:crypto_sign_ed25519_secret_to_curve25519(StaticSignPriv),
           ?assertEqual(StaticPub, Pub),
           ?assertEqual(StaticPriv, Priv)
        end},

      {"Enacl curve 25519 - scalarmult",
       fun() ->
           StaticPub      = <<102,16,55,173,201,137,188,198,144,107,127,69,151,98,216,48,103,236,169,211,168,207,118,58,10,107,38,163,147,31,27,44>>,
           StaticPriv     = <<48,94,153,51,21,66,116,24,163,46,116,33,235,80,122,211,3,93,56,193,103,55,225,237,225,227,92,249,207,18,20,71>>,

           Pub = enacl:curve25519_scalarmult_base(StaticPriv),
           ?assertEqual(StaticPub, Pub)
       end}
     ]}.

-endif.
