-module(aec_eaex3_tests).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

read_mnemonic_test() ->
  {ok, Secret} = eaex3:read("test/data/test1_aex3.json", "passwd"),
  ?assertEqual(ed25519_bip39_mnemonic, maps:get(type, Secret)),
  ?assertEqual(<<"option spy reduce crazy edge normal escape first suggest dance myth silent">>, maps:get(message, Secret)).

read_mnemonic_empty_passwd_test() ->
  {ok, Secret} = eaex3:read("test/data/test1_aex3.json", ""),
  ?assertEqual(ed25519_bip39_mnemonic, maps:get(type, Secret)),
  ?assertEqual(<<"option spy reduce crazy edge normal escape first suggest dance myth silent">>, maps:get(message, Secret)).

