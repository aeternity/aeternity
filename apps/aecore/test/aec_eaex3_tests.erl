-module(aec_eaex3_tests).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

read_mnemonic_test() ->
  File = filename:join(code:lib_dir(aecore), "test/data/test1_aex3.json"),
  {ok, Secret} = aec_eaex3:read(File, "passwd"),
  ?assertEqual(ed25519_bip39_mnemonic, maps:get(type, Secret)),
  ?assertEqual(<<"option spy reduce crazy edge normal escape first suggest dance myth silent">>, maps:get(message, Secret)).

read_mnemonic_empty_passwd_test() ->
  File = filename:join(code:lib_dir(aecore), "test/data/test2_aex3.json"),
  {ok, Secret} = aec_eaex3:read(File, ""),
  ?assertEqual(ed25519_bip39_mnemonic, maps:get(type, Secret)),
  ?assertEqual(<<"option spy reduce crazy edge normal escape first suggest dance myth silent">>, maps:get(message, Secret)).

