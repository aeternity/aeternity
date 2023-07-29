-module(aec_eaex10_tests).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

aeax10_slip10_derivation_ok_test() ->
  Test = fun({Type, Seed, DerivationPath, FP, CC, Priv, Pub}) ->
             ?debugFmt("Test derive: ~s", [DerivationPath]),
             DKey = aec_eaex10:derive_path_from_seed(Type, DerivationPath, hex_to_bin(Seed)),
             DKeyPub = aec_eaex10:private_to_public(DKey),
             PrivBin = hex_to_bin(Priv),
             ?assertEqual(hex_to_bin(CC), maps:get(chain_code, DKey)),
             ?assertEqual(hex_to_bin(FP), maps:get(fprint, DKey)),
             ?assertEqual(<<PrivBin/bytes>>, maps:get(priv_key, DKey)),
             ?assertEqual(hex_to_bin(Pub), maps:get(pub_key, DKeyPub))
         end,
  [ Test(V) || V <- test_vectors_aex10_slip10() ].

aeax10_derivation_ok_test() ->
  Test = fun({Seed, {AcIx, AdIx}, FP, CC, Priv, Pub}) ->
             ?debugFmt("Test derive: account ~p address: ~p", [AcIx, AdIx]),
             DKey = aec_eaex10:derive_aex10_from_seed(hex_to_bin(Seed), AcIx, AdIx),
             DKeyPub = aec_eaex10:private_to_public(DKey),
             PrivBin = hex_to_bin(Priv),
             ?assertEqual(hex_to_bin(CC), maps:get(chain_code, DKey)),
             ?assertEqual(hex_to_bin(FP), maps:get(fprint, DKey)),
             ?assertEqual(<<PrivBin/bytes>>, maps:get(priv_key, DKey)),
             ?assertEqual(hex_to_bin(Pub), maps:get(pub_key, DKeyPub))
         end,
  [ Test(V) || V <- test_vectors_aex10() ].

%% Test vectors from SLIP0010: https://github.com/satoshilabs/slips/blob/master/slip-0010.md
test_vectors_slip10() ->
  [{ ed25519,
     "000102030405060708090a0b0c0d0e0f",
     "m",
     "00000000",
     "90046a93de5380a72b5e45010748567d5ea02bbf6522f979e05c0d8d8ca9fffb",
     "2b4be7f19ee27bbf30c667b642d5f4aa69fd169872f8fc3059c08ebae2eb19e7",
     "00a4b2856bfec510abab89753fac1ac0e1112364e7d250545963f135f2a33188ed"},
   { ed25519,
     "000102030405060708090a0b0c0d0e0f",
     "m/0H",
     "ddebc675",
     "8b59aa11380b624e81507a27fedda59fea6d0b779a778918a2fd3590e16e9c69",
     "68e0fe46dfb67e368c75379acec591dad19df3cde26e63b93a8e704f1dade7a3",
     "008c8a13df77a28f3445213a0f432fde644acaa215fc72dcdf300d5efaa85d350c"},
   { ed25519,
     "000102030405060708090a0b0c0d0e0f",
     "m/0H/1H",
     "13dab143",
     "a320425f77d1b5c2505a6b1b27382b37368ee640e3557c315416801243552f14",
     "b1d0bad404bf35da785a64ca1ac54b2617211d2777696fbffaf208f746ae84f2",
     "001932a5270f335bed617d5b935c80aedb1a35bd9fc1e31acafd5372c30f5c1187"},
   { ed25519,
     "000102030405060708090a0b0c0d0e0f",
     "m/0H/1H/2H",
     "ebe4cb29",
     "2e69929e00b5ab250f49c3fb1c12f252de4fed2c1db88387094a0f8c4c9ccd6c",
     "92a5b23c0b8a99e37d07df3fb9966917f5d06e02ddbd909c7e184371463e9fc9",
     "00ae98736566d30ed0e9d2f4486a64bc95740d89c7db33f52121f8ea8f76ff0fc1"},
   { ed25519,
     "000102030405060708090a0b0c0d0e0f",
     "m/0H/1H/2H/2H",
     "316ec1c6",
     "8f6d87f93d750e0efccda017d662a1b31a266e4a6f5993b15f5c1f07f74dd5cc",
     "30d1dc7e5fc04c31219ab25a27ae00b50f6fd66622f6e9c913253d6511d1e662",
     "008abae2d66361c879b900d204ad2cc4984fa2aa344dd7ddc46007329ac76c429c"},
   { ed25519,
     "000102030405060708090a0b0c0d0e0f",
     "m/0H/1H/2H/2H/1000000000H",
     "d6322ccd",
     "68789923a0cac2cd5a29172a475fe9e0fb14cd6adb5ad98a3fa70333e7afa230",
     "8f94d394a8e8fd6b1bc2f3f49f5c47e385281d5c17e65324b0f62483e37e8793",
     "003c24da049451555d51a7014a37337aa4e12d41e485abccfa46b47dfb2af54b7a"}
  ].

%% NOTE: SLIP0010 is fuzzy about public key format, they prepend the ed25519
%% public key with a 0x00 byte. This affects the fingerprint(s). Here we
%% omit the 0-byte, but add it when computing fingerprints.
test_vectors_aex10_slip10() ->
  [ { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/0H/0H/0H",
      "0AC70E0D",
      "55DABD8682B4ED808AD21AF8798B34905EE8E1A0057F15DC151A8487CFAB630F",
      "624357924DABE9460677F574853CBE017B5506E53F41EC0179AFCA9519957925",
      "9A3D5001D37C85E72A953B1C0EB7C5BC8B5FBD4640AA4D488FA029CD526F206B"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/0H/0H/17H",
      "0AC70E0D",
      "98CAA03190E8F23CE5362ED3F3EAFCB4FB9A08B9E9329DC168BCB3EF563283BF",
      "E2ED1D3BC80DB4644D5D855E60791E72C2E98AA69ED4D024C659FF642D8F6B77",
      "833A6227139952868E49BBBDAAB17852C002B1651CC8A633F623A554A59A00FE"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/0H/0H/921H",
      "0AC70E0D",
      "BF9F7535FDD146027D732D4BB1FCE9309CF1561DFDDA6ACD68C399BBFB09828D",
      "00D113B53996EB8E0EC52FD41FD8EFE5E6719519106FED54416A3243DC603C7E",
      "9DEE1871C20AEF56672B1BAA9C521FABEDA048B0612330E92C9A1B967A4732D6"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/42H/0H/0H",
      "FB2212CF",
      "B2102A2258CB55AB3A4E20241D373AEEC92803666A4DDCED158B22406B6726BA",
      "F54EA1206A4DD02D4DC59EFFDBD89A7524E5BD719D3CCAC2F28F3CFA8D6A24C2",
      "5331419AA320C8DED7AD86DD63935692D384C4A82A0D83EDAC8093FAEB36BF2E"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/42H/0H/17H",
      "FB2212CF",
      "6F90E396148F2D769CC4791E41B3107EF530302D682CE52D3945BCCF5CFFA324",
      "5CBD84C82F04F791C5B262C933602456EA9BCDB74B2E738F7A06BF5A72598A34",
      "FD4F4FB851911073D6E1A0F7255A68C4A189824724DAB27A6260E6DEF3762A11"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/42H/0H/921H",
      "FB2212CF",
      "BABF55A4CA273CB0F889FB593AC5877E5B0C78EC645A17E193FE396B28306499",
      "C28EDF7DB1D3093D2254945E162E10CD3AB3D6862BEC9514C090DF5228ADB478",
      "44C3488A82A8D75EBF386E3027A7FA74F8102889E22B01951BE1022D2208ECE9"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/123H/0H/0H",
      "CADEACEF",
      "805C6C28D3FBB45E52D5CB37EDAE805EAF57CA408E79145B2E5F7861FA0A740F",
      "D399B502D691A0E07FD8B336C354BC5D5703B2B0DEC2D9894689E5810B88C47C",
      "8AB44B79F131B3F7CC9A19BFCFB0D7794720A1A9CAE9350DCCDD6702A1900E0D"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/123H/0H/17H",
      "CADEACEF",
      "F09AB6EEDD59084D9DAF915B32AC761F290DACC857BC64CC2613D139B2211081",
      "EEA0D75662F803629108A526EF0A69E3B5FB18CBE33A8E0CF0406D5617C82B23",
      "A53F0BEC67A82825C49C2A55DE3C56F0614D1581CA3B1452B2BFCB08E2DD6DCC"},
    { ed25519,
      "000102030405060708090a0b0c0d0e0f",
      "m/44H/457H/123H/0H/921H",
      "CADEACEF",
      "D27BB9AF44305B765AF39AE8A1D093C9B68331DCE961E71BD77807359C6ABF8C",
      "CA2298DC98D85E553833453806FEAE6918B2F672C7C1D6775A5F4C1B61332102",
      "3AC0548036589278CAD9000691D4589FF8CF5A687C27B3ACD2B6BC75D1FB518E"}
  ].

%% NOTE: SLIP0010 is fuzzy about public key format, they prepend the ed25519
%% public key with a 0x00 byte. This affects the fingerprint(s). Here we
%% omit the 0-byte, but add it when computing fingerprints.
test_vectors_aex10() ->
  [ { "000102030405060708090a0b0c0d0e0f",
      {0, 0},
      "0AC70E0D",
      "55DABD8682B4ED808AD21AF8798B34905EE8E1A0057F15DC151A8487CFAB630F",
      "624357924DABE9460677F574853CBE017B5506E53F41EC0179AFCA9519957925",
      "9A3D5001D37C85E72A953B1C0EB7C5BC8B5FBD4640AA4D488FA029CD526F206B"},
    { "000102030405060708090a0b0c0d0e0f",
      {0, 17},
      "0AC70E0D",
      "98CAA03190E8F23CE5362ED3F3EAFCB4FB9A08B9E9329DC168BCB3EF563283BF",
      "E2ED1D3BC80DB4644D5D855E60791E72C2E98AA69ED4D024C659FF642D8F6B77",
      "833A6227139952868E49BBBDAAB17852C002B1651CC8A633F623A554A59A00FE"},
    { "000102030405060708090a0b0c0d0e0f",
      {0, 921},
      "0AC70E0D",
      "BF9F7535FDD146027D732D4BB1FCE9309CF1561DFDDA6ACD68C399BBFB09828D",
      "00D113B53996EB8E0EC52FD41FD8EFE5E6719519106FED54416A3243DC603C7E",
      "9DEE1871C20AEF56672B1BAA9C521FABEDA048B0612330E92C9A1B967A4732D6"},
    { "000102030405060708090a0b0c0d0e0f",
      {42, 0},
      "FB2212CF",
      "B2102A2258CB55AB3A4E20241D373AEEC92803666A4DDCED158B22406B6726BA",
      "F54EA1206A4DD02D4DC59EFFDBD89A7524E5BD719D3CCAC2F28F3CFA8D6A24C2",
      "5331419AA320C8DED7AD86DD63935692D384C4A82A0D83EDAC8093FAEB36BF2E"},
    { "000102030405060708090a0b0c0d0e0f",
      {42, 17},
      "FB2212CF",
      "6F90E396148F2D769CC4791E41B3107EF530302D682CE52D3945BCCF5CFFA324",
      "5CBD84C82F04F791C5B262C933602456EA9BCDB74B2E738F7A06BF5A72598A34",
      "FD4F4FB851911073D6E1A0F7255A68C4A189824724DAB27A6260E6DEF3762A11"},
    { "000102030405060708090a0b0c0d0e0f",
      {42, 921},
      "FB2212CF",
      "BABF55A4CA273CB0F889FB593AC5877E5B0C78EC645A17E193FE396B28306499",
      "C28EDF7DB1D3093D2254945E162E10CD3AB3D6862BEC9514C090DF5228ADB478",
      "44C3488A82A8D75EBF386E3027A7FA74F8102889E22B01951BE1022D2208ECE9"},
    { "000102030405060708090a0b0c0d0e0f",
      {123, 0},
      "CADEACEF",
      "805C6C28D3FBB45E52D5CB37EDAE805EAF57CA408E79145B2E5F7861FA0A740F",
      "D399B502D691A0E07FD8B336C354BC5D5703B2B0DEC2D9894689E5810B88C47C",
      "8AB44B79F131B3F7CC9A19BFCFB0D7794720A1A9CAE9350DCCDD6702A1900E0D"},
    { "000102030405060708090a0b0c0d0e0f",
      {123, 17},
      "CADEACEF",
      "F09AB6EEDD59084D9DAF915B32AC761F290DACC857BC64CC2613D139B2211081",
      "EEA0D75662F803629108A526EF0A69E3B5FB18CBE33A8E0CF0406D5617C82B23",
      "A53F0BEC67A82825C49C2A55DE3C56F0614D1581CA3B1452B2BFCB08E2DD6DCC"},
    { "000102030405060708090a0b0c0d0e0f",
      {123, 921},
      "CADEACEF",
      "D27BB9AF44305B765AF39AE8A1D093C9B68331DCE961E71BD77807359C6ABF8C",
      "CA2298DC98D85E553833453806FEAE6918B2F672C7C1D6775A5F4C1B61332102",
      "3AC0548036589278CAD9000691D4589FF8CF5A687C27B3ACD2B6BC75D1FB518E"}
  ].

%% mk_aex_test_vectors_test() ->
%%   BasePath = "m/44H/457H/",
%%   Seed = "000102030405060708090a0b0c0d0e0f",
%%   AccountIxs = ["0H", "42H", "123H"],
%%   AddressIxs = ["0H", "17H", "921H"],
%%   ?debugFmt("", []),
%%   ?debugFmt("Test vector(s) for AEX-10", []),
%%   ?debugFmt("", []),
%%   ?debugFmt("Seed (hex): ~s", [Seed]),
%%   Derive = fun(AcIx, AdIx) ->
%%       Path = BasePath ++ AcIx ++ "/0H/" ++ AdIx,
%%       DKey = eaex10:private_to_public(eaex10:derive_path_from_seed(ed25519, Path, hex_to_bin(Seed))),
%%       ?debugFmt("  Chain: ~s", [Path]),
%%       ?debugFmt("    - Fingerprint: ~s", [bin_to_hex(maps:get(fprint, DKey))]),
%%       ?debugFmt("    - Chain code:  ~s", [bin_to_hex(maps:get(chain_code, DKey))]),
%%       ?debugFmt("    - Private:     ~s", [bin_to_hex(maps:get(priv_key, DKey))]),
%%       ?debugFmt("    - Public:      ~s", [bin_to_hex(maps:get(pub_key, DKey))])
%%       ?debugFmt("\n{ ed25519,\n  \"~s\",\n  \"~s\",\n  \"~s\",\n  \"~s\",\n  \"~s\",\n  \"~s\"},",
%%         [Seed, Path, bin_to_hex(maps:get(fprint, DKey)), bin_to_hex(maps:get(chain_code, DKey)),
%%          bin_to_hex(maps:get(priv_key, DKey)), bin_to_hex(maps:get(pub_key, DKey))])
%%     end,
%%   [ Derive(A, B) || A <- AccountIxs, B <- AddressIxs ].

-spec hex_to_bin(Input :: string()) -> binary().
hex_to_bin(S) ->
  hex_to_bin(S, []).
hex_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hex_to_bin(T, [V | Acc]).

-spec bin_to_hex(Input :: binary()) -> string().
bin_to_hex(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
