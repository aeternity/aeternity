%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aeu_blake2b module
%%%
%%%   In addition the aeu_blake2b module was compared to the C reference
%%%   implementation by writing a QuickCheck property.
%%% @end
%%%=============================================================================
-module(aeu_blake2b_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

blake2b_test_() ->
    {"Tests for BLAKE2b hash implementation",
     [ fun() -> blake2b(Data) end || Data <- test_data_blake2b() ]}.

blake2b({Msg0, Key0, ExpectedOut0}) ->
    Msg = mk_binary(Msg0),
    Key = mk_binary(Key0),
    ExpectedOut = mk_binary(ExpectedOut0),
    Result = aeu_blake2b:blake2b(Msg, Key, byte_size(ExpectedOut)),
    ?assertEqual(Result, ExpectedOut).

mk_binary(Bin) when is_binary(Bin) -> Bin;
mk_binary(HexStr) when is_list(HexStr) ->
    << << (erlang:list_to_integer([H], 16)):4 >> || H <- HexStr >>.

test_data_blake2b() ->
    [ %% {Message, Key, ExpectedHash}
      %% From Wikipedia
      %% https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2
      {<<>>,
       <<>>,
       "786A02F742015903C6C6FD852552D272912F4740E15847618A86E217F71F5419D25E1031AFEE585313896444934EB04B903A685B1448B755D56F701AFE9BE2CE"}
    , {<<"The quick brown fox jumps over the lazy dog">>,
       <<>>,
       "A8ADD4BDDDFD93E4877D2746E62817B116364A1FA7BC148D95090BC7333B3673F82401CF7AA2E4CB1ECD90296E3F14CB5413F8ED77BE73045B13914CDCD6A918"}

      %% From reference implementation testvectors
      %% https://github.com/BLAKE2/BLAKE2/tree/master/testvectors
      %%
      %% Non-keyed
    , {"00",
        "",
        "2FA3F686DF876995167E7C2E5D74C4C7B6E48F8068FE0E44208344D480F7904C36963E44115FE3EB2A3AC8694C28BCB4F5A0F3276F2E79487D8219057A506E4B"}
    , {"0001",
       "",
       "1C08798DC641ABA9DEE435E22519A4729A09B2BFE0FF00EF2DCD8ED6F8A07D15EAF4AEE52BBF18AB5608A6190F70B90486C8A7D4873710B1115D3DEBBB4327B5"}
    , {"00010203040506070809",
       "",
       "29102511D749DB3CC9B4E335FA1F5E8FACA8421D558F6A3F3321D50D044A248BA595CFC3EFD3D2ADC97334DA732413F5CBF4751C362BA1D53862AC1E8DABEEE8"}

      %% Keyed
    , {"",
       "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f",
       "10ebb67700b1868efb4417987acf4690ae9d972fb7a590c2f02871799aaa4786b5e996e8f0f4eb981fc214b005f42d2ff4233499391653df7aefcbc13fc51568"}
    , {"00",
       "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f",
       "961f6dd1e4dd30f63901690c512e78e4b45e4742ed197c3c5e45c549fd25f2e4187b0bc9fe30492b16b0d0bc4ef9b0f34c7003fac09a5ef1532e69430234cebd"}
    , {"0001",
       "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f",
       "da2cfbe2d8409a0f38026113884f84b50156371ae304c4430173d08a99d9fb1b983164a3770706d537f49e0c916d9f32b95cc37a95b99d857436f0232c88a965"}
    , {"00010203040506070809",
       "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f",
       "4fe181f54ad63a2983feaaf77d1e7235c2beb17fa328b6d9505bda327df19fc37f02c4b6f0368ce23147313a8e5738b5fa2a95b29de1c7f8264eb77b69f585cd"}
    ].


-endif.
