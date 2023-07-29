%%% File        : eaex3.erl
%%% Author      : Hans Svensson
%%% Description : Implementing AEX-3:
%%%               https://github.com/aeternity/AEXs/blob/master/AEXS/aex-3.md
%%%               Subset imported to aecore including only enough to read wallets
%%%               to avoid importing more deps to the core
%%% Created     : 6 Jan 2022 by Hans Svensson
-module(aec_eaex3).

-export([decrypt/2,
         read/2]).

read(File, Password) ->
  case file:read_file(File) of
    {ok, BinData} ->
      case jsx:decode(BinData, [return_maps]) of
        JSON when is_map(JSON) ->
          decrypt(JSON, Password);
        Err ->
          io:format("Error, could not read file: ~s\n  "
                    "Reason: JSON parse failed at ~p\n", [File, Err]),
          {error, invalid_json}
      end;
    {error, Reason} ->
      lager:error("Error: Could not read file: ~s: Reason ~p\n", [File, Reason]),
      {error, Reason}
  end.

decrypt(JSON, Password) ->
  try
    assert_version(1, JSON),
    Crypto       = get_crypto(JSON),
    UUID         = get_id(JSON),
    Comment      = get_name(JSON),
    KDF          = get_kdf(Crypto),
    SecretType   = get_secret(Crypto),
    {CAlgo, CText, COpts} = get_cipher(Crypto),

    PwHash = pwd_hash(KDF, Password),

    case decrypt_algo(CAlgo, CText, PwHash, COpts) of
      {ok, SecretMsg} ->
        {ok, #{id      => UUID,
               message => SecretMsg,
               type    => SecretType,
               comment => Comment}};
      {error, R} ->
        lager:error("Error, failed to decrypt: ~p\n", [R]),
        {error, {decrypt, R}}
    end
  catch throw:{ErrType, Reason} ->
    lager:info("Decrypt: ~200p\n", [JSON]),
    lager:error("Error, failed to decrypt, ~p: ~s\n", [ErrType, Reason]),
    {error, {ErrType, Reason}}
  end.

%% ---- Internal functions

%% ---- Key derivation and encrypt/decrypt

pwd_hash({argon2id, #{memlim := MemLimKb, opslim := OpsLim, salt := Salt}}, Password) ->
  enacl:pwhash(Password, Salt, OpsLim, MemLimKb * 1024);
pwd_hash({pbkdf2, #{rounds := Rounds, dklen := DKLen, hash := Hash, salt := Salt}}, Password) ->
  {ok, PwdHash} = epbkdf2:pbkdf2(Hash, Password, Salt, Rounds, DKLen),
  PwdHash.

decrypt_algo(chacha20_poly1305, CipherText, Key, #{nonce := Nonce}) ->
  <<MAC:16/binary, CMsg/binary>> = CipherText,
  case crypto:crypto_one_time_aead(chacha20_poly1305, Key, Nonce, CMsg, <<>>, MAC, false) of
    error -> {error, decrypt_failed};
    Msg   -> {ok, Msg}
  end.


%% ---- Get/Set parameters from/to JSON
get_crypto(# {<<"crypto">> := Crypto}) -> Crypto;
get_crypto(_) -> throw({missing_info, "crypto"}).

get_secret(#{<<"secret_type">> := <<"ed25519-slip0010-masterkey">>}) -> ed25519_slip0010_masterkey;
get_secret(#{<<"secret_type">> := <<"ed25519-bip39-mnemonic">>})     -> ed25519_bip39_mnemonic;
get_secret(# {<<"secret_type">> := Unknown}) -> throw({bad_secret_type, Unknown});
get_secret(_)                           -> throw({missing_info, "crypto/secret_type"}).

get_id(#{<<"id">> := UUID}) -> UUID;
get_id(_)               -> throw({missing_info, "id"}).

assert_version(ExpectedVsn, #{<<"version">> := ExpectedVsn}) -> ok;
assert_version(ExpectedVsn, #{<<"version">> := OtherVsn}) when is_integer(OtherVsn) ->
  throw({bad_version, "Got version " ++ integer_to_list(OtherVsn) ++
                      " expected " ++ integer_to_list(ExpectedVsn)});
assert_version(ExpectedVsn, #{<<"version">> := _BadVsn}) ->
  throw({bad_version, "Version should be a number (expecting " ++ integer_to_list(ExpectedVsn) ++ ")"});
assert_version(_ExpectedVsn, _) ->
  throw({missing_info, "version"}).

get_name(#{<<"name">> := Name}) -> Name;
get_name(_)                 -> "".

get_kdf(#{<<"kdf">> := KDFAlgo, <<"kdf_params">> := KDFParams}) ->
  case KDFAlgo of
    <<"argon2id">> -> get_kdfparams(argon2id, KDFParams);
    <<"pbkdf2">>   -> get_kdfparams(pbkdf2, KDFParams);
    _          -> throw({bad_kdf, KDFAlgo})
  end;
get_kdf(_) ->
  throw({missing_info, "crypto/kdf|kdf_params"}).

get_kdfparams(argon2id, #{<<"memlimit_kib">> := MemLimKb, <<"opslimit">> := OpsLim, <<"salt">> := Salt}) ->
  {argon2id, #{memlim => MemLimKb, opslim => OpsLim,
               salt => check_hex_to_bin("crypto/kdf_params/salt", Salt)}};
get_kdfparams(argon2id, _) ->
  throw({bad_kdf_params, "Expected: memlimit_kib, opslimit and salt"});
get_kdfparams(pbkdf2, #{<<"c">> := Rounds, <<"dklen">> := DKLen, <<"prf">> := Prf, <<"salt">> := Salt}) ->
  {pbkdf2, #{rounds => Rounds, dklen => DKLen, hash => check_pbkdf2_prf(Prf),
             salt => check_hex_to_bin("crypto/kdf_params/salt", Salt)}};
get_kdfparams(pbkdf2, _) ->
  throw({bad_kdf_params, "Expected: c, dklen, prf and salt"}).

get_cipher(#{<<"symmetric_alg">> := CipherAlg,
             <<"ciphertext">> := HexCipher,
             <<"cipher_params">> := CipherParams}) ->
  case CipherAlg of
    <<"chacha20-poly1305">> ->
      case CipherParams of
        #{<<"nonce">> := HexNonce} ->
          {chacha20_poly1305, check_hex_to_bin("crypto/ciphertext", HexCipher),
           #{nonce => check_hex_to_bin("crypto/cipher_params/nonce", HexNonce)}};
        _ ->
          throw({missing_info, "crypto/cipher_params/nonce"})
      end;
    _ ->
      throw({bad_cipher_algo, CipherAlg})
  end;
get_cipher(_) ->
  throw({missing_info, "crypto/symmetric_alg|ciphertext|cipher_params"}).

check_pbkdf2_prf(<<"hmac-", Hash/binary>>) ->
  case Hash of
    <<"sha256">> -> sha256;
    <<"sha512">> -> sha512;
    _ -> throw({bad_pbkdf2_hash_algo, Hash})
  end;
check_pbkdf2_prf(Prf) ->
  throw({bad_pbkdf2_prf, Prf}).

%% ---- HEX encode/decode
check_hex_to_bin(Param, MaybeHex) ->
  try
    hex_to_bin(MaybeHex)
  catch _:_ ->
    throw({bad_format, "'" ++ Param ++ "' should be hexadecimal"})
  end.

-spec hex_to_bin(Input :: binary()) -> binary().
hex_to_bin(S) ->
  hex_to_bin(S, []).
hex_to_bin(<<>>, Acc) ->
  list_to_binary(lists:reverse(Acc));
hex_to_bin(<<X,Y, T/binary>>, Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hex_to_bin(T, [V | Acc]).
