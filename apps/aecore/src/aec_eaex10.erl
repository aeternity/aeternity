-module(aec_eaex10).

-export([master_key/2,
         derive_aex10_from_seed/3, derive_aex10_from_seed/4,
         derive_aex10_from_masterkey/3, derive_aex10_from_masterkey/4,
         derive_path_from_seed/3,
         derive_path/2,
         private_key/1, public_key/1,
         encode_base58c/1, decode_base58c/1,
         private_to_public/1]).

-define(ED25519_SEED,   <<"ed25519 seed">>).

%% Example usage
%% node ./dist/index.js mnemonic-to-key "option spy reduce crazy edge normal escape first suggest dance myth silent"
%% 5c54edfd3552e36680472e85dafd2a4e9723f204e2e9654fd46acae715f89aadaba3292d5c1125dcf1a72aca526434475a859dd36463e9df99acc9f04d2c71de
%% ak_2JbFFRz1Ah8cE5FbJZzPzGYuTtuR26iCbJwF9nQEW9T7oujmhH
%%
%% io:format("~p~n",[ebip39:mnemonic_to_seed(<<"option spy reduce crazy edge normal escape first suggest dance myth silent">>, "")]).
%% <<181,117,19,72,54,174,189,199,25,65,215,149,199,125,114,171,38,178,3,202,164,
%%   50,216,172,221,3,243,239,154,155,110,191,141,196,226,254,183,134,77,123,189,
%%   144,77,171,70,44,245,199,189,28,143,234,85,112,33,171,55,231,31,152,110,120,
%%   229,141>>
%%
%% {ok, Seed} = epbkdf2:pbkdf2(sha512, Mnemonic, <<"mnemonic">>, 2048).
%% {ok,<<181,117,19,72,54,174,189,199,25,65,215,149,199,125,
%%       114,171,38,178,3,202,164,50,216,172,221,3,243,...>>}
%% A = aec_eaex10:derive_aex10_from_seed(Seed, 0,0).
%% #{chain_code =>
%%       <<12,50,145,220,40,191,247,92,128,28,181,73,125,229,57,33,
%%         90,202,46,43,105,88,186,45,172,130,11,53,...>>,
%%   child => 2147483648,curve => ed25519,depth => 5,
%%   fprint => <<197,237,153,25>>,
%%   priv_key =>
%%       <<92,84,237,253,53,82,227,102,128,71,46,133,218,253,42,78,
%%         151,35,242,4,226,233,101,79,212,106,202,231,...>>,
%%   pub_key => undefined}
%% aec_eaex10:public_key(A).
%% {ok,<<171,163,41,45,92,17,37,220,241,167,42,202,82,100,
%%       52,71,90,133,157,211,100,99,233,223,153,172,201,...>>}
%% {ok, Pub} = aec_eaex10:public_key(A).
%% {ok,<<171,163,41,45,92,17,37,220,241,167,42,202,82,100,
%%       52,71,90,133,157,211,100,99,233,223,153,172,201,...>>}
%% aeu_hex:bin_to_hex(Pub).
%% "ABA3292D5C1125DCF1A72ACA526434475A859DD36463E9DF99ACC9F04D2C71DE"
%% aeapi:format_account_pubkey(Pub).
%% <<"ak_2JbFFRz1Ah8cE5FbJZzPzGYuTtuR26iCbJwF9nQEW9T7oujmhH">>

-define(ED25519_ORDER, 16#1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED).

-define(HARD_OFFSET, 16#7FFFFFFF).

-type curve() :: ed25519.

-type binary_32() :: <<_:256>>.

-type derived_key() ::
  #{ curve      := curve(),
     depth      := non_neg_integer(),
     child      := non_neg_integer(),
     fprint     := <<_:32>>,
     priv_key   := binary_32(),
     pub_key    := undefined | binary_32() | <<_:264>>,
     chain_code := binary_32() }.

-spec private_key(DKey :: derived_key()) -> {ok, binary_32()} | no_private_key.
private_key(#{priv_key := undefined}) ->
  no_private_key;
private_key(#{priv_key := PrivKey}) ->
  {ok, PrivKey}.

-spec public_key(DKey :: derived_key()) -> {ok, binary_32()}.
public_key(DKey = #{pub_key := undefined}) ->
  public_key(private_to_public(DKey));
public_key(#{pub_key := PubKey}) ->
  {ok, PubKey}.

-spec private_to_public(DKey :: derived_key()) -> derived_key().
private_to_public(DKey = #{curve := Curve, pub_key := undefined,
                           priv_key := <<PrivateKey:32/binary>>}) ->
  DKey#{pub_key := private_to_public(Curve, PrivateKey)};
private_to_public(DKey) ->
  DKey. %% Public key field is already populated

-spec master_key(Curve :: curve(), Seed :: binary()) -> derived_key().
master_key(ed25519 = Curve, Seed) when byte_size(Seed) >= 16, byte_size(Seed) =< 64 ->
  <<ILeft:256, IRight:32/bytes>> = crypto:mac(hmac, sha512, curve_key(Curve), Seed),
  #{curve => Curve, depth => 0, fprint => <<0:32>>, child => 0,
    priv_key => <<ILeft:256>>, pub_key => undefined, chain_code => IRight}.

-spec derive_aex10_from_seed(Seed         :: binary(),
                             AccountIndex :: non_neg_integer(),
                             AddressIndex :: non_neg_integer()) -> derived_key().
derive_aex10_from_seed(Seed, AccountIndex, AddressIndex) ->
  derive_aex10_from_seed(Seed, AccountIndex, 0, AddressIndex).

-spec derive_aex10_from_seed(Seed         :: binary(),
                             AccountIndex :: non_neg_integer(),
                             Change       :: non_neg_integer(),
                             AddressIndex :: non_neg_integer()) -> derived_key().
derive_aex10_from_seed(Seed, AccountIndex, Change, AddressIndex) ->
  Path = "m/44H/457H/" ++ integer_to_list(AccountIndex) ++ "H/" ++
    integer_to_list(Change) ++ "H/" ++ integer_to_list(AddressIndex) ++ "H",
  derive_path_from_seed(ed25519, Path, Seed).

-spec derive_aex10_from_masterkey(MasterKey    :: derived_key(),
                                  AccountIndex :: non_neg_integer(),
                                  AddressIndex :: non_neg_integer()) -> derived_key().
derive_aex10_from_masterkey(MasterKey, AccountIndex, AddressIndex) ->
  derive_aex10_from_masterkey(MasterKey, AccountIndex, 0, AddressIndex).

-spec derive_aex10_from_masterkey(MasterKey    :: derived_key(),
                                  AccountIndex :: non_neg_integer(),
                                  Change       :: non_neg_integer(),
                                  AddressIndex :: non_neg_integer()) -> derived_key().
derive_aex10_from_masterkey(MasterKey, AccountIndex, Change, AddressIndex) ->
  Path = "m/44H/457H/" ++ integer_to_list(AccountIndex) ++ "H/" ++
    integer_to_list(Change) ++ "H/" ++ integer_to_list(AddressIndex) ++ "H",
  derive_path(Path, MasterKey).

-spec derive_path_from_seed(Curve :: curve(),
                            Path  :: string(),
                            Seed  :: binary()) -> derived_key().
derive_path_from_seed(Curve, Path, Seed) ->
  DKey = master_key(Curve, Seed),
  derive_path(Path, DKey).

-spec derive_path(Path  :: string(),
                  DKey0 :: derived_key()) -> derived_key().
derive_path(Path, DKey0) ->
  case {Path, key_type(DKey0)} of
    {"m" ++ Path1, private} ->
      derive_private_path(Path1, DKey0);
    {"m" ++ _Path1, public} ->
      error({invalid_derivation, "Can't derive private keys from a public key"});
    {"M" ++ Path1, private} ->
      DKey1 = derive_private_path(Path1, DKey0),
      private_to_public(DKey1);
    _ ->
      error({invalid_path, "Expecting path to start with 'm/' or 'M/'"})
  end.

-spec encode_base58c(Bin :: binary()) -> binary().
encode_base58c(Bin) ->
  C = check_str(Bin),
  binary_to_base58(iolist_to_binary([Bin, C])).

-spec decode_base58c(Bin :: binary()) -> binary().
decode_base58c(Bin) ->
  DecodedBin = base58_to_binary(Bin),
  Sz = byte_size(DecodedBin),
  BSz = Sz - 4,
  <<Body:BSz/binary, C:4/binary>> = DecodedBin,
  C = check_str(Body),
  Body.

%% --- Internal functions

private_to_public(ed25519, PrivateKey) ->
  #{public := PublicKey} = enacl:sign_seed_keypair(PrivateKey),
  PublicKey.

curve_key(ed25519)   -> ?ED25519_SEED.

parse_segment(Segment) ->
  case lists:reverse(Segment) of
    "H" ++ S -> {hard, list_to_integer(lists:reverse(S))};
    _        -> {normal, list_to_integer(Segment)}
  end.

key_type(#{priv_key := <<_/bytes>>})                    -> private;
key_type(#{pub_key := PubKey}) when PubKey /= undefined -> public.

binary_to_base58(Bin) ->
    iolist_to_binary(base58:binary_to_base58(Bin)).

base58_to_binary(Bin) when is_binary(Bin) ->
    base58:base58_to_binary(binary_to_list(Bin)).

check_str(Bin) ->
    <<C:32/bitstring,_/binary>> =
        sha256_hash(sha256_hash(Bin)),
    C.

sha256_hash(Bin) -> crypto:hash(sha256, Bin).

derive_private_path(Path, DKey) ->
  Segments = lists:map(fun parse_segment/1, string:lexemes(Path, "/")),
  lists:foldl(fun derive_private_path_segment/2, DKey, Segments).

derive_private_path_segment(Ix, DKey = #{curve := Curve}) ->
  derive_private_path_segment(Curve, Ix, DKey).

derive_private_path_segment(ed25519, {normal, _Ix}, _DKey) ->
  error({invalid_derivation, "Can't derive normal path for ed25519"});
derive_private_path_segment(ed25519, {hard, Ix0}, DKey = #{priv_key := Key, chain_code := CC}) ->
  Ix = Ix0 + ?HARD_OFFSET + 1,
  Data = <<0:8, Key:32/bytes, Ix:32>>,
  <<ILeft:256, IRight:32/bytes>> = crypto:mac(hmac, sha512, CC, Data),
  DKey#{priv_key := <<ILeft:256>>, pub_key := undefined, chain_code := IRight,
        depth := maps:get(depth, DKey) + 1, child := Ix,
        fprint := fingerprint(DKey)}.

fingerprint(DKey = #{pub_key := undefined}) ->
  fingerprint(private_to_public(DKey));
fingerprint(#{curve := ed25519, pub_key := PubKey}) ->
  %% https://github.com/satoshilabs/slips/issues/1251
  %% An extra 0-byte is inserted first for some reason.
  <<FP:4/bytes, _/bytes>> = crypto:hash(ripemd160, crypto:hash(sha256, <<0:8, PubKey/binary>>)),
  FP.


