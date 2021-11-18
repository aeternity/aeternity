%%% File        : aeu_ed25519_musig2.erl
%%% Author      : Hans Svensson
%%% Description : Experimental muSig2 implementation for ed25519
%%% Created     : 12 Nov 2021 by Hans Svensson
-module(aeu_ed25519_musig2).

-compile([export_all, nowarn_export_all]).

%% Ed255129 points are represented by their X-coordinates and a single (the
%% highest one!) bit for sign of Y. To be able to validate the produced
%% signatures with the standard signature check we have to stick to points
%% with positive Y coordinate in some places.

%% enacl standard secret
init(<<Priv:32/bytes, _:32/bytes>>) -> init(Priv);
%% short secret
init(SK = <<_:32/bytes>>) ->
  #{public := Pub} = enacl:sign_seed_keypair(SK),

  EPub = aeser_api_encoder:encode(account_pubkey, Pub),
  io:format("Initialized with keypair for account: ~s\n", [EPub]),
  {EPub, #{secret => SK, id => EPub}}.


%% TODO: a proof of signature for OtherPubKeys?
sign_setup(#{secret := SK}, OtherPubKeys) ->
  #{public := Pub0} = enacl:sign_seed_keypair(SK),
  #{p := Pub, flipped := FlippedPK} = mk_pos_pt(Pub0),

  {#{p := APK, flipped := FlippedAPK}, MyExp} = aggregate_pks(Pub, maybe_decode(OtherPubKeys)),

  EAPK = aeser_api_encoder:encode(account_pubkey, APK),
  io:format("Multi-sig account: ~p\n", [EAPK]),
  #{secret => SK, pub => Pub, flipped_pub => FlippedPK, exp => MyExp,
    apk => APK, flipped_apk => FlippedAPK, n => length(OtherPubKeys) + 1}.

sign_step1(SignState = #{secret := SK, apk := APK, exp := Exp,
                         flipped_apk := FlippedAPK, flipped_pub := FlippedPK},
           Msg0, MyNonces, OtherNonces) ->
  Msg = maybe_decode(Msg0),
  MyNoncePts = [ sc_mul(N) || N <- MyNonces ],
  <<Seed0:32/bytes, _/binary>> = crypto:hash(sha512, SK),
  Seed = negate(FlippedPK, clamp(Seed0)),

  [N1, N2] = accumulate_nonces([MyNoncePts | maybe_decode(OtherNonces)]),

  _E1 = <<1:8, 0:248>>,
  E2 = hash_to_scalar(<<APK/binary, N1/binary, N2/binary, Msg/binary>>),
  AN = pt_add(N1, sc_mul(E2, N2)),

  Challenge = hash_to_scalar(<<AN/binary, APK/binary, Msg/binary>>),
  [N1s, N2s] = MyNonces,

  Ns = add_sc(N1s, mul_sc(E2, negate(not is_pos_y(N2), N2s))),
  S  = add_sc(mul_sc(Challenge, Exp, negate(FlippedAPK, Seed)), Ns),
  ES = aeser_api_encoder:encode(bytearray, S),
  {ES, SignState#{my_s => S, ss => [S], an => AN, msg => Msg}};
sign_step1(_SignState, _Msg, _MyNonces, _OtherNonces) ->
  error(bad_sign_state).

sign_add_s(SignState = #{ss := Ss, n := N}, S0) ->
  S = maybe_decode(S0),
  NewSs = lists:usort([S | Ss]),
  case length(NewSs) == N of
    true ->
      Stot = sum_scalars(NewSs),
      {all, SignState#{s => Stot, ss := NewSs}};
    false ->
      {incomplete, SignState#{ss := NewSs}}
  end.

sign_finish(SignState = #{an := AN, s := S}) ->
  Sig = <<AN:32/bytes, S:32/bytes>>,
  ESig = aeser_api_encoder:encode(signature, Sig),
  {ESig, SignState#{sig => Sig}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aggregate_pks(MyPK, PKs) ->
  %% Sort the keys, and ensure we are using the Y-positive variant
  SortedPKs = [ pos_pt(PK) || PK <- lists:sort([MyPK | PKs]) ],

  AllPKs = << PK || PK <- SortedPKs >>,

  Factors = [ sc_mul(compute_exponent(AllPKs, PK), PK) || PK <- SortedPKs ],

  AggregatedPK = sum_pts(Factors),

  {mk_pos_pt(AggregatedPK), compute_exponent(AllPKs, pos_pt(MyPK))}.

accumulate_nonces([[] | _]) -> [];
accumulate_nonces(NoncesLists) ->
  Ns = [ hd(L) || L <- NoncesLists ],
  Ts = [ tl(L) || L <- NoncesLists ],
  [sum_pts(Ns) | accumulate_nonces(Ts)].

aggregate_announcement(AggrPK, [Pt1, Pt2], Msg) ->
  Val = hash_to_scalar(<<AggrPK/binary, Pt1/binary, Pt2/binary, Msg/binary>>),

  E1 = <<1:8, 0:248>>,
  E2 = Val,

  AggrAnn = pt_add(Pt1, sc_mul(E2, Pt2)),
  {mk_pos_pt(AggrAnn), [E1, E2]}.

pt_add(A, B) -> enacl:crypto_ed25519_add(A, B).
sc_mul(S) -> enacl:crypto_ed25519_scalarmult_base_noclamp(S).
sc_mul(S, P) -> enacl:crypto_ed25519_scalarmult_noclamp(S, P).
mul_sc(A, B) -> enacl:crypto_ed25519_scalar_mul(A, B).
mul_sc(A, B, C) -> mul_sc(mul_sc(A, B), C).
add_sc(A, B) -> enacl:crypto_ed25519_scalar_add(A, B).
add_sc(A, B, C) -> add_sc(add_sc(A, B), C).
neg_sc(A) -> enacl:crypto_ed25519_scalar_negate(A).

negate(false, N) -> N;
negate(true, N)  -> neg_sc(N).

sum_pts([P | Ps]) ->
   lists:foldl(fun(X, A) -> enacl:crypto_ed25519_add(X, A) end, P, Ps).

sum_scalars([P | Ps]) ->
   lists:foldl(fun(X, A) -> enacl:crypto_ed25519_scalar_add(X, A) end, P, Ps).

compute_exponent(AllPKs, PK) ->
  hash_to_scalar(<<AllPKs/binary, PK/binary>>).

hash_to_scalar(Binary) ->
  enacl:crypto_ed25519_scalar_reduce(crypto:hash(sha512, Binary)).


maybe_decode(Xs) when is_list(Xs) -> [maybe_decode(X) || X <- Xs];
maybe_decode(X) when is_binary(X) ->
  try aeser_api_encoder:decode(X) of
    {_, Bin} -> Bin;
    _        -> X
  catch _:_ ->
          X
  end.

neg_pt(<<B0_30:31/bytes, B31>>) -> <<B0_30/bytes, (B31 bor 16#80):8>>.
pos_pt(<<B0_30:31/bytes, B31>>) -> <<B0_30/bytes, (B31 band 16#7f):8>>.

is_pos_y(<<_:31/bytes, B31>>) ->
  (B31 band 16#80) == 0.

mk_pos_pt(P) ->
  case is_pos_y(P) of
    true  -> #{p => P,         flipped => false};
    false -> #{p => pos_pt(P), flipped => true}
  end.

%% Clamp a 32-byte value - i.e clear the lowest three bits of the last byte and
%% clear the highest and set the second highest of the first byte
clamp(<<B0:8, B1_30:30/bytes, B31:8>>) ->
  <<(B0 band 16#f8):8, B1_30/bytes, ((B31 band 16#7f) bor 16#40):8>>.

%%%%
test() ->
  PrivA = crypto:strong_rand_bytes(32),
  PrivB = crypto:strong_rand_bytes(32),

  {PubA, StateInitA} = init(PrivA),
  {PubB, StateInitB} = init(PrivB),

  State1A = sign_setup(StateInitA, [PubB]),
  State1B = sign_setup(StateInitB, [PubA]),

  Nonces_A = [compute_nonce(), compute_nonce()],
  NoncePts_A = [ sc_mul(P) || P <- Nonces_A ],

  Nonces_B = [compute_nonce(), compute_nonce()],
  NoncePts_B = [ sc_mul(P) || P <- Nonces_B ],

  Msg = crypto:strong_rand_bytes(32),

  {SA, State2A} = sign_step1(State1A, Msg, Nonces_A, [NoncePts_B]),
  {SB, State2B} = sign_step1(State1B, Msg, Nonces_B, [NoncePts_A]),

  {all, State3A} = sign_add_s(State2A, SB),

  {ESig, State4A = #{apk := APK}} = sign_finish(State3A),

  Sig = maybe_decode(ESig),

  true = enacl:sign_verify_detached(Sig, Msg, APK).

compute_nonce() ->
  enacl:crypto_ed25519_scalar_reduce(crypto:strong_rand_bytes(64)).

