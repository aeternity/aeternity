%%% File        : aeu_ed25519_multisig.erl
%%% Author      : Hans Svensson
%%% Description : Experimental ed25519-multisig
%%% Created     : 1 Oct 2021 by Hans Svensson
-module(aeu_ed25519_multisig).

-compile([export_all, nowarn_export_all]).

init(<<Priv:32/bytes, _:32/bytes>>) -> init(Priv);
init(SK = <<_:32/bytes>>) ->
  #{public := Pub} = enacl:sign_seed_keypair(SK),

  EPub = aeser_api_encoder:encode(account_pubkey, Pub),
  io:format("Created keypair for account: ~s\n", [EPub]),
  {EPub, #{secret => SK, id => EPub}}.

%% TODO: a proof of signature for OtherPubKeys?
sign_setup(#{secret := SK}, OtherPubKeys) ->
  #{public := Pub} = enacl:sign_seed_keypair(SK),
  PubAll = lists:foldl(fun enacl:crypto_ed25519_add/2, Pub, maybe_decode(OtherPubKeys)),
  EPubAll = aeser_api_encoder:encode(account_pubkey, PubAll),
  io:format("Multi-sig account: ~p\n", [EPubAll]),
  #{secret => SK, my_pub => Pub, pub => PubAll, n_pubs => length(OtherPubKeys) + 1}.

sign_step1(SignState = #{secret := SK}, Message0) ->
  Message = maybe_decode(Message0),
  <<_:32/bytes, H:32/bytes>> = crypto:hash(sha512, SK),
  U = crypto:strong_rand_bytes(32),

  R = enacl:crypto_ed25519_scalar_reduce(crypto:hash(sha512, <<H/bytes, U/bytes, Message/bytes>>)),

  RG = enacl:crypto_ed25519_scalarmult_base_noclamp(R),
  ERG = aeser_api_encoder:encode(bytearray, RG),
  {ERG, SignState#{r => R, rgs => [RG], msg => Message}};
sign_step1(_SignState, _Message) ->
  error(bad_sign_state).

sign_add_rg(SignState = #{rgs := RGs, n_pubs := NPubs}, RG0) ->
  RG = maybe_decode(RG0),
  NewRGs = lists:usort([RG | RGs]),
  case length(NewRGs) == NPubs of
    true ->
      RGAll  = lists:foldl(fun enacl:crypto_ed25519_add/2, RG, RGs),
      {all, SignState#{rg => RGAll, rgs := NewRGs}};
    false ->
      {incomplete, SignState#{rgs := NewRGs}}
  end.

sign_step2(SignState = #{secret := SK, r := R, rg := RGAll, pub := PubAll, msg := Message}) ->
  <<Seed0:32/bytes, _:32/bytes>> = crypto:hash(sha512, SK),
  Seed = clamp(Seed0),

  K = enacl:crypto_ed25519_scalar_reduce(crypto:hash(sha512, <<RGAll/bytes, PubAll/bytes, Message/bytes>>)),
  S = enacl:crypto_ed25519_scalar_add(R, enacl:crypto_ed25519_scalar_mul(K, Seed)),
  ES = aeser_api_encoder:encode(bytearray, S),
  {ES, SignState#{rg := RGAll, ss => [S]}};
sign_step2(_SignState) ->
  error(bad_sign_state).

sign_add_s(SignState = #{ss := Ss, n_pubs := NPubs}, S0) ->
  S = maybe_decode(S0),
  NewSs = lists:usort([S | Ss]),
  case length(NewSs) == NPubs of
    true ->
      SAll = lists:foldl(fun enacl:crypto_ed25519_scalar_add/2, S, Ss),
      {all, SignState#{s => SAll, ss := NewSs}};
    false ->
      {incomplete, SignState#{ss := NewSs}}
  end.

sign_finish(SignState = #{rg := RGAll, s := SAll}) ->
  Sig = <<RGAll:32/bytes, SAll:32/bytes>>,
  ESig = aeser_api_encoder:encode(signature, Sig),
  {ESig, SignState#{sig => Sig, s := SAll}}.

%% Clamp a 32-byte value - i.e clear the lowest three bits of the last byte and
%% clear the highest and set the second highest of the first byte
clamp(<<B0:8, B1_30:30/bytes, B31:8>>) ->
  <<(B0 band 16#f8):8, B1_30/bytes, ((B31 band 16#7f) bor 16#40):8>>.

maybe_decode(Xs) when is_list(Xs) -> [maybe_decode(X) || X <- Xs];
maybe_decode(X) when is_binary(X) ->
  try aeser_api_encoder:decode(X) of
    {_, Bin} -> Bin;
    _        -> X
  catch _:_ ->
          X
  end.

%% -------------------------
%% Simplistic multisig actor
%%
%% -------------------------

-type multisig_actor() :: {binary(), pid()}.
-define(LOG(S, Fmt, Args), log(S, Fmt, Args)).

-spec actor_new(SecretKey :: binary()) -> multisig_actor().
actor_new(SecretKey) ->
  {EPub, State} = init(SecretKey),
  {EPub, spawn(fun() -> actor_init(State) end)}.

actor_add_user(Actor, NewUser) ->
  actor_rpc(Actor, {new_user, NewUser}).

actor_new_sig(Actor, Id, Message) ->
  actor_rpc(Actor, {new_sig, Id, Message}).

actor_get_sig(Actor, Id) ->
  actor_rpc(Actor, {get_sig, Id}).

actor_stop(Actor) ->
  actor_rpc(Actor, stop).

actor_rpc({_Id, Pid}, Command) ->
%%   io:format("Sending: ~140p to: ~s\n", [Command, Id]),
  Ref = make_ref(),
  Pid ! {cmd, self(), Ref, Command},
  receive
    {Ref, Response} -> {ok, Response}
  after 500 ->
          {error, {Command, timeout}}
  end.

actor_init(State) ->
  actor_loop(State#{actors => [], signatures => []}).

actor_loop(State) ->
  receive
    {cmd, Pid, Ref, stop} ->
      ?LOG(State, "stopping - knows:\n    ~140p\n", [[Id || {Id, _} <- maps:get(actors, State)]]),
      Pid ! {Ref, stopped};
    {cmd, Pid, Ref, Command} ->
      ?LOG(State, "got command ~140p\n", [Command]),
      {Response, NewState} = handle_command(State, Command),
      Pid ! {Ref, Response},
      actor_loop(NewState);
    {msg, Id, Command} ->
      ?LOG(State, "got message from ~8s :\n    ~140p\n", [Id, Command]),
      NewState = handle_message(State, Id, Command),
      actor_loop(NewState);
    _Other ->
      ?LOG(State, "Unexpected: ~p\n", [_Other]),
      actor_loop(State)
  end.

handle_command(State, {new_user, NewUser}) ->
  {ok, handle_new_actor(State, undefined, NewUser)};
handle_command(State, {new_sig, Id, Message}) ->
  {ok, handle_new_sig(State, undefined, Id, Message)};
handle_command(State = #{signatures := Sigs}, {get_sig, Id}) ->
  case lists:keyfind(Id, 1, Sigs) of
    false ->
      {not_found, State};
    {_, #{sig := Signature}} ->
      {{ok, Signature}, State#{signatures := lists:keydelete(Id, 1, Sigs)}};
    {_, _} ->
      {not_ready, State}
  end.

handle_message(State, FromId, {new_user, {FromId, _}} = Message) ->
  handle_message_(State, FromId, Message);
handle_message(State = #{actors := Actors}, FromId, Message) ->
  case lists:keymember(FromId, 1, Actors) of
    true ->
      handle_message_(State, FromId, Message);
    false ->
      ?LOG(State, "message from unknown actor ~140p!??\n", [FromId]),
      State
  end.

handle_message_(State, FromId, {new_user, {Id, Pid}}) ->
  handle_new_actor(State, FromId, {Id, Pid});
handle_message_(State, FromId, {new_sig, SigId, Message}) ->
  handle_new_sig(State, FromId, SigId, Message);
handle_message_(State, _FromId, {sig_part1, SigId, PartSig}) ->
  handle_sig_part1(State, SigId, PartSig);
handle_message_(State, _FromId, {sig_part2, SigId, PartSig}) ->
  handle_sig_part2(State, SigId, PartSig).


handle_new_actor(State = #{id := MyId, actors := Actors}, FromId, {NewId, Pid}) ->
  case lists:keymember(NewId, 1, Actors) of
    true ->
      ?LOG(State, "Id ~s already in list of actors\n", [NewId]),
      State;
    false ->
      ?LOG(State, "Add ~s to list of actors and notifying others\n", [NewId]),
      [ Pid ! {msg, MyId, {new_user, {MyId, self()}}} || FromId /= NewId ],
      [ P ! {msg, MyId, {new_user, {NewId, Pid}}} || {I, #{pid := P}} <- Actors, I /= FromId ],
      NewActors =  [{NewId, #{pid => Pid}} | Actors],
      State#{actors := NewActors}
  end.

handle_new_sig(State = #{id := MyId, signatures := Sigs, actors := Actors, secret := SK},
               FromId, SigId, Message) ->
  case lists:keymember(SigId, 1, Sigs) of
    true ->
      ?LOG(State, "Signature ~p already in state\n", [SigId]),
      State;
    false ->
      ?LOG(State, "Adding new signature ~p to state\n", [SigId]),

      %% Send the new sig only if we was the first recipient
      [ P ! {msg, MyId, {new_sig, SigId, Message}} || {_I, #{pid := P}} <- Actors, FromId == undefined],

      SigState0 = sign_setup(#{secret => SK}, actor_ids(State)),
      {PartSig, SigState} = sign_step1(SigState0, Message),
      %% Send my partial signature to everyone
      [ P ! {msg, MyId, {sig_part1, SigId, PartSig}} || {_I, #{pid := P}} <- Actors ],

      State#{signatures := [{SigId, SigState} | Sigs]}
  end.

handle_sig_part1(State = #{id := MyId, signatures := Sigs, actors := Actors}, SigId, PartSig) ->
  case lists:keyfind(SigId, 1, Sigs) of
    false ->
      ?LOG(State, "Signature part 1 for unknown signature: ~p\n", [SigId]),
      State;
    {_, SigState} ->
      %% TODO: verify we are in step1?
      case sign_add_rg(SigState, PartSig) of
        {incomplete, NewSigState} ->
          State#{signatures := lists:keyreplace(SigId, 1, Sigs, {SigId, NewSigState})};
        {all, NewSigState} ->
          {PartSig2, NewSigState1} = sign_step2(NewSigState),
          [ P ! {msg, MyId, {sig_part2, SigId, PartSig2}} || {_I, #{pid := P}} <- Actors ],
          State#{signatures := lists:keyreplace(SigId, 1, Sigs, {SigId, NewSigState1})}
      end
  end.

handle_sig_part2(State = #{signatures := Sigs}, SigId, PartSig) ->
  case lists:keyfind(SigId, 1, Sigs) of
    false ->
      ?LOG(State, "Signature part 2 for unknown signature: ~p\n", [SigId]),
      State;
    {_, SigState} ->
      %% TODO: verify we are in step2?
      case sign_add_s(SigState, PartSig) of
        {incomplete, NewSigState} ->
          State#{signatures := lists:keyreplace(SigId, 1, Sigs, {SigId, NewSigState})};
        {all, NewSigState} ->
          {Signature, NewSigState1} = sign_finish(NewSigState),
          ?LOG(State, "computed signature ~p: ~140p\n", [SigId, Signature]),
          State#{signatures := lists:keyreplace(SigId, 1, Sigs, {SigId, NewSigState1})}
      end
  end.

actor_ids(#{actors := Actors}) ->
  [ Id || {Id, _} <- Actors ].

log(#{id := MyId}, Fmt, Args) ->
  io:format(io_lib:format("~8s : ", [MyId]) ++ Fmt, Args).



%%%
%%% Testing stuff
%%%

bar() ->
  SK = compute_nonce(),

  #{public := PK} = enacl:sign_seed_keypair(SK),
  <<Seed:32/bytes, _/binary>> = crypto:hash(sha512, SK),

  io:format("PK: ~140p\n", [PK]),
  io:format("SK: ~140p\n", [SK]),
  io:format("Sd: ~140p\n", [Seed]),
  io:format("X:  ~140p\n", [enacl:crypto_ed25519_scalarmult_base(Seed)]),

  ok.

neg_pt(<<B0_30:31/bytes, B31>>) -> <<B0_30/bytes, (B31 bor 16#80):8>>.
pos_pt(<<B0_30:31/bytes, B31>>) -> <<B0_30/bytes, (B31 band 16#7f):8>>.

is_pos_y(<<_:31/bytes, B31>>) ->
  (B31 band 16#80) == 0.

musig_keypair() ->
  SK = crypto:strong_rand_bytes(32),
  #{public := PK} = enacl:sign_seed_keypair(SK),
  case is_pos_y(PK) of
    true  ->
      <<Seed0:32/bytes, _/binary>> = crypto:hash(sha512, SK),
      #{public => PK, secret => SK, seed => clamp(Seed0)};
    false ->
      musig_keypair()
  end.

baz() ->
  Msg = crypto:strong_rand_bytes(32),

  #{public := PK_A, secret := Priv_A, seed := SK_A} = musig_keypair(),
  #{public := PK_B, secret := Priv_B, seed := SK_B} = musig_keypair(),

  [Xas, Yas] = [compute_nonce(), compute_nonce()],
  [Xbs, Ybs] = [compute_nonce(), compute_nonce()],
  [Xa, Ya] = [compute_nonce_pt(Xas), compute_nonce_pt(Yas)],
  [Xb, Yb] = [compute_nonce_pt(Xbs), compute_nonce_pt(Ybs)],
  X = pt_add(Xa, Xb),
  Y = pt_add(Ya, Yb),
  Ea = hash_to_scalar(<<PK_A/binary, PK_B/binary, PK_A/binary>>),
  Eb = hash_to_scalar(<<PK_A/binary, PK_B/binary, PK_B/binary>>),

  APK = sum_pts([sc_mul(Ea, PK_A), sc_mul(Eb, PK_B)]),
%%   #{p := APK, flipped := FlipAPK} = mk_pos_pt(APK0),
  E1 = hash_to_scalar(<<APK/binary, X/binary, Y/binary, Msg/binary>>),

  AN = pt_add(X, sc_mul(E1, Y)),
%%   #{p := AN, flipped := FlipAN} = mk_pos_pt(AN0), %% Dubious?

  Ch = hash_to_scalar(<<AN/binary, APK/binary, Msg/binary>>),

  Na = add_sc(Xas, mul_sc(E1, negate_if_flipped(not is_pos_y(Y), Yas))),
  Nb = add_sc(Xbs, mul_sc(E1, negate_if_flipped(not is_pos_y(Y), Ybs))),

  Sa = add_sc(mul_sc(Ch, Ea, negate_if_flipped(not is_pos_y(APK), SK_A)), Na),
  Sb = add_sc(mul_sc(Ch, Eb, negate_if_flipped(not is_pos_y(APK), SK_B)), Nb),
  S = add_sc(Sa, Sb),

  RHS = pt_add(sc_mul(Ch, APK), AN),
  LHS = sc_mul(S),
  io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [LHS, RHS]),

  ok.

baz3() ->
  Msg = crypto:strong_rand_bytes(32),

%%   #{public := PK_A, secret := Priv_A, seed := SK_A} = musig_keypair(),
  #{public := PK_B, secret := Priv_B, seed := SK_B} = musig_keypair(),
  #{public := PK_C, secret := Priv_C, seed := SK_C} = musig_keypair(),

  Priv_A = crypto:strong_rand_bytes(32),
  #{public := PK_A0} = enacl:sign_seed_keypair(Priv_A),
  <<SK_A0:32/bytes, _/binary>> = crypto:hash(sha512, Priv_A),
  SK_A1 = clamp(SK_A0),
  {PK_A, SK_A} = case is_pos_y(PK_A0) of
                   true -> {PK_A0, SK_A1};
                   false -> {pos_pt(PK_A0), neg_sc(SK_A1)} end,


  [Xas, Yas] = [compute_nonce(), compute_nonce()],
  [Xbs, Ybs] = [compute_nonce(), compute_nonce()],
  [Xcs, Ycs] = [compute_nonce(), compute_nonce()],
  [Xa, Ya] = [compute_nonce_pt(Xas), compute_nonce_pt(Yas)],
  [Xb, Yb] = [compute_nonce_pt(Xbs), compute_nonce_pt(Ybs)],
  [Xc, Yc] = [compute_nonce_pt(Xcs), compute_nonce_pt(Ycs)],
  X = sum_pts([Xa, Xb, Xc]),
  Y = sum_pts([Ya, Yb, Yc]),
  Ea = hash_to_scalar(<<PK_A/binary, PK_B/binary, PK_C/binary, PK_A/binary>>),
  Eb = hash_to_scalar(<<PK_A/binary, PK_B/binary, PK_C/binary, PK_B/binary>>),
  Ec = hash_to_scalar(<<PK_A/binary, PK_B/binary, PK_C/binary, PK_C/binary>>),

  APK0 = sum_pts([sc_mul(Ea, PK_A), sc_mul(Eb, PK_B), sc_mul(Ec, PK_C)]),
  #{p := APK, flipped := FlipAPK} = mk_pos_pt(APK0),
  E1 = hash_to_scalar(<<APK/binary, X/binary, Y/binary, Msg/binary>>),

  AN = pt_add(X, sc_mul(E1, Y)),
%%   AN0 = pt_add(X, sc_mul(E1, Y)),
%%   #{p := AN, flipped := FlipAN} = mk_pos_pt(AN0), %% Dubious?

  Ch = hash_to_scalar(<<AN/binary, APK/binary, Msg/binary>>),

%%   Na = negate_if_flipped(not is_pos_y(AN0), add_sc(Xas, mul_sc(E1, Yas))),
%%   Nb = negate_if_flipped(not is_pos_y(AN0), add_sc(Xbs, mul_sc(E1, Ybs))),
%%   Nc = negate_if_flipped(not is_pos_y(AN0), add_sc(Xcs, mul_sc(E1, Ycs))),
  Na = add_sc(Xas, mul_sc(E1, negate_if_flipped(not is_pos_y(Y), Yas))),
  Nb = add_sc(Xbs, mul_sc(E1, negate_if_flipped(not is_pos_y(Y), Ybs))),
  Nc = add_sc(Xcs, mul_sc(E1, negate_if_flipped(not is_pos_y(Y), Ycs))),

  Sa = add_sc(mul_sc(Ch, Ea, negate_if_flipped(not is_pos_y(APK0), SK_A)), Na),
  Sb = add_sc(mul_sc(Ch, Eb, negate_if_flipped(not is_pos_y(APK0), SK_B)), Nb),
  Sc = add_sc(mul_sc(Ch, Ec, negate_if_flipped(not is_pos_y(APK0), SK_C)), Nc),
  S = add_sc(Sa, Sb, Sc),

  RHS = pt_add(sc_mul(Ch, APK), AN),
  LHS = sc_mul(S),
  io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [LHS, RHS]),

  Sig = <<AN/binary, S/binary>>,

  V = enacl:sign_verify_detached(Sig, Msg, APK),

  io:format("Stat: ~p\n", [[is_pos_y(PK_A), is_pos_y(X), is_pos_y(Y), is_pos_y(APK0), is_pos_y(AN)]]),

  io:format("Ver: ~p\n", [V]),
  {V, {is_pos_y(X), is_pos_y(Y), is_pos_y(APK0), is_pos_y(AN)}}.

test(N) ->
  test(#{}, 0, N).

test(Stat, N, N) -> Stat;
test(Stat, N, M) ->
  {true, R} = baz3(),
  test(Stat#{R => maps:get(R, Stat, 0) + 1}, N + 1, M).

foo() ->
  %% MuSig2 with 2 participants...
  #{public := PK_A, secret := Priv_A, seed := SK_A} = musig_keypair(),
  #{public := PK_B, secret := Priv_B, seed := SK_B} = musig_keypair(),

%%   SK_A = crypto:strong_rand_bytes(32),
%%   SK_B = crypto:strong_rand_bytes(32),
%%   #{public := PK_A} = enacl:sign_seed_keypair(SK_A),
%%   #{public := PK_B} = enacl:sign_seed_keypair(SK_B),

%%   SK_A = compute_nonce(),
%%   SK_B = compute_nonce(),
%%   PK_A = enacl:crypto_ed25519_scalarmult_base_noclamp(SK_A),
%%   PK_B = enacl:crypto_ed25519_scalarmult_base_noclamp(SK_B),
  PK_A = enacl:crypto_ed25519_scalarmult_base(SK_A),

  Nonces_A = [compute_nonce(), compute_nonce()],
  NoncePts_A = [ compute_nonce_pt(P) || P <- Nonces_A ],

  Nonces_B = [compute_nonce(), compute_nonce()],
  NoncePts_B = [ compute_nonce_pt(P) || P <- Nonces_B ],

  %% A sends PK_A + NoncePts_A to B and vice versa.
  %% NOTE: everyone needs to know the order...
  Msg = crypto:strong_rand_bytes(32),

%%   {AggrPK, [Exp_A, Exp_B]} = aggregate_pks([PK_A, PK_B]),

%%   AggrNoncePts = aggregate_nonces([NoncePts_A, NoncePts_B]),
%%   {AggrAnn, [Exp0, Exp1]} = aggregate_announcement(AggrPK, AggrNoncePts, Msg),

  {AggrPK, AggrAnn, PartSig_A} = compute_partial(Msg, 1, SK_A, Nonces_A, [PK_A, PK_B], [NoncePts_A, NoncePts_B]),
  {AggrPK, AggrAnn, PartSig_B} = compute_partial(Msg, 2, SK_B, Nonces_B, [PK_A, PK_B], [NoncePts_A, NoncePts_B]),

  io:format("AggrPK:   ~140p\nAggrAnn:  ~140p\nPartsigA: ~140p\nPartsigB: ~140p\n", [AggrPK, AggrAnn, PartSig_A, PartSig_B]),

  Sig = <<_:32/bytes, AggrSig:32/bytes>> = prepare_signature([PartSig_A, PartSig_B], AggrAnn),

  [Xas, Yas] = Nonces_A,
  [Xbs, Ybs] = Nonces_B,
  [Xa, Ya] = NoncePts_A,
  [Xb, Yb] = NoncePts_B,
  X = pt_add(Xa, Xb),
  Y = pt_add(Ya, Yb),
  Ch = hash_to_scalar(<<AggrAnn/binary, AggrPK/binary, Msg/binary>>),
  Ea = hash_to_scalar(<<PK_A/binary, PK_B/binary, PK_A/binary>>),
  Eb = hash_to_scalar(<<PK_A/binary, PK_B/binary, PK_B/binary>>),
  K = pt_add(sc_mul(Ea, PK_A), sc_mul(Eb, PK_B)),
  Kp = pos_pt(K),
  E1 = hash_to_scalar(<<K/binary, X/binary, Y/binary, Msg/binary>>),
  E1p = hash_to_scalar(<<Kp/binary, X/binary, Y/binary, Msg/binary>>),
  L = pt_add(X, sc_mul(E1, Y)),
  Lp = pt_add(X, sc_mul(E1p, Y)),
  Lpp = pos_pt(Lp),

  io:format("K1:  ~140p\n", [K]),
  io:format("K1p: ~140p\n", [Kp]),
  io:format("K2:  ~140p\n", [AggrPK]),

  io:format("L1:   ~140p\n", [L]),
  io:format("L1p:  ~140p\n", [Lp]),
  io:format("L1pp: ~140p\n", [Lpp]),
  io:format("L2:   ~140p\n", [AggrAnn]),

  Na = add_sc(Xas, mul_sc(E1, Yas)),
  Nap = add_sc(Xas, mul_sc(E1p, Yas)),
%%   io:format("Ch:  ~140p\n", [Ch]),
%%   io:format("Ea:  ~140p\n", [Ea]),
%%   io:format("SK:  ~140p\n", [SK_A]),
%%   io:format("Na:  ~140p\n", [Na]),
%%   io:format("Na:  ~140p\n", [Nap]),
%%   io:format("SK': ~140p\n", [neg_sc(SK_A)]),
%%   io:format("Na': ~140p\n", [neg_sc(Na)]),
%%   io:format("Na': ~140p\n", [neg_sc(Nap)]),

  Sa = add_sc(mul_sc(Ch, Ea, SK_A), Na),
  Sa1 = add_sc(mul_sc(Ch, Ea, neg_sc(SK_A)), Na),
  Sa2 = add_sc(mul_sc(Ch, Ea, SK_A), neg_sc(Na)),
  Sa3 = add_sc(mul_sc(Ch, Ea, neg_sc(SK_A)), neg_sc(Na)),
  Sap = add_sc(mul_sc(Ch, Ea, SK_A), Nap),
  Sap1 = add_sc(mul_sc(Ch, Ea, neg_sc(SK_A)), Nap),
  Sap2 = add_sc(mul_sc(Ch, Ea, SK_A), neg_sc(Nap)),
  Sap3 = add_sc(mul_sc(Ch, Ea, neg_sc(SK_A)), neg_sc(Nap)),
%%   io:format("Sa:  ~140p\n", [Sa]),
%%   io:format("Sa1: ~140p\n", [Sa1]),
%%   io:format("Sa2: ~140p\n", [Sa2]),
%%   io:format("Sa3: ~140p\n", [Sa3]),
%%   io:format("Sap:  ~140p\n", [Sa]),
%%   io:format("Sap1: ~140p\n", [Sa1]),
%%   io:format("Sap2: ~140p\n", [Sa2]),
%%   io:format("Sap3: ~140p\n", [Sa3]),

  Nb = add_sc(Xbs, mul_sc(E1, Ybs)),
  Nbp = add_sc(Xbs, mul_sc(E1p, Ybs)),
  Sb = add_sc(mul_sc(Ch, Eb, SK_B), Nb),
  Sb1 = add_sc(mul_sc(Ch, Eb, neg_sc(SK_B)), Nb),
  Sb2 = add_sc(mul_sc(Ch, Eb, SK_B), neg_sc(Nb)),
  Sb3 = add_sc(mul_sc(Ch, Eb, neg_sc(SK_B)), neg_sc(Nb)),
  Sbp = add_sc(mul_sc(Ch, Eb, SK_B), Nbp),
  Sbp1 = add_sc(mul_sc(Ch, Eb, neg_sc(SK_B)), Nbp),
  Sbp2 = add_sc(mul_sc(Ch, Eb, SK_B), neg_sc(Nbp)),
  Sbp3 = add_sc(mul_sc(Ch, Eb, neg_sc(SK_B)), neg_sc(Nbp)),
  S = add_sc(Sa, Sb),
  S1 = add_sc(Sa1, Sb1),
  S2 = add_sc(Sa2, Sb2),
  S3 = add_sc(Sa3, Sb3),
  Sp = add_sc(Sap, Sbp),
  Sp1 = add_sc(Sap1, Sbp1),
  Sp2 = add_sc(Sap2, Sbp2),
  Sp3 = add_sc(Sap3, Sbp3),

  io:format("S1: ~140p\n", [S]),
  io:format("S1: ~140p\n", [S1]),
  io:format("S1: ~140p\n", [S2]),
  io:format("S1: ~140p\n", [S3]),
  io:format("S1: ~140p\n", [Sp]),
  io:format("S1: ~140p\n", [Sp1]),
  io:format("S1: ~140p\n", [Sp2]),
  io:format("S1: ~140p\n", [Sp3]),
  io:format("S2: ~140p\n", [AggrSig]),

  RHS0 = pt_add(sc_mul(Ch, K), L),
  RHS1 = pt_add(sc_mul(Ch, Kp), L),
  RHS2 = pt_add(sc_mul(Ch, K), Lp),
  RHS3 = pt_add(sc_mul(Ch, Kp), Lp),
  RHS4 = pt_add(sc_mul(Ch, K), Lpp),
  RHS5 = pt_add(sc_mul(Ch, Kp), Lpp),
  io:format("RHS0:  ~140p\n", [RHS0]),
  io:format("RHS1:  ~140p\n", [RHS1]),
  io:format("RHS2:  ~140p\n", [RHS2]),
  io:format("RHS3:  ~140p\n", [RHS3]),
  io:format("RHS4:  ~140p\n", [RHS4]),
  io:format("RHS5:  ~140p\n", [RHS5]),

  LHS0 = sc_mul(S),
  LHS1 = sc_mul(S1),
  LHS2 = sc_mul(S2),
  LHS3 = sc_mul(S3),
  LHS4 = sc_mul(Sp),
  LHS5 = sc_mul(Sp1),
  LHS6 = sc_mul(Sp2),
  LHS7 = sc_mul(Sp3),

  io:format("LHS0:  ~140p\n", [LHS0]),
  io:format("LHS1:  ~140p\n", [LHS1]),
  io:format("LHS2:  ~140p\n", [LHS2]),
  io:format("LHS3:  ~140p\n", [LHS3]),
  io:format("LHS4:  ~140p\n", [LHS4]),
  io:format("LHS5:  ~140p\n", [LHS5]),
  io:format("LHS6:  ~140p\n", [LHS6]),
  io:format("LHS7:  ~140p\n", [LHS7]),

  Sax = mul_sc(Ch, Ea, SK_A),
  Sbx = mul_sc(Ch, Eb, SK_B),
  Sx = add_sc(Sax, Sbx),
  Saxp = mul_sc(Ch, Ea, neg_sc(SK_A)),
  Sbxp = mul_sc(Ch, Eb, neg_sc(SK_B)),
  Sxp = add_sc(Saxp, Sbxp),

  RHS = sc_mul(Ch, K),
  LHS = sc_mul(Sx),
  RHSp = sc_mul(Ch, Kp),
  LHSp = sc_mul(Sxp),
  io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [LHS, RHS]),
  io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [LHSp, RHSp]),

  Say = case is_pos_y(Y) of
          true  -> add_sc(Xas, mul_sc(E1, Yas));
          false -> add_sc(Xas, mul_sc(E1, neg_sc(Yas))) end,
  Sby = case is_pos_y(Y) of
          true  -> add_sc(Xbs, mul_sc(E1, Ybs));
          false -> add_sc(Xbs, mul_sc(E1, neg_sc(Ybs))) end,
  Sy = add_sc(Say, Sby),

  Say1 = Na,
  Say2 = Nap,
  Say3 = neg_sc(Na),
  Say4 = neg_sc(Nap),
  Sby1 = Nb,
  Sby2 = Nbp,
  Sby3 = neg_sc(Nb),
  Sby4 = neg_sc(Nbp),
  Sy1 = add_sc(Say1, Sby1),
  Sy2 = add_sc(Say2, Sby2),
  Sy3 = add_sc(Say3, Sby3),
  Sy4 = add_sc(Say4, Sby4),

  io:format("RHS1:  ~140p\n", [L]),
  io:format("RHS2:  ~140p\n", [Lp]),
  io:format("RHS3:  ~140p\n", [Lpp]),

  io:format("LHS0:  ~140p\n", [sc_mul(Sy)]),
  io:format("LHS1:  ~140p\n", [sc_mul(Sy1)]),
  io:format("LHS2:  ~140p\n", [sc_mul(Sy2)]),
  io:format("LHS3:  ~140p\n", [sc_mul(Sy3)]),
  io:format("LHS4:  ~140p\n", [sc_mul(Sy4)]),

  io:format("X: ~p  Y: ~p\n", [is_pos_y(X), is_pos_y(Y)]),

  io:format("K part:\n"),
  case is_pos_y(K) of
    true  -> io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [LHS, RHS]);
    false -> io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [LHSp, RHSp])
  end,

  io:format("L part:\n"),
  io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [sc_mul(Sy), L]),

  S_ = case is_pos_y(K) of
         true  -> add_sc(Sax, Say, add_sc(Sbx, Sby));
         false -> add_sc(Saxp, Say, add_sc(Sbxp, Sby))
       end,

  io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [sc_mul(S_), pt_add(sc_mul(Ch, K), L)]),
  io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [sc_mul(S_), pt_add(sc_mul(Ch, Kp), L)]),


%%   LHS = enacl:crypto_ed25519_scalarmult_base_noclamp(AggrSig),
%%   RHS0 = enacl:crypto_ed25519_scalarmult_noclamp(Ch, AggrPK),
%%   RHS = enacl:crypto_ed25519_add(RHS0, AggrAnn),
%%   io:format("LHS:  ~140p\nRHS:  ~140p\n\n", [LHS, RHS]),

  true = enacl:sign_verify_detached(Sig, Msg, AggrPK),

  ok.

pt_add(A, B) -> enacl:crypto_ed25519_add(A, B).
sc_mul(S) -> enacl:crypto_ed25519_scalarmult_base_noclamp(S).
sc_mul(S, P) -> enacl:crypto_ed25519_scalarmult_noclamp(S, P).
mul_sc(A, B) -> enacl:crypto_ed25519_scalar_mul(A, B).
mul_sc(A, B, C) -> mul_sc(mul_sc(A, B), C).
add_sc(A, B) -> enacl:crypto_ed25519_scalar_add(A, B).
add_sc(A, B, C) -> add_sc(add_sc(A, B), C).
neg_sc(A) -> enacl:crypto_ed25519_scalar_negate(A).

mk_pos_pt(P) ->
  case is_pos_y(P) of
    true  -> #{p => P,         flipped => false};
    false -> #{p => pos_pt(P), flipped => true}
  end.

pt(#{p := P}) -> P;
pt(P)         -> P.

negate_if_flipped(true, N)  -> enacl:crypto_ed25519_scalar_negate(N);
negate_if_flipped(false, N) -> N.

compute_partial(Msg, Ix, SK0, Nonces, AllPKs, AllNoncePts) ->
  {#{p := AggrPK, flipped := AggrPKFlipped}, Exp} = aggregate_pks(Ix, AllPKs),
  io:format("AggrPK: ~p\n", [AggrPKFlipped]),
  SK = negate_if_flipped(AggrPKFlipped, SK0),

  AggrNoncePts = aggregate_nonces(AllNoncePts),

  {#{p := AggrAnn, flipped := AggrAnnFlipped}, [Exp1, Exp2]} =
    aggregate_announcement(AggrPK, AggrNoncePts, Msg),
  io:format("AggrAnn: ~p\n", [AggrAnnFlipped]),

  PrivNonce0 = add_sc(mul_sc(Exp1, lists:nth(1, Nonces)),
                      mul_sc(Exp2, lists:nth(2, Nonces))),

  PrivNonce = negate_if_flipped(AggrAnnFlipped, PrivNonce0),

  Challenge = hash_to_scalar(<<AggrAnn/binary, AggrPK/binary, Msg/binary>>),

  PartSig = add_sc(mul_sc(Challenge, Exp, SK), PrivNonce),
%%   io:format("Ch: ~140p\n", [Challenge]),
%%   io:format("Ex: ~140p\n", [Exp]),
%%   io:format("Sk: ~140p\n", [SK]),
%%   io:format("E1: ~140p\n", [Exp1]),
%%   io:format("E2: ~140p\n", [Exp2]),
%%   io:format("N1: ~140p\n", [lists:nth(1, Nonces)]),
%%   io:format("N2: ~140p\n", [lists:nth(2, Nonces)]),
%%   io:format("Nc: ~140p\n", [PrivNonce0]),
%%   io:format("Nc: ~140p\n", [PrivNonce]),
%%   io:format("PS: ~140p\n", [PartSig]),

%%   PartSig0 =
%%     enacl:crypto_ed25519_scalar_mul(Challenge,
%%       enacl:crypto_ed25519_scalar_mul(Exp, SK)),

%%   PartSig1 =
%%     enacl:crypto_ed25519_scalar_add(PartSig0,
%%       enacl:crypto_ed25519_scalar_mul(lists:nth(1, Nonces), Exp1)),

%%   PartSig2 =
%%     enacl:crypto_ed25519_scalar_add(PartSig1,
%%       enacl:crypto_ed25519_scalar_mul(lists:nth(2, Nonces), Exp2)),

  {AggrPK, AggrAnn, PartSig}.

prepare_signature(PartSigs, AggrAnn) ->
  AggrSig = sum_scalars(PartSigs),
  <<AggrAnn/binary, AggrSig/binary>>.

hash_to_scalar(Binary) ->
  enacl:crypto_ed25519_scalar_reduce(crypto:hash(sha512, Binary)).

compute_nonce() ->
  enacl:crypto_ed25519_scalar_reduce(crypto:strong_rand_bytes(64)).
%% compute_nonce() ->
%%   N = enacl:crypto_ed25519_scalar_reduce(crypto:strong_rand_bytes(64)),
%%   case is_pos_y(compute_nonce_pt(N)) of
%%     true  -> N;
%%     false -> enacl:crypto_ed25519_scalar_negate(N)
%%   end.

compute_nonce_pt(Nonce) ->
  enacl:crypto_ed25519_scalarmult_base_noclamp(Nonce).

compute_exponent(AllPKs, PK) ->
  hash_to_scalar(<<AllPKs/binary, PK/binary>>).

aggregate_pks(Ix, PKs) ->
  AllPKs = << PK || PK <- PKs >>,
  Exponents = [ compute_exponent(AllPKs, PK) || PK <- PKs ],

  Factors = [ enacl:crypto_ed25519_scalarmult_noclamp(Exp, PK)
              || {PK, Exp} <- lists:zip(PKs, Exponents) ],

  AggregatedPK = sum_pts(Factors),

  {mk_pos_pt(AggregatedPK), lists:nth(Ix, Exponents)}.

aggregate_nonces([[] | _]) -> [];
aggregate_nonces(NoncesLists) ->
  Ns = [ hd(L) || L <- NoncesLists ],
  Ts = [ tl(L) || L <- NoncesLists ],
  [sum_pts(Ns) | aggregate_nonces(Ts)].

aggregate_announcement(AggrPK, [Pt1, Pt2], Msg) ->
  Val = hash_to_scalar(<<AggrPK/binary, Pt1/binary, Pt2/binary, Msg/binary>>),

  E1 = <<1:8, 0:248>>,
  E2 = enacl:crypto_ed25519_scalar_mul(E1, Val),
  E2 = Val,

  AggrAnn = enacl:crypto_ed25519_add(Pt1, enacl:crypto_ed25519_scalarmult_noclamp(E2, Pt2)),
%%   io:format("Pt1: ~140p\nE2:  ~140p\nPt2: ~140p\n", [Pt1, E2, Pt2]),
  {mk_pos_pt(AggrAnn), [E1, E2]}.

sum_pts([P | Ps]) ->
   lists:foldl(fun(X, A) -> enacl:crypto_ed25519_add(X, A) end, P, Ps).

sum_scalars([P | Ps]) ->
   lists:foldl(fun(X, A) -> enacl:crypto_ed25519_scalar_add(X, A) end, P, Ps).


