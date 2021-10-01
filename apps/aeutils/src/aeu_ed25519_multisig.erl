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
