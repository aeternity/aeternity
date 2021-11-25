%%% File        : aeu_poseidon.erl
%%% Author      : Hans Svensson
%%% Description :
%%% Created     : 18 Aug 2021 by Hans Svensson
-module(aeu_poseidon).

-compile([export_all, nowarn_export_all]).

%% Number of full rounds
-define(NROUNDSF, 8).

%% Script calc_round_numbers.py output [56, 56, 56, 56, 57, 57, 57]
%% Values rounded up to a multiplicative of t.
-define(NROUNDSP, [56, 57, 56, 60, 60, 63, 64, 63]).

-define(Q, 16#73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001).

zero() ->
  mkFr(0).

ark(State, C, It) ->
  ark(State, C, It, 0).

ark([], _, _, _) -> [];
ark([E | Es], C, It, I) ->
  [add(E, maps:get(It + I, C)) | ark(Es, C, It, I + 1)].

exp5(E) ->
  E2 = mul(E, E),
  mul(E, mul(E2, E2)).

sbox(NRndsF, NRndsP, State, I) when I < NRndsF div 2; I >= NRndsF div 2 + NRndsP ->
  [ exp5(E) || E <- State ];
sbox(_, _, [E | Es], _) ->
  [exp5(E) | Es].

mix(State, M) ->
  [ mix(I, State, M) || I <- lists:seq(0, length(State) - 1) ].

mix(I, State, M) ->
  mix(I, 0, State, M, zero()).

mix(_, _, [], _, Acc) -> Acc;
mix(I, J, [E | Es], M, Acc) ->
  Mul = mul(maps:get(J, maps:get(I, M)), E),
  mix(I, J + 1, Es, M, add(Acc, Mul)).


hash(Input) ->
  T = length(Input) + 1,
  hash(T, Input).

hash(T, Input) when T > 1, T =< length(?NROUNDSP) ->
  NRndsF = ?NROUNDSF,
  NRndsP = lists:nth(T - 1, ?NROUNDSP),
  State0 = [zero() | Input],

  C = const_to_map(aeu_poseidon_constants:params_c_BLS12_381(T)),
  M = const_to_map(aeu_poseidon_constants:params_m_BLS12_381(T)),

  Step = fun(I, S0) ->
           S1 = ark(S0, C, I * T),
           S2 = sbox(NRndsF, NRndsP, S1, I),
           S3 = mix(S2, M),
           S3
         end,

  State1 = lists:foldl(Step, State0, lists:seq(0, NRndsF + NRndsP - 1)),

  hd(State1).


%%% Constants
%% Round constants
%% generate_parameters_grain.sage 1 0 255 5 8 60 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

const_to_map(Cs) when is_list(Cs) ->
  maps:from_list([{X, const_to_map(C)} || {X, C} <- lists:zip(lists:seq(0, length(Cs) - 1), Cs)]);
const_to_map(C) when is_integer(C) ->
  mkFr(C).

%% Internal API

%% mkFr(I) -> emcl:mk_Fr(C).
mkFr(I) -> I.

%% add(A, B) -> emcl:bnFr_add(A, B).
add(A, B) -> (A + B) rem 16#73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001.

%% mul(A, B) -> emcl:bnFr_mul(A, B).
mul(A, B) -> (A * B) rem 16#73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001.

ark3({S0, S1, S2}, {C0, C1, C2}) ->
  {S0 + C0, S1 + C1, S2 + C2}.

e5(X) ->
   Y = (X * X) rem ?Q,
   (((X * Y) rem ?Q) * Y) rem ?Q.

sbox3(R, {S0, S1, S2}) when R < 4; R > 60 ->
  {e5(S0), e5(S1), e5(S2)};
sbox3(_, {S0, S1, S2}) ->
  {e5(S0), S1, S2}.

mix3({S0, S1, S2}) ->
  {(S0 * 16#3d955d6c02fe4d7cb500e12f2b55eff668a7b4386bd27413766713c93f2acfcd +
    S1 * 16#3798866f4e6058035dcf8addb2cf1771fac234bcc8fc05d6676e77e797f224bf +
    S2 * 16#2c51456a7bf2467eac813649f3f25ea896eac27c5da020dae54a6e640278fda2) rem ?Q,
   (S0 * 16#20088ca07bbcd7490a0218ebc0ecb31d0ea34840e2dc2d33a1a5adfecff83b43 +
    S1 * 16#1d04ba0915e7807c968ea4b1cb2d610c7f9a16b4033f02ebacbb948c86a988c3 +
    S2 * 16#5387ccd5729d7acbd09d96714d1d18bbd0eeaefb2ddee3d2ef573c9c7f953307) rem ?Q,
   (S0 * 16#1e208f585a72558534281562cad89659b428ec61433293a8d7f0f0e38a6726ac +
    S1 * 16#0455ebf862f0b60f69698e97d36e8aafd4d107cae2b61be1858b23a3363642e0 +
    S2 * 16#569e2c206119e89455852059f707370e2c1fc9721f6c50991cedbbf782daef54) rem ?Q}.

hash3(A, B) ->
  Cs = aeu_poseidon_constants:params_c_BLS12_381(3),
  CRs = lists:zip(lists:seq(0, 64), chunkify3(Cs)),

  Step = fun({R, C}, S) ->
             X1 = ark3(S, C),
             X2 = sbox3(R, X1),
             mix3(X2)
         end,

  {S, _, _} = lists:foldl(Step, {0, A, B}, CRs),
  S.

chunkify3([]) -> [];
chunkify3([A, B, C | Cs]) -> [{A, B, C} | chunkify3(Cs)].
