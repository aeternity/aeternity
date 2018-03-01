%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   SipHash-2-4 without standard IV xor and specialized to precomputed key and 8 byte nonces
%%% @end
%%%=============================================================================

-module(aeu_siphash24).

-export([create_keys/1,
         hash/5]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(MAX64, 16#ffffffffffffffff).

-type hashable() :: integer().
-type siphash_key() :: integer().
-type sip_quadruple() :: {integer(), integer(), integer(), integer()}.%% in fact, uint64

-export_type([hashable/0,
              siphash_key/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec create_keys(binary()) ->
          {aeu_siphash24:siphash_key(),
           aeu_siphash24:siphash_key(),
           aeu_siphash24:siphash_key(),
           aeu_siphash24:siphash_key()}.
create_keys(Header) ->
    AuxHash = <<_:32/binary>> = aec_hash:blake2b_256_hash(Header),
    <<K0:64/little-unsigned,
      K1:64/little-unsigned,
      K2:64/little-unsigned,
      K3:64/little-unsigned>> = AuxHash,
    {K0, K1, K2, K3}.

-spec hash(siphash_key(), siphash_key(), siphash_key(), siphash_key(), hashable()) -> hashable().
hash(K0, K1, K2, K3, Nonce) ->
    V0 = K0,
    V1 = K1,
    V2 = K2,
    V3 = K3 bxor Nonce,
    {V01, V11, V21, V31} =
        sip_round(sip_round(sip_round(sip_round(sip_change(Nonce, sip_round(sip_round({V0, V1, V2, V3}))))))),
    ((V01 bxor V11) bxor (V21 bxor V31)) band 16#ffffffffffffffff.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% 1:
%% v0 += v1; v2 += v3; v1 = ROTL(v1,13); \
%% v3 = ROTL(v3,16);

%% 2:
%% v1 ^= v0; v3 ^= v2; \
%% v0 = ROTL(v0,32);

%% 3:
%% v2 += v1; v0 += v3; \
%% v1 = ROTL(v1,17);   v3 = ROTL(v3,21); \

%% 4:
%% v1 ^= v2; v3 ^= v0; v2 = ROTL(v2,32); \

-spec sip_round(sip_quadruple()) -> sip_quadruple().
sip_round({_V0, _V1, _V2, _V3} = Vs) ->
    sip_round4(sip_round3(sip_round2(sip_round1(Vs)))).

-spec sip_round1(sip_quadruple()) -> sip_quadruple().
sip_round1({V0, V1, V2, V3}) ->
    {(V0 + V1) band ?MAX64, rotl64(V1, 13), (V2 + V3) band ?MAX64, rotl64(V3, 16)}.

-spec sip_round2(sip_quadruple()) -> sip_quadruple().
sip_round2({V0, V1, V2, V3}) ->
    {rotl64(V0, 32), V1 bxor V0, V2, V3 bxor V2}.

-spec sip_round3(sip_quadruple()) -> sip_quadruple().
sip_round3({V0, V1, V2, V3}) ->
    {(V0 + V3) band ?MAX64, rotl64(V1, 17), (V2 + V1) band ?MAX64, rotl64(V3, 21)}.

-spec sip_round4(sip_quadruple()) -> sip_quadruple().
sip_round4({V0, V1, V2, V3}) ->
    {V0, V1 bxor V2, rotl64(V2, 32), V3 bxor V0}.

-spec sip_change(integer(), sip_quadruple()) -> sip_quadruple().
sip_change(Nonce, {V0, V1, V2, V3}) ->
    {V0 bxor Nonce, V1, V2 bxor 16#ff, V3}.

-spec rotl64(integer(), integer()) -> integer().
rotl64(X, B) ->
    ((X bsl B) bor (X bsr (64 - B))) band 16#ffffffffffffffff.

