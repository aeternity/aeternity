%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%  BLAKE2b implementation in Erlang - for details see: https://blake2.net
%%% @end
%%%=============================================================================

-module(aeu_blake2b).

-export([blake2b/3]).

-define(MAX_64BIT, 16#ffffffffffffffff).

-spec blake2b(Msg :: binary(), Key :: binary(), HashLen :: integer()) -> binary().
blake2b(Msg0, Key, HashLen) ->
    %% If message should be keyed, prepend message with padded key.
    Msg = <<(pad(128, Key))/binary, Msg0/binary>>,

    %% Set up the initial state
    Init  = (16#01010000 + (byte_size(Key) bsl 8) + HashLen),
    <<H0:64, H1_7/binary>> = blake_iv(),
    H = <<(H0 bxor Init):64, H1_7/binary>>,

    %% Perform the compression - message will be chopped into 128-byte chunks.
    State = blake2b_compress(H, Msg, 0),

    %% Just return the requested part of the hash
    binary_part(to_little_endian(State), {0, HashLen}).

blake2b_compress(H, <<Chunk:(128*8), Rest/binary>>, BCompr) when Rest /= <<>> ->
    H1 = blake2b_compress(H, <<Chunk:(128*8)>>, BCompr + 128, false),
    blake2b_compress(H1, Rest, BCompr + 128);
blake2b_compress(H, SmallChunk, BCompr) ->
    Size    = byte_size(SmallChunk),
    FillSize = (128 - Size) * 8,
    blake2b_compress(H, <<SmallChunk/binary, 0:FillSize>>, BCompr + Size, true).

blake2b_compress(H, Chunk0, BCompr, Last) ->
    Chunk = to_big_endian(Chunk0),
    <<V0_11:(12*64), V12:64, V13:64, V14:64, V15:64>> = <<H/binary, (blake_iv())/binary>>,
    V12_ = V12 bxor (BCompr band ?MAX_64BIT),
    V13_ = V13 bxor ((BCompr bsr 64) band ?MAX_64BIT),
    V14_ = case Last of
               false -> V14;
               true  -> V14 bxor ?MAX_64BIT
           end,
    V = <<V0_11:(12*64), V12_:64, V13_:64, V14_:64, V15:64>>,

    <<VLow:(8*64), VHigh:(8*64)>> =
        lists:foldl(fun(Round, Vx) -> blake2b_mix(Round, Chunk, Vx) end, V, lists:seq(0, 11)),

  <<HInt:(8*64)>> = H,
  <<((HInt bxor VLow) bxor VHigh):(8*64)>>.

blake2b_mix(Rnd, Chunk, V) ->
    <<V0:64, V1:64, V2:64, V3:64, V4:64, V5:64, V6:64, V7:64, V8:64,
      V9:64, V10:64, V11:64, V12:64, V13:64, V14:64, V15:64>> = V,
    <<M0:64, M1:64, M2:64, M3:64, M4:64, M5:64, M6:64, M7:64, M8:64,
      M9:64, M10:64, M11:64, M12:64, M13:64, M14:64, M15:64>> = Chunk,
    Ms = {M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15},
    M = fun(Ix) -> element(Ix+1, Ms) end,

    [S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15] = sigma(Rnd rem 10),

    {Vx0, Vx4, Vx8,  Vx12} = blake2b_mix(V0, V4, V8,  V12, M(S0), M(S1)),
    {Vx1, Vx5, Vx9,  Vx13} = blake2b_mix(V1, V5, V9,  V13, M(S2), M(S3)),
    {Vx2, Vx6, Vx10, Vx14} = blake2b_mix(V2, V6, V10, V14, M(S4), M(S5)),
    {Vx3, Vx7, Vx11, Vx15} = blake2b_mix(V3, V7, V11, V15, M(S6), M(S7)),

    {Vy0, Vy5, Vy10, Vy15} = blake2b_mix(Vx0, Vx5, Vx10, Vx15, M(S8),  M(S9)),
    {Vy1, Vy6, Vy11, Vy12} = blake2b_mix(Vx1, Vx6, Vx11, Vx12, M(S10), M(S11)),
    {Vy2, Vy7, Vy8,  Vy13} = blake2b_mix(Vx2, Vx7, Vx8,  Vx13, M(S12), M(S13)),
    {Vy3, Vy4, Vy9,  Vy14} = blake2b_mix(Vx3, Vx4, Vx9,  Vx14, M(S14), M(S15)),

    <<Vy0:64, Vy1:64, Vy2:64, Vy3:64, Vy4:64, Vy5:64, Vy6:64, Vy7:64, Vy8:64,
      Vy9:64, Vy10:64, Vy11:64, Vy12:64, Vy13:64, Vy14:64, Vy15:64>>.

blake2b_mix(Va, Vb, Vc, Vd, X, Y) ->
    Va1 = (Va + Vb + X) band ?MAX_64BIT,
    Vd1 = rotr64(32, Vd bxor Va1),

    Vc1 = (Vc + Vd1) band ?MAX_64BIT,
    Vb1 = rotr64(24, Vb bxor Vc1),

    Va2 = (Va1 + Vb1 + Y) band ?MAX_64BIT,
    Vd2 = rotr64(16, Va2 bxor Vd1),

    Vc2 = (Vc1 + Vd2) band ?MAX_64BIT,
    Vb2 = rotr64(63, Vb1 bxor Vc2),

    {Va2, Vb2, Vc2, Vd2}.

blake_iv() ->
    IV0 = 16#6A09E667F3BCC908,
    IV1 = 16#BB67AE8584CAA73B,
    IV2 = 16#3C6EF372FE94F82B,
    IV3 = 16#A54FF53A5F1D36F1,
    IV4 = 16#510E527FADE682D1,
    IV5 = 16#9B05688C2B3E6C1F,
    IV6 = 16#1F83D9ABFB41BD6B,
    IV7 = 16#5BE0CD19137E2179,
    <<IV0:64, IV1:64, IV2:64, IV3:64, IV4:64, IV5:64, IV6:64, IV7:64>>.

sigma(N) ->
    {_, Row} = lists:keyfind(N, 1, sigma()), Row.

sigma() ->
    [{0, [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15]},
     {1, [14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3]},
     {2, [11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4]},
     {3, [ 7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8]},
     {4, [ 9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13]},
     {5, [ 2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9]},
     {6, [12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11]},
     {7, [13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10]},
     {8, [ 6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5]},
     {9, [10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13,  0]}].

rotr64(N, I64) ->
    <<I64rot:64>> = rotr641(N, <<I64:64>>),
    I64rot.

rotr641(16, <<X:(64-16), Y:16>>) -> <<Y:16, X:(64-16)>>;
rotr641(24, <<X:(64-24), Y:24>>) -> <<Y:24, X:(64-24)>>;
rotr641(32, <<X:(64-32), Y:32>>) -> <<Y:32, X:(64-32)>>;
rotr641(63, <<X:(64-63), Y:63>>) -> <<Y:63, X:(64-63)>>.

pad(N, Bin) ->
    case (N - (byte_size(Bin) rem N)) rem N of
        0   -> Bin;
        Pad -> <<Bin/binary, 0:(Pad *8)>>
    end.

to_big_endian(Bin) -> to_big_endian(Bin, <<>>).
to_big_endian(<<>>, Acc) -> Acc;
to_big_endian(<<UInt64:1/little-unsigned-integer-unit:64, Rest/binary>>, Acc) ->
  to_big_endian(Rest, <<Acc/binary, UInt64:1/big-unsigned-integer-unit:64>>).

to_little_endian(Bin) -> to_little_endian(Bin, <<>>).
to_little_endian(<<>>, Acc) -> Acc;
to_little_endian(<<UInt64:1/big-unsigned-integer-unit:64, Rest/binary>>, Acc) ->
  to_little_endian(Rest, <<Acc/binary, UInt64:1/little-unsigned-integer-unit:64>>).

