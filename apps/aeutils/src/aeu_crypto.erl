%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Some crypto related helper functions.
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_crypto).

-export([ ecdsa_from_der_pk/1
        , ecdsa_from_der_sig/1
        , ecdsa_to_der_pk/1
        , ecdsa_to_der_sig/1
        , ecdsa_add_v/1

        , ecrecover/3
        , ecverify/3
        , ecverify/4
        ]).

%% Convert from OpenSSL/Erlang crypto formats...
ecdsa_from_der_pk(<<16#04, PK:64/binary>>) ->
    PK.

ecdsa_from_der_sig(<<16#30, _Len0:8, 16#02, Len1:8, Rest/binary>>) ->
    <<R:Len1/binary, 16#02, Len2:8, S/binary>> = Rest,
    <<(der_part_trunc(Len1, R)):32/binary, (der_part_trunc(Len2, S)):32/binary>>.

%% Convert to OpenSSL expected formats...
ecdsa_to_der_pk(Pubkey = <<_:64/binary>>) ->
    <<16#04, Pubkey:64/binary>>.

ecdsa_to_der_sig(<<R0:32/binary, S0:32/binary>>) ->
    {R1, S1} = {der_sig_part(R0), der_sig_part(S0)},
    {LR, LS} = {byte_size(R1), byte_size(S1)},
    <<16#30, (4 + LR + LS), 16#02, LR, R1/binary, 16#02, LS, S1/binary>>.

ecdsa_add_v(<<R:32/binary, S:32/binary>>) ->
    % TODO: calculate V
    <<27, R:32/binary, S:32/binary>>.

%% ECRECOVER
ecrecover(secp256k1, Hash, Sig) ->
    % ecrecover expects the signature to be 96 bytes long
    Input = <<Hash/binary, 0:(8*31), Sig/binary>>,
    case ecrecover:ecrecover(Input) of
        {ok, []} ->
            {ok, <<>>};
        {ok, Res} ->
            R = erlang:list_to_binary(Res),
            {ok, R};
        Err ->
            Err
    end.

%% ECVERIFY
ecverify(Msg, PK, Sig) -> ecverify(curve25519, Msg, PK, Sig).

ecverify(curve25519, Msg, PK, Sig) ->
    case enacl:sign_verify_detached(Sig, Msg, PK) of
        {ok, _}    -> true;
        {error, _} -> false
    end;
ecverify(secp256k1, Msg, PK0, Sig0) ->
    PK  = ecdsa_to_der_pk(PK0),
    Sig = ecdsa_to_der_sig(Sig0),
    %% Note that `sha256` is just there to indicate the length (32 bytes) of
    %% the digest, nothing is hashed with SHA256!
    crypto:verify(ecdsa, sha256, {digest, Msg}, Sig, [PK, secp256k1]).

%% Internal functions
der_part_trunc(33, <<0, Rest/binary>>)   -> Rest;
der_part_trunc(Len, Part) when Len =< 32 -> <<0:((32-Len)*8), Part/binary>>.

der_sig_part(P = <<1:1, _/bitstring>>) -> <<0:8, P/binary>>;
der_sig_part(<<0, Rest/binary>>)       -> der_sig_part(Rest);
der_sig_part(P)                        -> P.
