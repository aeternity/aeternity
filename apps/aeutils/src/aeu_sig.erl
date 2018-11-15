%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%      verify microblock signature
%%% @end
%%% Created : 29. Aug 2018 00:00
%%%-------------------------------------------------------------------

-module(aeu_sig).

%% API
-export([verify/2]).

-spec verify(aec_headers:micro_header(), aec_keys:pubkey())
             -> 'ok'| {'error', signature_verification_failed}.
verify(Header, MinerPubKey) ->
    Signature = aec_headers:signature(Header),
    Bin = aec_headers:serialize_to_signature_binary(Header),
    case enacl:sign_verify_detached(Signature, Bin, MinerPubKey) of
        {ok, _}    -> ok;
        {error, _} -> {error, signature_verification_failed}
    end.
