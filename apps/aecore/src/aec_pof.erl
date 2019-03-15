%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Proof of Fraud transaction
%%% @end
%%%=============================================================================

-module(aec_pof).

-include("blocks.hrl").

%% Behavior API
-export([deserialize/1,
         new/3,
         serialization_template/1,
         serialize/1,
         hash/1
        ]).

%% Getters
-export([header1/1,
         header2/1,
         pubkey/1
        ]).

%% Validators
-export([validate/1]).

-define(POF_VSN, 1).

-type pof() :: 'no_fraud' | map().
-type hash() :: <<>> | aec_hash:hash().
-export_type([pof/0,
              hash/0
             ]).

-spec new(aec_headers:header(), aec_headers:header(), miner_pubkey()) -> pof().
new(Header1, Header2, PubKey) ->
    #{header1 => Header1, header2 => Header2, pubkey => PubKey}.

-spec serialize(pof()) -> [binary()].
serialize(no_fraud) -> [];
serialize(#{header1  := Header1,
            header2  := Header2,
            pubkey   := PubKey
           }) ->
    SerializedHdr1 = aec_headers:serialize_to_binary(Header1),
    SerializedHdr2 = aec_headers:serialize_to_binary(Header2),
    Bin = aeser_chain_objects:serialize(
            pof,
            ?POF_VSN,
            serialization_template(?POF_VSN),
            [{header1, SerializedHdr1},
             {header2, SerializedHdr2},
             {pubkey, PubKey}
            ]),
    [Bin].

-spec deserialize([binary()]) -> pof().
deserialize([]) -> no_fraud;
deserialize([PoFBin]) when is_binary(PoFBin) ->
    [ {header1, SerializedHdr}
    , {header2, SerializedFraudHdr}
    , {pubkey, PubKey}
    ] = aeser_chain_objects:deserialize(
          pof,
          ?POF_VSN,
          serialization_template(?POF_VSN),
          PoFBin),
    #{header1 => aec_headers:deserialize_from_binary(SerializedHdr),
      header2 => aec_headers:deserialize_from_binary(SerializedFraudHdr),
      pubkey  => PubKey
     }.

-spec hash(pof()) -> hash().
hash(PoF) ->
    case serialize(PoF) of
        [] -> <<>>;
        [SerPof] -> aec_hash:hash(pof, SerPof)
    end.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec header1(map()) -> aec_headers:micro_header().
header1(#{header1 := Header}) ->
    Header.

-spec header2(map()) -> aec_headers:micro_header().
header2(#{header2 := Header}) ->
    Header.

-spec pubkey(map()) -> aec_keys:pubkey().
pubkey(#{pubkey := Pubkey}) ->
    Pubkey.

%%%===================================================================
%%% Validation
%%%===================================================================

validate(PoF) ->
    Checks =
        [fun() -> check_micro_siblings(PoF) end,
         fun() -> check_headers_are_different(PoF) end,
         fun() -> check_fraud_signatures(PoF) end],

    case aeu_validation:run(Checks) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

check_micro_siblings(#{header1 := Header1, header2 := Header2}) ->
    case (aec_headers:type(Header1) =:= micro)
        andalso (aec_headers:type(Header2) =:= micro)
        andalso (aec_headers:prev_hash(Header1)
                 =:= aec_headers:prev_hash(Header2)) of
        true -> ok;
        false -> {error, not_siblings}
    end.

check_headers_are_different(#{header1 := Header1, header2 := Header2}) ->
    case aec_headers:hash_header(Header1) =:= aec_headers:hash_header(Header2) of
        true  -> {error, same_header};
        false -> ok
    end.

check_fraud_signatures(#{header1 := FraudHeader1,
                         header2 := FraudHeader2,
                         pubkey  := Pubkey
                        }) ->
    Sig1 = aeu_sig:verify(FraudHeader1, Pubkey),
    Sig2 = aeu_sig:verify(FraudHeader2, Pubkey),
    case {Sig1, Sig2} of
        {ok, ok} -> ok;
        _ -> {error, fraud_header_dont_match_leader_key}
    end.

%%%===================================================================
%%% Internals
%%%===================================================================

serialization_template(?POF_VSN) ->
    [ {header1, binary}
    , {header2, binary}
    , {pubkey,  binary}
    ].
