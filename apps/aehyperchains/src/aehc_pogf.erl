%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Proof of generational fraud
%%% @end
%%%-------------------------------------------------------------------

-module(aehc_pogf).

-export([ new/2
        , from_db/1
        , hash/1
        ]).

-export([ hc_header1/1
        , hc_header2/1
        , pubkey/1
        ]).

-export([validate/1]).

-include("../../aecore/include/blocks.hrl").
-include("aehc_utils.hrl").

%% Two different keyheaders with the same prev_key pointer signed by the same leader
-record(hc_pogf, {
        hc_header1 :: aec_headers:key_header(),
        hc_header2 :: aec_headers:key_header()
    }).

-type pogf() :: no_pogf | #hc_pogf{}.
-export_type([pogf/0]).

-spec new(aec_headers:key_header(), aec_headers:key_header()) -> pogf().
new(Header1, Header2) ->
    #hc_pogf{hc_header1 = Header1, hc_header2 = Header2}.

-spec from_db(tuple()) -> pogf().
from_db(#hc_pogf{} = PoGF) -> PoGF.

-spec hash(pogf()) -> pogf_hash().
hash(no_pogf) ->
    <<0:?POGF_HASH_BYTES>>;
hash(#hc_pogf{hc_header1 = Header1, hc_header2 = Header2}) ->
    {ok, H1} = aec_headers:hash_header(Header1),
    {ok, H2} = aec_headers:hash_header(Header2),
    aec_hash:hash(hc_pogf, <<H1/binary, H2/binary>>).

-spec hc_header1(pogf()) -> aec_headers:key_header().
hc_header1(#hc_pogf{hc_header1 = Header1}) -> Header1.

-spec hc_header2(pogf()) -> aec_headers:key_header().
hc_header2(#hc_pogf{hc_header2 = Header2}) -> Header2.

-spec pubkey(pogf()) -> miner_pubkey().
pubkey(#hc_pogf{hc_header1 = Header}) ->
    aec_headers:miner(Header).

%%%===================================================================
%%% Validation
%%%===================================================================

validate(no_pogf) ->
    ok;
validate(#hc_pogf{hc_header1 = Header1, hc_header2 = Header2}) ->
    Checks =
        [ fun() -> check_headers_are_different(Header1, Header2) end
        , fun() -> check_key_siblings(Header1, Header2) end
        , fun() -> check_same_leader(Header1, Header2) end
        , fun() -> check_key_signatures(Header1, Header2) end
        , fun() -> check_correct_leader(Header1) end
        ],

    case aeu_validation:run(Checks) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

check_headers_are_different(Header1, Header2) ->
    case aec_headers:hash_header(Header1) =:= aec_headers:hash_header(Header2) of
        true  -> {error, same_header};
        false -> ok
    end.

check_key_siblings(Header1, Header2) ->
    case (aec_headers:type(Header1) =:= key)
        andalso (aec_headers:type(Header2) =:= key)
        andalso (aec_headers:prev_key_hash(Header1)
                 =:= aec_headers:prev_key_hash(Header2)) of
        true -> ok;
        false -> {error, not_siblings}
    end.

check_same_leader(Header1, Header2) ->
    case aec_headers:miner(Header1) =:= aec_headers:miner(Header2) of
        true  -> {error, same_header};
        false -> ok
    end.

check_key_signatures(_Header1, _Header2) ->
    %% TODO: Check whether Headers were signed by the same leader
    ok.

check_correct_leader(Header) ->
    _PrevHCKeyHash = aec_headers:prev_key_hash(Header),
    %% TODO: abstract out this snippet and perform the election at PrevHCKeyHash
    %% ParentHash = aehc_headers_plugin:parent_hash(Header),
    %% Candidates = aehc_parent_db:candidates_in_election_cycle(ParentHash),
    %% ValidLeader = aehc_staking_contract:perform_election(PrevHCKeyHash, Candidates),
    %% case aec_headers:miner(Header) =:= ValidLeader of
    %%  true -> ok;
    %%  false -> {error, wrong_leader}
    %% end.
    ok.
