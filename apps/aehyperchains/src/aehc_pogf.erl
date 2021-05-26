%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Proof of generational fraud
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_pogf).

-export([new/2
        , from_db/1
        , hash/1]).

-export([hc_header1/1
        , hc_header2/1
        , pubkey/1
        , validate/1]).

-export_type([pogf/0, maybe_pogf/0]).

-include_lib("aehyperchains/include/aehc_types.hrl").

-type key_header() :: aec_headers:key_header().

%% Two different keyheaders with the same prev_key pointer signed by the same leader
-record(hc_pogf, {
    hc_header1 :: key_header(),
    hc_header2 :: key_header()
}).
-type pogf() :: #hc_pogf{}.
-type maybe_pogf() :: pogf() | no_pogf.


%% API

-spec new(key_header(), key_header()) -> pogf().
new(Header1, Header2) ->
    #hc_pogf{hc_header1 = Header1, hc_header2 = Header2}.

-spec from_db(tuple()) -> pogf().
from_db(#hc_pogf{} = PoGF) -> PoGF.

-spec hash(maybe_pogf()) -> pogf_hash().
hash(no_pogf) -> <<0:?POGF_HASH_BYTES>>;
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

-spec validate(pogf()) -> ok | {error, term()}.
validate(no_pogf) -> ok;
validate(#hc_pogf{hc_header1 = Header1, hc_header2 = Header2}) ->
    Checks = [
        fun() -> check_headers_are_different(Header1, Header2) end
        , fun() -> check_key_siblings(Header1, Header2) end
        , fun() -> check_same_leader(Header1, Header2) end
        , fun() -> check_key_signatures(Header1, Header2) end
        , fun() -> check_correct_leader(Header1) end
    ],
    case aeu_validation:run(Checks) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec check_headers_are_different(header(), header()) -> ok | {error, same_header}.
check_headers_are_different(Header1, Header2) ->
    case aec_headers:hash_header(Header1) =:= aec_headers:hash_header(Header2) of
        true -> {error, same_header};
        false -> ok
    end.

-spec check_key_siblings(header(), header()) -> ok | {error, not_siblings}.
check_key_siblings(Header1, Header2) ->
    case (aec_headers:type(Header1) =:= key)
        andalso (aec_headers:type(Header2) =:= key)
        andalso (aec_headers:prev_key_hash(Header1)
            =:= aec_headers:prev_key_hash(Header2)) of
        true -> ok;
        false -> {error, not_siblings}
    end.

-spec check_same_leader(header(), header()) -> ok | {error, term()}.
check_same_leader(Header1, Header2) ->
    case aec_headers:miner(Header1) =:= aec_headers:miner(Header2) of
        true -> {error, same_header};
        false -> ok
    end.

-spec check_key_signatures(header(), header()) -> ok.
check_key_signatures(_Header1, _Header2) ->
    %% TODO: Check whether Headers were signed by the same leader
    ok.

-spec check_correct_leader(header()) -> ok.
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
