%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System hashing functions
%%%
%%%    We hash names in .test domain in a legacy format, whereas names
%%%    in the .chain domain are hashed differently.
%%%    If additional domains are added, we need to make their hash function
%%%    explicit in this code.
%%%
%%% @end
%%%=============================================================================

-module(aens_hash).

%% API
-export([name_hash/1,
         commitment_hash/2,
         to_auction_hash/1,
         from_auction_hash/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type name_hash() :: binary().
-type commitment_hash() :: binary().
-type auction_hash() :: binary().

-export_type([name_hash/0,
              commitment_hash/0,
              auction_hash/0]).

-define(NAME_HASH_BYTES, 32).
-define(COMMITMENT_HASH_BYTES, 32).
-define(AUCTION_HASH_BYTES, 33).


%%%===================================================================
%%% API
%%%===================================================================


-spec commitment_hash(binary(), integer()) -> commitment_hash().
commitment_hash(NameAscii, Salt) ->
    case aens_utils:name_domain(NameAscii) of
        {ok, Domain} ->
            case lists:member(Domain, aec_governance:non_test_registrars()) of
                true ->
                    assert_salt_positive(Salt),
                    SaltBin = int_to_bin(Salt),
                    hash(<<NameAscii/binary, SaltBin/binary>>);
                false ->
                    pre_lima_commitment_hash(NameAscii, Salt)
            end;
        _ ->
            %% This could be .test or any other wrong name backward compatible
            pre_lima_commitment_hash(NameAscii, Salt)
    end.

-spec name_hash(binary()) -> name_hash().
name_hash(NameAscii) ->
    case aens_utils:name_domain(NameAscii) of
        {ok, Domain} when Domain =:= <<"chain">> ->
            hash(NameAscii);
        _ ->
            %% This could be .test or any other wrong name backward compatible
            pre_lima_name_hash(NameAscii)
    end.

-spec to_auction_hash(name_hash()) -> auction_hash().
to_auction_hash(NameHash) ->
    <<NameHash/binary, 0:8>>.

-spec from_auction_hash(auction_hash()) -> name_hash().
from_auction_hash(AuctionHash) ->
    <<NameHash:?NAME_HASH_BYTES/unit:8, 0:8>> = AuctionHash,
    <<NameHash:?NAME_HASH_BYTES/unit:8>>.

-spec pre_lima_commitment_hash(binary(), integer()) -> commitment_hash().
pre_lima_commitment_hash(NameAscii, Salt) ->
    NameHash = pre_lima_name_hash(NameAscii),
    SaltBin = int_to_bin(Salt),
    hash(<<NameHash/binary, SaltBin/binary>>).

-spec pre_lima_name_hash(binary()) -> name_hash().
pre_lima_name_hash(NameAscii) ->
    Labels = binary:split(NameAscii, <<".">>, [global]),
    hash_labels(lists:reverse(Labels)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

hash_labels([]) ->
    empty_hash();
hash_labels([Label | Rest]) ->
    LabelHash = hash(Label),
    RestHash = hash_labels(Rest),
    hash(<<RestHash/binary, LabelHash/binary>>).

empty_hash() ->
    <<0:?NAME_HASH_BYTES/unit:8>>.

hash(Bin) ->
    aec_hash:hash(aens, Bin).

int_to_bin(Int) ->
    <<Int:?COMMITMENT_HASH_BYTES/integer-unit:8>>.

assert_salt_positive(Salt) when Salt > 0 -> ok;
assert_salt_positive(_) ->
    error(illegal_salt_value).
