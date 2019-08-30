%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System hashing functions
%%%
%%%    We hash names in .test domain in a legacy format, whereas names
%%%    in the .aet domain are hashed differently.
%%%    If additional domains are added, we need to make their hash function
%%%    explicit in this code.
%%%
%%% @end
%%%=============================================================================

-module(aens_hash).

%% API
-export([name_hash/1,
         commitment_hash/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type name_hash() :: binary().
-type commitment_hash() :: binary().

-export_type([name_hash/0,
              commitment_hash/0]).

-define(NAME_HASH_BYTES, 32).
-define(COMMITMENT_HASH_BYTES, 32).

%%%===================================================================
%%% API
%%%===================================================================


-spec commitment_hash(binary(), integer()) -> commitment_hash().
commitment_hash(NameAscii, Salt) ->
    case aens_utils:name_domain(NameAscii) of
        {ok, Domain} when Domain =:= <<"aet">> ->
            assert_salt_positive(Salt),
            SaltBin = int_to_bin(Salt),
            hash(<<NameAscii/binary, SaltBin/binary>>);
        _ ->
            %% This could be .test or any other wrong name backward compatible
            pre_lima_commitment_hash(NameAscii, Salt)
    end.

-spec name_hash(binary()) -> name_hash().
name_hash(NameAscii) ->
    case aens_utils:name_domain(NameAscii) of
        {ok, Domain} when Domain =:= <<"aet">> ->
            hash(NameAscii);
        _ ->
            %% This could be .test or any other wrong name backward compatible
            pre_lima_name_hash(NameAscii)
    end.


-spec pre_lima_commitment_hash(binary(), integer()) -> commitment_hash().
pre_lima_commitment_hash(NameAscii, Salt) ->
    NameHash = name_hash(NameAscii),
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
