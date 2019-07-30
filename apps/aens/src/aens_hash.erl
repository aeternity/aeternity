%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System hashing functions
%%% @end
%%%=============================================================================

-module(aens_hash).

-include("aens.hrl").

%% API
-export([name_hash/1,
         commitment_hash/2,
         top_parent/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type name_hash() :: <<_:?NAME_HASH_BYTES>>.
-type subname_hash() :: <<_:?SUBNAME_HASH_BYTES>>.
-type commitment_hash() :: <<_:?COMMITMENT_HASH_BYTES>>.

-export_type([name_hash/0,
              subname_hash/0,
              commitment_hash/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec commitment_hash(binary(), integer()) -> commitment_hash().
commitment_hash(NameAscii, Salt) ->
    NameHash = name_hash(NameAscii),
    SaltBin = int_to_bin(Salt),
    hash(<<NameHash/binary, SaltBin/binary>>).

-spec name_hash(binary() | [AsciiPart :: binary()]) -> name_hash() | subname_hash().
name_hash(<<NameAscii/binary>>) ->
    name_hash(binary:split(NameAscii, <<".">>, [global, trim]));
name_hash([_, _] = NameParts) ->
    hash_labels(NameParts);
name_hash([_, _ | _] = SubnameParts) ->
    TopName = lists:nthtail(length(SubnameParts) - 2, SubnameParts),
    DomainHash = hash_labels(TopName),
    SubdomainHash = hash_labels(SubnameParts),
    <<DomainHash/binary, SubdomainHash/binary>>.

-spec top_parent(binary()) -> binary() | none.
top_parent(<<_:?NAME_HASH_BYTES/binary>>) ->
    none;
top_parent(<<Hash:?SUBNAME_HASH_BYTES/binary>>) ->
    <<TopParent:?NAME_HASH_BYTES/binary, _/binary>> = Hash,
    TopParent.

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
