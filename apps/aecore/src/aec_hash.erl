-module(aec_hash).

-include("aec_hash.hrl").

-export([blake2b_256_hash/1,
         hash/2,
         sha256_hash/1
        ]).

-export_type([hashable/0, hash/0]).

-type hashable() :: binary().

%% Since Edoc is incapable of handling sensible ways to write the type,
%% and the type spec language is also crippled - lets use a magic number
-type hash() :: <<_:256>>. %% 256 = 32 * 8.

-type hash_type() :: pubkey | header | tx | signed_tx | pow | evm | aens.

-spec hash(hash_type(), hashable()) -> hash().
hash(evm, Bin) when is_binary(Bin) ->
    sha3_hash(Bin);
hash(_ObjType, Bin) when is_binary(Bin) ->
    blake2b_256_hash(Bin).

%%------------------------------------------------------------------------------
%% Calculate a 256 bit digest BLAKE2b hash value of a binary
%%------------------------------------------------------------------------------
-spec blake2b_256_hash(hashable()) -> hash().
blake2b_256_hash(Bin) ->
    {ok, Hash} = enacl:generichash(?HASH_BYTES, Bin),
    Hash.

%%------------------------------------------------------------------------------
%% Calculate the SHA256 hash value of a binary
%%------------------------------------------------------------------------------
-spec sha256_hash(hashable()) -> hash().
sha256_hash(Data) when is_binary(Data) ->
    <<_:?HASH_BYTES/unit:8>> = crypto:hash(sha256, Data).

%%------------------------------------------------------------------------------
%% Calculate the SHA256 hash value of a binary
%%------------------------------------------------------------------------------
-spec sha3_hash(hashable()) -> hash().
sha3_hash(Data) when is_binary(Data) ->
    sha3:hash(?HASH_BYTES*8, Data).

