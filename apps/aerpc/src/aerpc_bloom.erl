%%%-------------------------------------------------------------------
%%% @doc 2048-bit logs-bloom helper for transaction receipts and blocks.
%%%
%%% Implements eth's standard log bloom (yellow paper, appendix B / J):
%%% for each log, keccak256-hash the emitter address and every topic;
%%% from each digest take three 11-bit slices (bytes [0..1], [2..3],
%%% [4..5], big-endian, mask to 2047) and set those bit positions in
%%% the 2048-bit bloom. Bit position N counts from the LSB of the
%%% conceptual big integer, so a buffer stored as 256 big-endian bytes
%%% has bit 0 in `byte[255]' bit 0 (LSB) and bit 2047 in `byte[0]'
%%% bit 7 (MSB).
%%%
%%% The hash family is the same one `ae_sha3' and `aec_hash:hash(evm, _)'
%%% use -- the aeternity-flavoured `sha3' dep configured for eth-style
%%% Keccak, not NIST SHA3-256. That is required for bloom-bit
%%% interoperability with eth indexers.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_bloom).

-export([empty/0, of_logs/1]).

-define(BLOOM_BYTES, 256).
-define(BLOOM_BITS,  2048).

-define(EMPTY_HEX,
        <<"0x", (binary:copy(<<"0">>, ?BLOOM_BYTES * 2))/binary>>).

-spec empty() -> binary().
empty() ->
    ?EMPTY_HEX.

%% @doc Build the bloom hex (`0x' + 512 hex chars) for a list of raw
%% log triples `{Address, Topics, Data}'. Data is not bloomed; only
%% the emitter address and each topic contribute bits.
%%
%% Empty list yields the empty bloom -- byte-identical to `empty/0',
%% which keeps cosmetic regressions out of receipts that never had
%% logs.
-spec of_logs([{binary(), [binary()], binary()}] | []) -> binary().
of_logs([]) ->
    ?EMPTY_HEX;
of_logs(Logs) when is_list(Logs) ->
    Bloom0 = <<0:(?BLOOM_BYTES * 8)>>,
    Bloom  = lists:foldl(fun add_log/2, Bloom0, Logs),
    aerpc_encoding:to_hex_data(Bloom).

%% ===================================================================
%% Internal
%% ===================================================================

add_log({Address, Topics, _Data}, Bloom0) when is_binary(Address) ->
    Bloom1 = add_item(Address, Bloom0),
    lists:foldl(fun add_item/2, Bloom1,
                [T || T <- Topics, is_binary(T)]).

add_item(Item, Bloom) ->
    Digest = sha3:hash(256, Item),
    set_three_bits(Digest, Bloom).

set_three_bits(<<B0:16/big-unsigned, B1:16/big-unsigned, B2:16/big-unsigned,
                 _Rest/binary>>, Bloom) ->
    Bits = [B0 band (?BLOOM_BITS - 1),
            B1 band (?BLOOM_BITS - 1),
            B2 band (?BLOOM_BITS - 1)],
    lists:foldl(fun set_bit/2, Bloom, Bits).

set_bit(N, Bloom) ->
    ByteIdx = ?BLOOM_BYTES - 1 - (N div 8),
    BitMask = 1 bsl (N rem 8),
    <<Pre:ByteIdx/binary, Byte:8/unsigned, Post/binary>> = Bloom,
    <<Pre/binary, (Byte bor BitMask):8/unsigned, Post/binary>>.
