%%%-------------------------------------------------------------------
%%% @doc Wire-format helpers for the eth-compatible JSON-RPC layer.
%%%
%%% Every value that leaves this layer is `0x'-prefixed lower-case hex:
%%% integers as QUANTITY (minimal digits, `0x0' for zero), byte strings
%%% as DATA (fixed width, leading zeros preserved). Addresses, block
%%% hashes and tx hashes are all DATA -- an unmodified viem or ethers
%%% client parses no other form, so the AE-native `ak_...' / `ct_...' /
%%% `kh_...' / `th_...' encodings do not appear in any response.
%%%
%%% == Addresses are 20 bytes ==
%%%
%%% Every address-typed field -- `from', `to', `miner',
%%% `contractAddress', a log's `address' -- is exactly 20 bytes of
%%% lower-case `0x' hex, derived as the first 20 bytes of the 32-byte AE
%%% pubkey. `aerpc_addr_index' holds the reverse mapping so an inbound
%%% 20-byte address resolves back to its pubkey.
%%%
%%% An earlier revision emitted the full 32-byte pubkey on the theory
%%% that clients do not validate address width. Measured, they do:
%%% ethers v6 scored 4/12 against it and could not read a plain block,
%%% because its response formatter validates every address-typed field
%%% (`block.miner' alone sinks `getBlock') and its request path reads a
%%% non-20-byte string as an ENS name. viem's plain reads survived, but
%%% `parseAccount' rejected the width, so `call' and `estimateGas' -- the
%%% two methods this endpoint is default-off to protect -- were
%%% unreachable.
%%%
%%% Lower-case is load-bearing, not cosmetic: `getAddress/1' accepts an
%%% all-lower address and checksums it itself, but throws on mixed case
%%% that fails EIP-55. Never emitting mixed case is why no checksumming
%%% is implemented here.
%%%
%%% Hashes are unaffected and stay 32 bytes: block and transaction
%%% hashes are not address-typed and no client narrows them.
%%%
%%% Inputs stay permissive: `aerpc_account:decode_address/1' takes
%%% 20-byte, 32-byte, `ak_...' and `ct_...' alike, and
%%% `aerpc_block:decode_block_hash/1' takes `kh_...' alongside `0x'
%%% hex, so AE-native tooling is not locked out. Only the emitted side
%%% narrows.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_encoding).

-export([
          to_quantity/1
        , from_quantity/1
        , from_optional_quantity/2
        , to_hex_data/1
        , from_hex_data/1
        , format_account/1
        , format_contract/1
        , format_key_block_hash/1
        , format_micro_block_hash/1
        , format_tx_hash/1
        , zero_word/0
        ]).

-spec to_quantity(non_neg_integer()) -> binary().
to_quantity(0) ->
    <<"0x0">>;
to_quantity(N) when is_integer(N), N > 0 ->
    Hex = integer_to_binary(N, 16),
    Lower = string:lowercase(Hex),
    <<"0x", Lower/binary>>.

-spec from_quantity(binary()) -> non_neg_integer().
from_quantity(<<"0x", Hex/binary>>) when Hex =/= <<>> ->
    binary_to_integer(Hex, 16);
from_quantity(Bin) when is_binary(Bin), Bin =/= <<>> ->
    binary_to_integer(Bin, 16).

%% @doc Decode a hex `QUANTITY' that may be absent. `undefined' or an
%% empty binary yields `Default'; otherwise behaves like `from_quantity/1'.
%% Used for the optional `value', `gas', `nonce', ... fields on eth_call /
%% eth_estimateGas tx objects, where the caller may omit them.
-spec from_optional_quantity(binary() | undefined, non_neg_integer()) ->
    non_neg_integer().
from_optional_quantity(undefined, Default) -> Default;
from_optional_quantity(<<>>, Default)      -> Default;
from_optional_quantity(Bin, _Default) when is_binary(Bin) ->
    from_quantity(Bin).

%% @doc Encode an arbitrary byte sequence as `0x'-prefixed lower-case hex.
%% Distinct from `to_quantity/1', which strips leading zeros.
-spec to_hex_data(binary()) -> binary().
to_hex_data(Bin) when is_binary(Bin) ->
    Hex = binary:encode_hex(Bin),
    Lower = string:lowercase(Hex),
    <<"0x", Lower/binary>>.

%% @doc Decode `0x'-prefixed (or bare) hex into raw bytes. Inverse of
%% `to_hex_data/1'. Tolerates upper- or lower-case hex digits.
-spec from_hex_data(binary()) -> binary().
from_hex_data(<<"0x", Hex/binary>>) ->
    binary:decode_hex(Hex);
from_hex_data(Bin) when is_binary(Bin) ->
    binary:decode_hex(Bin).

%% @doc Emit an AE account pubkey as a 20-byte eth address. Neither
%% `aeapi:format_account_pubkey/1' (an `ak_...' string no eth client can
%% parse) nor the full pubkey (which ethers rejects outright).
-spec format_account(binary()) -> binary().
format_account(Pubkey) ->
    to_hex_data(aerpc_addr_index:to_addr20(Pubkey)).

%% @doc Contract pubkeys share the account key space and the same wire
%% form; kept as a separate function so callers still read as intended.
-spec format_contract(binary()) -> binary().
format_contract(Pubkey) ->
    to_hex_data(aerpc_addr_index:to_addr20(Pubkey)).

-spec format_key_block_hash(binary()) -> binary().
format_key_block_hash(Hash) ->
    to_hex_data(Hash).

-spec format_micro_block_hash(binary()) -> binary().
format_micro_block_hash(Hash) ->
    to_hex_data(Hash).

-spec format_tx_hash(binary()) -> binary().
format_tx_hash(Hash) ->
    to_hex_data(Hash).

%% @doc A 32-byte zero word. Used where the eth shape requires a hash
%% or word-sized field that AE has no counterpart for.
-spec zero_word() -> binary().
zero_word() ->
    <<"0x", (binary:copy(<<"0">>, 64))/binary>>.
