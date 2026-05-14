%%%-------------------------------------------------------------------
%%% @doc Wire-format helpers for the AE JSON-RPC layer.
%%%
%%% v0: integer quantities are emitted as hex strings with the `0x'
%%% prefix (matches the JSON-RPC convention many clients expect).
%%% Address-like values are emitted via `aeapi:format_*' in their
%%% native AE form (`ak_...', `ct_...', `kh_...', etc.) -- no 20-byte
%%% remapping. The `to_quantity'/`from_quantity' pair is a single
%%% choke-point so the wire format can later become switchable.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_encoding).

-export([
          to_quantity/1
        , from_quantity/1
        , to_hex_data/1
        , from_hex_data/1
        , format_account/1
        , format_contract/1
        , format_key_block_hash/1
        , format_micro_block_hash/1
        , format_tx_hash/1
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

-spec format_account(binary()) -> binary().
format_account(Pubkey) ->
    aeapi:format_account_pubkey(Pubkey).

-spec format_contract(binary()) -> binary().
format_contract(Pubkey) ->
    aeapi:format_contract_pubkey(Pubkey).

-spec format_key_block_hash(binary()) -> binary().
format_key_block_hash(Hash) ->
    aeapi:format_key_block_hash(Hash).

-spec format_micro_block_hash(binary()) -> binary().
format_micro_block_hash(Hash) ->
    aeapi:format_micro_block_hash(Hash).

-spec format_tx_hash(binary()) -> binary().
format_tx_hash(Hash) ->
    aeapi:format_tx_hash(Hash).
