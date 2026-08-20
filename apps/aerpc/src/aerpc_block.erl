%%%-------------------------------------------------------------------
%%% @doc Block / generation adapter for the AE JSON-RPC layer.
%%%
%%% Strategy G (see /tasks/eth-like-rpc-layer.md): an Ethereum "block"
%%% is mapped to an AE *generation* -- one key-block plus the
%%% micro-blocks beneath it. The `number' field is the key-block
%%% height; transactions are the flat-list of all micro-block txs.
%%%
%%% Hash and address-like fields are emitted in their AE-native form
%%% (`kh_...', `ak_...', etc.) per project policy: this layer does
%%% not map AE pubkeys onto 20-byte Eth addresses.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_block).

-export([
          by_hash/2
        , by_height/2
        , tx_count_by_hash/1
        , tx_count_by_height/1
        , resolve_tag/1
        , resolve_dry_run_top/1
        , resolve_id/1
        , decode_block_hash/1
        ]).

-define(EMPTY_LIST_KECCAK,
        <<"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347">>).
-define(EMPTY_TRIE_ROOT,
        <<"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421">>).
-define(ZERO_NONCE, <<"0x0000000000000000">>).

%% ===================================================================
%% Public API
%% ===================================================================

-spec by_hash(binary(), boolean()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_hash(HashIn, FullTxs) when is_binary(HashIn), is_boolean(FullTxs) ->
    case decode_block_hash(HashIn) of
        {ok, Hash} ->
            fetch_generation(Hash, FullTxs);
        {error, _, _} = Err ->
            Err
    end;
by_hash(_HashIn, _Full) ->
    {error, -32602, <<"Invalid params">>}.

-spec by_height(binary(), boolean()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_height(TagOrHex, FullTxs) when is_binary(TagOrHex), is_boolean(FullTxs) ->
    case resolve_tag(TagOrHex) of
        {ok, Height} ->
            case aec_chain:get_key_block_by_height(Height) of
                {ok, KeyBlock} ->
                    {ok, Hash} = aec_blocks:hash_internal_representation(KeyBlock),
                    fetch_generation(Hash, FullTxs);
                {error, _Reason} ->
                    {ok, null}
            end;
        {error, _, _} = Err ->
            Err
    end;
by_height(_TagOrHex, _Full) ->
    {error, -32602, <<"Invalid params">>}.

-spec tx_count_by_hash(binary()) ->
    {ok, binary()} | {error, integer(), binary()}.
tx_count_by_hash(HashIn) when is_binary(HashIn) ->
    case decode_block_hash(HashIn) of
        {ok, Hash} ->
            case aec_chain:get_generation_by_hash(Hash, forward) of
                {ok, #{micro_blocks := MBs}} ->
                    {ok, aerpc_encoding:to_quantity(count_txs(MBs))};
                error ->
                    {ok, <<"0x0">>}
            end;
        {error, _, _} = Err ->
            Err
    end;
tx_count_by_hash(_) ->
    {error, -32602, <<"Invalid params">>}.

-spec tx_count_by_height(binary()) ->
    {ok, binary()} | {error, integer(), binary()}.
tx_count_by_height(TagOrHex) when is_binary(TagOrHex) ->
    case resolve_tag(TagOrHex) of
        {ok, Height} ->
            case aec_chain:get_key_block_by_height(Height) of
                {ok, KeyBlock} ->
                    {ok, Hash} = aec_blocks:hash_internal_representation(KeyBlock),
                    tx_count_by_hash_bin(Hash);
                {error, _Reason} ->
                    {ok, <<"0x0">>}
            end;
        {error, _, _} = Err ->
            Err
    end;
tx_count_by_height(_) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Translate a block tag or hex height to a numeric height.
%% Tags map per the eth-rpc spec; AE has no formal finality so
%% `<<"safe">>' and `<<"finalized">>' currently follow `<<"latest">>'.
-spec resolve_tag(binary()) ->
    {ok, non_neg_integer()} | {error, integer(), binary()}.
resolve_tag(<<"latest">>)    -> top_height();
resolve_tag(<<"pending">>)   -> top_height();
resolve_tag(<<"safe">>)      -> top_height();
resolve_tag(<<"finalized">>) -> top_height();
resolve_tag(<<"earliest">>)  -> {ok, 0};
resolve_tag(<<"0x", _/binary>> = Hex) ->
    try {ok, aerpc_encoding:from_quantity(Hex)}
    catch _:_ -> {error, -32602, <<"Invalid params">>}
    end;
resolve_tag(_Other) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Resolve a block tag/hex to the shape `aec_dry_run:dry_run/4'
%% expects for its `Top' argument:
%%   * `top'         -- for `<<"latest">>' / `<<"pending">>' / etc.
%%   * `{height, N}' -- for explicit hex heights and `<<"earliest">>'.
%% Tags that resolve to the current top get the symbolic `top' rather
%% than a numeric height, because dry-run's `top' path uses the actual
%% top-of-chain microblock state (most-recent state, including pending
%% microblock txs), where `{height, N}' anchors right after the keyblock
%% of generation N.
-spec resolve_dry_run_top(binary()) ->
    {ok, top | {height, non_neg_integer()}} | {error, integer(), binary()}.
resolve_dry_run_top(<<"latest">>)    -> {ok, top};
resolve_dry_run_top(<<"pending">>)   -> {ok, top};
resolve_dry_run_top(<<"safe">>)      -> {ok, top};
resolve_dry_run_top(<<"finalized">>) -> {ok, top};
resolve_dry_run_top(<<"earliest">>)  -> {ok, {height, 0}};
resolve_dry_run_top(<<"0x", _/binary>> = Hex) ->
    try {ok, {height, aerpc_encoding:from_quantity(Hex)}}
    catch _:_ -> {error, -32602, <<"Invalid params">>}
    end;
resolve_dry_run_top(_Other) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Resolve a block identifier to a numeric height. Accepts either
%% the legacy binary form (`"latest"', `"0x123"', `kh_...', a 0x-32-byte
%% hex) or the EIP-1898 object form:
%%   * `#{<<"blockNumber">> => <<"0x...">>}'
%%   * `#{<<"blockHash">>   => <<"kh_..."|"0x..."(32-byte)>>}'
%%   * `#{<<"blockHash">>   => ..., <<"requireCanonical">> => true}'
%%
%% On the hash form: if `requireCanonical' is `true' and the hash is
%% off-canon, returns the AE-specific code `-39001'. If
%% `requireCanonical' is absent or `false', the height is returned as
%% recorded on the off-canon branch (caller may still read stale
%% state -- documented eth behaviour).
-spec resolve_id(binary() | map()) ->
    {ok, non_neg_integer()} | {error, integer(), binary()}.
resolve_id(Bin) when is_binary(Bin) ->
    case Bin of
        <<"kh_", _/binary>> -> height_of_hash(Bin, false);
        <<"0x", Hex/binary>> when byte_size(Hex) =:= 64 ->
            height_of_hash(Bin, false);
        _Other -> resolve_tag(Bin)
    end;
resolve_id(#{<<"blockNumber">> := Tag}) when is_binary(Tag) ->
    resolve_tag(Tag);
resolve_id(#{<<"blockHash">> := HashIn} = Obj) when is_binary(HashIn) ->
    Strict = maps:get(<<"requireCanonical">>, Obj, false),
    height_of_hash(HashIn, Strict);
resolve_id(_Other) ->
    {error, -32602, <<"Invalid block identifier">>}.

height_of_hash(HashIn, Strict) ->
    case decode_block_hash(HashIn) of
        {ok, Hash} -> check_height(Hash, Strict);
        {error, _, _} = Err -> Err
    end.

check_height(Hash, Strict) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} ->
            H = aec_headers:height(Header),
            case canonical_at_height(Hash, H) of
                true               -> {ok, H};
                false when Strict ->
                    {error, -39001,
                     <<"Block hash is not on the canonical chain">>};
                false              -> {ok, H}
            end;
        error ->
            {error, -32602, <<"Block hash not found">>}
    end.

canonical_at_height(Hash, Height) ->
    case aec_chain:get_key_block_by_height(Height) of
        {ok, KB} ->
            {ok, CanonicalHash} = aec_blocks:hash_internal_representation(KB),
            CanonicalHash =:= Hash;
        _Other -> false
    end.

%% @doc Decode a block hash from either canonical `kh_...' AE form or
%% `0x'-prefixed 32-byte hex. Returns the raw 32-byte binary.
-spec decode_block_hash(binary()) ->
    {ok, binary()} | {error, integer(), binary()}.
decode_block_hash(<<"kh_", _/binary>> = Encoded) ->
    case aeapi:decode_key_block_hash(Encoded) of
        {ok, Bin} -> {ok, Bin};
        _Error    -> {error, -32602, <<"Invalid params">>}
    end;
decode_block_hash(<<"0x", _/binary>> = Hex) ->
    try
        Bin = aerpc_encoding:from_hex_data(Hex),
        case byte_size(Bin) of
            32 -> {ok, Bin};
            _  -> {error, -32602, <<"Invalid params">>}
        end
    catch _:_ -> {error, -32602, <<"Invalid params">>}
    end;
decode_block_hash(_) ->
    {error, -32602, <<"Invalid params">>}.

%% ===================================================================
%% Internal
%% ===================================================================

top_height() ->
    case aec_chain:top_header() of
        undefined -> {error, -32603, <<"Chain not initialized">>};
        Header    -> {ok, aec_headers:height(Header)}
    end.

tx_count_by_hash_bin(Hash) ->
    case aec_chain:get_generation_by_hash(Hash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            {ok, aerpc_encoding:to_quantity(count_txs(MBs))};
        error ->
            {ok, <<"0x0">>}
    end.

count_txs(MBs) ->
    lists:sum([length(aec_blocks:txs(MB)) || MB <- MBs]).

fetch_generation(Hash, FullTxs) ->
    case aec_chain:get_generation_by_hash(Hash, forward) of
        {ok, #{key_block := KB, micro_blocks := MBs}} ->
            {ok, to_eth_shape(KB, MBs, Hash, FullTxs)};
        error ->
            {ok, null}
    end.

to_eth_shape(KeyBlock, MBs, Hash, FullTxs) ->
    Header = aec_blocks:to_key_header(KeyBlock),
    Height = aec_headers:height(Header),
    Beneficiary = aec_headers:beneficiary(Header),
    PrevKey = aec_headers:prev_key_hash(Header),
    StateRoot = aec_headers:root_hash(Header),
    TimestampMs = aec_headers:time_in_msecs(Header),
    Difficulty = aec_headers:difficulty(Header),
    Nonce = aec_headers:nonce(Header),
    GasUsed = sum_gas_used(MBs),
    Txs = transactions_field(MBs, Height, Hash, FullTxs),
    %% Pull the raw `{Address, Topics, Data}' triples once, both the
    %% bloom and the (future) receipts root derive from these.
    RawLogs    = aerpc_logs:raw_logs_for_block(Hash),
    Bloom      = aerpc_bloom:of_logs(RawLogs),
    Size       = block_serialized_size(KeyBlock, MBs),
    GasLimit   = aec_governance:block_gas_limit(),
    ReceiptsR  = receipts_root_marker(MBs),
    #{
        <<"number">>           => aerpc_encoding:to_quantity(Height),
        <<"hash">>             => aerpc_encoding:format_key_block_hash(Hash),
        <<"parentHash">>       => aerpc_encoding:format_key_block_hash(PrevKey),
        <<"nonce">>            => format_nonce(Nonce),
        <<"sha3Uncles">>       => ?EMPTY_LIST_KECCAK,
        <<"logsBloom">>        => Bloom,
        <<"transactionsRoot">> => aerpc_encoding:to_hex_data(StateRoot),
        <<"stateRoot">>        => aerpc_encoding:to_hex_data(StateRoot),
        <<"receiptsRoot">>     => ReceiptsR,
        <<"miner">>            => aerpc_encoding:format_account(Beneficiary),
        <<"difficulty">>       => aerpc_encoding:to_quantity(Difficulty),
        <<"totalDifficulty">>  => aerpc_encoding:to_quantity(Difficulty),
        <<"extraData">>        => <<"0x">>,
        <<"size">>             => aerpc_encoding:to_quantity(Size),
        <<"gasLimit">>         => aerpc_encoding:to_quantity(GasLimit),
        <<"gasUsed">>          => aerpc_encoding:to_quantity(GasUsed),
        <<"timestamp">>        => aerpc_encoding:to_quantity(TimestampMs div 1000),
        <<"transactions">>     => Txs,
        <<"uncles">>           => []
    }.

%% @doc Approximate block size: sum of `serialize_to_binary/1' over the
%% key-block and every micro-block. Cheap; the blocks are already in
%% memory at this point.
block_serialized_size(KeyBlock, MBs) ->
    KBSize = try byte_size(aec_blocks:serialize_to_binary(KeyBlock))
             catch _:_ -> 0
             end,
    KBSize + lists:sum([micro_block_size(MB) || MB <- MBs]).

micro_block_size(MB) ->
    try byte_size(aec_blocks:serialize_to_binary(MB))
    catch _:_ -> 0
    end.

%% @doc v1 marker for the receipts-root field. AE does not yet specify
%% a canonical receipts trie; we emit a deterministic non-zero hash
%% derived from the ordered tx hashes so the field is stable per-block
%% and obviously not zero-padded. Switch to a real receipts trie root
%% once that spec exists.
%%
%% Empty block (no txs) -> the empty-trie keccak constant, matching the
%% eth convention.
receipts_root_marker([]) -> ?EMPTY_TRIE_ROOT;
receipts_root_marker(MBs) ->
    Hashes = [aetx_sign:hash(STx) || MB <- MBs, STx <- aec_blocks:txs(MB)],
    case Hashes of
        []      -> ?EMPTY_TRIE_ROOT;
        _Other  ->
            Concat = iolist_to_binary(Hashes),
            aerpc_encoding:to_hex_data(sha3:hash(256, Concat))
    end.

format_nonce(N) when is_integer(N), N >= 0 ->
    Hex = integer_to_binary(N, 16),
    Padded = pad_left(Hex, 16, $0),
    Lower = string:lowercase(Padded),
    <<"0x", Lower/binary>>.

pad_left(Bin, Width, Pad) ->
    Size = byte_size(Bin),
    if
        Size >= Width -> Bin;
        true ->
            Padding = binary:copy(<<Pad>>, Width - Size),
            <<Padding/binary, Bin/binary>>
    end.

sum_gas_used(MBs) ->
    lists:sum([gas_used_for_micro_block(MB) || MB <- MBs]).

gas_used_for_micro_block(MB) ->
    Txs = aec_blocks:txs(MB),
    lists:sum([signed_tx_gas(STx) || STx <- Txs]).

%% Best-effort gas accounting: for tx types that carry an explicit gas
%% field (contract calls/creates), include it; everything else is 0 from
%% an EVM-style metering perspective.
signed_tx_gas(SignedTx) ->
    try
        {Mod, Tx} = aetx:specialize_callback(aetx_sign:tx(SignedTx)),
        case erlang:function_exported(Mod, gas, 1) of
            true  -> Mod:gas(Tx);
            false -> 0
        end
    catch _:_ -> 0
    end.

transactions_field(MBs, Height, BlockHash, true) ->
    flatten_txs(MBs, Height, BlockHash);
transactions_field(MBs, _Height, _Hash, false) ->
    [ aerpc_encoding:format_tx_hash(aetx_sign:hash(STx))
      || MB <- MBs, STx <- aec_blocks:txs(MB) ].

flatten_txs(MBs, Height, BlockHash) ->
    [ serialize_tx(STx, Height, BlockHash) ||
        MB <- MBs, STx <- aec_blocks:txs(MB) ].

serialize_tx(SignedTx, Height, BlockHash) ->
    TxHash = aetx_sign:hash(SignedTx),
    try
        BlockHashEnc =
            case BlockHash of
                <<>> -> <<"none">>;
                _    -> aeser_api_encoder:encode(micro_block_hash, BlockHash)
            end,
        MetaData = #{
            <<"block_height">> => Height,
            <<"block_hash">>   => BlockHashEnc,
            <<"hash">>         => aeser_api_encoder:encode(tx_hash, TxHash),
            <<"encoded_tx">>   => aeser_api_encoder:encode(
                                     transaction,
                                     aetx_sign:serialize_to_binary(SignedTx))
        },
        aetx_sign:serialize_for_client_inner(SignedTx, MetaData)
    catch _:_ ->
        #{<<"hash">> => aerpc_encoding:format_tx_hash(TxHash)}
    end.
