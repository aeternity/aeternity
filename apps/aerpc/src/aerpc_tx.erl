%%%-------------------------------------------------------------------
%%% @doc Transaction adapter for the eth-compatible JSON-RPC layer.
%%%
%%% Emits the eth transaction object, not AE's own field set. The
%%% earlier `aetx_sign:serialize_for_client/2' passthrough returned
%%% `ak_...' / `th_...' strings and AE-specific keys, which no eth
%%% client can read; `to_eth_tx/4' is the translation.
%%%
%%% The mapping is exact for the tx types eth has a counterpart for
%%% (`spend_tx' -> a value transfer, `contract_call_tx' -> a call,
%%% `contract_create_tx' -> a deploy) and degrades for the ones it does
%%% not (oracle, name, channel, paying-for, GA): those keep the common
%%% envelope -- hash, from, nonce, block position -- and report `to' as
%%% null with zero value/gas. That is lossy by construction; a caller
%%% needing the full AE tx body uses the node's own REST API, or
%%% `eth_getRawTransactionByHash' for the wire bytes.
%%%
%%% Signature fields. AE signs with ed25519 over a network-id-tagged
%%% payload; there is no secp256k1 recovery id, so `v' / `r' / `s' are
%%% NOT an eth signature and `ecrecover' over them yields nothing. We
%%% emit `r' and `s' as the two 32-byte halves of the first ed25519
%%% signature and `v' as `0x0', because ethers' response formatter
%%% expects the keys to be present and well-formed. Treat them as
%%% opaque bytes carrying the real signature, not as a recoverable one.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_tx).

-export([
          by_hash/1
        , by_block_hash_index/2
        , by_block_height_index/2
        , receipt/1
        , block_receipts_by_hash/1
        , block_receipts_by_height/1
        , raw_by_hash/1
        , to_eth_tx/4
        , gas_used_in_generation/1
        ]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec by_hash(binary()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_hash(HashIn) when is_binary(HashIn) ->
    case decode_tx_hash(HashIn) of
        {ok, TxHash} ->
            case aec_chain:find_tx_with_location(TxHash) of
                none ->
                    {ok, null};
                {mempool, SignedTx} ->
                    {ok, serialize_pending(SignedTx)};
                {MicroHash, SignedTx} when is_binary(MicroHash) ->
                    %% find_tx_with_location/1 returns the MICRO-block
                    %% hash. Eth's `blockHash' must be the block the tx
                    %% is in as eth_getBlockByHash understands it, which
                    %% here is the generation's key block -- emitting the
                    %% micro-block hash made the same tx read two
                    %% different ways disagree.
                    {ok, serialize_mined(SignedTx, MicroHash, TxHash)}
            end;
        {error, _, _} = Err ->
            Err
    end;
by_hash(_) ->
    {error, -32602, <<"Invalid params">>}.

-spec by_block_hash_index(binary(), non_neg_integer()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_block_hash_index(HashIn, Index) when is_binary(HashIn), is_integer(Index) ->
    case aerpc_block:decode_block_hash(HashIn) of
        {ok, BlockHash} ->
            nth_tx(BlockHash, Index);
        {error, _, _} = Err ->
            Err
    end;
by_block_hash_index(_, _) ->
    {error, -32602, <<"Invalid params">>}.

-spec by_block_height_index(binary(), non_neg_integer()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_block_height_index(TagOrHex, Index)
  when is_binary(TagOrHex), is_integer(Index) ->
    case aerpc_block:resolve_tag(TagOrHex) of
        {ok, Height} ->
            case aec_chain:get_key_block_by_height(Height) of
                {ok, KeyBlock} ->
                    {ok, BlockHash} =
                        aec_blocks:hash_internal_representation(KeyBlock),
                    nth_tx(BlockHash, Index);
                {error, _Reason} ->
                    {ok, null}
            end;
        {error, _, _} = Err ->
            Err
    end;
by_block_height_index(_, _) ->
    {error, -32602, <<"Invalid params">>}.

-spec receipt(binary()) ->
    {ok, map() | null} | {error, integer(), binary()}.
receipt(HashIn) when is_binary(HashIn) ->
    case decode_tx_hash(HashIn) of
        {ok, TxHash} ->
            case aec_chain:find_tx_with_location(TxHash) of
                none ->
                    {ok, null};
                {mempool, _Stx} ->
                    %% Eth: receipts are unavailable for pending txs.
                    {ok, null};
                {MicroHash, _SignedTx} when is_binary(MicroHash) ->
                    %% Walk the generation to find this tx's position,
                    %% accumulate prior cumulative-gas, and build the
                    %% receipt with the correct transactionIndex /
                    %% cumulativeGasUsed. The generation is keyed by the
                    %% KEY-block hash; handing get_generation_by_hash/2
                    %% the micro-block hash this returns made every
                    %% receipt come back null.
                    case generation_hash(MicroHash) of
                        {ok, KBHash} -> single_receipt(KBHash, TxHash);
                        error        -> {ok, null}
                    end
            end;
        {error, _, _} = Err ->
            Err
    end;
receipt(_) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Bulk-fetch every receipt for a block, addressed by its key-block
%% hash. Threads cumulative-gas across the fold so consecutive receipts
%% inside the same block have a monotonically non-decreasing
%% cumulativeGasUsed -- which is what eth-shaped indexers expect.
%% Returns `{ok, null}' if the block isn't found.
-spec block_receipts_by_hash(binary()) ->
    {ok, [map()] | null} | {error, integer(), binary()}.
block_receipts_by_hash(HashIn) when is_binary(HashIn) ->
    case aerpc_block:decode_block_hash(HashIn) of
        {ok, BlockHash} -> {ok, fold_block_receipts(BlockHash)};
        {error, _, _} = Err -> Err
    end;
block_receipts_by_hash(_) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Bulk-fetch every receipt for a block, addressed by tag/height.
-spec block_receipts_by_height(binary()) ->
    {ok, [map()] | null} | {error, integer(), binary()}.
block_receipts_by_height(TagOrHex) when is_binary(TagOrHex) ->
    case aerpc_block:resolve_tag(TagOrHex) of
        {ok, Height} ->
            case aec_chain:get_key_block_by_height(Height) of
                {ok, KeyBlock} ->
                    {ok, BlockHash} =
                        aec_blocks:hash_internal_representation(KeyBlock),
                    {ok, fold_block_receipts(BlockHash)};
                {error, _Reason} ->
                    {ok, null}
            end;
        {error, _, _} = Err ->
            Err
    end;
block_receipts_by_height(_) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Wire-encoded bytes for a signed tx, as 0x-hex. AE's
%% serialization, not eth's RLP -- callers that need to re-broadcast
%% must pipe back through an AE-aware path. Returns null for an
%% unknown hash.
-spec raw_by_hash(binary()) ->
    {ok, binary() | null} | {error, integer(), binary()}.
raw_by_hash(HashIn) when is_binary(HashIn) ->
    case decode_tx_hash(HashIn) of
        {ok, TxHash} ->
            case aec_chain:find_tx_with_location(TxHash) of
                none           -> {ok, null};
                {mempool, STx} -> {ok, encode_signed_tx(STx)};
                {_BH, STx}     -> {ok, encode_signed_tx(STx)}
            end;
        {error, _, _} = Err -> Err
    end;
raw_by_hash(_) ->
    {error, -32602, <<"Invalid params">>}.

encode_signed_tx(SignedTx) ->
    aerpc_encoding:to_hex_data(aetx_sign:serialize_to_binary(SignedTx)).

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc Resolve any block hash to the key-block hash of its generation.
%% `aec_chain:get_generation_by_hash/2' and eth's `blockHash' are both
%% keyed by the key block, while `find_tx_with_location/1' hands back the
%% micro block the tx actually sits in.
-spec generation_hash(binary()) -> {ok, binary()} | error.
generation_hash(BlockHash) ->
    case aec_chain:get_header(BlockHash) of
        {ok, Header} ->
            case aec_headers:type(Header) of
                micro -> {ok, aec_headers:prev_key_hash(Header)};
                key   -> {ok, BlockHash}
            end;
        error ->
            error
    end.

%% @doc Flatten a generation into `{SignedTx, MicroBlockHash}' pairs in
%% block order. The micro-block hash is carried per tx because the calls
%% trie is read AT a block's state and resets per generation: at the
%% key-block hash the generation's own calls do not exist yet, so a
%% contract call must be looked up at the state of the micro block that
%% contains it. Hashing is done once per micro block, not once per tx.
-spec generation_txs(binary()) -> {ok, [{term(), binary()}]} | error.
generation_txs(KBHash) ->
    case aec_chain:get_generation_by_hash(KBHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            {ok, lists:append([micro_block_txs(MB) || MB <- MBs])};
        error ->
            error
    end.

micro_block_txs(MB) ->
    MBHash = case aec_blocks:hash_internal_representation(MB) of
                 {ok, H} -> H;
                 _Other  -> <<>>
             end,
    [{STx, MBHash} || STx <- aec_blocks:txs(MB)].

%% @doc Walk the generation's tx list to find the requested tx,
%% accumulating cumulative-gas as we go. Returns the receipt with correct
%% transactionIndex + cumulativeGasUsed, or `{ok, null}' if the tx is
%% somehow not in the generation.
single_receipt(KBHash, TxHash) ->
    case generation_txs(KBHash) of
        {ok, Flat} ->
            BlockNumber = block_number(KBHash),
            walk_to_tx(Flat, TxHash, KBHash, BlockNumber, 0, 0);
        error ->
            {ok, null}
    end.

%% @doc Build receipts for every tx in the generation, in block order.
%% Returns `null' if the block doesn't exist, `[]' if it has no txs.
fold_block_receipts(KBHash) ->
    case generation_txs(KBHash) of
        {ok, Flat} ->
            BlockNumber = block_number(KBHash),
            fold_receipts_inner(Flat, KBHash, BlockNumber, 0, 0, []);
        error ->
            null
    end.

fold_receipts_inner([], _BH, _BN, _Idx, _Cum, Acc) ->
    lists:reverse(Acc);
fold_receipts_inner([{SignedTx, MBHash} | Rest], KBHash, BlockNumber,
                    Idx, Cum, Acc) ->
    TxHash = aetx_sign:hash(SignedTx),
    {Receipt, NewCum} =
        build_receipt(SignedTx, KBHash, MBHash, TxHash, BlockNumber, Idx, Cum),
    fold_receipts_inner(Rest, KBHash, BlockNumber, Idx + 1, NewCum,
                        [Receipt | Acc]).

walk_to_tx([], _TxHash, _BH, _BN, _Idx, _Cum) ->
    {ok, null};
walk_to_tx([{SignedTx, MBHash} | Rest], TxHash, KBHash, BlockNumber, Idx, Cum) ->
    case aetx_sign:hash(SignedTx) of
        TxHash ->
            {Receipt, _NewCum} =
                build_receipt(SignedTx, KBHash, MBHash, TxHash, BlockNumber,
                              Idx, Cum),
            {ok, Receipt};
        _Other ->
            %% Advance cumulative-gas with this tx's gas-used so that the
            %% match further down sees the right running total.
            NewCum = Cum + gas_used_for_signed_tx(SignedTx, MBHash),
            walk_to_tx(Rest, TxHash, KBHash, BlockNumber, Idx + 1, NewCum)
    end.

%% @doc Total gas actually consumed by a generation: the sum of the same
%% per-tx figures the receipts report, so `block.gasUsed' equals the sum
%% of its own receipts by construction. That equality is a cross-check
%% indexers run, and summing declared gas limits instead could never
%% satisfy it.
-spec gas_used_in_generation(binary()) -> non_neg_integer().
gas_used_in_generation(KBHash) ->
    case generation_txs(KBHash) of
        {ok, Flat} ->
            lists:sum([gas_used_for_signed_tx(STx, MBHash)
                       || {STx, MBHash} <- Flat]);
        error ->
            0
    end.

%% @doc Build one receipt with explicit TxIndex / CumulativeBefore inputs.
%% Returns `{Receipt, CumulativeAfter}' so a caller iterating across a
%% block can thread the running total.
%% `KBHash' is what the receipt reports as `blockHash'; `MBHash' is what
%% the call-object lookup is performed at. They are different hashes and
%% conflating them is what made every contract receipt read gasUsed 0x0,
%% status 0x1 -- indistinguishable from success, including for reverts.
-spec build_receipt(aetx_sign:signed_tx(), binary(), binary(), binary(),
                    non_neg_integer(), non_neg_integer(),
                    non_neg_integer()) ->
    {map(), non_neg_integer()}.
build_receipt(SignedTx, KBHash, MBHash, TxHash, BlockNumber, TxIndex,
              CumulativeBefore) ->
    Tx = aetx_sign:tx(SignedTx),
    {Type, _Body} = aetx:specialize_type(Tx),
    Origin = aetx:origin(Tx),
    {GasUsed, Status} = gas_and_status(Type, Tx, MBHash),
    Cumulative = CumulativeBefore + GasUsed,
    Receipt = #{
        <<"transactionHash">>   => aerpc_encoding:format_tx_hash(TxHash),
        <<"transactionIndex">>  => aerpc_encoding:to_quantity(TxIndex),
        <<"blockHash">>         => aerpc_encoding:format_key_block_hash(KBHash),
        <<"blockNumber">>       => aerpc_encoding:to_quantity(BlockNumber),
        <<"from">>              => format_account_or_null(Origin),
        <<"to">>                => to_field(Type, Tx),
        <<"cumulativeGasUsed">> => aerpc_encoding:to_quantity(Cumulative),
        <<"effectiveGasPrice">> => effective_gas_price(Tx),
        <<"gasUsed">>           => aerpc_encoding:to_quantity(GasUsed),
        <<"contractAddress">>   => contract_address(Type, Tx),
        <<"logs">>              => [],
        <<"logsBloom">>         => aerpc_bloom:empty(),
        <<"type">>              => <<"0x0">>,
        <<"status">>            => Status
    },
    {Receipt, Cumulative}.

block_number(BlockHash) ->
    case aec_chain:get_header(BlockHash) of
        {ok, Header} -> aec_headers:height(Header);
        error        -> 0
    end.

%% Return {GasUsed, Status} for one signed tx. Contract calls/creates
%% pull the real gas-used + status from the call object; non-contract
%% txs always succeed (status 0x1) and report 0 gas (AE spend-tx has no
%% EVM-style metering -- documented in plan 03).
gas_and_status(contract_call_tx, Tx, MBHash) ->
    case call_result(Tx, MBHash) of
        {ok, GasUsed, Status} -> {GasUsed, Status};
        none                  -> {0, <<"0x1">>}
    end;
gas_and_status(contract_create_tx, Tx, MBHash) ->
    case call_result(Tx, MBHash) of
        {ok, GasUsed, Status} -> {GasUsed, Status};
        none                  -> {0, <<"0x1">>}
    end;
gas_and_status(_Other, _Tx, _MBHash) ->
    {0, <<"0x1">>}.

%% Gas-used lookup for a single tx; mirrors the path gas_and_status/3
%% uses but returns just the integer. Non-contract txs contribute 0 to
%% the running cumulative. `MBHash' is the micro-block hash, not the
%% generation's key block -- see call_result/2.
gas_used_for_signed_tx(SignedTx, MBHash) ->
    Tx = aetx_sign:tx(SignedTx),
    {Type, _Body} = aetx:specialize_type(Tx),
    {Gas, _Status} = gas_and_status(Type, Tx, MBHash),
    Gas.

%% `aec_chain:get_contract_call/3' wants {ContractPubkey, CallId,
%% BlockHash}. The previous version passed the CALLER pubkey where the
%% call id belongs and probed `contract_pubkey/1' on `aect_call_tx',
%% which does not export it -- so the lookup never resolved and every
%% contract receipt reported gasUsed 0 with status 0x1, including
%% reverted calls. Both tx modules already compute the exact call id;
%% name them rather than probing.
call_result(Tx, BlockHash) ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        case call_lookup_keys(Mod, Inner) of
            {ok, ContractPK, CallId} ->
                case aec_chain:get_contract_call(ContractPK, CallId, BlockHash) of
                    {ok, Call} ->
                        Status = case aect_call:return_type(Call) of
                                     ok -> <<"0x1">>;
                                     _  -> <<"0x0">>
                                 end,
                        {ok, aect_call:gas_used(Call), Status};
                    {error, _Reason} ->
                        none
                end;
            none ->
                none
        end
    catch _:_ -> none
    end.

call_lookup_keys(aect_call_tx, Inner) ->
    {ok, aect_call_tx:ct_call_id(Inner), aect_call_tx:call_id(Inner)};
call_lookup_keys(aect_create_tx, Inner) ->
    {ok, aect_create_tx:contract_pubkey(Inner), aect_create_tx:call_id(Inner)};
call_lookup_keys(_Mod, _Inner) ->
    none.

%% Two accessor-name mistakes made every one of these fields null.
%% `aec_spend_tx' exports `recipient_id/1', not `recipient_pubkey/1',
%% and `aect_call_tx' exports `contract_id/1', not `contract_pubkey/1',
%% so `function_exported/3' was false and the `to' field was always
%% null. Independently, the surviving branches passed the OUTER `aetx'
%% record where the callback wanted its own inner tx, which raised
%% function_clause straight into `catch _:_ -> null'. Both are fixed by
%% going through the id accessors with the inner tx.
to_field(spend_tx, Tx) ->
    pubkey_from_id(Tx, recipient_id);
to_field(contract_call_tx, Tx) ->
    pubkey_from_id(Tx, contract_id);
to_field(contract_create_tx, _Tx) ->
    %% Eth reports a deploy with `to: null'; the new address is on the
    %% receipt's `contractAddress'.
    null;
to_field(_, _) ->
    null.

%% @doc Read an `aeser_id' field off a tx and emit its 32-byte pubkey.
%% A spend to a name (`{name, Hash}') has no account pubkey to report,
%% so it comes out null rather than as a name hash pretending to be an
%% address.
pubkey_from_id(Tx, Accessor) ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, Accessor, 1) of
            true ->
                case aeser_id:specialize(Mod:Accessor(Inner)) of
                    {account,  PK} -> aerpc_encoding:format_account(PK);
                    {contract, PK} -> aerpc_encoding:format_contract(PK);
                    _Other         -> null
                end;
            false ->
                null
        end
    catch _:_ -> null
    end.

contract_address(contract_create_tx, Tx) ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, contract_pubkey, 1) of
            true  -> aerpc_encoding:format_contract(Mod:contract_pubkey(Inner));
            false -> null
        end
    catch _:_ -> null
    end;
contract_address(_, _) ->
    null.

effective_gas_price(Tx) ->
    aerpc_encoding:to_quantity(gas_price_of(Tx)).

format_account_or_null(undefined) -> null;
format_account_or_null(<<>>)      -> null;
format_account_or_null(Pubkey) when is_binary(Pubkey) ->
    aerpc_encoding:format_account(Pubkey).

nth_tx(BlockHash, Index) ->
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            Flat = lists:flatten([aec_blocks:txs(MB) || MB <- MBs]),
            case nth_safe(Index + 1, Flat) of
                {ok, SignedTx} ->
                    TxHash = aetx_sign:hash(SignedTx),
                    {ok, serialize_mined(SignedTx, BlockHash, TxHash)};
                none ->
                    {ok, null}
            end;
        error ->
            {ok, null}
    end.

nth_safe(N, _) when N =< 0 -> none;
nth_safe(_, [])            -> none;
nth_safe(1, [H | _])       -> {ok, H};
nth_safe(N, [_ | T])       -> nth_safe(N - 1, T).

serialize_pending(SignedTx) ->
    %% Eth reports a mempool tx with null block position.
    to_eth_tx(SignedTx, null, null, null).

%% `MicroHash' is what find_tx_with_location/1 returns; both the eth
%% `blockHash' and the transactionIndex are properties of the generation,
%% so resolve to the key block first.
serialize_mined(SignedTx, MicroHash, TxHash) ->
    case generation_hash(MicroHash) of
        {ok, KBHash} ->
            BlockNumber = block_number(KBHash),
            TxIndex     = tx_index_in_block(KBHash, TxHash),
            to_eth_tx(SignedTx, KBHash, BlockNumber, TxIndex);
        error ->
            to_eth_tx(SignedTx, null, null, null)
    end.

%% @doc Position of a tx inside its generation, or `null' if it is not
%% found there. Eth requires `transactionIndex' on a mined tx, and it
%% is the flat index across the generation's micro-blocks -- the same
%% ordering `eth_getBlockByNumber' uses for its `transactions' array,
%% so the two agree by construction.
tx_index_in_block(KBHash, TxHash) ->
    case generation_txs(KBHash) of
        {ok, Flat} -> index_of(TxHash, Flat, 0);
        error      -> null
    end.

index_of(_TxHash, [], _N) -> null;
index_of(TxHash, [{STx, _MBHash} | Rest], N) ->
    case aetx_sign:hash(STx) of
        TxHash -> N;
        _Other -> index_of(TxHash, Rest, N + 1)
    end.

%% @doc Translate one AE signed tx into the eth transaction object.
%% `BlockHash' / `BlockNumber' / `TxIndex' are `null' for a pending tx.
-spec to_eth_tx(aetx_sign:signed_tx(), binary() | null,
                non_neg_integer() | null, non_neg_integer() | null) -> map().
to_eth_tx(SignedTx, BlockHash, BlockNumber, TxIndex) ->
    Tx = aetx_sign:tx(SignedTx),
    {Type, _Body} = aetx:specialize_type(Tx),
    {R, S} = signature_halves(SignedTx),
    #{
        <<"hash">>             => aerpc_encoding:format_tx_hash(
                                      aetx_sign:hash(SignedTx)),
        <<"blockHash">>        => maybe_hash(BlockHash),
        <<"blockNumber">>      => maybe_quantity(BlockNumber),
        <<"transactionIndex">> => maybe_quantity(TxIndex),
        <<"from">>             => format_account_or_null(aetx:origin(Tx)),
        <<"to">>               => to_field(Type, Tx),
        <<"value">>            => aerpc_encoding:to_quantity(value_of(Type, Tx)),
        <<"gas">>              => aerpc_encoding:to_quantity(gas_of(Tx)),
        <<"gasPrice">>         => aerpc_encoding:to_quantity(gas_price_of(Tx)),
        <<"input">>            => aerpc_encoding:to_hex_data(input_of(Type, Tx)),
        <<"nonce">>            => aerpc_encoding:to_quantity(nonce_of(Tx)),
        <<"type">>             => <<"0x0">>,
        <<"chainId">>          => aerpc_encoding:to_quantity(
                                      aerpc_chain_id:current()),
        <<"v">>                => <<"0x0">>,
        <<"r">>                => R,
        <<"s">>                => S
    }.

maybe_hash(null) -> null;
maybe_hash(Hash) -> aerpc_encoding:format_key_block_hash(Hash).

maybe_quantity(null) -> null;
maybe_quantity(N)    -> aerpc_encoding:to_quantity(N).

%% AE signature lists can be empty (dry-run synthesised txs) or carry
%% several signatures (multisig / GA). Eth has room for exactly one, so
%% the first is what is exposed; the full list is in the raw bytes.
signature_halves(SignedTx) ->
    Zero = aerpc_encoding:zero_word(),
    try aetx_sign:signatures(SignedTx) of
        [<<R:32/binary, S:32/binary>> | _] ->
            {aerpc_encoding:to_hex_data(R), aerpc_encoding:to_hex_data(S)};
        _Other ->
            {Zero, Zero}
    catch _:_ ->
        {Zero, Zero}
    end.

nonce_of(Tx) ->
    try aetx:nonce(Tx) of
        N when is_integer(N), N >= 0 -> N;
        _Other                       -> 0
    catch _:_ -> 0
    end.

gas_price_of(Tx) ->
    try aetx:gas_price(Tx) of
        N when is_integer(N), N >= 0 -> N;
        _Other                       -> 0   %% undefined for non-gas tx types
    catch _:_ -> 0
    end.

gas_of(Tx) ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, gas, 1) of
            true  -> Mod:gas(Inner);
            false -> 0
        end
    catch _:_ -> 0
    end.

%% The AE leg only. A contract call that moves AEX-9 tokens reports
%% `value: 0x0' here, exactly as an ERC-20 transfer does on eth.
value_of(Type, Tx)
  when Type =:= spend_tx;
       Type =:= contract_call_tx;
       Type =:= contract_create_tx ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, amount, 1) of
            true  -> Mod:amount(Inner);
            false -> 0
        end
    catch _:_ -> 0
    end;
value_of(_Other, _Tx) ->
    0.

input_of(Type, Tx) when Type =:= contract_call_tx;
                        Type =:= contract_create_tx ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, call_data, 1) of
            true  -> Mod:call_data(Inner);
            false -> <<>>
        end
    catch _:_ -> <<>>
    end;
input_of(_Other, _Tx) ->
    <<>>.

decode_tx_hash(<<"th_", _/binary>> = Encoded) ->
    case aeapi:decode_tx_hash(Encoded) of
        {ok, Bin} -> {ok, Bin};
        _Error    -> {error, -32602, <<"Invalid params">>}
    end;
decode_tx_hash(<<"0x", _/binary>> = Hex) ->
    try
        Bin = aerpc_encoding:from_hex_data(Hex),
        case byte_size(Bin) of
            32 -> {ok, Bin};
            _  -> {error, -32602, <<"Invalid params">>}
        end
    catch _:_ -> {error, -32602, <<"Invalid params">>}
    end;
decode_tx_hash(_) ->
    {error, -32602, <<"Invalid params">>}.
