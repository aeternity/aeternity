%%%-------------------------------------------------------------------
%%% @doc Transaction adapter for the AE JSON-RPC layer.
%%%
%%% Resolves an AE signed-tx to the JSON shape that ae_getTransactionBy*
%%% methods return. v1 emits the AE-native field set (via
%%% `aetx_sign:serialize_for_client/4') rather than re-mapping every
%%% AE tx type onto the eth tx shape -- a faithful translation requires
%%% a tx-type dispatch table that is non-trivial for non-spend / non-
%%% contract types (oracle, name, channel, paying-for, GA, ...). When
%%% that translation lands it slots in here without touching callers.
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
                {BlockHash, SignedTx} when is_binary(BlockHash) ->
                    {ok, serialize_mined(SignedTx, BlockHash, TxHash)}
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
                {BlockHash, _SignedTx} when is_binary(BlockHash) ->
                    %% Walk the block to find this tx's position, accumulate
                    %% prior cumulative-gas, and build the receipt with the
                    %% correct transactionIndex / cumulativeGasUsed.
                    single_receipt(BlockHash, TxHash)
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

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc Walk the block's tx list to find the requested tx, accumulating
%% cumulative-gas as we go. Returns the receipt with correct
%% transactionIndex + cumulativeGasUsed, or `{ok, null}' if the tx is
%% somehow not in the block (defensive — shouldn't happen since
%% find_tx_with_location/1 just returned this block hash).
single_receipt(BlockHash, TxHash) ->
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            Flat = lists:flatten([aec_blocks:txs(MB) || MB <- MBs]),
            BlockNumber = block_number(BlockHash),
            walk_to_tx(Flat, TxHash, BlockHash, BlockNumber, 0, 0);
        error ->
            {ok, null}
    end.

%% @doc Build receipts for every tx in the generation, in block order.
%% Returns `null' if the block doesn't exist, `[]' if it has no txs.
fold_block_receipts(BlockHash) ->
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            Flat = lists:flatten([aec_blocks:txs(MB) || MB <- MBs]),
            BlockNumber = block_number(BlockHash),
            fold_receipts_inner(Flat, BlockHash, BlockNumber, 0, 0, []);
        error ->
            null
    end.

fold_receipts_inner([], _BH, _BN, _Idx, _Cum, Acc) ->
    lists:reverse(Acc);
fold_receipts_inner([SignedTx | Rest], BlockHash, BlockNumber, Idx, Cum, Acc) ->
    TxHash = aetx_sign:hash(SignedTx),
    {Receipt, NewCum} =
        build_receipt(SignedTx, BlockHash, TxHash, BlockNumber, Idx, Cum),
    fold_receipts_inner(Rest, BlockHash, BlockNumber, Idx + 1, NewCum,
                        [Receipt | Acc]).

walk_to_tx([], _TxHash, _BH, _BN, _Idx, _Cum) ->
    {ok, null};
walk_to_tx([SignedTx | Rest], TxHash, BlockHash, BlockNumber, Idx, Cum) ->
    case aetx_sign:hash(SignedTx) of
        TxHash ->
            {Receipt, _NewCum} =
                build_receipt(SignedTx, BlockHash, TxHash, BlockNumber, Idx, Cum),
            {ok, Receipt};
        _Other ->
            %% Advance cumulative-gas with this tx's gas-used so that the
            %% match further down sees the right running total.
            NewCum = Cum + gas_used_for_signed_tx(SignedTx, BlockHash),
            walk_to_tx(Rest, TxHash, BlockHash, BlockNumber, Idx + 1, NewCum)
    end.

%% @doc Build one receipt with explicit TxIndex / CumulativeBefore inputs.
%% Returns `{Receipt, CumulativeAfter}' so a caller iterating across a
%% block can thread the running total.
-spec build_receipt(aetx_sign:signed_tx(), binary(), binary(),
                    non_neg_integer(), non_neg_integer(),
                    non_neg_integer()) ->
    {map(), non_neg_integer()}.
build_receipt(SignedTx, BlockHash, TxHash, BlockNumber, TxIndex, CumulativeBefore) ->
    Tx = aetx_sign:tx(SignedTx),
    {Type, _Body} = aetx:specialize_type(Tx),
    Origin = aetx:origin(Tx),
    {GasUsed, Status} = gas_and_status(Type, Tx, BlockHash),
    Cumulative = CumulativeBefore + GasUsed,
    Receipt = #{
        <<"transactionHash">>   => aerpc_encoding:format_tx_hash(TxHash),
        <<"transactionIndex">>  => aerpc_encoding:to_quantity(TxIndex),
        <<"blockHash">>         => aerpc_encoding:format_key_block_hash(BlockHash),
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
gas_and_status(contract_call_tx, Tx, BlockHash) ->
    case call_result(Tx, BlockHash) of
        {ok, GasUsed, Status} -> {GasUsed, Status};
        none                  -> {0, <<"0x1">>}
    end;
gas_and_status(contract_create_tx, Tx, BlockHash) ->
    case call_result(Tx, BlockHash) of
        {ok, GasUsed, Status} -> {GasUsed, Status};
        none                  -> {0, <<"0x1">>}
    end;
gas_and_status(_Other, _Tx, _BlockHash) ->
    {0, <<"0x1">>}.

%% Best-effort gas-used lookup for a single tx; mirrors the path
%% gas_and_status/3 uses but returns just the integer. Non-contract
%% txs contribute 0 to the running cumulative.
gas_used_for_signed_tx(SignedTx, BlockHash) ->
    Tx = aetx_sign:tx(SignedTx),
    {Type, _Body} = aetx:specialize_type(Tx),
    {Gas, _Status} = gas_and_status(Type, Tx, BlockHash),
    Gas.

call_result(Tx, BlockHash) ->
    try
        {Mod, _Body} = aetx:specialize_callback(Tx),
        ContractId =
            case erlang:function_exported(Mod, contract_pubkey, 1) of
                true -> Mod:contract_pubkey(Tx);
                false -> undefined
            end,
        CallerId =
            case erlang:function_exported(Mod, caller_pubkey, 1) of
                true -> Mod:caller_pubkey(Tx);
                false -> aetx:origin(Tx)
            end,
        case ContractId of
            undefined -> none;
            Cid ->
                case aec_chain:get_contract_call(Cid, CallerId, BlockHash) of
                    {ok, Call} ->
                        Status = case aect_call:return_type(Call) of
                                     ok -> <<"0x1">>;
                                     _  -> <<"0x0">>
                                 end,
                        {ok, aect_call:gas_used(Call), Status};
                    {error, _Reason} ->
                        none
                end
        end
    catch _:_ -> none
    end.

to_field(spend_tx, Tx) ->
    try
        {Mod, _Body} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, recipient_pubkey, 1) of
            true  -> format_account_or_null(Mod:recipient_pubkey(Tx));
            false -> null
        end
    catch _:_ -> null
    end;
to_field(contract_call_tx, Tx) ->
    try
        {Mod, _Body} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, contract_pubkey, 1) of
            true  -> aerpc_encoding:format_contract(Mod:contract_pubkey(Tx));
            false -> null
        end
    catch _:_ -> null
    end;
to_field(contract_create_tx, _Tx) ->
    null;
to_field(_, _) ->
    null.

contract_address(contract_create_tx, Tx) ->
    try
        {Mod, _Body} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, contract_pubkey, 1) of
            true  -> aerpc_encoding:format_contract(Mod:contract_pubkey(Tx));
            false -> null
        end
    catch _:_ -> null
    end;
contract_address(_, _) ->
    null.

effective_gas_price(Tx) ->
    try
        {Mod, _Body} = aetx:specialize_callback(Tx),
        case erlang:function_exported(Mod, gas_price, 1) of
            true  -> aerpc_encoding:to_quantity(Mod:gas_price(Tx));
            false -> <<"0x0">>
        end
    catch _:_ -> <<"0x0">>
    end.

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
    try aetx_sign:serialize_for_client_pending(SignedTx)
    catch _:_ ->
        #{<<"hash">> => aerpc_encoding:format_tx_hash(aetx_sign:hash(SignedTx))}
    end.

serialize_mined(SignedTx, BlockHash, TxHash) ->
    case aec_chain:get_header(BlockHash) of
        {ok, Header} ->
            try aetx_sign:serialize_for_client(Header, SignedTx)
            catch _:_ ->
                #{<<"hash">> => aerpc_encoding:format_tx_hash(TxHash)}
            end;
        error ->
            #{<<"hash">> => aerpc_encoding:format_tx_hash(TxHash)}
    end.

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
