%%%-------------------------------------------------------------------
%%% @doc Fee-history helpers for the eth-compat fee oracle.
%%%
%%% AE does not have EIP-1559: every contract tx pays a flat
%%% `gas_price' chosen by the sender, and the only floor is the
%%% mempool's configured `minimum_miner_gas_price'. The methods
%%% therefore degenerate to constants for `baseFeePerGas' and a
%%% real-but-usually-zero distribution for `reward'.
%%%
%%% Returning the methods (rather than `-32601') silences viem's
%%% fee-oracle warning on init, which the forked Ponder will hit
%%% on every client construction.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_fee).

-export([fee_history/3, max_priority_fee/0]).

-define(MAX_BLOCK_COUNT, 1024).
%% Window for the priority-fee tip estimator. 20 generations is the eth
%% reference implementation's choice; tuning hook lives here.
-define(PRIORITY_WINDOW, 20).
%% Percentile used by max_priority_fee (matches geth's "rough median").
-define(PRIORITY_PERCENTILE, 60).

%% ===================================================================
%% Public API
%% ===================================================================

-spec fee_history(non_neg_integer(), binary(),
                  [number()] | undefined) ->
    {ok, map()} | {error, integer(), binary()}.
fee_history(BlockCount, NewestBlock, RewardPercentiles)
  when is_integer(BlockCount), BlockCount >= 0,
       is_binary(NewestBlock) ->
    Capped = min(BlockCount, ?MAX_BLOCK_COUNT),
    case aerpc_block:resolve_tag(NewestBlock) of
        {ok, NewestH} ->
            {OldestH, Heights} = range(NewestH, Capped),
            Base = baseline_fee(),
            BaseList = [aerpc_encoding:to_quantity(Base)
                          || _ <- lists:seq(0, Capped)],
            Ratios = [gas_used_ratio(H) || H <- Heights],
            Result0 = #{<<"oldestBlock">>   => aerpc_encoding:to_quantity(OldestH),
                        <<"baseFeePerGas">> => BaseList,
                        <<"gasUsedRatio">>  => Ratios},
            Result = case RewardPercentiles of
                         undefined ->
                             Result0;
                         _Ps when is_list(_Ps) ->
                             Result0#{<<"reward">> =>
                                          rewards_for(Heights, _Ps, Base)}
                     end,
            {ok, Result};
        {error, _, _} = Err -> Err
    end;
fee_history(_BC, _NB, _Pcs) ->
    {error, -32602, <<"Invalid params">>}.

-spec max_priority_fee() -> {ok, binary()}.
max_priority_fee() ->
    TopH = top_height(),
    Floor = baseline_fee(),
    Window = lists:seq(max(0, TopH - ?PRIORITY_WINDOW + 1), TopH),
    Tips = lists:flatten([tips_at(H, Floor) || H <- Window]),
    P = case Tips of
            []     -> 0;
            _Other -> percentile(lists:sort(Tips), ?PRIORITY_PERCENTILE)
        end,
    {ok, aerpc_encoding:to_quantity(P)}.

%% ===================================================================
%% Internal
%% ===================================================================

range(NewestH, Count) when Count =< 0 ->
    {NewestH, []};
range(NewestH, Count) ->
    OldestH = max(0, NewestH - Count + 1),
    {OldestH, lists:seq(OldestH, NewestH)}.

baseline_fee() ->
    try aec_tx_pool:minimum_miner_gas_price() of
        N when is_integer(N), N >= 0 -> N;
        _Other                       -> 0
    catch _:_ -> 0
    end.

top_height() ->
    try aec_chain:top_header() of
        undefined -> 0;
        Header    -> aec_headers:height(Header)
    catch _:_ -> 0
    end.

%% gas_used_ratio: as eth spec, gasUsed / max(1, gasLimit) for each
%% block in the window. Always emitted as a JSON float so jsx encodes
%% it correctly.
gas_used_ratio(H) ->
    GasLimit = max(1, try aec_governance:block_gas_limit() catch _:_ -> 1 end),
    GasUsed  = block_gas_used(H),
    GasUsed / GasLimit.

block_gas_used(H) ->
    try
        case aec_chain:get_key_block_by_height(H) of
            {ok, KeyBlock} ->
                {ok, Hash} = aec_blocks:hash_internal_representation(KeyBlock),
                case aec_chain:get_generation_by_hash(Hash, forward) of
                    {ok, #{micro_blocks := MBs}} ->
                        lists:sum([sum_tx_gas(MB) || MB <- MBs]);
                    error -> 0
                end;
            _Other -> 0
        end
    catch _:_ -> 0
    end.

sum_tx_gas(MB) ->
    lists:sum([signed_tx_gas(STx) || STx <- aec_blocks:txs(MB)]).

%% Use the contract tx's gas-limit field as the conservative gas
%% measure here; the call-object lookup (used by aerpc_tx for receipts)
%% would be more accurate but more expensive, and feeHistory is
%% advisory.
signed_tx_gas(SignedTx) ->
    try
        {Mod, Tx} = aetx:specialize_callback(aetx_sign:tx(SignedTx)),
        case erlang:function_exported(Mod, gas, 1) of
            true  -> Mod:gas(Tx);
            false -> 0
        end
    catch _:_ -> 0
    end.

rewards_for(Heights, Percentiles, Floor) ->
    [percentiles_for_block(H, Percentiles, Floor) || H <- Heights].

percentiles_for_block(H, Percentiles, Floor) ->
    Tips = lists:sort(tips_at(H, Floor)),
    [aerpc_encoding:to_quantity(percentile(Tips, P)) || P <- Percentiles].

tips_at(H, Floor) ->
    try
        case aec_chain:get_key_block_by_height(H) of
            {ok, KeyBlock} ->
                {ok, Hash} = aec_blocks:hash_internal_representation(KeyBlock),
                case aec_chain:get_generation_by_hash(Hash, forward) of
                    {ok, #{micro_blocks := MBs}} ->
                        [max(0, gas_price_of(STx) - Floor)
                            || MB <- MBs, STx <- aec_blocks:txs(MB)];
                    error -> []
                end;
            _Other -> []
        end
    catch _:_ -> []
    end.

gas_price_of(SignedTx) ->
    try
        {Mod, Tx} = aetx:specialize_callback(aetx_sign:tx(SignedTx)),
        case erlang:function_exported(Mod, gas_price, 1) of
            true  -> Mod:gas_price(Tx);
            false -> 0
        end
    catch _:_ -> 0
    end.

%% Eth feeHistory percentiles are passed as floats in [0, 100].
%% Implementation note: linear interpolation between adjacent samples
%% would be more faithful, but eth clients tolerate a nearest-rank
%% estimator; cost vs benefit doesn't warrant interp here.
percentile([], _P)    -> 0;
percentile(Sorted, P) when is_number(P), P >= 0, P =< 100 ->
    N = length(Sorted),
    Idx = max(1, min(N, trunc((P / 100) * N + 0.5))),
    lists:nth(Idx, Sorted);
percentile(_Sorted, _P) -> 0.
