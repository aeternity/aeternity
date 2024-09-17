%%%-------------------------------------------------------------------
%%% @doc
%%% API for hyperchain chain related information.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain_hc).

%%% Hyperchain API
-export([ epoch/0
        , epoch/1
        , epoch_length/0
        , epoch_length/1
        , block_producer/0
        , block_producer/1
        %, prev_epoch_last_hash/0
        %, prev_epoch_last_hash/1
        , epoch_start_height/1
        ]).

-define(ELECTION_CONTRACT, election).
-define(STAKING_CONTRACT, staking).
-define(REWARDS_CONTRACT, rewards).

-spec epoch() -> 'undefined' | {ok, non_neg_integer()} | {error, chain_too_short}.
epoch() ->
    case aec_chain:top_height() of
        undefined -> undefined;
        Height -> epoch(Height)
    end.

-spec epoch_length() -> 'undefined' | {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_length() ->
    case aec_chain:top_height() of
        undefined -> undefined;
        Height -> epoch_length(Height)
    end.

-spec block_producer() -> 'undefined' | {ok, aec_keys:pubkey()} | {error, chain_too_short}.
block_producer() ->
    case aec_chain:top_height() of
        undefined -> undefined;
        Height -> block_producer(Height)
    end.

-spec epoch(non_neg_integer()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch(Height) ->
    call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "epoch", []).

-spec epoch_length(non_neg_integer()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_length(Height) ->
    call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "epoch_length", []).

-spec block_producer(non_neg_integer()) -> {ok, aec_keys:pubkey()} | {error, chain_too_short}.
block_producer(Height) ->
    call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "leader", []).

-spec epoch_start_height(non_neg_integer()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_start_height(Epoch) ->
    case epoch() of
        {ok, TopEpoch} when TopEpoch >= Epoch ->
            epoch_start_height(Epoch, TopEpoch, aec_chain:top_height());
        _ ->
            {error, chain_too_short}
    end.

epoch_start_height(Epoch, _EpochAtHeight, _Height) ->
  {ok, Epoch}.


%%% --- internal

call_consensus_contract_at_height(Contract, Height, Endpoint, Args) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} ->
           {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, Hash),
           aec_consensus_hc:call_consensus_contract_result(Contract, TxEnv, Trees, Endpoint, Args)
    end.

