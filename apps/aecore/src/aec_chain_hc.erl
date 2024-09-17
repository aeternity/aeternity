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
        , epoch_info/0
        , epoch_info/1
        , block_producer/0
        , block_producer/1
        , epoch_start_height/1
        ]).

-define(ELECTION_CONTRACT, election).
-define(STAKING_CONTRACT, staking).
-define(REWARDS_CONTRACT, rewards).

-spec epoch() -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch() ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height -> epoch(Height)
    end.

-spec epoch_length() -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_length() ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height -> epoch_length(Height)
    end.

-spec block_producer() -> {ok, aec_keys:pubkey()} | {error, chain_too_short}.
block_producer() ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height -> block_producer(Height)
    end.

-spec epoch_info() -> {ok, #{first => non_neg_integer(),
                             at => non_neg_integer(),
                             length => non_neg_integer()}} | {error, chain_too_short}.
epoch_info() ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height -> epoch_info(Height)
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

-spec epoch_info(non_neg_integer()) -> {ok, #{first => non_neg_integer(),
                                              at => non_neg_integer(),
                                              length => non_neg_integer()}}.
epoch_info(Height) ->
    {ok, LeftToFill} = call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "blocks_to_fill_epoch", []),
    {ok, Length} =  epoch_length(Height),
    HeightInEpoch = Length - LeftToFill,
    {ok, #{first => Height - HeightInEpoch,
           at => Height + HeightInEpoch,
           length => Length}}.

-spec epoch_start_height(non_neg_integer()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_start_height(Epoch) ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height ->
           {ok, #{first := StartHeight}} = epoch_info(Height),
           epoch_start_height(Epoch, StartHeight)
    end.

epoch_start_height(Epoch, EStartHeight) ->
    case epoch(EStartHeight) of
        {ok, EpochNum} when EpochNum == Epoch ->
            {ok, EStartHeight};
        {ok, EpochNum} when EpochNum > Epoch ->
            {ok, Length} = epoch_length(EStartHeight - 1),
            epoch_start_height(Epoch, EStartHeight - Length);
        _ ->
            {error, chain_too_short}
    end.

%%% --- internal

call_consensus_contract_at_height(Contract, Height, Endpoint, Args) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} ->
           {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, Hash),
           aec_consensus_hc:call_consensus_contract_result(Contract, TxEnv, Trees, Endpoint, Args)
    end.

