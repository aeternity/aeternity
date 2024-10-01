%%%-------------------------------------------------------------------
%%% @doc
%%% API for hyperchain chain related information.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain_hc).

%%% Hyperchain API
-export([ %% height determined
          epoch/0
        , epoch/1
        , epoch_length/0
        , epoch_length/1
        , epoch_info/0
        , epoch_info/1
        , validators_at_height/1
        , validator_schedule_at_height/1
        %% epoch determined
        , epoch_start_height/1
        , epoch_info_for_epoch/1
        , validator_schedule/1
        , entropy_hash/1
        ]).

-define(ELECTION_CONTRACT, election).
-define(STAKING_CONTRACT, staking).
-define(REWARDS_CONTRACT, rewards).

-type height() :: non_neg_integer() | {aetx_env:env(), aec_trees:trees()}.
-type epoch_info() :: map().

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

-spec epoch_info() -> {ok, epoch_info()} | {error, chain_too_short}.
epoch_info() ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height -> epoch_info(Height)
    end.

-spec epoch(height()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch(Height) ->
    call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "epoch", []).

-spec epoch_length(height()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_length(Height) ->
    call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "epoch_length", []).

-spec epoch_info(height()) -> {ok, epoch_info()}.
epoch_info(Height) ->
    {ok, {tuple, {Epoch, EpochInfo}}} = call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "epoch_info", []),
    {ok, epoch_info_map(Epoch, EpochInfo)}.

epoch_info_for_epoch(Epoch) ->
    Height = aec_chain:top_height(),
    {ok, EpochInfo} = call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "epoch_info_epoch", [Epoch]),
    {ok, epoch_info_map(Epoch, EpochInfo)}.


-spec epoch_start_height(non_neg_integer()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_start_height(Epoch) ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height ->
           epoch_start_height(Epoch, Height)
    end.

-spec validators_at_height(height()) -> {ok, [binary()]}.
validators_at_height(Height) ->
    {ok, Result} = call_consensus_contract_at_height(?STAKING_CONTRACT, Height, "sorted_validators", []),
    {ok, lists:map(fun({tuple, Staker}) -> Staker end, Result)}.

-spec validator_schedule(non_neg_integer()) -> {ok, #{integer() => binary()}}.
validator_schedule(Epoch) when Epoch > 1 ->
    case epoch_start_height(Epoch) of
      {ok, Height} -> validator_schedule_at_height(Height);
      Err -> Err
    end.

-spec validator_schedule_at_height(height()) -> {ok, #{integer() => binary()}}.
validator_schedule_at_height(Height) ->
    {ok, Result} = call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "get_validator_schedule", []),
    From = case Height of
                {TxEnv, _} -> aetx_env:height(TxEnv);
                _ when is_integer(Height) -> Height
           end,
    {ok, maps:from_list(enumerate(From, lists:map(fun({address, Address}) -> Address end, Result)))}.

-spec entropy_hash(non_neg_integer()) -> {ok, binary()} | {error, any()}.
entropy_hash(Epoch) ->
  aec_consensus_hc:get_entropy_hash(Epoch).

%%% --- internal

epoch_start_height(Epoch, Height) ->
    case epoch_info(Height) of
        {ok, #{epoch := EpochNum, first := StartHeight}} when EpochNum == Epoch ->
            {ok, StartHeight};
        {ok, #{epoch := EpochNum, first := StartHeight}} when EpochNum > Epoch ->
            epoch_start_height(Epoch, StartHeight - 2);
        _ ->
            {error, chain_too_short}
    end.

-if(?OTP_RELEASE < 25).
enumerate(From, List) ->
  lists:zip(lists:seq(From, From + length(List)-1), List).
-else.
enumerate(From, List) ->
    lists:enumerate(From, List).
-endif.

epoch_info_map(Epoch, EpochInfo) ->
    {tuple, {Start, Length, _Seed, _StakingDist}} = EpochInfo,
    #{first => Start, epoch => Epoch, length => Length, last  => Start + Length - 1}.

call_consensus_contract_at_height(Contract, {TxEnv, Trees}, Endpoint, Args) ->
    aec_consensus_hc:call_consensus_contract_result(Contract, TxEnv, Trees, Endpoint, Args);
call_consensus_contract_at_height(Contract, Height, Endpoint, Args) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} ->
           {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, Hash),
           call_consensus_contract_at_height(Contract, {TxEnv, Trees}, Endpoint, Args)
    end.