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
        , validators_at_height/1
        , validator_schedule_at_height/2
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
                             last => non_neg_integer()}} | {error, chain_too_short}.
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
                                              epoch => non_neg_integer(),
                                              last => non_neg_integer()}}.
epoch_info(Height) ->
    {ok, {tuple, {Epoch, EpochInfo}}} = call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "epoch_info", []),
    {tuple, {Start, Length, _, _}} = EpochInfo,
    {ok, #{first => Start,
           epoch => Epoch,
           last  => Start + Length - 1}}.

-spec epoch_start_height(non_neg_integer()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_start_height(Epoch) ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height ->
           epoch_start_height(Epoch, Height)
    end.

epoch_start_height(Epoch, Height) ->
    case epoch_info(Height) of
        {ok, #{epoch := EpochNum, first := StartHeight}} when EpochNum == Epoch ->
            {ok, StartHeight};
        {ok, #{epoch := EpochNum, first := StartHeight}} when EpochNum > Epoch ->
            epoch_start_height(Epoch, StartHeight - 1);
        _ ->
            {error, chain_too_short}
    end.

validators_at_height(Height) ->
    {ok, Result} = call_consensus_contract_at_height(?STAKING_CONTRACT, Height, "sorted_validators", []),
    {ok, lists:map(fun({tuple, Staker}) -> Staker end, Result)}.

validator_schedule_at_height(ParentEntropyBlockHash, Height) ->
  case validators_at_height(Height) of
      {ok, Validators} ->
           {ok, EpochLength} = epoch_length(Height),
           Args = [aefa_fate_code:encode_arg({bytes, ParentEntropyBlockHash}),
                   aefa_fate_code:encode_arg(Validators),
                   aefa_fate_code:encode_arg({integer, EpochLength})
                  ],
           {ok, Result} = call_consensus_contract_at_height(?ELECTION_CONTRACT, Height, "validator_schedule", Args),
           {ok, maps:from_list(enumerate(Height, lists:map(fun({address, Address}) -> Address end, Result)))};
      Err -> Err
  end.

%%% --- internal

-if(?OTP_RELEASE < 25).
enumerate(From, List) ->
  lists:zip(lists:seq(From, From + length(List)-1), List).
-else.
enumerate(From, List) ->
    lists:enumerate(From, List).
-endif.

call_consensus_contract_at_height(Contract, {TxEnv, Trees}, Endpoint, Args) ->
    aec_consensus_hc:call_consensus_contract_result(Contract, TxEnv, Trees, Endpoint, Args);

call_consensus_contract_at_height(Contract, Height, Endpoint, Args) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {error, chain_too_short};
        {ok, Hash} ->
           {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, Hash),
           call_consensus_contract_at_height(Contract, {TxEnv, Trees}, Endpoint, Args)
    end.

