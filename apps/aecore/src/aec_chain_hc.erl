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
        , pin_info/0
        , pin_info/1
        , pin_reward_info/0
        , pin_reward_info/1
        %% epoch determined
        , epoch_start_height/1
        , epoch_info_for_epoch/1
        , epoch_info_for_epoch/2
        , validator_schedule/4
        , entropy_hash/1
        , get_micro_blocks_between/2
        ]).

-export_type([run_env/0]).

-define(ELECTION_CONTRACT, election).
-define(STAKING_CONTRACT, staking).
-define(REWARDS_CONTRACT, rewards).

-type epoch() :: non_neg_integer().
-type height() :: non_neg_integer().
-type run_env() :: top | height() | {aetx_env:env(), aec_trees:trees()} | {hash, binary()}.
-type epoch_info() :: #{
    %% Mandatory
    epoch := pos_integer(),
    first := non_neg_integer(),
    length := non_neg_integer(),
    last := non_neg_integer(),
    %% Optional
    validators => list(binary()),
    seed => binary() | undefined
}.
-type pin_reward_info() :: map().

-spec epoch() -> {ok, epoch()} | {error, chain_too_short}.
epoch() ->
    epoch(top).

-spec epoch_length() -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_length() ->
    epoch_length(top).

-spec epoch_info() -> {ok, epoch_info()} | {error, chain_too_short}.
epoch_info() ->
    epoch_info(top).

-spec epoch(run_env()) -> {ok, epoch()} | {error, chain_too_short}.
epoch(RunEnv) ->
    call_consensus_contract_w_env(?ELECTION_CONTRACT, RunEnv, "epoch", []).

-spec epoch_length(run_env()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_length(RunEnv) ->
    call_consensus_contract_w_env(?ELECTION_CONTRACT, RunEnv, "epoch_length", []).

-spec epoch_info(run_env()) -> {ok, epoch_info()} | {error, chain_too_short}.
epoch_info(RunEnv) ->
    {ok, {tuple, {Epoch, EpochInfo}}} = call_consensus_contract_w_env(?ELECTION_CONTRACT, RunEnv, "epoch_info", []),
    {ok, epoch_info_map(Epoch, EpochInfo)}.

-spec epoch_info_for_epoch(non_neg_integer()) -> {ok, epoch_info()} | {error, chain_too_short}.
epoch_info_for_epoch(Epoch) ->
    epoch_info_for_epoch(top, Epoch).

-spec epoch_info_for_epoch(run_env(), non_neg_integer()) -> {ok, epoch_info()} | {error, chain_too_short}.
epoch_info_for_epoch(RunEnv, Epoch) ->
    {ok, EpochInfo} = call_consensus_contract_w_env(?ELECTION_CONTRACT, RunEnv, "epoch_info_epoch", [Epoch]),
    {ok, epoch_info_map(Epoch, EpochInfo)}.

-spec epoch_start_height(non_neg_integer()) -> {ok, non_neg_integer()} | {error, chain_too_short}.
epoch_start_height(Epoch) ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height ->
           epoch_start_height(Epoch, Height)
    end.

-spec validators_at_height(run_env()) -> {ok, [binary()]}.
validators_at_height(RunEnv) ->
    {ok, Result} = call_consensus_contract_w_env(?STAKING_CONTRACT, RunEnv, "sorted_validators", []),
    {ok, lists:map(fun({tuple, Staker}) -> Staker end, Result)}.

-spec validator_schedule(run_env(), binary(), [{binary(), non_neg_integer()}], non_neg_integer()) ->
          {ok, [binary()]} | {error, chain_too_short}.
validator_schedule(RunEnv, Seed, Validators, Length) ->
    Args = [{bytes, Seed}, encode_stakers(Validators), Length],
    {ok, Result} = call_consensus_contract_w_env(?ELECTION_CONTRACT, RunEnv, "validator_schedule", Args),
    {ok, lists:map(fun({address, Address}) -> Address end, Result)}.


pin_info() ->
    pin_info(top).

pin_info(RunEnv) ->
    {ok, Result} = call_consensus_contract_w_env(?ELECTION_CONTRACT, RunEnv, "pin_info", []),
    decode_option(Result, {fun(X) -> X end, undefined}).

-spec pin_reward_info() -> pin_reward_info().
pin_reward_info() ->
    pin_reward_info(top).


-spec pin_reward_info(run_env()) -> pin_reward_info().
pin_reward_info(RunEnv) ->
    {ok, Res} = call_consensus_contract_w_env(?ELECTION_CONTRACT, RunEnv, "pin_reward_info", []),
    {tuple, {Base, Current, CarryOver}} = Res,
    #{ base_pin_reward => Base, cur_pin_reward => Current, carry_over_pin_reward => CarryOver }.


%% This makes the dependency graph a circle, right?
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

-spec epoch_info_map(Epoch :: pos_integer(), EpochInfo::any()) -> epoch_info().
epoch_info_map(Epoch, EpochInfo) ->
    {tuple, {Start, Length, Seed, StakingDist}} = EpochInfo,
    SeedHash   = decode_option(Seed, {fun({bytes, Bin}) -> Bin end, undefined}),
    Validators = decode_option(StakingDist, {fun decode_stakers/1, undefined}),
    #{first => Start, epoch => Epoch, length => Length, last => Start + Length - 1,
      validators => Validators, seed => SeedHash}.

decode_option({variant, [0, 1], 0, {}}, {_SomeFun, NoneValue}) -> NoneValue;
decode_option({variant, [0, 1], 1, {SomeValue}}, {SomeFun, _NoneValue}) -> SomeFun(SomeValue).

decode_stakers(RawStakers) ->
    lists:map(fun({tuple, {{address, Staker}, Stake}}) -> {Staker, Stake} end, RawStakers).

encode_stakers(Stakers) ->
    lists:map(fun({Staker, Stake}) -> {tuple, {{address, Staker}, Stake}} end, Stakers).

call_consensus_contract_w_env(Contract, top, Endpoint, Args) ->
    case aec_chain:top_height() of
        undefined -> {error, chain_too_short};
        Height    -> call_consensus_contract_w_env(Contract, Height, Endpoint, Args)
    end;
call_consensus_contract_w_env(Contract, {hash, Hash}, Endpoint, Args) when is_binary(Hash) ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, Hash),
    call_consensus_contract_w_env(Contract, {TxEnv, Trees}, Endpoint, Args);
call_consensus_contract_w_env(Contract, Height, Endpoint, Args) when is_integer(Height), Height >= 0 ->
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error      -> {error, chain_too_short};
        {ok, Hash} -> call_consensus_contract_w_env(Contract, {hash, Hash}, Endpoint, Args)
    end;
call_consensus_contract_w_env(Contract, {TxEnv, Trees}, Endpoint, Args) ->
    aec_consensus_hc:call_consensus_contract_result(Contract, TxEnv, Trees, Endpoint, Args).

get_micro_blocks_between(From, To) ->
    {ok, KeyHdr} = aec_chain:get_key_header_by_height(To),
    get_micro_blocks_between(From, KeyHdr, []).

get_micro_blocks_between(Stop, KeyHdr, MBs) ->
    case Stop > aec_headers:height(KeyHdr) of
        true -> MBs;
        false ->
            {ok, PrevKeyHdr} = aec_chain:get_header(aec_headers:prev_key_hash(KeyHdr)),
            case aec_headers:prev_hash(KeyHdr) == aec_headers:prev_key_hash(KeyHdr) of
                true -> %% No Mb
                    get_micro_blocks_between(Stop, PrevKeyHdr, MBs);
                false ->
                    {ok, MB} = aec_chain:get_block(aec_headers:prev_hash(KeyHdr)),
                    get_micro_blocks_between(Stop, PrevKeyHdr, [MB | MBs])
            end
    end.

