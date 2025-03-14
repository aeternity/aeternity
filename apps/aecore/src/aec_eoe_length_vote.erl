%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_length_vote).
-behaviour(aec_eoe_gen_vote).

%% Export API functions
-export([start_link/2, negotiate/7, get_finalize_transaction/2, add_parent_block/2]).

%% Export aec_eoe_gen_vote callbacks
-export([init/1, init_state/6, reset_state/1, create_proposal/2, create_vote/3, finalize_call/2, vote_params/1, convert_payload_field/2, update_proposal_after_vote_majority/4]).

%%% aec_eoe_gen_vote callbacks

-define(PROPOSAL_TYPE, 4).
-define(VOTE_TYPE, 5).
-define(COMMIT_TYPE, 6).

-define(EPOCH_DELTA_FLD, <<"epoch_length_delta">>).


-define(FINALIZE_FUN_NAME, "finalize_epoch_length").

-record(data, {
                length                     :: non_neg_integer() | undefined,
                length_delta               :: non_neg_integer() | undefined
        }).

%% API to start the state machine
-spec start_link(#{binary() => binary()}, non_neg_integer())  -> {ok, pid()} | {error, atom()}.
start_link(Stakers, BlockTime) ->
    VoteTypes = #{proposal => ?PROPOSAL_TYPE, vote => ?VOTE_TYPE, commit => ?COMMIT_TYPE},
    aec_eoe_gen_vote:start_link(?MODULE, ?MODULE, VoteTypes, Stakers, BlockTime).

%% Negotiate a fork, called with preferred fork and epoch length delta
-spec negotiate(non_neg_integer(), non_neg_integer(), binary(), aec_keys:pubkey(), [{binary(), non_neg_integer()}], binary(), non_neg_integer()) -> ok.
negotiate(Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength) ->
    aec_eoe_gen_vote:negotiate(?MODULE, Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength).

-spec add_parent_block(non_neg_integer(), aec_parent_chain_block:block()) -> ok.
add_parent_block(Epoch, ParentBlock) ->
    aec_eoe_gen_vote:add_parent_block(?MODULE, Epoch, ParentBlock).

-spec get_finalize_transaction(aec_trees:trees(), non_neg_integer()) -> {ok, aetx_sign:signed_tx()} | {error, not_ready} | {error, term()}.
get_finalize_transaction(Trees, NonceOffset) ->
    aec_eoe_gen_vote:get_finalize_transaction(?MODULE, Trees, NonceOffset).

%%% aec_eoe_gen_vote callbacks

init(_Args) ->
    #data{}.

init_state(Epoch, _Hash, ParentBlocks, CurrentLength, BlockTime, Data) ->
    LengthDelta = calculate_delta(Epoch, ParentBlocks, CurrentLength, BlockTime),
    lager:debug("Suggesting delta ~p for epoch ~p", [LengthDelta, Epoch]),
    Data#data{length=CurrentLength, length_delta=LengthDelta}.

reset_state(_Data) ->
    #data{}.

create_proposal(Proposal0, #data{length_delta=LengthDelta}) ->
    Proposal =#{?EPOCH_DELTA_FLD => LengthDelta},
    maps:merge(Proposal0, Proposal).

create_vote(ProposalFields, VoteFlds0, #data{length_delta=LengthDelta}) ->
    case maps:get(?EPOCH_DELTA_FLD, ProposalFields, undefined) of
        undefined ->
            lager:warning("Epoch length not found in proposal ~p", [ProposalFields]),
            {error, no_proposal};
        LengthDelta ->
            VoteFlds =#{?EPOCH_DELTA_FLD => LengthDelta},
            {ok, maps:merge(VoteFlds0, VoteFlds)};
        ProposalLength ->
            lager:warning("Epoch length hash ~p does not match ~p", [LengthDelta, ProposalLength]),
            {error, hash_mismatch}
    end.

finalize_call(#{?EPOCH_DELTA_FLD := EpochDelta}, #data{length=EpochLength}) ->
    {?FINALIZE_FUN_NAME, [EpochLength + EpochDelta]}.

vote_params(#{?EPOCH_DELTA_FLD := EpochDelta}) ->
    [EpochDelta].

convert_payload_field(?EPOCH_DELTA_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(Key, Value) ->
    aec_eoe_gen_vote:convert_payload_field(Key, Value).

update_proposal_after_vote_majority(Proposal, Votes, Validators, Leader) ->
    SumFun = sum_epoch_delta(Validators),
    Totals = maps:fold(SumFun, {0,0}, Votes),
    {TotalStake, TotalEpochDelta} = SumFun(Leader, Proposal,Totals),
    EpochDelta = round(TotalEpochDelta / TotalStake),
    maps:put(?EPOCH_DELTA_FLD, EpochDelta, Proposal).

sum_epoch_delta(Validators) ->
    fun(Producer, Vote, {TotalStake, TotalEpochDelta}) ->
            {Stake, EpochDelta} = get_weighted_delta(Producer, Vote, Validators),
            {TotalStake + Stake, EpochDelta * Stake + TotalEpochDelta} end.

get_weighted_delta(Producer, #{?EPOCH_DELTA_FLD := EpochDelta}, Validators) ->
    Stake = proplists:get_value(Producer, Validators, 0),
    {Stake, EpochDelta}.


%% The first three epochs have the same seed
calculate_delta(Epoch, _ParentBlocks, _CurrentLength, _BlockTime) when Epoch =< 4 ->
    0;
calculate_delta(Epoch, ParentBlocks, CurrentLength, BlockTime) ->
    ExpectedTimeDiff = CurrentLength * BlockTime,
    TimeDiff = get_epoch_time_diff(Epoch, ParentBlocks, ExpectedTimeDiff),
    case (TimeDiff - ExpectedTimeDiff) / BlockTime of
        NegDiff when NegDiff < 0 ->
            case ceil(NegDiff) of
                NegDiff1 when NegDiff1 =< -CurrentLength ->
                    1 - CurrentLength;
                Rest ->
                    Rest
            end;
        Diff ->
            floor(Diff)
    end.

get_epoch_time_diff(Epoch, ParentBlocks, ExpectedTimeDiff) ->
  get_block_time_diff(maps:get(Epoch, ParentBlocks, undefined),maps:get(Epoch + 1, ParentBlocks, undefined), ExpectedTimeDiff).

get_block_time_diff(ParentBlock1, ParentBlock2, ExpectedTimeDiff) when ParentBlock1 == undefined ; ParentBlock2 == undefined ->
    ExpectedTimeDiff;
get_block_time_diff(ParentBlock1, ParentBlock2, _) ->
    aec_parent_chain_block:time(ParentBlock2) - aec_parent_chain_block:time(ParentBlock1).

