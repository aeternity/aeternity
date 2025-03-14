%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_fork_vote).
-behaviour(aec_eoe_gen_vote).

%% Export API functions
-export([start_link/2, negotiate/7, get_finalize_transaction/2, add_parent_block/2]).

%% Export aec_eoe_gen_vote callbacks
-export([init/1, init_state/6, reset_state/1, create_proposal/2, create_vote/3, finalize_call/2, vote_params/1, convert_payload_field/2, update_proposal_after_vote_majority/4]).

%%% aec_eoe_gen_vote callbacks

-define(PROPOSAL_TYPE, 1).
-define(VOTE_TYPE, 2).
-define(COMMIT_TYPE, 3).

-define(HASH_FLD, <<"block_hash">>).
-define(EPOCH_DELTA_FLD, <<"epoch_length_delta">>).


-define(FINALIZE_FUN_NAME, "finalize_epoch").

-record(data, {
                fork_hash                  :: binary() | undefined
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

init_state(Epoch, Hash, _ParentBlocks, _CurrentLength, _BlockTime, Data) ->
    lager:debug("Suggesting hash ~p for epoch ~p", [Hash, Epoch]),
    Data#data{fork_hash=Hash}.

reset_state(_Data) ->
    #data{}.

create_proposal(Proposal0, #data{fork_hash=Hash}) ->
    Proposal =#{?HASH_FLD => Hash},
    maps:merge(Proposal0, Proposal).

create_vote(ProposalFields, VoteFlds0, #data{fork_hash=Hash}) ->
    case maps:get(?HASH_FLD, ProposalFields, undefined) of
        undefined ->
            lager:warning("Hash field not found in proposal ~p", [ProposalFields]),
            {error, no_proposal};
        Hash ->
            VoteFlds =#{?HASH_FLD => Hash},
            {ok, maps:merge(VoteFlds0, VoteFlds)};
        ProposalHash ->
            lager:warning("Proposal hash ~p does not match hash ~p", [ProposalHash, Hash]),
            {error, hash_mismatch}
    end.

finalize_call(#{?HASH_FLD := Hash}, _Data) ->
    {?FINALIZE_FUN_NAME, [{bytes, Hash}]}.

vote_params(#{?HASH_FLD := Hash}) ->
    [{bytes, Hash}].

convert_payload_field(?EPOCH_DELTA_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(Key, Value) ->
    aec_eoe_gen_vote:convert_payload_field(Key, Value).

update_proposal_after_vote_majority(Proposal, _Votes, _Validators, _Leader) ->
    Proposal.
