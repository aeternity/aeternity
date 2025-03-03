%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_gen_vote).
-behaviour(gen_statem).

%% Export API functions
-export([start_link/3, negotiate/8, get_finalize_transaction/2, add_parent_block/3]).

%% Export gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% FSM states (as per gen_statem callback callback_mode/0)
-export([await_eoe/3, proposal/3, vote/3, finalize/3, complete/3]).


%% ==================================================================
%% Records and Types

-record(data, {
                epoch                      :: non_neg_integer() | undefined,
                height                     :: non_neg_integer() | undefined,
                fork_hash                  :: binary() | undefined,
                seed                       :: binary() | undefined,
                length                     :: non_neg_integer() | undefined,
                length_delta               :: non_neg_integer() | undefined,
                leader                     :: binary() | undefined,
                validators=[]              :: [{binary(), non_neg_integer()}],
                remaining_validators=#{}   :: #{binary() => non_neg_integer()},
                stakers=#{}                :: #{binary() => binary()},
                majority=0                 :: non_neg_integer(),
                proposal                   :: #{binary() => any()} | undefined,
                block_time                 :: non_neg_integer(),
                result                     :: {ok, binary()} | {error, no_consensus} | undefined,
                from                       :: pid() | undefined,
                votes=#{}                  :: #{binary() => #{binary() => any()}},
                parent_blocks=#{}          :: #{non_neg_integer() => aec_parent_chain_block:block()},
                other_votes=[]             :: list({non_neg_integer(), aetx_sign:signed_tx()}),
                vote_types                 :: vote_types()
            }).


-define(PROPOSAL_TYPE, 1).
-define(VOTE_TYPE, 2).
-define(COMMIT_TYPE, 3).

-define(HASH_FLD, <<"block_hash">>).
-define(HEIGHT_FLD, <<"block_height">>).
-define(EPOCH_DELTA_FLD, <<"epoch_length_delta">>).
-define(SIGNATURE_FLD, <<"signature">>).
-define(LEADER_FLD, <<"leader">>).
-define(EPOCH_FLD, <<"epoch">>).

-define(FINALIZE_FUN_NAME, "finalize_epoch").

-type vote_types() :: #{
    proposal => non_neg_integer(),
    vote => non_neg_integer(),
    commit => non_neg_integer()
}.

%% API to start the state machine
-spec start_link(atom(), #{binary() => binary()}, non_neg_integer())  -> {ok, pid()} | {error, atom()}.
start_link(EOEVoteType, Stakers, BlockTime) ->
    gen_statem:start_link({local, EOEVoteType}, ?MODULE, [Stakers, BlockTime], []).

%% Negotiate a fork, called with preferred fork and epoch length delta
-spec negotiate(atom(), non_neg_integer(), non_neg_integer(), binary(), aec_keys:pubkey(), [{binary(), non_neg_integer()}], binary(), non_neg_integer()) -> ok.
negotiate(EOEVoteType, Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength) ->
    gen_statem:cast(EOEVoteType, {negotiate, Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength}).

-spec add_parent_block(atom(), non_neg_integer(), aec_parent_chain_block:block()) -> ok.
add_parent_block(EOEVoteType, Epoch, ParentBlock) ->
    gen_statem:cast(EOEVoteType, {add_parent_block, Epoch, ParentBlock}).

-spec get_finalize_transaction(atom(), aec_trees:trees()) -> {ok, aetx_sign:signed_tx()} | {error, not_ready} | {error, term()}.
get_finalize_transaction(EOEVoteType, Trees) ->
    gen_statem:call(EOEVoteType, {get_finalize_transaction, Trees}).

%%% gen_statem callbacks

%% Initialization: Start in the await_eoe state
init([Stakers, BlockTime]) ->
    aec_events:subscribe(tx_received),
    aec_events:subscribe(new_epoch),
    VoteTypes = #{proposal => ?PROPOSAL_TYPE, vote => ?VOTE_TYPE, commit => ?COMMIT_TYPE},
    {ok, await_eoe, #data{stakers=Stakers,block_time=BlockTime,vote_types=VoteTypes}}.

%% Set the callback mode to state functions
callback_mode() ->
    state_functions.

%%% State: AwaitEndOfEpoch
await_eoe(cast, {negotiate, Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength}, #data{block_time=BlockTime, proposal=Proposal, parent_blocks = ParentBlocks} = Data) ->
    LengthDelta = calculate_delta(Epoch, ParentBlocks, CurrentLength, BlockTime),
    lager:debug("Suggesting delta ~p for epoch ~p", [LengthDelta, Epoch]),
    Data1 = set_validators(Validators, Data#data{epoch=Epoch, height=Height, fork_hash=Hash, seed=Seed, length=CurrentLength, leader=Leader, validators = Validators, length_delta=LengthDelta}),
    case is_leader(Data1) of
        false ->
            case Proposal of
                undefined ->
                    lager:info("End of epoch ~p waiting for proposal", [Epoch]),
                    {next_state, proposal, Data1,
                        [{state_timeout,BlockTime,no_proposal}]};
                #{?LEADER_FLD := Leader, ?EPOCH_FLD := Epoch} ->
                    Data2 = handle_voting(Data1),
                    {next_state, vote, Data2,
                        [{state_timeout,BlockTime,no_quorum}]};
                _ ->
                    lager:info("Discarding invalid proposal ~p, end of epoch ~p waiting for proposal", [Proposal, Epoch]),
                    {next_state, proposal, Data1#data{proposal = undefined},
                        [{state_timeout,BlockTime,no_proposal}]}

            end;
        true ->
            lager:info("Sending proposal for end of epoch ~p hash: ~p", [Epoch, Hash]),
            Data2 = Data1#data{proposal = #{?HASH_FLD => Hash, ?EPOCH_DELTA_FLD => LengthDelta}},
            %% block time * 2 because need to wait for votes to arrive
            Next = {next_state, proposal, Data2, [{state_timeout, BlockTime * 2, no_quorum}]},
            send(fun create_proposal/1, Data1, Next)
    end;
await_eoe(info, {gproc_ps_event, tx_received, #{info := SignedTx}}, Data) ->
    handle_proposal(SignedTx, Data);
await_eoe(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

%%% State: Proposal
proposal(info, {gproc_ps_event, tx_received, #{info := SignedTx}}, Data) ->
    %% Handle the proposal phase
    %% Check the transaction contains a proposal
    handle_proposal(SignedTx, Data);
proposal(state_timeout, no_proposal, #data{epoch = Epoch} = Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Proposal timeout for epoch ~p", [Epoch]),
    %% Reply with no consensus
    handle_no_consensus(Data);
proposal(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

%%% State: Vote
vote(info, {gproc_ps_event, tx_received, #{info := Tx}}, #data{vote_types=#{vote := VoteType}} = Data) ->
    %% Handle the voting phase
    %% Check the transaction contains a vote
    %% If more than two thirds of votes agree send a commit then transition to finalization phase
    handle_vote(VoteType, Tx, Data, fun on_valid_vote/3, fun on_other_vote/3);
vote(state_timeout, no_quorum, #data{epoch = Epoch} = Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Voting timeout for epoch ~p", [Epoch]),
    %% Reply with no consensus, if new leader use preferred fork
    handle_no_consensus(Data);
vote(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).


%%% State: Finalize
finalize(info, {gproc_ps_event, tx_received, #{info := Tx}}, #data{vote_types=#{commit := CommitType}} = Data) ->
    handle_vote(CommitType, Tx, Data, fun on_valid_commit/3, fun on_other_vote_type/3);
finalize(state_timeout, no_quorum, #data{epoch = Epoch} = Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Finalize timeout for epoch ~p", [Epoch]),
    handle_no_consensus(Data);
finalize(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

complete(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

create_proposal(#data{leader=Leader, stakers=Stakers, epoch=Epoch, height=Height, fork_hash=Hash, length_delta=LengthDelta, vote_types=#{proposal := ProposalType}}) ->
    PrivKey = get_staker_private_key(Leader, Stakers),
    Proposal =#{?HASH_FLD => Hash,
                ?EPOCH_DELTA_FLD => LengthDelta,
                ?HEIGHT_FLD => Height},
    ProposalPayload = create_payload(Proposal, PrivKey),
    create_vote_transaction(Leader, PrivKey, Epoch, ProposalType, ProposalPayload).

create_votes(Type, #data{proposal=ProposalFields, leader=Leader, stakers=Stakers, validators=Validators, epoch=Epoch, height=Height, fork_hash=Hash, length_delta=LengthDelta}) ->
    case maps:get(?HASH_FLD, ProposalFields, undefined) of
        undefined ->
            lager:warning("Hash field not found in proposal ~p", [ProposalFields]),
            [];
        Hash ->
            VoteFlds =#{?HASH_FLD => Hash,
                        ?EPOCH_DELTA_FLD => LengthDelta,
                        ?HEIGHT_FLD => Height},
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, []);
        ProposalHash ->
            lager:warning("Proposal hash ~p does not match hash ~p", [ProposalHash, Hash]),
            []
    end.


create_votes(_VoteFlds, _Leader, [], _Stakers, _Epoch, Type, Votes) ->
    lager:debug("Created ~p votes of type ~p", [length(Votes), Type]),
    Votes;
create_votes(VoteFlds, Leader, [{Leader,_}|Validators], Stakers, Epoch, Type, Votes) ->
    create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, Votes);
create_votes(VoteFlds, Leader, [{Validator,_}|Validators], Stakers, Epoch, Type, Votes) ->
    case get_staker_private_key(Validator, Stakers) of
        undefined ->
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, Votes);
        PrivKey ->
            VotePayload = create_payload(VoteFlds, PrivKey),
            Vote = create_vote_transaction(Validator, PrivKey, Epoch, Type, VotePayload),
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, [Vote|Votes])
    end.

handle_proposal(SignedTx, #data{leader=undefined, epoch=undefined, vote_types=#{proposal := ProposalType} = VoteTypes} = Data) ->
    case convert_transaction(SignedTx, VoteTypes) of
        {ok, ProposalType, Epoch, Leader, ProposalFields} ->
            %% Store the leader so it can be used for validation laster
            ProposalFields1 = maps:put(?LEADER_FLD, Leader, ProposalFields),
            %% The epoch is also need for validation
            ProposalFields2 = maps:put(?EPOCH_FLD, Epoch, ProposalFields1),
            {keep_state, Data#data{proposal=ProposalFields2}};
        _ ->
            keep_state_and_data
    end;
handle_proposal(SignedTx, #data{leader=Leader, epoch=Epoch, vote_types=#{proposal := ProposalType} = VoteTypes} = Data) ->
    %% Handle the proposal phase
    %% Check the transaction contains a proposal
    case convert_transaction(SignedTx, VoteTypes) of
        {ok, ProposalType, Epoch, Leader, ProposalFields} ->
            Data1 = Data#data{proposal=ProposalFields},
            Data2 = handle_voting(Data1),
            check_voting_majority(Data2);
        {ok, ProposalType, Epoch, InvalidProducer, _ProposalFields} ->
            lager:warning("Received proposal from invalid leader ~p", [InvalidProducer]),
            keep_state_and_data;
        {ok, ProposalType, InvalidEpoch, Leader, _ProposalFields} ->
            lager:warning("Received proposal from invalid epoch ~p", [InvalidEpoch]),
            keep_state_and_data;
        {ok, ProposalType, InvalidEpoch, InvalidProducer, _ProposalFields} ->
            lager:warning("Received proposal from invalid epoch ~p and invalid leader ~p", [InvalidEpoch, InvalidProducer]),
            keep_state_and_data;
        {ok, _Type, _, _, _} ->
            keep_state_and_data;
        {error, not_vote} ->
            keep_state_and_data;
        {error, Reason} ->
            lager:warning("Could not process proposal ~p for reason ~p", [SignedTx, Reason]),
            keep_state_and_data
    end.

handle_vote(Type, SignedTx, #data{vote_types=VoteTypes} = Data, OnValidFun, OnOtherVoteType) ->
    case convert_transaction(SignedTx, VoteTypes) of
        {ok, Type, _Epoch, Validator, VoteFields} ->
            %% Check if a validator
            case can_vote(Validator, Data) of
                true ->
                    lager:info("Received a vote from ~p of type ~p (vote: ~p)", [Validator, Type, VoteFields]),
                    Data1 = count_vote(Validator, Data),
                    %% Check if reached two thirds
                    OnValidFun(Validator, VoteFields, Data1);
                {error, not_a_validator} ->
                    lager:warning("Received a vote from a non validator ~p", [Validator]),
                    keep_state_and_data;
                {error, already_voted} ->
                    lager:warning("Received a vote from a validator ~p that has already voted", [Validator]),
                    keep_state_and_data
            end;
        {ok, OtherType, _, _, _} ->
            OnOtherVoteType(OtherType, SignedTx, Data);
        {error, not_vote} ->
            keep_state_and_data;
        {error, Reason} ->
            lager:warning("Could not process vote ~p for reason ~p", [SignedTx, Reason]),
            keep_state_and_data
    end.

create_payload(Payload0, PrivKey) ->
    Payload = maps:map(fun(_Key, Value) -> fld(Value) end, Payload0),
    SignData = get_sign_data(Payload),
    Signature = enacl:sign_detached(SignData, PrivKey),
    maps:put(?SIGNATURE_FLD, Signature, Payload).

fld(Value) when is_integer(Value) ->
    integer_to_binary(Value);
fld(Value) when is_binary(Value) ->
    Value.

create_vote_transaction(Pubkey, PrivKey, Epoch, Type, VotePayload) ->
    Account = aeser_id:create(account, Pubkey),
    VoteM = #{  voter_id     => Account
              , epoch        => Epoch
              , type         => Type
              , data         => VotePayload},
    {ok, VoteTx} = aec_hc_vote_tx:new(VoteM),
    Bin0 = aetx:serialize_to_binary(VoteTx),
    Bin = aec_hash:hash(signed_tx, Bin0),
    BinForNetwork = aec_governance:add_network_id(Bin),
    Signatures = [enacl:sign_detached(BinForNetwork, PrivKey)],
    aetx_sign:new(VoteTx, Signatures).

on_valid_vote(Validator, VoteFields, #data{votes=Votes} = Data) ->
    Votes1 = maps:put(Validator, VoteFields, Votes),
    check_voting_majority(Data#data{votes = Votes1}).

check_voting_majority(#data{majority = CurrentMajority, validators=Validators, block_time=BlockTime, votes=Votes, epoch=Epoch} = Data) ->
    case CurrentMajority =< 0 of
        true ->
            lager:info("Quorum achieved for voting for epoch ~p has ~p votes", [Epoch, maps:size(Votes)]),
            Majority = calculate_majority(Validators),
            #data{proposal=Proposal} = Data1 = update_proposal_after_vote_majority(Data),
            Result = create_finalize_call(Votes, Proposal, Data1),
            Data2 = Data1#data{result = Result},
            send_commits(Data2),
            Stake = calculate_leader_stake(Data2),
            lager:debug("Leader implicitly voting with stake ~p, remaining = ~p", [Stake, Majority - Stake]),
            Data3 = Data2#data{majority = Majority - Stake, remaining_validators = maps:from_list(Validators)},
            check_other_votes({next_state, finalize, Data3, [{state_timeout,BlockTime,no_quorum}]});
        false ->
            {next_state, vote, Data, [{state_timeout,BlockTime,no_quorum}]}
    end.

check_other_votes({next_state, _, #data{other_votes = []}, _} = NextEvent) ->
    NextEvent;
check_other_votes({next_state, NextState, #data{other_votes = [{_Type, SignedTx}|OtherVotes],vote_types=#{commit := CommitType}} = Data, Actions}) ->
    Data1 = Data#data{other_votes = OtherVotes},
    case handle_vote(CommitType, SignedTx, Data1, fun on_valid_commit/3, fun on_other_vote_type/3) of
        Result when element(1, Result) == next_state ->
            Result;
        keep_state_and_data ->
            check_other_votes({next_state, NextState, Data1, Actions});
        Result when element(1, Result) == keep_state ->
            check_other_votes({next_state, NextState, element(2, Result), Actions})
    end.

on_valid_commit(_Validator, _CommitFields, #data{result = Result, majority = CurrentMajority, block_time=BlockTime, from=From, epoch = Epoch} = Data) ->
    case CurrentMajority =< 0 of
        true ->
            lager:info("Quorum achieved for commit for epoch ~p", [Epoch]),
            Actions = case From of
                        undefined ->
                            [];
                        _ ->
                            lager:debug("Replying quorum achieved for commits, result ~p", [Result]),
                            [{reply, From, convert_to_finalize_transaction(Result, Data)}]
                      end,
            {next_state, complete, Data, Actions};
        false ->
            {keep_state, Data, [{state_timeout,BlockTime,no_quorum}]}
    end.

on_other_vote(CommitType, SignedTx, #data{other_votes=OtherVotes, vote_types=#{commit := CommitType}} = Data) ->
    {keep_state, Data#data{other_votes = [{CommitType, SignedTx}|OtherVotes]}};
on_other_vote(_Type, _SignedTx, _Data) ->
    keep_state_and_data.

on_other_vote_type(Type, _SignedTx, _Data) ->
    lager:warning("Other vote type ~p", [Type]),
    keep_state_and_data.

handle_voting(#data{majority = Majority} = Data) ->
    Stake = calculate_leader_stake(Data),
    send_votes(Data),
    lager:debug("Handle voting - leader implicilty votes with stake: ~p remaining: ~p", [Stake, Majority - Stake]),
    Data#data{majority = Majority - Stake}.

send_votes(#data{epoch = Epoch, vote_types = #{vote := VoteType}} = Data) ->
    lager:info("Sending votes for epoch ~p", [Epoch]),
    lists:foreach(fun aec_hc_vote_pool:push/1, create_votes(VoteType, Data)).

send_commits(#data{epoch = Epoch, vote_types = #{commit := CommitType}} = Data) ->
    lager:info("Sending commits for epoch ~p", [Epoch]),
    lists:foreach(fun aec_hc_vote_pool:push/1, create_votes(CommitType, Data)).


send(CreateTxFun, Data, Success) ->
    SignedTx = CreateTxFun(Data),
    case aec_hc_vote_pool:push(SignedTx) of
        ok ->
            Success;
        {error, Reason} ->
            lager:debug("~p tx failed: ~p", [SignedTx, Reason]),
            handle_no_consensus(Data)
    end.

convert_transaction(SignedTx, #{proposal := ProposalType, vote := VoteType, commit := CommitType}) ->
    Tx = aetx_sign:tx(SignedTx),
    case aetx:specialize_type(Tx) of
        {hc_vote_tx, VoteTx} ->
            case aec_hc_vote_tx:type(VoteTx) of
                Type when Type == ProposalType; Type == VoteType; Type == CommitType ->
                    Payload = aec_hc_vote_tx:data(VoteTx),
                    PubKey = aec_hc_vote_tx:voter_pubkey(VoteTx),
                    case check_signature(PubKey, Payload) of
                        ok ->
                            case convert_payload_fields(Payload) of
                                {ok, FieldMap} ->
                                    {ok, Type, aec_hc_vote_tx:epoch(VoteTx), PubKey, FieldMap};
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end;
                _ ->
                    {error, not_vote}
            end;
        _ ->
            {error, not_vote}
    end.

convert_payload_fields(Fields) ->
    try
        {ok, maps:map(fun convert_payload_field/2, Fields)}
    catch error:Reason:StackTrace ->
        lager:warning("Error decoding payload: ~p ~p", [Reason, StackTrace]),
        {error, Reason}
    end.

convert_payload_field(?HEIGHT_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(?EPOCH_DELTA_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(_Key, Value) ->
    Value.

check_signature(PubKey, Payload) ->
    case get_signature_from_payload(Payload) of
        {ok, Signature} ->
            SignData = get_sign_data(Payload),
            case enacl:sign_verify_detached(Signature, SignData, PubKey) of
                true  -> ok;
                false ->
                    {error, signature_verification_failed}
            end;
        _ ->
            {error, signature_verification_failed}
    end.

get_sign_data(Payload) ->
    Fields = lists:sort(maps:to_list(maps:remove(?SIGNATURE_FLD, Payload))),
    << <<Key/binary, (fld(Value))/binary>> || {Key, Value} <- Fields >>.

get_signature_from_payload(Payload) ->
    case maps:get(?SIGNATURE_FLD, Payload, undefined) of
        undefined ->
            {error, signature_not_found};
        Signature ->
            {ok, Signature}
    end.

%%% Termination and Code Change Handlers
terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, _State, Data, _Extra) ->
    {ok, Data}.

%%% Helper function
is_leader(#data{leader=Leader, stakers=Stakers}) ->
    maps:is_key(Leader, Stakers).

can_vote(Validator, #data{remaining_validators = RemainingValidators, validators = Validators}) ->
    case maps:is_key(Validator, RemainingValidators) of
        true ->
            true;
        false ->
            case lists:keyfind(Validator, 1, Validators) of
                false ->
                    {error, not_a_validator};
                _ ->
                    {error, already_voted}
            end
    end.

count_vote(Validator, #data{remaining_validators = RemainingValidators, majority = Majority} = Data) ->
    case maps:get(Validator, RemainingValidators, undefined) of
        undefined ->
            Data;
        Stake ->
            lager:debug("Voter had ~p stake, remaining: ~p", [Stake, Majority - Stake]),
            Data#data{majority = Majority - Stake, remaining_validators = maps:remove(Validator, RemainingValidators)}
    end.

handle_no_consensus(#data{from = From} = Data) ->
    Result = {error, no_consensus},
    Actions = case From of
                    undefined ->
                        [];
                    _ ->
                        [{reply, From, Result}]
                  end,
    {next_state, complete, Data#data{result = Result}, Actions}.

handle_common_event(cast, {add_parent_block, Epoch, ParentBlock}, #data{parent_blocks = ParentBlocks} = Data) ->
    ParentBlocks1 = maps:put(Epoch, ParentBlock, ParentBlocks),
    {keep_state, Data#data{parent_blocks = ParentBlocks1}};
handle_common_event(info, {gproc_ps_event, new_epoch, #{info := _EpochInfo}}, Data) ->
    {next_state, await_eoe, reset_data(Data)};
handle_common_event({call,From}, {get_finalize_transaction, Trees}, #data{result=Result, leader = Leader} = Data) ->
    case Leader of
        undefined ->
            {keep_state_and_data, [{reply, From, {error, not_ready}}]};
        _ ->
            case Result of
                undefined ->
                    {keep_state, Data#data{from=From}};
                _ ->
                    lager:debug("Replying quorum achieved for commits, result ~p", [Result]),
                    {keep_state, Data, [{reply, From, convert_to_finalize_transaction(Result, Trees, Data)}]}
            end
    end;
handle_common_event(_E, _Msg, #data{}) ->
    keep_state_and_data.

convert_to_finalize_transaction(Result, Data) ->
    case aec_chain:get_top_state() of
        {ok, Trees} ->
            convert_to_finalize_transaction(Result, Trees, Data);
        Error ->
            {error, Error}
    end.

convert_to_finalize_transaction({ok, CallData}, Trees, #data{leader=Leader, stakers=Stakers, epoch=Epoch}) ->
    case aec_consensus_hc:get_entropy_hash(Epoch + 2) of
        %% Don't create the transaction until the seed is available.
        {ok, _Seed} ->
            {ok, Tx} = aec_chain_hc:create_consensus_call_contract_transaction(Leader, Trees, aeser_api_encoder:encode(contract_bytearray, CallData), 0),
            Bin0 = aetx:serialize_to_binary(Tx),
            Bin = aec_hash:hash(signed_tx, Bin0),
            BinForNetwork = aec_governance:add_network_id(Bin),
            PrivKey = get_staker_private_key(Leader, Stakers),
            Signatures = [enacl:sign_detached(BinForNetwork, PrivKey)],
            STx = aetx_sign:new(Tx, Signatures),
            {ok, STx};
        Error ->
            Error
    end;
convert_to_finalize_transaction(Result, _Trees, _Data) ->
    Result.

calculate_leader_stake(#data{validators = Validators, leader = Leader}) ->
    case lists:keyfind(Leader, 1, Validators) of
        false -> 0;
        {_, Stake} -> Stake
    end.

calculate_majority(Validators) ->
    TotalStake = lists:foldl(fun({_, Stake}, Accum) -> Stake + Accum end, 0, Validators),
    %% 2/3 majority
    (2 * TotalStake + 2) div 3.

reset_data(#data{stakers = Stakers, block_time=BlockTime, epoch = Epoch, parent_blocks = ParentBlocks, vote_types = VoteTypes}) ->
    #data{stakers = Stakers, block_time=BlockTime, parent_blocks=remove_old_blocks(Epoch, ParentBlocks), vote_types = VoteTypes}.

remove_old_blocks(undefined, ParentBlocks) ->
    ParentBlocks;
remove_old_blocks(Epoch, ParentBlocks) ->
    TargetEpoch = Epoch - 4,
    maps:filter(fun(E, _B) -> E > TargetEpoch end, ParentBlocks).

set_validators(Validators, Data) ->
    Majority = calculate_majority(Validators),
    lager:info("Majority required ~p", [Majority]),
    Data#data{validators = Validators, majority = Majority, remaining_validators = maps:from_list(Validators)}.

get_staker_private_key(Staker, Stakers) ->
    maps:get(Staker, Stakers, undefined).

create_finalize_call(Votes, #{?HASH_FLD := Hash, ?EPOCH_DELTA_FLD := EpochDelta}, #data{epoch = Epoch, seed=Seed, leader=Leader, length = EpochLength, parent_blocks = ParentBlocks}) ->
    Seed1 = case Seed of
                undefined ->
                    case maps:get(Epoch, ParentBlocks, undefined) of
                        undefined ->
                            <<0>>;
                        ParentBlock ->
                            aec_parent_chain_block:hash(ParentBlock)
                    end;
                _ ->
                    Seed
            end,
    VotesList = maps:fold(fun create_vote_call/3, [], Votes),
    aeb_fate_abi:create_calldata(?FINALIZE_FUN_NAME, [Epoch, {bytes, Hash}, EpochLength + EpochDelta, {bytes, Seed1}, {address, Leader}, VotesList]).

create_vote_call(Producer, #{?HASH_FLD := Hash, ?EPOCH_DELTA_FLD := EpochDelta, ?SIGNATURE_FLD := Signature} = Payload, Accum) ->
    [{tuple, {{address, Producer}, {bytes, Hash}, EpochDelta, {bytes, get_sign_data(Payload)}, {bytes, Signature}}} | Accum].

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

update_proposal_after_vote_majority(#data{proposal=Proposal, votes=Votes, validators = Validators, leader=Leader} = Data) ->
    SumFun = sum_epoch_delta(Validators),
    Totals = maps:fold(SumFun, {0,0}, Votes),
    {TotalStake, TotalEpochDelta} = SumFun(Leader, Proposal,Totals),
    EpochDelta = round(TotalEpochDelta / TotalStake),
    UpdatedProposal = maps:put(?EPOCH_DELTA_FLD, EpochDelta, Proposal),
    Data#data{proposal = UpdatedProposal}.

sum_epoch_delta(Validators) ->
    fun(Producer, Vote, {TotalStake, TotalEpochDelta}) ->
            {Stake, EpochDelta} = get_weighted_delta(Producer, Vote, Validators),
            {TotalStake + Stake, EpochDelta * Stake + TotalEpochDelta} end.

get_weighted_delta(Producer, #{?EPOCH_DELTA_FLD := EpochDelta}, Validators) ->
    Stake = proplists:get_value(Producer, Validators, 0),
    {Stake, EpochDelta}.
