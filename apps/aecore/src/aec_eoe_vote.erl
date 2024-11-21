%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_vote).
-behaviour(gen_statem).

%% Export API functions
-export([start_link/2, negotiate/5, validators/2, get_result/0]).

%% Export gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% FSM states (as per gen_statem callback callback_mode/0)
-export([await_eoe/3, proposal/3, vote/3, finalize/3]).


%% ==================================================================
%% Records and Types

-record(data, {
                epoch,
                height,
                fork_hash,
                length_delta,
                leader,
                validators=[],
                remaining_validators=#{},
                stakers=#{},
                majority=0,
                proposal,
                block_time,
                num_calls=0,
                result,
                from,
                protocol}).


-define(PROPOSAL_TYPE, 1).
-define(VOTE_TYPE, 2).
-define(COMMIT_TYPE, 3).

-define(EPOCH_FLD, <<"epoch">>).
-define(HASH_FLD, <<"block_hash">>).
-define(HEIGHT_FLD, <<"block_height">>).
-define(PRODUCER_FLD, <<"producer">>).
-define(EPOCH_DELTA_FLD, <<"epoch_length_delta">>).
-define(SIGNATURE_FLD, <<"signature">>).

%% API to start the state machine
start_link(Stakers, BlockTime) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Stakers, BlockTime], []).

%% Negotiate a fork, called with preferred fork and epoch length delta
negotiate(Epoch, Height, Hash, LengthDelta, Protocol) ->
    gen_statem:cast(?MODULE, {negotiate, Epoch, Height, Hash, LengthDelta, Protocol}).

validators(Validators, Epoch) ->
    gen_statem:cast(?MODULE, {validators, Validators, Epoch}).

get_result() ->
    gen_statem:call(?MODULE, get_result).

%%% gen_statem callbacks

%% Initialization: Start in the await_eoe state
init([Stakers, BlockTime]) ->
    aec_events:subscribe(tx_received),
    {ok, await_eoe, #data{stakers=Stakers,block_time=BlockTime}}.

%% Set the callback mode to state functions
callback_mode() ->
    state_functions.

%%% State: AwaitEndOfEpoch
%% TODO get transactions that come in before negotiate has been called
await_eoe(cast, {negotiate, Epoch, Height, Hash, LengthDelta, Protocol}, #data{block_time=BlockTime, epoch=CurrentEpoch, proposal=Proposal, num_calls = NumCalls, majority=Majority} = Data) ->
    case Epoch == CurrentEpoch of
        true ->
            Data1 = Data#data{epoch=Epoch, height=Height, fork_hash=Hash, length_delta=LengthDelta, protocol=Protocol, num_calls = NumCalls + 1},
            case is_leader(Data1) of
                false ->
                    case Proposal of
                        undefined ->
                            lager:info("End of epoch ~p waiting for proposal", [Epoch]),
                            {next_state, proposal, Data1,
                                [{state_timeout,BlockTime,no_proposal}]};
                        _ ->
                            Data2 = handle_voting(Data1),
                            {next_state, vote, Data2,
                                [{state_timeout,BlockTime,no_quorum}]}
                    end;
                true ->
                    Stake = calculate_stake(Data1),
                    lager:info("Sending proposal for end of epoch ~p hash: ~p", [Epoch, Hash]),
                    LeaderProposal = #{?HASH_FLD => Hash, ?EPOCH_DELTA_FLD => LengthDelta},
                    %% block time * 2 because need to wait for votes to arrive
                    send(fun create_proposal/1, Data1, {next_state, vote, Data1#data{majority = Majority - Stake, proposal=LeaderProposal}, [{state_timeout,BlockTime * 2,no_quorum}]})
            end;
        false ->
            lager:warning("Validators for are set for epoch ~p, received end of epoch for epoch ~p", [CurrentEpoch, Epoch]),
            keep_state_and_data
    end;
%% TODO check signature & if already voted
await_eoe(cast, {validators, _Validators, Epoch}, #data{epoch=Epoch}) ->
    keep_state_and_data;
await_eoe(cast, {validators, Validators, Epoch}, Data) ->
    Majority = calculate_majority(Validators),
    lager:info("Majority required ~p", [Majority]),
    {Leader, _} = lists:last(Validators),
    Data1 = reset_data(Data),
    {keep_state, Data1#data{validators = Validators, majority=Majority, remaining_validators=maps:from_list(Validators), epoch=Epoch, leader=Leader}};
await_eoe(info, {gproc_ps_event, tx_received, #{info := SignedTx}}, Data) ->
    handle_proposal(SignedTx, Data);
await_eoe(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

%%% State: Proposal
proposal(info, {gproc_ps_event, tx_received, #{info := SignedTx}}, Data) ->
    %% Handle the proposal phase
    %% Check the transaction contains a proposal
    handle_proposal(SignedTx, Data);
proposal(state_timeout, no_proposal, Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Proposal timeout"),
    %% Reply with no consensus
    handle_no_consensus(Data);
proposal(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

%%% State: Vote
vote(info, {gproc_ps_event, tx_received, #{info := Tx}}, Data) ->
    %% Handle the voting phase
    %% Check the transaction contains a vote
    %% If more than two thirds of votes agree send a commit then transition to finalization phase
    handle_vote(?VOTE_TYPE, Tx, Data, fun check_voting_majority/1);
vote(state_timeout, no_quorum, Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Voting timeout"),
    %% Reply with no consensus, if new leader use preferred fork
    handle_no_consensus(Data);
vote(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).


%%% State: Finalize
finalize(info, {gproc_ps_event, tx_received, #{info := Tx}}, Data) ->
    handle_vote(?COMMIT_TYPE, Tx, Data, fun check_commit_majority/1);
finalize(state_timeout, no_quorum, Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Finalize timeout"),
    handle_no_consensus(Data);
finalize(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).


create_proposal(#data{leader=Leader, stakers=Stakers, epoch=Epoch, height=Height, fork_hash=Hash, length_delta=LengthDelta}) ->
    PrivKey = get_staker_private_key(Leader, Stakers),
    Proposal =#{?HASH_FLD => aeser_api_encoder:encode(key_block_hash, Hash),
                ?EPOCH_DELTA_FLD => LengthDelta,
                ?HEIGHT_FLD => Height},
    ProposalPayload = create_payload(Proposal, PrivKey),
    create_vote_transaction(Leader, PrivKey, Epoch, ?PROPOSAL_TYPE, ProposalPayload).


create_votes(Type, #data{proposal=ProposalFields, leader=Leader, stakers=Stakers, validators=Validators, epoch=Epoch, height=Height, fork_hash=Hash, length_delta=LengthDelta}) ->
    case maps:get(?HASH_FLD, ProposalFields, undefined) of
        undefined ->
            lager:warning("Hash field not found in proposal ~p", [ProposalFields]),
            [];
        Hash ->
            VoteFlds =#{?HASH_FLD => aeser_api_encoder:encode(key_block_hash, Hash),
                        ?EPOCH_DELTA_FLD => LengthDelta,
                        ?HEIGHT_FLD => Height},
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, []);
        ProposalHash ->
            lager:warning("Proposal hash ~p does not match hash ~p", [aeser_api_encoder:encode(key_block_hash, ProposalHash), aeser_api_encoder:encode(key_block_hash, Hash)]),
            []
    end.


create_votes(_VoteFlds, _Leader, [], _Stakers, _Epoch, _Type, Votes) ->
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


handle_proposal(SignedTx, #data{leader=Leader, epoch=Epoch, fork_hash=Hash} = Data) ->
    %% Handle the proposal phase
    %% Check the transaction contains a proposal
    case convert_transaction(SignedTx) of
        {ok, ?PROPOSAL_TYPE, ProposalEpoch, ProposalValidator, ProposalFields} ->
            case ProposalEpoch of
                Epoch ->
                    case ProposalValidator of
                        Leader ->
                            Data1 = Data#data{proposal=ProposalFields},
                            case Hash of
                                undefined ->
                                    {keep_state, Data1};
                                _ ->
                                    Data2 = handle_voting(Data1),
                                    check_voting_majority(Data2)
                            end;
                        InvalidProducer ->
                                lager:warning("Received proposal from invalid leader ~p ~p", [InvalidProducer, Leader]),
                                keep_state_and_data
                    end;
                InvalidEpoch ->
                    lager:warning("Received proposal from invalid epoch ~p", [InvalidEpoch]),
                    keep_state_and_data
                end;
        {ok, _Type, _, _, _} ->
            %% TODO is it possible to get a vote before the proposal?
            keep_state_and_data;
        {error, not_vote} ->
            keep_state_and_data;
        {error, Reason} ->
            lager:warning("Could not process proposal ~p for reason ~p", [SignedTx, Reason]),
            keep_state_and_data
    end.

handle_vote(Type, SignedTx, Data, CheckMajorityFun) ->
    case convert_transaction(SignedTx) of
        {ok, Type, _Epoch, Validator, VoteFields} ->
            %% Check if a validator
            case can_vote(Validator, Data) of
                true ->
                    lager:info("Received a vote: ~p", [VoteFields]),
                    Data1 = count_vote(Validator, Data),
                    %% Check if reached two thirds
                    CheckMajorityFun(Data1);
                {error, not_a_validator} ->
                    lager:warning("Received a vote from a non validator ~p", [Validator]),
                    keep_state_and_data;
                {error, already_voted} ->
                    lager:warning("Received a vote from a validator ~p that has already voted", [Validator]),
                    keep_state_and_data
            end;
        {ok, _Type, _, _, _} ->
            keep_state_and_data;
        {error, not_vote} ->
            keep_state_and_data;
        {error, Reason} ->
            lager:warning("Could not process vote ~p for reason ~p", [SignedTx, Reason]),
            keep_state_and_data
    end.


create_payload(Payload, PrivKey) ->
    Fields = maps:map(fun fld/2, Payload),
    PayloadBin = iolist_to_binary(lists:foldl(fun({Key, Value}, Accum) -> [<<Key/binary,Value/binary>>|Accum] end, [], lists:sort(maps:to_list(Fields)))),
    Signature = enacl:sign_detached(PayloadBin, PrivKey),
    maps:put(?SIGNATURE_FLD, aeser_api_encoder:encode(signature, Signature), Fields).

fld(_FieldName, Value) when is_integer(Value) ->
    integer_to_binary(Value);
fld(_FieldName, Value) ->
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


check_voting_majority(#data{proposal=Proposal, majority = CurrentMajority, validators=Validators, block_time=BlockTime} = Data) ->
    case CurrentMajority =< 0 of
        true ->
            lager:info("Quorum achieved for voting"),
            Majority = calculate_majority(Validators),
            Result = {ok, Proposal},
            Data1 = Data#data{result = Result},
            send_commits(Data1),
            {next_state, finalize, Data1#data{majority = Majority, remaining_validators = maps:from_list(Validators)}};
        false ->
            {keep_state, Data, [{state_timeout,BlockTime,no_quorum}]}
    end.

check_commit_majority(#data{result = Result, majority = CurrentMajority, block_time=BlockTime, from=From} = Data) ->
    case CurrentMajority =< 0 of
        true ->
            lager:info("Quorum achieved for commit"),
            Actions = case From of
                        undefined ->
                            [{state_timeout,BlockTime,no_quorum}];
                        _ ->
                            lager:info("Replying quorum achieved for commits"),
                            [{reply, From, Result}]
                      end,
            {next_state, finalize, Data, Actions};
        false ->
            {keep_state, Data, [{state_timeout,BlockTime,no_quorum}]}
    end.

handle_voting(#data{majority = Majority} = Data) ->
    Stake = calculate_stake(Data),
    send_votes(Data),
    Data#data{majority = Majority - Stake}.

send_votes(Data) ->
    lager:info("Sending votes"),
    lists:foreach(fun aec_hc_vote_pool:push/1, create_votes(?VOTE_TYPE, Data)).

send_commits(Data) ->
    lager:info("Sending commits"),
    lists:foreach(fun aec_hc_vote_pool:push/1, create_votes(?COMMIT_TYPE, Data)).


send(CreateTxFun, Data, Success) ->
    SignedTx = CreateTxFun(Data),
    case aec_hc_vote_pool:push(SignedTx) of
        ok ->
            Success;
        {error, Reason} ->
            lager:debug("~p tx failed: ~p", [SignedTx, Reason]),
            handle_no_consensus(Data)
    end.

convert_transaction(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    case aetx:specialize_type(Tx) of
        {hc_vote_tx, VoteTx} ->
            case aec_hc_vote_tx:type(VoteTx) of
                Type when Type == ?PROPOSAL_TYPE; Type == ?VOTE_TYPE; Type == ?COMMIT_TYPE ->
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

convert_payload_field(?HASH_FLD, Value) ->
    {ok, Hash} = aeser_api_encoder:safe_decode(block_hash,Value),
    Hash;
convert_payload_field(?HEIGHT_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(?EPOCH_DELTA_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(_Key, Value) ->
    Value.

check_signature(PubKey, Payload) ->
    case get_signatute_from_payload(Payload) of
        {ok, Signature} ->
                Fields = lists:sort(maps:to_list(maps:remove(?SIGNATURE_FLD, Payload))),
                Data = iolist_to_binary(lists:foldl(fun({Key, Value}, Accum) -> [<<Key/binary,Value/binary>>|Accum] end, [], Fields)),
            case enacl:sign_verify_detached(Signature, Data, PubKey) of
                true  -> ok;
                false ->
                    {error, signature_verification_failed}
            end;
        _ ->
            {error, signature_verification_failed}
    end.


get_signatute_from_payload(Payload) ->
    case maps:get(?SIGNATURE_FLD, Payload, undefined) of
        undefined ->
            {error, signature_not_found};
        Signature ->
            aeser_api_encoder:safe_decode(signature, Signature)
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
    {next_state, await_eoe, Data#data{result = Result}, Actions}.

handle_common_event({call,From}, get_result, #data{result=Result, num_calls = NumCalls, epoch=Epoch} = Data) ->
    case Epoch of
        undefined ->
            {keep_state_and_data, [{reply, From, not_ready}]};
        _ ->
            IsLeader = is_leader(Data),
            case (not IsLeader) or ((NumCalls > 0) and IsLeader) of
                true ->
                    Data1 = Data#data{num_calls = NumCalls + 1},
                    case Result of
                        undefined ->
                            {keep_state, Data1#data{from=From}};
                        _ ->
                            {keep_state, Data1, [{reply, From, Result}]}
                    end;
                false ->
                    {keep_state, Data#data{num_calls = NumCalls + 1}, [{reply, From, not_ready}]}
            end
    end;
handle_common_event(_E, _Msg, #data{}) ->
    lager:info("Common ~p ~p", [_E, _Msg]),
    %% TODO
    keep_state_and_data.


calculate_stake(#data{validators = Validators, stakers = Stakers}) ->
    lists:foldl(fun({Validator, Stake}, Accum) ->
        case maps:is_key(Validator, Stakers) of
            true ->
                Accum;
            false ->
                Stake + Accum
        end end, 0, Validators).

calculate_majority(Validators) ->
    TotalStake = lists:foldl(fun({_, Stake}, Accum) -> Stake + Accum end, 0, Validators),
    math:ceil((2 * TotalStake) / 3).

reset_data(#data{stakers = Stakers, block_time=BlockTime}) ->
    #data{stakers = Stakers, block_time=BlockTime}.

get_staker_private_key(Staker, Stakers) ->
    maps:get(Staker, Stakers, undefined).

