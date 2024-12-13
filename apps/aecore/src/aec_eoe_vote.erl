%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_vote).
-behaviour(gen_statem).

%% Export API functions
-export([start_link/3, negotiate/8, get_finalize_transaction/1]).

%% Export gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% FSM states (as per gen_statem callback callback_mode/0)
-export([await_eoe/3, proposal/3, vote/3, finalize/3, complete/3]).


%% ==================================================================
%% Records and Types

-type create_contract_call_fun() :: fun((aec_keys:pubkey(), aec_trees:trees(), aeser_api_encoder:encoded(), aect_contracts:amount())
    -> {ok, aetx:tx()}).

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
                create_contract_call_fun   :: create_contract_call_fun(),
                from                       :: pid() | undefined,
                votes=#{}                  :: #{binary() => #{binary() => any()}}
            }).


-define(PROPOSAL_TYPE, 1).
-define(VOTE_TYPE, 2).
-define(COMMIT_TYPE, 3).

-define(HASH_FLD, <<"block_hash">>).
-define(HEIGHT_FLD, <<"block_height">>).
-define(EPOCH_DELTA_FLD, <<"epoch_length_delta">>).
-define(SIGNATURE_FLD, <<"signature">>).

%% API to start the state machine
-spec start_link(#{binary() => binary()}, non_neg_integer(), create_contract_call_fun())  -> {ok, pid()} | {error, atom()}.
start_link(Stakers, BlockTime, CreateContractCallFun) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Stakers, BlockTime, CreateContractCallFun], []).

%% Negotiate a fork, called with preferred fork and epoch length delta
-spec negotiate(non_neg_integer(), non_neg_integer(), binary(), aec_keys:pubkey(), [{binary(), non_neg_integer()}], binary(), non_neg_integer(), non_neg_integer()) -> ok.
negotiate(Epoch, Height, Hash, Leader, Validators, Seed, LengthDelta, CurrentLength) ->
    gen_statem:cast(?MODULE, {negotiate, Epoch, Height, Hash, Leader, Validators, Seed, LengthDelta, CurrentLength}).

-spec get_finalize_transaction(aec_trees:trees()) -> {ok, aetx_sign:signed_tx()} | {error, not_ready} | {error, term()}.
get_finalize_transaction(Trees) ->
    gen_statem:call(?MODULE, {get_finalize_transaction, Trees}).

%%% gen_statem callbacks

%% Initialization: Start in the await_eoe state
init([Stakers, BlockTime, CreateContractCallFun]) ->
    aec_events:subscribe(tx_received),
    aec_events:subscribe(new_epoch),
    {ok, await_eoe, #data{stakers=Stakers,block_time=BlockTime,create_contract_call_fun=CreateContractCallFun}}.

%% Set the callback mode to state functions
callback_mode() ->
    state_functions.

%%% State: AwaitEndOfEpoch
await_eoe(cast, {negotiate, Epoch, Height, Hash, Leader, Validators, Seed, LengthDelta, CurrentLength}, #data{block_time=BlockTime, proposal=Proposal, majority=Majority} = Data) ->
    Data1 = set_validators(Validators, Data#data{epoch=Epoch, height=Height, fork_hash=Hash, seed=Seed, length=CurrentLength, leader=Leader, validators = Validators, length_delta=LengthDelta}),
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
    handle_vote(?VOTE_TYPE, Tx, Data, fun on_valid_vote/3);
vote(state_timeout, no_quorum, Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Voting timeout"),
    %% Reply with no consensus, if new leader use preferred fork
    handle_no_consensus(Data);
vote(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).


%%% State: Finalize
finalize(info, {gproc_ps_event, tx_received, #{info := Tx}}, Data) ->
    handle_vote(?COMMIT_TYPE, Tx, Data, fun on_valid_commit/3);
finalize(state_timeout, no_quorum, Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("Finalize timeout"),
    handle_no_consensus(Data);
finalize(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

complete(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

create_proposal(#data{leader=Leader, stakers=Stakers, epoch=Epoch, height=Height, fork_hash=Hash, length_delta=LengthDelta}) ->
    PrivKey = get_staker_private_key(Leader, Stakers),
    Proposal =#{?HASH_FLD => Hash,
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
            VoteFlds =#{?HASH_FLD => Hash,
                        ?EPOCH_DELTA_FLD => LengthDelta,
                        ?HEIGHT_FLD => Height},
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, []);
        ProposalHash ->
            lager:warning("Proposal hash ~p does not match hash ~p", [ProposalHash, Hash]),
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
        {ok, ?PROPOSAL_TYPE, Epoch, Leader, ProposalFields} ->
            Data1 = Data#data{proposal=ProposalFields},
            case Hash of
                %% Negotation hasn't begun yet
                undefined ->
                    {keep_state, Data1};
                _ ->
                    Data2 = handle_voting(Data1),
                    check_voting_majority(Data2)
            end;
        {ok, ?PROPOSAL_TYPE, Epoch, InvalidProducer, _ProposalFields} ->
            lager:warning("Received proposal from invalid leader ~p", [InvalidProducer]),
            keep_state_and_data;
        {ok, ?PROPOSAL_TYPE, InvalidEpoch, Leader, _ProposalFields} ->
            lager:warning("Received proposal from invalid epoch ~p", [InvalidEpoch]),
            keep_state_and_data;
        {ok, ?PROPOSAL_TYPE, InvalidEpoch, InvalidProducer, _ProposalFields} ->
            lager:warning("Received proposal from invalid epoch ~p and invalid leader ~p", [InvalidEpoch, InvalidProducer]),
            keep_state_and_data;
        {ok, _Type, _, _, _} ->
            %% TODO is it possible to get a vote before the proposal?
            keep_state_and_data;
        {error, not_vote} ->
            keep_state_and_data;
        {error, Reason} ->
            lager:warning("Could not process proposal ~p for reason ~p", [SignedTx, Reason]),
            keep_state_and_data
    end.

handle_vote(Type, SignedTx, Data, OnValidFun) ->
    case convert_transaction(SignedTx) of
        {ok, Type, _Epoch, Validator, VoteFields} ->
            %% Check if a validator
            case can_vote(Validator, Data) of
                true ->
                    lager:info("Received a vote: ~p", [VoteFields]),
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
    maps:put(?SIGNATURE_FLD, Signature, Fields).

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

on_valid_vote(Validator, VoteFields, #data{votes=Votes} = Data) ->
    Votes1 = maps:put(Validator, VoteFields, Votes),
    check_voting_majority(Data#data{votes = Votes1}).

check_voting_majority(#data{proposal=Proposal, majority = CurrentMajority, validators=Validators, block_time=BlockTime, votes=Votes} = Data) ->
    case CurrentMajority =< 0 of
        true ->
            lager:info("Quorum achieved for voting"),
            Majority = calculate_majority(Validators),
            Result = create_finalize_call(Votes, Proposal, Data),
            Data1 = Data#data{result = Result},
            send_commits(Data1),
            {next_state, finalize, Data1#data{majority = Majority, remaining_validators = maps:from_list(Validators)}};
        false ->
            {next_state, vote, Data, [{state_timeout,BlockTime,no_quorum}]}
    end.

on_valid_commit(_Validator, _CommitFields, #data{result = Result, majority = CurrentMajority, block_time=BlockTime, from=From} = Data) ->
    case CurrentMajority =< 0 of
        true ->
            lager:info("Quorum achieved for commit"),
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

convert_payload_field(?HEIGHT_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(?EPOCH_DELTA_FLD, Value) ->
    binary_to_integer(Value);
convert_payload_field(_Key, Value) ->
    Value.

check_signature(PubKey, Payload) ->
    case get_signature_from_payload(Payload) of
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
    lager:info("Common ~p ~p", [_E, _Msg]),
    %% TODO
    keep_state_and_data.

convert_to_finalize_transaction(Result, Data) ->
    case aec_chain:get_top_state() of
        {ok, Trees} ->
            convert_to_finalize_transaction(Result, Trees, Data);
        Error ->
            {error, Error}
    end.

convert_to_finalize_transaction({ok, CallData}, Trees, #data{create_contract_call_fun = CreateContractCallFun, leader=Leader, stakers=Stakers, epoch=Epoch}) ->
    case aec_consensus_hc:get_entropy_hash(Epoch + 2) of
        %% Don't create the transaction until the seed is available.
        {ok, _Seed} ->
            {ok, Tx} = CreateContractCallFun(Leader, Trees, aeser_api_encoder:encode(contract_bytearray, CallData), 0),
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
    trunc(math:ceil((2 * TotalStake) / 3)).

reset_data(#data{stakers = Stakers, block_time=BlockTime, create_contract_call_fun=CreateContractCallFun}) ->
    #data{stakers = Stakers, block_time=BlockTime, create_contract_call_fun = CreateContractCallFun}.

set_validators(Validators, Data) ->
    Majority = calculate_majority(Validators),
    lager:info("Majority required ~p", [Majority]),
    Data#data{validators = Validators, majority=Majority, remaining_validators=maps:from_list(Validators)}.

get_staker_private_key(Staker, Stakers) ->
    maps:get(Staker, Stakers, undefined).

create_finalize_call(Votes, #{?HASH_FLD := Hash, ?EPOCH_DELTA_FLD := EpochDelta}, #data{epoch = Epoch, seed=Seed, leader=Leader, length = EpochLength}) ->
    Seed1 = case Seed of
                undefined ->
                    <<0>>;
                _ ->
                    Seed
            end,
    VotesList = maps:fold(fun create_vote_call/3, [], Votes),
    aeb_fate_abi:create_calldata("finalize_epoch", [Epoch, {bytes, Hash}, EpochLength + EpochDelta, {bytes, Seed1}, {address, Leader}, VotesList]).

create_vote_call(Producer, #{?HASH_FLD := Hash, ?EPOCH_DELTA_FLD := EpochDelta, ?SIGNATURE_FLD := Signature}, Accum) ->
    [{tuple, {{address, Producer}, {bytes, Hash}, EpochDelta, {bytes, Signature}}}|Accum].

