%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_gen_vote).
-behaviour(gen_statem).

%% Export API functions
-export([start_link/5, negotiate/8, get_finalize_transaction/3, add_parent_block/3, convert_payload_field/2]).

%% Export gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% FSM states (as per gen_statem callback callback_mode/0)
-export([await_eoe/3, proposal/3, vote/3, finalize/3, complete/3]).

%% behaviour
-callback init(list(term())) -> term().
-callback init_state(non_neg_integer(), binary(), #{non_neg_integer() => aec_parent_chain_block:block()}, non_neg_integer(), non_neg_integer(), term()) -> term().
-callback reset_state(term()) -> term().
-callback create_proposal(proposal(), term()) -> proposal().
-callback create_vote(proposal(), #{binary() => any()}, term()) -> {ok, #{binary() => any()}} | {error, term()}.
-callback vote_params(vote()) -> [term()].
-callback finalize_call(proposal(), term()) -> {list(), [term()]}.
-callback convert_payload_field(binary(), binary()) -> term().
-callback vote_description() -> term().

%% ==================================================================
%% Records and Types

-record(data, {
                epoch                      :: non_neg_integer() | undefined,
                height                     :: non_neg_integer() | undefined,
                seed                       :: binary() | undefined,
                leader                     :: binary() | undefined,
                validators=[]              :: [{binary(), non_neg_integer()}],
                remaining_validators=#{}   :: #{binary() => non_neg_integer()},
                stakers=#{}                :: #{binary() => binary()},
                majority=0                 :: non_neg_integer(),
                proposal                   :: proposal() | undefined,
                block_time                 :: non_neg_integer(),
                result                     :: {ok, binary()} | {error, no_consensus} | undefined,
                from                       :: pid() | undefined,
                nonce_offset=1             :: non_neg_integer(),
                votes=#{}                  :: #{binary() => vote()},
                parent_blocks=#{}          :: #{non_neg_integer() => aec_parent_chain_block:block()},
                other_votes=[]             :: list({non_neg_integer(), aetx_sign:signed_tx()}),
                vote_types                 :: vote_types(),
                rev_vote_types             :: #{non_neg_integer() => atom()},
                module                     :: atom(),
                state                      :: term()
            }).

-define(HEIGHT_FLD, <<"block_height">>).
-define(SIGNATURE_FLD, <<"signature">>).
-define(LEADER_FLD, <<"leader">>).
-define(EPOCH_FLD, <<"epoch">>).

-define(NewTx(TxEvent), TxEvent == tx_created; TxEvent == tx_received).

-type vote_types() :: #{
    proposal => non_neg_integer(),
    vote => non_neg_integer(),
    commit => non_neg_integer()
}.

-type proposal() :: #{binary() => any()}.
-type vote() :: #{binary() => any()}.

-export_type([vote_types/0, proposal/0, vote/0]).


%% API to start the state machine
-spec start_link(atom(), atom(), vote_types(), #{binary() => binary()}, non_neg_integer())  -> {ok, pid()} | {error, atom()}.
start_link(EOEVoteType, Module, VoteTypes, Stakers, BlockTime) ->
    gen_statem:start_link({local, EOEVoteType}, ?MODULE, [Module, VoteTypes, Stakers, BlockTime], []).

%% Negotiate a fork, called with preferred fork and epoch length delta
-spec negotiate(atom(), non_neg_integer(), non_neg_integer(), binary(), aec_keys:pubkey(), [{binary(), non_neg_integer()}], binary(), non_neg_integer()) -> ok.
negotiate(EOEVoteType, Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength) ->
    gen_statem:cast(EOEVoteType, {negotiate, Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength}).

-spec add_parent_block(atom(), non_neg_integer(), aec_parent_chain_block:block()) -> ok.
add_parent_block(EOEVoteType, Epoch, ParentBlock) ->
    gen_statem:cast(EOEVoteType, {add_parent_block, Epoch, ParentBlock}).

-spec get_finalize_transaction(atom(), aec_trees:trees(), non_neg_integer()) -> {ok, aetx_sign:signed_tx()} | {error, not_ready} | {error, term()}.
get_finalize_transaction(EOEVoteType, Trees, NonceOffset) ->
    gen_statem:call(EOEVoteType, {get_finalize_transaction, Trees, NonceOffset}).

%%% gen_statem callbacks

%% Initialization: Start in the await_eoe state
init([Module, VoteTypes, Stakers, BlockTime]) ->
    aec_events:subscribe(tx_received),
    aec_events:subscribe(tx_created),
    aec_events:subscribe(new_epoch),
    State = Module:init([Stakers, BlockTime]),
    RevVoteTypes = maps:fold(fun(K, V, Acc) -> maps:put(V, K, Acc) end, #{}, VoteTypes),
    {ok, await_eoe, #data{stakers=Stakers,block_time=BlockTime,vote_types=VoteTypes,rev_vote_types=RevVoteTypes,module=Module,state=State}}.

%% Set the callback mode to state functions
callback_mode() ->
    state_functions.

%%% State: AwaitEndOfEpoch
await_eoe(cast, {negotiate, Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength}, #data{block_time=BlockTime, proposal=Proposal, parent_blocks=ParentBlocks, state=ClientState, module=Module} = Data) ->
    NewClientState = Module:init_state(Epoch, Hash, ParentBlocks, CurrentLength, BlockTime, ClientState),
    VoteDesc = vote_description(Data),
    Data1 = set_validators(Validators, Data#data{epoch=Epoch, height=Height, seed=Seed, leader=Leader, validators = Validators, state=NewClientState}),
    case is_leader(Data1) of
        false ->
            case Proposal of
                undefined ->
                    lager:info("End of epoch ~p waiting for ~p proposal", [Epoch, VoteDesc]),
                    {next_state, proposal, Data1,
                        [{state_timeout,BlockTime,no_proposal}]};
                #{?LEADER_FLD := Leader, ?EPOCH_FLD := Epoch} ->
                    Data2 = handle_voting(Data1),
                    {next_state, vote, Data2,
                        [{state_timeout,BlockTime,no_quorum}]};
                _ ->
                    lager:info("Discarding invalid ~p proposal ~p, end of epoch ~p waiting for proposal", [VoteDesc, Proposal, Epoch]),
                    {next_state, proposal, Data1#data{proposal = undefined},
                        [{state_timeout,BlockTime,no_proposal}]}

            end;
        true ->
            lager:info("Sending ~p proposal for end of epoch ~p hash: ~p", [VoteDesc, Epoch, Hash]),
            NewProposal = Module:create_proposal(#{?HEIGHT_FLD => Height}, NewClientState),
            Data2 = Data1#data{proposal = NewProposal},
            Next = {next_state, proposal, Data2, [{state_timeout, BlockTime, no_proposal}]},
            send(fun create_proposal/1, Data2, Next)
    end;
await_eoe(info, {gproc_ps_event, tx_received, #{info := SignedTx}}, Data) ->
    handle_proposal(SignedTx, Data);
await_eoe(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

%%% State: Proposal
proposal(info, {gproc_ps_event, TxEvent, #{info := SignedTx}}, Data) when ?NewTx(TxEvent) ->
    %% Handle the proposal phase
    %% Check the transaction contains a proposal
    handle_proposal(SignedTx, Data);
proposal(state_timeout, no_proposal, #data{epoch = Epoch} = Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("~p proposal timeout for epoch ~p", [vote_description(Data), Epoch]),
    %% Reply with no consensus
    handle_no_consensus(Data);
proposal(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

%%% State: Vote
vote(info, {gproc_ps_event, TxEvent, #{info := Tx}}, #data{vote_types=#{vote := VoteType}} = Data) when ?NewTx(TxEvent) ->
    %% Handle the voting phase
    %% Check the transaction contains a vote
    %% If more than two thirds of votes agree send a commit then transition to finalization phase
    handle_vote(VoteType, Tx, Data, fun on_valid_vote/3, fun on_other_vote/3);
vote(state_timeout, no_quorum, #data{epoch = Epoch} = Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("~p voting timeout for epoch ~p", [vote_description(Data), Epoch]),
    %% Reply with no consensus, if new leader use preferred fork
    handle_no_consensus(Data);
vote(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).


%%% State: Finalize
finalize(info, {gproc_ps_event, TxEvent, #{info := Tx}}, #data{vote_types=#{commit := CommitType}} = Data) when ?NewTx(TxEvent) ->
    handle_vote(CommitType, Tx, Data, fun on_valid_commit/3, fun on_other_vote_type/3);
finalize(state_timeout, no_quorum, #data{epoch = Epoch} = Data) ->
    %% Handle timeout if no proposal is received
    lager:warning("~p finalize timeout for epoch ~p", [vote_description(Data), Epoch]),
    handle_no_consensus(Data);
finalize(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

complete(Type, Msg, D) ->
    handle_common_event(Type, Msg, D).

create_proposal(#data{leader=Leader, stakers=Stakers, epoch=Epoch, proposal = Proposal, vote_types=#{proposal := ProposalType}}) ->
    PrivKey = get_staker_private_key(Leader, Stakers),
    ProposalPayload = create_payload(Proposal, PrivKey),
    create_vote_transaction(Leader, PrivKey, Epoch, ProposalType, ProposalPayload).

create_votes(Type, #data{proposal=ProposalFields, leader=Leader, stakers=Stakers, validators=Validators, epoch=Epoch, height=Height, module=Module, state=ClientState} = Data) ->
    case Module:create_vote(ProposalFields, #{?HEIGHT_FLD => Height}, ClientState) of
        {ok, VoteFlds} ->
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, [], vote_type_description(Type, Data), vote_description(Data));
        {error, _Reason} ->
            []
    end.

create_votes(_VoteFlds, _Leader, [], _Stakers, _Epoch, Type, Votes, TypeDesc, Desc) ->
    lager:debug("Created ~p ~p votes of type ~p(~p)", [length(Votes), Desc, TypeDesc, Type]),
    Votes;
create_votes(VoteFlds, Leader, [{Leader,_}|Validators], Stakers, Epoch, Type, Votes, TypeDesc, Desc) ->
    create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, Votes, TypeDesc, Desc);
create_votes(VoteFlds, Leader, [{Validator,_}|Validators], Stakers, Epoch, Type, Votes, TypeDesc, Desc) ->
    case get_staker_private_key(Validator, Stakers) of
        undefined ->
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, Votes, TypeDesc, Desc);
        PrivKey ->
            VotePayload = create_payload(VoteFlds, PrivKey),
            Vote = create_vote_transaction(Validator, PrivKey, Epoch, Type, VotePayload),
            create_votes(VoteFlds, Leader, Validators, Stakers, Epoch, Type, [Vote|Votes], TypeDesc, Desc)
    end.

handle_proposal(SignedTx, #data{leader=undefined, epoch=undefined, vote_types=#{proposal := ProposalType} = VoteTypes, module=Module} = Data) ->
    case convert_transaction(SignedTx, VoteTypes, Module) of
        {ok, ProposalType, Epoch, Leader, ProposalFields} ->
            %% Store the leader so it can be used for validation laster
            ProposalFields1 = maps:put(?LEADER_FLD, Leader, ProposalFields),
            %% The epoch is also need for validation
            ProposalFields2 = maps:put(?EPOCH_FLD, Epoch, ProposalFields1),
            {keep_state, Data#data{proposal=ProposalFields2}};
        _ ->
            keep_state_and_data
    end;
handle_proposal(SignedTx, #data{leader=Leader, epoch=Epoch, vote_types=#{proposal := ProposalType} = VoteTypes, module=Module} = Data) ->
    VoteDesc = vote_description(Data),
    %% Handle the proposal phase
    %% Check the transaction contains a proposal
    case convert_transaction(SignedTx, VoteTypes, Module) of
        {ok, ProposalType, Epoch, Leader, ProposalFields} ->
            Data1 = Data#data{proposal=ProposalFields},
            Data2 = handle_voting(Data1),
            check_voting_majority(Data2);
        {ok, ProposalType, Epoch, InvalidProducer, _ProposalFields} ->
            lager:warning("Received ~p proposal from invalid leader ~p", [VoteDesc, InvalidProducer]),
            keep_state_and_data;
        {ok, ProposalType, InvalidEpoch, Leader, _ProposalFields} ->
            lager:warning("Received ~p proposal from invalid epoch ~p", [VoteDesc, InvalidEpoch]),
            keep_state_and_data;
        {ok, ProposalType, InvalidEpoch, InvalidProducer, _ProposalFields} ->
            lager:warning("Received ~p proposal from invalid epoch ~p and invalid leader ~p", [VoteDesc, InvalidEpoch, InvalidProducer]),
            keep_state_and_data;
        {ok, _Type, _, _, _} ->
            keep_state_and_data;
        {error, not_vote} ->
            keep_state_and_data;
        {error, Reason} ->
            lager:warning("Could not process ~p proposal ~p for reason ~p", [VoteDesc, SignedTx, Reason]),
            keep_state_and_data
    end.

handle_vote(Type, SignedTx, #data{vote_types=VoteTypes, module=Module} = Data, OnValidFun, OnOtherVoteType) ->
    VoteDesc = vote_description(Data),
    case convert_transaction(SignedTx, VoteTypes, Module) of
        {ok, Type, _Epoch, Validator, VoteFields} ->
            %% Check if a validator
            case can_vote(Validator, Data) of
                true ->
                    lager:info("Received a ~p vote from ~p of type ~p(~p) (vote: ~p)", [VoteDesc, Validator, vote_type_description(Type, Data), Type, VoteFields]),
                    Data1 = count_vote(Validator, Data),
                    %% Check if reached two thirds
                    OnValidFun(Validator, VoteFields, Data1);
                {error, not_a_validator} ->
                    lager:warning("Received a ~p vote of type ~p(~p) from a non validator ~p", [VoteDesc, vote_type_description(Type, Data), Type, Validator]),
                    keep_state_and_data;
                {error, already_voted} ->
                    lager:debug("Received a ~p vote of type ~p(~p) from a validator ~p that has already voted", [VoteDesc, vote_type_description(Type, Data), Type, Validator]),
                    keep_state_and_data
            end;
        {ok, OtherType, _, _, _} ->
            OnOtherVoteType(OtherType, SignedTx, Data);
        {error, not_vote} ->
            keep_state_and_data;
        {error, Reason} ->
            lager:warning("Could not process ~p vote ~p for reason ~p", [VoteDesc, SignedTx, Reason]),
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

check_voting_majority(#data{majority=CurrentMajority, validators=Validators, block_time=BlockTime, votes=Votes, epoch=Epoch, proposal=Proposal} = Data) ->
    case CurrentMajority =< 0 of
        true ->
            VoteDesc = vote_description(Data),
            lager:info("Quorum achieved for ~p voting for epoch ~p has ~p votes", [VoteDesc, Epoch, maps:size(Votes)]),
            Majority = calculate_majority(Validators),
            Data1 = Data#data{proposal=Proposal},
            Result = create_finalize_call(Votes, Proposal, Data1),
            Data2 = Data1#data{result = Result},
            send_commits(Data2),
            Stake = calculate_leader_stake(Data2),
            lager:debug("Leader implicitly ~p voting with stake ~p, remaining = ~p", [VoteDesc, Stake, Majority - Stake]),
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
            lager:info("Quorum achieved for ~p commit for epoch ~p", [vote_description(Data), Epoch]),
            Actions = case From of
                        undefined ->
                            [];
                        _ ->
                            lager:debug("Replying quorum achieved for ~p commits, result ~p", [vote_description(Data), Result]),
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

on_other_vote_type(Type, _SignedTx, Data) ->
    lager:debug("Other ~p vote type ~p(~p)", [vote_description(Data), vote_type_description(Type, Data), Type]),
    keep_state_and_data.

handle_voting(#data{majority = Majority} = Data) ->
    Stake = calculate_leader_stake(Data),
    send_votes(Data),
    lager:debug("Handle ~p voting - leader implicilty votes with stake: ~p remaining: ~p", [vote_description(Data), Stake, Majority - Stake]),
    Data#data{majority = Majority - Stake}.

send_votes(#data{epoch = Epoch, vote_types = #{vote := VoteType}} = Data) ->
    lager:info("Sending ~p votes for epoch ~p", [vote_description(Data), Epoch]),
    lists:foreach(fun aec_hc_vote_pool:push/1, create_votes(VoteType, Data)).

send_commits(#data{epoch = Epoch, vote_types = #{commit := CommitType}} = Data) ->
    lager:info("Sending ~p commits for epoch ~p", [vote_description(Data), Epoch]),
    lists:foreach(fun aec_hc_vote_pool:push/1, create_votes(CommitType, Data)).


send(CreateTxFun, Data, Success) ->
    SignedTx = CreateTxFun(Data),
    case aec_hc_vote_pool:push(SignedTx) of
        ok ->
            Success;
        {error, Reason} ->
            lager:debug("~p ~p tx failed: ~p", [vote_description(Data), SignedTx, Reason]),
            handle_no_consensus(Data)
    end.

convert_transaction(SignedTx, #{proposal := ProposalType, vote := VoteType, commit := CommitType}, Module) ->
    Tx = aetx_sign:tx(SignedTx),
    case aetx:specialize_type(Tx) of
        {hc_vote_tx, VoteTx} ->
            case aec_hc_vote_tx:type(VoteTx) of
                Type when Type == ProposalType; Type == VoteType; Type == CommitType ->
                    Payload = aec_hc_vote_tx:data(VoteTx),
                    PubKey = aec_hc_vote_tx:voter_pubkey(VoteTx),
                    case check_signature(PubKey, Payload) of
                        ok ->
                            case convert_payload_fields(Module, Payload) of
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

convert_payload_fields(Module, Fields) ->
    try
        {ok, maps:map(fun Module:convert_payload_field/2, Fields)}
    catch error:Reason:StackTrace ->
        lager:warning("Error decoding payload: ~p ~p", [Reason, StackTrace]),
        {error, Reason}
    end.

convert_payload_field(?HEIGHT_FLD, Value) ->
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
            lager:debug("~p voter had ~p stake, remaining: ~p", [vote_description(Data), Stake, Majority - Stake]),
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
handle_common_event({call,From}, {get_finalize_transaction, Trees, NonceOffset}, #data{result=Result, leader = Leader} = Data) ->
    case Leader of
        undefined ->
            {keep_state_and_data, [{reply, From, {error, not_ready}}]};
        _ ->
            case Result of
                undefined ->
                    {keep_state, Data#data{from=From, nonce_offset=NonceOffset}};
                _ ->
                    lager:debug("Replying quorum achieved for ~p commits, result ~p", [vote_description(Data), Result]),
                    {keep_state, Data, [{reply, From, convert_to_finalize_transaction(Result, Trees, NonceOffset, Data)}]}
            end
    end;
handle_common_event(_E, _Msg, #data{}) ->
    keep_state_and_data.

convert_to_finalize_transaction(Result, #data{nonce_offset = NonceOffset} = Data) ->
    case aec_chain:get_top_state() of
        {ok, Trees} ->
            convert_to_finalize_transaction(Result, Trees, NonceOffset, Data);
        Error ->
            {error, Error}
    end.

convert_to_finalize_transaction({ok, CallData}, Trees, NonceOffset, #data{leader=Leader, stakers=Stakers, epoch=Epoch}) ->
    case aec_consensus_hc:get_entropy_hash(Epoch + 2) of
        %% Don't create the transaction until the seed is available.
        {ok, _Seed} ->
            {ok, Tx} = aec_chain_hc:create_consensus_call_contract_transaction(Leader, Trees, aeser_api_encoder:encode(contract_bytearray, CallData), 0, NonceOffset),
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
convert_to_finalize_transaction(Result, _Trees, _NonceOffset, _Data) ->
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

reset_data(#data{stakers = Stakers, block_time=BlockTime, epoch = Epoch, parent_blocks = ParentBlocks, vote_types = VoteTypes, rev_vote_types = RevVoteTypes, module = Module, state = ClientState}) ->
    NewClientState = Module:reset_state(ClientState),
    #data{stakers = Stakers, block_time=BlockTime, parent_blocks=remove_old_blocks(Epoch, ParentBlocks), vote_types=VoteTypes, rev_vote_types = RevVoteTypes, module=Module, state=NewClientState}.

remove_old_blocks(undefined, ParentBlocks) ->
    ParentBlocks;
remove_old_blocks(Epoch, ParentBlocks) ->
    TargetEpoch = Epoch - 4,
    maps:filter(fun(E, _B) -> E > TargetEpoch end, ParentBlocks).

set_validators(Validators, Data) ->
    Majority = calculate_majority(Validators),
    lager:info("~p majority required ~p", [vote_description(Data), Majority]),
    Data#data{validators = Validators, majority = Majority, remaining_validators = maps:from_list(Validators)}.

get_staker_private_key(Staker, Stakers) ->
    maps:get(Staker, Stakers, undefined).

create_finalize_call(Votes, Proposal, #data{epoch = Epoch, seed=Seed, leader=Leader, parent_blocks = ParentBlocks, module = Module, state = ClientState}) ->
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
    VotesList = maps:fold(fun(Producer, Payload, Accum) -> create_vote_call(Module, Producer, Payload, Accum) end, [], Votes),
    {FunName, FunArgs} = Module:finalize_call(Proposal, ClientState),
    aeb_fate_abi:create_calldata(FunName, [Epoch|FunArgs] ++ [{bytes, Seed1}, {address, Leader}, VotesList]).

create_vote_call(Module, Producer, #{?SIGNATURE_FLD := Signature} = Payload, Accum) ->
    Params = Module:vote_params(Payload),
    Params1 = [{address, Producer}|Params] ++ [{bytes, get_sign_data(Payload)}, {bytes, Signature}],
    [{tuple, list_to_tuple(Params1)} | Accum].

vote_description(#data{module=Module}) ->
    Module:vote_description().

vote_type_description(TypeNum, #data{rev_vote_types = RevVoteTypes}) ->
    maps:get(TypeNum, RevVoteTypes, undefined).
