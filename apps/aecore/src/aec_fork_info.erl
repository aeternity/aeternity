%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% Metadata about the given fork
%%% ForkId is local with regards to the node and for recently gossiped blocks
%%% nodes might disagree about the exact value - fork_id is eventually consistent
%%% across the network.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_fork_info).
-author("ra").

-export([new/1, new/4
        , decompose/1
        , difficulty/1
        , id/1, id/2
        , fees/1, fees/2
        , fraud/1, fraud/2
        , fees_fraud/1, fees_fraud/3
        , fees_fraud_difficulty/4]).

-export_type([fork_info/0]).

-type hash() :: aec_hash:hash().
-type difficulty() :: integer().
-type fees() :: non_neg_integer().

-record(fork_info, {
    fork_id :: hash()
    , difficulty = 0 :: difficulty()
    , fees = 0 :: fees()
    , fraud = false :: boolean()
}).
-opaque fork_info() :: #fork_info{}.


%% API

-spec new(hash()) -> fork_info().
new(ForkId) -> #fork_info{fork_id = ForkId}.

-spec new(hash(), difficulty(), fees(), boolean()) -> fork_info().
new(ForkId, Difficulty, Fees, Fraud) ->
    #fork_info
    {
        fork_id = ForkId,
        difficulty = Difficulty,
        fees = Fees,
        fraud = Fraud
    }.

-spec decompose(fork_info()) -> {hash(), difficulty(), fees(), boolean()}.
decompose(ForkInfo) ->
    #fork_info
    {
        fork_id = Id,
        difficulty = Difficulty,
        fees = Fees,
        fraud = Fraud
    } = ForkInfo,
    {Id, Difficulty, Fees, Fraud}.

-spec difficulty(fork_info()) -> difficulty().
difficulty(#fork_info{difficulty = Difficulty}) -> Difficulty.

-spec id(fork_info()) -> hash().
id(#fork_info{fork_id = Id}) -> Id.

-spec id(fork_info(), hash()) -> fork_info().
id(ForkInfo, Id) -> ForkInfo#fork_info{fork_id = Id}.

-spec fees(fork_info()) -> fees().
fees(#fork_info{fees = Fees}) -> Fees.

-spec fees(fork_info(), fees()) -> fork_info().
fees(ForkInfo, Fees) -> ForkInfo#fork_info{fees = Fees}.

-spec fraud(fork_info()) -> boolean().
fraud(#fork_info{fraud = Fraud}) -> Fraud.

-spec fraud(fork_info(), boolean()) -> fork_info().
fraud(ForkInfo, Fraud) -> ForkInfo#fork_info{fraud = Fraud}.

-spec fees_fraud(fork_info()) -> {fees(), boolean()}.
fees_fraud(#fork_info{fees = Fees, fraud = Fraud}) -> {Fees, Fraud}.

-spec fees_fraud(fork_info(), fees(), boolean()) -> fork_info().
fees_fraud(ForkInfo, Fees, Fraud) ->
    ForkInfo#fork_info{fees = Fees, fraud = Fraud}.

-spec fees_fraud_difficulty(fork_info(), fees(), boolean(), difficulty()) ->
    fork_info().
fees_fraud_difficulty(ForkInfo, Fees, Fraud, Difficulty) ->
    ForkInfo#fork_info{fees = Fees, fraud = Fraud, difficulty = Difficulty}.
