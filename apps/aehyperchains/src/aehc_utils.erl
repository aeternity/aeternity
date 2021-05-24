%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%
%%% @end
-module(aehc_utils).

-export([ hc_enabled/0
        , submit_commitment/2]).

-spec hc_enabled() -> boolean().
hc_enabled() ->
    Config = aec_consensus:get_consensus(),
    HC = [1 || {_, {aehc_consensus_hyperchains, _}} <- Config],
    HC /= [].

%% Submits a commitment transaction for the parent chain
%% Right now used for mocking :)
-spec submit_commitment(node(), binary()) -> aehc_parent_block:parent_block().
submit_commitment(KeyNode, Delegate) ->
    _C = aehc_commitment:new(aehc_commitment_header:new(Delegate, aec_block_insertion:node_hash(KeyNode)), no_pogf),
    error(todo).
