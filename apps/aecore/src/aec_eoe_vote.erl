%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_vote).
-behaviour(supervisor).

%% Export API functions
-export([start_link/2, negotiate/7, get_finalize_transaction/1, add_parent_block/2]).

%% Supervisor callbacks
-export([init/1]).


%% API to start the supervisor
-spec start_link(#{binary() => binary()}, non_neg_integer())  -> {ok, pid()} | {error, atom()}.
start_link(Stakers, BlockTime) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Stakers, BlockTime]).

%% Negotiate a fork, called with preferred fork and epoch length delta
-spec negotiate(non_neg_integer(), non_neg_integer(), binary(), aec_keys:pubkey(), [{binary(), non_neg_integer()}], binary(), non_neg_integer()) -> ok.
negotiate(Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength) ->
    call_children(fun(Module) -> Module:negotiate(Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength) end).

-spec add_parent_block(non_neg_integer(), aec_parent_chain_block:block()) -> ok.
add_parent_block(Epoch, ParentBlock) ->
    call_children(fun(Module) -> Module:add_parent_block(Epoch, ParentBlock) end).

-spec get_finalize_transaction(aec_trees:trees()) -> {ok, aetx_sign:signed_tx()} | {error, not_ready} | {error, term()}.
get_finalize_transaction(Trees) ->
    aec_eoe_fork_vote:get_finalize_transaction(Trees).

init(Args) ->
    ChildSpecs = [child_spec(aec_eoe_fork_vote, Args), child_spec(aec_eoe_length_vote, Args)],
    {ok, {{one_for_one, 3, 30}, ChildSpecs}}.

child_spec(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, 3000, worker, [Mod]}.

call_children(Function) ->
    Children = supervisor:which_children(?MODULE),
    lists:foreach(fun({_Id, _Pid, _Type, [Module]}) ->
        Function(Module) end, Children).