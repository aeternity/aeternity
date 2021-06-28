-module(aehc_utils).

-export([ hc_enabled/0
        , delegate/0
        , submit_commitment/2
        , delegates/1
        ]).

-include("../../aecore/include/blocks.hrl").
-include("aehc_utils.hrl").

-spec hc_enabled() -> boolean().
hc_enabled() ->
    Config = aec_consensus:get_consensus(),
    HC = [1 || {_, {aehc_consensus_hyperchains, _}} <- Config],
    HC /= [].

-spec submit_commitment(node(), binary()) -> aehc_parent_block:parent_block().
submit_commitment(KeyNode, Delegate) ->
    aec_events:subscribe(parent_top_changed),

    C = aehc_commitment:new(aehc_commitment_header:new(Delegate, aec_block_insertion:node_hash(KeyNode)), no_pogf),
    ok = aehc_parent_mng:commit(C),

    receive
        {gproc_ps_event, parent_top_changed, _Info} ->
            ok
    end,
    {_, ParentBlock} = aehc_parent_mng:pop(),

    ParentBlock.

-spec delegates(block_header_hash()) -> [commiter_pubkey()].
delegates(ParentHash) ->
    {ok, Block, Trees} = aehc_parent_mng:get_block_by_hash(ParentHash),

    Commitments = aehc_parent_block:commitments_in_block(Block),
    Accounts = [aehc_commitment_header:hc_delegate(aehc_commitment:header(X)) || X <- Commitments],

    [begin {value, Delegate} = aehc_delegates_trees:lookup(A, Trees), Delegate end|| A <- Accounts].

delegate() ->
    %% TODO The delegate address should be should  be distinguished via PoS configuration scope
    {ok, Delegate} = aeu_env:user_config_or_env([<<"mining">>, <<"beneficiary">>], aecore, beneficiary),
    Delegate.

%% NOTE: In order to start and the common sync procedure HC performs:
%% - fetching the parent chain data via network interface (connector);
%% - processing thq queue of parent chain blocks via acknowledgement interface
%% andalso begin aehc_utils:confirm_commitment(), pop() end,
%% aehc_utils:stake(),
