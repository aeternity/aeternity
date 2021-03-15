-module(aehc_utils).
-export([ hc_enabled/0
        , submit_commitment/2
        , confirm_commitment/0
        ]).

-spec hc_enabled() -> boolean().
hc_enabled() ->
    Config = aec_consensus:get_consensus(),
    HC = [1 || {_, {aehc_consensus_hyperchains, _}} <- Config],
    HC /= [].

-spec submit_commitment(node(), binary()) -> ok.
submit_commitment(KeyNode, Delegate) ->
    aec_events:subscribe(parent_top_changed),

    C = aehc_commitment:new(aehc_commitment_header:new(Delegate, aec_block_insertion:node_hash(KeyNode)), no_pogf),
    ok = aehc_parent_mng:commit(C).

-spec confirm_commitment() -> aehc_parent_block:parent_block().
confirm_commitment() ->
    receive
        {gproc_ps_event, parent_top_changed, _Info} ->
            ok
    end.


%% TODO To supply test helpers for Gregozh
