%%%
%%% Utility module for instantly mining blocks in tests and when dev mode is enabled
%%%

-module(aec_instant_mining_plugin).
-export([ emit_mb/0
        , emit_kb/0
        , instant_tx_confirm_hook/1
        , instant_tx_confirm_enabled/0 ]).

emit_mb() ->
    TopHash = aec_chain:top_block_hash(),
    {ok, MicroBlock, _} = aec_block_micro_candidate:create(TopHash),
    {ok, MicroBlockS} = aec_keys:sign_micro_block(MicroBlock),
    ok = aec_conductor:post_block(MicroBlockS),
    MicroBlockS.

emit_kb() ->
    TopHash = aec_chain:top_block_hash(),
    {ok, Beneficiary} = aec_conductor:get_beneficiary(),
    {ok, Block} = aec_block_key_candidate:create(TopHash, Beneficiary),
    ok = aec_conductor:add_synced_block(Block),
    Block.

%% Executed after a transaction was successfully inserted to the tx pool
%% This is a placeholder for dev mode
instant_tx_confirm_hook(_) ->
    ok.

instant_tx_confirm_enabled() ->
    true.
