%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").

-define(CONNECTOR, aehc_chain_sim_connector).

hyperchains_simulator_test_() ->
    {foreach,
        fun() ->
            application:ensure_started(gproc),
            ok = application:ensure_started(crypto),

            aec_test_utils:mock_genesis_and_forks(),
            Dir = aec_test_utils:aec_keys_setup(),
            aehc_parent_mng:start_link(),
            Dir
        end,
        fun(TmpDir) ->
            aec_test_utils:aec_keys_cleanup(TmpDir),
            aec_test_utils:unmock_genesis_and_forks()
        end,
        [{"Fetched block == Persisted block",
            fun() ->
                [Block|_] = generate_chain(0, 1),
                aehc_parent_mng:publish_block(?CONNECTOR, Block),
                timer:sleep(10),
                Hash = aehc_parent_block:hash_block(Block),
                %% The extracted block from DB has TO be absolutely identical;
                ?assertEqual(Block, aehc_parent_db:get_parent_block(Hash)),
                ok
            end},
            {"Fetched chain == Persisted chain",
                fun() ->
                    Chain = generate_chain(0, 1000),
                    TopBlock = hd(Chain),
                    GenesisBlock = lists:last(Chain),
                    [aehc_parent_mng:publish_block(?CONNECTOR, Block)||Block <- Chain],
                    TopBlockHash = aehc_parent_block:hash_block(TopBlock),
                    GenesisBlockHash = aehc_parent_block:hash_block(GenesisBlock),
                    %% The parent chain log from DB has to be absolutely identical;
                    ?assertEqual(Chain, traverse(TopBlockHash, GenesisBlockHash)),
                    ok
                end},
            {"Forked chain == Persisted chain",
                fun() ->
                    Chain = generate_chain(main, 0, 600),
                    ForkedChain = generate_chain(fork, 500, 1000),
                    TopBlock = hd(ForkedChain),
                    GenesisBlock = lists:last(ForkedChain),
                    [aehc_parent_mng:publish_block(?CONNECTOR, Block)||Block <- Chain],
                    [aehc_parent_mng:publish_block(?CONNECTOR, Block)||Block <- ForkedChain],
                    TopBlockHash = aehc_parent_block:hash_block(TopBlock),
                    GenesisBlockHash = aehc_parent_block:hash_block(GenesisBlock),
                    %% The parent chain log from DB has to be absolutely identical after fork switch has applied;
                    ?assertEqual(ForkedChain, traverse(TopBlockHash, GenesisBlockHash)),
                    ok
                end}
        ]}.

%%%===================================================================
%%%  parent chain demo generator
%%%===================================================================
%% NOTE: This generator is extremely simplified and source of entropy is based directly on Height param;
%% Now is the late night time (hope to agree some better solution with gorbak25 and radrow tomorrow);
generate_chain(From, To) ->
    generate_chain(main, From, To).

generate_chain(Fork, From, To) ->
    GenesisBlock = generate_block(Fork, From, generate_hash(From)),
    lists:foldl(
        fun (CurrentHeight, {Acc, Prev}) ->
            PrevHash = aehc_parent_block:prev_hash_block(Prev),
            Block = generate_block(Fork, CurrentHeight, PrevHash),
            [Block|Acc]
        end,
        {[], GenesisBlock},
        lists:seq(From + 1, To)
    ).

generate_block(Fork, Height, PrevHash) ->
    Header = aehc_parent_block:new_header(generate_hash([Fork, Height]), PrevHash, Height),
    %% TODO: To provide source of entropy for commitments data;
    Commitments = [],
    aehc_parent_block:new_block(Header, Commitments).

generate_hash(Input) ->
    aec_hash:hash(demo, term_to_binary(Input)).

%%%===================================================================
%%%  parent chain log traversing
%%%===================================================================
traverse(From, To) ->
    TopBlock = aehc_parent_db:get_parent_block(From),
    traverse(TopBlock, To, []).

traverse(Block, To, Acc) ->
    case aehc_parent_block:prev_hash_block(Block) of
        To ->
            lists:reverse([Block|Acc]);
        Prev ->
            PrevBlock = aehc_parent_db:get_parent_block(Prev),
            traverse(PrevBlock, To, [Block|Acc])
    end.
