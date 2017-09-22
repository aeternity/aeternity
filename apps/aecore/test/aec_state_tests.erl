-module(aec_state_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

-define(TEST_PUB, <<4,130,41,165,13,201,185,26,2,151,146,68,56,108,22,242,94,157,95,191,140,86,
                    145,96,71,82,28,176,23,5,128,17,245,174,170,199,54,248,167,43,185,12,108,91,
                    107,188,126,242,98,36,211,79,105,50,16,124,227,93,228,142,83,163,126,167,206>>).


all_test_() ->
    {foreach,
        fun() ->
            {ok, _} = aec_chain:start_link(aec_block_genesis:genesis_block_as_deserialized_from_network()),
            {ok, _} = aec_state:start_link(),
            ok
        end,
        fun(_) ->
            ok = aec_state:stop(),
            ok = aec_chain:stop()
        end,
        [fun(_) ->
            [{"Check force tree API ",
                fun() ->
                    {ok, {Height, Trees}} = aec_state:get_trees(),
                    ?assertEqual(0, Height),

                    {ok, EmptyTrees} = aec_trees:all_trees_new(),
                    ?assertEqual(Trees, EmptyTrees),

                    AccountTreeEmpty = aec_trees:accounts(EmptyTrees),
                    Account0 = aec_accounts:new(?TEST_PUB, 10, 1),
                    AccountTree0 = aec_accounts:put(Account0, AccountTreeEmpty),
                    Trees1 = aec_trees:set_accounts(EmptyTrees, AccountTree0),

                    {ok, {_HeightOut1, TreesOut1}} = aec_state:force_trees(Trees1, 1),
                    ?assertEqual(TreesOut1, Trees1),
                    {ok, {HeightOut2, TreesOut2}} = aec_state:get_trees(),
                    ?assertEqual(1, HeightOut2),
                    ?assertEqual(TreesOut2, Trees1)
                end}]
        end]}.
