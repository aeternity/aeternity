%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng_SUITE).
-author("sojourner").

-include_lib("common_test/include/ct.hrl").

-define(CONNECTOR, aehc_chain_sim_connector).

%% Test server callbacks
-export([ suite/0
        , all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ init/1
        , fetch_block/1
        , fetch_height/1
        , fork_switch/1
        , accept_connector/1
        , terminate/1
        ]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,1}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) -> Config1
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before the suite.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> term()
%%
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) -> Config1
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case.
%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> term()
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_Case, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
    [ init, fetch_block, fetch_height, fork_switch, terminate].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

init(_Config) ->
    {ok, Pid} = aehc_parent_mng:start_link(), true = is_pid(Pid),
    ok.

fetch_block(_Config) ->
    [Block|_] = generate_chain(0, 1),
    aehc_parent_mng:publish_block(?CONNECTOR, Block),
    timer:sleep(10),
    Hash = aehc_parent_block:hash_block(Block),
    %% The extracted block from DB has TO be absolutely identical;
    Block = aehc_parent_db:get_parent_block(Hash),
    ok.

fetch_height(_Config) ->
    Chain = generate_chain(0, 1000),
    TopBlock = hd(Chain),
    GenesisBlock = lists:last(Chain),
    [aehc_parent_mng:publish_block(?CONNECTOR, Block)||Block <- Chain],
    TopBlockHash = aehc_parent_block:hash_block(TopBlock),
    GenesisBlockHash = aehc_parent_block:hash_block(GenesisBlock),
    %% The parent chain log from DB has to be absolutely identical;
    Chain = traverse(TopBlockHash, GenesisBlockHash),
    ok.

fork_switch(_Config) ->
    Chain = generate_chain(main, 0, 600),
    ForkedChain = generate_chain(fork, 500, 1000),
    TopBlock = hd(ForkedChain),
    GenesisBlock = lists:last(ForkedChain),
    [aehc_parent_mng:publish_block(?CONNECTOR, Block)||Block <- Chain],
    [aehc_parent_mng:publish_block(?CONNECTOR, Block)||Block <- ForkedChain],
    TopBlockHash = aehc_parent_block:hash_block(TopBlock),
    GenesisBlockHash = aehc_parent_block:hash_block(GenesisBlock),
    %% The parent chain log from DB has to be absolutely identical after fork switch has applied;
    ForkedChain = traverse(TopBlockHash, GenesisBlockHash),
    ok.

accept_connector(_Config) ->
    ok.

terminate(_Config) ->
    ok.

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
