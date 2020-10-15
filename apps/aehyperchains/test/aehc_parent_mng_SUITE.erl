%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng_SUITE).
-author("sojourner").

-include_lib("common_test/include/ct.hrl").

-define(VIEW, <<"chain_sim">>).

%% Test server callbacks
-export([ suite/0
        , all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ fetch_block/1
        , fetch_height/1
        , fork_switch/1
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
    %% aec_chain_sim related apps;
    application:ensure_started(gproc),
    ok = application:ensure_started(crypto),

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
init_per_testcase(_TestCase, Config) ->
    %% aehc_parent_mng related mocks;
    meck:new(aehc_app, [passthrough]),
    TrackersConf = [
        #{
            <<"name">> => <<"chain_sim">>,
            <<"connector">> => #{
                <<"module">> => <<"aehc_chain_sim_connector">>,
                <<"args">> => #{
                    <<"host">> => <<"localhost">>,
                    <<"port">> => 8332,
                    <<"stub">> => true}
            },
            %% Here is binary format to avoid decode -> encode overhead;
            <<"genesis_hash">> => <<137,206,99,218,17,5,58,206,158,154,48,45,
                104,202,228,107,195,195,17,77,208,56,184,
                52,164,245,229,245,163,141,193,230>>,
            <<"note">> => <<"Hyperchains simulator">>
        }],
    meck:expect(aehc_app, trackers_config, 0, TrackersConf),
    meck:new(aehc_utils, [passthrough]),
    meck:expect(aehc_utils, hc_enabled, 0, true),
    %% aec_chain_sim_ related mocks;
    aec_test_utils:mock_genesis_and_forks(),
    Dir = aec_test_utils:aec_keys_setup(),

    %% aehc_tracker related install;
    {ok, _} = aec_db_error_store:start_link(),
    aec_test_utils:start_chain_db(),
    aehc_db:create_tables(ram),
    Tabs = [Tab || {Tab, _} <- aehc_parent_db:table_specs(ram)],
    ok = mnesia:wait_for_tables(Tabs, 10000),

    {ok, Pid} = aehc_sup:start_link(), true = is_pid(Pid),
    [aehc_parent_mng:start_view(aehc_app:tracker_name(Conf), Conf) || Conf <- aehc_app:trackers_config()],

    [{dir, Dir}, {pid, Pid}|Config].
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
end_per_testcase(_TestCase, Config) ->
    %% aehc_parent_mng related mocks;
    meck:unload(aehc_app),
    meck:unload(aehc_utils),
    %% aec_chain_sim related mocks;
    aec_test_utils:unmock_genesis_and_forks(),
    aec_test_utils:aec_keys_cleanup(?config(dir, Config)),
    %% aehc_tracker related uninstall;
    aec_test_utils:stop_chain_db(),
    ok = aec_db_error_store:stop(),
    exit(?config(pid, Config), normal),
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
    [ fetch_block ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

fetch_block(_Config) ->
    Block = generate_block(main, 0, undefined),
    aehc_parent_mng:publish_block(?VIEW, Block),
    timer:sleep(100),
    Hash = aehc_parent_block:hash_block(Block),
    %% The extracted block from DB has TO be absolutely identical;
    Block = aehc_parent_db:get_parent_block(Hash),
    ok.

fetch_height(_Config) ->
    Chain = generate_chain(0, 10),
    TopBlock = hd(Chain),
    GenesisBlock = lists:last(Chain),
    [aehc_parent_mng:publish_block(?VIEW, Block)||Block <- Chain],
    TopBlockHash = aehc_parent_block:hash_block(TopBlock),
    GenesisBlockHash = aehc_parent_block:hash_block(GenesisBlock),
    %% The parent chain log from DB has to be absolutely identical;
    timer:sleep(100),
    Chain = traverse(TopBlockHash, GenesisBlockHash),
    ok.

fork_switch(_Config) ->
    Chain = generate_chain(main, 0, 600),
    ForkedChain = generate_chain(fork, 500, 1000),
    TopBlock = hd(ForkedChain),
    GenesisBlock = lists:last(ForkedChain),
    [aehc_parent_mng:publish_block(?VIEW, Block)||Block <- Chain],
    [aehc_parent_mng:publish_block(?VIEW, Block)||Block <- ForkedChain],
    TopBlockHash = aehc_parent_block:hash_block(TopBlock),
    GenesisBlockHash = aehc_parent_block:hash_block(GenesisBlock),
    %% The parent chain log from DB has to be absolutely identical after fork switch has applied;
    timer:sleep(100),
    ForkedChain = traverse(TopBlockHash, GenesisBlockHash),
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
    {Res, _} = lists:foldl(
        fun (CurrentHeight, {Acc, Prev}) ->
            PrevHash = aehc_parent_block:prev_hash_block(Prev),
            Block = generate_block(Fork, CurrentHeight, PrevHash),
            {[Block|Acc], Block}
        end,
        {[], GenesisBlock},
        lists:seq(From + 1, To)
    ),
    Res.

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
