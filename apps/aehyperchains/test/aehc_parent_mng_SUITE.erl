%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng_SUITE).
-author("sojourner").

-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SIM_VIEW, <<"chain_sim">>).
-define(SIM_CONNECTOR, <<"aehc_chain_sim_connector">>).

%% Test server callbacks
-export([ suite/0
        , all/0
        , groups/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ fetch_block/1
        , fetch_height/1
        , fetch_fork/1
        ]).

-export([ post_commitment/1 ]).
%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
all() ->
    [{group, fetch}, {group, post}].

groups() ->
    [
        {fetch, [sequence], [fetch_block, fetch_height, fetch_fork]},
        {post, [], [post_commitment]}
    ].

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    %% aec_chain_sim related apps;
    application:ensure_started(gproc),
    ok = application:ensure_started(crypto),
    meck:new(aehc_utils, [passthrough]),
    meck:expect(aehc_utils, hc_enabled, 0, true),
    %% aec_chain_sim_ related mocks;
    aec_test_utils:mock_genesis_and_forks(),
    Dir = aec_test_utils:aec_keys_setup(),
    %% aehc_tracker related install;
    aec_test_utils:start_chain_db(),
    aehc_db:create_tables(ram),
    Tabs = [Tab || {Tab, _} <- aehc_parent_db:table_specs(ram)],
    ok = mnesia:wait_for_tables(Tabs, 10000),

    GenesisState = aec_block_genesis:genesis_block_with_state(),
    [{dir, Dir}, {genesis_state, GenesisState}|Config].

end_per_group(_, _Config) ->
%%    aec_test_utils:aec_keys_cleanup(?config(dir, Config)),
    %% aehc_tracker related uninstall;
    ok = aec_test_utils:stop_chain_db().

init_per_testcase(_, Config) ->
    %% aehc_parent_mng related mocks;
    meck:new(aehc_utils, [passthrough]),
    meck:expect(aehc_utils, hc_enabled, 0, true),
    meck:new(aeu_env, [passthrough]),
    meck:expect(aeu_env, find_config, 2, setup(?config(genesis_state, Config))),
    {ok, _} = aec_db_error_store:start_link(),
    {ok, Pid} = aehc_parent_mng:start_link(), true = is_pid(Pid),
    ct:log("~naehc_parent_mng Pid is ~p~n",[Pid]),
    Config.

end_per_testcase(_, _Config) ->
    %% aehc_parent_mng related mocks;
    meck:unload(aehc_app),
    aehc_parent_mng:stop(),
    ok = aec_db_error_store:stop().

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

fetch_block(_Config) ->
    {ok, Block} = aec_chain_sim:add_microblock(),
    timer:sleep(1000),
    {ok, Hash} = aec_headers:hash_header(aec_blocks:to_header(Block)),
    ParentBlock = aehc_parent_db:get_parent_block(Hash),
    true = (Hash == aehc_parent_block:hash_block(ParentBlock)),
    ok.

fetch_height(Config) ->
    %% The synchronized height == produced by simulator;
    {GenesisBlock, _} = ?config(genesis_state, Config),
    {ok, GenesisHash} = aec_headers:hash_header(aec_blocks:to_header(GenesisBlock)),
    Res = [aec_chain_sim:add_microblock() || _ <- lists:seq(1, 5)],
    {ok, TopBlock} = aec_chain_sim:add_keyblock(),
    timer:sleep(1000),
    {ok, TopHash} = aec_headers:hash_header(aec_blocks:to_header(TopBlock)),
    Log = traverse(TopHash, GenesisHash),
    true = (length([TopBlock|Res]) == length(Log)),
    ok.

fetch_fork(Config) ->
    %% The main chain;
    {GenesisBlock, _} = ?config(genesis_state, Config),
    {ok, GenesisHash} = aec_headers:hash_header(aec_blocks:to_header(GenesisBlock)),
    [aec_chain_sim:add_microblock() || _ <- lists:seq(1, 5)],
    {ok, MainTop} = aec_chain_sim:add_keyblock(),
    {ok, ForkHash} = aec_headers:hash_header(aec_blocks:to_header(MainTop)),
    %% Fork (Demo) chain;
    {ok, _} = aec_chain_sim:fork_from_hash(demo, ForkHash),
    [aec_chain_sim:add_microblock(demo) || _ <- lists:seq(1, 3)],
    {ok, DemoEnd} = aec_chain_sim:add_keyblock(),
    aec_chain_sim:fork_switch(demo),
    timer:sleep(1000),

    {ok, EndHash} = aec_headers:hash_header(aec_blocks:to_header(DemoEnd)),
    DemoLog = traverse(EndHash, GenesisHash),
    GenesisHash = aehc_parent_block:prev_hash_block(lists:last(DemoLog)),
    ok.

post_commitment(_Config) ->
    Dir = aec_test_utils:aec_keys_setup(),
    Delegate = account(),
    %% The main intention of this call is to emulate post action with signed payload from delegate;
    %% Fee, nonce, ttl and amount fields have decorated nature;
    KeyblockHash = aec_chain:top_key_block_hash(),
    PoGF = aehc_pogf:hash(no_pogf),
    Header = aehc_commitment_header:new(Delegate, KeyblockHash, PoGF),
    Payload = term_to_binary(aehc_commitment:new(Header), [compressed]),
    {ok, Tx} = tx(Delegate, Payload),
    %% The next format is prepared accordingly to simualtor internal representation;
    aec_chain_sim:sign_and_push(Delegate, Tx),
    aec_chain_sim:add_keyblock(),

    aec_test_utils:aec_keys_cleanup(Dir),
    ok.

%%%===================================================================
%%%  Configuration level
%%%===================================================================
setup({Block, _} = State) ->
    %% NOTE: Genesis header on the simulator is a dynamic entity, code performs preliminary init;
    {ok, GenesisHash} = aec_headers:hash_header(aec_blocks:to_header(Block)),
    {ok, #{
        <<"mode">> => <<"monolith">>,
        <<"primary">> => #{
            <<"module">> => ?SIM_CONNECTOR,
            <<"args">> => #{
                <<"genesis_state">> => State
            },
            <<"address">> => GenesisHash
        }
    }}.

%%%===================================================================
%%%  parent chain log traversing
%%%===================================================================
traverse(From, To) ->
    TopBlock = aehc_parent_db:get_parent_block(From),
    traverse(TopBlock, To, []).

traverse(Block, To, Acc) ->
    Prev = aehc_parent_block:prev_hash_block(Block),
    case Prev of
        _ when Prev == To; Prev == <<"">> ->
            lists:reverse([Block|Acc]);
        _ ->
            PrevBlock = aehc_parent_db:get_parent_block(Prev),
            traverse(PrevBlock, To, [Block|Acc])
    end.

%%%===================================================================
%%%  Commitment modelling
%%%===================================================================
-spec account() -> aec_keys:pubkey().
account() ->
    Amount = round(math:pow(10, 10)) * aec_test_utils:min_gas_price(),
    {ok, #{pubkey := Res}} = aec_chain_sim:new_account(Amount * aec_test_utils:min_gas_price()),
    Res.

-spec tx(aec_keys:pubkey(), binary()) -> {ok, aetx:tx()}.
tx(Delegate, Payload) ->
    aec_spend_tx:new(
        #{
            sender_id => aeser_id:create(account, Delegate),
            recipient_id => aeser_id:create(account, Delegate),
            amount => 1,
            fee => 5,
            nonce => 1,
            payload => Payload,
            ttl => 0
        }).
