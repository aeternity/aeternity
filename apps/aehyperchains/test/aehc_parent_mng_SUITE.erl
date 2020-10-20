%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng_SUITE).
-author("sojourner").

-include_lib("common_test/include/ct.hrl").

-define(SIM_VIEW, <<"chain_sim">>).
-define(SIM_CONNECTOR, <<"aehc_chain_sim_connector">>).

-define(LOG(Fmt, Args), io:fwrite("~w:~w/~w - " ++ Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

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

-export([ post_block/1 ]).
%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

all() ->
    [{group, fetch}, {group, post}].

groups() ->
    [
        {fetch, [sequence], [fetch_block, fetch_height, fetch_fork]},
        {post, [], [post_block]}
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

    [{dir, Dir}|Config].

end_per_group(_, Config) ->
%%    aec_test_utils:aec_keys_cleanup(?config(dir, Config)),
    %% aehc_tracker related uninstall;
    ok = aec_test_utils:stop_chain_db().

init_per_testcase(_, Config) ->
    %% aehc_parent_mng related mocks;
    meck:new(aehc_utils, [passthrough]),
    meck:expect(aehc_utils, hc_enabled, 0, true),
    meck:new(aehc_app, [passthrough]),
    meck:expect(aehc_app, trackers_config, 0, trackers_conf(aec_block_genesis:genesis_header())),
    {ok, _} = aec_db_error_store:start_link(),
    {ok, Pid} = aehc_sup:start_link(), true = is_pid(Pid),
    [{ok, _} = aehc_parent_mng:start_view(aehc_app:tracker_name(Conf), Conf) || Conf <- aehc_app:trackers_config()],
    [{pid, Pid}|Config].

end_per_testcase(_, Config) ->
    %% aehc_parent_mng related mocks;
    meck:unload(aehc_app),
    exit(?config(pid, Config), normal),
    ok = aec_db_error_store:stop().

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

fetch_block(_Config) ->
    {ok, SimBlock} = aec_chain_sim:add_keyblock(),
    timer:sleep(1000),
    Hash = maps:get(hash, SimBlock),
    Block = aehc_parent_db:get_parent_block(Hash),
    true = (Hash == aehc_parent_block:hash_block(Block)),
    ok.

fetch_height(_Config) ->
    {ok, Start} = aec_chain_sim:add_keyblock(),
    Res = [aec_chain_sim:add_keyblock() || _ <- lists:seq(2, 100)],
    {ok, End} = lists:last(Res),
    timer:sleep(1000),
    Log = traverse(maps:get(hash, End), maps:get(hash, Start)),
    true = (length(Res) == length(Log)),
    ok.

fetch_fork(_Config) ->
    %% The main chain;
    MainRes = [aec_chain_sim:add_keyblock() || _ <- lists:seq(1, 80)],
    {ok, MainEnd} = lists:last(MainRes),
    %% Fork (Demo) chain;
    {ok, DemoStart} = aec_chain_sim:fork_from_hash(demo, maps:get(hash, MainEnd)),
    DemoRes = [aec_chain_sim:add_keyblock(demo) || _ <- lists:seq(1, 20)],
    {ok, DemoEnd} = lists:last(DemoRes),
    aec_chain_sim:fork_switch(demo),
    timer:sleep(1000),

    DemoLog = traverse(maps:get(hash, DemoEnd), maps:get(hash, DemoStart)),
    true = (length(DemoRes) == length(DemoLog)),
    ok.

post_block(_Config) ->
    Dir = aec_test_utils:aec_keys_setup(),

    {ok, PrivKey} = aec_keys:sign_privkey(),
    {ok, Pub} = aec_keys:pubkey(),
    Delegate = aeser_id:create(account, Pub),
    %% The main intention of this call is to emulate post action with signed payload from delegate;
    %% Fee, nonce, ttl and amount fields have decorated nature;
    KeyblockHash = aec_chain:top_key_block_hash(),
    PoGF = aehc_pogf:hash(no_pogf),
    Header = aehc_commitment_header:new(Delegate, KeyblockHash, PoGF),

    Payload = term_to_binary(aehc_commitment:new(Header), [compressed]),
    {ok, Tx} = aec_spend_tx:new(#{ sender_id => Delegate, recipient_id => Delegate, amount => 1,
        fee => 5, nonce => 1, payload => Payload, ttl => 0 }),
    BinaryTx = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    SignedTx = aetx_sign:new(Tx, [enacl:sign_detached(BinaryTx, PrivKey)]),
    TxHash = aetx_sign:hash(SignedTx),
    %% The next format is prepared accordingly to simualtor internal representation;
    aec_chain_sim:push(#{ tx_hash => TxHash, signed_tx  => SignedTx }),
    timer:sleep(1000),

    {ok, SimBlock} = aec_chain_sim:add_keyblock(),
    Block = aehc_parent_db:get_parent_block(maps:get(hash, SimBlock)),
    true = aehc_parent_block:is_hc_parent_block(Block),

    aec_test_utils:aec_keys_cleanup(Dir),
    ok.
%%%===================================================================
%%%  Configuration level
%%%===================================================================

trackers_conf(GenesisHeader) ->
    %% NOTE: Genesis header on the simulator is a dynamic entity, code performs preliminary init;
    {ok, GenesisHash} = aec_headers:hash_header(GenesisHeader),
    [
        #{
            <<"name">> => ?SIM_VIEW,
            <<"connector">> => #{
                <<"module">> => ?SIM_CONNECTOR,
                <<"args">> => #{
                    <<"genesis_header">> => GenesisHeader
                }
            },
            %% NOTE: Simulator operates via encoded hashes;
            <<"genesis_hash">> => GenesisHash,
            <<"note">> => <<"Hyperchains simulator based parent chain">>
        }].

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
