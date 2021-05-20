-module(aehc_full_node_SUITE).

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
-export([ simple_full_node/1
        ]).

-define(BIG_AMOUNT, 10000000000000000000000000000 * aec_test_utils:min_gas_price()).

all() ->
    [{group, full_node}].

groups() ->
    [
        {full_node, [sequence], [simple_full_node]}
    ].

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

simple_full_node(_Config) ->
    %% init_per_testcase?
    application:ensure_started(gproc),
    GenesisState = aec_block_genesis:genesis_block_with_state(),
    {ok, _} = aehc_chain_sim_connector:start_link(#{<<"genesis_state">> => GenesisState}),

    %% Start aehc_parent_mng process to receive commitments
    {ok, AehcSupPid} = aehc_sup:start_link(),
    true = is_pid(AehcSupPid),

    %% Setup some accounts
    {ok, #{privkey := Sk1, pubkey := Pk1}} = aec_chain_sim:new_account(?BIG_AMOUNT),
    Account1 = aeser_id:create(account, Pk1),

    %% use account's private key to sign
    meck:expect(aec_keys, sign_privkey, 0, {ok, Sk1}),

    %% dry-run
    ok = aehc_chain_sim_connector:dry_send_tx(Account1, <<"Test commitment">>, aehc_pogf:hash(no_pogf)),

    %% send_tx to submit commitment
    ok = aehc_chain_sim_connector:send_tx(Account1, <<"Test commitment">>, aehc_pogf:hash(no_pogf)),

    %% move tx from mempool to microblock on "parent chain"
    aec_chain_sim:add_microblock(),

    %% TODO: handle commitments in new microblock
    ok.
