%%%=============================================================================
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%%   Unit tests for aec_consensus.erl
%%% @end
%%%=============================================================================
-module(aec_consensus_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

consensus_one() -> #{<<"name">> => <<"eunit_one">>}.
consensus_one(Config) -> #{<<"name">> => <<"eunit_one">>, <<"config">> => Config}.
consensus_two() -> #{<<"name">> => <<"eunit_two">>}.
consensus_two(Config) -> #{<<"name">> => <<"eunit_two">>, <<"config">> => Config}.
consensus_three() -> #{<<"name">> => <<"eunit_three">>}.
consensus_three(Config) -> #{<<"name">> => <<"eunit_three">>, <<"config">> => Config}.
consensus_unknown() -> #{<<"name">> => <<"consensus_unknown">>}.

setup_module(Name, CanBeTurnedOff) ->
    meck:expect(Name, can_be_turned_off, fun() -> CanBeTurnedOff end),
    meck:expect(Name, assert_config, fun(X) when map_size(X) =:= 0 -> ok end),
    meck:expect(Name, extra_from_header, fun(_) -> #{consensus => Name} end).

setup_module(Name, CanBeTurnedOff, Config) ->
    meck:expect(Name, can_be_turned_off, fun() -> CanBeTurnedOff end),
    meck:expect(Name, assert_config, fun(X) -> true = X =:= Config, ok end),
    meck:expect(Name, extra_from_header, fun(_) -> #{consensus => Name} end).

configuration_test_() ->
    {foreach,
        fun() ->
            meck:new(module_eunit_one, [non_strict]),
            meck:new(module_eunit_two, [non_strict]),
            meck:new(module_eunit_three, [non_strict]),
            ok
        end,
        fun(_) ->
            meck:unload(module_eunit_one),
            meck:unload(module_eunit_two),
            meck:unload(module_eunit_three),
            application:unset_env(aecore, consensus),
            aec_consensus:set_consensus()
        end,
        [ {"Unknown consensus", fun test_unknown_consensus/0}
        , {"The first consensus starts always at zero height", fun test_genesis_consensus/0}
        , {"Can pass user configuration to the consensus module", fun test_pass_config/0}
        , {"Can use the same consensus multiple times at different heights", fun test_same_consensus_different_height/0}
        , {"Consensus modules which can't be disabled", fun test_permanent_consensus/0}
        , {"Consensus config is persisted", fun test_persisted_config/0}
        , {"Query consensus at height", fun test_query_consensus/0}
        ]
    }.

test_unknown_consensus() ->
    setup_module(module_eunit_one, true),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_unknown()}),
    ?assertError(unknown_consensus, aec_consensus:check_env()),
    ok.

test_genesis_consensus() ->
    setup_module(module_eunit_one, true),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_one()}),
    ?assertEqual(ok, aec_consensus:check_env()),
    application:set_env(aecore, consensus, #{<<"1">> => consensus_one()}),
    ?assertError(first_consensus_does_not_activate_at_genesis, aec_consensus:check_env()),
    application:set_env(aecore, consensus, #{<<"-1">> => consensus_one()}),
    ?assertError(first_consensus_does_not_activate_at_genesis, aec_consensus:check_env()),
    ok.

test_pass_config() ->
    setup_module(module_eunit_one, true, #{}),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_one(#{})}),
    ?assertEqual(ok, aec_consensus:check_env()),

    Cfg = #{<<"Key">> => <<"Value">>},
    setup_module(module_eunit_one, true, Cfg),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_one(Cfg)}),
    ?assertEqual(ok, aec_consensus:check_env()),

    setup_module(module_eunit_one, true, a),
    setup_module(module_eunit_two, true, b),
    setup_module(module_eunit_three, true, c),
    application:set_env(aecore, consensus, #{ <<"0">> => consensus_one(a)
                                            , <<"1">> => consensus_two(b)
                                            , <<"2">> => consensus_three(c)
                                            }),
    ?assertEqual(ok, aec_consensus:check_env()),
    ok.

test_same_consensus_different_height() ->
    setup_module(module_eunit_one, true),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_one(), <<"1">> => consensus_one()}),
    ?assertEqual(ok, aec_consensus:check_env()),
    application:set_env(aecore, consensus, #{<<"1">> => consensus_one(), <<"0">> => consensus_one()}),
    ?assertEqual(ok, aec_consensus:check_env()),
    ok.

test_permanent_consensus() ->
    setup_module(module_eunit_one, false),
    setup_module(module_eunit_two, true),
    setup_module(module_eunit_three, true),

    application:set_env(aecore, consensus, #{<<"0">> => consensus_one()}),
    ?assertEqual(ok, aec_consensus:check_env()),

    application:set_env(aecore, consensus, #{<<"0">> => consensus_one(), <<"1">> => consensus_one()}),
    ?assertError(cannot_turn_off_consensus, aec_consensus:check_env()),

    application:set_env(aecore, consensus, #{<<"0">> => consensus_one(), <<"1">> => consensus_two()}),
    ?assertError(cannot_turn_off_consensus, aec_consensus:check_env()),

    application:set_env(aecore, consensus, #{<<"0">> => consensus_two(), <<"1">> => consensus_one()}),
    ?assertEqual(ok, aec_consensus:check_env()),

    application:set_env(aecore, consensus, #{ <<"0">> => consensus_two()
                                            , <<"1">> => consensus_one()
                                            }),
    ?assertEqual(ok, aec_consensus:check_env()),

    application:set_env(aecore, consensus, #{ <<"0">> => consensus_two()
                                            , <<"1">> => consensus_two()
                                            , <<"2">> => consensus_three()
                                            , <<"3">> => consensus_one()
                                            }),
    ?assertEqual(ok, aec_consensus:check_env()),

    application:set_env(aecore, consensus, #{ <<"0">> => consensus_two()
                                            , <<"1">> => consensus_two()
                                            , <<"2">> => consensus_one()
                                            , <<"3">> => consensus_three()
                                            }),
    ?assertError(cannot_turn_off_consensus, aec_consensus:check_env()),
    ok.

test_persisted_config() ->
    %% It's very dangerous to change the consensus config while the node is running
    %% Consensus changes need to be explicitly requested to be applied
    setup_module(module_eunit_one, true),
    setup_module(module_eunit_two, true),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_one()}),
    ?assertEqual(ok, aec_consensus:check_env()),
    aec_consensus:set_consensus(),
    ?assertEqual(module_eunit_one, aec_consensus:get_genesis_consensus_module()),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_two()}),
    ?assertEqual(ok, aec_consensus:check_env()),
    ?assertEqual(module_eunit_one, aec_consensus:get_genesis_consensus_module()),
    aec_consensus:set_consensus(),
    ?assertEqual(module_eunit_two, aec_consensus:get_genesis_consensus_module()),
    ok.

test_query_consensus() ->
    F = fun(Module, Config, Heights) ->
        [begin
            ?assertEqual(Module, aec_consensus:get_consensus_module_at_height(H)),
            ?assertEqual(Config, aec_consensus:get_consensus_config_at_height(H))
         end || H <- Heights]
    end,

    setup_module(module_eunit_one, true),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_one()}),
    ?assertEqual(ok, aec_consensus:check_env()),
    aec_consensus:set_consensus(),
    ?assertEqual(module_eunit_one, aec_consensus:get_genesis_consensus_module()),
    F(module_eunit_one, #{}, [0, 1, 10, 100, 1000]),

    setup_module(module_eunit_one, true, a),
    setup_module(module_eunit_two, true, b),
    setup_module(module_eunit_three, true, c),
    application:set_env(aecore, consensus, #{ <<"0">> => consensus_one(a)
                                            , <<"1">> => consensus_two(b)
                                            , <<"4">> => consensus_three(c)
                                            }),
    ?assertEqual(ok, aec_consensus:check_env()),
    aec_consensus:set_consensus(),
    ?assertEqual(module_eunit_one, aec_consensus:get_genesis_consensus_module()),
    F(module_eunit_one, a, [0]),
    F(module_eunit_two, b, [1,2,3]),
    F(module_eunit_three, c, [4,5,6,10,15,20,100,1000]),

    ok.

genesis_test_() ->
    {foreach,
        fun() ->
            meck:new(module_eunit_one, [non_strict]),
            meck:new(aec_fork_block_settings, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(aec_fork_block_settings),
            meck:unload(module_eunit_one),
            application:unset_env(aecore, consensus),
            aec_consensus:set_consensus()
        end,
        [ {"Can inject account to the genesis state", fun test_inject_account_at_genesis/0}]
    }.

test_inject_account_at_genesis() ->
    setup_module(module_eunit_one, true, a),
    application:set_env(aecore, consensus, #{<<"0">> => consensus_one(a)}),
    ?assertEqual(ok, aec_consensus:check_env()),
    aec_consensus:set_consensus(),

    [{PK1, _} = Account1, {PK2, _}] = generate_accounts(2),
    meck_genesis_accounts([Account1]),
    meck:expect(module_eunit_one, genesis_transform_trees, fun(Trees, a) -> Trees end),
    meck:expect(module_eunit_one, genesis_raw_header, fun() -> aec_consensus_common_tests:genesis_raw_header() end),
    meck:expect(module_eunit_one, assert_key_target_range, fun(T) -> aec_consensus_common_tests:assert_key_target_range(T) end),
    G1 = aec_consensus:get_genesis_hash(),
    [{PK1, _}] = aeu_mtrees:to_list(aec_trees:accounts(aec_block_genesis:populated_trees())),

    %% Try inserting another account at genesis
    meck:expect(module_eunit_one, genesis_transform_trees, fun(Trees, a) ->
                                                           aec_trees:grant_fee(PK2, Trees, 400)
                                                           end),
    [{PK2, _}, {PK1, _}] = aeu_mtrees:to_list(aec_trees:accounts(aec_block_genesis:populated_trees())),
    %% Changes to the genesis block need to be explicitly persisited
    G1 = aec_consensus:get_genesis_hash(),
    aec_consensus:set_genesis_hash(),
    G2 = aec_consensus:get_genesis_hash(),
    ?assertNotEqual(G1, G2),
    %% The genesis transform always operates on an initially populated state
    meck_genesis_accounts([]),
    G2 = aec_consensus:get_genesis_hash(),
    [{PK2, _}] = aeu_mtrees:to_list(aec_trees:accounts(aec_block_genesis:populated_trees())),
    aec_consensus:set_genesis_hash(),
    G3 = aec_consensus:get_genesis_hash(),
    ?assertNotEqual(G1, G3),
    ?assertNotEqual(G2, G3),
    ok.

meck_genesis_accounts(AccountsList) ->
    meck:expect(aec_fork_block_settings, genesis_accounts,
                fun() -> AccountsList end).

generate_accounts(Count) ->
    generate_accounts(Count, []).

generate_accounts(Count, Accum) when Count < 1 ->
    Accum;
generate_accounts(Count, Accum) ->
    Pubkey = <<Count:32/unit:8>>,
    generate_accounts(Count - 1 ,[{Pubkey, Count} | Accum]).

-endif.
