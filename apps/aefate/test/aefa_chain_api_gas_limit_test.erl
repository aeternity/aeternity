%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Regression tests for the FATE GASLIMIT opcode (Chain.block_gas_limit).
%%%    Its value reaches the state root, so it follows the network's limit and
%%%    never one node's opinion of it: on a network in
%%%    ?FIXED_BLOCK_GAS_LIMIT_NETWORKS the operator's `aecore`
%%%    `block_gas_limit` knob moves nothing, and on a network that leaves the
%%%    limit to its own nodes the opcode and block admission move together.
%%%    Pinned: the historical 6,000,000, both sides of that test, and that
%%%    nothing between the opcode and aec_governance reads configuration on its
%%%    own account.
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_chain_api_gas_limit_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

-define(OWNER_PUBKEY,    <<16#B7:256>>).
-define(CONTRACT_PUBKEY, <<16#C7:256>>).
-define(CALLER_PUBKEY,   <<16#CF:256>>).

%% Written out rather than read from aec_governance, so the number is pinned
%% rather than restated: this is what every node has answered for all of
%% history, and moving it is a fork-gated decision that has to edit this line.
-define(HISTORICAL_BLOCK_GAS_LIMIT, 6000000).

%% Nothing derives this from aec_hard_forks:sorted_protocol_versions/0 - that
%% returns only the protocols the eunit VM's network id enables, so on a
%% single-protocol lane it would quietly shrink "every protocol version" to
%% one row and still pass.
-define(ALL_PROTOCOLS, [ ?ROMA_PROTOCOL_VSN
                       , ?MINERVA_PROTOCOL_VSN
                       , ?FORTUNA_PROTOCOL_VSN
                       , ?LIMA_PROTOCOL_VSN
                       , ?IRIS_PROTOCOL_VSN
                       , ?CERES_PROTOCOL_VSN
                       , ?ARCUS_PROTOCOL_VSN
                       , ?SALUS_PROTOCOL_VSN
                       ]).

%% Far from 6,000,000 and not a multiple of it: a wrong answer cannot look
%% like a rounding of the right one.
-define(OVERRIDE_BLOCK_GAS_LIMIT, 1234567).

%% The networks whose limit is the network's, not the operator's - the list
%% aec_governance tests at both read sites. Written out rather than read from
%% there: the macro is module-local, and a patch that shortens the list has to
%% edit this line rather than quietly take these cases with it.
-define(FIXED_NETWORK_IDS, [<<"ae_mainnet">>, <<"ae_uat">>]).

%% ae_dev is the node's own configurable network; the second stands in for a
%% hyperchain, whose ids this module cannot know.
-define(CONFIGURABLE_NETWORK_IDS, [<<"ae_dev">>, <<"nw_id_for_testing">>]).

%%%===================================================================
%%% (a) + (b) The opcode's value, end to end through aefa_fate_op
%%%===================================================================

gas_limit_opcode_test_() ->
    {foreach,
     fun() ->
             Saved = application:get_env(aecore, block_gas_limit),
             ok = aec_governance:clear_network_id_cache(),
             meck:new(aec_governance, [passthrough]),
             Saved
     end,
     fun(Saved) ->
             %% Cleared first: a throw from meck:unload/1 must not leave a
             %% pinned test id behind for the rest of the eunit VM.
             ok = aec_governance:clear_network_id_cache(),
             meck:unload(aec_governance),
             restore_env(block_gas_limit, Saved)
     end,
     [{"GASLIMIT is the historical value at every protocol version",
       fun opcode_is_historical_at_every_protocol/0},
      {"GASLIMIT does not move when the knob is set on a network that fixes it",
       fun opcode_is_immune_to_the_knob_on_a_fixed_network/0},
      {"GASLIMIT is the configured limit where the network leaves it open",
       fun opcode_follows_the_knob_on_a_configurable_network/0},
      {"The knob is real: it moves the opcode and block admission together",
       fun the_knob_moves_both_read_sites_together/0}]}.

%% Resolution is mocked - see resolve_network_id/0 in aec_governance. The
%% read-back is not decoration: the eunit VM is started with -network_id
%% local_<protocol>_testnet, which is a configurable id, so a mock that failed
%% to take would leave the fixed-network cases silently exercising the other
%% lane and passing for the wrong reason.
with_network_id(NetworkId, Fun) ->
    meck:expect(aec_governance, resolve_network_id, 0, NetworkId),
    ok = aec_governance:clear_network_id_cache(),
    ?assertEqual(NetworkId, aec_governance:get_network_id()),
    Fun().

opcode_is_historical_at_every_protocol() ->
    ok = application:unset_env(aecore, block_gas_limit),
    [ with_network_id(
        NetworkId,
        fun() ->
                [?assertEqual({NetworkId, Protocol,
                               aeb_fate_data:make_integer(?HISTORICAL_BLOCK_GAS_LIMIT)},
                              {NetworkId, Protocol, gaslimit_opcode(Protocol)})
                 || Protocol <- ?ALL_PROTOCOLS]
        end)
      || NetworkId <- ?FIXED_NETWORK_IDS ++ ?CONFIGURABLE_NETWORK_IDS ].

%% aec_governance:check_block_gas_limit/1 runs once at boot, so the override
%% here is the one a remote shell sets afterwards. Without the network test at
%% the read site the FATE opcode answers ?OVERRIDE_BLOCK_GAS_LIMIT on every row.
opcode_is_immune_to_the_knob_on_a_fixed_network() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = application:unset_env(aecore, block_gas_limit),
                Unset = [gaslimit_opcode(P) || P <- ?ALL_PROTOCOLS],
                ok = application:set_env(aecore, block_gas_limit,
                                         ?OVERRIDE_BLOCK_GAS_LIMIT),
                Overridden = [gaslimit_opcode(P) || P <- ?ALL_PROTOCOLS],
                ?assertEqual({NetworkId, lists:zip(?ALL_PROTOCOLS, Unset)},
                             {NetworkId, lists:zip(?ALL_PROTOCOLS, Overridden)}),
                ?assertEqual(aeb_fate_data:make_integer(?HISTORICAL_BLOCK_GAS_LIMIT),
                             hd(Overridden))
        end)
      || NetworkId <- ?FIXED_NETWORK_IDS ].

%% Where the limit is that deployment's own, a contract asking for it must be
%% told the number its own nodes admit blocks by. Before the read-site test the
%% FATE opcode answered ?HISTORICAL_BLOCK_GAS_LIMIT while the node ran on
%% ?OVERRIDE_BLOCK_GAS_LIMIT.
opcode_follows_the_knob_on_a_configurable_network() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = application:set_env(aecore, block_gas_limit,
                                         ?OVERRIDE_BLOCK_GAS_LIMIT),
                [?assertEqual({NetworkId, Protocol,
                               aeb_fate_data:make_integer(?OVERRIDE_BLOCK_GAS_LIMIT)},
                              {NetworkId, Protocol, gaslimit_opcode(Protocol)})
                 || Protocol <- ?ALL_PROTOCOLS]
        end)
      || NetworkId <- ?CONFIGURABLE_NETWORK_IDS ].

%% Control. Without it the two cases above would also pass on a node where the
%% knob had stopped working altogether, which is a different bug with the same
%% symptom - and would wrongly read as "the knob was removed". What it pins is
%% the property the opcode's safety now rests on: the number a contract is told
%% and the number the node admits blocks by are the same read.
the_knob_moves_both_read_sites_together() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = application:set_env(aecore, block_gas_limit,
                                         ?OVERRIDE_BLOCK_GAS_LIMIT),
                Admission = aec_governance:block_gas_limit(),
                ?assertEqual({NetworkId, aeb_fate_data:make_integer(Admission)},
                             {NetworkId, gaslimit_opcode(?CERES_PROTOCOL_VSN)})
        end)
      || NetworkId <- ?FIXED_NETWORK_IDS ++ ?CONFIGURABLE_NETWORK_IDS ].

%% Through the real opcode, not aefa_chain_api:gas_limit/1 directly: the
%% opcode is what a contract executes, and it is the wiring between the two
%% that this change touches.
gaslimit_opcode(Protocol) ->
    ES = engine_state(Protocol),
    ES1 = aefa_fate_op:gaslimit({stack, 0}, ES),
    aefa_engine_state:accumulator(ES1).

engine_state(Protocol) ->
    TxEnv = aetx_env:tx_env(_Height = 1, Protocol),
    ChainApi = aefa_chain_api:new(#{ gas_price => 1
                                   , fee       => 0
                                   , origin    => ?CALLER_PUBKEY
                                   , trees     => trees_with_one_contract()
                                   , tx_env    => TxEnv
                                   }),
    ES = aefa_engine_state:new(_Gas = 1000000, _Value = 0,
                               #{caller => ?CALLER_PUBKEY},
                               aefa_stores:new(), ChainApi, #{},
                               ?VM_FATE_SOPHIA_2),
    aefa_engine_state:set_current_contract(?CONTRACT_PUBKEY, ES).

trees_with_one_contract() ->
    CTVersion = #{vm => ?VM_FATE_SOPHIA_2, abi => ?ABI_FATE_SOPHIA_1},
    Contract0 = aect_contracts:new(?OWNER_PUBKEY, _Nonce = 1, CTVersion,
                                   _Code = <<"unused-in-this-test">>, _Deposit = 0),
    Contract1 = aect_contracts:set_pubkey(?CONTRACT_PUBKEY, Contract0),
    Account = aec_accounts:new(?CONTRACT_PUBKEY, 0),
    Trees0 = aec_trees:new_without_backend(),
    Trees1 = aec_trees:set_contracts(Trees0,
                aect_state_tree:insert_contract(Contract1, aec_trees:contracts(Trees0))),
    aec_trees:set_accounts(Trees1,
                aec_accounts_trees:enter(Account, aec_trees:accounts(Trees1))).

restore_env(Key, undefined) ->
    application:unset_env(aecore, Key);
restore_env(Key, {ok, Value}) ->
    application:set_env(aecore, Key, Value).

%%%===================================================================
%%% (c) Nothing between the opcode and aec_governance reads configuration on
%%% its own account. Walks the compiled call graph rather than setting one
%%% value, so it holds for every future edit too.
%%%
%%% This used to assert that NO configuration is reachable from the opcode at
%%% all. That is no longer the invariant and asserting it would be a lie: on a
%%% network that leaves the limit to its own nodes, the limit IS configuration,
%%% deliberately. aec_governance is therefore a leaf here - which side of
%%% ?FIXED_BLOCK_GAS_LIMIT_NETWORKS a network falls on is aec_governance_tests'
%%% subject, and both_arities_are_one_closure_test/0 there is where "the opcode
%%% cannot drift from block admission" is proven off the beams. What this
%%% module still owns is that the FATE path reaches the limit only through the
%%% consensus arity and adds no configuration of its own on the way.
%%%===================================================================

%% Reading configuration, as opposed to reading a constant. get_env is the
%% one that bit us; the neighbours are here because a "fix" that swaps
%% get_env for get_all_env would otherwise pass.
-define(CONFIG_READS, [ {application, get_env, 1}
                      , {application, get_env, 2}
                      , {application, get_env, 3}
                      , {application, get_all_env, 0}
                      , {application, get_all_env, 1}
                      , {aeu_env, user_config, 0}
                      , {aeu_env, user_config, 1}
                      , {aeu_env, user_config, 2}
                      , {aeu_env, user_config_or_env, 3}
                      , {aeu_env, user_config_or_env, 4}
                      , {aeu_env, user_map_or_env, 4}
                      ]).

%% A closure this small has room to grow a long way before the bound is a
%% real constraint; today it is 7. Exceeding it aborts rather than truncates.
-define(WALK_LIMIT, 500).

no_config_read_between_the_opcode_and_the_limit_test() ->
    Reached = reachable_from({aefa_chain_api, gas_limit, 1}),
    ?assertEqual([], [MFA || MFA <- Reached, lists:member(MFA, ?CONFIG_READS)]),
    %% The walk has to have walked. A closure that collapsed to nothing - an
    %% unreadable beam, a call form this module does not recognise - would
    %% satisfy the assertion above while proving nothing at all.
    ?assert(lists:member({aec_governance, block_gas_limit, 1}, Reached)),
    ?assert(lists:member({aetx_env, consensus_version, 1}, Reached)),
    %% The node-local arity is the same closure today, so taking it would give
    %% the same number - but it takes no protocol, and a repricing clause added
    %% above the catch-all would then reach every read site except this one.
    ?assertNot(lists:member({aec_governance, block_gas_limit, 0}, Reached)).

reachable_from(Root) ->
    walk([Root], sets:new(), 0).

walk([], Seen, _N) ->
    sets:to_list(Seen);
walk(_Pending, _Seen, N) when N > ?WALK_LIMIT ->
    erlang:error({call_graph_walk_limit_exceeded, ?WALK_LIMIT});
walk([MFA | Rest], Seen, N) ->
    case sets:is_element(MFA, Seen) of
        true  -> walk(Rest, Seen, N);
        false -> walk(callees(MFA) ++ Rest, sets:add_element(MFA, Seen), N + 1)
    end.

%% Leaves. They are recorded in the visited set (so the assertions above see
%% them) but not descended into: erlang is preloaded and has no beam to read,
%% application is the thing being looked for rather than a place to search,
%% and aec_governance is the boundary this walk stops at - see the section
%% comment above.
callees({erlang, _, _})        -> [];
callees({application, _, _})   -> [];
callees({aec_governance, _, _}) -> [];
callees({M, F, A}) ->
    case code:which(M) of
        Path when is_list(Path) ->
            {beam_file, _, _, _, _, Fs} = beam_disasm:file(Path),
            case [Code || {function, Fn, Ar, _, Code} <- Fs, Fn =:= F, Ar =:= A] of
                [Code] -> lists:flatmap(fun call_targets/1, Code);
                []     -> erlang:error({no_such_function, {M, F, A}})
            end;
        NoBeam ->
            erlang:error({cannot_disassemble, M, NoBeam})
    end.

call_targets({call, _, MFA})                         -> [MFA];
call_targets({call_only, _, MFA})                    -> [MFA];
call_targets({call_last, _, MFA, _})                 -> [MFA];
call_targets({call_ext, _, {extfunc, M, F, A}})      -> [{M, F, A}];
call_targets({call_ext_only, _, {extfunc, M, F, A}}) -> [{M, F, A}];
call_targets({call_ext_last, _, {extfunc, M, F, A}, _}) -> [{M, F, A}];
call_targets({make_fun2, MFA, _, _, _})              -> [MFA];
call_targets({make_fun3, MFA, _, _, _, _})           -> [MFA];
%% An indirect call is a hole in the proof, not something to walk past.
call_targets({apply, _})           -> erlang:error(dynamic_apply_in_call_graph);
call_targets({apply_last, _, _})   -> erlang:error(dynamic_apply_in_call_graph);
call_targets({call_fun, _})        -> erlang:error(dynamic_fun_call_in_call_graph);
call_targets({call_fun2, _, _, _}) -> erlang:error(dynamic_fun_call_in_call_graph);
call_targets(_)                    -> [].
