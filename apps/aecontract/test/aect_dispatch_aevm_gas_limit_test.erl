%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Regression tests for the AEVM GASLIMIT opcode (0x45), sibling of
%%%    aefa_chain_api_gas_limit_test. The value reaches the state root, so it
%%%    follows the network's limit and never one node's opinion of it: on a
%%%    network in ?FIXED_BLOCK_GAS_LIMIT_NETWORKS the operator's `aecore`
%%%    `block_gas_limit` knob moves nothing, and on a network that leaves the
%%%    limit to its own nodes the opcode and block admission move together.
%%%    Pinned: the historical 6,000,000, both sides of that test, and that the
%%%    dispatch site takes the consensus arity and reads no configuration of
%%%    its own.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_dispatch_aevm_gas_limit_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

-define(OWNER_PUBKEY,    <<16#B7:256>>).
-define(CONTRACT_PUBKEY, <<16#C7:256>>).
-define(CALLER_PUBKEY,   <<16#CF:256>>).

%% Written out rather than read from aec_governance, so the number is pinned
%% rather than restated. Moving it is a fork-gated decision that has to edit
%% this line.
-define(HISTORICAL_BLOCK_GAS_LIMIT, 6000000).

%% Far from 6,000,000 and not a multiple of it: a wrong answer cannot look
%% like a rounding of the right one.
-define(OVERRIDE_BLOCK_GAS_LIMIT, 1234567).

%% Nothing here derives the protocol list from
%% aec_hard_forks:sorted_protocol_versions/0 - that returns only the protocols
%% the eunit VM's network id enables, so on a single-protocol lane it would
%% quietly shrink "every protocol version" to one row and still pass.
-define(ALL_PROTOCOLS, [ ?ROMA_PROTOCOL_VSN
                       , ?MINERVA_PROTOCOL_VSN
                       , ?FORTUNA_PROTOCOL_VSN
                       , ?LIMA_PROTOCOL_VSN
                       , ?IRIS_PROTOCOL_VSN
                       , ?CERES_PROTOCOL_VSN
                       , ?ARCUS_PROTOCOL_VSN
                       , ?SALUS_PROTOCOL_VSN
                       ]).

-define(ALL_AEVM_VMS, [ ?VM_AEVM_SOPHIA_1
                      , ?VM_AEVM_SOPHIA_2
                      , ?VM_AEVM_SOPHIA_3
                      , ?VM_AEVM_SOPHIA_4
                      ]).

%% Written out AND cross-checked against is_legal_version_at_protocol/3 below.
%% Deriving it alone would let a narrowed guard shrink the matrix and still
%% pass; writing it alone would let it claim rows the node would refuse.
-define(LEGAL_CALL_MATRIX,
        [ {?ROMA_PROTOCOL_VSN,    [?VM_AEVM_SOPHIA_1]}
        , {?MINERVA_PROTOCOL_VSN, [?VM_AEVM_SOPHIA_1, ?VM_AEVM_SOPHIA_2]}
        , {?FORTUNA_PROTOCOL_VSN, [?VM_AEVM_SOPHIA_1, ?VM_AEVM_SOPHIA_2,
                                   ?VM_AEVM_SOPHIA_3]}
        , {?LIMA_PROTOCOL_VSN,    ?ALL_AEVM_VMS}
        , {?IRIS_PROTOCOL_VSN,    ?ALL_AEVM_VMS}
        , {?CERES_PROTOCOL_VSN,   ?ALL_AEVM_VMS}
        , {?ARCUS_PROTOCOL_VSN,   ?ALL_AEVM_VMS}
        , {?SALUS_PROTOCOL_VSN,   ?ALL_AEVM_VMS}
        ]).

%% GASLIMIT ; PUSH1 0 ; RETURN. On a Sophia VM the stack at RETURN is
%% [0, GasLimit, CalldataPtr] and RETURN takes the second word as a pointer;
%% for an unboxed word the pointer is the value, so nothing launders it.
-define(GASLIMIT_BYTE_CODE, <<16#45, 16#60, 16#00, 16#f3>>).

%%%===================================================================
%%% (a) + (b) The opcode's value, end to end through aect_dispatch:run/2
%%%===================================================================

%% The networks whose limit is the network's, not the operator's - the list
%% aec_governance tests at both read sites. Written out rather than read from
%% there: the macro is module-local, and a patch that shortens the list has to
%% edit this line rather than quietly take these cases with it.
-define(FIXED_NETWORK_IDS, [<<"ae_mainnet">>, <<"ae_uat">>]).

%% ae_dev is the node's own configurable network; the second stands in for a
%% hyperchain, whose ids this module cannot know.
-define(CONFIGURABLE_NETWORK_IDS, [<<"ae_dev">>, <<"nw_id_for_testing">>]).

aevm_gas_limit_opcode_test_() ->
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
     [{"The legal-call matrix agrees with aect_contracts",
       fun the_matrix_is_the_node_s_own/0},
      {"AEVM GASLIMIT is the historical value everywhere an AEVM call is legal",
       fun opcode_is_historical_on_the_whole_matrix/0},
      {"AEVM GASLIMIT does not move when the knob is set on a network that fixes it",
       fun opcode_is_immune_to_the_knob_on_a_fixed_network/0},
      {"AEVM GASLIMIT is the configured limit where the network leaves it open",
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

%% Guards the matrix itself. Without this a future narrowing of
%% is_legal_version_at_protocol_/3 - or a typo here - would leave the two
%% behavioural cases making a true statement about a smaller surface than the
%% one they name.
the_matrix_is_the_node_s_own() ->
    Derived = [{P, [VM || VM <- ?ALL_AEVM_VMS, is_legal_call(VM, P)]}
               || P <- ?ALL_PROTOCOLS],
    ?assertEqual(?LEGAL_CALL_MATRIX, Derived).

opcode_is_historical_on_the_whole_matrix() ->
    ok = application:unset_env(aecore, block_gas_limit),
    [ with_network_id(
        NetworkId,
        fun() ->
                [?assertEqual({NetworkId, P, VM, ?HISTORICAL_BLOCK_GAS_LIMIT},
                              {NetworkId, P, VM, gaslimit_opcode(P, VM)})
                 || {P, VM} <- pairs()]
        end)
      || NetworkId <- ?FIXED_NETWORK_IDS ++ ?CONFIGURABLE_NETWORK_IDS ].

%% The red witness. aec_governance:check_block_gas_limit/1 refuses to start a
%% node configured this way, but it runs once at boot - so the override here is
%% the one that arrives afterwards, from a remote shell. Without the network
%% test at the read site every row answers ?OVERRIDE_BLOCK_GAS_LIMIT.
opcode_is_immune_to_the_knob_on_a_fixed_network() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = application:unset_env(aecore, block_gas_limit),
                Unset = [{P, VM, gaslimit_opcode(P, VM)} || {P, VM} <- pairs()],
                ok = application:set_env(aecore, block_gas_limit,
                                         ?OVERRIDE_BLOCK_GAS_LIMIT),
                Overridden = [{P, VM, gaslimit_opcode(P, VM)} || {P, VM} <- pairs()],
                ?assertEqual({NetworkId, Unset}, {NetworkId, Overridden}),
                [?assertEqual({NetworkId, P, VM, ?HISTORICAL_BLOCK_GAS_LIMIT},
                              {NetworkId, P, VM, Limit})
                 || {P, VM, Limit} <- Overridden]
        end)
      || NetworkId <- ?FIXED_NETWORK_IDS ].

%% The other red witness. Where the limit is that deployment's own, a contract
%% asking for it must be told the number its own nodes admit blocks by. Before
%% the read-site test every row here answered ?HISTORICAL_BLOCK_GAS_LIMIT while
%% the node ran on ?OVERRIDE_BLOCK_GAS_LIMIT.
opcode_follows_the_knob_on_a_configurable_network() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = application:set_env(aecore, block_gas_limit,
                                         ?OVERRIDE_BLOCK_GAS_LIMIT),
                [?assertEqual({NetworkId, P, VM, ?OVERRIDE_BLOCK_GAS_LIMIT},
                              {NetworkId, P, VM, gaslimit_opcode(P, VM)})
                 || {P, VM} <- pairs()]
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
                [?assertEqual({NetworkId, P, VM, Admission},
                              {NetworkId, P, VM, gaslimit_opcode(P, VM)})
                 || {P, VM} <- pairs()]
        end)
      || NetworkId <- ?FIXED_NETWORK_IDS ++ ?CONFIGURABLE_NETWORK_IDS ].

pairs() ->
    [{P, VM} || {P, VMs} <- ?LEGAL_CALL_MATRIX, VM <- VMs].

is_legal_call(VM, Protocol) ->
    aect_contracts:is_legal_version_at_protocol(
      call, #{vm => VM, abi => ?ABI_AEVM_SOPHIA_1}, Protocol).

%%%===================================================================
%%% Driving the opcode through aect_dispatch:run/2, not aevm_eeevm:eval/1:
%%% the defect was in the Env run_common/2 builds, so a test building its own
%%% Env would supply the value it is checking.
%%%===================================================================

gaslimit_opcode(Protocol, VMVersion) ->
    {Call, _Trees, _Env} =
        aect_dispatch:run(#{vm => VMVersion, abi => ?ABI_AEVM_SOPHIA_1},
                          call_def(Protocol, VMVersion)),
    ok = aect_call:return_type(Call),
    {ok, GasLimit} = aeb_heap:from_binary(word, aect_call:return_value(Call)),
    GasLimit.

call_def(Protocol, VMVersion) ->
    {ok, CallData} = aeb_aevm_abi:create_calldata("gaslimit", [], [], word),
    CallerId = aeser_id:create(account, ?CALLER_PUBKEY),
    ContractId = aeser_id:create(contract, ?CONTRACT_PUBKEY),
    Nonce = 1,
    Height = 1,
    GasPrice = 1,
    #{ caller     => ?CALLER_PUBKEY
     , contract   => ?CONTRACT_PUBKEY
     , gas        => 1000000
     , fee        => 0
     , gas_price  => GasPrice
     , call_data  => CallData
     , amount     => 0
     , call_stack => []
       %% A map short-circuits aect_dispatch:maybe_deserialize_code/1, so the
       %% test does not have to serialise a contract just to have it taken
       %% apart again.
     , code       => #{ byte_code => ?GASLIMIT_BYTE_CODE
                      , type_info => [type_info()] }
     , store      => aect_contracts_store:new()
     , call       => aect_call:new(CallerId, Nonce, ContractId, Height, GasPrice)
     , trees      => trees_with_one_contract(VMVersion)
     , tx_env     => aetx_env:tx_env(Height, Protocol)
     , off_chain  => false
     , origin     => ?CALLER_PUBKEY
     , creator    => ?OWNER_PUBKEY
     , allow_init => false
     }.

%% Has to hash to the same function type hash aeb_aevm_abi:create_calldata/4
%% produces above, or aect_dispatch:run/2 answers unknown_function before it
%% ever reaches run_common/2.
type_info() ->
    aeb_aevm_abi:function_type_info(<<"gaslimit">>, _Payable = false, [], word).

trees_with_one_contract(VMVersion) ->
    CTVersion = #{vm => VMVersion, abi => ?ABI_AEVM_SOPHIA_1},
    Contract0 = aect_contracts:new(?OWNER_PUBKEY, _Nonce = 1, CTVersion,
                                   ?GASLIMIT_BYTE_CODE, _Deposit = 0),
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
%%% (c) The dispatch site's own reads, off the compiled beams: it takes the
%%% consensus arity and reads no configuration itself, and nothing below the
%%% Env map adds one back.
%%%
%%% There used to be a third part here asserting that block_gas_limit/1's own
%%% closure reads no configuration. That is no longer the invariant and
%%% asserting it would be a lie: on a network that leaves the limit to its own
%%% nodes, the limit IS configuration, deliberately. Which side of
%%% ?FIXED_BLOCK_GAS_LIMIT_NETWORKS a network falls on is aec_governance_tests'
%%% subject, and both_arities_are_one_closure_test/0 there is where "the opcode
%%% cannot drift from block admission" is now proven off the beams.
%%%===================================================================

%% Reading configuration, as opposed to reading a constant. get_env is the one
%% that bit us; the neighbours are here because a "fix" that swaps get_env for
%% get_all_env would otherwise pass.
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

%% A closure this small has room to grow a long way before the bound is a real
%% constraint. Exceeding it aborts rather than truncates.
-define(WALK_LIMIT, 500).

dispatch_site_reads_the_consensus_arity_test() ->
    Callees = direct_callees({aect_dispatch, run_common, 2}),
    ?assert(Callees =/= []),
    %% The defect itself, read off the compiled bytecode. Both AEVM and FATE
    %% clauses of run_common/2 are in this set, and neither may use the
    %% node-local arity.
    ?assertNot(lists:member({aec_governance, block_gas_limit, 0}, Callees)),
    ?assert(lists:member({aec_governance, block_gas_limit, 1}, Callees)),
    ?assert(lists:member({aetx_env, consensus_version, 1}, Callees)),
    %% ...and the number it puts in the Env map comes from that call and
    %% nowhere else: the dispatch site reads no configuration on its own
    %% account, whatever aec_governance goes on to decide.
    ?assertEqual([], [MFA || MFA <- Callees, lists:member(MFA, ?CONFIG_READS)]).

%% ...and nothing between the Env map and the opcode adds one back.
no_config_read_below_the_opcode_test() ->
    Reached = reachable_from({aevm_eeevm_state, gaslimit, 1}),
    ?assertEqual([], [MFA || MFA <- Reached, lists:member(MFA, ?CONFIG_READS)]),
    ?assert(lists:member({aevm_eeevm_state, gaslimit, 1}, Reached)).

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
%% them) but not descended into: erlang and maps are preloaded or have no beam
%% worth reading, and application is the thing being looked for, not a place
%% to search.
callees({erlang, _, _})      -> [];
callees({maps, _, _})        -> [];
callees({application, _, _}) -> [];
callees(MFA)                 -> disasm_callees(MFA, strict).

%% Two strictnesses on purpose: inside the walk an indirect call is a hole in
%% an absence proof, so it aborts; a direct-callee membership test can hide
%% nothing, and run_common/2's error branch emits lager parse-transform forms.
direct_callees(MFA) -> disasm_callees(MFA, tolerant).

disasm_callees({M, F, A}, Mode) ->
    case code:which(M) of
        Path when is_list(Path) ->
            {beam_file, _, _, _, _, Fs} = beam_disasm:file(Path),
            case [Code || {function, Fn, Ar, _, Code} <- Fs, Fn =:= F, Ar =:= A] of
                [Code] -> lists:flatmap(fun(I) -> call_targets(I, Mode) end, Code);
                []     -> erlang:error({no_such_function, {M, F, A}})
            end;
        NoBeam ->
            erlang:error({cannot_disassemble, M, NoBeam})
    end.

call_targets({call, _, MFA}, _)                            -> [MFA];
call_targets({call_only, _, MFA}, _)                       -> [MFA];
call_targets({call_last, _, MFA, _}, _)                    -> [MFA];
call_targets({call_ext, _, {extfunc, M, F, A}}, _)         -> [{M, F, A}];
call_targets({call_ext_only, _, {extfunc, M, F, A}}, _)    -> [{M, F, A}];
call_targets({call_ext_last, _, {extfunc, M, F, A}, _}, _) -> [{M, F, A}];
call_targets({make_fun2, MFA, _, _, _}, _)                 -> [MFA];
call_targets({make_fun3, MFA, _, _, _, _}, _)              -> [MFA];
%% An indirect call is a hole in the proof, not something to walk past.
call_targets({apply, _}, strict)           -> erlang:error(dynamic_apply_in_call_graph);
call_targets({apply_last, _, _}, strict)   -> erlang:error(dynamic_apply_in_call_graph);
call_targets({call_fun, _}, strict)        -> erlang:error(dynamic_fun_call_in_call_graph);
call_targets({call_fun2, _, _, _}, strict) -> erlang:error(dynamic_fun_call_in_call_graph);
call_targets(_, _)                         -> [].
