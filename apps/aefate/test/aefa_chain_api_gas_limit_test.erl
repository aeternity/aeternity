%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Regression tests for the FATE GASLIMIT opcode (Chain.block_gas_limit).
%%%    Its value reaches the state root, so it may not follow the operator's
%%%    `aecore` `block_gas_limit` knob. Pinned: the historical 6,000,000, the
%%%    knob, and that no path from the opcode reaches application:get_env.
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

%%%===================================================================
%%% (a) + (b) The opcode's value, end to end through aefa_fate_op
%%%===================================================================

gas_limit_opcode_test_() ->
    {foreach,
     fun() -> application:get_env(aecore, block_gas_limit) end,
     fun(Saved) -> restore_env(block_gas_limit, Saved) end,
     [{"GASLIMIT is the historical value at every protocol version",
       fun opcode_is_historical_at_every_protocol/0},
      {"GASLIMIT does not move when the aecore knob is set",
       fun opcode_is_immune_to_the_knob/0},
      {"The knob is real: it still moves block admission",
       fun the_knob_still_moves_block_admission/0}]}.

opcode_is_historical_at_every_protocol() ->
    ok = application:unset_env(aecore, block_gas_limit),
    [?assertEqual({Protocol, aeb_fate_data:make_integer(?HISTORICAL_BLOCK_GAS_LIMIT)},
                  {Protocol, gaslimit_opcode(Protocol)})
     || Protocol <- ?ALL_PROTOCOLS].

%% The red witness. On the unpatched tree every row here answers
%% ?OVERRIDE_BLOCK_GAS_LIMIT.
opcode_is_immune_to_the_knob() ->
    ok = application:unset_env(aecore, block_gas_limit),
    Unset = [gaslimit_opcode(P) || P <- ?ALL_PROTOCOLS],
    ok = application:set_env(aecore, block_gas_limit, ?OVERRIDE_BLOCK_GAS_LIMIT),
    Overridden = [gaslimit_opcode(P) || P <- ?ALL_PROTOCOLS],
    ?assertEqual(lists:zip(?ALL_PROTOCOLS, Unset),
                 lists:zip(?ALL_PROTOCOLS, Overridden)).

%% Control. Without it the two tests above would also pass on a node where
%% the knob had stopped working altogether, which is a different bug with the
%% same symptom - and would wrongly read as "the knob was removed".
the_knob_still_moves_block_admission() ->
    ok = application:set_env(aecore, block_gas_limit, ?OVERRIDE_BLOCK_GAS_LIMIT),
    ?assertEqual(?OVERRIDE_BLOCK_GAS_LIMIT, aec_governance:block_gas_limit()),
    ?assertNotEqual(aec_governance:block_gas_limit(),
                    aec_governance:block_gas_limit(?CERES_PROTOCOL_VSN)).

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
%%% (c) No configuration read on any path. Walks the compiled call graph
%%% rather than setting one value, so it holds for every future edit too.
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

no_config_read_on_any_path_test() ->
    Reached = reachable_from({aefa_chain_api, gas_limit, 1}),
    ?assertEqual([], [MFA || MFA <- Reached, lists:member(MFA, ?CONFIG_READS)]),
    %% The walk has to have walked. A closure that collapsed to nothing - an
    %% unreadable beam, a call form this module does not recognise - would
    %% satisfy the assertion above while proving nothing at all.
    ?assert(lists:member({aec_governance, block_gas_limit, 1}, Reached)),
    ?assert(lists:member({aetx_env, consensus_version, 1}, Reached)).

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

%% Leaves. They are recorded in the visited set (so the assertion above sees
%% them) but not descended into: erlang is preloaded and has no beam to read,
%% and application is the thing being looked for, not a place to search.
callees({erlang, _, _})     -> [];
callees({application, _, _}) -> [];
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
