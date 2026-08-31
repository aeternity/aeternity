%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    The node-side per-protocol price table for FATE instructions and the
%%%    path it reaches. Two things are proved: that the table is inert as
%%%    shipped, generating a line-for-line identical dispatch; and that it
%%%    works, a hypothetical entry charging the new price from v8 on only.
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_fate_op_gas_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

-define(CALLER_PUBKEY, <<16#CA:256>>).
-define(START_GAS, 1000000).

%% The hypothetical repricing driven through the whole path below. NOP costs 1
%% gas in the base table and does nothing at all when executed; make it cost
%% this from Salus (v8) on. The figure is one no instruction charges, so a
%% probe reading it back cannot be reading something else.
-define(PROBE_OP, 'NOP').
-define(PROBE_BASE_GAS, 1).
-define(PROBE_NEW_GAS, 12345).
-define(PROBE_MODULE, aefa_fate_eval_op_gas_probe).

%%%===================================================================
%%% Shipped inert
%%%===================================================================

overrides_are_empty_test() ->
    ?assertEqual([], aefa_fate_op_gas:overrides()).

%% The load-bearing one for "this changes nothing": with an empty table
%% gas_term/2 hands back the base table's own term, unchanged and not merely
%% equal, for every instruction there is.
generated_prices_are_unchanged_test() ->
    Ops = aeb_fate_generate_ops:get_ops(),
    ?assert(length(Ops) > 100),
    [ ?assert(aefa_fate_op_gas:gas_term(OpName, Gas) =:= Gas)
      || #{opname := OpName, gas := Gas} <- Ops ].

%%%===================================================================
%%% Schedule construction
%%%===================================================================

%% aefa_fate_op_gas spells the lowest protocol as a literal rather than
%% including hard_forks.hrl, because aefa_gen_dispatch compiles it before the
%% umbrella's other applications are on the code path. This is the pin.
lowest_protocol_vsn_matches_hard_forks_test() ->
    ?assertEqual([{?ROMA_PROTOCOL_VSN, 10}], aefa_fate_op_gas:schedule(10)).

schedule_of_a_schedule_is_itself_test() ->
    Iris = [{?IRIS_PROTOCOL_VSN, 5000}, {?LIMA_PROTOCOL_VSN, 100}],
    ?assertEqual(Iris, aefa_fate_op_gas:schedule(Iris)).

%% A flat price gains a protocol dimension; the old price keeps every protocol
%% below the new one.
override_on_a_flat_price_test() ->
    ?assertEqual([{?SALUS_PROTOCOL_VSN, 12345}, {?ROMA_PROTOCOL_VSN, 10}],
                 aefa_fate_op_gas:gas_term('RETURN', 10,
                                           [{'RETURN', [{?SALUS_PROTOCOL_VSN, 12345}]}])).

%% An instruction the base table already prices per protocol -- ?GAS_IRIS(A, B)
%% -- keeps both of its existing steps underneath the new one.
override_on_an_existing_schedule_test() ->
    Base = [{?IRIS_PROTOCOL_VSN, 5000}, {?LIMA_PROTOCOL_VSN, 100}],
    ?assertEqual([{?SALUS_PROTOCOL_VSN, 7000},
                  {?IRIS_PROTOCOL_VSN, 5000},
                  {?LIMA_PROTOCOL_VSN, 100}],
                 aefa_fate_op_gas:gas_term('SPEND', Base,
                                           [{'SPEND', [{?SALUS_PROTOCOL_VSN, 7000}]}])).

%% Several protocols at once, as long as they descend.
override_with_two_steps_test() ->
    Repriced = [{?SALUS_PROTOCOL_VSN, 30}, {?ARCUS_PROTOCOL_VSN, 20}],
    ?assertEqual(Repriced ++ [{?ROMA_PROTOCOL_VSN, 10}],
                 aefa_fate_op_gas:gas_term('RETURN', 10, [{'RETURN', Repriced}])).

%% An instruction with no entry is untouched even when the table is non-empty.
override_touches_only_the_named_instruction_test() ->
    Table = [{'NOP', [{?SALUS_PROTOCOL_VSN, 99}]}],
    ?assert(aefa_fate_op_gas:gas_term('RETURN', 10, Table) =:= 10).

%%%===================================================================
%%% What the table refuses
%%%===================================================================

%% Restating the price of a protocol the base table already prices would change
%% what already-forked blocks charged. That is the whole reason the table
%% exists, so it is refused rather than merged.
refuses_to_reprice_an_already_priced_protocol_test() ->
    Base = [{?IRIS_PROTOCOL_VSN, 5000}, {?LIMA_PROTOCOL_VSN, 100}],
    ?assertError({gas_override_not_forward_only, 'SPEND', _, _},
                 aefa_fate_op_gas:gas_term('SPEND', Base,
                                           [{'SPEND', [{?IRIS_PROTOCOL_VSN, 7000}]}])).

%% Even against a flat base: a flat price covers every protocol from Roma on.
refuses_to_reprice_roma_test() ->
    ?assertError({gas_override_not_forward_only, 'RETURN', _, _},
                 aefa_fate_op_gas:gas_term('RETURN', 10,
                                           [{'RETURN', [{?ROMA_PROTOCOL_VSN, 12}]}])).

%% Arcus is the first protocol with no activation height on any network. Ceres
%% clears the forward-only check against a Lima/Iris base -- it is above both --
%% and is refused anyway, because Ceres is what mainnet is running.
refuses_to_reprice_an_activated_protocol_test() ->
    Base = [{?IRIS_PROTOCOL_VSN, 5000}, {?LIMA_PROTOCOL_VSN, 100}],
    ?assertError({gas_override_below_first_repriceable_protocol, 'SPEND', [?CERES_PROTOCOL_VSN], _},
                 aefa_fate_op_gas:gas_term('SPEND', Base,
                                           [{'SPEND', [{?CERES_PROTOCOL_VSN, 7000}]}])).

%% The floor is a hand-maintained constant that has to be raised the release a
%% protocol is scheduled. This is what fails when someone forgets.
first_repriceable_protocol_is_the_first_unscheduled_one_test() ->
    Floor = aefa_fate_op_gas:first_repriceable_protocol(),
    ?assertEqual(?ARCUS_PROTOCOL_VSN, Floor),
    %% Nothing at or above the floor may carry an activation height on a real
    %% network -- having none is exactly what makes a protocol repriceable.
    [ ?assertEqual({Net, []},
                   {Net, [ P || P <- maps:keys(aec_hard_forks:protocols_from_network_id(Net)),
                                P >= Floor ]})
      || Net <- [<<"ae_mainnet">>, <<"ae_uat">>] ].

%% aefa_engine_state:get_gas/2 walks a schedule top-down and stops at the first
%% entry the consensus version reaches, so an ascending one silently misprices.
refuses_an_ascending_schedule_test() ->
    Ascending = [{?ARCUS_PROTOCOL_VSN, 20}, {?SALUS_PROTOCOL_VSN, 30}],
    ?assertError({bad_gas_override, 'RETURN', _},
                 aefa_fate_op_gas:gas_term('RETURN', 10, [{'RETURN', Ascending}])).

%%%===================================================================
%%% The consuming half: aefa_engine_state:spend_gas/2 against a real engine
%%%===================================================================

%% The merged schedule charged through the production spend_gas/2, at every
%% protocol from Lima up. New price from Salus on; everything below unmoved.
spend_gas_resolves_the_schedule_per_protocol_test() ->
    Base = [{?IRIS_PROTOCOL_VSN, 5000}, {?LIMA_PROTOCOL_VSN, 100}],
    Term = aefa_fate_op_gas:gas_term('SPEND', Base,
                                     [{'SPEND', [{?SALUS_PROTOCOL_VSN, 7000}]}]),
    Protocols = [?LIMA_PROTOCOL_VSN, ?IRIS_PROTOCOL_VSN, ?CERES_PROTOCOL_VSN,
                 ?ARCUS_PROTOCOL_VSN, ?SALUS_PROTOCOL_VSN],
    ?assertEqual([{?LIMA_PROTOCOL_VSN,   100},
                  {?IRIS_PROTOCOL_VSN,  5000},
                  {?CERES_PROTOCOL_VSN, 5000},
                  {?ARCUS_PROTOCOL_VSN, 5000},
                  {?SALUS_PROTOCOL_VSN, 7000}],
                 [ {P, charge(P, Term)} || P <- Protocols ]),
    %% And below Salus the merged schedule is indistinguishable from the base.
    [ ?assertEqual(charge(P, Base), charge(P, Term))
      || P <- Protocols -- [?SALUS_PROTOCOL_VSN] ].

%%%===================================================================
%%% End to end: nothing above proves aefa_gen_dispatch consults the table.
%%% These run it twice, diff what it wrote, then compile and execute it.
%%%===================================================================

generated_dispatch_test_() ->
    {setup,
     fun generate_dispatches/0,
     fun cleanup_probe/1,
     fun(Ctx) ->
        [ {"The shipped table changes not one line of the generated dispatch",
           fun() -> shipped_table_is_inert(Ctx) end}
        , {"An entry rewrites exactly the instruction it names",
           fun() -> entry_rewrites_one_instruction(Ctx) end}
        , {"The repriced instruction charges the new price from v8 on, and the "
           "old price at v7 and below",
           fun probe_charges_per_protocol/0}
        ]
     end}.

%% The generator is run against an unmodified aebytecode op table with the real
%% aefa_fate_op_gas, and its output compared with the generator as it was before
%% the table existed -- reproduced here by generating with a table that returns
%% the base price for everything.
shipped_table_is_inert(#{shipped := Shipped, pristine := Pristine}) ->
    ?assertEqual(Pristine, Shipped).

entry_rewrites_one_instruction(#{shipped := Shipped, probe := Probe}) ->
    ShippedLines = binary:split(Shipped, <<"\n">>, [global]),
    ProbeLines   = binary:split(Probe,   <<"\n">>, [global]),
    ?assertEqual(length(ShippedLines), length(ProbeLines)),
    Diffs = [ {S, P} || {S, P} <- lists:zip(ShippedLines, ProbeLines), S =/= P ],
    ?assertEqual(1, length(Diffs)),
    [{ShippedLine, ProbeLine}] = Diffs,
    ?assertNotEqual(nomatch, binary:match(ShippedLine, <<"aefa_fate_op:nop(">>)),
    ?assertNotEqual(nomatch, binary:match(ShippedLine, <<"spend_gas(1, EngineState)">>)),
    ?assertNotEqual(nomatch, binary:match(ProbeLine, <<"aefa_fate_op:nop(">>)),
    ?assertNotEqual(
       nomatch,
       binary:match(ProbeLine,
                    list_to_binary(io_lib:format("spend_gas(~w, EngineState)",
                                                 [[{?SALUS_PROTOCOL_VSN, ?PROBE_NEW_GAS},
                                                   {?ROMA_PROTOCOL_VSN, ?PROBE_BASE_GAS}]])))).

probe_charges_per_protocol() ->
    ?assertEqual(?PROBE_NEW_GAS,  eval_probe_op(?SALUS_PROTOCOL_VSN)),
    ?assertEqual(?PROBE_BASE_GAS, eval_probe_op(?ARCUS_PROTOCOL_VSN)),
    ?assertEqual(?PROBE_BASE_GAS, eval_probe_op(?CERES_PROTOCOL_VSN)),
    ?assertEqual(?PROBE_BASE_GAS, eval_probe_op(?IRIS_PROTOCOL_VSN)),
    ?assertEqual(?PROBE_BASE_GAS, eval_probe_op(?LIMA_PROTOCOL_VSN)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Gas actually deducted by spend_gas/2 for Term at consensus version Protocol.
charge(Protocol, Term) ->
    ES = fresh_engine_state(Protocol),
    ?START_GAS - aefa_engine_state:gas(aefa_engine_state:spend_gas(Term, ES)).

%% Gas actually deducted by the generated dispatch for the repriced instruction.
eval_probe_op(Protocol) ->
    ES = fresh_engine_state(Protocol),
    {next, ES1} = ?PROBE_MODULE:eval(?PROBE_OP, ES),
    ?START_GAS - aefa_engine_state:gas(ES1).

fresh_engine_state(Protocol) ->
    TxEnv = aetx_env:tx_env(_Height = 1, Protocol),
    ChainApi = aefa_chain_api:new(#{ gas_price => 1
                                   , fee       => 0
                                   , origin    => ?CALLER_PUBKEY
                                   , trees     => aec_trees:new_without_backend()
                                   , tx_env    => TxEnv
                                   }),
    ES = aefa_engine_state:new(?START_GAS, _Value = 0, #{caller => ?CALLER_PUBKEY},
                               aefa_stores:new(), ChainApi, #{}, ?VM_FATE_SOPHIA_2),
    %% Sanity: the env must resolve to the protocol this measurement claims.
    Protocol = aefa_engine_state:consensus_version(ES),
    ES.

%% Three runs of the real priv/aefa_gen_dispatch over the real aebytecode op
%% table: the shipped table (default argument), a table that overrides nothing
%% at all, and a table that reprices one instruction. The probe run is loaded
%% under its own module name so it can be executed.
generate_dispatches() ->
    Dir = probe_dir(),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Shipped  = generate(Dir, "shipped",  default),
    Pristine = generate(Dir, "pristine", pristine_gas_module_source()),
    Probe0   = generate(Dir, "probe",    probe_gas_module_source()),
    ProbeSrc = filename:join(Dir, atom_to_list(?PROBE_MODULE) ++ ".erl"),
    ok = file:write_file(
           ProbeSrc,
           re:replace(Probe0, "-module\\(aefa_fate_eval\\)",
                      "-module(" ++ atom_to_list(?PROBE_MODULE) ++ ")",
                      [{return, binary}])),
    {ok, ?PROBE_MODULE, Bin} = compile:file(ProbeSrc, [binary, return_errors]),
    {module, ?PROBE_MODULE} = code:load_binary(?PROBE_MODULE, ProbeSrc, Bin),
    #{dir => Dir, shipped => Shipped, pristine => Pristine, probe => Probe0}.

generate(Dir, Name, GasModuleSource) ->
    Out = filename:join(Dir, Name ++ "_eval.erl"),
    Script = filename:join(code:lib_dir(aefate, priv), "aefa_gen_dispatch"),
    ?assert(filelib:is_regular(Script)),
    Args = case GasModuleSource of
               default ->
                   Out;
               Source ->
                   GasSrc = filename:join(Dir, Name ++ "_op_gas.erl"),
                   ok = file:write_file(GasSrc, Source),
                   Out ++ " " ++ GasSrc
           end,
    %% Both lib dirs, not one: _build/test/lib/aebytecode is a symlink into
    %% the default profile, so resolving aefate from it reaches a build that
    %% has never seen this test's application code.
    ErlLibs = filename:dirname(code:lib_dir(aefate)) ++ ":" ++
              filename:dirname(code:lib_dir(aebytecode)),
    Cmd = lists:flatten(io_lib:format("ERL_LIBS=~s escript ~s ~s 2>&1",
                                      [ErlLibs, Script, Args])),
    Output = os:cmd(Cmd),
    Generated = case file:read_file(Out) of
                    {ok, Bin} -> Bin;
                    _         -> <<>>
                end,
    %% The generator opens its output file before it starts writing, so a crash
    %% part-way leaves a file that exists and says nothing. Check the content.
    case binary:match(Generated, <<"-module(aefa_fate_eval).">>) of
        nomatch -> erlang:error({dispatch_generator_failed, Cmd, Output,
                                 {generated_bytes, byte_size(Generated)}});
        _       -> ok
    end,
    Generated.

pristine_gas_module_source() ->
    "-module(pristine_op_gas).\n"
    "-export([gas_term/2]).\n"
    "gas_term(_OpName, BaseGas) -> BaseGas.\n".

probe_gas_module_source() ->
    lists:flatten(
      io_lib:format(
        "-module(probe_op_gas).~n"
        "-export([gas_term/2]).~n"
        "gas_term(OpName, BaseGas) ->~n"
        "    aefa_fate_op_gas:gas_term(OpName, BaseGas, [{~w, [{~w, ~w}]}]).~n",
        [?PROBE_OP, ?SALUS_PROTOCOL_VSN, ?PROBE_NEW_GAS])).

probe_dir() ->
    filename:join("/tmp", "aefa_fate_op_gas_probe." ++ os:getpid()).

cleanup_probe(#{dir := Dir}) ->
    code:purge(?PROBE_MODULE),
    code:delete(?PROBE_MODULE),
    code:purge(?PROBE_MODULE),
    _ = [ file:delete(F) || F <- filelib:wildcard(filename:join(Dir, "*")) ],
    _ = file:del_dir(Dir),
    ok.
