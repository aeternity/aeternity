-module(aehttp_whitepaper_SUITE).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export([working_insurance/1,
         can_insure_twice_in_a_row/1,
         can_insure_after_expiration/1,
         insurer_can_withdraw_after_insurance_expires/1,
         insurer_can_withdraw_if_no_insurance/1,
         insurer_can_withdraw_exess_tokens/1,
         insurer_can_deposit_multiple_times/1,

         can_not_insure_if_not_enough_compensation/1,
         insurer_can_not_insure/1,
         insured_can_not_withdraw/1,
         insurer_can_not_withdraw_compensation/1,
         insured_can_not_deposit/1,
         claim_insurance_missing_response/1,
         claim_insurance_for_different_city/1,
         claim_insurance_outside_of_range/1,
         claim_insurance_different_response/1,
         claim_insurance_insurer/1,
         can_not_claim_twice/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(NODE, dev1).
-define(WS, aehttp_ws_test_utils).
-define(DEFAULT_MIN_DEPTH_FACTOR, 10).
-define(DEFAULT_MIN_DEPTH, 3).
-define(MAX_MINED_BLOCKS, 20).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).

-define(SLOGAN, slogan(?FUNCTION_NAME, ?LINE)).
-define(SLOGAN(Param), slogan(?FUNCTION_NAME, ?LINE, Param)).

-define(CHECK_INFO(Timeout), check_info(?LINE, Timeout)).
-define(PEEK_MSGQ, peek_msgq(?LINE)).

-define(ORACLE_TTL, 2000).
-define(QUERY_TTL, 100).
-define(RESPONSE_TTL, 100).

all() -> [{group, happy_path},
          {group, unhappy_path}].

groups() ->
    [{happy_path, [],
      [ working_insurance,
        can_insure_twice_in_a_row,
        can_insure_after_expiration,
        insurer_can_withdraw_after_insurance_expires,
        insurer_can_withdraw_if_no_insurance,
        insurer_can_withdraw_exess_tokens,
        insurer_can_deposit_multiple_times
      ]},
     {unhappy_path, [],
      [ can_not_insure_if_not_enough_compensation,
        insurer_can_not_insure,
        insured_can_not_withdraw,
        insurer_can_not_withdraw_compensation,
        insured_can_not_deposit,
        claim_insurance_missing_response,
        claim_insurance_for_different_city,
        claim_insurance_outside_of_range,
        claim_insurance_different_response,
        claim_insurance_insurer,
        can_not_claim_twice
      ]}
    ].

suite() -> [].

init_per_suite(Config) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, fate_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, fate_not_in_minerva};
        ?FORTUNA_PROTOCOL_VSN -> {skip, fate_not_in_fortuna};
        Vsn when Vsn >= ?LIMA_PROTOCOL_VSN ->
            Forks = aecore_suite_utils:forks(),
            DefCfg = #{<<"chain">> =>
                          #{<<"persist">> => true,
                            <<"hard_forks">> => Forks},
                      <<"mining">> =>
                          #{<<"micro_block_cycle">> => 1,
                            %% disable name claim auction
                            <<"name_claim_bid_timeout">> => 0}},
            {ok, StartedApps} = application:ensure_all_started(gproc),
            Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg,
                                                        [{instant_mining, true}, {symlink_name, "latest.whitepaper"}, {test_module, ?MODULE}] ++ Config),
            aehttp_sc_SUITE:start_node([ {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
                                       , {started_apps, StartedApps}
                                       , {ws_port, 12340}] ++ Config1)
    end.

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    aehttp_sc_SUITE:stop_node(Config).

init_per_group(Group, Config0) ->
    VM = fate, 
    Config1 = aect_test_utils:init_per_group(VM, Config0),
    Config2 = aehttp_sc_SUITE:reset_participants(Group, Config1),
    Config2.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    aect_test_utils:setup_testcase(Config),
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    ?CHECK_INFO(20),
    Config1 = [{tc_start, os:timestamp()} | Config],
    OracleOwnerPair = {_OraclePubkey, _OraclePrivkey} = aecore_suite_utils:generate_key_pair(),
    ok = initialize_account(200000000 * aec_test_utils:min_gas_price(),
                            OracleOwnerPair),
    Config2 = [{oracle_owner, OracleOwnerPair} | Config1],
    register_oracle_service(Config2),
    Config2.

end_per_testcase(_Case, Config) ->
    case ?CHECK_INFO(20) of
        [] -> ok;
        [_|_] = L ->
            ct:comment("~p unhandled messages", [length(L)])
    end,
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p",
           [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
             || {_,N} <- ?config(nodes, Config)]]),
    ok.

check_info(L, Timeout) ->
    receive
        Msg ->
            ct:log("[~p] UNEXPECTED: ~p", [L, Msg]),
            [Msg|check_info(L, Timeout)]
    after Timeout ->
            []
    end.

working_insurance(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    ContractName = channel_whitepaper_example,
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(ContractName,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the valid range
    ct:log("InsuredFrom: ~p, HailstormHeight: ~p, InsuredTo: ~p",
            [InsuredFrom, HailstormHeight, InsuredTo]),
    true = HailstormHeight >= InsuredFrom andalso HailstormHeight =< InsuredTo,
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

can_insure_after_expiration(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations1 = 1,
    Generations2 = 100,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations1, Cfg),
    {ok, [InsuredFrom1, InsuredTo1]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    mine_blocks(10),
    Height = current_height(),
    %% ensure not insured
    true = InsuredTo1 < Height,

    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations2, Cfg),
    {ok, [InsuredFrom2, InsuredTo2]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),

    %% since insurance expired, the interval is reset 
    true = InsuredFrom1 < InsuredFrom2,
    %% the range is expected
    ExpectedInsuredTo = InsuredFrom2 + Generations2,
    {InsuredTo2, InsuredTo2} = {InsuredTo2, ExpectedInsuredTo},

    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

can_insure_twice_in_a_row(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations1 = 100,
    Generations2 = 10,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations1, Cfg),
    {ok, [InsuredFrom1, InsuredTo1]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations2, Cfg),
    {ok, [InsuredFrom2, InsuredTo2]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),

    %% no blocks had been mined, so the first interval had not expired yet
    {InsuredFrom1, InsuredFrom1} = {InsuredFrom1, InsuredFrom2},
    %% the range is just extended
    ExpectedInsuredTo = InsuredTo1 + Generations2,
    {InsuredTo2, InsuredTo2} = {InsuredTo2, ExpectedInsuredTo},

    ok = unregister_channel_events(AllEvents, Pids),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok.

insurer_can_withdraw_after_insurance_expires(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    Generations = 1,
    PricePerGeneration = 2,
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    aehttp_sc_SUITE:with_registered_events(
      [sign, info, get, error, update], [IConnPid, RConnPid],
      fun() ->
          {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
          OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
          {UpdateVolley, _ReverseUpdateVolley} =
              aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
          Args =
              [OracleAddress,
              add_quotes(City),
              _PricePerGen = integer_to_list(PricePerGeneration),
              _Compensation = integer_to_list(Reward)],
          {UnsignedStateTx, _Updates, _Code} =
              aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                               Args, IConnPid,
                                               UpdateVolley, Cfg, 0),
          ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
          ContractName = channel_whitepaper_example,
          {ok, []} =
              call_offchain_contract(initiator, ContractPubkey,
                                    ContractName, "deposit", [], Reward, Cfg),
          {ok, []} =
              call_offchain_contract(responder, ContractPubkey,
                                    ContractName, "insure",
                                    [],
                                    Generations * PricePerGeneration, Cfg),
          {ok, [_InsuredFrom, InsuredTo]} =
              call_offchain_contract(responder, ContractPubkey,
                                    ContractName, "get_insurance_range", [], 0, Cfg),
          mine_blocks(20),
          Height = current_height(),
          %% test assumes that this is expired
          true = Height > InsuredTo,
          {ok, []} =
              call_offchain_contract(initiator, ContractPubkey,
                                    ContractName, "withdraw",
                                    [integer_to_list(Reward + Generations * PricePerGeneration)],
                                    0, Cfg),
          ok
      end),
    aehttp_sc_SUITE:sc_ws_close_(Cfg).

insurer_can_deposit_multiple_times(Cfg0) ->
    City = "Sofia, Bulgaria",
    Deposit1 = 100,
    Reward = 10000,
    Deposit2 = Reward - Deposit1,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Deposit1, Cfg),
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Deposit2, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the valid range
    ct:log("InsuredFrom: ~p, HailstormHeight: ~p, InsuredTo: ~p",
            [InsuredFrom, HailstormHeight, InsuredTo]),
    true = HailstormHeight >= InsuredFrom andalso HailstormHeight =< InsuredTo,
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

insurer_can_withdraw_exess_tokens(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    Generations = 100,
    PricePerGeneration = 2,
    Bonus = 10,
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    aehttp_sc_SUITE:with_registered_events(
      [sign, info, get, error, update], [IConnPid, RConnPid],
      fun() ->
          {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
          OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
          {UpdateVolley, _ReverseUpdateVolley} =
              aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
          Args =
              [OracleAddress,
              add_quotes(City),
              _PricePerGen = integer_to_list(PricePerGeneration),
              _Compensation = integer_to_list(Reward)],
          {UnsignedStateTx, _Updates, _Code} =
              aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                               Args, IConnPid,
                                               UpdateVolley, Cfg, 0),
          ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
          ContractName = channel_whitepaper_example,
          {ok, []} =
              call_offchain_contract(initiator, ContractPubkey,
                                    ContractName, "deposit", [], Reward + Bonus, Cfg),
          {ok, []} =
              call_offchain_contract(responder, ContractPubkey,
                                    ContractName, "insure",
                                    [],
                                    Generations * PricePerGeneration, Cfg),
          {ok, [InsuredFrom, InsuredTo]} =
              call_offchain_contract(responder, ContractPubkey,
                                    ContractName, "get_insurance_range", [], 0, Cfg),
          mine_blocks(10),
          Height = current_height(),
          %% test assumes that this is not expired
          true = Height >= InsuredFrom andalso Height =< InsuredTo,
          {ok, []} =
              call_offchain_contract(initiator, ContractPubkey,
                                    ContractName, "withdraw",
                                    [integer_to_list(Bonus + Generations * PricePerGeneration)],
                                    0, Cfg),
          ok
      end),
    aehttp_sc_SUITE:sc_ws_close_(Cfg).

insurer_can_withdraw_if_no_insurance(Cfg0) ->
    City = "Sofia, Bulgaria",
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    aehttp_sc_SUITE:with_registered_events(
      [sign, info, get, error, update], [IConnPid, RConnPid],
      fun() ->
          {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
          OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
          {UpdateVolley, _ReverseUpdateVolley} =
              aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
          Compensation = 10000,
          Args =
              [OracleAddress,
              add_quotes(City),
              _PricePerGen = "3",
              _Compensation = integer_to_list(Compensation)],
          {UnsignedStateTx, _Updates, _Code} =
              aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                               Args, IConnPid,
                                               UpdateVolley, Cfg, 0),
          ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
          ContractName = channel_whitepaper_example,
          {ok, []} =
              call_offchain_contract(initiator, ContractPubkey,
                                    ContractName, "deposit", [], Compensation, Cfg),
          {ok, [_InsuredFrom, InsuredTo]} =
              call_offchain_contract(responder, ContractPubkey,
                                    ContractName, "get_insurance_range", [], 0, Cfg),
          mine_blocks(10),
          Height = current_height(),
          %% test assumes that this is expired
          true = Height > InsuredTo,
          {ok, []} =
              call_offchain_contract(initiator, ContractPubkey,
                                    ContractName, "withdraw",
                                    [integer_to_list(Compensation)], 0, Cfg),
          ok
      end),
    aehttp_sc_SUITE:sc_ws_close_(Cfg).

can_not_insure_if_not_enough_compensation(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, _} = aehttp_sc_SUITE:sc_ws_get_balance(IConnPid, RPubkey, Cfg),
    {revert, <<"not_enough_compensation">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

insurer_can_not_insure(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {revert, <<"service_provider">>} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

insured_can_not_withdraw(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {revert, <<"not_service_provider">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "withdraw",
                              [integer_to_list(Reward + Generations * PricePerGeneration)],
                              0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

insurer_can_not_withdraw_compensation(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {revert, <<"not_enough_compensation">>} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "withdraw",
                              [integer_to_list(Reward + Generations * PricePerGeneration)],
                              0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

insured_can_not_deposit(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {revert, <<"not_service_provider">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok.

claim_insurance_missing_response(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    %% this query remains unanswered
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the valid range
    ct:log("InsuredFrom: ~p, HailstormHeight: ~p, InsuredTo: ~p",
            [InsuredFrom, HailstormHeight, InsuredTo]),
    true = HailstormHeight >= InsuredFrom andalso HailstormHeight =< InsuredTo,
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),
    {revert, <<"no_response">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

claim_insurance_for_different_city(Cfg0) ->
    City = "Sofia, Bulgaria",
    DifferentCity = "Berlin, Germany",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(DifferentCity, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the valid range
    ct:log("InsuredFrom: ~p, HailstormHeight: ~p, InsuredTo: ~p",
            [InsuredFrom, HailstormHeight, InsuredTo]),
    true = HailstormHeight >= InsuredFrom andalso HailstormHeight =< InsuredTo,
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),
    {revert, <<"different_city">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

claim_insurance_outside_of_range(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HeightInThePast = 1,
    HeightInTheFuture = 100000000000000,
    QueryIdInThePast = ask_oracle_service(City, HeightInThePast, Cfg0),
    answer_oracle_query(QueryIdInThePast, true, Cfg0),
    QueryIdInTheFuture = ask_oracle_service(City, HeightInTheFuture, Cfg0),
    answer_oracle_query(QueryIdInTheFuture, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the invalid range
    ct:log("HeightInThePast: ~p, InsuredFrom: ~p, InsuredTo: ~p, HeightInTheFuture: ~p",
            [HeightInThePast, InsuredFrom, InsuredTo, HeightInTheFuture]),
    true = HeightInThePast < InsuredFrom andalso HeightInTheFuture > InsuredTo,
    {revert, <<"not_in_insurance_range">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance",
                              [aeser_api_encoder:encode(oracle_query_id, QueryIdInThePast)],
                              0, Cfg),
    {revert, <<"not_in_insurance_range">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance",
                              [aeser_api_encoder:encode(oracle_query_id, QueryIdInTheFuture)],
                              0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

claim_insurance_different_response(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, false, Cfg0), %% no hailstorm
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the valid range
    ct:log("InsuredFrom: ~p, HailstormHeight: ~p, InsuredTo: ~p",
            [InsuredFrom, HailstormHeight, InsuredTo]),
    true = HailstormHeight >= InsuredFrom andalso HailstormHeight =< InsuredTo,
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),
    {revert, <<"different_response">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

claim_insurance_insurer(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the valid range
    ct:log("InsuredFrom: ~p, HailstormHeight: ~p, InsuredTo: ~p",
            [InsuredFrom, HailstormHeight, InsuredTo]),
    true = HailstormHeight >= InsuredFrom andalso HailstormHeight =< InsuredTo,
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),
    {revert, <<"service_provider">>} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.

can_not_claim_twice(Cfg0) ->
    City = "Sofia, Bulgaria",
    Reward = 10000,
    PricePerGeneration = 2,
    Generations = 1000,
    HailstormHeight = current_height() + 50,
    QueryId = ask_oracle_service(City, HailstormHeight, Cfg0),
    answer_oracle_query(QueryId, true, Cfg0),
    Cfg = channel_open(Cfg0),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := _RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {OraclePubkey, _OraclePrivkey} = ?config(oracle_owner, Cfg),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(initiator, Cfg),
    Args =
        [OracleAddress,
        add_quotes(City),
        _PricePerGen = integer_to_list(PricePerGeneration),
        _Compensation = integer_to_list(Reward)],
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(channel_whitepaper_example,
                                          Args, IConnPid,
                                          UpdateVolley, Cfg, 0),
    ContractPubkey = contract_id_from_create_update(IPubkey, UnsignedStateTx),
    ContractName = channel_whitepaper_example,
    {ok, []} =
        call_offchain_contract(initiator, ContractPubkey,
                              ContractName, "deposit", [], Reward, Cfg),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "insure",
                              [],
                              PricePerGeneration * Generations, Cfg),
    {ok, [InsuredFrom, InsuredTo]} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "get_insurance_range", [], 0, Cfg),
    {InsuredFrom, InsuredTo, Generations} =
        {InsuredFrom, InsuredFrom + Generations, Generations},
    mine_blocks(10),
    %% test assumes that this is in the valid range
    ct:log("InsuredFrom: ~p, HailstormHeight: ~p, InsuredTo: ~p",
            [InsuredFrom, HailstormHeight, InsuredTo]),
    true = HailstormHeight >= InsuredFrom andalso HailstormHeight =< InsuredTo,
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),
    {ok, []} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    %% at this point the insure must be closed so calling it a second time is
    %% bound to fail
    {revert, <<"not_in_insurance_range">>} =
        call_offchain_contract(responder, ContractPubkey,
                              ContractName, "claim_insurance", [EncQueryId], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_(Cfg),
    ok = unregister_channel_events(AllEvents, Pids),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal helper funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_oracle_service(Cfg) ->
    %% Register an oracle. It will be used in an off-chain contract
    %% Oracle ask itself a question and answers it
    {OraclePubkey, OraclePrivkey} = ?config(oracle_owner, Cfg),
    QuestionFormat =
        case aect_test_utils:backend() of
            fate -> iolist_to_binary(aeb_fate_encoding:serialize_type({tuple,
                                                                       [string,
                                                                        integer]}))
        end,
    ResponseFormat =
        case aect_test_utils:backend() of
            fate -> iolist_to_binary(aeb_fate_encoding:serialize_type(boolean))
        end,
    QueryFee = 3,
    register_oracle(OraclePubkey, OraclePrivkey,
                    #{query_format    => QuestionFormat,
                      response_format => ResponseFormat,
                      query_fee       => QueryFee,
                      query_ttl       => ?QUERY_TTL,
                      abi_version     => aect_test_utils:abi_version(),
                      oracle_ttl      => {delta, ?ORACLE_TTL}
                     }),
    OraclePubkey.

ask_oracle_service(City, Timestamp, Cfg) ->
    {OraclePubkey, OraclePrivkey} = ?config(oracle_owner, Cfg),
    SophiaString =
        fun(S0) ->
            S = list_to_binary(S0),
            case aect_test_utils:backend() of
                fate -> S 
            end
        end,
    Question = {tuple, {SophiaString(City), Timestamp}},
    Q = aeb_fate_encoding:serialize(Question),
    QueryId = query_oracle(OraclePubkey, OraclePrivkey, %oracle asks oracle
                           OraclePubkey,
                           #{query        => Q,
                             query_ttl    => {delta, ?QUERY_TTL - 1},
                             response_ttl => {delta, ?RESPONSE_TTL}}),
    QueryId.

answer_oracle_query(QueryId, Answer, Cfg) ->
    {OraclePubkey, OraclePrivkey} = ?config(oracle_owner, Cfg),
    R = aeb_fate_encoding:serialize(Answer),
    respond_oracle(OraclePubkey, OraclePrivkey, QueryId,
                   R, #{response_ttl => {delta, ?RESPONSE_TTL}}).

register_oracle(OraclePubkey, OraclePrivkey, Opts) ->
    case rpc(aec_chain, get_oracle, [OraclePubkey]) of
        {error, not_found} -> pass;
        {ok, Oracle} ->
            Height = current_height(),
            TTL = aeo_oracles:ttl(Oracle), % absolute TTL
            ExpBlocksCnt = TTL - Height,
            ct:log("Already an oracle, mining ~p blocks so it expires",
                   [ExpBlocksCnt]),
            Node = aecore_suite_utils:node_name(?NODE),
            aecore_suite_utils:mine_key_blocks(Node, ExpBlocksCnt + 1)
    end,
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:register_tx(OraclePubkey, Opts#{nonce => Nonce}, #{}),
    aehttp_sc_SUITE:sign_post_mine(Tx, OraclePrivkey),
    OracleId = aeser_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {ok, 200, _Resp} = get_oracles_by_pubkey_sut(OracleId),
    ok.

query_oracle(FromPubkey, FromPrivkey, OraclePubkey, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [FromPubkey]),
    Tx = aeo_test_utils:query_tx(FromPubkey, aeser_id:create(oracle, OraclePubkey),
                                 Opts#{nonce => Nonce}, #{}),
    aehttp_sc_SUITE:sign_post_mine(Tx, FromPrivkey),
    {aeo_query_tx, QueryTx} = aetx:specialize_callback(Tx),
    aeo_query_tx:query_id(QueryTx).

respond_oracle(OraclePubkey, OraclePrivkey, QueryId, Response, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:response_tx(OraclePubkey, QueryId,
                                    Response, Opts#{nonce => Nonce}, #{}),
    aehttp_sc_SUITE:sign_post_mine(Tx, OraclePrivkey),
    ok.

current_height() ->
    case rpc(aec_chain, top_header, []) of
        undefined -> 1;
        Header -> aec_headers:height(Header) + 1
    end.

get_oracles_by_pubkey_sut(Pubkey) ->
    Host = external_address(),
    http_request(Host, get, "oracles/" ++ http_uri:encode(Pubkey), []).

initialize_account(Amount, KeyPair) ->
    initialize_account(Amount, KeyPair, true).

initialize_account(Amount, {Pubkey, _Privkey}, Check) ->
    Fee = ?SPEND_FEE,
    Node = aecore_suite_utils:node_name(?NODE),
    MaxMined = ?MAX_MINED_BLOCKS + (Amount div aec_governance:block_mine_reward(1)),
    ct:pal("Mining ~p blocks at most for ~p tokens", [MaxMined, Amount]),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Amount, Fee),
    TxHash = aehttp_sc_SUITE:sign_and_post_tx(SpendTx),
    if Check ->
        aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [TxHash], MaxMined),
        assert_balance_at_least(Pubkey, Amount),
        ok;
       true ->
        TxHash
    end.

post_spend_tx(RecipientId, Amount, Fee) ->
    {_, Sender} = aecore_suite_utils:sign_keys(?NODE),
    SenderId = aeser_api_encoder:encode(account_pubkey, Sender),
    post_spend_tx(SenderId, RecipientId, Amount, Fee, <<"foo">>).

post_spend_tx(SenderId, RecipientId, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/spend",
                 #{sender_id => SenderId,
                   recipient_id => RecipientId,
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

assert_balance_at_least(Pubkey, MaxExpectedBalance) ->
    assert_balance(Pubkey, MaxExpectedBalance, equal_or_greater).

assert_balance(Pubkey, ExpectedBalance, Action) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := ActualBalance}} =
        get_accounts_by_pubkey_sut(Address),
    Res =
        case Action of
            equal -> ExpectedBalance =:= ActualBalance;
            equal_or_greater -> ActualBalance >= ExpectedBalance;
            equal_or_less -> ActualBalance =< ExpectedBalance
        end,
    {true, _, _, _} = {Res, ActualBalance, Action, ExpectedBalance}.

get_accounts_by_pubkey_sut(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

add_quotes(B) when is_binary(B) -> <<"\"", B/binary, "\"">>;
add_quotes(Str) when is_list(Str) -> "\"" ++ Str ++  "\"".

contract_id_from_create_update(Owner, OffchainTx) ->
    {CB, Tx} = aetx:specialize_callback(OffchainTx),
    Round = CB:round(Tx),
    aect_contracts:compute_contract_pubkey(Owner, Round).

get_decoded_result(ConnPid, Contract, Function, [Update], UnsignedTx, Config) ->
    GetParams = ws_get_call_params(Update, UnsignedTx),
    ws_send_tagged(ConnPid, <<"channels.get.contract_call">>,
                    GetParams),
    {ok, _, <<"contract_call">>, CallRes} =
        aehttp_sc_SUITE:wait_for_channel_event(ConnPid, get, Config),
    #{<<"caller_id">>         := _CallerId,
      <<"caller_nonce">>      := CallRound,
      <<"contract_id">>       := _ContractId,
      <<"gas_price">>         := _,
      <<"gas_used">>          := _,
      <<"height">>            := CallRound,
      <<"return_type">>       := ReturnType0,
      <<"return_value">>      := ReturnValue} = CallRes,
    {ok, BinCode} = aect_test_utils:read_contract(?SOPHIA_LIMA_AEVM, Contract),
    {_ReturnType, _ReturnVal} =
        case ReturnType0 of
            <<"ok">> ->
                Res = aect_test_utils:decode_call_result(binary_to_list(BinCode), Function, ok,
                                           ReturnValue),
                {ok, Res};
            <<"revert">> ->
                {ok, Reason} = aect_test_utils:decode_data(string, ReturnValue),
                {revert, Reason};
            <<"error">> -> {error, <<>>}
        end.

ws_send_tagged(ConnPid, Method, Payload) ->
    ?WS:json_rpc_notify(
       ConnPid,
       #{ <<"method">> => Method
        , <<"params">> => Payload }).

ws_get_call_params(Update, UnsignedTx) ->
    {CB1, Tx1} = aetx:specialize_callback(UnsignedTx),
    CallRound = CB1:round(Tx1),
    CallerPubKey = extract_caller(Update),
    ContractPubKey = extract_contract_pubkey(Update),
    CallerId = aeser_api_encoder:encode(account_pubkey, CallerPubKey),
    ContractId = aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
    #{contract_id => ContractId,
      caller_id   => CallerId,
      round       => CallRound}.

extract_caller(#{<<"op">> := <<"OffChainNewContract">>,
                 <<"owner">> := EncOwnerId}) ->
    {ok, Owner} = aeser_api_encoder:safe_decode(account_pubkey, EncOwnerId),
    Owner;
extract_caller(#{<<"op">> := <<"OffChainCallContract">>,
                 <<"caller_id">> := EncCallerId}) ->
    {ok, Caller} = aeser_api_encoder:safe_decode(account_pubkey, EncCallerId),
    Caller.

extract_contract_pubkey(#{<<"op">> := <<"OffChainCallContract">>,
                          <<"contract_id">> := EncContractId}) ->
    {ok, ContractPubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncContractId),
    ContractPubKey.

call_offchain_contract(Who, ContractPubkey, ContractName, Function, Arguments,
                       Amount, Cfg) ->
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(Who, Cfg),
    ConnPid =
        case Who of
            initiator -> IConnPid;
            responder -> RConnPid
        end,
    #{tx := Tx, updates := Updates} =
        aehttp_sc_SUITE:call_a_contract_(Function, Arguments, ContractPubkey,
                        ContractName,
                        ConnPid, UpdateVolley,
                        Amount, Cfg, #{}),
    R = get_decoded_result(ConnPid, ContractName,
                           Function, Updates, Tx, Cfg),
    R.

mine_blocks(Cnt) ->
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:mine_key_blocks(Node, Cnt).

register_channel_events(Events, Pids) ->
    [ok = ?WS:register_test_for_channel_events(Pid, Events)
     || Pid <- Pids],
    ok.

unregister_channel_events(Events, Pids) ->
    [ok = ?WS:unregister_test_for_channel_events(Pid, Events)
     || Pid <- Pids],
    ok.

channel_open(Cfg0) ->
    GOpts = ?config(tc_group_properties, Cfg0),
    Group = ?config(name, GOpts),
    {_, TestName} = proplists:get_value(current, ct:get_status()),
    MsgLogFile = filename:join([?config(priv_dir, Cfg0),
                                "channel_docs",
                                "whitepaper",
                                atom_to_list(Group),
                                atom_to_list(TestName)++ ".md"]),
    Cfg = aehttp_sc_SUITE:sc_ws_open_(Cfg0, #{}, ?DEFAULT_MIN_DEPTH, MsgLogFile),
    Cfg.
