-module(aehttp_coin_toss_SUITE).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export([player_wins_heads/1,
         player_wins_tails/1,
         player_loses_heads/1,
         player_loses_tails/1,
         small_amount/1,
         big_amount/1,
         really_short_key/1,
         long_key/1,
         couple_of_deposits/1,
         can_withdraw_before_bet/1,
         can_withdraw_after_winning_bet/1,
         can_have_a_couple_of_sequential_games/1,
         can_deposit_and_withdrawal/1,
         casino_disputes_player_inactivity/1,
         player_disputes_casino_inactivity/1
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

-define(HEADS, "heads").
-define(TAILS, "tails").

all() -> [{group, casino_is_initiator},
          {group, casino_is_responder},
          {group, different_params},
          {group, sequential_events}
         ].

groups() ->
    [
     {casino_is_initiator, [sequence], [{group, happy_path}]},
     {casino_is_responder, [sequence], [{group, happy_path}]},
     {happy_path, [sequence],
      [ player_wins_heads,
        player_wins_tails,
        player_loses_heads,
        player_loses_tails
      ]},
     {different_params, [sequence],
      [ small_amount,
        big_amount,
        really_short_key,
        long_key
      ]},
     {sequential_events, [sequence],
      [ 
       couple_of_deposits,
       can_withdraw_before_bet,
       can_withdraw_after_winning_bet,
       can_have_a_couple_of_sequential_games,
       can_deposit_and_withdrawal
      ]},
     {dispute, [sequence],
      [
       casino_disputes_player_inactivity,
       player_disputes_casino_inactivity
      ]
     }
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
                                                        [{symlink_name, "latest.whitepaper"}, {test_module, ?MODULE}] ++ Config),
            aehttp_sc_SUITE:start_node([ {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
                                       , {started_apps, StartedApps}
                                       , {ws_port, 12340}] ++ Config1)
    end.

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    aehttp_sc_SUITE:stop_node(Config).

init_per_group(casino_is_initiator, Config) ->
    [{roles, {initiator, responder}} | Config];
init_per_group(casino_is_responder, Config) ->
    [{roles, {responder, initiator}} | Config];
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
    Config1.

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

player_wins_tails(Cfg) ->
    {Casino, Player} = ?config(roles, Cfg),
    Stake = 10,
    Deposit = 10000,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = "abc",
                  _PlayerGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

player_loses_tails(Cfg) ->
    {Casino, Player} = ?config(roles, Cfg),
    Stake = 10,
    Deposit = 10000,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = "abc",
                  _PlayerGuess = ?TAILS,
                  _ActualSide = ?HEADS,
                  _Outcome = lose).

player_wins_heads(Cfg) ->
    {Casino, Player} = ?config(roles, Cfg),
    Stake = 10,
    Deposit = 10000,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = "abc",
                  _PlayerGuess = ?HEADS,
                  _ActualSide = ?HEADS,
                  _Outcome = win).
    
player_loses_heads(Cfg) ->
    {Casino, Player} = ?config(roles, Cfg),
    Stake = 10,
    Deposit = 10000,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = "abc",
                  _PlayerGuess = ?HEADS,
                  _ActualSide = ?TAILS,
                  _Outcome = lose).

small_amount(Cfg) ->
    {Casino, Player} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 1,
    Deposit = 10000,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = "abc",
                  _PlayerGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

big_amount(Cfg) ->
    {Casino, Player} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 100,
    Deposit = 100,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = "abc",
                  _PlayerGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

really_short_key(Cfg) ->
    {Casino, Player} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 1,
    Deposit = 100,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = "",
                  _PlayerGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

long_key(Cfg) ->
    {Casino, Player} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 1,
    Deposit = 100,

    success_story(Cfg, Casino, Player, Stake, Deposit,
                  _Key = string:join(lists:duplicate(100, "a"), ""),
                  _PlayerGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

couple_of_deposits(Cfg0) ->
    {Casino, Player} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Deposit = 100,
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    %% no issue providing a hash
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.

can_withdraw_before_bet(Cfg0) ->
    {Casino, Player} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    WithdrawAmt = 10,
    FirstDeposit = 2 * WithdrawAmt,
    Deposit = 100,
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    {CasinoBalance0, PlayerBalance0} = get_offchain_balances(Casino, Player, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], FirstDeposit, Cfg),
    {CasinoBalance1, PlayerBalance1} = get_offchain_balances(Casino, Player, Cfg),
    assert_equal(CasinoBalance0, CasinoBalance1 + FirstDeposit),
    assert_equal(PlayerBalance0, PlayerBalance1),

    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "withdraw",
                               [integer_to_list(WithdrawAmt)], 0, Cfg),
    {CasinoBalance2, PlayerBalance2} = get_offchain_balances(Casino, Player, Cfg),
    assert_equal(CasinoBalance0, CasinoBalance2 + FirstDeposit - WithdrawAmt),
    assert_equal(PlayerBalance0, PlayerBalance2),

    %% withdraw again, just to make sure
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "withdraw",
                               [integer_to_list(WithdrawAmt)], 0, Cfg),
    {CasinoBalance3, PlayerBalance3} = get_offchain_balances(Casino, Player, Cfg),
    assert_equal(CasinoBalance0, CasinoBalance3 + FirstDeposit - WithdrawAmt - WithdrawAmt),
    assert_equal(PlayerBalance0, PlayerBalance3),
    %% deposit again
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    %% no issue providing a hash
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.

can_withdraw_after_winning_bet(Cfg0) ->
    {Casino, Player} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    PlayerGuess = ?TAILS,
    ActualSide = ?TAILS,
    Stake = 10,
    Deposit = 100,
    WithdrawAmt = 2,
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),

    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    %% no issue providing a hash
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], 0, Cfg),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "bet",
                              [add_quotes(PlayerGuess)], Stake, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "resolve",
                              [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),

    {CasinoBalance0, PlayerBalance0} = get_offchain_balances(Casino, Player, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "withdraw",
                               [integer_to_list(WithdrawAmt)], 0, Cfg),
    {CasinoBalance1, PlayerBalance1} = get_offchain_balances(Casino, Player, Cfg),
    assert_equal(CasinoBalance0, CasinoBalance1 - WithdrawAmt),
    assert_equal(PlayerBalance0, PlayerBalance1),

    %% withdraw again, just to make sure
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "withdraw",
                               [integer_to_list(WithdrawAmt)], 0, Cfg),
    {CasinoBalance3, PlayerBalance3} = get_offchain_balances(Casino, Player, Cfg),
    assert_equal(CasinoBalance0, CasinoBalance3 - 2 * WithdrawAmt),
    assert_equal(PlayerBalance0, PlayerBalance3),

    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.

can_have_a_couple_of_sequential_games(Cfg0) ->
    {Casino, Player} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    Stake = 10,
    Deposit = 100,
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    BetRound =
        fun(ActualSide, PlayerGuess) ->
            {ok, Hash} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "compute_hash",
                                      [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
            {ok, []} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "provide_hash",
                                      [Hash], 0, Cfg),
            {ok, []} =
                call_offchain_contract(Player, ContractPubkey,
                                      ContractName, "bet",
                                      [add_quotes(PlayerGuess)], Stake, Cfg),
            {ok, []} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "resolve",
                                      [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg)
        end,
    BetRound(?HEADS, ?HEADS), % win
    BetRound(?TAILS, ?HEADS), % lose
    BetRound(?TAILS, ?TAILS), % win
    BetRound(?HEADS, ?HEADS), % win
    BetRound(?HEADS, ?TAILS), % lose
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.

can_deposit_and_withdrawal(Cfg0) ->
    {Casino, Player} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    Stake = 10,
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),

    Deposit =
        fun(DepositAmt) ->
            {ok, []} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "deposit",
                                      [], DepositAmt, Cfg)
        end,
    Withdraw =
        fun(WithdrawAmt) ->
            {ok, []} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "withdraw",
                                      [integer_to_list(WithdrawAmt)], 0, Cfg)
        end,
    BetRound =
        fun(ActualSide, PlayerGuess) ->
            {ok, Hash} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "compute_hash",
                                      [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
            {ok, []} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "provide_hash",
                                      [Hash], 0, Cfg),
            {ok, []} =
                call_offchain_contract(Player, ContractPubkey,
                                      ContractName, "bet",
                                      [add_quotes(PlayerGuess)], Stake, Cfg),
            {ok, []} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "resolve",
                                      [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg)
        end,
    Deposit(100),
    Deposit(10),
    Withdraw(1),
    BetRound(?HEADS, ?HEADS), % win
    Withdraw(2),
    Withdraw(3),
    Deposit(10),
    Withdraw(4),
    BetRound(?TAILS, ?HEADS), % lose
    Withdraw(2),
    Withdraw(3),
    Deposit(10),
    Withdraw(4),
    BetRound(?TAILS, ?TAILS), % win
    Withdraw(2),
    Withdraw(3),
    Deposit(10),
    Withdraw(4),
    BetRound(?HEADS, ?HEADS), % win
    Withdraw(2),
    Withdraw(3),
    Deposit(10),
    Withdraw(4),
    BetRound(?HEADS, ?TAILS), % lose
    Withdraw(2),
    Withdraw(3),
    Deposit(10),
    Withdraw(4),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.

casino_disputes_player_inactivity(Cfg0) ->
    {Casino, Player} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Deposit = 100,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),
    mine_blocks(1),
    {ok, Hash} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    %% no issue providing a hash
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], 0, Cfg),
    mine_blocks(ReactionTime),
    %% not allowed yet
    {revert,<<"not_yet_allowed">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_dispute_no_bet",
                               [], 0, Cfg),
    %% go beyond the limit of ReactionTime + 1
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_dispute_no_bet",
                               [], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.

player_disputes_casino_inactivity(Cfg0) ->
    {Casino, Player} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    PlayerGuess = ?TAILS,
    ActualSide = ?TAILS,
    Stake = 1,
    Deposit = 100,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    %% no issue providing a hash
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], 0, Cfg),
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "bet",
                              [add_quotes(PlayerGuess)], Stake, Cfg),
    mine_blocks(ReactionTime),
    %% not allowed yet
    {revert,<<"not_yet_allowed">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "player_dispute_no_reveal",
                               [], 0, Cfg),
    %% go beyond the limit of ReactionTime + 1
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "player_dispute_no_reveal",
                               [], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.

success_story(Cfg0, Casino, Player, Stake, Deposit, Key, PlayerGuess,
              ActualSide, Outcome) ->
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {CasinoAddress, PlayerAddress} = prepare_for_test(Casino, Player, Cfg),
    Args = [CasinoAddress, PlayerAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Casino, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "deposit",
                               [], Deposit, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], 0, Cfg),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "bet",
                              [add_quotes(PlayerGuess)], Stake, Cfg),
    {CasinoBalance0, PlayerBalance0} = get_offchain_balances(Casino, Player, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "resolve",
                              [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    {CasinoBalance1, PlayerBalance1} = get_offchain_balances(Casino, Player, Cfg),
    %% casino account did not gain anything; if the player lost, the tokens
    %% are in the contract itself
    assert_equal(CasinoBalance0, CasinoBalance1),
    case Outcome of
        win ->
            %% player won twice the stake
            assert_equal(PlayerBalance0, PlayerBalance1 - 2 * Stake);
        lose ->
            %% player didn't win anything
            assert_equal(PlayerBalance0, PlayerBalance1)
    end,
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Player),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal helper funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

assert_balance(Pubkey, ExpectedBalance) ->
    assert_balance(Pubkey, ExpectedBalance, equal).

assert_balance_at_most(Pubkey, ExpectedBalance) ->
    assert_balance(Pubkey, ExpectedBalance, equal_or_less).

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

create_offchain_contract(Who, ContractName, Args, Cfg) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    {UpdateVolley, _ReverseUpdateVolley} =
        aehttp_sc_SUITE:produce_update_volley_funs(Who, Cfg),
    {ConnPid, Pubkey} =
        case Who of
            initiator -> {IConnPid, IPubkey};
            responder -> {RConnPid, RPubkey}
        end,
    {UnsignedStateTx, _Updates, _Code} =
        aehttp_sc_SUITE:create_contract_(ContractName,
                                         Args, ConnPid,
                                         UpdateVolley, Cfg),
    _ContractPubkey = contract_id_from_create_update(Pubkey,
                                                     UnsignedStateTx).

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

prepare_for_test(Casino, Player, Cfg) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {CasinoPubkey, PlayerPubkey} =
        case {Casino, Player} of
            {initiator, responder} -> {IPubkey, RPubkey};
            {responder, initiator} -> {RPubkey, IPubkey}
        end,
    CasinoAddress = aeser_api_encoder:encode(account_pubkey, CasinoPubkey),
    PlayerAddress = aeser_api_encoder:encode(account_pubkey, PlayerPubkey),
    {CasinoAddress, PlayerAddress}.

get_offchain_balances(Casino, Player, Cfg) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    {CasinoPubkey, PlayerPubkey} =
        case {Casino, Player} of
            {initiator, responder} -> {IPubkey, RPubkey};
            {responder, initiator} -> {RPubkey, IPubkey}
        end,
    {ok, {CBal, PBal}} =
        aehttp_sc_SUITE:sc_ws_get_both_balances(IConnPid,
                                                CasinoPubkey,
                                                PlayerPubkey,
                                                Cfg),
    {ok, {CBal, PBal}} =
        aehttp_sc_SUITE:sc_ws_get_both_balances(RConnPid,
                                                CasinoPubkey,
                                                PlayerPubkey,
                                                Cfg),
    {CBal, PBal}.

assert_equal(Val1, Val2) ->
    {Val1, Val1} = {Val1, Val2}.
