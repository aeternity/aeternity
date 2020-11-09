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

-export([casino_wins_heads/1,
         casino_wins_tails/1,
         casino_loses_heads/1,
         casino_loses_tails/1,
         small_amount/1,
         big_amount/1,
         really_short_key/1,
         long_key/1,
         can_have_a_couple_of_sequential_games/1,
         player_disputes_casino_inactivity/1,
         casino_disputes_player_inactivity/1,
         can_play_after_casino_dispute/1,
         can_play_after_player_dispute/1,

         provide_hash_fails/1,
         casino_pick_fails/1,
         reveal_fails/1,
         casino_dispute_fails/1,
         player_dispute_fails/1,

         intended_usage/1
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

all() -> [{group, player_is_initiator},
          {group, player_is_responder},
          {group, different_params},
          {group, sequential_events}
         ].

groups() ->
    [
     {player_is_initiator, [sequence], [{group, happy_path}, {group, negative_path}]},
     {player_is_responder, [sequence], [{group, happy_path}, {group, negative_path}]},
     {happy_path, [sequence],
      [ casino_wins_heads,
        casino_wins_tails,
        casino_loses_heads,
        casino_loses_tails,
        intended_usage
      ]},
     {different_params, [sequence],
      [ small_amount,
        big_amount,
        really_short_key,
        long_key
      ]},
     {sequential_events, [sequence],
      [ 
       can_have_a_couple_of_sequential_games,
       can_play_after_casino_dispute,
       can_play_after_player_dispute
      ]},
     {dispute, [sequence],
      [
       player_disputes_casino_inactivity,
       casino_disputes_player_inactivity
      ]},
     {negative_path, [sequence],
      [
       provide_hash_fails,
       casino_pick_fails,
       reveal_fails,
       casino_dispute_fails,
       player_dispute_fails
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
                          #{<<"persist">> => false,
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

init_per_group(player_is_initiator, Config) ->
    [{roles, {initiator, responder}} | Config];
init_per_group(player_is_responder, Config) ->
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

casino_wins_tails(Cfg) ->
    {Player, Casino} = ?config(roles, Cfg),
    Stake = 10,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = "abc",
                  _CasinoGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

casino_loses_tails(Cfg) ->
    {Player, Casino} = ?config(roles, Cfg),
    Stake = 10,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = "abc",
                  _CasinoGuess = ?TAILS,
                  _ActualSide = ?HEADS,
                  _Outcome = lose).

casino_wins_heads(Cfg) ->
    {Player, Casino} = ?config(roles, Cfg),
    Stake = 10,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = "abc",
                  _CasinoGuess = ?HEADS,
                  _ActualSide = ?HEADS,
                  _Outcome = win).
    
casino_loses_heads(Cfg) ->
    {Player, Casino} = ?config(roles, Cfg),
    Stake = 10,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = "abc",
                  _CasinoGuess = ?HEADS,
                  _ActualSide = ?TAILS,
                  _Outcome = lose).

small_amount(Cfg) ->
    {Player, Casino} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 1,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = "abc",
                  _CasinoGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

big_amount(Cfg) ->
    {Player, Casino} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 100,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = "abc",
                  _CasinoGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

really_short_key(Cfg) ->
    {Player, Casino} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 1,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = "",
                  _CasinoGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

long_key(Cfg) ->
    {Player, Casino} = proplists:get_value(roles, Cfg, {initiator, responder}),
    Stake = 1,
    success_story(Cfg, Player, Casino, Stake,
                  _Key = string:join(lists:duplicate(100, "a"), ""),
                  _CasinoGuess = ?TAILS,
                  _ActualSide = ?TAILS,
                  _Outcome = win).

can_have_a_couple_of_sequential_games(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    Stake = 10,
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    BetRound =
        fun(ActualSide, CasinoGuess) ->
            {ok, Hash} =
                call_offchain_contract(Player, ContractPubkey,
                                      ContractName, "compute_hash",
                                      [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
            {ok, []} =
                call_offchain_contract(Player, ContractPubkey,
                                      ContractName, "provide_hash",
                                      [Hash], Stake, Cfg),
            {ok, []} =
                call_offchain_contract(Casino, ContractPubkey,
                                      ContractName, "casino_pick",
                                      [add_quotes(CasinoGuess)], Stake, Cfg),
            {ok, _Winner} =
                call_offchain_contract(Player, ContractPubkey,
                                      ContractName, "reveal",
                                      [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg)
        end,
    BetRound(?HEADS, ?HEADS), % win
    BetRound(?TAILS, ?HEADS), % lose
    BetRound(?TAILS, ?TAILS), % win
    BetRound(?HEADS, ?HEADS), % win
    BetRound(?HEADS, ?TAILS), % lose
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

player_disputes_casino_inactivity(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    mine_blocks(1),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    mine_blocks(1),
    %% no issue providing a hash
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    {PlayerBalance0, _CasinoBalance0} = get_offchain_balances(Player, Casino, Cfg),
    mine_blocks(ReactionTime),
    %% not allowed yet
    {revert,<<"not_yet_allowed">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "player_dispute_no_pick",
                               [], 0, Cfg),
    %% go beyond the limit of ReactionTime + 1
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "player_dispute_no_pick",
                               [], 0, Cfg),
    {PlayerBalance1, _CasinoBalance0} = get_offchain_balances(Player, Casino, Cfg),
    %% ensure player got their cash back
    assert_equal(PlayerBalance0, PlayerBalance1 - Stake),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

casino_disputes_player_inactivity(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    CasinoGuess = ?TAILS,
    ActualSide = ?TAILS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    mine_blocks(1),
    %% no issue providing a hash
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(CasinoGuess)], Stake, Cfg),
    mine_blocks(ReactionTime),
    %% not allowed yet
    {revert,<<"not_yet_allowed">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_dispute_no_reveal",
                               [], 0, Cfg),
    %% go beyond the limit of ReactionTime + 1
    mine_blocks(1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_dispute_no_reveal",
                               [], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

can_play_after_casino_dispute(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    CasinoGuess1 = ?TAILS,
    ActualSide1 = ?TAILS,
    CasinoGuess2 = ?TAILS,
    ActualSide2 = ?HEADS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide1)], 0, Cfg),
    mine_blocks(1),
    %% no issue providing a hash
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(CasinoGuess1)], Stake, Cfg),
    mine_blocks(ReactionTime + 1),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_dispute_no_reveal",
                               [], 0, Cfg),
    %% ensure can play again
    {ok, Hash2} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide2)], 0, Cfg),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash2], Stake, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(CasinoGuess2)], Stake, Cfg),
    {ok, _Winner} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "reveal",
                              [add_quotes(Key), add_quotes(ActualSide2)], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

can_play_after_player_dispute(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide1 = ?TAILS,
    CasinoGuess2 = ?TAILS,
    ActualSide2 = ?HEADS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide1)], 0, Cfg),
    %% no issue providing a hash
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    mine_blocks(ReactionTime + 1),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "player_dispute_no_pick",
                               [], 0, Cfg),
    %% ensure can play again
    {ok, Hash2} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide2)], 0, Cfg),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash2], Stake, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(CasinoGuess2)], Stake, Cfg),
    {ok, _Winner} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "reveal",
                              [add_quotes(Key), add_quotes(ActualSide2)], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.


success_story(Cfg0, Player, Casino, Stake, Key, CasinoGuess,
              ActualSide, Outcome) ->
    ReactionTime = 30,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    {PlayerBalance0, CasinoBalance0} = get_offchain_balances(Player, Casino, Cfg),
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(CasinoGuess)], Stake, Cfg),
    {ok, Winner} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "reveal",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    {PlayerBalance1, CasinoBalance1} = get_offchain_balances(Player, Casino, Cfg),
    case Outcome of
        win ->
            %% player lost the stake, casino won it
            assert_equal(Winner, participant_address(Casino, Cfg)),
            assert_equal(PlayerBalance0, PlayerBalance1 + Stake),
            assert_equal(CasinoBalance0, CasinoBalance1 - Stake);
        lose ->
            %% player won the stake, casino lost it
            assert_equal(Winner, participant_address(Player, Cfg)),
            assert_equal(PlayerBalance0, PlayerBalance1 - Stake),
            assert_equal(CasinoBalance0, CasinoBalance1 + Stake)
    end,
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

provide_hash_fails(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    {ok, OtherHash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes("this is irrelevant but yet different"), add_quotes(ActualSide)], 0, Cfg),
    true = OtherHash =/= Hash,
    %% casino can not provide hash 
    {revert, <<"not_player">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    %% no issue player providing a hash
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    %% casino still can not replace hash 
    {revert, <<"not_player">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [OtherHash], Stake, Cfg),
    %% player can not replace hash 
    {revert, <<"already_has_hash">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [OtherHash], Stake, Cfg),
    %% yet game is still playable
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(ActualSide)], Stake, Cfg),
    %% casino still can not replace hash 
    {revert, <<"not_player">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "provide_hash",
                               [OtherHash], Stake, Cfg),
    %% player can not replace hash 
    {revert, <<"already_has_hash">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [OtherHash], Stake, Cfg),
    {ok, _Winner} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "reveal",
                              [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

casino_pick_fails(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Bet = ?HEADS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    %% player can not call casino_pick 
    {revert, <<"not_casino">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(Bet)], Stake, Cfg),
    %% casino can not pick while no game
    {revert, <<"no_hash">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(Bet)], Stake, Cfg),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    %% let the game start
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    %% player still can not call casino_pick
    {revert, <<"not_casino">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(Bet)], Stake, Cfg),
    %% casino can not use random string as a pick
    {revert, <<"invalid_coin_side">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(<<"random string">>)], Stake, Cfg),
    %% casino cannot stake too much
    StakeBin = integer_to_binary(Stake),
    WrongStakeMsg = <<"wrong_stake, expected ", StakeBin/binary>>,
    {revert, WrongStakeMsg} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(Bet)], Stake + 1, Cfg),
    %% casino cannot stake too little
    {revert, WrongStakeMsg} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(Bet)], Stake - 1, Cfg),
    %% casino can pick
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(Bet)], Stake, Cfg),
    %% player still can not call casino_pick
    {revert, <<"not_casino">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(Bet)], Stake, Cfg),
    %% casino can not overwrite 
    {revert, <<"there_is_a_pick_already">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(Bet)], Stake, Cfg),
    {ok, _Winner} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "reveal",
                              [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    %% player can not call casino_pick
    {revert, <<"not_casino">>} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(Bet)], Stake, Cfg),
    %% casino can not pick while no game
    {revert, <<"no_hash">>} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(Bet)], Stake, Cfg),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

reveal_fails(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Bet = ?HEADS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    CorrectRevealFun =
        fun(Who) ->
            call_offchain_contract(Who, ContractPubkey,
                                   ContractName, "reveal",
                                   [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg)
        end,
    %% casino can not reveal
    {revert, <<"not_player">>} = CorrectRevealFun(Casino),
    %% player can not reveal while no game
    {revert, <<"there_is_no_pick">>} = CorrectRevealFun(Player),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    %% let the game start
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    %% casino still can not reveal
    {revert, <<"not_player">>} = CorrectRevealFun(Casino),
    %% player can not reveal while no pick
    {revert, <<"there_is_no_pick">>} = CorrectRevealFun(Player),
    %% casino can pick
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(Bet)], Stake, Cfg),
    %% casino still can not reveal
    {revert, <<"not_player">>} = CorrectRevealFun(Casino),
    %% player can not post wrong key
    OtherKey = <<"asdf">>,
    true = Key =/= OtherKey,
    {revert, <<"invalid_key_and_answer">>} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "reveal",
                              [add_quotes(OtherKey), add_quotes(ActualSide)], 0, Cfg),
    %% player can not post wrong coin side
    {revert, <<"invalid_coin_side">>} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "reveal",
                              [add_quotes(Key), add_quotes(<<"random wrong side">>)], 0, Cfg),
    {ok, _Winner} = CorrectRevealFun(Player),
    %% casino can not reveal as no game
    {revert, <<"not_player">>} = CorrectRevealFun(Casino),
    %% player can not reveal while no casino pick
    {revert, <<"there_is_no_pick">>} = CorrectRevealFun(Player),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.


casino_dispute_fails(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Bet = ?HEADS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    CorrectDisputeFun =
        fun(Who) ->
            call_offchain_contract(Who, ContractPubkey,
                                   ContractName, "casino_dispute_no_reveal",
                                   [], 0, Cfg)
        end,
    %% player can not dispute
    {revert, <<"not_casino">>} = CorrectDisputeFun(Player),
    %% casino can not dispute while no game
    {revert, <<"there_is_no_pick">>} = CorrectDisputeFun(Casino),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    %% let the game start
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    %% player can not dispute
    {revert, <<"not_casino">>} = CorrectDisputeFun(Player),
    %% casino can not dispute while no casino pick
    {revert, <<"there_is_no_pick">>} = CorrectDisputeFun(Casino),
    %% casino can pick
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(Bet)], Stake, Cfg),
    %% player can not dispute
    {revert, <<"not_casino">>} = CorrectDisputeFun(Player),
    %% casino can not dispute while not matured
    {revert, <<"not_yet_allowed">>} = CorrectDisputeFun(Casino),
    %% after the reaction time expires, casino can dispute
    mine_blocks(ReactionTime + 1),
    {ok, []} = CorrectDisputeFun(Casino),
    %% player can not dispute
    {revert, <<"not_casino">>} = CorrectDisputeFun(Player),
    %% casino can not dispute while no game
    {revert, <<"there_is_no_pick">>} = CorrectDisputeFun(Casino),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

player_dispute_fails(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Stake = 1,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg),
    CorrectDisputeFun =
        fun(Who) ->
            call_offchain_contract(Who, ContractPubkey,
                                   ContractName, "player_dispute_no_pick",
                                   [], 0, Cfg)
        end,
    %% casino can not dispute
    {revert, <<"not_player">>} = CorrectDisputeFun(Casino),
    %% player can not dispute while no game
    {revert, <<"no_hash">>} = CorrectDisputeFun(Player),
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    %% let the game start
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    %% casino can not dispute
    {revert, <<"not_player">>} = CorrectDisputeFun(Casino),
    %% player can not dispute while no game
    {revert, <<"not_yet_allowed">>} = CorrectDisputeFun(Player),
    mine_blocks(ReactionTime + 1),
    {ok, []} = CorrectDisputeFun(Player),
    %% casino can not dispute
    {revert, <<"not_player">>} = CorrectDisputeFun(Casino),
    %% casino can not dispute while no game
    {revert, <<"no_hash">>} = CorrectDisputeFun(Player),
    %% play another game to prove player can not dispute while there is an
    %% actual game going on
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(ActualSide)], Stake, Cfg),
    %% casino can not dispute
    {revert, <<"not_player">>} = CorrectDisputeFun(Casino),
    %% casino can not dispute while no game
    {revert, <<"there_is_a_pick_already">>} = CorrectDisputeFun(Player),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

intended_usage(Cfg0) ->
    {Player, Casino} = proplists:get_value(roles, Cfg0, {initiator, responder}),
    Key = "qwerty",
    ActualSide = ?TAILS,
    Stake = 100,
    ReactionTime = 5,
    Cfg = channel_open(Cfg0),
    {PlayerAddress, CasinoAddress} = prepare_for_test(Player, Casino, Cfg),
    Args = [PlayerAddress, CasinoAddress, integer_to_list(ReactionTime)],
    ContractName = coin_toss,
    %% At first create the contract in the off-chain state tree. We will later
    %% on call it by its pubkey;
    %% At contract creation time we don't provide initial deposit. The player
    %% participant will provide stake with providing a hash
    ContractPubkey = create_offchain_contract(Player, ContractName, Args, Cfg,
                                              0),
    %% The player picks heads or tails and a random string used for source for
    %% entropy. This must be done aside (ex. via a dry run) as both the pick
    %% and the key string must be kept secret
    {ok, Hash} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    %% The game starts as the player provides the hash; Note that it
    %% includes an amount to be dedicated to the contract as well
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash], Stake, Cfg),
    %% The casino picks heads or tails and provides this to the contract The
    %% casino spends as many tokens as they like. The amount staked by the
    %% casino must be at most the amount dedicated to the contract as this
    %% is as much as one might win Here the casino will win all tokens
    %% staked
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                              ContractName, "casino_pick",
                              [add_quotes(ActualSide)],
                              Stake, %% note the same amount staked by the player
                              Cfg),
    %% The player reveals both the key string and the actual coin side
    {ok, Winner} =
        call_offchain_contract(Player, ContractPubkey,
                              ContractName, "reveal",
                              [add_quotes(Key), add_quotes(ActualSide)], 0, Cfg),
    %% casino won
    assert_equal(Winner, participant_address(Casino, Cfg)),
    %% Since the previous game ended, now player must provide more tokens to
    %% contract with the hash
    {ok, Hash2} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "compute_hash",
                               [add_quotes(<<"some other key">>), %% note that the key should be different
                                add_quotes(?TAILS)],
                                0,
                                Cfg),
    {ok, []} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "provide_hash",
                               [Hash2],
                               Stake, %% player provides stake with the hash itself
                               Cfg),
    %% This time the casino loses
    {ok, []} =
        call_offchain_contract(Casino, ContractPubkey,
                               ContractName, "casino_pick",
                               [add_quotes(?HEADS)],
                               Stake, %% note the amount staked by the casino
                               Cfg),
    %% player reveals and casino loses the game
    {ok, Winner2} =
        call_offchain_contract(Player, ContractPubkey,
                               ContractName, "reveal",
                               [add_quotes(<<"some other key">>),
                                add_quotes(?TAILS)], 0, Cfg),
    assert_equal(Winner2, participant_address(Player, Cfg)),
    aehttp_sc_SUITE:sc_ws_close_mutual_(Cfg, Casino),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal helper funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    {ok, BinCode} = aect_test_utils:read_contract(?SOPHIA_IRIS_FATE, Contract),
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
    create_offchain_contract(Who, ContractName, Args, Cfg, 0).

create_offchain_contract(Who, ContractName, Args, Cfg, Deposit) ->
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
                                         UpdateVolley, Cfg, Deposit),
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

prepare_for_test(Player, Casino, Cfg) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    Pids = [IConnPid, RConnPid],
    AllEvents = [sign, info, get, error, update],
    ok = register_channel_events(AllEvents, Pids),
    {PlayerPubkey, CasinoPubkey} =
        case {Player, Casino} of
            {initiator, responder} -> {IPubkey, RPubkey};
            {responder, initiator} -> {RPubkey, IPubkey}
        end,
    PlayerAddress = aeser_api_encoder:encode(account_pubkey, PlayerPubkey),
    CasinoAddress = aeser_api_encoder:encode(account_pubkey, CasinoPubkey),
    {PlayerAddress, CasinoAddress}.

get_offchain_balances(Player, Casino, Cfg) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Cfg),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, Cfg),
    {PlayerPubkey, CasinoPubkey} =
        case {Player, Casino} of
            {initiator, responder} -> {IPubkey, RPubkey};
            {responder, initiator} -> {RPubkey, IPubkey}
        end,
    {ok, {PBal, CBal}} =
        aehttp_sc_SUITE:sc_ws_get_both_balances(IConnPid,
                                                PlayerPubkey,
                                                CasinoPubkey,
                                                Cfg),
    {ok, {PBal, CBal}} =
        aehttp_sc_SUITE:sc_ws_get_both_balances(RConnPid,
                                                PlayerPubkey,
                                                CasinoPubkey,
                                                Cfg),
    {PBal, CBal}.

assert_equal(Val1, Val2) ->
    {Val1, Val1} = {Val1, Val2}.

participant_address(Who, Cfg) ->
    Participants = proplists:get_value(participants, Cfg),
    #{pub_key := Pubkey} = maps:get(Who, Participants),
    aeser_api_encoder:encode(account_pubkey, Pubkey).


