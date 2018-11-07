%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_pow_cuckoo module
%%% @end
%%%=============================================================================
-module(aec_pow_cuckoo_tests).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("pow.hrl").

-define(TEST_MODULE, aec_pow_cuckoo).

-define(TEST_BIN, <<"wsffgujnjkqhduihsahswgdf">>).

-define(TEST_HIGH_NONCE, 74). %% Nonce with solution with high target.

pow_test_() ->
    {setup,
     fun() ->
             ok = meck:new(aeu_env, [passthrough]),
             aec_test_utils:mock_fast_and_deterministic_cuckoo_pow(),
             ok = application:ensure_started(erlexec)
     end,
     fun(_) ->
             ok = meck:unload(aeu_env)
     end,
     [{"Generate with a winning nonce and high target threshold, verify it",
       {timeout, 60,
        fun() ->
                Target = ?HIGHEST_TARGET_SCI,
                Nonce = ?TEST_HIGH_NONCE,
                Res = ?TEST_MODULE:generate(?TEST_BIN, Target, Nonce),
                {ok, {Nonce, Soln}} = Res,
                ?assertMatch(L when length(L) == 42, Soln),

                %% verify the nonce and the solution
                Res2 = ?TEST_MODULE:verify(?TEST_BIN, Nonce, Soln, Target),
                ?assert(Res2)
        end}
      },
      {"Generate with a winning nonce but low target threshold, shall fail",
       {timeout, 90,
        fun() ->
                Target = 16#01010000,
                Nonce = ?TEST_HIGH_NONCE,
                Res = ?TEST_MODULE:generate(?TEST_BIN, Target, Nonce),
                ?assertEqual({error, no_solution}, Res),

                %% Any attempts to verify such nonce with a solution
                %% found with high target threshold shall fail.
                %%
                %% Obtain solution with high target threshold ...
                HighTarget = ?HIGHEST_TARGET_SCI,
                {ok, {Nonce, Soln2}} =
                    ?TEST_MODULE:generate(?TEST_BIN, HighTarget, Nonce),
                ?assertMatch(L when length(L) == 42, Soln2),
                %% ... then attempt to verify such solution (and
                %% nonce) with the low target threshold (shall fail).
                ?assertNot(?TEST_MODULE:verify(?TEST_BIN, Nonce, Soln2, Target))
        end}
      },
      {"Attempt to verify wrong solution for nonce that has a solution shall fail",
       fun() ->
               Target = ?HIGHEST_TARGET_SCI,
               Nonce = ?TEST_HIGH_NONCE,
               Res = ?TEST_MODULE:generate(?TEST_BIN, Target, Nonce),
               {ok, {Nonce, Soln}} = Res,
               ?assertMatch(L when length(L) == 42, Soln),

               WrongSoln = lists:seq(0, 41),
               ?assertMatch(L when length(L) == 42, WrongSoln),
               ?assertNotEqual(Soln, WrongSoln),
               ?assertNot(?TEST_MODULE:verify(?TEST_BIN, Nonce, WrongSoln, Target))
       end},
      {"Attempt to verify nonce that does not have a solution (providing a dummy solution) shall fail",
       fun() ->
               Target = ?HIGHEST_TARGET_SCI,
               Nonce = 1,
               ?assertMatch({error, no_solution},
                            ?TEST_MODULE:generate(?TEST_BIN, Target, Nonce)),

               DummySoln = lists:seq(0, 41),
               ?assertMatch(L when length(L) == 42, DummySoln),
               ?assertNot(?TEST_MODULE:verify(?TEST_BIN, Nonce, DummySoln, Target))
       end},
      {"Attempt to verify nonce that is too big shall fail gracefully",
       fun() ->
               % this is a premined working solution for size 27
               Hash = <<83,237,15,231,60,2,35,26,173,64,55,84,59,100,88,146,91,
                        124,171,211,193,86,167,83,17,153,168,99,84,72,33,186>>,
               Pow = [2253069,4506519,4850569,8551070,9391218,15176443,22052028,
                      24045664,29484700,31332105,38588547,39046239,43427572,
                      53979472,58387992,60256309,62282050,67357873,68186886,
                      69815968,71809484,73494956,74992447,76953489,82132560,
                      84075861,84934950,85804033,87920415,96539757,96818481,
                      98049225,98464641,98907580,110711166,115480621,117062778,
                      117537386,120015599,125293300,125684682,129332159],
               Nonce = 17654096256755765485,
               Target = 536940240,
               ?assertNot(?TEST_MODULE:verify(Hash, Nonce, Pow, Target))
       end}
     ]
    }.

misc_test_() ->
    {setup,
     fun() -> ok end ,
     fun(_) -> ok end,
     [{"Conversion of a solution to binary",
       fun() ->
               Soln = [5936046,6000450,9980569,10770186,11256679,11557293,
                       12330374,16556162,25308926,27241299,29693321,31019885,
                       38091840,44351975,46970870,55597976,57712943,76763622,
                       78513115,78670397,82776188,82841920,84299614,86421603,
                       87878232,87913313,92453652,93430969,94032236,94428148,
                       97119256,102408900,104747553,108943266,112048126,
                       112561693,118817859,118965199,121744219,122178237,
                       132944539,133889045],
               NodeSize = ?TEST_MODULE:get_node_size(),
               ?assertEqual(42*NodeSize, size(?TEST_MODULE:solution_to_binary(
                                                 lists:sort(Soln), NodeSize * 8, <<>>)))
       end}
     ]
    }.

kill_ospid_miner_test_() ->
    {setup,
     fun() ->
           ok = application:ensure_started(erlexec)
     end,
     fun(_) ->
           application:stop(erlexec)
     end,
     [ {"Run miner in OS and kill it by killing parent",
       fun() ->
            Self = self(),
            ?assertEqual([], exec:which_children()),  %% no zombies around
            Pid = spawn(fun() ->
                          Self ! {aec_pow_cuckoo:generate(?TEST_BIN, 12837272, 128253), self()}
                      end),
            timer:sleep(200),                        %% give some time to start the miner OS pid
            ?assertEqual(1, length(exec:which_children())),  %% We did create a new one.
            exit(Pid, shutdown),
            timer:sleep(1000),                       %% give it some time to kill the miner OS pid
            ?assertEqual([], exec:which_children()), %% at least erlexec believes it died

            Res = os:cmd("ps | grep mean29-generic | grep -v grep"),
            ?assertMatch([], Res)
        end}
     ]
    }.


-endif.
