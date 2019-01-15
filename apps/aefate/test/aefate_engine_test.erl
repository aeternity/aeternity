%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate engine
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_engine_test).

-include_lib("eunit/include/eunit.hrl").

control_flow_test_() ->
    make_calls(control_flow()).

boolean_test_() ->
    make_calls(booleans()).

comp_test_() ->
    make_calls(comps()).

arith_test_() ->
    make_calls(arith()).

jumpif_test_() ->
    make_calls(conditional_jump()).


memory_test_() ->
    make_calls(memory()).

memory_restore_test_() ->
    make_calls(memory_restore()).


make_calls(ListOfCalls) ->
    Chain = setup_chain(),
    [{lists:flatten(io_lib:format("call(~p,~p,~p)->~p", [C,F,A,R])),
      fun() ->
              Call = make_call(C,F,A),
              #{accumulator := Res,
                trace := Trace} = aefa_fate:run(Call, Chain),
              ?assertEqual({R, Trace}, {Res, Trace})

      end}
     || {C,F,A,R} <- ListOfCalls].



control_flow() ->
    [ {<<"test">>, <<"id">>, [42], 42}
    , {<<"test">>, <<"jumps">>, [], 0}
    , {<<"test">>, <<"inc">>, [0], 2}
    , {<<"test">>, <<"call">>, [0], 4}
    , {<<"test">>, <<"tailcall">>, [0], 3}
    , {<<"remote">>, <<"add_five">>, [1], 6}
    , {<<"test">>, <<"remote_call">>, [4],10}
    , {<<"test">>, <<"remote_tailcall">>, [4],9}
    ].

booleans() ->
    [ {<<"bool">>, <<"and">>, [true, true], true}
    , {<<"bool">>, <<"and">>, [true, false], false}
    , {<<"bool">>, <<"or">>,  [true, false], true}
    , {<<"bool">>, <<"not">>,  [true], false}
    , {<<"bool">>, <<"not">>,  [false], true}
    ].

comps() ->
    [ {<<"comp">>, <<"lt">>, [1, 2], true}
    , {<<"comp">>, <<"gt">>, [1, 2], false}
    , {<<"comp">>, <<"elt">>,  [2, 2], true}
    , {<<"comp">>, <<"egt">>,  [3, 2], true}
    , {<<"comp">>, <<"eq">>,  [4, 4], true}
    ].

arith() ->
    [ {<<"arith">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"add">>, [1, 2], 3}
            , {<<"sub">>, [3, 2], 1}
            , {<<"mul">>, [2, 2], 4}
            , {<<"div">>, [8, 4], 2}
            , {<<"mod">>, [9, 4], 1}
            ]
    ].

conditional_jump() ->
    [ {<<"jumpif">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"skip">>, [0, 42], 42}
            , {<<"skip">>, [1, 42], 43}
            ]
    ].

memory() ->
    [ {<<"memory">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"dest_add">>, [1, 2], 3}
            ,  {<<"dest_add_imm">>, [1], 3}
            ,  {<<"dest_add_stack">>, [1, 2], 3}
            ]
    ].


memory_restore() ->
    [ {<<"memory">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"call">>, [17], 17}
            ]
    ].


make_call(Contract, Function, Arguments) ->
    #{ contract  => Contract
     , function  => Function
     , arguments => Arguments}.


setup_chain() ->
    #{ contracts => setup_contracts()}.


setup_contracts() ->
    #{ <<"test">> =>
           [ {<<"id">>, {[integer], integer}, [{0, [{return_r, {arg, 0}}]}]}
           , {<<"jumps">>, {[], integer},
              [{0, [ push_a_0
                   , {jump, 3}]}
              ,{1, [ nop ]}
              ,{2, [ nop
                   , return]}
              , {3, [ nop
                    , {jump, 1}]}]
             }
           , {<<"inc">>, {[integer],integer},
              [{0, [ {inc_a_1_r, {arg,0}}
                   , inc_a_1_a
                   , return
                   ]}]
             }
           , {<<"call">>, {[integer],integer},
              [{0, [ {inc_a_1_r, {arg, 0}}
                   , {call_local, <<"inc">>}]}
              ,{1, [ inc_a_1_a
                   , return]}
              ]
             }
           , {<<"tailcall">>, {[integer],integer},
              [{0, [ {inc_a_1_r, {arg,0}}
                   , {tailcall_local, <<"inc">>}]}
              ]
             }
           , { <<"remote_call">>
             , {[integer],integer}
             , [ {0, [ {push, {arg,0}},
                       {call_remote, <<"remote">>, <<"add_five">>} ]}
               , {1, [ inc_a_1_a,
                       return]}
               ]
             }
           , { <<"remote_tailcall">>
             , {[integer],integer}
             , [ {0, [ {push, {arg,0}},
                       {tailcall_remote, <<"remote">>, <<"add_five">>} ]}
               ]
             }
           ]
     , <<"remote">> =>
           [ {<<"add_five">>, {[integer], integer},
              [{0, [{add_a_i_r, 5, {arg, 0}}
                    , return]}]
             }
           ]
     , <<"bool">> =>
           [ {<<"and">>
             , {[boolean, boolean], boolean}
             , [ {0, [ {and_a_r_r, {arg, 0}, {arg, 1}}
                     , return]}]}
           , {<<"or">>
             , {[boolean, boolean], boolean}
             , [ {0, [ {or_a_r_r, {arg, 0}, {arg, 1}}
                     , return]}]}
           , {<<"not">>
             , {[boolean], boolean}
             , [ {0, [ {not_a_r, {arg, 0}}
                     , return]}]}
           ]

     , <<"comp">> =>
           [ {<<"lt">>
             , {[integer, integer], boolean}
             , [ {0, [ {lt_a_r_r, {arg, 0}, {arg, 1}}
                     , return]}]}
           ,  {<<"gt">>
              , {[integer, integer], boolean}
              , [ {0, [ {gt_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"egt">>
              , {[integer, integer], boolean}
              , [ {0, [ {egt_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"elt">>
              , {[integer, integer], boolean}
              , [ {0, [ {elt_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"eq">>
              , {[integer, integer], boolean}
              , [ {0, [ {eq_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"neq">>
              , {[integer, integer], boolean}
              , [ {0, [ {neq_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ]
     , <<"arith">> =>
           [ {<<"add">>
             , {[integer, integer], integer}
             , [ {0, [ {add_a_r_r, {arg, 0}, {arg, 1}}
                     , return]}]}
           ,  {<<"sub">>
              , {[integer, integer], integer}
              , [ {0, [ {sub_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"mul">>
              , {[integer, integer], integer}
              , [ {0, [ {mul_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"div">>
              , {[integer, integer], integer}
              , [ {0, [ {div_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"mod">>
              , {[integer, integer], integer}
              , [ {0, [ {mod_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"pow">>
              , {[integer, integer], integer}
              , [ {0, [ {pow_a_r_r, {arg, 0}, {arg, 1}}
                      , return]}]}
           ]
     , <<"jumpif">> =>
           [ {<<"skip">>
             , {[integer, integer], integer}
             , [ {0, [ {push, {arg, 1}}
                     , push_a_0
                     , {eq_a_a_r, {arg, 0}}
                     , {jumpif_a, 2}
                     ]}
               , {1, [ inc_a_1_a ]}
               , {2, [ return]}
               ]}
           ]
     , <<"memory">> =>
           [ {<<"call">>
             , {[integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , push_a_0
                     , {call_local, <<"write">>}
                     ]
                 }
               , {1, [ {push, {var, 1}}
                     , return
                     ]}
               ]}
           , {<<"write">>
             , {[integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , return
                     ]}
               ]}
           , {<<"dest_add">>
             , {[integer, integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , {store, {var, 2}, {arg, 1}}
                     , {add_r_r_r, {var, 3}, {var, 1}, {var, 2}}
                     , {push, {var, 3}}
                     , return
                     ]}
               ]}
           , {<<"dest_add_imm">>
             , {[integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , {add_r_r_r, {var, 3}, {var, 1}, {immediate, 2}}
                     , {push, {var, 3}}
                     , return
                     ]}
               ]}
           , {<<"dest_add_stack">>
             , {[integer, integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , {push, {arg, 1}}
                     , {add_r_r_r, {var, 3}, {var, 1}, {stack, 0}}
                     , {push, {var, 3}}
                     , return
                     ]}
               ]}
           ]

       }.


