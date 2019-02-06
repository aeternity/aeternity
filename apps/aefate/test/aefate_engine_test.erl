%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate engine
%%%  make TEST=aefate_engine_test eunit
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


tuple_test_() ->
    make_calls(tuple()).

map_test_() ->
    make_calls(map()).

list_test_() ->
    make_calls(list()).

string_test_() ->
    make_calls(string()).

bits_test_() ->
    make_calls(bits()).

make_calls(ListOfCalls) ->
    Chain = setup_chain(),
    [{lists:flatten(io_lib:format("call(~p,~p,~p)->~p~n~p : ~p", [C,F,A,R,
                                                             aefa_data:encode(A),
                                                             aefa_encoding:serialize(aefa_data:encode(A))])),
      fun() ->
              FateArgs = aefa_encoding:serialize(aefa_data:encode(A)),
              FateRes = aefa_data:encode(R),
              Call = make_call(C,F,FateArgs),
              #{accumulator := Res,
                trace := Trace} = aefa_fate:run(Call, Chain),
              ?assertEqual({FateRes, Trace}, {Res, Trace})

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

tuple() ->
    [ {<<"tuple">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"make_2tuple">>, [1, 2], {1, 2}}
            ,  {<<"make_0tuple">>, [], {}}
            ,  {<<"make_5tuple">>, [1, 2, 3, 4, 5], {1, 2, 3, 4, 5}}
            ,  { <<"element1">>, [1, 2], 2}
            ,  { <<"element">>, [{1, 2}, 0], 1}
            ]
    ].

map() ->
    [ {<<"map">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"make_empty_map">>, [], #{}}
            ,  {<<"map_update">>, [#{},42,true], #{42 => true}}
            ,  {<<"map_lookup">>, [#{42 => true}, 42], true}
            ,  {<<"map_lookup_default">>, [#{42 => true}, 17], false}
            ,  {<<"map_member">>, [#{42 => false}, 42], true}
            ,  {<<"map_member">>, [#{42 => false}, 17], false}
            ,  {<<"map_from_list">>, [[{1, true}, {2, false}, {42, true}]],
                #{ 1 => true, 2 => false, 42 => true}}
            ]
    ].


list() ->
    [ {<<"list">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"make_nil">>, [], []}
            ,  {<<"cons">>, [42,[]], [42]}
            ,  {<<"head">>, [[42]], 42}
            ,  {<<"tail">>, [[42]], []}
            ,  {<<"length">>, [[1,2,3,4]], 4}
            ]
    ].


string() ->
    [ {<<"string">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"str_equal">>, [<<"">>, <<"">>], true}
            , {<<"str_equal">>, [<<"">>, <<"1">>], false}
            , {<<"str_equal">>, [<<"1">>, <<"1">>], true}
            , {<<"str_equal">>, [<<"a longer string">>, <<"a longer string">>], true}
            , {<<"str_equal">>, [<<"a longer string">>, <<"a longer String">>], false}
            , {<<"str_join">>, [<<"">>, <<"">>], <<"">>}
            , {<<"str_join">>, [<<"1">>, <<"">>], <<"1">>}
            , {<<"str_join">>, [<<"">>, <<"1">>], <<"1">>}
            , {<<"str_join">>, [<<"a longer ">>, <<"string">>], <<"a longer string">>}
            , {<<"int_to_str">>, [0], <<"0">>}
            , {<<"int_to_str">>, [1], <<"1">>}
            , {<<"int_to_str">>, [4711], <<"4711">>}
            , {<<"int_to_addr">>, [0], {address, <<"1">>}}
            , {<<"int_to_addr">>, [1], {address, <<"2">>}}
            , {<<"int_to_addr">>, [375576359605656387439355756185499065667174933136290370646365126117934444451883981459509],
               {address, <<"2U9MkZK9JXTUemAURfCd8BDQZcXK4Gk8Hwfqxf1ASSYNrQnhjz">>}}
            , {<<"addr_to_str">>, [{address, <<"2U9MkZK9JXTUemAURfCd8BDQZcXK4Gk8Hwfqxf1ASSYNrQnhjz">>}],
               <<"2U9MkZK9JXTUemAURfCd8BDQZcXK4Gk8Hwfqxf1ASSYNrQnhjz">>}
            , {<<"str_reverse">>, [<<"">>], <<"">>}
            , {<<"str_reverse">>, [<<"1">>], <<"1">>}
            , {<<"str_reverse">>, [<<"12">>], <<"21">>}
            , {<<"str_reverse">>, [<<"123">>], <<"321">>}
            , {<<"str_reverse">>, [<<"1234">>], <<"4321">>}
            , {<<"str_reverse">>, [<<"12345">>], <<"54321">>}
            , {<<"str_reverse">>, [<<"123456789">>], <<"987654321">>}
            , {<<"str_reverse">>, [<<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>],
               <<"zyxwvutsrqponmkjihgfedcbaZYXWVUTSRQPNMLKJHGFEDCBA987654321">>}
            ]
    ].


bits() ->
    [ {<<"bits">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"all">>, [], {bits, -1}}
            , {<<"none">>, [], {bits, 0}}
            , {<<"all_n">>, [4], {bits, 15}}
            , {<<"set">>, [3], {bits, 8}}
            , {<<"clear">>, [0], {bits, -2}}
            , {<<"test">>, [{bits, 15}, 1], true}
            , {<<"sum">>, [{bits,15}], 4}
            , {<<"union">>, [{bits,1}, {bits, 2}], {bits, 3}}
            , {<<"intersection">>, [{bits,15}, {bits, 2}], {bits, 2}}
            , {<<"difference">>, [{bits,15}, {bits, 2}], {bits, 13}}
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
              [{0, [{add, {stack, 0}, {immediate, 5}, {arg, 0}}
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
             , [ {0, [ {add, {stack, 0}, {arg, 0}, {arg, 1}}
                     , return]}]}
           ,  {<<"sub">>
              , {[integer, integer], integer}
              , [ {0, [ {sub, {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"mul">>
              , {[integer, integer], integer}
              , [ {0, [ {mul, {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"div">>
              , {[integer, integer], integer}
              , [ {0, [ {'div',  {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"mod">>
              , {[integer, integer], integer}
              , [ {0, [ {mod, {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"pow">>
              , {[integer, integer], integer}
              , [ {0, [ {pow, {stack, 0}, {arg, 0}, {arg, 1}}
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
                     , {return_r, {var, 1}}
                     ]}
               ]}
           , {<<"dest_add">>
             , {[integer, integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , {store, {var, 2}, {arg, 1}}
                     , {add, {var, 3}, {var, 1}, {var, 2}}
                     , {push, {var, 3}}
                     , return
                     ]}
               ]}
           , {<<"dest_add_imm">>
             , {[integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , {add, {var, 3}, {var, 1}, {immediate, 2}}
                     , {push, {var, 3}}
                     , return
                     ]}
               ]}
           , {<<"dest_add_stack">>
             , {[integer, integer], integer}
             , [ {0, [ {store, {var, 1}, {arg, 0}}
                     , {push, {arg, 1}}
                     , {add, {var, 3}, {var, 1}, {stack, 0}}
                     , {push, {var, 3}}
                     , return
                     ]}
               ]}
           ]
     , <<"tuple">> =>
           [ {<<"make_0tuple">>
             , {[], {tuple, []}}
             , [ {0, [ {make_tuple, 0}
                     , return]}
               ]}
           , {<<"make_2tuple">>
             , {[integer, integer], {tuple, [integer, integer]}}
             , [ {0, [ {push, {arg, 0}}
                     , {push, {arg, 1}}
                     , {make_tuple, 2}
                     ,  return]}
               ]}
           , {<<"make_5tuple">>
             , {[integer, integer, integer, integer, integer],
                {tuple, [integer, integer, integer, integer, integer]}}
             , [ {0, [ {push, {arg, 0}}
                     , {push, {arg, 1}}
                     , {push, {arg, 2}}
                     , {push, {arg, 3}}
                     , {push, {arg, 4}}
                     , {make_tuple, 5}
                     ,  return]}
               ]}
           , {<<"element1">>
             , {[integer, integer], integer}
             , [ {0, [ {push, {arg, 0}}
                     , {push, {arg, 1}}
                     , {make_tuple, 2}
                     , {element, integer, {stack, 0}, {immediate, 1}, {stack, 0}}
                     , return]}
               ]}
           , {<<"element">>
             , {[{tuple, [integer, integer]}, integer], integer}
             , [ {0, [ {element, integer, {stack, 0}, {arg, 1}, {arg, 0}}
                     , return]}
               ]}
           ]
     , <<"map">> =>
           [ {<<"make_empty_map">>
             , {[], {map, integer, boolean}}
             , [ {0, [ {map_empty, {stack, 0}}
                     , return]}
               ]}
           , {<<"map_update">>
             , {[{map, integer, boolean}, integer, boolean], {map, integer, boolean}}
             , [ {0, [ {map_update, {stack, 0}, {arg, 0}, {arg, 1}, {arg, 2}}
                     ,  return]}
               ]}
           , {<<"map_lookup">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {map_lookup, {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  return]}
               ]}
           , {<<"map_lookup_default">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {map_lookup_default, {stack, 0}, {arg, 0}, {arg, 1}, {immediate, false}}
                     ,  return]}
               ]}
           , {<<"map_member">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {map_member, {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  return]}
               ]}
           , {<<"map_member">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {map_member, {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  return]}
               ]}
           , {<<"map_from_list">>
             , {[{list, {tuple, [integer, boolean]}}], {map, integer, boolean}}
               , [ {0, [ {map_from_list, {stack, 0}, {arg, 0}}
                       , return]}
                 ]}
             ]
     , <<"list">> =>
           [ {<<"make_nil">>
             , {[], {list, integer}}
             , [ {0, [ {nil, {stack, 0}}
                     , return]}
               ]}
           , {<<"cons">>
             , {[integer, {list, integer}], {list, integer}}
             , [ {0, [ {cons, {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  return]}
               ]}
           , {<<"head">>
             , {[{list, integer}], integer}
             , [ {0, [ {hd, {stack, 0}, {arg, 0}}
                     ,  return]}
               ]}
           , {<<"tail">>
             , {[{list, integer}], {list, integer}}
             , [ {0, [ {tl, {stack, 0}, {arg, 0}}
                     , return]}
               ]}
           , {<<"length">>
             , {[{list, integer}], integer}
             , [ {0, [ {length, {stack, 0}, {arg, 0}}
                     , return]}
               ]}
           ]
     , <<"string">> =>
           [ {<<"str_equal">>
             , {[string, string], boolean}
             , [ {0, [ {str_equal, {stack, 0}, {arg, 0}, {arg, 1}}
                     , return]}
               ]}
           , {<<"str_join">>
             , {[string, string], string}
             , [ {0, [ {str_join, {stack, 0}, {arg, 0}, {arg, 1}}
                     , return]}
               ]}
           , {<<"int_to_str">>
             , {[integer], string}
             , [ {0, [ {int_to_str, {stack, 0}, {arg, 0}}
                     , return]}
               ]}
           , {<<"addr_to_str">>
             , {[address], string}
             , [ {0, [ {addr_to_str, {stack, 0}, {arg, 0}}
                     , return]}
               ]}
           , {<<"int_to_addr">>
             , {[integer], address}
             , [ {0, [ {int_to_addr, {stack, 0}, {arg, 0}}
                     , return]}
               ]}
           , {<<"str_reverse">>
             , {[string], string}
             , [ {0, [ {str_reverse, {stack, 0}, {arg, 0}}
                     , return]}
               ]}
           ]

     , <<"bits">> =>
           [ {<<"all">>
             , {[], bits}
             , [ {0, [ bits_all
                     , return]}]}
           , {<<"all_n">>
             , {[integer], bits}
             , [ {0, [ {bits_all_n, {stack, 0}, {arg, 0}}
                     , return]}]}
           ,  {<<"none">>
              , {[], bits}
              , [ {0, [ bits_none
                      , return]}]}
           ,  {<<"set">>
              , {[integer], bits}
              , [ {0, [ {bits_none, {var, 1}}
                      , {bits_set, {stack, 0}, {var, 1}, {arg, 0}}
                      , return]}]}
           ,  {<<"clear">>
              , {[integer], bits}
              , [ {0, [ {bits_all, {var, 1}}
                      , {bits_clear, {stack, 0}, {var, 1}, {arg, 0}}
                      , return]}]}
           ,  {<<"test">>
              , {[bits, integer], boolean}
              , [ {0, [ {bits_test, {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"sum">>
              , {[bits], integer}
              , [ {0, [ {bits_sum, {stack, 0}, {arg, 0}}
                      , return]}]}
           ,  {<<"union">>
              , {[bits, bits], bits}
              , [ {0, [ {bits_union, {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"intersection">>
              , {[bits, bits], bits}
              , [ {0, [ {bits_intersection, {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}
           ,  {<<"difference">>
              , {[bits, bits], bits}
              , [ {0, [ {bits_difference, {stack, 0}, {arg, 0}, {arg, 1}}
                      , return]}]}

           ]

       }.


