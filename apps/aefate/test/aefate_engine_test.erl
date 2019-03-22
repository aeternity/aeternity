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

variant_test_() ->
    make_calls(variant()).

bits_test_() ->
    make_calls(bits()).

make_calls(ListOfCalls) ->
    Chain = setup_chain(),
    [{lists:flatten(io_lib:format("call(~p,~p,~p)->~p~n~p : ~p",
                                  [C,F,A,R,
                                   aeb_fate_data:encode(A),
                                   aeb_fate_encoding:serialize(
                                     aeb_fate_data:encode(A))])),
      fun() ->
              Call = make_call(C,F,A),
              case R of
                  {error, E} ->
                      try aefa_fate:run(Call, Chain) of
                          nomatch -> ok
                      catch throw:{Error, #{trace := Trace}} ->
                              ?assertEqual({E, Trace}, {Error, Trace})
                      end;
                  _ ->
                      FateRes = aeb_fate_data:encode(R),
                      #{accumulator := Res,
                        trace := Trace} = aefa_fate:run(Call, Chain),
                      ?assertEqual({FateRes, Trace}, {Res, Trace})
              end
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
            ,  {<<"cons_error">>, [42,[true]], {error, <<"Type error in cons: 42 is not of type boolean">>}}
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
            , {<<"int_to_addr">>, [0], {address, <<0:256>>}}
            , {<<"int_to_addr">>, [1], {address, <<1:256>>}}
            , {<<"int_to_addr">>, [961981404445573111893532125566728879481945428377512160796083762629444620341],
               {address, <<2,32,118,81,108,18,247,243,45,243,82,75,159,144,74,117,157,38,78,99,114,239,
                           217,210,67,40,207,3,52,111,208,53>>}}
            , {<<"addr_to_str">>, [{address, <<2,32,118,81,108,18,247,243,45,243,82,75,
                                               159,144,74,117,157,38,78,99,114,239,
                                               217,210,67,40,207,3,52,111,208,53>>}],
               <<"9JXTUemAURfCd8BDQZcXK4Gk8Hwfqxf1ASSYNrQnhjz">>}
            , {<<"addr_to_str">>, [{address, "9JXTUemAURfCd8BDQZcXK4Gk8Hwfqxf1ASSYNrQnhjz"}],
               <<"9JXTUemAURfCd8BDQZcXK4Gk8Hwfqxf1ASSYNrQnhjz">>}
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


variant() ->
    [ {<<"variant">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"switch">>, [{variant, 2, 0, {}}], 0}
            , {<<"switch">>, [{variant, 2, 1, {42}}], 42}
            , {<<"test">>, [{variant, 2, 1, {42}}, 1], true}
            , {<<"test">>, [{variant, 2, 1, {42}}, 2], false}
            , {<<"element">>, [{variant, 2, 1, {42}}, 1], 42}
            , {<<"make">>, [2, 1, 1, [42]], {variant, 2, 1, {42}}}
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
     , call => aeb_fate_encoding:serialize(
                 {tuple, {Function, {tuple, list_to_tuple(
                                              [aeb_fate_data:encode(A) || A <- Arguments]
                                             )}
                         }
                 }
                )
     }.


setup_chain() ->
    #{ contracts => setup_contracts()}.

setup_contracts() ->
    Cs = contracts(),
    NewCs = [{C, setup_contract(Functions)}
             || {C, Functions} <- maps:to_list(Cs)],
    maps:from_list(NewCs).

setup_contract(Functions) ->
    lists:foldl(
      fun({FunctionName, Signature, BBs}, State) ->
              set_function_code(FunctionName, Signature, BBs, State)
      end,
      #{functions => #{}},
      Functions).

set_function_code(Name, Signature, BBs, #{functions := Functions} = S) ->
    NewFunctions = maps:put(Name, {Signature, set_bbs(BBs, #{})}, Functions),
    maps:put(functions, NewFunctions, S).

set_bbs([], BBs) -> BBs;
set_bbs([{N, Code}|Rest], BBs) -> 
    set_bbs(Rest, BBs#{N => Code}).
     

contracts() ->
    #{ <<"test">> =>
           [ {<<"id">>, {[integer], integer},
              [{0, [{'RETURNR', {arg, 0}}]}]}
           , {<<"jumps">>, {[], integer},
              [ {0, [ {'PUSH', {immediate, 0}}
                    , {'JUMP', {immediate, 3}}]}
              , {1, [ 'NOP' ]}
              , {2, [ 'NOP'
                    , 'RETURN']}
              , {3, [ 'NOP'
                    , {'JUMP', {immediate, 1}}]}]
             }
           , {<<"inc">>, {[integer],integer},
              [{0, [ {'ADD', {stack, 0}, {arg,0}, {immediate, 1}}
                   , {'INC', {stack, 0}}
                   , 'RETURN'
                   ]}]
             }
           , {<<"call">>, {[integer],integer},
              [{0, [ {'ADD', {stack, 0}, {arg,0}, {immediate, 1}}
                   , {'CALL', {immediate, <<"inc">>}}]}
              ,{1, [ {'INC', {stack, 0}}
                   , 'RETURN']}
              ]
             }
           , {<<"tailcall">>, {[integer],integer},
              [{0, [ {'ADD', {stack, 0}, {arg,0}, {immediate, 1}}
                   , {'CALL_T', {immediate, <<"inc">>}}]}
              ]
             }
           , { <<"remote_call">>
             , {[integer],integer}
             , [ {0, [ {'PUSH', {arg,0}},
                       {'CALL_R', {immediate, <<"remote">>}, {immediate, <<"add_five">>}} ]}
               , {1, [ {'INC', {stack, 0}},
                       'RETURN']}
               ]
             }
           , { <<"remote_tailcall">>
             , {[integer],integer}
             , [ {0, [ {'PUSH', {arg,0}},
                       {'CALL_TR', {immediate, <<"remote">>}, {immediate, <<"add_five">>}} ]}
               ]
             }
           ]
     , <<"remote">> =>
           [ {<<"add_five">>, {[integer], integer},
              [{0, [{'ADD', {stack, 0}, {immediate, 5}, {arg, 0}}
                    , 'RETURN']}]
             }
           ]
     , <<"bool">> =>
           [ {<<"and">>
             , {[boolean, boolean], boolean}
             , [ {0, [ {'AND', {stack, 0}, {arg, 0}, {arg, 1}}
                     , 'RETURN']}]}
           , {<<"or">>
             , {[boolean, boolean], boolean}
             , [ {0, [ {'OR', {stack, 0}, {arg, 0}, {arg, 1}}
                     , 'RETURN']}]}
           , {<<"not">>
             , {[boolean], boolean}
             , [ {0, [ {'NOT', {stack, 0}, {arg, 0}}
                     , 'RETURN']}]}
           ]

     , <<"comp">> =>
           [ {<<"lt">>
             , {[integer, integer], boolean}
             , [ {0, [ {'LT', {stack, 0}, {arg, 0}, {arg, 1}}
                     , 'RETURN']}]}
           ,  {<<"gt">>
              , {[integer, integer], boolean}
              , [ {0, [ {'GT', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"egt">>
              , {[integer, integer], boolean}
              , [ {0, [ {'EGT', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"elt">>
              , {[integer, integer], boolean}
              , [ {0, [ {'ELT', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"eq">>
              , {[integer, integer], boolean}
              , [ {0, [ {'EQ', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"neq">>
              , {[integer, integer], boolean}
              , [ {0, [ {'NEQ', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ]
     , <<"arith">> =>
           [ {<<"add">>
             , {[integer, integer], integer}
             , [ {0, [ {'ADD', {stack, 0}, {arg, 0}, {arg, 1}}
                     , 'RETURN']}]}
           ,  {<<"sub">>
              , {[integer, integer], integer}
              , [ {0, [ {'SUB', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"mul">>
              , {[integer, integer], integer}
              , [ {0, [ {'MUL', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"div">>
              , {[integer, integer], integer}
              , [ {0, [ {'DIV',  {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"mod">>
              , {[integer, integer], integer}
              , [ {0, [ {'MOD', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"pow">>
              , {[integer, integer], integer}
              , [ {0, [ {'POW', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ]
     , <<"jumpif">> =>
           [ {<<"skip">>
             , {[integer, integer], integer}
             , [ {0, [ {'PUSH', {arg, 1}}
                     , {'PUSH', {immediate, 0}}
                     , {'EQ', {stack, 0}, {stack, 0}, {arg, 0}}
                     , {'JUMPIF', {stack, 0}, {immediate, 2}}
                     ]}
               , {1, [ {'INC', {stack, 0}} ]}
               , {2, [ 'RETURN']}
               ]}
           ]
     , <<"memory">> =>
           [ {<<"call">>
             , {[integer], integer}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'PUSH', {immediate, 0}}
                     , {'CALL', {immediate, <<"write">>}}
                     ]
                 }
               , {1, [ {'PUSH', {var, 1}}
                     , 'RETURN'
                     ]}
               ]}
           , {<<"write">>
             , {[integer], integer}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'RETURNR', {var, 1}}
                     ]}
               ]}
           , {<<"dest_add">>
             , {[integer, integer], integer}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'STORE', {var, 2}, {arg, 1}}
                     , {'ADD', {var, 3}, {var, 1}, {var, 2}}
                     , {'PUSH', {var, 3}}
                     , 'RETURN'
                     ]}
               ]}
           , {<<"dest_add_imm">>
             , {[integer], integer}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'ADD', {var, 3}, {var, 1}, {immediate, 2}}
                     , {'PUSH', {var, 3}}
                     , 'RETURN'
                     ]}
               ]}
           , {<<"dest_add_stack">>
             , {[integer, integer], integer}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'PUSH', {arg, 1}}
                     , {'ADD', {var, 3}, {var, 1}, {stack, 0}}
                     , {'PUSH', {var, 3}}
                     , 'RETURN'
                     ]}
               ]}
           ]
     , <<"tuple">> =>
           [ {<<"make_0tuple">>
             , {[], {tuple, []}}
             , [ {0, [ {'TUPLE', {immediate, 0}}
                     , 'RETURN']}
               ]}
           , {<<"make_2tuple">>
             , {[integer, integer], {tuple, [integer, integer]}}
             , [ {0, [ {'PUSH', {arg, 0}}
                     , {'PUSH', {arg, 1}}
                     , {'TUPLE', {immediate, 2}}
                     ,  'RETURN']}
               ]}
           , {<<"make_5tuple">>
             , {[integer, integer, integer, integer, integer],
                {tuple, [integer, integer, integer, integer, integer]}}
             , [ {0, [ {'PUSH', {arg, 0}}
                     , {'PUSH', {arg, 1}}
                     , {'PUSH', {arg, 2}}
                     , {'PUSH', {arg, 3}}
                     , {'PUSH', {arg, 4}}
                     , {'TUPLE', {immediate, 5}}
                     ,  'RETURN']}
               ]}
           , {<<"element1">>
             , {[integer, integer], integer}
             , [ {0, [ {'PUSH', {arg, 0}}
                     , {'PUSH', {arg, 1}}
                     , {'TUPLE', {immediate, 2}}
                     , {'ELEMENT', integer, {stack, 0}, {immediate, 1}, {stack, 0}}
                     , 'RETURN']}
               ]}
           , {<<"element">>
             , {[{tuple, [integer, integer]}, integer], integer}
             , [ {0, [ {'ELEMENT', integer, {stack, 0}, {arg, 1}, {arg, 0}}
                     , 'RETURN']}
               ]}
           ]
     , <<"map">> =>
           [ {<<"make_empty_map">>
             , {[], {map, integer, boolean}}
             , [ {0, [ {'MAP_EMPTY', {stack, 0}}
                     , 'RETURN']}
               ]}
           , {<<"map_update">>
             , {[{map, integer, boolean}, integer, boolean], {map, integer, boolean}}
             , [ {0, [ {'MAP_UPDATE', {stack, 0}, {arg, 0}, {arg, 1}, {arg, 2}}
                     ,  'RETURN']}
               ]}
           , {<<"map_lookup">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {'MAP_LOOKUP', {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  'RETURN']}
               ]}
           , {<<"map_lookup_default">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {'MAP_LOOKUPD', {stack, 0}, {arg, 0}, {arg, 1}, {immediate, false}}
                     ,  'RETURN']}
               ]}
           , {<<"map_member">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {'MAP_MEMBER', {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  'RETURN']}
               ]}
           , {<<"map_member">>
             , {[{map, integer, boolean}, integer], boolean}
             , [ {0, [ {'MAP_MEMBER', {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  'RETURN']}
               ]}
           , {<<"map_from_list">>
             , {[{list, {tuple, [integer, boolean]}}], {map, integer, boolean}}
               , [ {0, [ {'MAP_FROM_LIST', {stack, 0}, {arg, 0}}
                       , 'RETURN']}
                 ]}
             ]
     , <<"list">> =>
           [ {<<"make_nil">>
             , {[], {list, integer}}
             , [ {0, [ {'NIL', {stack, 0}}
                     , 'RETURN']}
               ]}
           , {<<"cons">>
             , {[integer, {list, integer}], {list, integer}}
             , [ {0, [ {'CONS', {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  'RETURN']}
               ]}
           , {<<"cons_error">>
             , {[integer, {list, boolean}], {list, integer}}
             , [ {0, [ {'CONS', {stack, 0}, {arg, 0}, {arg, 1}}
                     ,  'RETURN']}
               ]}
           , {<<"head">>
             , {[{list, integer}], integer}
             , [ {0, [ {'HD', {stack, 0}, {arg, 0}}
                     ,  'RETURN']}
               ]}
           , {<<"tail">>
             , {[{list, integer}], {list, integer}}
             , [ {0, [ {'TL', {stack, 0}, {arg, 0}}
                     , 'RETURN']}
               ]}
           , {<<"length">>
             , {[{list, integer}], integer}
             , [ {0, [ {'LENGTH', {stack, 0}, {arg, 0}}
                     , 'RETURN']}
               ]}
           ]
     , <<"string">> =>
           [ {<<"str_equal">>
             , {[string, string], boolean}
             , [ {0, [ {'STR_EQ', {stack, 0}, {arg, 0}, {arg, 1}}
                     , 'RETURN']}
               ]}
           , {<<"str_join">>
             , {[string, string], string}
             , [ {0, [ {'STR_JOIN', {stack, 0}, {arg, 0}, {arg, 1}}
                     , 'RETURN']}
               ]}
           , {<<"int_to_str">>
             , {[integer], string}
             , [ {0, [ {'INT_TO_STR', {stack, 0}, {arg, 0}}
                     , 'RETURN']}
               ]}
           , {<<"addr_to_str">>
             , {[address], string}
             , [ {0, [ {'ADDR_TO_STR', {stack, 0}, {arg, 0}}
                     , 'RETURN']}
               ]}
           , {<<"int_to_addr">>
             , {[integer], address}
             , [ {0, [ {'INT_TO_ADDR', {stack, 0}, {arg, 0}}
                     , 'RETURN']}
               ]}
           , {<<"str_reverse">>
             , {[string], string}
             , [ {0, [ {'STR_REVERSE', {stack, 0}, {arg, 0}}
                     , 'RETURN']}
               ]}
           ]

     , <<"variant">> =>
           [ {<<"switch">>
             , {[{variant, 2}], integer}
             , [ {0, [ {'SWITCH_V2', {arg,0}, {immediate, 1}, {immediate, 2}}]}
               , {1, [{'RETURNR', {immediate, 0}}]}
               , {2, [{'RETURNR', {immediate, 42}}]}
               ]}
           , {<<"test">>
             , {[{variant, 2}, integer], boolean}
             , [ {0, [ {'VARIANT_TEST', {stack, 0}, {arg,0}, {arg, 1}}
                     , 'RETURN']}
               ]}
           , {<<"element">>
             , {[{variant, 2}, integer], integer}
             , [ {0, [ {'VARIANT_ELEMENT', {stack, 0}, {arg,0}, {arg, 1}}
                     , 'RETURN']}
               ]}
           , {<<"make">>
             , {[integer, integer, integer, {list, integer}], {variant, 2}}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'STORE', {var, 2}, {arg, 1}}
                     , {'STORE', {var, 3}, {arg, 3}}
                     , {'STORE', {var, 4}, {arg, 2}}
                     , {'JUMP', {immediate, 2}}
                     ]}
               , {1, [ {'VARIANT', {stack, 0}, {var, 1}, {var, 2}, {var, 4}}
                     , 'RETURN'
                     ]}
               , {2, [ {'IS_NIL', {stack, 0}, {var, 3}}
                     , {'JUMPIF', {stack, 0}, {immediate, 1}}
                     ]}
               , {3, [ {'HD', {stack, 0}, {var, 3}}
                     , {'TL', {var, 3}, {var, 3}}
                     , {'JUMP', {immediate, 2}}]}
               ]}
           ]

     , <<"bits">> =>
           [ {<<"all">>
             , {[], bits}
             , [ {0, [ 'BITS_ALL'
                     , 'RETURN']}]}
           , {<<"all_n">>
             , {[integer], bits}
             , [ {0, [ {'BITS_ALL_N', {stack, 0}, {arg, 0}}
                     , 'RETURN']}]}
           ,  {<<"none">>
              , {[], bits}
              , [ {0, [ 'BITS_NONE'
                      , 'RETURN']}]}
           ,  {<<"set">>
              , {[integer], bits}
              , [ {0, [ {'BITS_NONE', {var, 1}}
                      , {'BITS_SET', {stack, 0}, {var, 1}, {arg, 0}}
                      , 'RETURN']}]}
           ,  {<<"clear">>
              , {[integer], bits}
              , [ {0, [ {'BITS_ALL', {var, 1}}
                      , {'BITS_CLEAR', {stack, 0}, {var, 1}, {arg, 0}}
                      , 'RETURN']}]}
           ,  {<<"test">>
              , {[bits, integer], boolean}
              , [ {0, [ {'BITS_TEST', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"sum">>
              , {[bits], integer}
              , [ {0, [ {'BITS_SUM', {stack, 0}, {arg, 0}}
                      , 'RETURN']}]}
           ,  {<<"union">>
              , {[bits, bits], bits}
              , [ {0, [ {'BITS_OR', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"intersection">>
              , {[bits, bits], bits}
              , [ {0, [ {'BITS_AND', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}
           ,  {<<"difference">>
              , {[bits, bits], bits}
              , [ {0, [ {'BITS_DIFF', {stack, 0}, {arg, 0}, {arg, 1}}
                      , 'RETURN']}]}

           ]

       }.


