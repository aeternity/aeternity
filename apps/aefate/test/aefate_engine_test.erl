%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate engine
%%%  make TEST=aefate_engine_test eunit
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_engine_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").

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

chain_and_call_test_() ->
    make_calls(chain_and_call()).

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

meta_test_() ->
    make_calls(meta()).

make_calls(ListOfCalls) ->
    Cache = setup_contracts_in_cache(),
    Trees = setup_contracts_in_trees(aec_trees:new_without_backend()),
    aefa_fate_op:load_pre_iris_map_ordering(),
    %% Dummy values since they should not come into play in this test
    Env = #{ trees => Trees
           , caller => <<123:256>>
           , origin => <<123:256>>
           , gas_price => 1
           , fee => 621
           , tx_env => aetx_env:tx_env(1)
           },
    [{lists:flatten(io_lib:format("call(~p,~p,~p)->~p~n~p : ~p",
                                  [C,F,A,R,
                                   aefa_test_utils:encode(A),
                                   aeb_fate_encoding:serialize(
                                     aefa_test_utils:encode(A))])),
      fun() ->
              Spec = make_call(C,F,A),
              case R of
                  {error, E} ->
                      case aefa_fate:run_with_cache(Spec, Env, Cache) of
                          {ok, ES} ->
                              Res = aefa_engine_state:accumulator(ES),
                              Trace = aefa_engine_state:trace(ES),
                              ?assertEqual(R, {ok, Res, Trace});
                          {error, Error, ES} ->
                              Trace = aefa_engine_state:trace(ES),
                              ?assertEqual({E, Trace}, {Error, Trace})
                      end;
                  _ ->
                      FateRes = aefa_test_utils:encode(R),
                      case aefa_fate:run_with_cache(Spec, Env, Cache) of
                          {ok, ES} ->
                              Res = aefa_engine_state:accumulator(ES),
                              Trace = aefa_engine_state:trace(ES),
                              ?assertEqual({FateRes, Trace}, {Res, Trace});
                          {revert, Msg, ES} ->
                              Trace = aefa_engine_state:trace(ES),
                              ?assertEqual({FateRes, Trace}, {{revert, Msg}, Trace})
                      end
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

chain_and_call() ->
    [ {<<"chain_and_call">>, <<"call_fee">>, [], 621}
    , {<<"chain_and_call">>, <<"call_gasprice">>, [], 1}
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
            , {<<"call2">>, [17], 17}
            , {<<"call3">>, [17], {error, <<"Undefined var: {var,1}">>}}
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
    BigList = [{I, I rem 2 == 0} || I <- lists:seq(0, 100)],
    BigMap = maps:from_list(BigList),
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
            ,  {<<"map_to_list">>, [#{ 1 => true, 2 => false, 42 => true}],
                [{1, true}, {2, false}, {42, true}]}
            % Test really big map to list
            , case aec_governance:get_network_id() of
                PreIris when PreIris =:= <<"local_roma_testnet">>;
                             PreIris =:= <<"local_fortuna_testnet">>;
                             PreIris =:= <<"local_minerva_testnet">>;
                             PreIris =:= <<"local_lima_testnet">> ->
                    {<<"map_to_list">>, [BigMap], maps:to_list(BigMap)};
                _PostIris ->
                    {<<"map_to_list">>, [BigMap], BigList}
              end
            ]
    ].


list() ->
    [ {<<"list">>, F, A, R} ||
        {F,A,R} <-
            [ {<<"make_nil">>, [], []}
            ,  {<<"cons">>, [42,[]], [42]}
            ,  {<<"head">>, [[42]], 42}
            ,  {<<"tail">>, [[42]], []}
            %% Operation disabled
            %% ,  {<<"length">>, [[1,2,3,4]], 4}
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
            %% Operation disabled
            %% , {<<"int_to_addr">>, [0], {address, <<0:256>>}}
            %% , {<<"int_to_addr">>, [1], {address, <<1:256>>}}
            %% , {<<"int_to_addr">>, [961981404445573111893532125566728879481945428377512160796083762629444620341],
            %%    {address, <<2,32,118,81,108,18,247,243,45,243,82,75,159,144,74,117,157,38,78,99,114,239,
            %%                217,210,67,40,207,3,52,111,208,53>>}}
            , {<<"addr_to_str">>, [{address, <<2,32,118,81,108,18,247,243,45,243,82,75,
                                               159,144,74,117,157,38,78,99,114,239,
                                               217,210,67,40,207,3,52,111,208,53>>}],
              <<"ak_wKvyTJ147VfZWeGVUnx274hVWRFL2cN5FPrNeGgTWPjpXS17">>}
            , {<<"addr_to_str">>, [{address, <<"ak_wKvyTJ147VfZWeGVUnx274hVWRFL2cN5FPrNeGgTWPjpXS17">>}],
               %% TODO: Should be  <<"ak_wKvyTJ147VfZWeGVUnx274hVWRFL2cN5FPrNeGgTWPjpXS17">>
               %% with latest aebytecode version.
               <<"ak_bfm4SvXeu4uunNMayrMhZ6TsoZ17k5XadQbxHGXztLhA8PqAnKCcQdAkML6i6vsNh5zxmHaxoEP">>
              }
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
            [ {<<"switch">>, [{variant, [0,1], 0, {}}], 0}
            , {<<"switch">>, [{variant, [0,1], 1, {42}}], 42}
            , {<<"switch2">>, [{variant, [0,1,2], 1, {42}}], {error, <<"Type error in switch: wrong size 3">>}}
            , {<<"test">>, [{variant, [0,1], 1, {42}}, 1], true}
            , {<<"test">>, [{variant, [0,1], 1, {42}}, 2], false}
            , {<<"element">>, [{variant, [0,1], 1, {42}}, 0], 42}
            , {<<"make">>, [[0,1], 1, 1, [42]], {variant, [0,1], 1, {42}}}
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

meta() ->
    [ {<<"meta">>, <<"bytecode_hash">>, [],
       {variant, [0,1], 1, {{bytes,
                             <<7,159,155,183,53,227,53,68,
                               77,156,86,211,100,186,212,51,
                               177,245,45,103,173,181,44,224,
                               247,185,222,195,77,77,212,73>>
                            }}}}
    , {<<"meta">>, <<"bytecode_hash_fail">>, [],
       {variant, [0,1], 0, {}}}
    , {<<"meta">>, <<"create">>, [], 2137}
    , {<<"meta">>, <<"clone">>, [], 2137}
    , {<<"meta">>, <<"clone_created">>, [], 2137}
    , {<<"meta">>, <<"clone_gas_limit">>, [], {variant, [0,1], 0, {}}}
    ].

make_call(Contract, Function0, Arguments) ->
    Function = aeb_fate_code:symbol_identifier(Function0),
    #{ contract  => pad_contract_name(Contract)
     , gas => 100000
     , fee => 621
     , value => 0
     , store => aect_contracts_store:new()
     , call => aeb_fate_encoding:serialize(
                 {tuple, {Function, {tuple, list_to_tuple(
                                              [aefa_test_utils:encode(A) || A <- Arguments]
                                             )}
                         }
                 }
                )
     , vm_version => ?VM_FATE_SOPHIA_2
     , allow_init => true
     }.

setup_contracts_in_cache() ->
    Cs = contracts(),
    NewCs = [{pad_contract_name(C), {setup_contract(Functions), ?VM_FATE_SOPHIA_2}}
             || {C, Functions} <- maps:to_list(Cs)],
    maps:from_list(NewCs).

setup_contracts_in_trees(Trees) ->
    Cs = contracts(),
    {NewTrees, _} = lists:foldl(
      fun({C, Functions}, {Ts0, Nonce}) ->
              FateCode = setup_contract(Functions),
              ByteCode = aeb_fate_code:serialize(FateCode),
              Code     = aeser_contract_code:serialize(
                           #{byte_code => ByteCode,
                             type_info => [],
                             contract_source =>
                                 %% Ode to the idea of making this field mandatory
                                 "main :: IO ()\n" ++
                                 "main = putStrLn \"what are you staring at?\"\n"
                           }),

              PK = pad_contract_name(C),
              Acct = aec_accounts:new(PK, 1000000),
              Contr0 = aect_contracts:new(
                        _Owner = pad_contract_name(<<"mother_test">>),
                        _Nonce = Nonce,
                        _CTVersion = #{vm  => ?VM_FATE_SOPHIA_2,
                                       abi => ?ABI_FATE_SOPHIA_1},
                        _Code = Code,
                        _Deposit = 0
                       ),
              Contr1 = aect_contracts:set_pubkey(PK, Contr0),

              Ts1 = aec_trees:set_contracts(Ts0,
                aect_state_tree:insert_contract(Contr1, aec_trees:contracts(Ts0))),
              Ts2 = aec_trees:set_accounts(Ts1,
                aec_accounts_trees:enter(Acct, aec_trees:accounts(Ts1))),
              {Ts2, Nonce + 1}
      end,
      {Trees, 0},
      maps:to_list(Cs)
     ),

    NewTrees.

pad_contract_name(Name) ->
    PadSize = 32 - byte_size(Name),
    iolist_to_binary([Name, lists:duplicate(PadSize, "_")]).

setup_contract(Functions) ->
    lists:foldl(
      fun({FunctionName, Signature, BBs}, FateCode) ->
              set_function_code(FunctionName, Signature, BBs, FateCode)
      end,
      aeb_fate_code:new(),
      Functions).

set_function_code(Name, Signature, BBs, FateCode) ->
    aeb_fate_code:insert_fun(Name, [], Signature, maps:from_list(BBs), FateCode).

contracts() ->
    #{ <<"test">> =>
           [ {<<"id">>, {[integer], integer},
              [{0, [{'RETURNR', {arg, 0}}]}]}
           , {<<"jumps">>, {[], integer},
              [ {0, [ {'PUSH', {immediate, 0}}
                    , {'JUMP', {immediate, 3}}]}
              , {1, [ 'NOP'
                    , {'JUMP', {immediate, 2}}
                    ]}
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
                   , {'CALL', {immediate, aeb_fate_code:symbol_identifier(<<"inc">>)}}]}
              ,{1, [ {'INC', {stack, 0}}
                   , 'RETURN']}
              ]
             }
           , {<<"tailcall">>, {[integer],integer},
              [{0, [ {'ADD', {stack, 0}, {arg,0}, {immediate, 1}}
                   , {'CALL_T', {immediate, aeb_fate_code:symbol_identifier(<<"inc">>)}}]}
              ]
             }
           , { <<"remote_call">>
             , {[integer],integer}
             , [ {0, [ {'PUSH', {arg,0}},
                       {'CALL_R',
                        {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"remote">>))},
                        {immediate, aeb_fate_code:symbol_identifier(<<"add_five">>)},
                        {immediate, {typerep, {tuple, [integer]}}},
                        {immediate, {typerep, integer}},
                        {immediate, 0}
                       } ]}
               , {1, [ {'INC', {stack, 0}},
                       'RETURN']}
               ]
             }
           ]
    , <<"meta">> =>
           [ { <<"bytecode_hash">>, {[], {variant, [{tuple, []}, {tuple, [{bytes, 32}]}]}}
             , [ {0, [ {'BYTECODE_HASH',
                        {stack, 0},
                        {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"remote">>))}}
                     , 'RETURN']}
               ]
             }
           , { <<"bytecode_hash_fail">>, {[], {variant, [{tuple, []}, {tuple, [{bytes, 32}]}]}}
             , [ {0, [ {'BYTECODE_HASH',
                        {stack, 0},
                        {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"nonexistent_dude">>))}}
                     , 'RETURN']}
               ]
             }
           , { <<"clone">>, {[], integer}
             , [ {0, [ {'CLONE',
                        {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"remote">>))},
                        {immediate, {typerep, {tuple, []}}},
                        {immediate, 0},
                        {immediate, false}
                       }
                     ]
                 }
               , {1, [ 'DUPA'
                     , {'POP', {arg, 0}}
                     , {'PUSH', {immediate, 2132}}
                     , {'CALL_R',
                        {arg, 0},
                        {immediate, aeb_fate_code:symbol_identifier(<<"add_five">>)},
                        {immediate, {typerep, {tuple, [integer]}}},
                        {immediate, {typerep, integer}},
                        {immediate, 0}
                       }
                     ]
                 }
               , {2, [ {'NEQ', {stack, 0}, {stack, 0}, {immediate, 2137}}
                     , {'JUMPIF', {stack, 0}, {immediate, 12}}
                     ]
                 }
               , {3,
                     [ {'CLONE',
                        {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"remote">>))},
                        {immediate, {typerep, {tuple, []}}},
                        {immediate, 0},
                        {immediate, true}
                       }
                     ]
                 }
               , {4, [ 'DUPA'
                     , {'VARIANT_TEST', {stack, 0}, {stack, 0}, {immediate, 1}}
                     , {'NOT', {stack, 0}, {stack, 0}}
                     , {'JUMPIF', {stack, 0}, {immediate, 13}}
                     ]
                 }
               , {5,
                     [ {'VARIANT_ELEMENT', {arg, 0}, {stack, 0}, {immediate, 0}}
                     , {'PUSH', {immediate, 2132}}
                     , {'CALL_R',
                        {arg, 0},
                        {immediate, aeb_fate_code:symbol_identifier(<<"add_five">>)},
                        {immediate, {typerep, {tuple, [integer]}}},
                        {immediate, {typerep, integer}},
                        {immediate, 0}
                       }
                     ]
                 }
               , {6, [ {'POP', {arg, 0}}
                     , {'NEQ', {arg, 0}, {arg, 0}, {immediate, 2137}}
                     , {'JUMPIF', {arg, 0}, {immediate, 14}}
                     ]
                 }
               , {7, [ {'PUSH', {immediate, 2138}}
                     , {'CLONE',
                        {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"remote">>))},
                        {immediate, {typerep, {tuple, [integer]}}},
                        {immediate, 0},
                        {immediate, true}
                       }
                     ]
                 }
               , {8, [ {'VARIANT_TEST', {stack, 0}, {stack, 0}, {immediate, 0}}
                     , {'NOT', {stack, 0}, {stack, 0}}
                     , {'JUMPIF', {stack, 0}, {immediate, 15}}
                     ]
                 }
               , {9, [ {'CLONE',
                        {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"abortion">>))},
                        {immediate, {typerep, {tuple, [integer]}}},
                        {immediate, 0},
                        {immediate, true}
                       }
                     ]
                 }
               , {10, [ {'VARIANT_TEST', {stack, 0}, {stack, 0}, {immediate, 0}}
                      , {'NOT', {stack, 0}, {stack, 0}}
                      , {'JUMPIF', {stack, 0}, {immediate, 16}}
                      ]
                 }
               , {11, [ {'RETURNR', {immediate, 2137}} ]
                 }
               , {12, [{'ABORT', {immediate, <<"CLONE: unprotected bad result">>}}]}
               , {13, [{'ABORT', {immediate, <<"CLONE: not-Some on success">>}}]}
               , {14, [{'ABORT', {immediate, <<"CLONE: protected bad result">>}}]}
               , {15, [{'ABORT', {immediate, <<"CLONE: not-None on type fail">>}}]}
               , {16, [{'ABORT', {immediate, <<"CLONE: not-None on runtime fail">>}}]}
               ]
             }
           , { <<"create">>, {[], integer}
             , [ {0, [ {'PUSH', {immediate, 2}}
                     , {'CREATE',
                        {immediate,
                         %% payable contract C =
                         %%     type state = int
                         %% entrypoint init(x) =
                         %%    Contract.balance * 1000 + Chain.balance(Contract.address) * 100 + Call.value * 10 + x
                         %% stateful entrypoint set(x) = put(x)
                         %% entrypoint get() = state
                         %% payable entrypoint value() = Call.value
                         %% entrypoint caller_is_creator() = Call.caller == Contract.creator
                         %% entrypoint balance() = Contract.balance
                         aeb_fate_data:make_contract_bytearray(
                           <<248,242,70,3,160,56,23,62,19,216,177,213,230,73,200,76,
                             41,93,71,122,234,59,177,235,92,237,179,132,224,17,244,
                             149,141,110,23,252,67,192,184,197,184,113,254,12,124,37,
                             201,0,55,0,23,125,0,85,0,32,0,0,254,47,134,91,217,0,55,
                             0,7,1,2,130,254,68,214,68,31,0,55,1,7,55,0,11,0,22,48,
                             20,94,0,115,0,22,48,111,36,83,0,22,48,111,130,3,168,20,
                             0,20,0,20,18,130,0,1,3,63,254,180,140,22,132,0,55,0,7,
                             83,0,0,254,214,48,121,144,4,55,0,7,11,0,0,254,232,196,
                             94,182,0,55,1,7,55,0,26,6,130,0,1,3,63,184,77,47,6,17,
                             12,124,37,201,69,99,97,108,108,101,114,95,105,115,95,99,
                             114,101,97,116,111,114,17,47,134,91,217,13,103,101,116,
                             17,68,214,68,31,17,105,110,105,116,17,180,140,22,132,29,
                             98,97,108,97,110,99,101,17,214,48,121,144,21,118,97,108,
                             117,101,17,232,196,94,182,13,115,101,116,130,47,0,133,
                             52,46,51,46,48,1>>)},
                        {immediate, {typerep, {tuple, [integer]}}},
                        {immediate, 1}
                       }]}
               , {1, [ 'DUPA' %% Save the contract address

                       %% Check if state was set correctly
                       %% Since init does what it does it also checks the initial value
                     , {'CALL_R',
                       {stack, 0},
                       {immediate, aeb_fate_code:symbol_identifier(<<"get">>)},
                       {immediate, {typerep, {tuple, []}}},
                       {immediate, {typerep, integer}},
                       {immediate, 0}
                      }
                     ]}
               , {2, [ {'NEQ', {stack, 0}, {stack, 0}, {immediate, 1102}}
                     , {'JUMPIF', {stack, 0}, {immediate, 12}}
                     ]
                 }
               , {3,
                     [ 'DUPA'

                       %% Check if the balance has been preserved
                     , {'CALL_R',
                        {stack, 0},
                        {immediate, aeb_fate_code:symbol_identifier(<<"balance">>)},
                        {immediate, {typerep, {tuple, []}}},
                        {immediate, {typerep, integer}},
                        {immediate, 0}
                       }
                     ]}
               , {4, [ {'NEQ', {stack, 0}, {stack, 0}, {immediate, 1}}
                     , {'JUMPIF', {stack, 0}, {immediate, 13}}
                     ]
                 }
               , {5,
                     [ 'DUPA'

                       %% Check if value is transferred correctly
                     , {'CALL_R',
                        {stack, 0},
                        {immediate, aeb_fate_code:symbol_identifier(<<"value">>)},
                        {immediate, {typerep, {tuple, []}}},
                        {immediate, {typerep, integer}},
                        {immediate, 2136}
                       }
                     ]}
               , {6, [ {'NEQ', {stack, 0}, {stack, 0}, {immediate, 2136}}
                     , {'JUMPIF', {stack, 0}, {immediate, 14}}
                     ]
                 }
               , {7,
                     [ 'DUPA'

                       %% Check if the value has been registered in the balance
                     , {'CALL_R',
                        {stack, 0},
                        {immediate, aeb_fate_code:symbol_identifier(<<"balance">>)},
                        {immediate, {typerep, {tuple, []}}},
                        {immediate, {typerep, integer}},
                        {immediate, 0}
                       }
                     ]}
               , {8, [ {'NEQ', {stack, 0}, {stack, 0}, {immediate, 2137}}
                     , {'JUMPIF', {stack, 0}, {immediate, 15}}
                     ]
                 }
               , {9,
                     [ 'DUPA'

                       %% Check if the creator is set to this contract
                     , {'CALL_R',
                        {stack, 0},
                        {immediate, aeb_fate_code:symbol_identifier(<<"caller_is_creator">>)},
                        {immediate, {typerep, {tuple, []}}},
                        {immediate, {typerep, boolean}},
                        {immediate, 0}
                       }
                     ]}
               , {10, [ {'NOT', {stack, 0}, {stack, 0}}
                     , {'JUMPIF', {stack, 0}, {immediate, 16}}
                     ]
                 }
               , {11, %% Success
                  [ {'RETURNR', {immediate, 2137}} ]}

                 %% Return error
               , {12, [{'ABORT', {immediate, <<"CREATE: wrong initial state or initial funds">>}}]}
               , {13, [{'ABORT', {immediate, <<"CREATE: wrong balance returned">>}}]}
               , {14, [{'ABORT', {immediate, <<"CREATE: wrong Call.value">>}}]}
               , {15, [{'ABORT', {immediate, <<"CREATE: call with value didn't add funds">>}}]}
               , {16, [{'ABORT', {immediate, <<"CREATE: creator is not the caller">>}}]}
               ]
             }
           , {<<"clone_created">>, {[], integer},
              [ {0, [ {'PUSH', {immediate, 666}}
                    , {'CREATE',
                       {immediate,
                        %% payable contract C =
                        %%   type state = int
                        %%   entrypoint init(x) = x
                        %%   stateful entrypoint check() =
                        %%     if(Contract.creator == Call.caller)
                        %%       state * 10 + Contract.balance
                        %%     else -1
                        aeb_fate_data:make_contract_bytearray(
                          <<248,121,70,3,160,134,111,75,0,216,33,138,236,11,52,253,
                            5,19,156,55,72,210,164,173,168,49,181,72,217,97,200,88,
                            101,166,14,101,222,192,184,76,176,254,68,214,68,31,0,55,
                            1,7,55,0,26,6,130,0,1,3,63,254,175,10,64,88,0,55,0,7,85,
                            0,125,0,32,0,7,12,4,1,3,130,83,0,22,56,130,20,20,0,0,
                            151,47,2,17,68,214,68,31,17,105,110,105,116,17,175,10,
                            64,88,21,99,104,101,99,107,130,47,0,133,52,46,51,46,48,1
                          >>)
                       },
                       {immediate, {typerep, {tuple, [integer]}}},
                       {immediate, 0}
                      }
                    ]
                }
                %% [C]
              , {1, [ {'POP', {arg, 0}}
                      %% [] arg0=C
                    , {'PUSH', {immediate, 1}}
                      %% [1] arg0=C
                    , {'CLONE',
                       {arg, 0},
                       {immediate, {typerep, {tuple, [integer]}}},
                       {immediate, 3},
                       {immediate, false}
                      }
                    ]}
                %% [C1]
              , {2, [ 'DUPA'
                      %% [C1, C1]
                    , {'POP', {arg, 0}}
                      %% [C1] arg0=C1
                    , {'PUSH', {immediate, 2}}
                      %% [2, C1] arg0=C1
                    , {'CLONE_G',
                       {arg, 0},
                       {immediate, {typerep, {tuple, [integer]}}},
                       {immediate, 4},
                       {immediate, 1000000},
                       {immediate, false}
                      }
                    ]
                }
                %% [C2, C1]
                %% Assert they are different
              , {3, [ {'DUP', {immediate, 1}}
                      %% [C1, C2, C1]
                    , {'DUP', {immediate, 1}}
                      %% [C2, C1, C2, C1]
                    , {'POP', {arg, 0}}
                      %% [C1, C2, C1] arg0=C2
                    , {'EQ', {stack, 0}, {stack, 0}, {arg, 0}}
                      %% [C1==C2, C2, C1]
                    , {'JUMPIF', {stack, 0}, {immediate, 10}}
                    ]
                }
                %% [C2, C1]
                %% Assert they have same bytecode hash
              , {4, [ {'DUP', {immediate, 1}}
                      %% [C1, C2, C1]
                    , {'DUP', {immediate, 1}}
                      %% [C2, C1, C2, C1]
                    , {'BYTECODE_HASH', {arg, 0}, {stack, 0}}
                      %% [C1, C2, C1] arg0=#C2
                    , {'BYTECODE_HASH', {stack, 0}, {stack, 0}}
                      %% [#C1, C2, C1] arg0=#C2
                    , {'EQ', {stack, 0}, {arg, 0}, {stack, 0}}
                      %% [#C2==#C1, C2, C1]
                    , {'NOT', {stack, 0}, {stack, 0}}
                      %% [#C2!=#C1, C2, C1]
                    , {'JUMPIF', {stack, 0}, {immediate, 11}}
                    ]
                }
                %% [C2, C1]
                %% Check that values and states are kept separately
                %% Also ensure that the creator is set correctly
              , {5, [ {'CALL_R',
                       {stack, 0},
                       {immediate, aeb_fate_code:symbol_identifier(<<"check">>)},
                       {immediate, {typerep, {tuple, []}}},
                       {immediate, {typerep, integer}},
                       {immediate, 0}
                      }
                    ]
                }
                %% [C2.check, C1]
              , {6, [ {'EQ', {stack, 0}, {stack, 0}, {immediate, 24}}
                      %% [C2.check==29, C1]
                    , {'NOT', {stack, 0}, {stack, 0}}
                      %% [C2.check!=29, C1]
                    , {'JUMPIF', {stack, 0}, {immediate, 12}}
                    ]
                }
                %% [C1]
                %% Check previous contract as well
              , {7, [ {'CALL_R',
                       {stack, 0},
                       {immediate, aeb_fate_code:symbol_identifier(<<"check">>)},
                       {immediate, {typerep, {tuple, []}}},
                       {immediate, {typerep, integer}},
                       {immediate, 0}
                      }
                    ]
                }
                %% [C1.check]
              , {8, [ {'EQ', {stack, 0}, {stack, 0}, {immediate, 13}}
                      %% [C1.check==18]
                    , {'NOT', {stack, 0}, {stack, 0}}
                      %% [C1.check!=18]
                    , {'JUMPIF', {stack, 0}, {immediate, 13}}
                    ]
                }
              , {9, [{'RETURNR', {immediate, 2137}}]}
              , {10, [{'ABORT', {immediate, <<"CLONED CONTRACT HAS THE SAME PK">>}}]}
              , {11, [{'ABORT', {immediate, <<"CLONED CONTRACT HAS DIFFERENT CODEHASH">>}}]}
              , {12, [{'ABORT', {immediate, <<"BAD STATE/VALUE SEPARATION ON YOUNG">>}}]}
              , {13, [{'ABORT', {immediate, <<"BAD STATE/VALUE SEPARATION ON OLD">>}}]}
              ]
             }
           , {<<"clone_gas_limit">>, {[], integer},
              [ {0, [ {'CLONE_G',
                       {immediate, aeb_fate_data:make_contract(pad_contract_name(<<"remote">>))},
                       {immediate, {typerep, {tuple, []}}},
                       {immediate, 0},
                       {immediate, 1},
                       {immediate, true}
                      }
                    ]
                }
              , {1, [ 'RETURN' ]
                }
              ]
             }
           ]
     , <<"abortion">> => %% this contract is not supposed to be born
           [ {<<"init">>, {[], {tuple, []}},
              [ {0, [ {'ABORT', {immediate, <<"thank u from saving me from the pain of existence">>}} ]}
              ]
             }
           ]
     , <<"chain_and_call">> =>
           [ {<<"call_fee">>, {[], integer},
              [{0, [ {'FEE', {stack, 0}}
                   , 'RETURN'
                   ]}
              ]
             }
           , {<<"call_gasprice">>, {[], integer},
              [{0, [ {'GASPRICE', {stack, 0}}
                   , 'RETURN'
                   ]}
              ]
             }
           ]
     , <<"remote">> =>
           [ {<<"add_five">>, {[integer], integer},
              [{0, [ {'ADD', {stack, 0}, {immediate, 5}, {arg, 0}}
                   , 'RETURN'
                   ]}]
             }
           , {<<"init">>, {[], {tuple, []}},
              [ {0, [ {'TUPLE', {stack, 0}, {immediate, 0}}
                    , 'RETURN'
                    ]}
              ]
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
               , {1, [ {'INC', {stack, 0}}
                     , {'JUMP', {immediate, 2}}
                     ]}
               , {2, [ 'RETURN']}
               ]}
           ]
     , <<"memory">> =>
           [ {<<"call">>
             , {[integer], integer}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'PUSH', {immediate, 0}}
                     , {'CALL', {immediate, aeb_fate_code:symbol_identifier(<<"write">>)}}
                     ]
                 }
               , {1, [ {'PUSH', {var, 1}}
                     , 'RETURN'
                     ]}
               ]}
           ,  {<<"call2">>
             , {[integer], integer}
             , [ {0, [ {'STORE', {var, 2}, {arg, 0}}
                     , {'PUSH', {immediate, 0}}
                     , {'CALL', {immediate, aeb_fate_code:symbol_identifier(<<"write">>)}}
                     ]
                 }
               , {1, [ {'PUSH', {var, 2}}
                     , 'RETURN'
                     ]}
               ]}
           ,  {<<"call3">>
             , {[integer], integer}
             , [ {0, [ {'STORE', {var, 1}, {arg, 0}}
                     , {'PUSH', {immediate, 0}}
                     , {'CALL', {immediate, aeb_fate_code:symbol_identifier(<<"read">>)}}
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
           , {<<"read">>
             , {[integer], integer}
             , [ {0, [ {'RETURNR', {var, 1}}
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
             , [ {0, [ {'TUPLE', {stack, 0}, {immediate, 0}}
                     , 'RETURN']}
               ]}
           , {<<"make_2tuple">>
             , {[integer, integer], {tuple, [integer, integer]}}
             , [ {0, [ {'PUSH', {arg, 0}}
                     , {'PUSH', {arg, 1}}
                     , {'TUPLE', {stack, 0}, {immediate, 2}}
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
                     , {'TUPLE', {stack, 0}, {immediate, 5}}
                     ,  'RETURN']}
               ]}
           , {<<"element1">>
             , {[integer, integer], integer}
             , [ {0, [ {'PUSH', {arg, 0}}
                     , {'PUSH', {arg, 1}}
                     , {'TUPLE', {stack, 0}, {immediate, 2}}
                     , {'ELEMENT', {stack, 0}, {immediate, 1}, {stack, 0}}
                     , 'RETURN']}
               ]}
           , {<<"element">>
             , {[{tuple, [integer, integer]}, integer], integer}
             , [ {0, [ {'ELEMENT', {stack, 0}, {arg, 1}, {arg, 0}}
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
           , {<<"map_to_list">>
             , {[{map, integer, boolean}], {list, {tuple, [integer, boolean]}}}
               , [ {0, [ {'MAP_TO_LIST', {stack, 0}, {arg, 0}}
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
             , [ {0, [ {'EQ', {stack, 0}, {arg, 0}, {arg, 1}}
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
             , {[{variant, [{tuple, []}, {tuple, [integer]}]}], integer}
             , [ {0, [ {'SWITCH_V2', {arg,0}, {immediate, 1}, {immediate, 2}}]}
               , {1, [{'RETURNR', {immediate, 0}}]}
               , {2, [{'RETURNR', {immediate, 42}}]}
               ]}
           , {<<"switch2">>
             , {[{variant, [{tuple, []}, {tuple, [integer]}, {tuple, [integer, integer]}]}], integer}
             , [ {0, [ {'SWITCH_V2', {arg,0}, {immediate, 1}, {immediate, 2}}]}
               , {1, [{'RETURNR', {immediate, 0}}]}
               , {2, [{'RETURNR', {immediate, 42}}]}
               ]}
           , {<<"test">>
             , {[{variant, [{tuple, []}, {tuple, [integer]}]}, integer], boolean}
             , [ {0, [ {'VARIANT_TEST', {stack, 0}, {arg,0}, {arg, 1}}
                     , 'RETURN']}
               ]}
           , {<<"element">>
             , {[{variant, [{tuple, []}, {tuple, [integer]}]}, integer], integer}
             , [ {0, [ {'VARIANT_ELEMENT', {stack, 0}, {arg,0}, {arg, 1}}
                     , 'RETURN']}
               ]}
           , {<<"make">>
             , {[{list, integer}, integer, integer, {list, integer}], {variant, [{tuple, []}, {tuple, [integer]}]}}
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
             , [ {0, [ 'BITS_ALLA'
                     , 'RETURN']}]}
           , {<<"all_n">>
             , {[integer], bits}
             , [ {0, [ {'BITS_ALL_N', {stack, 0}, {arg, 0}}
                     , 'RETURN']}]}
           ,  {<<"none">>
              , {[], bits}
              , [ {0, [ 'BITS_NONEA'
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
