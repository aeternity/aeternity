-module(aefa_fate).
-export([run/2]).

-export([get_trace/1]).

-include("aefa_data.hrl").

-ifdef(TEST).
-define(trace(I,S), S#{trace => [{I, erlang:process_info(self(), reductions)} |get_trace(S)]}).
-else.
-define(trace(I,S), S).
-endif.

run(What, Chain) ->
    EngineState = setup_engine(What, Chain),
    execute(EngineState).

execute(EngineState) ->
    Instructions = current_bb(EngineState),
    loop(Instructions, EngineState).

loop(Instructions, EngineState) ->
    case step(Instructions, EngineState) of
        {stop, FinalState} ->
            FinalState;
        {jump, BB, NewState} ->
            {NewInstructions, State2} = jump(BB, NewState),
            loop(NewInstructions, State2)
    end.

step([], EngineState) ->
    BB = next_bb_index(EngineState),
    {jump, BB, EngineState};
step([I|Is], EngineState0) ->
    ES = ?trace(I, EngineState0),
    case eval(I, ES) of
        {next, NewState} -> step(Is, NewState);
        {jump,_BB,_NewState} = Res -> Res;
        {stop, _NewState} = Res -> Res

    end.

%% ------------------------------------------------------
%% Call/return instructions
%% ------------------------------------------------------
eval(return, EngineState) ->
    pop_call_stack(EngineState);
eval({call_local, Function}, EngineState) ->
    Signature = get_function_signature(Function, EngineState),
    ok = check_signature(Signature, EngineState),
    ES2 = push_return_address(EngineState),
    {jump, 0,  set_current_function(Function, ES2)};
eval({tailcall_local, Function}, EngineState) ->
    Signature = get_function_signature(Function, EngineState),
    ok = check_signature(Signature, EngineState),
    {jump, 0,  set_current_function(Function, EngineState)};
eval({call_remote, Contract, Function}, EngineState) ->
    ES1 = push_return_address(EngineState),
    ES2 = set_function(Contract, Function, ES1),
    Signature = get_function_signature(Function, ES2),
    ok = check_signature(Signature, ES2),
    {jump, 0, ES2};
eval({tailcall_remote, Contract, Function}, EngineState) ->
    ES2 = set_function(Contract, Function, EngineState),
    Signature = get_function_signature(Function, ES2),
    ok = check_signature(Signature, ES2),
    {jump, 0, ES2};

%% ------------------------------------------------------
%% Control flow instructions
%% ------------------------------------------------------
eval({jump, BB}, EngineState) ->
    {jump, BB, EngineState};

eval({jumpif_a, BB}, EngineState) ->
    case is_true_a(EngineState) of
        {true, ES1} -> {jump, BB, ES1};
        {false, ES1} -> {next, ES1}
    end;

%% ------------------------------------------------------
%% Integer instructions
%% ------------------------------------------------------
eval(push_a_0, EngineState) ->
    {next, push_int(?MAKE_FATE_INTEGER(0), EngineState)};
eval(inc_a_1_a, EngineState) ->
    {next, inc_acc(EngineState)};
eval({add_a_i_a, X}, EngineState) ->
    {next, add_aia(X, EngineState)};
eval(add_a_a_a, EngineState) ->
    {next, add_aaa(EngineState)};
eval(sub_a_a_a, EngineState) ->
    {next, sub_aaa(EngineState)};
eval(mul_a_a_a, EngineState) ->
    {next, mul_aaa(EngineState)};
eval(div_a_a_a, EngineState) ->
    {next, div_aaa(EngineState)};
eval(mod_a_a_a, EngineState) ->
    {next, mod_aaa(EngineState)};
eval(pow_a_a_a, EngineState) ->
    {next, pow_aaa(EngineState)};



%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------
eval(lt_a_a_a, EngineState) ->
    {next, lt_aaa(EngineState)};
eval(gt_a_a_a, EngineState) ->
    {next, gt_aaa(EngineState)};
eval(elt_a_a_a, EngineState) ->
    {next, elt_aaa(EngineState)};
eval(egt_a_a_a, EngineState) ->
    {next, egt_aaa(EngineState)};
eval(eq_a_a_a, EngineState) ->
    {next, eq_aaa(EngineState)};
eval(neq_a_a_a, EngineState) ->
    {next, neq_aaa(EngineState)};


%% ------------------------------------------------------
%% Boolean instructions
%% ------------------------------------------------------
eval(push_a_true, EngineState) ->
    {next, push_bool(?FATE_TRUE, EngineState)};
eval(push_a_false, EngineState) ->
    {next, push_bool(?FATE_FALSE, EngineState)};
eval(and_a_a_a, EngineState) ->
    {next, and_aaa(EngineState)};
eval(or_a_a_a, EngineState) ->
    {next, or_aaa(EngineState)};
eval(not_a_a, EngineState) ->
    {next, not_aa(EngineState)};

%% ------------------------------------------------------
%% Integer instructions
%% ------------------------------------------------------
eval(dup, EngineState) ->
    {next, dup(EngineState)};



%% ------------------------------------------------------
%% Other Instructions
%% ------------------------------------------------------
eval(nop, EngineState) ->
    {next, EngineState}.




%% -----------------------------------------------------------



setup_engine(#{ contract := Contract
              , function := Function
              , arguments := Arguments}, Chain) ->
    ES1 = new_engine_state(Chain),
    ES2 = set_function(Contract, Function, ES1),
    ES3 = push_arguments(Arguments, ES2),
    ES3.



set_function(Contract, Function, #{ chain := Chain
                                  , contracts := Contracts} = ES) ->
    {ES2, Code} =
        case maps:get(Contract, Contracts, void) of
            void ->
                ContractCode = chain_get_contract(Contract, Chain),
                {ES#{contracts => maps:put(Contract, ContractCode, Contracts)},
                 ContractCode};
            ContractCode ->
                {ES, ContractCode}
        end,
    ES3 = ES2#{current_contract => Contract},
    ES4 = setup_functions(Code, ES3),
    set_current_function(Function, ES4).

%get_current_contract(#{current_contract := Contract}) ->
%    Contract.

setup_functions(ContractCode, ES) ->
    lists:foldl(
      fun({FunctionName, Signature, BBs}, State) ->
              set_function_code(FunctionName, Signature, BBs, State)
      end,
      ES,
      ContractCode).

set_current_function(Function, ES) ->
    ES1 = ES#{current_function => Function
             , current_bb => 0},
    BBs = get_function_code(Function, ES1),
    lists:foldl(
      fun({BB, Instructions}, State) ->
              set_instructions(BB, Instructions, State)
      end,
      ES1,
      BBs).

set_instructions(BB, Is, #{bbs := BBs} = ES) ->
    NewBBs = maps:put(BB, Is, BBs),
    maps:put(bbs, NewBBs, ES).

set_function_code(Name, Signature, BBs, #{functions := Functions} = ES) ->
    NewFunctions = maps:put(Name, {Signature, BBs}, Functions),
    maps:put(functions, NewFunctions, ES).

get_function_code(Name, #{functions := Functions}) ->
    case maps:get(Name, Functions, void) of
        void ->  throw({error, {trying_to_call_function, Name}});
        {_Signature, Code} -> Code
    end.

get_function_signature(Name,  #{functions := Functions}) ->
    case maps:get(Name, Functions, void) of
        void ->  throw({error, {trying_to_call_function, Name}});
        {Signature, _Code} -> Signature
    end.

check_signature({ArgTypes, _RetSignature},
                #{ accumulator := Acc
                 , accumulator_stack := Stack}
               =_EngineState) ->
    case check_arg_types(ArgTypes, [Acc, Stack]) of
        ok -> ok;
        {error, T, V}  ->
            throw({error, {value_does_not_match_type, V, T}})
    end.

check_arg_types([], _) -> ok;
check_arg_types([T|Ts], [A|As]) ->
    case check_type(T,A) of
        true -> check_arg_types(Ts, As);
        false -> {error, T, A}
    end.

check_type(integer, I) when ?IS_FATE_INTEGER(I) -> true;
check_type(boolean, B) when ?IS_FATE_BOOLEAN(B) -> true;
check_type({list,_ET}, L) when ?IS_FATE_LIST(L) -> true; %% TODO: check element type
check_type(_T, _V) -> false.



jump(BB, ES) ->
    NewES = set_current_bb_index(BB, ES),
    Instructions = current_bb(NewES),
    {Instructions, NewES}.

%% ------------------------------------------------------
%% Integer instructions
%% ------------------------------------------------------


push_int(I,
         #{ accumulator := X
          , accumulator_stack := Stack } = ES) when ?IS_FATE_INTEGER(I) ->
    ES#{ accumulator => I
       , accumulator_stack => [X|Stack]}.

add_aia(X, #{accumulator := Y} = ES) when ?IS_FATE_INTEGER(X)
                                          , ?IS_FATE_INTEGER(X) ->
    ES#{accumulator := ?MAKE_FATE_INTEGER(?FATE_INTEGER_VALUE(X)
                                          + ?FATE_INTEGER_VALUE(Y)
                                         )}.
add_aaa(#{ accumulator := A
         , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                      , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) + ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

sub_aaa(#{ accumulator := A
         , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                      , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) - ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

mul_aaa(#{ accumulator := A
         , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                      , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) * ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

div_aaa(#{ accumulator := A
         , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                      , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) div ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

mod_aaa(#{ accumulator := A
         , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                      , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) rem ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

pow_aaa(#{ accumulator := A
         , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                      , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => math:pow(?FATE_INTEGER_VALUE(A), ?FATE_INTEGER_VALUE(B))
       , accumulator_stack => Stack}.



inc_acc(#{accumulator := X} = ES) when ?IS_FATE_INTEGER(X) ->
    ES#{accumulator := ?MAKE_FATE_INTEGER(?FATE_INTEGER_VALUE(X)+1)}.

%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------
lt_aaa(#{ accumulator := A
        , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                     , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) < ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

gt_aaa(#{ accumulator := A
        , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                     , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) > ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

elt_aaa(#{ accumulator := A
        , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                     , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) =< ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

egt_aaa(#{ accumulator := A
        , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                     , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) >= ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

eq_aaa(#{ accumulator := A
        , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                     , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) =:= ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.

neq_aaa(#{ accumulator := A
        , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_INTEGER(A)
                                                     , ?IS_FATE_INTEGER(B) ->
    ES#{ accumulator => ?FATE_INTEGER_VALUE(A) =/= ?FATE_INTEGER_VALUE(B)
       , accumulator_stack => Stack}.


%% ------------------------------------------------------
%% Boolean instructions
%% ------------------------------------------------------
push_bool(B,
         #{ accumulator := A
          , accumulator_stack := Stack } = ES) when ?IS_FATE_BOOLEAN(B) ->
    ES#{ accumulator => B
       , accumulator_stack => [A|Stack]}.

and_aaa(#{ accumulator := A
          , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_BOOLEAN(A)
                                                       , ?IS_FATE_BOOLEAN(B) ->
    ES#{ accumulator => (A and B)
       , accumulator_stack => Stack}.

or_aaa(#{ accumulator := A
          , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_BOOLEAN(A)
                                                       , ?IS_FATE_BOOLEAN(B) ->
    ES#{ accumulator => (A or B)
       , accumulator_stack => Stack}.

not_aa(#{ accumulator := A} = ES) when ?IS_FATE_BOOLEAN(A) ->
    ES#{ accumulator => (not A)}.

is_true_a(#{ accumulator := A} = ES) when ?IS_FATE_BOOLEAN(A) ->
    pop(ES).


%% ------------------------------------------------------
%% Arguments & Accumulator (-stack)
%% ------------------------------------------------------


push_arguments(Args, #{ accumulator := X
                      , accumulator_stack := XS} = ES) ->
    push_arguments(lists:reverse(Args), X, XS, ES).

push_arguments([], Acc, Stack, ES) ->
    ES#{ accumulator := Acc
       , accumulator_stack := Stack};
push_arguments([A|As], Acc, Stack, ES ) ->
    push_arguments(As, A, [Acc, Stack], ES).


pop(#{ accumulator := X, accumulator_stack := []} = ES) ->
    {X, ES#{accumulator => ?FATE_VOID}};
pop(#{ accumulator := X, accumulator_stack := [V|Stack]} = ES) ->
    {X, ES#{ accumulator => V
           , accumulator_stack := Stack
           }}.

dup(#{ accumulator := X, accumulator_stack := Stack} = ES) ->
    ES#{ accumulator => X
       , accumulator_stack := [X|Stack]}.

%% ------------------------------------------------------
%% BBs
set_current_bb_index(BB, ES) ->
    ES#{ current_bb => BB }.

current_bb(#{ current_bb := BB} = ES) ->
    get_bb(BB, ES).

get_bb(BB, #{bbs := BBS}) ->
    case maps:get(BB, BBS, void) of
        void -> throw({error, {trying_to_reach_bb, BB}});
        Instrucitions -> Instrucitions
    end.

next_bb_index(#{ current_bb := BB}) ->
    %% TODO check BB + 1 exists.
    BB + 1.


%% ------------------------------------------------------
get_trace(#{trace := T}) -> T.


push_return_address(#{ current_bb := BB
                     , current_function := Function
                     , current_contract := Contract
                     , call_stack := Stack } = ES) ->
    ES#{ call_stack => [{Contract, Function, BB+1}|Stack]}.

pop_call_stack(#{ call_stack := []} = ES) -> {stop, ES};
pop_call_stack(#{ call_stack := [{Contract, Function, BB}| Rest]
                , current_contract := CurrentContract
                , current_function := CurrentFunction} = ES) ->
    ES1 = ES#{ call_stack => Rest},
    if CurrentContract =:= Contract -> %% Local return
            if CurrentFunction =:= Function -> % self return
                    {jump, BB, ES1};
               true -> %% Other function same contract
                    ES2 = set_current_function(Function, ES1),
                    {jump, BB, ES2}
            end;
       true -> %% Non local return
            ES2 = set_function(Contract, Function, ES1),
            {jump, BB, ES2}
    end.


new_engine_state(Chain) ->
    #{ current_bb => 0
     , bbs => #{}
     , memory => #{}
     , chain => Chain
     , trace => []
     , accumulator => ?FATE_VOID
     , accumulator_stack => []
     , functions =>  #{} %% Cashe for current contract.
     , contracts => #{} %% Cashe for loaded contracts.
     , current_contract => ?FATE_VOID
     , current_function => ?FATE_VOID
     , call_stack => []
     }.

%% ----------------------------

%% TODO: real chain interface
chain_get_contract(ContractAddress, #{ contracts :=  Contracts} = _Chain) ->
    case maps:get(ContractAddress, Contracts, void) of
        void -> throw({error, calling, ContractAddress});
        C -> C
    end.

