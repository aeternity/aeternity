%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Translator from Aesophia Icode to Aevm Assebly
%%% @end
%%% Created : 21 Dec 2017
%%%
%%%-------------------------------------------------------------------
-module(aeso_icode_to_asm).

-export([convert/2]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aeso_icode.hrl").

i(Code) -> aeb_opcodes:mnemonic(Code).

%% We don't track purity or statefulness in the type checker yet.
is_pure({FName, _, _, _})     -> FName == "init".
is_stateful({FName, _, _, _}) -> FName /= "init".

is_public({{builtin, _}, _, _, _}) -> false;
is_public(_) -> true.

convert(#{ contract_name := _ContractName
         , state_type := StateType
         , functions := Functions
              },
        _Options) ->
    %% Create a function dispatcher
    DispatchFun = {"_main", [{"arg", "_"}],
                   {switch, {var_ref, "arg"},
                    [{{tuple, [fun_hash(FName)|make_args(Args)]},
                      icode_seq([ hack_return_address(Fun, length(Args) + 1) ] ++
                                [ {funcall, {var_ref, "_copy_state"}, [prim_state]} || not is_pure(Fun) ] ++
                                [{encode, 0, TypeRep, {funcall, {var_ref, FName}, make_args(Args)}}])}
                     || Fun={FName, Args, _, TypeRep} <- Functions, is_public(Fun) ]},
                   word},
    %% Find the type-reps we need encoders for
    {InTypes, OutTypes} = remote_call_type_reps(Functions),
    TypeReps = all_type_reps([TypeRep || {_, _, _, TypeRep} <- Functions] ++ InTypes ++ [StateType]),
    OutTypeReps = all_type_reps(OutTypes ++ [StateType]),
    Encoders = [make_encoder(T) || T <- TypeReps],
    Decoders = [make_decoder(T) || T <- OutTypeReps],
    Library = [make_copymem(), make_copystate(StateType)],
    NewFunctions = Functions ++ [DispatchFun] ++ Encoders ++ Decoders ++ Library,
    %% Create a function environment
    Funs = [{Name, length(Args), make_ref()}
            || {Name, Args, _Body, _Type} <- NewFunctions],
    %% Create dummy code to call the main function with one argument
    %% taken from the stack
    StopLabel = make_ref(),
    StatefulStopLabel = make_ref(),
    MainFunction = lookup_fun(Funs, "_main"),
    DispatchCode = [%% read all call data into memory at address 32
                    i(?CALLDATASIZE),
                    push(0), push(32),
                    i(?CALLDATACOPY),
                    %% push two return addresses to stop, one for stateful
                    %% functions and one for non-stateful functions.
                    push_label(StopLabel),
                    push_label(StatefulStopLabel),
                    %% The first word of the calldata is a pointer to the
                    %% actual calldata.
                    push(32), i(?MLOAD),
                    jump(MainFunction),
                    jumpdest(StatefulStopLabel),
                    %% A pointer to a binary is on top of the stack
                    %% Get size of data area (it will be the last thing on the
                    %% heap, so we can use MSIZE to compute it).
                    dup(1),    %% Ptr Ptr
                    i(?MSIZE), %% MSize Ptr Ptr
                    i(?SUB),   %% Size Ptr
                    swap(1),   %% Ptr Size
                    %% Now we need to do the same thing for the state
                    push(0), i(?MLOAD),          %% State Ptr Size
                    assemble_expr(Funs, [{"state", "_"}, dummy, dummy], nontail, {encode, 0, StateType, {var_ref, "state"}}),
                                                 %% StateBinPtr State Ptr Size
                    push(0), i(?MSTORE), pop(1), %% Ptr Size  Mem[0] := StateBinPtr
                    i(?RETURN),
                    jumpdest(StopLabel),
                    %% Same as StatefulStopLabel above
                    dup(1), i(?MSIZE), i(?SUB), swap(1),
                    %% Set state pointer to 0 to indicate that we didn't change state
                    push(0), dup(1), i(?MSTORE),
                    i(?RETURN)
                   ],

    %% Code is a deep list of instructions, containing labels and
    %% references to them. Labels take the form {'JUMPDEST', Ref}, and
    %% references take the form {push_label, Ref}, which is translated
    %% into a PUSH instruction.
    Code = [assemble_function(Funs, Name, Args, Body)
            || {Name, Args, Body, _Type} <- NewFunctions],
    resolve_references(
        [%% i(?COMMENT), "CONTRACT: " ++ ContractName,
         DispatchCode,
         Code]).

make_args(Args) ->
    [{var_ref, [I-1 + $a]} || I <- lists:seq(1, length(Args))].

fun_hash(Name) ->
    {tuple, [{integer, X} || X <- [length(Name)|aeso_data:binary_to_words(list_to_binary(Name))]]}.

%% Expects two return addresses below N elements on the stack. Picks the top
%% one for stateful functions and the bottom one for non-stateful.
hack_return_address(Fun, N) ->
    case is_stateful(Fun) of
        true  -> {inline_asm, [i(?MSIZE)]};
        false ->
            {inline_asm,     %% X1 .. XN State NoState
             [ dup(N + 2)    %% NoState X1 .. XN State NoState
             , swap(N + 1)   %% State X1 .. XN NoState NoState
             ]}  %% Top of the stack will be discarded.
    end.

assemble_function(Funs, Name, Args, Body) ->
    [jumpdest(lookup_fun(Funs, Name)),
     assemble_expr(Funs, lists:reverse(Args), tail, Body),
     %% swap return value and first argument
     pop_args(length(Args)),
     swap(1),
     i(?JUMP)].

assemble_expr(_Funs, _Stack, _Tail, {inline_asm, Code}) ->
    Code;   %% Unsafe! Code should take care to respect the stack!
assemble_expr(Funs, Stack, _TailPosition, {var_ref, Id}) ->
    case lists:keymember(Id, 1, Stack) of
        true ->
            dup(lookup_var(Id, Stack));
        false ->
            %% Build a closure
            %% When a top-level fun is called directly, we do not
            %% reach this case.
            Eta = make_ref(),
            Continue = make_ref(),
            [i(?MSIZE),
             push_label(Eta),
             dup(2),
             i(?MSTORE),
             jump(Continue),
             %% the code of the closure
             jumpdest(Eta),
             %% pop the pointer to the function
             pop(1),
             jump(lookup_fun(Funs, Id)),
             jumpdest(Continue)]
    end;
assemble_expr(_, _, _, {missing_field, Format, Args}) ->
    io:format(Format, Args),
    error(missing_field);
assemble_expr(_Funs, _Stack, _, {integer, N}) ->
    push(N);
assemble_expr(Funs, Stack, _, {tuple, Cpts}) ->
    %% We build tuples right-to-left, so that the first write to the
    %% tuple extends the memory size. Because we use ?MSIZE as the
    %% heap pointer, we must allocate the tuple AFTER computing the
    %% first element.
    %% We store elements into the tuple as soon as possible, to avoid
    %% keeping them for a long time on the stack.
    case lists:reverse(Cpts) of
        [] ->
            i(?MSIZE);
        [Last|Rest] ->
            [assemble_expr(Funs, Stack, nontail, Last),
             %% allocate the tuple memory
             i(?MSIZE),
             %% compute address of last word
             push(32 * (length(Cpts) - 1)), i(?ADD),
             %% Stack: <last-value> <pointer>
             %% Write value to memory (allocates the tuple)
             swap(1), dup(2), i(?MSTORE),
             %% Stack: pointer to last word written
             [[%% Update pointer to next word to be written
               push(32), swap(1), i(?SUB),
               %% Compute element
               assemble_expr(Funs, [pointer|Stack], nontail, A),
               %% Write element to memory
               dup(2), i(?MSTORE)]
               %% And we leave a pointer to the last word written on
               %% the stack
              || A <- Rest]]
            %% The pointer to the entire tuple is on the stack
    end;
assemble_expr(_Funs, _Stack, _, {list, []}) ->
    %% Use Erik's value of -1 for []
    [push(0), i(?NOT)];
assemble_expr(Funs, Stack, _, {list, [A|B]}) ->
    assemble_expr(Funs, Stack, nontail, {tuple, [A, {list, B}]});
assemble_expr(Funs, Stack, _, {unop, '!', A}) ->
    case A of
        {binop, Logical, _, _} when Logical=='&&'; Logical=='||' ->
            assemble_expr(Funs, Stack, nontail, {ifte, A, {integer, 0}, {integer, 1}});
        _ ->
            [assemble_expr(Funs, Stack, nontail, A),
             i(?ISZERO)
            ]
    end;
assemble_expr(Funs, Stack, _, {unop, Op, A}) ->
    [assemble_expr(Funs, Stack, nontail, A),
     assemble_prefix(Op)];
assemble_expr(Funs, Stack, Tail, {binop, '&&', A, B}) ->
    assemble_expr(Funs, Stack, Tail, {ifte, A, B, {integer, 0}});
assemble_expr(Funs, Stack, Tail, {binop, '||', A, B}) ->
    assemble_expr(Funs, Stack, Tail, {ifte, A, {integer, 1}, B});
assemble_expr(Funs, Stack, Tail, {binop, '::', A, B}) ->
    %% Take advantage of optimizations in tuple construction.
    assemble_expr(Funs, Stack, Tail, {tuple, [A, B]});
assemble_expr(Funs, Stack, _, {binop, Op, A, B}) ->
    %% EEVM binary instructions take their first argument from the top
    %% of the stack, so to get operands on the stack in the right
    %% order, we evaluate from right to left.
    [assemble_expr(Funs, Stack, nontail, B),
     assemble_expr(Funs, [dummy|Stack], nontail, A),
     assemble_infix(Op)];
assemble_expr(Funs, Stack, _, {lambda, Args, Body}) ->
    Function = make_ref(),
    FunBody  = make_ref(),
    Continue = make_ref(),
    NoMatch  = make_ref(),
    FreeVars = free_vars({lambda, Args, Body}),
    {NewVars, MatchingCode} = assemble_pattern(FunBody, NoMatch, {tuple, [{var_ref, "_"}|FreeVars]}),
    BodyCode = assemble_expr(Funs, NewVars ++ lists:reverse([ {Arg#arg.name, Arg#arg.type} || Arg <- Args ]), tail, Body),
    [assemble_expr(Funs, Stack, nontail, {tuple, [{label, Function}|FreeVars]}),
     jump(Continue), %% will be optimized away
     jumpdest(Function),
     %% A pointer to the closure is on the stack
     MatchingCode,
     jumpdest(FunBody),
     BodyCode,
     pop_args(length(Args)+length(NewVars)),
     swap(1),
     i(?JUMP),
     jumpdest(NoMatch), %% dead code--raise an exception just in case
     push(0),
     i(?NOT),
     i(?MLOAD),
     i(?STOP),
     jumpdest(Continue)];
assemble_expr(_, _, _, {label, Label}) ->
    push_label(Label);
assemble_expr(Funs, Stack, _, {encode, Base, TypeRep, A}) ->
    %% Encode a value A of type TypeRep as a binary. Leaves a pointer to the
    %% binary on top of the stack.
    %%
    %% For boxed types the encoding is a relative pointer into a following heap fragment
    %% with relative addresses. `Base` is the relative address of the start of the binary,
    %% so that if the binary were written to address `Base` it would be a valid
    %% heap object.
    %% For instance with Pad = 0, the tuple ("pink", (99, "apple")) would be encoded as
    %%    Offset   0   32   64     96     128    160   192     224    256
    %%    Word   160   99   96      5  "apple"   224    32       4  "pink"
    %%
    %% For unboxed types the encoding is simply the value.

    SubBase = [ [push(Base), swap(1), i(?SUB)] || Base /= 0 ],

      %% First assemble the value to be encoded. This may allocate memory.
    [ assemble_expr(Funs, Stack, nontail, A), %% [ value ]
                                           %% Allocate space for the first word
      i(?MSIZE),                           %% [ addr value ]
      push(0),                             %% [ 0 addr value ]
      dup(2),                              %% [ addr 0 addr value ]
      i(?MSTORE),                          %% [ addr value ]    -- allocate
      swap(1), dup(2), SubBase,            %% [ base value addr ]
      %% Call the encoder function. This returns a relative pointer for boxed
      %% types, and `value` for unboxed types.
      assemble_expr(Funs, [{"base", "_"}, {"value", "_"} | Stack], nontail,
        {funcall, {var_ref, encoder_name(TypeRep)},
        [{var_ref, "base"}, {var_ref, "value"}]}),     %% [ result base value addr ]
      dup(4), i(?MSTORE),                              %% [ base value addr ]  Mem[addr] := result
      pop(2)];                                         %% [ addr ]
assemble_expr(Funs, Stack0, _Tail, {decode, TypeRep}) ->
    %% Inverse of encoding. Top of the stack should contain a pointer to the
    %% blob.
    [_ | Stack] = Stack0,
    [ dup(1),                                          %% [ ptr ptr ]
      i(?MLOAD),                                       %% [ value ptr ]
      assemble_expr(Funs, [{"value", "_"}, {"base", "_"} | Stack], nontail,
        {funcall, {var_ref, decoder_name(TypeRep)},
            [{var_ref, "base"}, {var_ref, "value"}]}), %% [ decoded value ptr ]
      pop_args(2) ];                                   %% [ decoded ]
assemble_expr(Funs, Stack, nontail, {funcall, Fun, Args}) ->
    Return = make_ref(),
    %% This is the obvious code:
    %%   [{push_label, Return},
    %%    assemble_exprs(Funs, [return_address|Stack], Args++[Fun]),
    %%    'JUMP',
    %%    {'JUMPDEST', Return}];
    %% Its problem is that it stores the return address on the stack
    %% while the arguments are computed, which is unnecessary. To
    %% avoid that, we compute the last argument FIRST, and replace it
    %% with the return address using a SWAP.
    %%
    %% assemble_function leaves the code pointer of the function to
    %% call on top of the stack, and--if the function is not a
    %% top-level name--a pointer to its tuple of free variables. In
    %% either case a JUMP is the right way to call it.
    case Args of
        [] ->
            [push_label(Return),
             assemble_function(Funs, [return_address|Stack], Fun),
             i(?JUMP),
             jumpdest(Return)];
        _ ->
            {Init, [Last]} = lists:split(length(Args) - 1, Args),
            [assemble_exprs(Funs, Stack, [Last|Init]),
             %% Put the return address in the right place, which also
             %% reorders the args correctly.
             push_label(Return),
             swap(length(Args)),
             assemble_function(Funs, [dummy || _ <- Args] ++ [return_address|Stack], Fun),
             i(?JUMP),
             jumpdest(Return)]
    end;
assemble_expr(Funs, Stack, tail, {funcall, Fun, Args}) ->
    IsTopLevel = is_top_level_fun(Funs, Stack, Fun),
    %% If the fun is not top-level, then it may refer to local
    %% variables and must be computed before stack shuffling.
    ArgsAndFun = Args++[Fun || not IsTopLevel],
    ComputeArgsAndFun = assemble_exprs(Funs, Stack, ArgsAndFun),
    %% Copy arguments back down the stack to the start of the frame
    ShuffleSpec = lists:seq(length(ArgsAndFun), 1, -1) ++ [discard || _ <- Stack],
    Shuffle = shuffle_stack(ShuffleSpec),
    [ComputeArgsAndFun, Shuffle,
     if IsTopLevel ->
             %% still need to compute function
             assemble_function(Funs, [], Fun);
        true ->
             %% need to unpack a closure
             [dup(1), i(?MLOAD)]
     end,
     i(?JUMP)];
assemble_expr(Funs, Stack, Tail, {ifte, Decision, Then, Else}) ->
    %% This compilation scheme introduces a lot of labels and
    %% jumps. Unnecessary ones are removed later in
    %% resolve_references.
    Close = make_ref(),
    ThenL = make_ref(),
    ElseL = make_ref(),
    [assemble_decision(Funs, Stack, Decision, ThenL, ElseL),
     jumpdest(ElseL),
     assemble_expr(Funs, Stack, Tail, Else),
     jump(Close),
     jumpdest(ThenL),
     assemble_expr(Funs, Stack, Tail, Then),
     jumpdest(Close)
    ];
assemble_expr(Funs, Stack, Tail, {switch, A, Cases}) ->
    Close = make_ref(),
    [assemble_expr(Funs, Stack, nontail, A),
     assemble_cases(Funs, Stack, Tail, Close, Cases),
     {'JUMPDEST', Close}];
%% State primitives
%%  (A pointer to) the contract state is stored at address 0.
assemble_expr(_Funs, _Stack, _Tail, prim_state) ->
    [push(0), i(?MLOAD)];
assemble_expr(Funs, Stack, _Tail, #prim_put{ state = State }) ->
    [assemble_expr(Funs, Stack, nontail, State),
     push(0), i(?MSTORE),   %% We need something for the unit value on the stack,
     i(?MSIZE)];            %% MSIZE is the cheapest instruction.
%% Environment primitives
assemble_expr(_Funs, _Stack, _Tail, prim_contract_address) ->
    [i(?ADDRESS)];
assemble_expr(_Funs, _Stack, _Tail, prim_call_origin) ->
    [i(?ORIGIN)];
assemble_expr(_Funs, _Stack, _Tail, prim_caller) ->
    [i(?CALLER)];
assemble_expr(_Funs, _Stack, _Tail, prim_call_value) ->
    [i(?CALLVALUE)];
assemble_expr(_Funs, _Stack, _Tail, prim_gas_price) ->
    [i(?GASPRICE)];
assemble_expr(_Funs, _Stack, _Tail, prim_coinbase) ->
    [i(?COINBASE)];
assemble_expr(_Funs, _Stack, _Tail, prim_timestamp) ->
    [i(?TIMESTAMP)];
assemble_expr(_Funs, _Stack, _Tail, prim_block_height) ->
    [i(?NUMBER)];
assemble_expr(_Funs, _Stack, _Tail, prim_difficulty) ->
    [i(?DIFFICULTY)];
assemble_expr(_Funs, _Stack, _Tail, prim_gas_limit) ->
    [i(?GASLIMIT)];
assemble_expr(Funs, Stack, _Tail, #prim_balance{ address = Addr }) ->
    [assemble_expr(Funs, Stack, nontail, Addr),
     i(?BALANCE)];
assemble_expr(Funs, Stack, _Tail, #prim_block_hash{ height = Height }) ->
    [assemble_expr(Funs, Stack, nontail, Height),
     i(?BLOCKHASH)];
assemble_expr(Funs, Stack, Tail,
              #prim_call_contract{ gas      = Gas
                                 , address  = To
                                 , value    = Value
                                 , arg      = Arg
                                 , arg_type = ArgT
                                 , out_type = OutT }) ->
    %% ?CALL takes (from the top)
    %%   Gas, To, Value, IOffset, ISize, OOffset, OSize
    %% We need to
    %%  - encode the argument
    %%  - push the arguments to ?CALL
    %%  - ?CALL
    %%  - decode the return value
    OSize = 0,  %% Note: The ?CALL instruction takes the out size as an argument, but
                %%       our EVM implementation doesn't actually need it.
    %% Note: assemble expressions first, since this may allocate memory.
    Exprs = [Value, Gas, To],                 %% Stack
    [%% i(?COMMENT(io_lib:format("Call primop: To ~p, Value: ~p", [To, Value]))),
      assemble_exprs(Funs, Stack, Exprs)      %%  To Gas Value
    , assemble_expr(Funs, [dummy, dummy, dummy | Stack],
            nontail, {encode, 32, ArgT, Arg}) %%  IOffset To Gas Value
    , i(?MSIZE)                               %%  OOffset IOffset To Gas Value
    , dup(2), dup(2)                          %%  OOffset IOffset OOffset IOffset To Gas Value
    , i(?SUB)                                 %%  ISize OOffset IOffset To Gas Value
    , push(OSize)                             %%  OSize ISize OOffset IOffset To Gas Value
    , dup(3)                                  %%  OOffset OSize ISize OOffset IOffset To Gas Value
    , shuffle_stack([1, 2, 4, 3, 5, 7, 8, 6]) %%  Gas To Value IOffset ISize OOffset OSize OOffset
    , i(?CALL)                                %%  Result OOffset
    , pop(1)                                  %%  OOffset         -- ignoring result for now
    , assemble_expr(Funs, [pointer | Stack], Tail, {decode, OutT})
                                              %%  ResultPtr
    ].


assemble_exprs(_Funs, _Stack, []) ->
    [];
assemble_exprs(Funs, Stack, [E|Es]) ->
    [assemble_expr(Funs, Stack, nontail, E),
     assemble_exprs(Funs, [dummy|Stack], Es)].

assemble_decision(Funs, Stack, {binop, '&&', A, B}, Then, Else) ->
    Label = make_ref(),
    [assemble_decision(Funs, Stack, A, Label, Else),
     jumpdest(Label),
     assemble_decision(Funs, Stack, B, Then, Else)];
assemble_decision(Funs, Stack, {binop, '||', A, B}, Then, Else) ->
    Label = make_ref(),
    [assemble_decision(Funs, Stack, A, Then, Label),
     jumpdest(Label),
     assemble_decision(Funs, Stack, B, Then, Else)];
assemble_decision(Funs, Stack, {unop, '!', A}, Then, Else) ->
    assemble_decision(Funs, Stack, A, Else, Then);
assemble_decision(Funs, Stack, {ifte, A, B, C}, Then, Else) ->
    TrueL  = make_ref(),
    FalseL = make_ref(),
    [assemble_decision(Funs, Stack, A, TrueL, FalseL),
     jumpdest(TrueL),  assemble_decision(Funs, Stack, B, Then, Else),
     jumpdest(FalseL), assemble_decision(Funs, Stack, C, Then, Else)];
assemble_decision(Funs, Stack, Decision, Then, Else) ->
    [assemble_expr(Funs, Stack, nontail, Decision),
     jump_if(Then), jump(Else)].

%% Entered with value to switch on on top of the stack
%% Evaluate selected case, then jump to Close with result on the
%% stack.
assemble_cases(_Funs, _Stack, _Tail, _Close, []) ->
    %% No match! What should be do? There's no real way to raise an
    %% exception, except consuming all the gas.
    %% There should not be enough gas to do this:
    [push(1), i(?NOT),
     i(?MLOAD),
     %% now stop, so that jump optimizer realizes we will not fall
     %% through this code.
     i(?STOP)];
assemble_cases(Funs, Stack, Tail, Close, [{Pattern, Body}|Cases]) ->
    Succeed = make_ref(),
    Fail = make_ref(),
    {NewVars, MatchingCode} =
        assemble_pattern(Succeed, Fail, Pattern),
    %% In the code that follows, if this is NOT the last case, then we
    %% save the value being switched on, and discard it on
    %% success. The code is simpler if this IS the last case.
    [[dup(1) || Cases /= []],   %% save value for next case, if there is one
     MatchingCode,
     jumpdest(Succeed),
     %% Discard saved value, if we saved one
     [case NewVars of
          [] ->
              pop(1);
          [_] ->
              %% Special case for peep-hole optimization
              pop_args(1);
          _ ->
              [swap(length(NewVars)), pop(1)]
      end
      || Cases/=[]],
     assemble_expr(Funs,
                   case Cases of
                       [] -> NewVars;
                       _  -> reorder_vars(NewVars)
                   end
                   ++Stack, Tail, Body),
     %% If the Body makes a tail call, then we will not return
     %% here--but it doesn't matter, because
     %% (a) the NewVars will be popped before the tailcall
     %% (b) the code below will be deleted since it is dead
     pop_args(length(NewVars)),
     jump(Close),
     jumpdest(Fail),
     assemble_cases(Funs, Stack, Tail, Close, Cases)].

%% Entered with value to match on top of the stack.
%% Generated code removes value, and
%%   - jumps to Fail if no match, or
%%   - binds variables, leaves them on the stack, and jumps to Succeed
%% Result is a list of variables to add to the stack, and the matching
%% code.
assemble_pattern(Succeed, Fail, {integer, N}) ->
    {[], [push(N),
         i(?EQ),
         jump_if(Succeed),
         jump(Fail)]};
assemble_pattern(Succeed, _Fail, {var_ref, "_"}) ->
    {[], [i(?POP), jump(Succeed)]};
assemble_pattern(Succeed, Fail, {missing_field, _, _}) ->
    %% Missing record fields are quite ok in patterns.
    assemble_pattern(Succeed, Fail, {var_ref, "_"});
assemble_pattern(Succeed, _Fail, {var_ref, Id}) ->
    {[{Id, "_"}], jump(Succeed)};
assemble_pattern(Succeed, _Fail, {tuple, []}) ->
    {[], [pop(1), jump(Succeed)]};
assemble_pattern(Succeed, Fail, {tuple, [A]}) ->
    %% Treat this case specially, because we don't need to save the
    %% pointer to the tuple.
    {AVars, ACode} = assemble_pattern(Succeed, Fail, A),
    {AVars, [i(?MLOAD),
            ACode]};
assemble_pattern(Succeed, Fail, {tuple, [A|B]}) ->
    %% Entered with the address of the tuple on the top of the
    %% stack. We will duplicate the address before matching on A.
    Continue = make_ref(),  %% the label for matching B
    Pop1Fail = make_ref(),  %% pop 1 word and goto Fail
    PopNFail = make_ref(),  %% pop length(AVars) words and goto Fail
    {AVars, ACode} =
        assemble_pattern(Continue, Pop1Fail, A),
    {BVars, BCode} =
        assemble_pattern(Succeed, PopNFail, {tuple, B}),
    {BVars ++ reorder_vars(AVars),
     [%% duplicate the pointer so we don't lose it when we match on A
      dup(1),
      i(?MLOAD),
      ACode,
      jumpdest(Continue),
      %% Bring the pointer to the top of the stack--this reorders AVars!
      swap(length(AVars)),
      push(32),
      i(?ADD),
      BCode,
      case AVars of
          [] ->
              [jumpdest(Pop1Fail), pop(1),
               jumpdest(PopNFail),
               jump(Fail)];
          _ ->
              [{'JUMPDEST', PopNFail}, pop(length(AVars)-1),
               {'JUMPDEST', Pop1Fail}, pop(1),
               {push_label, Fail}, 'JUMP']
      end]};
assemble_pattern(Succeed, Fail, {list, []}) ->
    %% [] is represented by -1.
    {[], [push(1),
          i(?ADD),
          jump_if(Fail),
          jump(Succeed)]};
assemble_pattern(Succeed, Fail, {list, [A|B]}) ->
    assemble_pattern(Succeed, Fail, {binop, '::', A, {list, B}});
assemble_pattern(Succeed, Fail, {binop, '::', A, B}) ->
    %% Make sure it's not [], then match as tuple.
    NotNil = make_ref(),
    {Vars, Code} = assemble_pattern(Succeed, Fail, {tuple, [A, B]}),
    {Vars, [dup(1), push(1), i(?ADD),
            jump_if(NotNil),
            jump(Fail),
            jumpdest(NotNil),
            Code]}.

%% When Vars are on the stack, with a value we want to discard
%% below them, then we swap the top variable with that value and pop.
%% This reorders the variables on the stack, as follows:
reorder_vars([]) ->
    [];
reorder_vars([V|Vs]) ->
    Vs ++ [V].

assemble_prefix('-') -> [push(0), i(?SUB)];
assemble_prefix('bnot') -> i(?NOT).

assemble_infix('+')    -> i(?ADD);
assemble_infix('-')    -> i(?SUB);
assemble_infix('*')    -> i(?MUL);
assemble_infix('/')    -> i(?SDIV);
assemble_infix('bor')  -> i(?OR);
assemble_infix('band') -> i(?AND);
assemble_infix('bxor') -> i(?XOR);
assemble_infix('<')    -> i(?SLT);    %% comparisons are SIGNED
assemble_infix('>')    -> i(?SGT);
assemble_infix('==')   -> i(?EQ);
assemble_infix('<=')   -> [i(?SGT), i(?ISZERO)];
assemble_infix('=<')   -> [i(?SGT), i(?ISZERO)];
assemble_infix('>=')   -> [i(?SLT), i(?ISZERO)];
assemble_infix('!=')   -> [i(?EQ), i(?ISZERO)];
assemble_infix('!')    -> [i(?ADD), i(?MLOAD)].
%% assemble_infix('::') -> [i(?MSIZE), write_word(0), write_word(1)].

%% a function may either refer to a top-level function, in which case
%% we fetch the code label from Funs, or it may be a lambda-expression
%% (including a top-level function passed as a parameter). In the
%% latter case, the function value is a pointer to a tuple of the code
%% pointer and the free variables: we keep the pointer and push the
%% code pointer onto the stack. In either case, we are ready to enter
%% the function with JUMP.
assemble_function(Funs, Stack, Fun) ->
    case is_top_level_fun(Funs, Stack, Fun) of
        true ->
            {var_ref, Name} = Fun,
            {push_label, lookup_fun(Funs, Name)};
        false ->
            [assemble_expr(Funs, Stack, nontail, Fun),
             dup(1),
             i(?MLOAD)]
    end.

free_vars(V={var_ref, _}) ->
    [V];
free_vars({switch, E, Cases}) ->
    lists:umerge(free_vars(E),
                 lists:umerge([free_vars(Body)--free_vars(Pattern)
                               || {Pattern, Body} <- Cases]));
free_vars({lambda, Args, Body}) ->
    free_vars(Body) -- [{var_ref, Arg#arg.name} || Arg <- Args];
free_vars(T) when is_tuple(T) ->
    free_vars(tuple_to_list(T));
free_vars([H|T]) ->
    lists:umerge(free_vars(H), free_vars(T));
free_vars(_) ->
    [].




%% Type-directed encoders

%% We need encoding functions for the argument types of remote contract calls.
remote_call_type_reps(X) -> remote_call_type_reps(X, {[], []}).

remote_call_type_reps(#prim_call_contract{ arg_type = ArgT, out_type = OutT }, {In, Out}) ->
    {[ArgT | In], [OutT | Out]};
remote_call_type_reps([H | T], Acc) ->
    remote_call_type_reps(T, remote_call_type_reps(H, Acc));
remote_call_type_reps(T, Acc) when is_tuple(T) ->
    remote_call_type_reps(tuple_to_list(T), Acc);
remote_call_type_reps(M, Acc) when is_map(M) ->
    remote_call_type_reps(maps:values(M), Acc);
remote_call_type_reps(_, Acc) -> Acc.

%% Given the type-reps appearing in the program, include children and
%% eliminate duplicates.
all_type_reps(InSource) ->
    all_type_reps(InSource, []).

all_type_reps([], Found) ->
    Found;
all_type_reps([TR|InSource], Found) ->
    case lists:member(TR, Found) of
        true ->
            all_type_reps(InSource, Found);
        false ->
            Nested = case TR of
                         {tuple, TRs} -> TRs;
                         {list, T}    -> [T];
                         {option, T}  -> [T];
                         typerep      -> [{list, typerep}]; %% tuple case has a list of typereps
                         _            -> []
                     end,
            all_type_reps(Nested ++ InSource, [TR|Found])
    end.

%% We generate encoder function definitions for each type
%% rep. Encoders take a base address as an argument, and a value to
%% copy, and construct a copy of the value with all addresses relative
%% to the base address. Very untyped!
%%
%% More precisely:
%%
%%  _encode_T(base, value) returns a copy of `value` with all pointers relative
%%  to `base`. So, for unboxed types (word) it simply returns `value`, and for
%%  boxed types it returns an offset from `base` where the encoded value is
%%  stored. Thus, if called with the top of the heap as `base`, it returns 0
%%  (for boxed types).

encoder_name(TR) ->
    "_encode_" ++ lists:flatten(io_lib:write(TR)).

decoder_name(TR) ->
    "_decode_" ++ lists:flatten(io_lib:write(TR)).

make_encoder(TR) ->
    {encoder_name(TR), [{"base", "_"}, {"value", "_"}], make_encoder_body(TR), word}.

make_decoder(TR) ->
    {decoder_name(TR), [{"base", "_"}, {"value", "_"}], make_decoder_body(TR), TR}.

make_encoder_body(word) ->
    {var_ref, "value"};
make_encoder_body(typerep) ->
    Con = fun(Tag, Args) -> {tuple, [{integer, Tag} | Args]} end,
    Rel = fun(E) -> {binop, '-', E, {var_ref, "base"}} end,
    Enc = fun(T, X) -> {funcall, {var_ref, encoder_name(T)}, [{var_ref, X}]} end,
    {switch, {var_ref, "value"},
        [{Con(?TYPEREP_WORD_TAG, []),   Rel(Con(?TYPEREP_WORD_TAG, []))},
         {Con(?TYPEREP_STRING_TAG, []), Rel(Con(?TYPEREP_STRING_TAG, []))},
         {Con(?TYPEREP_LIST_TAG, [{var_ref, "t"}]),
            Rel(Con(?TYPEREP_LIST_TAG, [Enc(typerep, "t")]))},
         {Con(?TYPEREP_OPTION_TAG, [{var_ref, "t"}]),
            Rel(Con(?TYPEREP_OPTION_TAG, [Enc(typerep, "t")]))},
         {Con(?TYPEREP_TUPLE_TAG, [{var_ref, "ts"}]),
            Rel(Con(?TYPEREP_TUPLE_TAG, [Enc({list, typerep}, "ts")]))}]};
make_encoder_body(string) ->
    %% matching against a singleton tuple reads an address
    {switch, {var_ref, "value"},
     [{{tuple, [{var_ref, "length"}]},
       %% allocate the first word
      {switch, {tuple, [{var_ref, "length"}]},
       [{{var_ref, "result"},
         {funcall, {var_ref, "_copymem"},
          [%% address to copy from
           {binop, '+', {integer, 32}, {var_ref, "value"}},
           %% number of bytes to copy
           {var_ref, "length"},
           %% final result
           {binop, '-', {var_ref, "result"}, {var_ref, "base"}}
          ]}}
       ]}
      }]};
make_encoder_body({tuple, TRs}) ->
    Vars = make_vars(length(TRs)),
    {switch, {var_ref, "value"},
     [{{tuple, Vars},
       {binop, '-',
        {tuple, [{funcall, {var_ref, encoder_name(TR)}, [{var_ref, "base"}, V]}
                || {TR, V} <- lists:zip(TRs, Vars)]},
        {var_ref, "base"}}}
     ]};
make_encoder_body({list, TR}) ->
    {switch, {var_ref, "value"},
     [{{list, []}, {list, []}},
      {{tuple, [{var_ref, "head"}, {var_ref, "tail"}]},
       {binop, '-',
        {tuple, [{funcall, {var_ref, encoder_name(TR)},
                 [{var_ref, "base"}, {var_ref, "head"}]},
                {funcall, {var_ref, encoder_name({list, TR})},
                 [{var_ref, "base"}, {var_ref, "tail"}]}]},
        {var_ref, "base"}}}
     ]};
make_encoder_body({option, TR}) ->
    {switch, {var_ref, "value"},
     [{{list, []}, {list, []}},
      {{tuple, [{var_ref, "elem"}]},
       {binop, '-',
        {tuple, [{funcall, {var_ref, encoder_name(TR)},
                 [{var_ref, "base"}, {var_ref, "elem"}]}]},
        {var_ref, "base"}}}
     ]};
make_encoder_body(function) ->
    {integer, 33333333333333333}.

%% TODO: update pointers in-place so save memory!
%% Note: we never need to decode typereps.
make_decoder_body(word) ->
    {var_ref, "value"};
make_decoder_body({tuple, []}) ->
    {integer, 0};
make_decoder_body(string) ->
    %% the value is a relative pointer to the string, so add base to it
    {binop, '+', {var_ref, "value"}, {var_ref, "base"}};
make_decoder_body({tuple, TRs}) ->
    %% value: relative pointer to tuple cell
    Ptr  = {binop, '+', {var_ref, "value"}, {var_ref, "base"}},
    Vars = make_vars(length(TRs)),
    Body = {tuple, [ {funcall, {var_ref, decoder_name(TR)}, [{var_ref, "base"}, X]}
                     || {X, TR} <- lists:zip(Vars, TRs) ]},
    {switch, Ptr, [{{tuple, Vars}, Body}]};
make_decoder_body({list, TR}) ->
    Ptr  = {binop, '+', {var_ref, "value"}, {var_ref, "base"}},
    Head = {var_ref, "head"},
    Tail = {var_ref, "tail"},
    Nil  = {list, []},
    Decode = fun(T, V) ->
                {funcall, {var_ref, decoder_name(T)},
                          [{var_ref, "base"}, V]}
             end,
    {switch, {var_ref, "value"},
        [{Nil, Nil},
         {{var_ref, "_"},
            {switch, Ptr, [{{tuple, [Head, Tail]},
                {tuple, [Decode(TR, Head), Decode({list, TR}, Tail)]}}]}}]};
make_decoder_body({option, TR}) ->
    Ptr  = {binop, '+', {var_ref, "value"}, {var_ref, "base"}},
    Elem = {var_ref, "elem"},
    None = {list, []},
    Decode = fun(T, V) ->
                {funcall, {var_ref, decoder_name(T)},
                          [{var_ref, "base"}, V]}
             end,
    {switch, {var_ref, "value"},
        [{None, None},
         {{var_ref, "_"},
            {switch, Ptr, [{{tuple, [Elem]},
                {tuple, [Decode(TR, Elem)]}}]}}]};
make_decoder_body(function) ->
    error(cannot_decode_function_types).

make_vars(N) ->
    [{var_ref, "_v" ++ integer_to_list(I)} || I <- lists:seq(1, N)].

%% Generates a definition of a function to copy N bytes from address A
%% to the heap pointer, and return the final argument.
make_copymem() ->
    {"_copymem", [{"addr", "_"}, {"length", "_"}, {"result", "_"}],
     {ifte, {binop, '>', {var_ref, "length"}, {integer, 0}},
      %% read the word at addr
      {switch, {var_ref, "addr"},
       [{{tuple, [{var_ref, "word"}]},
         %% write the word at the heap pointer
         {switch, {tuple, [{var_ref, "word"}]},
          [{{var_ref, "_"},
            %% and loop
            {funcall, {var_ref, "_copymem"},
             [{binop, '+', {var_ref, "addr"}, {integer, 32}},
              {binop, '-', {var_ref, "length"}, {integer, 32}},
              {var_ref, "result"}]}}]}}]},
      {var_ref, "result"}},
    word}.

%% Generates a function _copy_state that decodes a binary blob at the argument
%% address.
make_copystate(StateType) ->
    {"_copy_state", [{"addr", "_"}],
     #switch{expr = {decode, StateType},  %% Replaces addr with new state pointer
             cases =
                [{#var_ref{name = "addr"},
                 {inline_asm,
                    [ dup(1), push(0), i(?MSTORE)
                    , i(?MSIZE), i(?MSIZE)  %% dummy result of switch and dummy return
                    ]}}] },
     {tuple, []}}.

%% shuffle_stack reorders the stack, for example before a tailcall. It is called
%% with a description of the current stack, and how the final stack
%% should appear. The argument is a list containing
%%   a NUMBER for each element that should be kept, the number being
%%     the position this element should occupy in the final stack
%%   discard, for elements that can be discarded.
%% The positions start at 1, referring to the variable to be placed at
%% the bottom of the stack, and ranging up to the size of the final stack.
shuffle_stack([]) ->
    [];
shuffle_stack([discard|Stack]) ->
    [i(?POP) | shuffle_stack(Stack)];
shuffle_stack([N|Stack]) ->
    case length(Stack) + 1 - N of
        0 ->
            %% the job should be finished
            CorrectStack = lists:seq(N - 1, 1, -1),
            CorrectStack = Stack,
            [];
        MoveBy ->
            {Pref, [_|Suff]} = lists:split(MoveBy - 1, Stack),
            [swap(MoveBy) | shuffle_stack([lists:nth(MoveBy, Stack) | Pref ++ [N|Suff]])]
    end.



lookup_fun(Funs, Name) ->
    case [Ref || {Name1, _, Ref} <- Funs,
                 Name == Name1] of
        [Ref] -> Ref;
        []    -> error({undefined_function, Name})
    end.

is_top_level_fun(_Funs, Stack, {var_ref, Id}) ->
    not lists:keymember(Id, 1, Stack);
is_top_level_fun(_, _, _) ->
    false.

lookup_var(Id, Stack) ->
    lookup_var(1, Id, Stack).

lookup_var(N, Id, [{Id, _Type}|_]) ->
    N;
lookup_var(N, Id, [_|Stack]) ->
    lookup_var(N + 1, Id, Stack);
lookup_var(_, Id, []) ->
    error({var_not_in_scope, Id}).

%% Smart instruction generation

%% TODO: handle references to the stack beyond depth 16. Perhaps the
%% best way is to repush variables that will be needed in
%% subexpressions before evaluating he subexpression... i.e. fix the
%% problem in assemble_expr, rather than here. A fix here would have
%% to save the top elements of the stack in memory, duplicate the
%% targetted element, and then repush the values from memory.
dup(N) when 1 =< N, N =< 16 ->
    i(?DUP1 + N - 1).

push(N) ->
    Bytes = binary:encode_unsigned(N),
    true = size(Bytes) =< 32,
    [i(?PUSH1 + size(Bytes) - 1) |
     binary_to_list(Bytes)].

%% Pop N values from UNDER the top element of the stack.
%% This is a pseudo-instruction so peephole optimization can
%% combine pop_args(M), pop_args(N) to pop_args(M+N)
pop_args(0) ->
    [];
pop_args(N) ->
    {pop_args, N}.
%%    [swap(N), pop(N)].

pop(N) ->
    [i(?POP) || _ <- lists:seq(1, N)].

swap(0) ->
    %% Doesn't exist, but is logically a no-op.
    [];
swap(N) when 1 =< N, N =< 16 ->
    i(?SWAP1 + N - 1).

jumpdest(Label)   -> {i(?JUMPDEST), Label}.
push_label(Label) -> {push_label, Label}.

jump(Label)    -> [push_label(Label), i(?JUMP)].
jump_if(Label) -> [push_label(Label), i(?JUMPI)].

%% ICode utilities (TODO: move to separate module)

icode_noname() -> #var_ref{name = "_"}.

icode_seq([A]) -> A;
icode_seq([A | As]) ->
    icode_seq(A, icode_seq(As)).

icode_seq(A, B) ->
    #switch{ expr = A, cases = [{icode_noname(), B}] }.

%% Stack: <N elements> ADDR
%% Write elements at addresses ADDR, ADDR+32, ADDR+64...
%% Stack afterwards: ADDR
% write_words(N) ->
%      [write_word(I) || I <- lists:seq(N-1, 0, -1)].

%% Unused at the moment. Comment out to please dialyzer.
%% write_word(I) ->
%%     [%% Stack: elements e ADDR
%%        swap(1),
%%        dup(2),
%%        %% Stack: elements ADDR e ADDR
%%        push(32*I),
%%        i(?ADD),
%%        %% Stack: elements ADDR e ADDR+32I
%%        i(?MSTORE)].

%% Resolve references, and convert code from deep list to flat list.
%% List elements are:
%%   Opcodes
%%   Byte values
%%   {'JUMPDEST', Ref}   -- assembles to ?JUMPDEST and sets Ref
%%   {push_label, Ref}  -- assembles to ?PUSHN address bytes

%% For now, we assemble all code addresses as three bytes.

resolve_references(Code) ->
    Peephole = peep_hole(lists:flatten(Code)),
    %% WARNING: Optimizing jumps reorders the code and deletes
    %% instructions. When debugging the assemble_ functions, it can be
    %% useful to replace the next line by:
    %% Instrs = lists:flatten(Code),
    %% thus disabling the optimization.
    OptimizedJumps = optimize_jumps(Peephole),
    Instrs = lists:reverse(peep_hole_backwards(lists:reverse(OptimizedJumps))),
    Labels = define_labels(0, Instrs),
    lists:flatten([use_labels(Labels, I) || I <- Instrs]).

define_labels(Addr, [{'JUMPDEST', Lab}|More]) ->
    [{Lab, Addr}|define_labels(Addr + 1, More)];
define_labels(Addr, [{push_label, _}|More]) ->
    define_labels(Addr + 4, More);
define_labels(Addr, [{pop_args, N}|More]) ->
    define_labels(Addr + N + 1, More);
define_labels(Addr, [_|More]) ->
    define_labels(Addr + 1, More);
define_labels(_, []) ->
    [].

use_labels(_, {'JUMPDEST', _}) ->
    'JUMPDEST';
use_labels(Labels, {push_label, Ref}) ->
    case proplists:get_value(Ref, Labels) of
        undefined ->
            error({undefined_label, Ref});
        Addr when is_integer(Addr) ->
            [i(?PUSH3),
             Addr div 65536, (Addr div 256) rem 256, Addr rem 256]
    end;
use_labels(_, {pop_args, N}) ->
    [swap(N), pop(N)];
use_labels(_, I) ->
    I.

%% Peep-hole optimization.
%% The compilation of conditionals can introduce jumps depending on
%% constants 1 and 0. These are removed by peep-hole optimization.

peep_hole(['PUSH1', 0, {push_label, _}, 'JUMPI'|More]) ->
    peep_hole(More);
peep_hole(['PUSH1', 1, {push_label, Lab}, 'JUMPI'|More]) ->
    [{push_label, Lab}, 'JUMP'|peep_hole(More)];
peep_hole([{pop_args, M}, {pop_args, N}|More]) when M + N =< 16 ->
    peep_hole([{pop_args, M + N}|More]);
peep_hole([I|More]) ->
    [I|peep_hole(More)];
peep_hole([]) ->
    [].

%% Peep-hole optimization on reversed instructions lists.

peep_hole_backwards(Code) ->
    NewCode = peep_hole_backwards1(Code),
    if Code == NewCode -> Code;
       true            -> peep_hole_backwards(NewCode)
    end.

peep_hole_backwards1(['ADD', 0, 'PUSH1'|Code]) ->
    peep_hole_backwards1(Code);
peep_hole_backwards1(['POP', UnOp|Code]) when UnOp=='MLOAD';UnOp=='ISZERO';UnOp=='NOT' ->
    peep_hole_backwards1(['POP'|Code]);
peep_hole_backwards1(['POP', BinOp|Code]) when
    %% TODO: more binary operators
    BinOp=='ADD';BinOp=='SUB';BinOp=='MUL';BinOp=='SDIV' ->
    peep_hole_backwards1(['POP', 'POP'|Code]);
peep_hole_backwards1(['POP', _, 'PUSH1'|Code]) ->
    peep_hole_backwards1(Code);
peep_hole_backwards1([I|Code]) ->
    [I|peep_hole_backwards1(Code)];
peep_hole_backwards1([]) ->
    [].

%% Jump optimization:
%%   Replaces a jump to a jump with a jump to the final destination
%%   Moves basic blocks to eliminate an unconditional jump to them.

%% The compilation of conditionals generates a lot of labels and
%% jumps, some of them unnecessary. This optimization phase reorders
%% code so that as many jumps as possible can be eliminated, and
%% replaced by just falling through to the destination label. This
%% both optimizes the code generated by conditionals, and converts one
%% call of a function into falling through into its code--so it
%% reorders code quite aggressively. Function returns are indirect
%% jumps, however, and are never optimized away.

%% IMPORTANT: since execution begins at address zero, then the first
%% block of code must never be moved elsewhere. The code below has
%% this property, because it processes blocks from left to right, and
%% because the first block does not begin with a label, and so can
%% never be jumped to--hence no code can be inserted before it.

%% The optimization works by taking one block of code at a time, and
%% then prepending blocks that jump directly to it, and appending
%% blocks that it jumps directly to, resulting in a jump-free sequence
%% that is as long as possible. To do so, we store blocks in the form
%% {OptionalLabel, Body, OptionalJump} which represents the code block
%% OptionalLabel++Body++OptionalJump; the optional parts are the empty
%% list of instructions if not present.  Two blocks can be merged if
%% the first ends in an OptionalJump to the OptionalLabel beginning
%% the second; the OptionalJump can then be removed (and the
%% OptionalLabel if there are no other references to it--this happens
%% during dead code elimination.

%% TODO: the present implementation is QUADRATIC, because we search
%% repeatedly for matching blocks to merge with the first one, storing
%% the blocks in a list. A near linear time implementation could use
%% two ets tables, one keyed on the labels, and the other keyed on the
%% final jumps.

optimize_jumps(Code) ->
    JJs = jumps_to_jumps(Code),
    ShortCircuited = [short_circuit_jumps(JJs, Instr) || Instr <- Code],
    NoDeadCode = eliminate_dead_code(ShortCircuited),
    MovedCode = merge_blocks(moveable_blocks(NoDeadCode)),
    %% Moving code may have made some labels superfluous.
    eliminate_dead_code(MovedCode).


jumps_to_jumps([{'JUMPDEST', Label}, {push_label, Target}, 'JUMP'|More]) ->
    [{Label, Target}|jumps_to_jumps(More)];
jumps_to_jumps([{'JUMPDEST', Label}, {'JUMPDEST', Target}|More]) ->
    [{Label, Target}|jumps_to_jumps([{'JUMPDEST', Target}|More])];
jumps_to_jumps([_|More]) ->
    jumps_to_jumps(More);
jumps_to_jumps([]) ->
    [].

short_circuit_jumps(JJs, {push_label, Lab}) ->
    case proplists:get_value(Lab, JJs) of
        undefined ->
            {push_label, Lab};
        Target ->
            %% I wonder if this will ever loop infinitely?
            short_circuit_jumps(JJs, {push_label, Target})
    end;
short_circuit_jumps(_JJs, Instr) ->
    Instr.

eliminate_dead_code(Code) ->
    Jumps = lists:usort([Lab || {push_label, Lab} <- Code]),
    NewCode = live_code(Jumps, Code),
    if Code==NewCode ->
            Code;
       true ->
            eliminate_dead_code(NewCode)
    end.

live_code(Jumps, ['JUMP'|More]) ->
    ['JUMP'|dead_code(Jumps, More)];
live_code(Jumps, ['STOP'|More]) ->
    ['STOP'|dead_code(Jumps, More)];
live_code(Jumps, [{'JUMPDEST', Lab}|More]) ->
    case lists:member(Lab, Jumps) of
        true ->
            [{'JUMPDEST', Lab}|live_code(Jumps, More)];
        false ->
            live_code(Jumps, More)
    end;
live_code(Jumps, [I|More]) ->
    [I|live_code(Jumps, More)];
live_code(_, []) ->
    [].

dead_code(Jumps, [{'JUMPDEST', Lab}|More]) ->
    case lists:member(Lab, Jumps) of
        true ->
            [{'JUMPDEST', Lab}|live_code(Jumps, More)];
        false ->
            dead_code(Jumps, More)
    end;
dead_code(Jumps, [_I|More]) ->
    dead_code(Jumps, More);
dead_code(_, []) ->
    [].

%% Split the code into "moveable blocks" that control flow only
%% reaches via jumps.
moveable_blocks([]) ->
    [];
moveable_blocks([I]) ->
    [[I]];
moveable_blocks([Jump|More]) when Jump=='JUMP'; Jump=='STOP' ->
    [[Jump]|moveable_blocks(More)];
moveable_blocks([I|More]) ->
    [Block|MoreBlocks] = moveable_blocks(More),
    [[I|Block]|MoreBlocks].

%% Merge blocks to eliminate jumps where possible.
merge_blocks(Blocks) ->
    BlocksAndTargets = [label_and_jump(B) || B <- Blocks],
    [I || {Pref, Body, Suff} <- merge_after(BlocksAndTargets),
          I <- Pref++Body++Suff].

%% Merge the first block with other blocks that come after it
merge_after(All=[{Label, Body, [{push_label, Target}, 'JUMP']}|BlocksAndTargets]) ->
    case [{B, J} || {[{'JUMPDEST', L}], B, J} <- BlocksAndTargets,
                   L == Target] of
        [{B, J}|_] ->
            merge_after([{Label, Body ++ [{'JUMPDEST', Target}] ++ B, J}|
                         lists:delete({[{'JUMPDEST', Target}], B, J},
                                      BlocksAndTargets)]);
        [] ->
            merge_before(All)
    end;
merge_after(All) ->
    merge_before(All).

%% The first block cannot be merged with any blocks that it jumps
%% to... but maybe it can be merged with a block that jumps to it!
merge_before([Block={[{'JUMPDEST', Label}], Body, Jump}|BlocksAndTargets]) ->
    case [{L, B, T} || {L, B, [{push_label, T}, 'JUMP']} <- BlocksAndTargets,
                     T == Label] of
        [{L, B, T}|_] ->
            merge_before([{L, B ++ [{'JUMPDEST', Label}] ++ Body, Jump}
                          |lists:delete({L, B, [{push_label, T}, 'JUMP']}, BlocksAndTargets)]);
        _ ->
            [Block | merge_after(BlocksAndTargets)]
    end;
merge_before([Block|BlocksAndTargets]) ->
    [Block | merge_after(BlocksAndTargets)];
merge_before([]) ->
    [].

%% Convert each block to a PREFIX, which is a label or empty, a
%% middle, and a SUFFIX which is a JUMP to a label, or empty.
label_and_jump(B) ->
    {Label, B1} = case B of
                     [{'JUMPDEST', L}|More1] ->
                         {[{'JUMPDEST', L}], More1};
                     _ ->
                         {[], B}
                 end,
    {Target, B2} = case lists:reverse(B1) of
                      ['JUMP', {push_label, T}|More2] ->
                          {[{push_label, T}, 'JUMP'], lists:reverse(More2)};
                      _ ->
                          {[], B1}
                  end,
    {Label, B2, Target}.
