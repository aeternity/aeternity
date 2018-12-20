%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle the machine state for the EEEVM.
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_eeevm_state).
-export([ accountbalance/2
        , add_log/2
        , add_trace/2
        , add_callcreates/2
        , address/1
        , blockhash/2
        , bloom/2
        , calldepth/1
        , call_stack/1
        , call_contract/6
        , caller/1
        , callcreates/1
        , chain_state/1
        , chain_api/1
        , code/1
        , coinbase/1
        , cp/1
        , data/1
        , difficulty/1
        , do_return/3
        , do_revert/3
        , do_trace/1
        , extcode/2
        , extcode/4
        , extcodesize/2
        , gas/1
        , gaslimit/1
        , gasprice/1
        , gastable/1
        , get_contract_call_input/4
        , heap_to_binary/3
        , heap_to_heap/3
        , init/2
        , jumpdests/1
        , logs/1
        , origin/1
        , out/1
        , maps/1
        , mem/1
        , no_recursion/1
        , number/1
        , return_contract_call_result/7
        , return_data/1
        , save_store/1
        , set_code/2
        , set_cp/2
        , set_gas/2
        , set_jumpdests/2
        , set_maps/2
        , set_mem/2
        , set_out/2
        , set_selfdestruct/2
        , set_stack/2
        , set_storage/2
        , set_chain_state/2
        , stack/1
        , storage/1
        , timestamp/1
        , trace/1
        , trace_format/3
        , trace_fun/1
        , value/1
        , vm_version/1
        ]).

-include_lib("aecontract/src/aecontract.hrl").
-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aevm_eeevm.hrl").

-type state() :: map().

-export_type([state/0]).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-define(DEBUG_LOG(Format, Data), begin lager:debug(Format, Data), ?TEST_LOG(Format, Data) end).
-else.
-define(TEST_LOG(Format, Data), ok).
-define(DEBUG_LOG(Format, Data), lager:debug(Format, Data)).
-endif.

-spec init(map(), map()) -> state().
init(#{ env  := Env
      , exec := Exec
      , pre  := Pre} = _Spec, Opts) ->
    Address = maps:get(address, Exec),
    BlockHashFun = get_blockhash_fun(Opts, Env),
    NoRecursion = maps:get(no_recursion, Opts, false),
    GasTable = get_gas_table(Opts),

    ChainState = maps:get(chainState, Env),
    ChainAPI =  maps:get(chainAPI, Env),
    OutType = maps:get(out_type, Exec, undefined),

    State =
        #{ address     => Address
         , caller      => maps:get(caller, Exec)
         , return_data => maps:get(return_data, Exec, <<>>)
         , data        => maps:get(data, Exec)
         , gas         => maps:get(gas, Exec)
         , gas_price   => maps:get(gasPrice, Exec)
         , origin      => maps:get(origin, Exec)
         , value       => maps:get(value, Exec)
         , call_stack  => maps:get(call_stack, Exec, [])

         , coinbase   => maps:get(currentCoinbase, Env)
         , difficulty => maps:get(currentDifficulty, Env)
         , gas_limit  => maps:get(currentGasLimit, Env)
         , number     => maps:get(currentNumber, Env)
         , timestamp  => maps:get(currentTimestamp, Env)

         , ext_code_blocks => get_ext_code_blocks(Pre)
         , ext_code_sizes  => get_ext_code_sizes(Pre)
         , block_hash_fun  => BlockHashFun
         , no_recursion => NoRecursion

         , do_trace  => maps:get(trace, Opts, false)
         , trace => []
         , trace_fun => init_trace_fun(Opts)

         , vm_version => maps:get(vm_version, Env)

         , maps => aevm_eeevm_maps:init_maps()

         , chain_state => ChainState
         , chain_api   => ChainAPI

         , out_type => OutType

         , gas_table => GasTable
         },

    init_vm(State,
            maps:get(code, Exec),
            maps:get(mem, Exec, #{mem_size => 0}),
            maps:get(store, Exec),
            maps:get(call_data_type, Exec, undefined),
            OutType
           ).


init_vm(State, Code, Mem, Store, CallDataType, OutType) ->
    State1 =
        State#{ out       => <<>>
              , call      => #{}
              , callcreates => []
              , code      => Code
              , cp        => 0
              , logs      => []
              , memory    => Mem
              , return_data => <<>>
              , stack     => []
              },
    %% Solidity contracts want their state in the 'store', and Sophia contracts
    %% want it on the heap.
    case vm_version(State) of
        ?AEVM_01_Solidity_01 ->
            aevm_eeevm_store:init(Store, State1);
        VmVersion when (CallDataType =:= undefined orelse OutType =:= undefined)
                       andalso ?IS_AEVM_SOPHIA(VmVersion) ->
            error({bad_vm_setup, missing_call_data_type});
        VmVersion when ?IS_AEVM_SOPHIA(VmVersion) ->
            case is_reentrant_call(State) of
                true -> %% Sophia doesn't allow reentrant calls
                    init_error(reentrant_call);
                false ->
                    %% We need to import the state first, since the map ids in the store are fixed.
                    State2 = import_state_from_store(Store, State1),
                    %% Next we write the calldata on top of the heap and put a pointer
                    %% to it on the stack.
                    Calldata = data(State1),
                    %% Calldata can contain maps, so we can't simply write it
                    %% to memory. The first element of the calldata tuple is
                    %% the function name
                    HeapSize = aevm_eeevm_memory:size_in_words(State2) * 32,
                    case aeso_data:binary_to_heap(CallDataType, Calldata,
                                                  aevm_eeevm_maps:next_id(maps(State2)),
                                                  HeapSize) of
                        {ok, CalldataHeap} ->
                            {Ptr, State3} = write_heap_value(CalldataHeap, State2),
                            aevm_eeevm_stack:push(Ptr, State3);
                        {error, Err} ->
                            lager:debug("Invalid calldata: ~p\n", [Err]),
                            init_error(bad_call_data)
                    end
            end
    end.

-spec init_error(atom()) -> no_return().
init_error(What) ->
    throw({init_error, What}).

is_reentrant_call(State) ->
    lists:member(address(State), call_stack(State)).

import_state_from_store(Store, State0) ->
    State = State0#{ storage => Store },
    case aevm_eeevm_store:get_sophia_state_type(Store) of
        false ->
            %% No state yet (init function). Write 0 to the state pointer.
            aevm_eeevm_memory:store(0, 0, State);
        _StateType ->
            %% The state value in the store already has the correct offset (32),
            %% so no need to translate it.
            StateValue = aevm_eeevm_store:get_sophia_state(Store),
            32 = aeso_data:heap_value_offset(StateValue),
            {StatePtr, State1} = write_heap_value(StateValue, State),
            aevm_eeevm_memory:store(0, StatePtr, State1)
    end.

do_return(Us0, Us1, State) ->
    case vm_version(State) of
        VMV when ?IS_AEVM_SOPHIA(VMV) ->
            %% In Sophia Us1 is a pointer to the actual value.
            %% The type of the value is in the state (from meta data)
            Type = out_type(State),
            case heap_to_binary(Type, Us1, State) of
                {ok, Out, GasUsed} ->
                    {set_out(Out, State), GasUsed};
                {error, _} ->
                    {State, gas(State) + 1}
            end;
        ?AEVM_01_Solidity_01 ->
            %% Us0 is pointer to a return data binary and Us1 is the size.
            {Out, State1} = aevm_eeevm_memory:get_area(Us0, Us1, State),
            {set_out(Out, State1), 0}
    end.

do_revert(Us0, Us1, State0) ->
    %% Us0 is a pointer to the revert string binary and Us1 is its size.
    case vm_version(State0) of
        VMV when ?IS_AEVM_SOPHIA(VMV) ->
            %% In Sophia Us1 is a pointer to the actual value.
            %% The type of the value is always string.
            case heap_to_binary(string, Us1, State0) of
                {ok, Out, GasUsed} ->
                    set_out(Out, spend_gas(GasUsed, State0));
                {error, _} = Err ->
                    lager:error("Error reading revert value: ~p\n~s",
                                 [Err, format_mem(mem(State0))]),
                    aevm_eeevm:eval_error(out_of_gas)
            end;
        ?AEVM_01_Solidity_01 ->
            {Out, State1} = aevm_eeevm_memory:get_area(Us0, Us1, State0),
            set_out(Out, State1)
    end.

heap_to_binary(Type, Ptr, State) ->
    Store = storage(State),
    Heap  = mem(State),
    Maps  = maps(State),
    Value = aeso_data:heap_value(Maps, Ptr, Heap),
    MaxWords = aevm_gas:mem_limit_for_gas(gas(State), State),
    case aeso_data:heap_to_binary(Type, Store, Value, MaxWords * 32) of
        {ok, Bin} ->
            GasUsed = aevm_gas:mem_gas(byte_size(Bin) div 32, State),
            {ok, Bin, GasUsed};
        {error, _} = Err ->
            Err
    end.

spend_gas(Gas, State) ->
    TotalGas = gas(State),
    case TotalGas - Gas of
        GasLeft when GasLeft >=  0 -> set_gas(GasLeft, State);
        GasLeft when GasLeft < 0 -> aevm_eeevm:eval_error(out_of_gas)
    end.

heap_to_heap(Type, Ptr, State) ->
    Heap  = mem(State),
    Maps  = maps(State),
    Value = aeso_data:heap_value(Maps, Ptr, Heap),
    MaxWords = aevm_gas:mem_limit_for_gas(gas(State), State),
    case aeso_data:heap_to_heap(Type, Value, 32, MaxWords) of
        {ok, NewValue} ->
            GasUsed = aevm_gas:mem_gas(byte_size(aeso_data:heap_value_heap(NewValue)) div 32, State),
            {ok, NewValue, GasUsed};
        {error, _} = Err ->
            Err
    end.

return_contract_call_result(To, Input, Addr,_Size, ReturnData, Type, State) ->
    case aevm_eeevm_state:vm_version(State) of
        ?AEVM_01_Solidity_01 ->
            {1, aevm_eeevm_memory:write_area(Addr, ReturnData, State)};
        VMV when ?IS_AEVM_SOPHIA(VMV) ->
            %% For Sophia, ignore the Addr and put the result on the
            %% top of the heap
            HeapSize = aevm_eeevm_memory:size_in_words(State) * 32,
            case is_local_primop(To, Input) of
                true ->
                    %% Local primops (like map primops) return heap values
                    <<Ptr:256, Bin/binary>> = ReturnData,
                    HeapVal = aeso_data:heap_value(maps(State), Ptr, Bin, 32),
                    case aeso_data:heap_to_heap(Type, HeapVal, HeapSize) of
                        {ok, Out} ->
                            write_heap_value(Out, State);
                        {error, _} = Err ->
                            lager:error("Failed to decode primop return value\n"
                                        "~p\n", [Err]),
                            aevm_eeevm:eval_error(out_of_gas)
                    end;
                false ->
                    NextId = aevm_eeevm_maps:next_id(maps(State)),
                    case aeso_data:binary_to_heap(Type, ReturnData, NextId, HeapSize) of
                        {ok, Out} ->
                            write_heap_value(Out, State);
                        {error, _} = Err ->
                            lager:error("Failed to decode contract return value\n"
                                        "~p\n", [Err]),
                            aevm_eeevm:eval_error(out_of_gas)
                    end
            end
    end.

is_local_primop(?PRIM_CALLS_CONTRACT, Calldata) ->
    aevm_ae_primops:is_local_primop(Calldata);
is_local_primop(_, _) -> false.

-spec save_store(state()) -> {ok, state()} | {error, term()}.
save_store(#{ chain_state := ChainState
            , chain_api   := ChainAPI
            , vm_version  := VmVersion } = State) ->
    case VmVersion of
        ?AEVM_01_Solidity_01 ->
            Store = aevm_eeevm_store:to_binary(State),
            {ok, State#{ chain_state => ChainAPI:set_store(Store, ChainState)}};
        VMV when ?IS_AEVM_SOPHIA(VMV) ->
            %% A typerep for the state type is on top of the stack, and the state
            %% pointer is at address 0.
            {Addr, _} = aevm_eeevm_memory:load(0, State),
            case Addr of            %% A contract can write 0 to the state pointer
                0 -> {ok, State};   %% to indicate that the state didn't change.
                _ ->
                    Type     = aevm_eeevm_store:get_sophia_state_type(storage(State)),
                    {Ptr, _} = aevm_eeevm_memory:load(Addr, State),
                    case heap_to_heap(Type, Ptr, State) of
                        {ok, StateValue1, GasUsed} ->
                            Store = aevm_eeevm_store:set_sophia_state(StateValue1, storage(State)),
                            {ok, spend_gas(GasUsed, State#{ chain_state => ChainAPI:set_store(Store, ChainState) })};
                        {error, _} ->
                            {error, out_of_gas}
                    end
            end
    end.

get_contract_call_input(Target, IOffset, ISize, State) ->
    case vm_version(State) of
        ?AEVM_01_Solidity_01 ->
            {Arg, State1} = aevm_eeevm_memory:get_area(IOffset, ISize, State),
            {Arg, undefined, State1};
        VMVersion when ?IS_AEVM_SOPHIA(VMVersion) ->
            %% In Sophia:
            %%   ISize is the (integer) type hash for primops that needs to be
            %%         type checked (otherwise 0).
            %%   IOffset is a pointer into the heap to the arguments
            ArgPtr     = IOffset,
            Heap       = mem(State),
            TargetKey  = <<Target:256>>,
            ChainAPI   = chain_api(State),
            ChainState = chain_state(State),
            Store      = storage(State),
            HeapValue = aeso_data:heap_value(maps(State), ArgPtr, Heap),
            case Target == ?PRIM_CALLS_CONTRACT of
                true ->
                    %% The first argument is the primop id
                    {ok, Bin} = aeso_data:heap_to_binary({tuple, [word]}, Store, HeapValue),
                    {ok, {Prim}} = aeso_data:from_binary({tuple, [word]}, Bin),
                    {ArgTypes, OutType} = aevm_ae_primops:types(Prim, HeapValue, Store, State),
                    DataType = {tuple, [word|ArgTypes]},
                    TypeHash   = <<ISize:256>>,
                    case aevm_ae_primops:check_type_hash(Prim, ArgTypes, OutType, TypeHash) of
                        ok ->
                            {ok, Arg, GasUsed} = heap_to_binary(DataType, ArgPtr, State),
                            {Arg, OutType, spend_gas(GasUsed, State)};
                        error ->
                            aevm_eeevm:eval_error(out_of_gas)
                    end;
                false ->
                    %% The first element in the arg tuple is the function hash
                    {ok, Bin} = aeso_data:heap_to_binary({tuple, [word]}, Store, HeapValue),
                    {ok, {TypeHashInt}} = aeso_data:from_binary({tuple, [word]}, Bin),
                    TypeHash = <<TypeHashInt:256>>,
                    case ChainAPI:get_contract_fun_types(TargetKey, VMVersion,
                                                         TypeHash, ChainState) of
                        {ok, ArgType, OutType} ->
                            DataType = {tuple, [word, ArgType]},
                            {ok, Arg, GasUsed} = heap_to_binary(DataType, ArgPtr, State),
                            {Arg, OutType, spend_gas(GasUsed, State)};
                        {error, _Err} ->
                            aevm_eeevm:eval_error(out_of_gas)
                    end
            end
    end.

-spec write_heap_value(aeso_data:heap_value(), state()) -> {non_neg_integer(), state()}.
write_heap_value(HeapValue, State) ->
    Ptr    = aeso_data:heap_value_pointer(HeapValue),
    Mem    = aeso_data:heap_value_heap(HeapValue),
    Maps   = aeso_data:heap_value_maps(HeapValue),
    Offs   = aeso_data:heap_value_offset(HeapValue),
    State1 = aevm_eeevm_memory:write_area(Offs, Mem, State),
    State2 = aevm_eeevm_maps:merge(Maps, State1),
    {Ptr, State2}.

call_contract(Caller, Target, CallGas, Value, Data, State) ->
    case vm_version(State) of
        VMV when ?IS_AEVM_SOPHIA(VMV), Target == ?PRIM_CALLS_CONTRACT ->
            aevm_ae_primops:call(CallGas, Value, Data, State);
        _ ->
            CallStack  = [Caller | call_stack(State)],
            TargetKey  = <<Target:256>>,
            ChainAPI   = chain_api(State),
            ChainState = chain_state(State),
            {Res, ChainState1} =
                ChainAPI:call_contract(TargetKey, CallGas, Value, Data,
                                       CallStack, ChainState),
            GasSpent = aevm_chain_api:gas_spent(Res),
            {Tag, Return} = aevm_chain_api:return_value(Res),
            {Tag, Return, GasSpent, set_chain_state(ChainState1, State)}
    end.


get_ext_code_sizes(#{} = Pre) ->
    maps:from_list(
      [{Address, byte_size(C)} || {Address, #{code := C}} <-maps:to_list(Pre)]).

get_ext_code_blocks(#{} = Pre) ->
    maps:from_list(
      [{Address, C} || {Address, #{code := C}} <-maps:to_list(Pre)]).

get_blockhash_fun(Opts, Env) ->
    GenesisHeight = aec_block_genesis:height(),
    case maps:get(blockhash, Opts, default) of
        default ->
            fun(N, State) ->
                    CurrentNumber = maps:get(currentNumber, Env),
                    if
                        N < GenesisHeight -> 0;
                        N >= CurrentNumber -> 0;
                        CurrentNumber - 256 > N -> 0;
                        true ->
                            ChainState = chain_state(State),
                            ChainAPI = chain_api(State),
                            Hash = ChainAPI:blockhash(N, ChainState),
                            <<Val:256/integer-unsigned>> = Hash,
                            Val
                    end
            end;
        sha3 ->
            fun(N,_A) ->
                    %% Because the data of the blockchain is not
                    %% given, the opcode BLOCKHASH could not
                    %% return the hashes of the corresponding
                    %% blocks. Therefore we define the hash of
                    %% block number n to be SHA3-256("n").
                    CurrentNumber = maps:get(currentNumber, Env),
                    if
                        N < GenesisHeight -> 0;
                        N >= CurrentNumber -> 0;
                        CurrentNumber - 256 > N -> 0;
                        true ->
                            BinN = integer_to_binary(N),
                            Hash = aec_hash:hash(evm, BinN),
                            <<Val:256/integer-unsigned>> = Hash,
                            Val
                    end
            end
    end.

get_gas_table(Opts) ->
    case maps:get(gas_table, Opts, default) of
        default ->
            aec_governance:vm_gas_table();
        GasTable when is_map(GasTable) ->
            GasTable
    end.


init_trace_fun(Opts) ->
    maps:get(trace_fun, Opts, fun(S,A) -> lager:debug(S,A) end).


accountbalance(Address, State) ->
    Chain = chain_api(State),
    ChainState = chain_state(State),
    PubKey = <<Address:256>>,
    Chain:get_balance(PubKey, ChainState).
address(State)     -> maps:get(address, State).
blockhash(N,State) -> (maps:get(block_hash_fun, State))(N,State).
calldepth(State) -> length(call_stack(State)).
call_stack(State) -> maps:get(call_stack, State).
caller(State)    -> maps:get(caller, State).
callcreates(State) -> maps:get(callcreates, State).
code(State)      -> maps:get(code, State).
coinbase(State)  -> maps:get(coinbase, State).
cp(State)        -> maps:get(cp, State).
data(State)      -> maps:get(data, State).
difficulty(State)-> maps:get(difficulty, State).
extcodesize(Adr, State) ->
    maps:get(Adr band ?MASK160, maps:get(ext_code_sizes, State), 0).
extcode(Account, Start, Length, State) ->
    CodeBlock = maps:get(Account band ?MASK160,
                         maps:get(ext_code_blocks, State), <<>>),
    aevm_eeevm_utils:bin_copy(Start, Length, CodeBlock).
extcode(Account, State) ->
    maps:get(Account band ?MASK160,
             maps:get(ext_code_blocks, State), <<>>).
no_recursion(State) ->
    maps:get(no_recursion, State).


jumpdests(State)   -> maps:get(jumpdests, State).
stack(State)       -> maps:get(stack, State).
mem(State)         -> maps:get(memory, State).
number(State)      -> maps:get(number, State).
origin(State)      -> maps:get(origin, State).
out(State)         -> maps:get(out, State).
out_type(State)    -> maps:get(out_type, State).
return_data(State) -> maps:get(return_data, State).
gas(State)         -> maps:get(gas, State).
gaslimit(State)    -> maps:get(gas_limit, State).
gasprice(State)    -> maps:get(gas_price, State).
gastable(State)    -> maps:get(gas_table, State).
logs(State)        -> maps:get(logs, State).
storage(State)     -> maps:get(storage, State).
value(State)       -> maps:get(value, State).
timestamp(State)   -> maps:get(timestamp, State).

do_trace(State)    -> maps:get(do_trace, State).
trace(State)       -> maps:get(trace, State).
trace_fun(State)   -> maps:get(trace_fun, State).

maps(State)        -> maps:get(maps, State).

chain_state(State) -> maps:get(chain_state, State).
chain_api(State)   -> maps:get(chain_api, State).

vm_version(State)  -> maps:get(vm_version, State).

add_log(Entry, State)    ->
    Log = maps:get(logs, State),
    maps:put(logs, Log ++ [Entry], State).

set_cp(Value, State)      -> maps:put(cp, Value, State).
set_code(Value, State)    -> maps:put(code, Value, State).
set_stack(Value, State)   -> maps:put(stack, Value, State).
set_mem(Value, State)     -> maps:put(memory, Value, State).
set_out(Value, State)     -> maps:put(out, Value, State).
set_gas(Value, State)     -> maps:put(gas, Value, State).
set_storage(Value, State) -> maps:put(storage, Value, State).
set_jumpdests(Value, State)    -> maps:put(jumpdests, Value, State).
set_selfdestruct(Value, State) -> maps:put(selfdestruct, Value, State).
set_maps(Maps, State)         -> maps:put(maps, Maps, State).
set_chain_state(Value, State) -> maps:put(chain_state, Value, State).

add_callcreates(#{ data := _
                 , destination := _
                 , gasLimit := _
                 , value := _} = Callcreates, #{callcreates := Old} = State) ->
    State#{callcreates => [Callcreates|Old]}.

add_trace(T, State) ->
    Trace = trace(State),
    maps:put(trace, Trace ++ [T], State).

trace_format(String, Argument, State) ->
    Account   = aevm_eeevm_state:address(State) band 65535,
    CP   = aevm_eeevm_state:cp(State),
    Code = aevm_eeevm_state:code(State),
    OP   = aevm_eeevm:code_get_op(CP, Code),
    case do_trace(State) of
        true ->
            F = trace_fun(State),
            F("[~4.16.0B] ~8.16.0B : ~w",
              [Account, CP, aevm_opcodes:op_name(OP)]),
            F(" ~s", [format_stack(stack(State))]),
            F(" ~s", [format_mem(mem(State))]),
            F(" ~p", [gas(State)]),
            F(String, Argument),
            add_trace([{CP, OP, aevm_opcodes:op_name(OP)}], State);
        false ->
            State
    end.

-define(MAXMEMPOS,200).

format_mem(Mem) ->
   lists:flatten(
     "[" ++
         format_mem(lists:sort(maps:to_list(Mem)), ?MAXMEMPOS)
     ++ "]").
format_mem([],_) -> [];
format_mem( _,0) -> " ...";
format_mem([{N,V}|Rest], P) when is_integer(N) ->
    [ ", " || P < ?MAXMEMPOS ]
        ++ io_lib:format("~w:~p",[N,format_word(V)])
        ++ format_mem(Rest, P-1);
format_mem([_|Rest], P) -> format_mem(Rest, P).

format_stack(S) ->
    Words = [ format_word(N) || N <- S ],
    case lists:all(fun(W) -> is_integer(W) end, Words) of
        true -> io_lib:format("~w", [Words]);   %% Avoid accidental string printing
        false -> io_lib:format("~1000p", [Words])   %% Nice printing of recovered strings
    end.

%% Try to find strings and negative numbers
format_word(N) when <<N:256>> < <<1, 0:248>> -> N;
format_word(N) ->
    Bytes = binary_to_list(<<N:256>>),
    {S, Rest} = lists:splitwith(fun(X) -> X /= 0 end, Bytes),
    case lists:usort(Rest) of
        [0] -> S;
        _   ->
            <<X:256/signed>> = <<N:256>>,
            X
    end.


%% We define the Bloom filter function, M, to reduce a log
%% entry into a single 256-byte hash:
%% (23) M(O) ≡ V_(t∈{Oa}∪Ot) (M3:2048(t))
%% where M3:2048 is a specialised Bloom filter that sets
%% three bits out of 2048, given an arbitrary byte sequence.
%% It does this through taking the low-order 11 bits of each
%% of the first three pairs of bytes in a Keccak-256 hash of
%% the byte sequence. Formally:
%% (24) M3:2048(x : x ∈ B) ≡ y : y ∈ B256 where:
%% (25) y = (0, 0, ..., 0) except:
%% (26) ∀i∈{0,2,4} : Bm(x,i)(y) = 1
%% (27) m(x, i) ≡ KEC(x)[i, i + 1] mod 2048
%% where B is the bit reference function such that Bj (x)
%% equals the bit of index j (indexed from 0) in the byte array x.
bloom(State, Filter) ->
    Data = maps:get(logs, State),
    bloom_filter(Data, Filter).


bloom_filter(Data, Filter) ->
    Hash = aec_hash:hash(evm, Data),
    Bits = bloom_bits(Hash),
    Filter bor Bits.

bloom_bits(Hash) ->
    {Bit1, Hash1} = bloom_bit(Hash),
    {Bit2, Hash2} = bloom_bit(Hash1),
    {Bit3,     _} = bloom_bit(Hash2),

    (1 bsl Bit1) bor (1 bsl Bit2) bor (1 bsl Bit3).

bloom_bit(<<Bits:16/integer, Rest/bitstring>>) ->
    {Bits band 2047, Rest}.

