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
        , do_trace/1
        , extcode/2
        , extcode/4
        , extcodesize/2
        , gas/1
        , gaslimit/1
        , gasprice/1
        , get_contract_call_input/3
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
        , return_contract_call_result/6
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

-spec init(map(), map()) -> state().
init(#{ env  := Env
      , exec := Exec
      , pre  := Pre} = Spec, Opts) ->
    Address = maps:get(address, Exec),
    BlockHashFun = get_blockhash_fun(Opts, Env),
    NoRecursion = maps:get(no_recursion, Opts, false),

    ChainState = maps:get(chainState, Env),
    ChainAPI =  maps:get(chainAPI, Env),

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

         , environment =>
               #{ spec => Spec
                , options => Opts }
         },

    init_vm(State,
            maps:get(code, Exec),
            maps:get(mem, Exec, #{mem_size => 0}),
            ChainAPI:get_store(ChainState)).


init_vm(State, Code, Mem, Store) ->
    State1 =
        State#{ out       => <<>>
              , call      => #{}
              , callcreates => []
              , code      => Code
              , cp        => 0
              , logs      => []
              , memory    => Mem
              , storage   => #{}
              , return_data => <<>>
              , stack     => []
              },
    %% Solidity contracts want their state in the 'store', and Sophia contracts
    %% want it on the heap.
    case vm_version(State) of
        ?AEVM_01_Solidity_01 ->
            aevm_eeevm_store:init(Store, State1);
        ?AEVM_01_Sophia_01 ->
            %% We need to import the state first, since the map ids in the store are fixed.
            State2 = import_state_from_store(Store, State1),
            %% Next we write the calldata on top of the heap and put a pointer
            %% to it on the stack.
            Calldata = data(State1),
            %% Calldata can contain maps, so we can't simply write it
            %% to memory. The calldata should be a pair of a typerep
            %% and the actual calldata.
            HeapSize = aevm_eeevm_memory:size_in_words(State2) * 32,
            case aeso_data:from_binary({tuple, [typerep]}, Calldata) of
                {ok, {Type}} ->
                    {ok, CalldataHeap} = aeso_data:binary_to_heap({tuple, [typerep, Type]}, Calldata,
                                                                  aevm_eeevm_maps:next_id(maps(State2)), HeapSize),
                    {Ptr, State3} = write_heap_value(CalldataHeap, State2),
                    aevm_eeevm_stack:push(Ptr, State3);
                {error, Err} ->
                    io:format("** Error invalid calldata: ~p\n", [Err]),
                    set_gas(0, State2)
            end
    end.

%% TODO: Currently the Store is saved both in the chain state and the VM state.
%%       Refactor?
get_store(#{chain_api := ChainAPI, chain_state := ChainState}) ->
    ChainAPI:get_store(ChainState).

import_state_from_store(Store, State) ->
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
        ?AEVM_01_Sophia_01 ->
            try
                %% In Sophia Us0 is a pointer to a typerep for the return value, and
                %% Us1 is a pointer to the actual value.
                Heap       = mem(State),
                {ok, Type} = aeso_data:from_heap(typerep, Heap, Us0),
                {ok, Out}  = aeso_data:heap_to_binary(Type, get_store(State), aeso_data:heap_value(maps(State), Us1, Heap)),
                set_out(Out, State)
            catch _:_ ->
                io:format("** Error reading return value\n~s", [format_mem(mem(State))]),
                set_gas(0, State)   %% Consume all gas on failure
            end;
        ?AEVM_01_Solidity_01 ->
            %% Us0 is pointer to a return data binary and Us1 is the size.
    {Out, State1} = aevm_eeevm_memory:get_area(Us0, Us1, State),
            set_out(Out, State1)
    end.

heap_to_binary(Type, Ptr, State) ->
    Store = get_store(State),
    Heap  = mem(State),
    Maps  = maps(State),
    Value = aeso_data:heap_value(Maps, Ptr, Heap),
    aeso_data:heap_to_binary(Type, Store, Value).

heap_to_heap(Type, Ptr, State) ->
    Heap  = mem(State),
    Maps  = maps(State),
    Value = aeso_data:heap_value(Maps, Ptr, Heap),
    {ok, NewValue} = aeso_data:heap_to_heap(Type, Value, 32),
    NewPtr = aeso_data:heap_value_pointer(NewValue),
    NewBin = aeso_data:heap_value_heap(NewValue),
    {ok, <<NewPtr:256, NewBin/binary>>}.

return_contract_call_result(To, Input, Addr, Size, ReturnData, State) ->
    case aevm_eeevm_state:vm_version(State) of
        ?AEVM_01_Solidity_01 ->
            {1, aevm_eeevm_memory:write_area(Addr, ReturnData, State)};
        ?AEVM_01_Sophia_01 ->
            try
                %% For Sophia, we use the Size argument to store the typerep of
                %% the result. We also ignore the Addr and put the result on
                %% top of the heap.
                TypePtr    = Size,
                HeapSize   = aevm_eeevm_memory:size_in_words(State) * 32,
                Heap       = mem(State),
                {ok, Type} = aeso_data:from_heap(typerep, Heap, TypePtr),
                OutValue =
                    case is_local_primop(To, Input) of
                        true ->
                            %% Local primops (like map primops) return heap values
                            <<Ptr:256, Bin/binary>> = ReturnData,
                            HeapVal = aeso_data:heap_value(maps(State), Ptr, Bin, 32),
                            {ok, Out} = aeso_data:heap_to_heap(Type, HeapVal, HeapSize),
                            Out;
                        false ->
                            {ok, Out} = aeso_data:binary_to_heap(Type, ReturnData, aevm_eeevm_maps:next_id(maps(State)), HeapSize),
                            Out
                    end,
                write_heap_value(OutValue, State)
            catch _:Err ->
                io:format("** Failed to decode contract return value\n~P\n~p\n", [erlang:get_stacktrace(), 20, Err]),
                {0, set_gas(0, State)}
            end
    end.

is_local_primop(?PRIM_CALLS_CONTRACT, Calldata) ->
    aevm_ae_primops:is_local_primop(Calldata);
is_local_primop(_, _) -> false.

-spec save_store(state()) -> state().
save_store(#{ chain_state := ChainState
            , chain_api   := ChainAPI
            , vm_version  := VmVersion } = State) ->
    case VmVersion of
        ?AEVM_01_Solidity_01 ->
            Store = aevm_eeevm_store:to_binary(State),
            State#{ chain_state => ChainAPI:set_store(Store, ChainState)};
        ?AEVM_01_Sophia_01 ->
            %% A typerep for the state type is on top of the stack, and the state
            %% pointer is at address 0.
            try
                {Addr, _} = aevm_eeevm_memory:load(0, State),
                case Addr of        %% A contract can write 0 to the state pointer
                    0 -> State;     %% to indicate that the state didn't change.
                    _ ->
                        {TypePtr, _} = aevm_eeevm_stack:pop(State),
                        Heap         = mem(State),
                        {ok, Type}   = aeso_data:from_heap(typerep, Heap, TypePtr),
                        {Ptr, _}     = aevm_eeevm_memory:load(Addr, State),
                        Store        = get_store(State),
                        StateValue   = aeso_data:heap_value(maps(State), Ptr, Heap),
                        {ok, StateValue1} = aeso_data:heap_to_heap(Type, StateValue, 32),
                        Store1       = aevm_eeevm_store:set_sophia_state(StateValue1, Store),
                        State#{ chain_state => ChainAPI:set_store(Store1, ChainState) }
                end
            catch _:_ ->
                io:format("** Error reading updated state\n~s\n~p\n", [format_mem(mem(State)), erlang:get_stacktrace()]),
                State
            end
    end.

get_contract_call_input(IOffset, ISize, State) ->
    case vm_version(State) of
        ?AEVM_01_Solidity_01 ->
            aevm_eeevm_memory:get_area(IOffset, ISize, State);
        ?AEVM_01_Sophia_01 ->
            %% In Sophia the ISize is the type of the arguments and IOffset is
            %% a pointer into the heap.
            TypePtr = ISize,
            Ptr     = IOffset,
            Heap       = mem(State),
            {ok, Type} = aeso_data:from_heap(typerep, Heap, TypePtr),
            %% TODO: This is a bit awkward since we need to pass the argument
            %%       typerep in the calldata. Will be much better when types
            %%       are in contract metadata!
            %% We put a temporary pair of the type and the value on the heap
            %% and call heap_to_binary on that.
            PairPtr   = 32 * aevm_eeevm_memory:size_in_words(State),
            TmpState  = aevm_eeevm_memory:write_area(PairPtr, <<TypePtr:256, Ptr:256>>, State),
            TmpHeap   = mem(TmpState),
            {ok, Arg} = aeso_data:heap_to_binary({tuple, [typerep, Type]}, get_store(TmpState),
                                                 aeso_data:heap_value(maps(TmpState), PairPtr, TmpHeap)),
            {Arg, State}
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
        ?AEVM_01_Sophia_01 when Target == ?PRIM_CALLS_CONTRACT ->
            Res = aevm_ae_primops:call(Value, Data, State),
            Res;
        _ ->
            CallStack  = [Caller | call_stack(State)],
            TargetKey  = <<Target:256>>,
            ChainAPI   = chain_api(State),
            ChainState = chain_state(State),
            try ChainAPI:call_contract(TargetKey, CallGas, Value, Data, CallStack, ChainState) of
                {ok, Res, ChainState1} ->
                    GasSpent = aevm_chain_api:gas_spent(Res),
                    Return   = aevm_chain_api:return_value(Res),
                    {ok, Return, GasSpent, set_chain_state(ChainState1, State)};
                {error, Err} -> {error, Err}
            catch K:Err ->
                lager:error("~w:call_contract(~w, ~w, ~w, ~w, ~w, _) crashed with ~w:~w",
                            [ChainAPI, TargetKey, CallGas, Value, Data, CallStack, K, Err]),
                {error, Err}
            end
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
return_data(State) -> maps:get(return_data, State).
gas(State)         -> maps:get(gas, State).
gaslimit(State)    -> maps:get(gas_limit, State).
gasprice(State)    -> maps:get(gas_price, State).
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

