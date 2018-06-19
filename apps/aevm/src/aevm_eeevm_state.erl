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
        , add_trace/2
        , add_callcreates/2
        , address/1
        , blockhash/3
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
        , extcode/2
        , extcode/4
        , extcodesize/2
        , gas/1
        , gaslimit/1
        , gasprice/1
        , init/1
        , init/2
        , jumpdests/1
        , logs/1
        , origin/1
        , out/1
        , mem/1
        , no_recursion/1
        , number/1
        , return_data/1
	, save_store/1
        , set_code/2
        , set_cp/2
        , set_gas/2
        , set_jumpdests/2
        , set_logs/2
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

-spec init(map()) -> state().
init(Spec) -> init(Spec, #{}).

-spec save_store(state()) -> state().
save_store(#{ chain_state := ChainState
	    , chain_api   := ChainAPI
            , vm_version  := VmVersion } = State) ->
    case VmVersion of
        ?AEVM_01_Solidity_01 ->
            Store = aevm_eeevm_store:to_binary(State),
            State#{ chain_state => ChainAPI:set_store(Store, ChainState)};
        ?AEVM_01_Sophia_01 ->
            %% The serialized state is on top of the heap and the pointer to it
            %% at address 0.
            try
                {Addr, _} = aevm_eeevm_memory:load(0, State),
                case Addr of        %% A contract can write 0 to the state pointer
                    0 -> State;     %% to indicate that the state didn't change.
                    _ -> Size      = aevm_eeevm_memory:size_in_words(State) * 32 - Addr,
                         {Data, _} = aevm_eeevm_memory:get_area(Addr, Size, State),
                         Store     = aevm_eeevm_store:from_sophia_state(Data),
                         State#{ chain_state => ChainAPI:set_store(Store, ChainState) }
                end
            catch _:_ ->
                io:format("** Error reading updated state\n~s", [format_mem(mem(State))]),
                State
            end
    end.

-spec init(map(), map()) -> state().
init(#{ env  := Env
      , exec := Exec
      , pre  := Pre} = Spec, Opts) ->
    Address = maps:get(address, Exec),
    BlockHashFun = get_blockhash_fun(Opts, Env, Address),
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

         , chain_state => ChainState
         , chain_api   => ChainAPI

         , environment =>
               #{ spec => Spec
                , options => Opts }

         },

    init_vm(State,
            maps:get(code, Exec, #{}),
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
            %% Leave room for the calldata at address 32
            Addr = byte_size(data(State1)) + 32,
            Data = aevm_eeevm_store:to_sophia_state(Store),
            State2 = aevm_eeevm_memory:write_area(Addr, Data, State1),
            aevm_eeevm_memory:store(0, Addr, State2)
    end.

call_contract(Caller, Target, CallGas, Value, Data, State) ->
    case vm_version(State) of
        ?AEVM_01_Sophia_01 when Target == 0 ->  %% Primitive call
            aevm_ae_primops:call(Value, Data, State);
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

get_blockhash_fun(Opts, Env, H) ->
    case maps:get(blockhash, Opts, default) of
        %% default -> fun(N,A) -> aevm_eeevm_env:get_block_hash(H,N,A) end;
        %% sha3 ->
        _ -> fun(N,_A) ->
                        %% Because the data of the blockchain is not
                        %% given, the opcode BLOCKHASH could not
                        %% return the hashes of the corresponding
                        %% blocks. Therefore we define the hash of
                        %% block number n to be SHA3-256("n").
                        CurrentNumber = maps:get(currentNumber, Env),
                        if (N >= CurrentNumber) or (_A == 256) or (H==0) -> 0;
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
    maps:get(trace_fun, Opts, fun(S,A) -> io:format(S,A) end).


accountbalance(Address, State) ->
    Chain = chain_api(State),
    ChainState = chain_state(State),
    PubKey = <<Address:256>>,
    Chain:get_balance(PubKey, ChainState).
address(State)     -> maps:get(address, State).
blockhash(N,A,State) -> (maps:get(block_hash_fun, State))(N,A).
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

chain_state(State) -> maps:get(chain_state, State).
chain_api(State)   -> maps:get(chain_api, State).

vm_version(State)  -> maps:get(vm_version, State).

set_cp(Value, State)      -> maps:put(cp, Value, State).
set_code(Value, State)    -> maps:put(code, Value, State).
set_stack(Value, State)   -> maps:put(stack, Value, State).
set_mem(Value, State)     -> maps:put(memory, Value, State).
set_out(Value, State)     -> maps:put(out, Value, State).
set_gas(Value, State)     -> maps:put(gas, Value, State).
set_logs(Value, State)    -> maps:put(logs, Value, State).
set_storage(Value, State) -> maps:put(storage, Value, State).
set_jumpdests(Value, State)    -> maps:put(jumpdests, Value, State).
set_selfdestruct(Value, State) -> maps:put(selfdestruct, Value, State).
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
