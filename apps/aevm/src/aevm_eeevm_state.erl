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
        , caller/1
        , callcreates/1
	, code/1
	, coinbase/1
	, cp/1
	, data/1
	, difficulty/1
	, extbalance/2
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
        , prepare_for_call/7
	, mem/1
        , no_recursion/1
	, number/1
	, return_data/1
        , chain_state/1
        , chain_api/1
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
	, value/1
	]).

-include("aevm_eeevm.hrl").

init(Spec) -> init(Spec, #{}).

init(#{ env  := Env
      , exec := Exec
      , pre  := Pre} = Spec, Opts) ->
    Address = maps:get(address, Exec),
    BlockHashFun = get_blockhash_fun(Opts, Env, Address),
    NoRecursion = maps:get(no_recursion, Opts, false),

    State =
	#{ address     => Address
	 , caller      => maps:get(caller, Exec)
	 , return_data => maps:get(return_data, Exec, <<>>)
	 , data        => maps:get(data, Exec)
	 , gas         => maps:get(gas, Exec)
	 , gas_price   => maps:get(gasPrice, Exec)
	 , origin      => maps:get(origin, Exec)
	 , value       => maps:get(value, Exec)
	 , calldepth   => 0

	 , coinbase   => maps:get(currentCoinbase, Env)
	 , difficulty => maps:get(currentDifficulty, Env)
	 , gas_limit  => maps:get(currentGasLimit, Env)
	 , number     => maps:get(currentNumber, Env)
	 , timestamp  => maps:get(currentTimestamp, Env)

	 , balances        => get_balances(Pre)
	 , ext_code_blocks => get_ext_code_blocks(Pre)
	 , ext_code_sizes  => get_ext_code_sizes(Pre)
	 , block_hash_fun  => BlockHashFun
	 , no_recursion => NoRecursion

	 , do_trace  => maps:get(trace, Opts, false)
	 , trace => []
	 , trace_fun => init_trace_fun(Opts)

         , chain_state => maps:get(chainState, Env)
         , chain_api   => maps:get(chainAPI, Env)

	 , environment =>
	       #{ spec => Spec
		, options => Opts }

	 },

    init_vm(State,
	    maps:get(code, Exec),
	    init_storage(Address, Pre)).

init_vm(State, Code, Store) ->
    State#{ out       => <<>>
	  , call      => #{}
	  , callcreates => []
	  , code      => Code
	  , cp        => 0
	  , logs      => []
	  , memory    => #{}
	  , return_data => <<>>
	  , stack     => []
	  , storage   => Store
	  }.

prepare_for_call(Caller, Dest, CallGas, Value, Code, CallDepth, State) ->
    #{ environment := #{ spec := #{ pre := Pre}}} = State,
    Store = init_storage(Dest, Pre),
    State1 = init_vm(State, Code, Store),
    State1#{ address => Dest
	   , gas => CallGas
	   , value => Value
	   , caller    => Caller
	   , calldepth => CallDepth + 1
	   }.

init_storage(Address, #{} = Pre) ->
    case maps:get(Address, Pre, undefined) of
        undefined -> #{};
        #{storage := S} -> S
    end.

%%get_code(Address, #{} = Pre) ->
%%    case maps:get(Address, Pre, undefined) of
%%        undefined -> <<>>;
%%        #{code := Code} -> Code
%%    end.


get_ext_code_sizes(#{} = Pre) ->
    maps:from_list(
      [{Address, byte_size(C)} || {Address, #{code := C}} <-maps:to_list(Pre)]).

get_ext_code_blocks(#{} = Pre) ->
    maps:from_list(
      [{Address, C} || {Address, #{code := C}} <-maps:to_list(Pre)]).

get_balances(#{} = Pre) ->
    maps:from_list(
      [{Address, B} || {Address, #{balance := B}}
			   <- maps:to_list(Pre)]).

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
    maps:get(Address band ?MASK160, maps:get(balances, State), 0).
address(State)     -> maps:get(address, State).
blockhash(N,A,State) -> (maps:get(block_hash_fun, State))(N,A).
calldepth(State) -> maps:get(calldepth, State).
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
extbalance(Account, State) ->
    maps:get(Account band ?MASK160,
	     maps:get(balances, State), <<>>).
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
	    F(" ~w", [stack(State)]),
	    F(" ~s", [format_mem(mem(State))]),
	    F(" ~p", [gas(State)]),
	    F(String, Argument),
	    add_trace([{CP, OP, aevm_opcodes:op_name(OP)}], State);
	false ->
	    State
    end.

-define(MAXMEMPOS,5).

format_mem(Mem) ->
   lists:flatten(
     "[" ++
	 format_mem(lists:sort(maps:to_list(Mem)), ?MAXMEMPOS)
     ++ "]").
format_mem([],_) -> [];
format_mem( _,0) -> " ...";
format_mem([{N,V}|Rest], ?MAXMEMPOS) when is_integer(N) ->
    io_lib:format("~w:~w",[N,V]) ++ format_mem(Rest, ?MAXMEMPOS-1);
format_mem([{N,V}|Rest],          P) when is_integer(N) ->
    io_lib:format(", ~w:~w",[N,V]) ++ format_mem(Rest, P-1);
format_mem([_|Rest], P) -> format_mem(Rest, P).
