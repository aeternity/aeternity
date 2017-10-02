-module(aevm_eeevm_state).
%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle the machine state for the EEEVM.
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-export([ code/1
	, cp/1
	, gas/1
	, init/1
	, mem/1
	, stack/1
	, set_code/2
	, set_cp/2
	, set_gas/2
	, set_mem/2
	, set_stack/2
	, trace_format/3
	]).

init(Spec) ->
    Code = maps:get(code, Spec),
    Gas = maps:get(gas, Spec),
    Trace = maps:get(trace, Spec, false),
    TraceFun = maps:get(trace_fun, Spec, fun(S,A) -> io:format(S,A) end),

    #{ stack     => []
     , mem       => #{}
     , code      => Code
     , gas       => Gas
     , cp        => 0
     , do_trace  => Trace
     , trace_fun => TraceFun
     , trace     => []
     }.


cp(State)    -> maps:get(cp, State).
code(State)  -> maps:get(code, State).
stack(State) -> maps:get(stack, State).
mem(State)   -> maps:get(mem, State).
gas(State)   -> maps:get(gas, State).
do_trace(State) -> maps:get(do_trace, State).
trace(State) -> maps:get(trace, State).
trace_fun(State) -> maps:get(trace_fun, State).


set_cp(Value, State)    -> maps:put(cp, Value, State).
set_code(Value, State)  -> maps:put(code, Value, State).
set_stack(Value, State) -> maps:put(stack, Value, State).
set_mem(Value, State)   -> maps:put(mem, Value, State).
set_gas(Value, State)   -> maps:put(gas, Value, State).

add_trace(T, State) ->
    Trace = maps:get(trace, State),
    maps:put(trace, Trace ++ [T], State).

trace_format(String, Argument, State) ->
    CP   = aevm_eeevm_state:cp(State),
    Code = aevm_eeevm_state:code(State),
    OP   = aevm_eeevm:code_get_op(CP, Code),
    case do_trace(State) of
	true ->
	    F = trace_fun(State),
	    F("~8.16.0B : ~2.16.0B", [CP, OP]),
	    F(" ~w", [stack(State)]),
	    F(String, Argument),
	    add_trace({CP, OP}, State);
	false ->
	    State
    end.
