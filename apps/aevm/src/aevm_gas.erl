%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Calculate gas cost of operations
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_gas).

-export([ op_cost/2
        , mem_cost/2
        ]).
-export([ call_cap/2
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aevm_eeevm.hrl").
-include("aevm_gas.hrl").

%%====================================================================
%% API
%%====================================================================

%% NOTE: This is just purely the op cost, not including memory cost
op_cost(Op, State) ->
    try aevm_opcodes:op_base_cost(Op) + op_dynamic_cost(Op, State)
    catch error:{badarg, _, _} -> State %% TODO: This is not right
    end.

%% NOTE: This is just purely the mem cost, not including op cost
mem_cost(State1, State2) ->
    Size1 = aevm_eeevm_memory:size_in_words(State1),
    case aevm_eeevm_memory:size_in_words(State2) of
        Size1 -> 0;
        Size2 ->
            Before = Size1*?GMEMORY + round(math:floor((Size1 * Size1)/512)),
            After  = Size2*?GMEMORY + round(math:floor((Size2 * Size2)/512)),
            After - Before
    end.

call_cap(?CALL, State) ->
    {CGascap, _} = call_dynamic_cost_components(State),
    CGascap;
call_cap(?CALLCODE, State) ->
    {CGascap, _} = call_dynamic_cost_components(State),
    CGascap;
call_cap(?DELEGATECALL, State) -> %% TODO This is a placeholder.
    {CGascap, _} = call_dynamic_cost_components(State),
    CGascap.

op_dynamic_cost(?CALL, State) ->
    call_dynamic_cost(State);
op_dynamic_cost(?DELEGATECALL, State) ->
    call_dynamic_cost(State);
op_dynamic_cost(?CALLCODE, State) ->
    call_dynamic_cost(State);
op_dynamic_cost(?CALLDATACOPY, State) ->
    ?GCOPY * round(ceil(peek(2, State)/32));
op_dynamic_cost(?CODECOPY, State) ->
    ?GCOPY * round(ceil(peek(2, State)/32));
op_dynamic_cost(?EXTCODECOPY, State) ->
    ?GCOPY * round(ceil(peek(3, State)/32));
op_dynamic_cost(?LOG0, State) -> ?GLOGDATA * peek(1, State);
op_dynamic_cost(?LOG1, State) -> ?GLOGDATA * peek(1, State);
op_dynamic_cost(?LOG2, State) -> ?GLOGDATA * peek(1, State);
op_dynamic_cost(?LOG3, State) -> ?GLOGDATA * peek(1, State);
op_dynamic_cost(?LOG4, State) -> ?GLOGDATA * peek(1, State);
op_dynamic_cost(?SHA3, State) ->
    Us1 = peek(1, State),
    ?GSHA3WORD * round(ceil(Us1/32));
op_dynamic_cost(?SSTORE, State) ->
    Us0 = peek(0, State),
    Us1 = peek(1, State),
    Old = aevm_eeevm_store:load(Us0, State),
    case (Us1 =/= 0) andalso (Old =:= 0) of
        true  -> ?GSSET;   %% Additional storage is needed
        false -> ?GSRESET  %% Resetting a new value in the store.
    end;
op_dynamic_cost(?EXP, State) ->
    case peek(1, State) of
        0 -> 0;
        Us1 -> ?GEXPBYTE * (1 + floor_log_256(Us1))
    end;
op_dynamic_cost(_Op,_State) ->
    0.

%%====================================================================
%% Internal functions
%%====================================================================

call_dynamic_cost(State) ->
    {CGascap, CExtra} = call_dynamic_cost_components(State),
    CGascap + CExtra.

call_dynamic_cost_components(State) ->
    Gas = aevm_eeevm_state:gas(State),
    Us0 = peek(0, State),
    %%Us1 = peek(1, State), %% TODO: Needed for CNEW.
    Us2 = peek(2, State),
    CNew = 0, %% TODO: Is this a new account?
    CXfer = case Us2 =:= 0 of
                true  -> 0;
                false -> ?GCALLVALUE
            end,
    CExtra = CNew + CXfer + ?GCALL,
    CGascap = case Gas >= CExtra of
                  true  -> min(all_but_one_64th(Gas - CExtra), Us0);
                  false -> Us0 %% TODO Can this case ever happen without causing out-of-gas when subtracting CExtra?
              end,
    {CGascap, CExtra}.

all_but_one_64th(X) ->
    X - round(floor(X/64)).

peek(N, State) ->
    aevm_eeevm_stack:peek(N, State).

floor_log_256(X) when is_integer(X), X > 0 ->
    floor_log_256(X, -1).

floor_log_256(0,  N) -> N;
floor_log_256(X, N) -> floor_log_256(X bsr 8, N + 1).
