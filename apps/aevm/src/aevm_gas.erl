%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Calculate gas of operations
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_gas).

-export([ op_gas/2
        , mem_gas/2
        , mem_limit_for_gas/2
        , mem_expansion_gas/2
        ]).

-export([ call_cap/2
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aevm_eeevm.hrl").

-import(aevm_eeevm_state, [gastable/1]).

%%====================================================================
%% API
%%====================================================================

%% NOTE: This is just purely the op gas, not including memory gas
op_gas(Op, State) ->
    try aevm_opcodes:op_base_gas(Op, State) + op_dynamic_gas(Op, State)
    catch error:{badarg, _, _} -> State %% TODO: This is not right
    end.

%% NOTE: This is just purely the mem gas, not including op gas
mem_expansion_gas(State1, State2) ->
    Size1 = aevm_eeevm_memory:size_in_words(State1),
    case aevm_eeevm_memory:size_in_words(State2) of
        Size1 -> 0;
        Size2 -> mem_gas(Size2, State1) - mem_gas(Size1, State1)
    end.

mem_gas(Size, State) ->
    Size * maps:get('GMEMORY', gastable(State)) + round(math:floor((Size * Size) / 512)).

%% Gives an upper bound on the number of words we can afford with the given
%% gas. Used to terminate Sophia serialization early if we can't pay for the
%% resulting binary.
%% Satisfies
%%   mem_limit_for_gas(mem_gas(Size, State), State) > Size
%%   mem_gas(mem_limit_for_gas(Gas, State), State) > Gas
%% but doesn't guarantee that the bound is tight.
mem_limit_for_gas(0, _) -> 0;
mem_limit_for_gas(Gas, State) ->
    GMem  = maps:get('GMEMORY', gastable(State)),
    %% Ignoring the quadratic factor gives us an upper bound
    Limit = Gas div GMem + 1,
    %% We can shrink it by repeatedly halving it until it's too low. This gives
    %% a bound that's always within a factor two of the least upper bound.
    Shrink = fun Shrink(L) ->
                    case mem_gas(L div 2, State) =< Gas of
                        true -> L;
                        false -> Shrink(L div 2)
                    end end,
    Shrink(Limit).

call_cap(?CALL, State) ->
    {CGascap, _} = call_dynamic_gas_components(State),
    CGascap;
call_cap(?CALLCODE, State) ->
    {CGascap, _} = call_dynamic_gas_components(State),
    CGascap;
call_cap(?DELEGATECALL, State) -> %% TODO This is a placeholder.
    {CGascap, _} = call_dynamic_gas_components(State),
    CGascap.

op_dynamic_gas(?CALL, State) ->
    call_dynamic_gas(State);
op_dynamic_gas(?DELEGATECALL, State) ->
    call_dynamic_gas(State);
op_dynamic_gas(?CALLCODE, State) ->
    call_dynamic_gas(State);
op_dynamic_gas(?CALLDATACOPY, State) ->
    maps:get('GCOPY', gastable(State)) * round(ceil(peek(2, State)/32));
op_dynamic_gas(?CODECOPY, State) ->
    maps:get('GCOPY', gastable(State)) * round(ceil(peek(2, State)/32));
op_dynamic_gas(?EXTCODECOPY, State) ->
    maps:get('GCOPY', gastable(State)) * round(ceil(peek(3, State)/32));
op_dynamic_gas(?LOG0, State) -> maps:get('GLOGDATA', gastable(State)) * peek(1, State);
op_dynamic_gas(?LOG1, State) -> maps:get('GLOGDATA', gastable(State)) * peek(1, State);
op_dynamic_gas(?LOG2, State) -> maps:get('GLOGDATA', gastable(State)) * peek(1, State);
op_dynamic_gas(?LOG3, State) -> maps:get('GLOGDATA', gastable(State)) * peek(1, State);
op_dynamic_gas(?LOG4, State) -> maps:get('GLOGDATA', gastable(State)) * peek(1, State);
op_dynamic_gas(?SHA3, State) ->
    Us1 = peek(1, State),
    maps:get('GSHA3WORD', gastable(State)) * round(ceil(Us1/32));
op_dynamic_gas(?SSTORE, State) ->
    Us0 = peek(0, State),
    Us1 = peek(1, State),
    Old = aevm_eeevm_store:load(Us0, State),
    case (Us1 =/= 0) andalso (Old =:= 0) of
        true  -> maps:get('GSSET', gastable(State));   %% Additional storage is needed
        false -> maps:get('GSRESET', gastable(State))  %% Resetting a new value in the store.
    end;
op_dynamic_gas(?EXP, State) ->
    case peek(1, State) of
        0 -> 0;
        Us1 -> maps:get('GEXPBYTE', gastable(State)) * (1 + floor_log_256(Us1))
    end;
op_dynamic_gas(_Op,_State) ->
    0.

%%====================================================================
%% Internal functions
%%====================================================================

call_dynamic_gas(State) ->
    {CGascap, CExtra} = call_dynamic_gas_components(State),
    CGascap + CExtra.

call_dynamic_gas_components(State) ->
    Gas = aevm_eeevm_state:gas(State),
    Us0 = peek(0, State), %% Gas
    Us2 = peek(2, State), %% Value
    CXfer = case Us2 =:= 0 of
                true  -> 0;
                false -> maps:get('GCALLVALUE', gastable(State))
            end,
    %% AEVM does not allow calls to nonexisting addresses
    %% so no CNew component.
    CExtra = CXfer + maps:get('GCALL', gastable(State)),
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
