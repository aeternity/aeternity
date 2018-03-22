%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle the stack
%%% @end
%%% Created : 0 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_eeevm_stack).

-export([ dup/2
        , peek/2
        , pop/1
        , push/2
        , swap/2
        ]).

-include("aevm_eeevm.hrl").

%%====================================================================
%% API
%%====================================================================

push(Arg, State) ->
    Val = Arg band ?MASK256,
    Stack   = aevm_eeevm_state:stack(State),
    if length(Stack) < 1024 ->
	    aevm_eeevm_state:set_stack([Val|Stack], State);
       true ->
	    throw(?aevm_eval_error(out_of_stack, State))
    end.

pop(State) ->
    case aevm_eeevm_state:stack(State) of
	[Arg|Stack] ->
	    {Arg, aevm_eeevm_state:set_stack(Stack, State)};
	[] ->
	    throw(?aevm_eval_error(error_pop_empty_stack, State))
    end.

peek(N, State) when is_integer(N), N >= 0 ->
    peek_n(N, aevm_eeevm_state:stack(State)).

peek_n(0, [X|_]) -> X;
peek_n(1, [_,X|_]) -> X;
peek_n(2, [_,_,X|_]) -> X;
peek_n(N, List) ->
    try lists:nth(N + 1, List)
    catch _:_ -> error({badarg, N, List})
    end.

dup(N, State) ->
    case aevm_eeevm_state:stack(State) of
	[] ->
	    throw(?aevm_eval_error(error_dup_empty_stack, State));
	Stack ->
	    case length(Stack) < N of
		true ->
		    throw(?aevm_eval_error(error_dup_too_small_stack, State));
		false ->
		    Val = lists:nth(N, Stack),
		    push(Val, State)
	    end
    end.

swap(N, State) ->
    case aevm_eeevm_state:stack(State) of
	[] ->
    	    throw(?aevm_eval_error(error_swap_empty_stack, State));
	[Top|Rest] ->
	    case length(Rest) < N of
		true ->
		    throw(?aevm_eval_error(error_swap_too_small_stack, State));
		false ->
		    Nth = lists:nth(N, Rest),
		    Stack = [Nth| set_nth(N, Top, Rest)],
		    aevm_eeevm_state:set_stack(Stack, State)
	    end
    end.

set_nth(1, Val, [_|Rest]) -> [Val|Rest];
set_nth(N, Val, [E|Rest]) -> [E|set_nth(N-1, Val, Rest)].
