%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% ADT for the engine state
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_engine_state).

-export([ new/5
        ]).

%% Getters
-export([ accumulator/1
        , accumulator_stack/1
        , bbs/1
        , call_stack/1
        , call_value/1
        , caller/1
        , chain_api/1
        , contracts/1
        , current_bb/1
        , current_contract/1
        , current_function/1
        , functions/1
        , gas/1
        , logs/1
        , memory/1
        , trace/1
        ]).

%% Setters
-export([ set_accumulator/2
        , set_accumulator_stack/2
        , set_bbs/2
        , set_call_stack/2
        , set_call_value/2
        , set_caller/2
        , set_chain_api/2
        , set_contracts/2
        , set_current_bb/2
        , set_current_contract/2
        , set_current_function/2
        , set_functions/2
        , set_gas/2
        , set_logs/2
        , set_memory/2
        , set_trace/2
        ]).

%% More complex stuff
-export([ check_reentrant_remote/2
        , current_bb_instructions/1
        , dup_accumulator/1
        , dup_accumulator/2
        , drop_accumulator/2
        , pop_accumulator/1
        , pop_call_stack/1
        , push_accumulator/2
        , push_arguments/2
        , push_call_stack/1
        , push_gas_cap/2
        , spend_gas/2
        , update_for_remote_call/3
        ]).

-ifdef(TEST).
-export([ add_trace/2
        ]).
-endif.

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").

-type void_or_fate() :: ?FATE_VOID | aeb_fate_data:fate_type().
-type void_or_address() :: ?FATE_VOID | aeb_fate_data:fate_address().

-record(es, { accumulator       :: void_or_fate()
            , accumulator_stack :: [aeb_fate_data:fate_type()]
            , bbs               :: map()
            , call_stack        :: [tuple()] %% TODO: Better type
            , caller            :: aeb_fate_data:fate_address()
            , call_value        :: non_neg_integer()
            , chain_api         :: aefa_chain_api:state()
            , contracts         :: map() %% Cache for loaded contracts.
            , current_bb        :: non_neg_integer()
            , current_contract  :: void_or_address()
            , current_function  :: ?FATE_VOID | binary()
            , functions         :: map()    %% Cache for current contract.
            , gas               :: integer()
            , logs              :: [term()] %% TODO: Not used properly yet
            , memory            :: map()    %% Environment #{name => val}
            , seen_contracts    :: [Pubkey :: <<_:256>>]
                                   %% Call stack of contracts (including tail calls)
            , trace             :: list()
            }).

-opaque state() :: #es{}.
-export_type([ state/0
             ]).

-spec new(non_neg_integer(), non_neg_integer(), map(), aefa_chain_api:state(), map()) -> state().
new(Gas, Value, Spec, APIState, Contracts) ->
    [error({bad_init_arg, X, Y}) || {X, Y} <- [{gas, Gas}, {value, Value}],
                                    not (is_integer(Y) andalso Y >= 0)],
    #es{ accumulator       = ?FATE_VOID
       , accumulator_stack = []
       , bbs               = #{}
       , call_stack        = []
       , caller            = aeb_fate_data:make_address(maps:get(caller, Spec))
       , call_value        = Value
       , chain_api         = APIState
       , contracts         = Contracts
       , current_bb        = 0
       , current_contract  = ?FATE_VOID
       , current_function  = ?FATE_VOID
       , functions         = #{}
       , gas               = Gas
       , logs              = []
       , memory            = #{}
       , seen_contracts    = []
       , trace             = []
       }.

%%%===================================================================
%%% API
%%%===================================================================

-ifdef(TEST).
add_trace(I, #es{trace = Trace} = ES) ->
    ES#es{trace = [{I, erlang:process_info(self(), reductions)}|Trace]}.
-endif.

-spec update_for_remote_call(aeb_fate_data:fate_address(), term(), state()) -> state().
update_for_remote_call(Address, ContractCode, #es{current_contract = Current} = ES) ->
    #{functions := Code} = ContractCode,
    ES#es{ functions = Code
         , current_contract = Address
         , caller = Current
         }.

-spec check_reentrant_remote(aeb_fate_data:fate_address(), state()) ->
                                    {ok, state()} | error.
check_reentrant_remote(Current, #es{current_contract = Current}) ->
    error;
check_reentrant_remote(?FATE_ADDRESS(Pubkey), #es{seen_contracts = Seen} = ES) ->
    case lists:member(Pubkey, Seen) of
        true ->
            error;
        false ->
            ?FATE_ADDRESS(Current) = current_contract(ES),
            {ok, ES#es{seen_contracts = [Current|Seen]}}
    end.

%%%------------------
%%% Accumulator stack

-spec push_arguments([aeb_fate_data:fate_type()], state()) -> state().
push_arguments(Args, #es{accumulator_stack = Stack, accumulator = Acc} = ES) ->
    push_arguments(lists:reverse(Args), Acc, Stack, ES).

push_arguments([], Acc, Stack, ES) ->
    ES#es{ accumulator = Acc
         , accumulator_stack = Stack};
push_arguments([A|As], Acc, Stack, ES) ->
    push_arguments(As, A, [Acc | Stack], ES).

-spec push_call_stack(state()) -> state().
push_call_stack(#es{ current_bb = BB
                   , current_function = Function
                   , current_contract = Contract
                   , call_stack = Stack
                   , call_value = Value
                   , memory = Mem} = ES) ->
    ES#es{call_stack = [{Contract, Function, BB+1, Mem, Value}|Stack]}.

%% TODO: Make better types for all these things
-spec pop_call_stack(state()) ->
                            'empty' |
                            {'local', _, non_neg_integer(), state()} |
                            {'remote', aeb_fate_data:fate_address(), _, non_neg_integer(), state()}.
pop_call_stack(#es{call_stack = Stack, current_contract = Current} = ES) ->
    case Stack of
        [] -> empty;
        [{gas_store, StoredGas}| Rest] ->
            ES1 = ES#es{ gas = StoredGas + gas(ES)
                       , call_stack = Rest
                       },
            pop_call_stack(ES1);
        [{Current, Function, BB, Mem, Value}| Rest] ->
            {local, Function, BB,
             ES#es{ call_value = Value
                  , memory = Mem
                  , call_stack = Rest
                  }};
        [{?FATE_ADDRESS(Pubkey) = Contract, Function, BB, Mem, Value}| Rest] ->
            Seen = pop_seen_contracts(Pubkey, ES),
            {remote, Contract, Function, BB,
             ES#es{ call_value = Value
                  , memory = Mem
                  , call_stack = Rest
                  , seen_contracts = Seen
                  }}
    end.

pop_seen_contracts(Pubkey, #es{seen_contracts = Seen}) ->
    %% NOTE: We might have remote tailcalls leaving entries here,
    %% but not in the actual call stack. Drop until we reach the
    %% contract we are returning to.
    [_|Seen1] = lists:dropwhile(fun(X) -> X =/= Pubkey end, Seen),
    Seen1.

-spec push_gas_cap(pos_integer(), state()) -> state().
push_gas_cap(GasCap, #es{gas = AvailableGas} = ES) when GasCap >= AvailableGas ->
    %% Nothing is reserved
    ES;
push_gas_cap(GasCap, #es{ gas = AvailableGas
                        , call_stack = Stack} = ES) when GasCap < AvailableGas ->
    ES#es{ call_stack = [{gas_store, AvailableGas - GasCap}|Stack]
         , gas        = GasCap
         }.

-spec push_accumulator(aeb_fate_data:fate_type(), state()) -> state().
push_accumulator(V, #es{ accumulator = ?FATE_VOID
                       , accumulator_stack = [] } = ES) ->
    ES#es{ accumulator = V
         , accumulator_stack = []};
push_accumulator(V, #es{ accumulator = X
                       , accumulator_stack = Stack } = ES) ->
    ES#es{ accumulator = V
         , accumulator_stack = [X|Stack]}.

-spec pop_accumulator(state()) -> {aeb_fate_data:fate_type(), state()}.
pop_accumulator(#es{accumulator = X, accumulator_stack = []} = ES) ->
    {X, ES#es{accumulator = ?FATE_VOID}};
pop_accumulator(#es{accumulator = X, accumulator_stack = [V|Stack]} = ES) ->
    {X, ES#es{ accumulator = V
             , accumulator_stack = Stack
           }}.

-spec dup_accumulator(state()) -> state().
dup_accumulator(#es{accumulator = X, accumulator_stack = Stack} = ES) ->
    ES#es{ accumulator = X
         , accumulator_stack = [X|Stack]}.

-spec dup_accumulator(pos_integer(), state()) -> state().
dup_accumulator(N, #es{accumulator = X, accumulator_stack = Stack} = ES) ->
    {X1, Stack1} = get_n(N, [X|Stack]),
    ES#es{ accumulator = X1
         , accumulator_stack = [X|Stack1]}.

get_n(0, [X|XS]) -> {X, [X|XS]};
get_n(N, [X|XS]) ->
    {Y, List} = get_n(N-1, XS),
    {Y, [X|List]}.

-spec drop_accumulator(non_neg_integer(), state()) -> state().
drop_accumulator(0, ES) -> ES;
drop_accumulator(N, #es{accumulator_stack = [V|Stack]} = ES) ->
    drop_accumulator(N-1, ES#es{accumulator = V, accumulator_stack = Stack});
drop_accumulator(N, #es{accumulator_stack = []} = ES) ->
    drop_accumulator(N-1, ES#es{accumulator = ?FATE_VOID, accumulator_stack = []}).

%%%------------------

-spec current_bb_instructions(state()) -> list().
current_bb_instructions(#es{current_bb = BB, bbs = BBS} = ES) ->
    case maps:get(BB, BBS, void) of
        void -> aefa_fate:abort({trying_to_reach_bb, BB}, ES);
        Instructions -> Instructions
    end.

%%%------------------

-spec spend_gas(non_neg_integer(), state()) -> state().
spend_gas(X, #es{gas = Gas} = ES) ->
    NewGas = Gas - X,
    case NewGas < 0 of
        true  -> aefa_fate:abort(out_of_gas, ES);
        false -> ES#es{gas = NewGas}
    end.

%%%------------------

-spec accumulator(state()) -> void_or_fate().
accumulator(#es{accumulator = X}) ->
    X.

-spec set_accumulator(void_or_fate(), state()) -> state().
set_accumulator(X, ES) ->
    ES#es{accumulator = X}.

%%%------------------

-spec accumulator_stack(state()) -> [aeb_fate_data:fate_type()].
accumulator_stack(#es{accumulator_stack = X}) ->
    X.

-spec set_accumulator_stack([aeb_fate_data:fate_type()], state()) -> state().
set_accumulator_stack(X, ES) ->
    ES#es{accumulator_stack = X}.

%%%------------------

-spec bbs(state()) -> map().
bbs(#es{bbs = X}) ->
    X.

-spec set_bbs(map(), state()) -> state().
set_bbs(X, ES) ->
    ES#es{bbs = X}.

%%%------------------

-spec call_stack(state()) -> list().
call_stack(#es{call_stack = X}) ->
    X.

-spec set_call_stack(list(), state()) -> state().
set_call_stack(X, ES) ->
    ES#es{call_stack = X}.

%%%------------------

-spec call_value(state()) -> non_neg_integer().
call_value(#es{call_value = X}) ->
    X.

-spec set_call_value(non_neg_integer(), state()) -> state().
set_call_value(X, ES) when is_integer(X), X >= 0 ->
    ES#es{call_value = X}.

%%%------------------

-spec caller(state()) -> aeb_fate_data:fate_address().
caller(#es{caller = X}) ->
    X.

-spec set_caller(aeb_fate_data:fate_address(), state()) -> state().
set_caller(X, ES) ->
    ES#es{caller = X}.

%%%------------------

-spec chain_api(state()) -> aefa_chain_api:state().
chain_api(#es{chain_api = X}) ->
    X.

-spec set_chain_api(aefa_chain_api:state(), state()) -> state().
set_chain_api(X, ES) ->
    ES#es{chain_api = X}.

%%%------------------

-spec contracts(state()) -> map().
contracts(#es{contracts = X}) ->
    X.

-spec set_contracts(map(), state()) -> state().
set_contracts(X, ES) ->
    ES#es{contracts = X}.

%%%------------------

-spec current_bb(state()) -> non_neg_integer().
current_bb(#es{current_bb = X}) ->
    X.

-spec set_current_bb(non_neg_integer(), state()) -> state().
set_current_bb(X, ES) ->
    ES#es{current_bb = X}.

%%%------------------

-spec current_contract(state()) -> aeb_fate_data:fate_address().
current_contract(#es{current_contract = X}) ->
    X.

-spec set_current_contract(aeb_fate_data:fate_address(), state()) -> state().
set_current_contract(X, ES) ->
    ES#es{current_contract = X}.

%%%------------------

-spec current_function(state()) -> binary().
current_function(#es{current_function = X}) ->
    X.

-spec set_current_function(binary(), state()) -> state().
set_current_function(X, ES) ->
    ES#es{current_function = X}.

%%%------------------

-spec functions(state()) -> map().
functions(#es{functions = X}) ->
    X.

-spec set_functions(map(), state()) -> state().
set_functions(X, ES) ->
    ES#es{functions = X}.

%%%------------------

-spec gas(state()) -> integer().
gas(#es{gas = X}) ->
    X.

-spec set_gas(integer(), state()) -> state().
set_gas(X, ES) ->
    ES#es{gas = X}.

%%%------------------

-spec logs(state()) -> list().
logs(#es{logs = X}) ->
    X.

-spec set_logs(list(), state()) -> state().
set_logs(X, ES) ->
    ES#es{logs = X}.

%%%------------------

-spec memory(state()) -> map().
memory(#es{memory = X}) ->
    X.

-spec set_memory(map(), state()) -> state().
set_memory(X, ES) ->
    ES#es{memory = X}.

%%%------------------

-spec trace(state()) -> list().
trace(#es{trace = X}) ->
    X.

-spec set_trace(list(), state()) -> state().
set_trace(X, ES) ->
    ES#es{trace = X}.


