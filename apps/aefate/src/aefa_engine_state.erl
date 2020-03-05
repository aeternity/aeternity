%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% ADT for the engine state
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_engine_state).

-export([ new/7
        , finalize/1
        ]).

%% Getters
-export([ accumulator/1
        , accumulator_stack/1
        , bbs/1
        , call_stack/1
        , call_value/1
        , caller/1
        , chain_api/1
        , code_cache/1
        , creator_cache/1
        , current_bb/1
        , current_contract/1
        , current_function/1
        , current_tvars/1
        , functions/1
        , gas/1
        , logs/1
        , memory/1
        , stores/1
        , trace/1
        , vm_version/1
        ]).

%% Setters
-export([ set_accumulator/2
        , set_accumulator_stack/2
        , set_bbs/2
        , set_call_stack/2
        , set_call_value/2
        , set_caller/2
        , set_chain_api/2
        , set_code_cache/2
        , set_creator_cache/2
        , set_current_bb/2
        , set_current_contract/2
        , set_current_function/2
        , set_current_tvars/2
        , set_functions/2
        , set_gas/2
        , add_log/2
        , set_memory/2
        , set_stores/2
        , set_trace/2
        ]).

%% More complex stuff
-export([ check_reentrant_remote/2
        , collect_gas_stores_on_error/1
        , collect_gas_stores_on_exit/1
        , collect_gas_stores_on_revert/1
        , current_bb_instructions/1
        , dup_accumulator/1
        , dup_accumulator/2
        , drop_accumulator/2
        , in_auth_context/1
        , is_onchain/1
        , pop_accumulator/1
        , pop_call_stack/1
        , push_accumulator/2
        , push_arguments/2
        , push_call_stack/1
        , push_gas_cap/2
        , push_return_type_check/4
        , spend_gas/2
        , spend_gas_for_new_cells/2
        , update_for_remote_call/4
        ]).

-ifdef(TEST).
-export([ add_trace/2
        ]).
-endif.

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").

-type void_or_fate() :: ?FATE_VOID | aeb_fate_data:fate_type().
-type pubkey() :: <<_:256>>.

-record(es, { accumulator       :: void_or_fate()
            , accumulator_stack :: [aeb_fate_data:fate_type()]
            , bbs               :: map()
            , call_stack        :: [tuple()] %% TODO: Better type
            , caller            :: aeb_fate_data:fate_address()
            , call_value        :: non_neg_integer()
            , chain_api         :: aefa_chain_api:state()
            , code_cache        :: map() %% Cache for loaded contracts.
            , creator_cache     :: map() %% Cache for creators of contracts
            , created_cells     :: integer() %% Heap memory used
            , current_bb        :: non_neg_integer()
            , current_contract  :: ?FATE_VOID | pubkey()
            , current_function  :: ?FATE_VOID | binary()
            , current_tvars     :: map()    %% Instantiations for type variables in the current call (needed when type checking return value)
            , functions         :: map()    %% Cache for current contract.
            , gas               :: integer()
            , logs              :: [term()]
            , memory            :: map()    %% Environment #{name => val}
            , seen_contracts    :: [pubkey()]
                                   %% Call stack of contracts (including tail calls)
            , stores            :: aefa_stores:store()
            , trace             :: list()
            , vm_version        :: non_neg_integer()
            }).

-opaque state() :: #es{}.
-export_type([ state/0
             ]).

-spec new(non_neg_integer(), non_neg_integer(), map(), aefa_stores:store(), aefa_chain_api:state(), map(), non_neg_integer()) -> state().
new(Gas, Value, Spec, Stores, APIState, CodeCache, VMVersion) ->
    [error({bad_init_arg, X, Y}) || {X, Y} <- [{gas, Gas}, {value, Value}],
                                    not (is_integer(Y) andalso Y >= 0)],
    #es{ accumulator       = ?FATE_VOID
       , accumulator_stack = []
       , bbs               = #{}
       , call_stack        = []
       , caller            = aeb_fate_data:make_address(maps:get(caller, Spec))
       , call_value        = Value
       , chain_api         = APIState
       , code_cache        = CodeCache
       , creator_cache     = #{}
       , created_cells     = 0
       , current_bb        = 0
       , current_contract  = ?FATE_VOID
       , current_function  = ?FATE_VOID
       , current_tvars     = #{}
       , functions         = #{}
       , gas               = Gas
       , logs              = []
       , memory            = #{}
       , seen_contracts    = []
       , stores            = Stores
       , trace             = []
       , vm_version        = VMVersion
       }.

aefa_stores(#es{ vm_version = Version }) ->
    case Version >= ?VM_FATE_SOPHIA_2 of
        true  -> aefa_stores;
        false -> aefa_stores_lima
    end.

-spec finalize(state()) -> {ok, state()} | {error, out_of_gas}.
finalize(#es{chain_api = API, stores = Stores} = ES) ->
    Gas = gas(ES),
    Aefa_stores = aefa_stores(ES),
    case Aefa_stores:finalize(API, Gas, Stores) of
        {ok, Stores1, GasLeft} ->
            {ok, ES#es{chain_api = Stores1, gas = GasLeft}};
        {error, out_of_gas} ->
            {error, out_of_gas}
    end.

%%%===================================================================
%%% API
%%%===================================================================

-ifdef(TEST).
add_trace(I, #es{trace = Trace} = ES) ->
    ES#es{trace = [{I, erlang:process_info(self(), reductions)}|Trace]}.
-endif.

-spec update_for_remote_call(pubkey(), term(), aeb_fate_data:fate_address(), state()) -> state().
update_for_remote_call(Contract, ContractCode, Caller, ES) ->
    ES#es{ functions = aeb_fate_code:functions(ContractCode)
         , current_contract = Contract
         , caller = Caller
         }.

-spec check_reentrant_remote(aeb_fate_data:fate_contract(), state()) ->
                                    {ok, state()} | error.
check_reentrant_remote(?FATE_CONTRACT(Current), #es{current_contract = Current}) ->
    error;
check_reentrant_remote(?FATE_CONTRACT(Pubkey), #es{seen_contracts = Seen} = ES) ->
    case lists:member(Pubkey, Seen) of
        true ->
            error;
        false ->
            {ok, ES#es{seen_contracts = [current_contract(ES)|Seen]}}
    end.

-spec is_onchain(state()) -> boolean().
is_onchain(#es{chain_api = APIState}) ->
    aefa_chain_api:is_onchain(APIState).

-spec in_auth_context(state()) -> boolean().
in_auth_context(#es{chain_api = APIState}) ->
    undefined =/= aetx_env:ga_tx_hash(aefa_chain_api:tx_env(APIState)).


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
                   , current_tvars    = TVars
                   , accumulator = Acc
                   , accumulator_stack = AccS
                   , call_stack = Stack
                   , call_value = Value
                   , caller = Caller
                   , memory = Mem} = ES) ->
    AccS1 = [Acc || Acc /= void] ++ AccS,
    ES#es{accumulator       = void,
          accumulator_stack = [],
          call_stack        = [{Caller, Contract, Function, TVars, BB + 1, AccS1, Mem, Value}|Stack]}.

%% TODO: Make better types for all these things
-spec pop_call_stack(state()) ->
                            {'empty', state()} |
                            {'return_check', map(), protected | unprotected, aeb_fate_data:fate_type_type(),
                                             aefa_stores:store(), aefa_chain_api:state(), state()} |
                            {'local', _, map(), non_neg_integer(), state()} |
                            {'remote', aeb_fate_data:fate_address(), aeb_fate_data:fate_contract(),
                                       _, map(), non_neg_integer(), state()}.
pop_call_stack(#es{accumulator = ReturnValue,
                   call_stack = Stack,
                   current_contract = Current} = ES) ->
    case Stack of
        [] -> {empty, ES};
        [{return_check, TVars, Protected, Stores, API, ReturnType}| Rest] ->
            {return_check, TVars, Protected, ReturnType, Stores, API, ES#es{ call_stack = Rest}};
        [{gas_store, StoredGas}| Rest] ->
            ES1 = ES#es{ gas = StoredGas + gas(ES)
                       , call_stack = Rest
                       },
            pop_call_stack(ES1);
        [{_Caller, Current, Function, TVars, BB, AccS, Mem, Value}| Rest] ->
            {local, Function, TVars, BB,
             ES#es{ call_value = Value
                  , accumulator = ReturnValue
                  , accumulator_stack = AccS
                  , memory = Mem
                  , call_stack = Rest
                  }};
        [{Caller, Pubkey, Function, TVars, BB, AccS, Mem, Value}| Rest] ->
            Seen = pop_seen_contracts(Pubkey, ES),
            {remote, Caller, aeb_fate_data:make_contract(Pubkey), Function, TVars, BB,
             ES#es{ call_value = Value
                  , accumulator = ReturnValue
                  , accumulator_stack = AccS
                  , memory = Mem
                  , call_stack = Rest
                  , seen_contracts = Seen
                  }}
    end.

-spec collect_gas_stores_on_error(state()) -> integer().
collect_gas_stores_on_error(#es{call_stack = Stack}) ->
    collect_gas_stores(Stack, 0).

-spec collect_gas_stores_on_exit(state()) -> integer().
collect_gas_stores_on_exit(#es{call_stack = Stack}) ->
    collect_gas_stores(Stack, 0).

-spec collect_gas_stores_on_revert(state()) -> integer().
collect_gas_stores_on_revert(#es{call_stack = Stack, gas = Gas}) ->
    collect_gas_stores(Stack, Gas).

collect_gas_stores([{gas_store, Gas}|Left], AccGas) ->
    collect_gas_stores(Left, AccGas + Gas);
collect_gas_stores([{return_check, _, _, _, _, _}|Left], AccGas) ->
    collect_gas_stores(Left, AccGas);
collect_gas_stores([{_, _, _, _, _, _, _, _}|Left], AccGas) ->
    collect_gas_stores(Left, AccGas);
collect_gas_stores([], AccGas) ->
    AccGas.

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

-spec push_return_type_check(aeb_fate_data:fate_type_type(), #{}, unprotected | protected, state()) -> state().
push_return_type_check(RetType, TVars, Protected, #es{ call_stack = Stack, stores = Stores, chain_api = API } = ES) ->
    %% Note that the TVars must correspond to the bindings for the
    %% return type.  Typically, the current_tvars corresponds to the
    %% next function in the call stack.
    ES#es{ call_stack = [{return_check, TVars, Protected, Stores, API, RetType}|Stack]}.

-spec push_accumulator(aeb_fate_data:fate_type(), state()) -> state().
push_accumulator(V, #es{ accumulator = ?FATE_VOID
                       , accumulator_stack = [] } = ES) ->
    ES1 = ES#es{ accumulator = V
               , accumulator_stack = []},
    spend_gas_for_new_cells(1, ES1);
push_accumulator(V, #es{ accumulator = X
                       , accumulator_stack = Stack } = ES) ->
    ES1 = ES#es{ accumulator = V
               , accumulator_stack = [X|Stack]},
    spend_gas_for_new_cells(1, ES1).

-spec pop_accumulator(state()) -> {aeb_fate_data:fate_type(), state()}.
pop_accumulator(#es{accumulator = X, accumulator_stack = [], created_cells = C} = ES) ->
    {X, ES#es{ accumulator = ?FATE_VOID
             , created_cells = C - 1}};
pop_accumulator(#es{accumulator = X,
                    accumulator_stack = [V|Stack],
                    created_cells = C} = ES) ->
    {X, ES#es{ accumulator = V
             , accumulator_stack = Stack
             , created_cells = C - 1
             }}.

-spec dup_accumulator(state()) -> state().
dup_accumulator(#es{accumulator = X, accumulator_stack = Stack} = ES) ->
    ES1 = ES#es{ accumulator = X
               , accumulator_stack = [X|Stack]},
    spend_gas_for_new_cells(1, ES1).

-spec dup_accumulator(pos_integer(), state()) -> state().
dup_accumulator(N, #es{accumulator = X, accumulator_stack = Stack} = ES) ->
    {X1, Stack1} = get_n(N, [X|Stack]),
    ES1 = ES#es{ accumulator = X1
               , accumulator_stack = [X|Stack1]},
    spend_gas_for_new_cells(1, ES1).

get_n(0, [X|XS]) -> {X, [X|XS]};
get_n(N, [X|XS]) ->
    {Y, List} = get_n(N-1, XS),
    {Y, [X|List]}.

-spec drop_accumulator(non_neg_integer(), state()) -> state().
drop_accumulator(0, ES) -> ES;
drop_accumulator(N, #es{accumulator_stack = [V|Stack],
                        created_cells = C
                       } = ES) ->
    drop_accumulator(N-1, ES#es{ accumulator = V
                               , accumulator_stack = Stack
                               , created_cells = C - 1
                               });
drop_accumulator(N, #es{accumulator_stack = [],
                        created_cells = C
                       } = ES) ->
    drop_accumulator(N-1, ES#es{accumulator = ?FATE_VOID
                               , accumulator_stack = []
                               , created_cells = C - 1
                               }).

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

%% The gas price per cell increases by 1 for each kibiword (each 1024 64-bit word) used.
-spec spend_gas_for_new_cells(non_neg_integer(), state()) -> state().
spend_gas_for_new_cells(NewCells, #es{ created_cells = Cells } = ES) when NewCells + Cells =< 1024 ->
    TotalCells = Cells + NewCells,
    spend_gas(NewCells, ES#es{ created_cells = TotalCells });
spend_gas_for_new_cells(1, #es{ created_cells = Cells } = ES) ->
    TotalCells = Cells + 1,
    CellCost = 1 + (TotalCells bsr 10),
    spend_gas(CellCost, ES#es{ created_cells = TotalCells });
spend_gas_for_new_cells(NewCells, #es{ created_cells = Cells } = ES) ->
    TotalCells = Cells + NewCells,
    CellCost = 1 + (TotalCells bsr 10),
    spend_gas(NewCells * CellCost, ES#es{ created_cells = TotalCells }).

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

-spec code_cache(state()) -> map().
code_cache(#es{code_cache = X}) ->
    X.

-spec set_code_cache(map(), state()) -> state().
set_code_cache(X, ES) ->
    ES#es{code_cache = X}.

%%%------------------

-spec creator_cache(state()) -> map().
creator_cache(#es{creator_cache = X}) ->
    X.

-spec set_creator_cache(map(), state()) -> state().
set_creator_cache(X, ES) ->
    ES#es{creator_cache = X}.

%%%------------------

-spec current_bb(state()) -> non_neg_integer().
current_bb(#es{current_bb = X}) ->
    X.

-spec set_current_bb(non_neg_integer(), state()) -> state().
set_current_bb(X, ES) ->
    ES#es{current_bb = X}.

%%%------------------

-spec current_contract(state()) -> pubkey().
current_contract(#es{current_contract = X}) ->
    X.

-spec set_current_contract(pubkey(), state()) -> state().
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

-spec current_tvars(state()) -> map().
current_tvars(#es{current_tvars = X}) ->
    X.

-spec set_current_tvars(map(), state()) -> state().
set_current_tvars(X, ES) ->
    ES#es{current_tvars = X}.

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

-spec add_log(term(), state()) -> state().
add_log(X, ES) ->
    ES#es{logs = [X | ES#es.logs]}.

%%%------------------

-spec memory(state()) -> map().
memory(#es{memory = X}) ->
    X.

-spec set_memory(map(), state()) -> state().
set_memory(X, ES) ->
    ES#es{memory = X}.

%%%------------------

-spec stores(state()) -> aefa_stores:store().
stores(#es{stores=X}) ->
    X.

-spec set_stores(aefa_stores:store(), state()) -> state().
set_stores(X, ES) ->
    ES#es{stores=X}.

%%%------------------

-spec trace(state()) -> list().
trace(#es{trace = X}) ->
    X.

-spec set_trace(list(), state()) -> state().
set_trace(X, ES) ->
    ES#es{trace = X}.

%%%------------------
-spec vm_version(state()) -> non_neg_integer().
vm_version(#es{vm_version = X}) ->
    X.
