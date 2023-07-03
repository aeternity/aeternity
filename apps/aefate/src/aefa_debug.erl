-module(aefa_debug).

%% Debug info getters
-export([ breakpoints/1
        , current_instruction/1
        , debugger_status/1
        , debugger_location/1
        , call_stack/1
        ]).

%% Debug info setters
-export([ set_breakpoints/2
        , set_debugger_status/2
        , set_debugger_location/2
        ]).

%% Debug info functions
-export([ new/0
        , add_variable_register/3
        , del_variable_register/3
        , get_variable_register/2
        , inc_current_instruction/1
        , reset_current_instruction/1
        , debugger_resume/2
        , contract_name/2
        , set_contract_name/3
        , push_call_stack/1
        , pop_call_stack/1
        ]).

-export_type([info/0]).

-type debugger_status() :: break
                         | continue
                         | stepin
                         | {stepover, any()}  %% TODO: better type here
                         | {stepout, any()}.  %% TODO: better type here

-type debugger_location() :: none | {string(), integer()}.
-type pubkey() :: <<_:256>>.

-record(dbginf, { status = continue       :: debugger_status()
                , location = none         :: debugger_location()
                , breakpoints = []        :: list()
                , current_instruction = 0 :: integer()
                , vars_registers = #{}    :: #{string() => list(tuple())}
                , call_stack = []         :: [{string(), pos_integer()}]
                , contract_names = #{}    :: #{pubkey() => string()}
                }).

-opaque info() :: #dbginf{}.


-spec new() -> info().
new() ->
    #dbginf{}.


-spec push_call_stack(info()) -> info().
push_call_stack(Info = #dbginf{location = Loc, call_stack = Stack}) ->
    Info#dbginf{call_stack = [Loc | Stack]}.

-spec pop_call_stack(info()) -> info().
pop_call_stack(Info = #dbginf{call_stack = []}) ->
    Info;
pop_call_stack(Info = #dbginf{call_stack = [_ | Rest]}) ->
    Info#dbginf{call_stack = Rest}.


-spec breakpoints(info()) -> list().
breakpoints(#dbginf{breakpoints = Breakpoints}) ->
    Breakpoints.

-spec set_breakpoints(list(), info()) -> info().
set_breakpoints(BPs, Info) ->
    Info#dbginf{breakpoints = BPs}.


-spec current_instruction(info()) -> integer().
current_instruction(#dbginf{current_instruction = Current}) ->
    Current.

-spec inc_current_instruction(info()) -> info().
inc_current_instruction(Info = #dbginf{current_instruction = Current}) ->
    Info#dbginf{current_instruction = Current + 1}.

-spec reset_current_instruction(info()) -> info().
reset_current_instruction(Info) ->
    Info#dbginf{current_instruction = 0}.


-spec debugger_status(info()) -> debugger_status().
debugger_status(#dbginf{status = Status}) ->
    Status.

-spec set_debugger_status(debugger_status(), info()) -> info().
set_debugger_status(Status, Info) ->
    Info#dbginf{status = Status}.


-spec debugger_location(info()) -> debugger_location().
debugger_location(#dbginf{location = Location}) ->
    Location.

-spec set_debugger_location(debugger_location(), info()) -> info().
set_debugger_location(Location, Info) ->
    Info#dbginf{location = Location}.


-spec call_stack(info()) -> [{string(), pos_integer()}].
call_stack(#dbginf{call_stack = Stack}) ->
    Stack.


-spec add_variable_register(string(), tuple(), info()) -> info().
add_variable_register(Var, Reg, Info = #dbginf{vars_registers = VarsRegs}) ->
    Old = maps:get(Var, VarsRegs, []),
    New = [Reg | Old],
    Info#dbginf{vars_registers = VarsRegs#{Var => New}}.

-spec del_variable_register(string(), tuple(), info()) -> info().
del_variable_register(Var, Reg, Info = #dbginf{vars_registers = VarsRegs}) ->
    New = lists:delete(Reg, maps:get(Var, VarsRegs, [])),
    Info#dbginf{vars_registers = VarsRegs#{Var => New}}.

-spec get_variable_register(string(), info()) -> tuple().
get_variable_register(Var, #dbginf{vars_registers = VarsRegs}) ->
    case maps:get(Var, VarsRegs, [undefined]) of
        []        -> undefined;
        [Reg | _] -> Reg
    end.


-spec debugger_resume([tuple()], info()) -> info().
debugger_resume(CallStack, Info = #dbginf{status = {Step, Stack}})
  when Step == stepover andalso length(CallStack) =< length(Stack);
       Step == stepout  andalso length(CallStack)  < length(Stack) ->
    Info#dbginf{status = break};
debugger_resume(_CallStack, Info = #dbginf{status = stepin}) ->
    Info#dbginf{status = break};
debugger_resume(_CallStack, Info) ->
    Info.


-spec contract_name(pubkey(), info()) -> string() | pubkey().
contract_name(ContractPK, #dbginf{contract_names = ContractNames}) ->
    maps:get(ContractPK, ContractNames, ContractPK).

-spec set_contract_name(pubkey(), string(), info()) -> info().
set_contract_name(ContractPK, ContractName, Info = #dbginf{contract_names = OldNames}) ->
    NewNames = OldNames#{ContractPK => ContractName},
    Info#dbginf{contract_names = NewNames}.
