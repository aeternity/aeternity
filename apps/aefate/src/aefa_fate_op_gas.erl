%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Node-side per-protocol prices for FATE instructions. The base table
%%%    lives in aeb_fate_generate_ops in the pinned aebytecode dependency;
%%%    priv/aefa_gen_dispatch folds any entry here into the schedule it emits.
%%%    An empty table generates a byte-identical dispatch.
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_fate_op_gas).

-export([ overrides/0
        , gas_term/2
        , gas_term/3
        , schedule/1
        , first_repriceable_protocol/0
        ]).

%% Floor marker, not a lookup key: get_gas/2 returns a schedule's last entry
%% whatever its key. A literal rather than an include because aefa_gen_dispatch
%% compiles this module before the other applications are on the code path.
-define(LOWEST_PROTOCOL_VSN, 1).

%% Lowest protocol an entry may name. RAISE IT THE RELEASE A PROTOCOL IS GIVEN
%% AN ACTIVATION HEIGHT: a table that still permits an activated protocol
%% permits rewriting its history. The test pins this to hard_forks.hrl.
-define(FIRST_REPRICEABLE_PROTOCOL_VSN, 7).

-type gas()       :: non_neg_integer().
-type protocol()  :: pos_integer().
%% Strictly descending in protocol, as aefa_engine_state:get_gas/2 expects.
-type schedule()  :: [{protocol(), gas()}, ...].
-type gas_term()  :: gas() | schedule().
-type override()  :: {atom(), schedule()}.

-export_type([ gas/0
             , protocol/0
             , schedule/0
             , gas_term/0
             , override/0
             ]).

%% Empty: nothing is repriced at Arcus. To price OP from protocol P on, add
%% {OP, [{P, Gas}]} - strictly descending, above every protocol the base table
%% already prices for OP, and at or above first_repriceable_protocol/0.
-spec overrides() -> [override()].
overrides() ->
    [].

%% See ?FIRST_REPRICEABLE_PROTOCOL_VSN. Exported so the pin against
%% hard_forks.hrl can live in the test rather than in this module's includes.
-spec first_repriceable_protocol() -> protocol().
first_repriceable_protocol() ->
    ?FIRST_REPRICEABLE_PROTOCOL_VSN.

%% The price aefa_gen_dispatch should emit for OpName, given the price the base
%% table carries for it.
-spec gas_term(atom(), gas_term()) -> gas_term().
gas_term(OpName, BaseGas) ->
    gas_term(OpName, BaseGas, overrides()).

-spec gas_term(atom(), gas_term(), [override()]) -> gas_term().
gas_term(OpName, BaseGas, Overrides) ->
    case lists:keyfind(OpName, 1, Overrides) of
        false ->
            %% Handed back unchanged rather than normalised: with no override
            %% the generated dispatch must be identical to the one built
            %% without this module at all.
            BaseGas;
        {OpName, Repriced} ->
            Base = schedule(BaseGas),
            ok = check_schedule(OpName, Repriced),
            ok = check_forward_only(OpName, Repriced, Base),
            ok = check_not_yet_activated(OpName, Repriced),
            Repriced ++ Base
    end.

%% Normalise a price to schedule form. A bare integer prices every protocol,
%% so it becomes a one-entry schedule marked at the lowest protocol.
-spec schedule(gas_term()) -> schedule().
schedule(Gas) when is_integer(Gas), Gas >= 0 ->
    [{?LOWEST_PROTOCOL_VSN, Gas}];
schedule([{P, G} | _] = Schedule) when is_integer(P), is_integer(G) ->
    Schedule.

check_schedule(_OpName, [{P, G}]) when is_integer(P), P > 0, is_integer(G), G >= 0 ->
    ok;
check_schedule(OpName, [{P1, G1}, {P2, _} = Next | Rest])
  when is_integer(P1), P1 > 0, is_integer(G1), G1 >= 0, is_integer(P2), P2 < P1 ->
    check_schedule(OpName, [Next | Rest]);
check_schedule(OpName, Schedule) ->
    erlang:error({bad_gas_override, OpName, Schedule}).

%% The new prices must all sit above the ones the base table already states for
%% this instruction, so the base schedule survives underneath them intact.
check_forward_only(OpName, Repriced, Base) ->
    LowestNew  = lists:min([P || {P, _} <- Repriced]),
    HighestOld = lists:max([P || {P, _} <- Base]),
    case LowestNew > HighestOld of
        true  -> ok;
        false -> erlang:error({gas_override_not_forward_only, OpName, Repriced, Base})
    end.

%% And they must all name a protocol that has not been given an activation
%% height yet, whatever the base table happens to price.
check_not_yet_activated(OpName, Repriced) ->
    Floor = first_repriceable_protocol(),
    case [P || {P, _} <- Repriced, P < Floor] of
        []       -> ok;
        Activated -> erlang:error({gas_override_below_first_repriceable_protocol,
                                   OpName, Activated, Floor})
    end.
