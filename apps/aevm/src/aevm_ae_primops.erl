%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Handle interaction with the aeternity chain
%%%     through calls to AEternity primitive operations at address 0.
%%% @end
%%% Created : 22 May 2018
%%%-------------------------------------------------------------------

-module(aevm_ae_primops).
-export([call/3]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").

-define(BASE_ADDRESS, 64). %% Byte offset for data
-spec call( non_neg_integer(), binary(), aevm_eeevm_state:state()) ->
                  {ok, binary(), non_neg_integer(), aevm_eeevm_state:state()}
                      | {error, any()}.
call(Value, Data, State) ->
    %% TODO: use aeso_data:from_binary() (but it doesn't take a base address at the moment)
    case Data of
        <<?BASE_ADDRESS:256, ?PRIM_CALL_SPEND:256, Recipient:256>> ->
            spend(Recipient, Value, State);
        _ ->
            %% Throw out of gas for illegal call
            %% TODO: Better error for illegal call.
            {error, out_of_gas}
    end.


%% ------------------------------------------------------------------
%% Basic account operations.
%% ------------------------------------------------------------------


spend(Recipient, Value, State) ->
    ChainAPI   = aevm_eeevm_state:chain_api(State),
    ChainState = aevm_eeevm_state:chain_state(State),

    case ChainAPI:spend(<<Recipient:256>>, Value, ChainState) of
        {ok, ChainState1} ->
            UnitReturn = {ok, <<0:256>>}, %% spend returns unit
            GasSpent   = 0,         %% Already costs lots of gas
            {ok, UnitReturn, GasSpent,
             aevm_eeevm_state:set_chain_state(ChainState1, State)};
        {error, _} = Err -> Err
    end.

