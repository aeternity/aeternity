%%%-------------------------------------------------------------------
%%% @doc Read-only contract calls via `aec_dry_run:dry_run/4'.
%%%
%%% Powers `ae_call' (returns the raw FATE/AEVM return bytes) and
%%% `ae_estimateGas' (returns the call's `gas_used'). Both methods
%%% share the same dry-run path; the only difference is which field
%%% of the resulting call object they project.
%%%
%%% Inputs follow the eth-shaped tx object:
%%%   * `to'    -- contract id, AE-native `ct_...' or 32-byte `0x...'
%%%   * `from'  -- optional caller; defaults to dry_run's magic high-
%%%                balance account (no nonce / balance constraint applies)
%%%   * `input' -- FATE call-data bytes, `0x...' hex
%%%   * `value' -- optional amount, hex quantity, default 0
%%%   * `gas'   -- optional gas limit, hex quantity, default
%%%                ?DEFAULT_CALL_GAS (matches the existing dry-run REST
%%%                endpoint's default for call requests)
%%%
%%% On revert the error envelope carries `data: <hex>' so the caller
%%% can decode the FATE error message (eth convention; see
%%% `aerpc_jsonrpc:error/4').
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_call).

-include_lib("aecontract/include/aecontract.hrl").

-export([call/2, estimate_gas/2]).

%% Mirrors apps/aehttp/src/aehttp_dispatch_ext.erl's DEFAULT_CALL_REQ_GAS_LIMIT;
%% lifted here to keep aerpc free of aehttp dependencies.
-define(DEFAULT_CALL_GAS, 1000000).

%% Fee field is required by aect_call_tx:new/1 but ignored by dry_run
%% (dry_run skips fee accounting). Mirrors the value the existing
%% REST dry-run endpoint uses for synthesized call txs.
-define(DUMMY_FEE, 1000000 * 1000000).

%% Same for gas_price: any non-zero value works; dry_run won't bill it.
-define(DUMMY_GAS_PRICE, 1000000).

-define(DUMMY_NONCE, 1).

%% ===================================================================
%% Public API
%% ===================================================================

-spec call(map(), binary()) ->
    {ok, binary()}
  | {error, integer(), binary()}
  | {error, integer(), binary(), term()}.
call(TxObj, BlockId) when is_map(TxObj), is_binary(BlockId) ->
    case do_dry_run(TxObj, BlockId) of
        {ok, CallObj}                  -> ok_return_value(CallObj);
        {revert, CallObj}              -> revert_error(CallObj);
        {error, _, _, _} = Err4        -> Err4;
        {error, _, _} = Err3           -> Err3
    end;
call(_TxObj, _BlockId) ->
    {error, -32602, <<"Invalid params">>}.

-spec estimate_gas(map(), binary()) ->
    {ok, binary()}
  | {error, integer(), binary()}
  | {error, integer(), binary(), term()}.
estimate_gas(TxObj, BlockId) when is_map(TxObj), is_binary(BlockId) ->
    case do_dry_run(TxObj, BlockId) of
        {ok, CallObj} ->
            {ok, aerpc_encoding:to_quantity(aect_call:gas_used(CallObj))};
        {revert, CallObj} ->
            revert_error(CallObj);
        {error, _, _, _} = Err4 -> Err4;
        {error, _, _} = Err3    -> Err3
    end;
estimate_gas(_TxObj, _BlockId) ->
    {error, -32602, <<"Invalid params">>}.

%% ===================================================================
%% Internal
%% ===================================================================

do_dry_run(TxObj, BlockId) ->
    with_required_contract(TxObj, fun(ContractPK, ABI) ->
        with_caller(TxObj, fun(CallerPK) ->
            with_input(TxObj, fun(CallData) ->
                with_top(BlockId, fun(Top) ->
                    Amount = aerpc_encoding:from_optional_quantity(
                                 maps:get(<<"value">>, TxObj, undefined), 0),
                    Gas    = aerpc_encoding:from_optional_quantity(
                                 maps:get(<<"gas">>,   TxObj, undefined),
                                 ?DEFAULT_CALL_GAS),
                    build_and_run(Top, CallerPK, ContractPK, ABI,
                                  CallData, Amount, Gas)
                end)
            end)
        end)
    end).

with_required_contract(TxObj, K) ->
    case maps:get(<<"to">>, TxObj, undefined) of
        undefined ->
            {error, -32602, <<"Missing required 'to' field">>};
        ToBin when is_binary(ToBin) ->
            case aerpc_account:decode_address(ToBin) of
                {ok, ContractPK} ->
                    case aec_chain:get_contract(ContractPK) of
                        {ok, Contract} ->
                            ABI = aect_contracts:abi_version(Contract),
                            K(ContractPK, ABI);
                        {error, _Reason} ->
                            %% No contract at that address. Eth returns
                            %% empty data ("0x") for calls to non-contract
                            %% addresses; mirror that.
                            {ok, no_contract}
                    end;
                {error, _, _} = Err -> Err
            end;
        _ -> {error, -32602, <<"Invalid 'to' field">>}
    end.

with_caller(TxObj, K) ->
    case maps:get(<<"from">>, TxObj, undefined) of
        undefined ->
            %% Use dry_run's magic high-balance account. Defined locally
            %% as a 32-byte binary that dry_run/4 seeds with a huge
            %% balance; mirrors aec_dry_run:?MR_MAGIC.
            K(<<1:32/unit:8>>);
        FromBin when is_binary(FromBin) ->
            case aerpc_account:decode_address(FromBin) of
                {ok, CallerPK}       -> K(CallerPK);
                {error, _, _} = Err  -> Err
            end;
        _ -> {error, -32602, <<"Invalid 'from' field">>}
    end.

with_input(TxObj, K) ->
    case maps:get(<<"input">>, TxObj, maps:get(<<"data">>, TxObj, undefined)) of
        undefined ->
            %% No call data => empty bytes; FATE rejects this for most
            %% entrypoints, but the dispatcher should not. Let dry_run
            %% fail with a meaningful error in that case.
            K(<<>>);
        Hex when is_binary(Hex) ->
            try
                K(aerpc_encoding:from_hex_data(Hex))
            catch _:_ ->
                {error, -32602, <<"Invalid 'input' field (expected 0x hex)">>}
            end;
        _ -> {error, -32602, <<"Invalid 'input' field">>}
    end.

with_top(BlockId, K) ->
    case aerpc_block:resolve_dry_run_top(BlockId) of
        {ok, Top}            -> K(Top);
        {error, _, _} = Err  -> Err
    end.

build_and_run(_Top, _Caller, no_contract, _ABI, _CallData, _Amt, _Gas) ->
    %% Address has no contract code. Eth returns "0x" for such calls.
    %% We surface that as an `ok' tuple carrying empty bytes so the
    %% public-API `call/2' projects it correctly while `estimate_gas/2'
    %% emits "0x0".
    {ok, no_contract};
build_and_run(Top, CallerPK, ContractPK, ABI, CallData, Amount, Gas) ->
    case build_call_tx(CallerPK, ContractPK, ABI, CallData, Amount, Gas) of
        {ok, Tx} ->
            run_and_extract(Top, Tx);
        {error, Reason} ->
            {error, -32602,
             iolist_to_binary(io_lib:format("Failed to build call tx: ~p",
                                            [Reason]))}
    end.

build_call_tx(CallerPK, ContractPK, ABI, CallData, Amount, Gas) ->
    aect_call_tx:new(#{
        caller_id   => aeser_id:create(account,  CallerPK),
        nonce       => ?DUMMY_NONCE,
        contract_id => aeser_id:create(contract, ContractPK),
        abi_version => ABI,
        fee         => ?DUMMY_FEE,
        amount      => Amount,
        gas         => Gas,
        gas_price   => ?DUMMY_GAS_PRICE,
        call_data   => CallData
    }).

run_and_extract(Top, Tx) ->
    %% dry_run/4 accepts `[{tx, Tx}]` and prepares a dummy signature
    %% internally. We pass no extra accounts; the magic caller is added
    %% by dry_run's `setup_dry_run/2'.
    case aec_dry_run:dry_run(Top, [], [{tx, Tx}], [{tx_events, false}]) of
        {ok, {[{contract_call_tx, {ok, CallObj}}], _Events}} ->
            case aect_call:return_type(CallObj) of
                ok    -> {ok, CallObj};
                _Other -> {revert, CallObj}
            end;
        {ok, {[{contract_call_tx, {error, Reason}}], _Events}} ->
            {error, -32603, internal_msg(Reason)};
        {error, Reason} ->
            {error, -32603, internal_msg(Reason)}
    end.

ok_return_value(no_contract) ->
    {ok, <<"0x">>};
ok_return_value(CallObj) ->
    {ok, aerpc_encoding:to_hex_data(aect_call:return_value(CallObj))}.

revert_error(CallObj) ->
    Type = aect_call:return_type(CallObj),
    Data = aerpc_encoding:to_hex_data(aect_call:return_value(CallObj)),
    Msg  = case Type of
               revert -> <<"execution reverted">>;
               error  -> <<"execution error">>;
               _Other -> <<"call failed">>
           end,
    {error, -32003, Msg, Data}.

internal_msg(Bin) when is_binary(Bin) ->
    Bin;
internal_msg(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).
