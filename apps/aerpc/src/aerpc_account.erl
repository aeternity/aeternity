%%%-------------------------------------------------------------------
%%% @doc Account / state lookups for the AE JSON-RPC layer.
%%%
%%% This module is the single choke-point for address decoding: any
%%% method that takes an address as input routes through
%%% `decode_address/1', which accepts all four forms a caller can
%%% plausibly send -- `ak_...', `ct_...', 32-byte `0x' hex, and the
%%% 20-byte `0x' hex this layer itself emits.
%%%
%%% The 20-byte form resolves through `aerpc_addr_index'. Its three-way
%%% answer is why `decode_address/1' has three return shapes rather than
%%% two: `{unknown, Addr20}' means the index is complete and no such
%%% account exists, which is where eth's zero/empty semantics apply, and
%%% is deliberately distinct from the error returned while the index is
%%% still building. Collapsing those two would mean reporting balance
%%% zero for an account we simply have not indexed yet.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_account).

-export([
          balance/2
        , code/1
        , tx_count/2
        , decode_address/1
        ]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec balance(binary(), binary() | map()) ->
    {ok, binary()} | {error, integer(), binary()}.
balance(AddrIn, BlockId)
  when is_binary(AddrIn), (is_binary(BlockId) orelse is_map(BlockId)) ->
    with_account(AddrIn, BlockId,
        fun(Account) ->
            {ok, aerpc_encoding:to_quantity(aec_accounts:balance(Account))}
        end,
        <<"0x0">>);
balance(_AddrIn, _Tag) ->
    {error, -32602, <<"Invalid params">>}.

-spec code(binary()) ->
    {ok, binary()} | {error, integer(), binary()}.
code(AddrIn) when is_binary(AddrIn) ->
    case decode_address(AddrIn) of
        {ok, Pubkey} ->
            case aec_chain:get_contract_with_code(Pubkey) of
                {ok, _Contract, Bytecode} ->
                    %% Returns FATE (or legacy AEVM) bytecode -- documented
                    %% divergence from Eth's EVM bytecode.
                    {ok, aerpc_encoding:to_hex_data(Bytecode)};
                {error, _Reason} ->
                    {ok, <<"0x">>}
            end;
        {unknown, _Addr20} ->
            %% Index complete and nothing maps to it: no contract there,
            %% which is eth's empty-code answer.
            {ok, <<"0x">>};
        {error, _, _} = Err ->
            Err
    end;
code(_AddrIn) ->
    {error, -32602, <<"Invalid params">>}.

-spec tx_count(binary(), binary() | map()) ->
    {ok, binary()} | {error, integer(), binary()}.
tx_count(AddrIn, <<"latest">>) ->
    next_nonce(AddrIn);
tx_count(AddrIn, <<"pending">>) ->
    next_nonce(AddrIn);
tx_count(AddrIn, BlockId)
  when is_binary(AddrIn), (is_binary(BlockId) orelse is_map(BlockId)) ->
    %% At historical heights AE's account.nonce is the on-chain nonce of
    %% the last included tx, which already matches eth's "count of mined
    %% txs" semantics -- no -1 needed.
    with_account(AddrIn, BlockId,
        fun(Account) ->
            {ok, aerpc_encoding:to_quantity(aec_accounts:nonce(Account))}
        end,
        <<"0x0">>);
tx_count(_AddrIn, _Tag) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Decode an address from any accepted form. Supports:
%%   * `0x' + 40 hex chars -- the eth-shaped 20-byte address this layer
%%     emits; resolved back to a pubkey via `aerpc_addr_index'
%%   * `0x' + 64 hex chars -- raw 32-byte pubkey (lossless wide form)
%%   * `ak_...' -- AE account pubkey
%%   * `ct_...' -- AE contract pubkey
-spec decode_address(binary()) ->
    {ok, binary()} | {unknown, binary()} | {error, integer(), binary()}.
decode_address(<<"ak_", _/binary>> = Encoded) ->
    case aeapi:decode_account_pubkey(Encoded) of
        {ok, Bin} -> {ok, Bin};
        _Error    -> {error, -32602, <<"Invalid address">>}
    end;
decode_address(<<"ct_", _/binary>> = Encoded) ->
    case aeapi:decode_contract_pubkey(Encoded) of
        {ok, Bin} -> {ok, Bin};
        _Error    -> {error, -32602, <<"Invalid address">>}
    end;
decode_address(<<"0x", _/binary>> = Hex) ->
    try aerpc_encoding:from_hex_data(Hex) of
        <<Pubkey:32/binary>> ->
            {ok, Pubkey};
        <<Addr20:20/binary>> ->
            resolve20(Addr20);
        _Other ->
            {error, -32602, <<"Invalid address">>}
    catch _:_ -> {error, -32602, <<"Invalid address">>}
    end;
decode_address(_) ->
    {error, -32602, <<"Invalid address">>}.

resolve20(Addr20) ->
    case aerpc_addr_index:resolve(Addr20) of
        {ok, Pubkey} -> {ok, Pubkey};
        unknown      -> {unknown, Addr20};
        incomplete   -> aerpc_errors:address_index_not_ready()
    end.

%% ===================================================================
%% Internal
%% ===================================================================

next_nonce(AddrIn) ->
    case decode_address(AddrIn) of
        {ok, Pubkey} ->
            case aec_next_nonce:pick_for_account(Pubkey) of
                {ok, NextNonce} ->
                    %% AE nonces are 1-based; Eth tx count is the
                    %% nonce-of-the-last-included tx (zero for an account
                    %% with no txs). Drop one to align.
                    {ok, aerpc_encoding:to_quantity(max(0, NextNonce - 1))};
                {error, _Reason} ->
                    {ok, <<"0x0">>}
            end;
        {unknown, _Addr20} ->
            {ok, <<"0x0">>};
        {error, _, _} = Err ->
            Err
    end.

with_account(AddrIn, BlockId, OnAccount, DefaultIfMissing) ->
    case decode_address(AddrIn) of
        {ok, Pubkey} ->
            case lookup_account(Pubkey, BlockId) of
                {value, Account}      -> OnAccount(Account);
                none                  -> {ok, DefaultIfMissing};
                {error, _Reason}      -> {ok, DefaultIfMissing};
                {error, _, _} = Err   -> Err   %% EIP-1898 -39001 etc.
            end;
        {unknown, _Addr20} ->
            %% Backfill is complete and no account derives this address,
            %% so eth's "unknown address" default is the right answer.
            %% While the backfill is still running decode_address/1
            %% returns -32007 instead and never reaches here.
            {ok, DefaultIfMissing};
        {error, _, _} = Err ->
            Err
    end.

lookup_account(Pubkey, <<"latest">>) ->
    aec_chain:get_account(Pubkey);
lookup_account(Pubkey, <<"pending">>) ->
    aec_chain:get_account(Pubkey);
lookup_account(Pubkey, BlockId) ->
    case aerpc_block:resolve_id(BlockId) of
        {ok, Height}             -> aec_chain:get_account_at_height(Pubkey, Height);
        {error, _, _} = Err      -> Err
    end.
