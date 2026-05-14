%%%-------------------------------------------------------------------
%%% @doc Account / state lookups for the AE JSON-RPC layer.
%%%
%%% This module is the single choke-point for address decoding: any
%%% method that takes an address as input routes through
%%% `decode_address/1', which accepts the canonical AE forms
%%% (`ak_...', `ct_...') as well as `0x'-prefixed 32-byte hex.
%%%
%%% Per project policy this layer never re-derives an Eth 20-byte
%%% address; pubkeys are emitted in their native AE form by callers
%%% that need them.
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

-spec balance(binary(), binary()) ->
    {ok, binary()} | {error, integer(), binary()}.
balance(AddrIn, TagOrHex) when is_binary(AddrIn), is_binary(TagOrHex) ->
    with_account(AddrIn, TagOrHex,
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
        {error, _, _} = Err ->
            Err
    end;
code(_AddrIn) ->
    {error, -32602, <<"Invalid params">>}.

-spec tx_count(binary(), binary()) ->
    {ok, binary()} | {error, integer(), binary()}.
tx_count(AddrIn, <<"latest">>) ->
    next_nonce(AddrIn);
tx_count(AddrIn, <<"pending">>) ->
    next_nonce(AddrIn);
tx_count(AddrIn, TagOrHex) when is_binary(AddrIn), is_binary(TagOrHex) ->
    %% At historical heights AE's account.nonce is the on-chain nonce of
    %% the last included tx, which already matches eth's "count of mined
    %% txs" semantics -- no -1 needed.
    with_account(AddrIn, TagOrHex,
        fun(Account) ->
            {ok, aerpc_encoding:to_quantity(aec_accounts:nonce(Account))}
        end,
        <<"0x0">>);
tx_count(_AddrIn, _Tag) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Decode an address from any accepted form. Supports:
%%   * `ak_...' -- AE account pubkey
%%   * `ct_...' -- AE contract pubkey
%%   * `0x' + 64 hex chars -- raw 32-byte hex (lossless wide form)
-spec decode_address(binary()) ->
    {ok, binary()} | {error, integer(), binary()}.
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
    try
        Bin = aerpc_encoding:from_hex_data(Hex),
        case byte_size(Bin) of
            32 -> {ok, Bin};
            _  -> {error, -32602, <<"Invalid address">>}
        end
    catch _:_ -> {error, -32602, <<"Invalid address">>}
    end;
decode_address(_) ->
    {error, -32602, <<"Invalid address">>}.

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
        {error, _, _} = Err ->
            Err
    end.

with_account(AddrIn, TagOrHex, OnAccount, DefaultIfMissing) ->
    case decode_address(AddrIn) of
        {ok, Pubkey} ->
            case lookup_account(Pubkey, TagOrHex) of
                {value, Account}      -> OnAccount(Account);
                none                  -> {ok, DefaultIfMissing};
                {error, _Reason}      -> {ok, DefaultIfMissing}
            end;
        {error, _, _} = Err ->
            Err
    end.

lookup_account(Pubkey, <<"latest">>) ->
    aec_chain:get_account(Pubkey);
lookup_account(Pubkey, <<"pending">>) ->
    aec_chain:get_account(Pubkey);
lookup_account(Pubkey, TagOrHex) ->
    case aerpc_block:resolve_tag(TagOrHex) of
        {ok, Height} -> aec_chain:get_account_at_height(Pubkey, Height);
        _Other       -> {error, bad_tag}
    end.
