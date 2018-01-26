%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract call transaction
%%% @end
%%%=============================================================================
-module(aect_call_tx).

-include("contract_txs.hrl").
-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/trees.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/1,
         serialize/1,
         deserialize/1,
         type/0,
         for_client/1
        ]).

%% Additional getters
-export([contract/1,
         sender/1]).


-define(CONTRACT_CALL_TX_TYPE, <<"contract_call">>).
-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_FEE, 2).

-opaque call_tx() :: #contract_call_tx{}.

-export_type([call_tx/0]).

-spec sender(call_tx()) -> pubkey().
sender(#contract_call_tx{sender = SenderPubKey}) ->
    SenderPubKey.

-spec contract(call_tx()) -> pubkey().
contract(#contract_call_tx{contract = ContractPubKey}) ->
    ContractPubKey.

-spec new(map()) -> {ok, call_tx()}.
new(#{sender   := SenderPubKey,
      nonce    := Nonce,
      contract := Contract,
      fee      := Fee}) ->
    {ok, #contract_call_tx{sender   = SenderPubKey,
                           nonce    = Nonce,
                           contract = Contract,
                           fee      = Fee}}.

-spec fee(call_tx()) -> integer().
fee(#contract_call_tx{fee = F}) ->
    F.

-spec nonce(call_tx()) -> non_neg_integer().
nonce(#contract_call_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(call_tx()) -> pubkey().
origin(#contract_call_tx{sender = SenderPubKey}) ->
    SenderPubKey.

%% SenderAccount should exist, and have enough funds for the fee + the call_fee.
%% Contract should exist, and call_fee should be enough
%% Fee should cover TTL
-spec check(call_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#contract_call_tx{sender = SenderPubKey, nonce = Nonce,
                        contract = ContractPubKey,
                        fee = Fee} = CallTx, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(SenderPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_contract(ContractPubKey, Trees, Height) end,
         fun() -> check_call(CallTx, Trees, Height) end
        ],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(call_tx()) -> [pubkey()].
signers(#contract_call_tx{sender = SenderPubKey}) ->
    [SenderPubKey].

-spec process(call_tx(), trees(), height()) -> {ok, trees()}.
process(#contract_call_tx{sender = _SenderPubKey, nonce = _Nonce, fee = _Fee
                         } = _Query, Trees, _Height) ->
    %% PLACEHOLDER
    {ok, Trees}.

serialize(#contract_call_tx{sender   = SenderPubKey,
                            nonce    = Nonce,
                            contract = ContractPubKey,
                            fee      = Fee}) ->
    [#{<<"type">>     => type()},
     #{<<"vsn">>      => version()},
     #{<<"sender">>   => SenderPubKey},
     #{<<"nonce">>    => Nonce},
     #{<<"contract">> => ContractPubKey},
     #{<<"fee">>      => Fee}].

deserialize([#{<<"type">>     := ?CONTRACT_CALL_TX_TYPE},
             #{<<"vsn">>      := ?CONTRACT_CALL_TX_VSN},
             #{<<"sender">>   := SenderPubKey},
             #{<<"nonce">>    := Nonce},
             #{<<"contract">> := ContractPubKey},
             #{<<"fee">>      := Fee}]) ->
    #contract_call_tx{sender   = SenderPubKey,
                      nonce    = Nonce,
                      contract = ContractPubKey,
                      fee      = Fee}.

-spec type() -> binary().
type() ->
    ?CONTRACT_CALL_TX_TYPE.

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CALL_TX_VSN.

for_client(#contract_call_tx{sender   = SenderPubKey,
                             nonce    = Nonce,
                             contract = ContractPubKey,
                             fee      = Fee}) ->
    #{<<"type">>     => <<"ContractQueryTxObject">>, % swagger schema name
      <<"vsn">>      => version(),
      <<"sender">>   => aec_base58c:encode(account_pubkey, SenderPubKey),
      <<"nonce">>    => Nonce,
      <<"contract">> => aec_base58c:encode(account_pubkey, ContractPubKey), %% TODO: different tag?
      <<"fee">>      => Fee}.

%% -- Local functions  -------------------------------------------------------

check_call(_CallTx, _Trees, _Height) ->
    %% PLACEHOLDER
    ok.

check_contract(_ContractPubKey, _Trees, _Height) ->
    %% PLACEHOLDER
    ok.

