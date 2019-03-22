%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Module defining the Attach transaction for Generalized Accounts
%%% @end
%%%=============================================================================
-module(aega_attach_tx).

-behavior(aetx).

-include_lib("apps/aecontract/src/aecontract.hrl").

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Additional getters
-export([auth_fun/1,
         owner_id/1,
         owner_pubkey/1,
         code/1,
         contract_pubkey/1,
         abi_version/1,
         ct_version/1,
         vm_version/1,
         gas_limit/1,
         gas_price/1,
         call_data/1,
         call_id/1,
         is_legal_contract_version/1
        ]).

-define(GA_ATTACH_TX_VSN, 1).
-define(GA_ATTACH_TX_TYPE, ga_attach_tx).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-record(ga_attach_tx, {
          owner_id    :: aeser_id:id(),
          nonce       :: non_neg_integer(),
          code        :: binary(),
          auth_fun    :: binary(),
          call_data   :: binary(),
          ct_version  :: aect_contracts:version(),
          fee         :: aect_contracts:amount(),
          gas         :: aect_contracts:amount(),
          gas_price   :: aect_contracts:amount(),
          ttl         :: aetx:tx_ttl()
        }).

-type amount() :: aect_contracts:amount().

-opaque tx() :: #ga_attach_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Getters

-spec auth_fun(tx()) -> binary().
auth_fun(#ga_attach_tx{auth_fun = AuthFun}) ->
    AuthFun.

-spec owner_id(tx()) -> aeser_id:id().
owner_id(#ga_attach_tx{owner_id = OwnerId}) ->
    OwnerId.

-spec owner_pubkey(tx()) -> aec_keys:pubkey().
owner_pubkey(#ga_attach_tx{owner_id = OwnerId}) ->
    aeser_id:specialize(OwnerId, account).

-spec code(tx()) -> binary().
code(#ga_attach_tx{code = X}) ->
    X.

-spec contract_pubkey(tx()) -> aec_keys:pubkey().
contract_pubkey(#ga_attach_tx{} = Tx) ->
    aect_contracts:compute_contract_pubkey(owner_pubkey(Tx), nonce(Tx)).

-spec ct_version(tx()) -> aect_contracts:version().
ct_version(#ga_attach_tx{ct_version = X}) ->
    X.

-spec abi_version(tx()) -> aect_contracts:abi_version().
abi_version(#ga_attach_tx{ct_version = #{abi := X}}) ->
    X.

-spec vm_version(tx()) -> aect_contracts:vm_version().
vm_version(#ga_attach_tx{ct_version = #{vm := X}}) ->
    X.

-spec gas(tx()) -> amount().
gas(#ga_attach_tx{} = Tx) ->
    gas_limit(Tx).

-spec gas_limit(tx()) -> amount().
gas_limit(#ga_attach_tx{gas = X}) ->
    X.

-spec gas_price(tx()) -> amount().
gas_price(#ga_attach_tx{gas_price = X}) ->
    X.

-spec call_data(tx()) -> binary().
call_data(#ga_attach_tx{call_data = X}) ->
    X.

-spec call_id(tx()) -> aect_call:id().
call_id(#ga_attach_tx{} = Tx) ->
    aect_call:id(owner_pubkey(Tx), nonce(Tx), contract_pubkey(Tx)).

-spec is_legal_contract_version(aect_contracts:version()) -> boolean().
is_legal_contract_version(#{vm := VMVersion}) when ?IS_VM_SOPHIA(VMVersion) ->
    VMVersion >= ?VM_AEVM_SOPHIA_2;
is_legal_contract_version(#{}) ->
    false.

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#ga_attach_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ga_attach_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{owner_id    := OwnerId,
      nonce       := Nonce,
      code        := Code,
      auth_fun    := AuthFun,
      vm_version  := VMVersion,
      abi_version := ABIVersion,
      gas         := Gas,
      gas_price   := GasPrice,
      call_data   := CallData,
      fee         := Fee} = Args) ->
    account = aeser_id:specialize_type(OwnerId),
    Tx = #ga_attach_tx{owner_id    = OwnerId,
                             nonce       = Nonce,
                             code        = Code,
                             auth_fun    = AuthFun,
                             ct_version  = #{vm => VMVersion, abi => ABIVersion},
                             gas         = Gas,
                             gas_price   = GasPrice,
                             call_data   = CallData,
                             fee         = Fee,
                             ttl         = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?GA_ATTACH_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ga_attach_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ga_attach_tx{} = Tx) ->
    owner_pubkey(Tx).

%% Owner should exist, and have enough funds for the fee, the amount
%% the deposit and the gas
-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ga_attach_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ga_attach_tx{} = Tx, _) ->
    {ok, [owner_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#ga_attach_tx{} = Tx, Trees, Env) ->
    Instructions =
        aec_tx_processor:ga_attach_tx_instructions(
          owner_pubkey(Tx),
          gas(Tx),
          gas_price(Tx),
          abi_version(Tx),
          vm_version(Tx),
          code(Tx),
          auth_fun(Tx),
          call_data(Tx),
          fee(Tx),
          nonce(Tx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

serialize(#ga_attach_tx{owner_id   = OwnerId,
                        nonce      = Nonce,
                        code       = Code,
                        auth_fun   = AuthFun,
                        ct_version = CTVersion,
                        fee        = Fee,
                        ttl        = TTL,
                        gas        = Gas,
                        gas_price  = GasPrice,
                        call_data  = CallData}) ->
    {version(),
     [ {owner_id, OwnerId}
     , {nonce, Nonce}
     , {code, Code}
     , {auth_fun, AuthFun}
     , {ct_version, aect_contracts:pack_vm_abi(CTVersion)}
     , {fee, Fee}
     , {ttl, TTL}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?GA_ATTACH_TX_VSN,
            [ {owner_id, OwnerId}
            , {nonce, Nonce}
            , {code, Code}
            , {auth_fun, AuthFun}
            , {ct_version, CTVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    account = aeser_id:specialize_type(OwnerId),
    #ga_attach_tx{owner_id   = OwnerId,
                  nonce      = Nonce,
                  code       = Code,
                  auth_fun   = AuthFun,
                  ct_version = aect_contracts:split_vm_abi(CTVersion),
                  fee        = Fee,
                  ttl        = TTL,
                  gas        = Gas,
                  gas_price  = GasPrice,
                  call_data  = CallData}.

serialization_template(?GA_ATTACH_TX_VSN) ->
    [ {owner_id, id}
    , {nonce, int}
    , {code, binary}
    , {auth_fun, binary}
    , {ct_version, int}
    , {fee, int}
    , {ttl, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

for_client(#ga_attach_tx{ owner_id    = OwnerId,
                          nonce       = Nonce,
                          code        = Code,
                          auth_fun    = AuthFun,
                          ct_version  = CTVersion,
                          fee         = Fee,
                          ttl         = TTL,
                          gas         = Gas,
                          gas_price   = GasPrice,
                          call_data   = CallData}) ->
    #{<<"owner_id">>    => aeser_api_encoder:encode(id_hash, OwnerId),
      <<"nonce">>       => Nonce,
      <<"code">>        => aeser_api_encoder:encode(contract_bytearray, Code),
      <<"auth_fun">>    => aeu_hex:hexstring_encode(AuthFun),
      <<"vm_version">>  => aeu_hex:hexstring_encode(<<(maps:get(vm, CTVersion)):16>>),
      <<"abi_version">> => aeu_hex:hexstring_encode(<<(maps:get(abi, CTVersion)):16>>),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aeser_api_encoder:encode(contract_bytearray, CallData)}.

%%%===================================================================
%%% Internal functions

-spec version() -> non_neg_integer().
version() ->
    ?GA_ATTACH_TX_VSN.

