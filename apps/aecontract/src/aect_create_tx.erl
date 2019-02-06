%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract register transaction
%%% @end
%%%=============================================================================
-module(aect_create_tx).

-include("aecontract.hrl").

-behavior(aetx).

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
-export([owner_id/1,
         owner_pubkey/1,
         code/1,
         contract_pubkey/1,
         vm_version/1,
         deposit/1,
         amount/1,
         gas_limit/1,
         gas_price/1,
         call_data/1,
         call_id/1
        ]).

-define(CONTRACT_CREATE_TX_VSN, 1).
-define(CONTRACT_CREATE_TX_TYPE, contract_create_tx).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-record(contract_create_tx, {
          owner_id   :: aec_id:id(),
          nonce      :: non_neg_integer(),
          code       :: binary(),
          vm_version :: aect_contracts:vm_version(),
          fee        :: aect_contracts:amount(),
          deposit    :: aect_contracts:amount(),
          amount     :: aect_contracts:amount(),
          gas        :: aect_contracts:amount(),
          gas_price  :: aect_contracts:amount(),
          call_data  :: binary(),
          ttl        :: aetx:tx_ttl()
        }).

-type amount() :: aect_contracts:amount().

-opaque tx() :: #contract_create_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Getters

-spec owner_id(tx()) -> aec_id:id().
owner_id(#contract_create_tx{owner_id = OwnerId}) ->
    OwnerId.

-spec owner_pubkey(tx()) -> aec_keys:pubkey().
owner_pubkey(#contract_create_tx{owner_id = OwnerId}) ->
    aec_id:specialize(OwnerId, account).

-spec code(tx()) -> binary().
code(#contract_create_tx{code = X}) ->
    X.

-spec contract_pubkey(tx()) -> aec_keys:pubkey().
contract_pubkey(#contract_create_tx{} = Tx) ->
    aect_contracts:compute_contract_pubkey(owner_pubkey(Tx), nonce(Tx)).

-spec vm_version(tx()) -> aect_contracts:vm_version().
vm_version(#contract_create_tx{vm_version = X}) ->
    X.

-spec deposit(tx()) -> amount().
deposit(#contract_create_tx{deposit = X}) ->
    X.

-spec amount(tx()) -> amount().
amount(#contract_create_tx{amount = X}) ->
    X.

-spec gas(tx()) -> amount().
gas(#contract_create_tx{} = Tx) ->
    gas_limit(Tx).

-spec gas_limit(tx()) -> amount().
gas_limit(#contract_create_tx{gas = X}) ->
    X.

-spec gas_price(tx()) -> amount().
gas_price(#contract_create_tx{gas_price = X}) ->
    X.

-spec call_data(tx()) -> binary().
call_data(#contract_create_tx{call_data = X}) ->
    X.

-spec call_id(tx()) -> aect_call:id().
call_id(#contract_create_tx{} = Tx) ->
    aect_call:id(owner_pubkey(Tx), nonce(Tx), contract_pubkey(Tx)).

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#contract_create_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#contract_create_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{owner_id   := OwnerId,
      nonce      := Nonce,
      code       := Code,
      vm_version := VmVersion,
      deposit    := Deposit,
      amount     := Amount,
      gas        := Gas,
      gas_price  := GasPrice,
      call_data  := CallData,
      fee        := Fee} = Args) ->
    account = aec_id:specialize_type(OwnerId),
    Tx = #contract_create_tx{owner_id   = OwnerId,
                             nonce      = Nonce,
                             code       = Code,
                             vm_version = VmVersion,
                             deposit    = Deposit,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData,
                             fee        = Fee,
                             ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?CONTRACT_CREATE_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#contract_create_tx{} = Tx) ->
    owner_pubkey(Tx).

%% Owner should exist, and have enough funds for the fee, the amount
%% the deposit and the gas
-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#contract_create_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#contract_create_tx{} = Tx, _) ->
    {ok, [owner_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#contract_create_tx{} = Tx, Trees, Env) ->
    Instructions =
        aec_tx_processor:contract_create_tx_instructions(
          owner_pubkey(Tx),
          amount(Tx),
          deposit(Tx),
          gas(Tx),
          gas_price(Tx),
          vm_version(Tx),
          code(Tx),
          call_data(Tx),
          fee(Tx),
          nonce(Tx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

serialize(#contract_create_tx{owner_id   = OwnerId,
                              nonce      = Nonce,
                              code       = Code,
                              vm_version = VmVersion,
                              fee        = Fee,
                              ttl        = TTL,
                              deposit    = Deposit,
                              amount     = Amount,
                              gas        = Gas,
                              gas_price  = GasPrice,
                              call_data  = CallData}) ->
    {version(),
     [ {owner_id, OwnerId}
     , {nonce, Nonce}
     , {code, Code}
     , {vm_version, VmVersion}
     , {fee, Fee}
     , {ttl, TTL}
     , {deposit, Deposit}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CREATE_TX_VSN,
            [ {owner_id, OwnerId}
            , {nonce, Nonce}
            , {code, Code}
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {deposit, Deposit}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    account = aec_id:specialize_type(OwnerId),
    #contract_create_tx{owner_id   = OwnerId,
                        nonce      = Nonce,
                        code       = Code,
                        vm_version = VmVersion,
                        fee        = Fee,
                        ttl        = TTL,
                        deposit    = Deposit,
                        amount     = Amount,
                        gas        = Gas,
                        gas_price  = GasPrice,
                        call_data  = CallData}.

serialization_template(?CONTRACT_CREATE_TX_VSN) ->
    [ {owner_id, id}
    , {nonce, int}
    , {code, binary}
    , {vm_version, int}
    , {fee, int}
    , {ttl, int}
    , {deposit, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

for_client(#contract_create_tx{ owner_id   = OwnerId,
                                nonce      = Nonce,
                                code       = Code,
                                vm_version = VmVersion,
                                fee        = Fee,
                                ttl        = TTL,
                                deposit    = Deposit,
                                amount     = Amount,
                                gas        = Gas,
                                gas_price  = GasPrice,
                                call_data  = CallData}) ->
    #{<<"owner_id">>    => aehttp_api_encoder:encode(id_hash, OwnerId),
      <<"nonce">>       => Nonce,
      <<"code">>        => aehttp_api_encoder:encode(contract_bytearray, Code),
      <<"vm_version">>  => aeu_hex:hexstring_encode(<<VmVersion:8>>),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"deposit">>     => Deposit,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aehttp_api_encoder:encode(contract_bytearray, CallData)}.

%%%===================================================================
%%% Internal functions

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CREATE_TX_VSN.

