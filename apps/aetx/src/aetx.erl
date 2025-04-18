%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
%%% @doc
%%% ADT containing all different transactions
%%% @end
%%%-------------------------------------------------------------------

-module(aetx).

-export([ accounts/1
        , deep_fee/1
        , deep_fee/2
        , deserialize_from_binary/1
        , fee/1
        , from_db_format/1
        , gas_limit/3
        , used_gas/4
        , inner_gas_limit/3
        , fee_gas/3
        , gas_price/1
        , min_gas_price/3
        , ttl/1
        , size/1
        , min_fee/3
        , new/2
        , nonce/1
        , origin/1
        , process/3
        , custom_apply/4
        , serialize_for_client/1
        , serialize_to_binary/1
        , signers/2
        , specialize_type/1
        , specialize_callback/1
        , update_tx/2
        , valid_at_protocol/2
        , check_protocol/2
        , swagger_name_to_type/1
        , type_to_swagger_name/1
        , tx_min_gas/2
        , tx_type/1
        ]).

-ifdef(TEST).
-export([tx/1]).
-endif.

-define(IS_CONTRACT_TX(T), ((T =:= contract_create_tx) or (T =:= contract_call_tx)
                            or (T =:= ga_meta_tx) or (T =:= ga_attach_tx))).
-define(HAS_GAS_TX(T), (?IS_CONTRACT_TX(T) or (T =:= channel_force_progress_tx))).

-include_lib("aecontract/include/hard_forks.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-record(aetx, { type :: tx_type()
              , cb   :: module()
              , size :: non_neg_integer() %% 0 needed for ga inner tx
              , tx   :: tx_instance() }).

-opaque tx() :: #aetx{}.

-type tx_type() :: spend_tx
                 | oracle_register_tx
                 | oracle_extend_tx
                 | oracle_query_tx
                 | oracle_response_tx
                 | name_preclaim_tx
                 | name_claim_tx
                 | name_transfer_tx
                 | name_update_tx
                 | name_revoke_tx
                 | contract_create_tx
                 | contract_call_tx
                 | ga_attach_tx
                 | ga_meta_tx
                 | channel_create_tx
                 | channel_deposit_tx
                 | channel_withdraw_tx
                 | channel_force_progress_tx
                 | channel_close_mutual_tx
                 | channel_close_solo_tx
                 | channel_slash_tx
                 | channel_settle_tx
                 | channel_snapshot_solo_tx
                 | channel_set_delegates_tx
                 | channel_offchain_tx
                 | channel_client_reconnect_tx
                 | paying_for_tx
                 | hc_vote_tx.

%% dialyzer chokes on the following type - 20+ opaque types in a
%% union is too much apparently :-(
-type tx_instance() :: term().
%% -type tx_instance() :: aec_spend_tx:tx()
%%                      | aeo_register_tx:tx()
%%                      | aeo_extend_tx:tx()
%%                      | aeo_query_tx:tx()
%%                      | aeo_response_tx:tx()
%%                      | aens_preclaim_tx:tx()
%%                      | aens_claim_tx:tx()
%%                      | aens_transfer_tx:tx()
%%                      | aens_update_tx:tx()
%%                      | aens_revoke_tx:tx()
%%                      | aect_create_tx:tx()
%%                      | aect_call_tx:tx()
%%                      | aega_attach_tx:tx()
%%                      | aega_meta_tx:tx()
%%                      | aesc_create_tx:tx()
%%                      | aesc_deposit_tx:tx()
%%                      | aesc_withdraw_tx:tx()
%%                      | aesc_force_progress_tx:tx()
%%                      | aesc_close_mutual_tx:tx()
%%                      | aesc_close_solo_tx:tx()
%%                      | aesc_slash_tx:tx()
%%                      | aesc_settle_tx:tx()
%%                      | aesc_snapshot_solo_tx:tx()
%%                      | aesc_set_delegates_tx:tx()
%%                      | aesc_offchain_tx:tx()
%%                      | aec_paying_for_tx:tx().

-type tx_ttl() :: 0 | aec_blocks:height().
%% A transaction TTL is either an absolute block height, or the transaction
%% does not have a TTL. The latter is represented as 0 to get a small
%% serialization. `aetx:ttl/1' returns `max_ttl' in this case.

-export_type([ tx/0
             , tx_instance/0
             , tx_type/0
             , tx_ttl/0 ]).

%%%===================================================================
%%% Behaviour definition
%%%===================================================================

-callback new(Args :: map()) ->
    {ok, Tx :: tx()} | {error, Reason :: term()}.

-callback type() -> atom().

-callback version(tx_instance()) -> non_neg_integer().

-callback fee(Tx :: tx_instance()) ->
    Fee :: integer().

-callback gas(Tx :: tx_instance()) ->
    Gas :: non_neg_integer().

-callback gas_price(Tx :: tx_instance()) ->
    GasPrice :: aect_contracts:amount() | undefined.

-callback ttl(Tx :: tx_instance()) ->
    TTL :: aec_blocks:height().

-callback nonce(Tx :: tx_instance()) ->
    Nonce :: non_neg_integer().

-callback origin(Tx :: tx_instance()) ->
    Origin :: aec_keys:pubkey() | undefined.

-callback signers(Tx :: tx_instance(), Trees :: aec_trees:trees()) ->
    {ok, [aec_keys:pubkey()]} | {error, atom()}.

-callback check(Tx :: tx_instance(), aec_trees:trees(), aetx_env:env()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.

-callback process(Tx :: tx_instance(), aec_trees:trees(), aetx_env:env()) ->
    {ok, NewTrees :: aec_trees:trees()}
  | {ok, NewTrees :: aec_trees:trees(), NewEnv :: aetx_env:env()}
  | {error, Reason :: term()}.

-callback serialize(Tx :: tx_instance()) ->
    term().

-callback serialization_template(Vsn :: non_neg_integer()) ->
    term().

-callback deserialize(Vsn :: integer(), SerializedTx :: term()) ->
    Tx :: tx_instance().

-callback for_client(Tx :: tx_instance()) ->
    map().

-callback valid_at_protocol(Protocol :: aec_hard_forks:protocol_vsn(),
                            Tx :: tx_instance()) -> boolean().


-optional_callbacks([gas_price/1]).

%%%===================================================================
%%% Getters and setters
%%%===================================================================

-spec new(CallbackModule :: module(),  Tx :: tx_instance()) ->
    Tx :: tx().
new(Callback, Tx) ->
    Type          = Callback:type(),
    {Vsn, Fields} = Callback:serialize(Tx),
    Template      = Callback:serialization_template(Vsn),
    Size          = byte_size(aeser_chain_objects:serialize(Type, Vsn, Template, Fields)),
    #aetx{ type = Type, cb = Callback, size = Size, tx = Tx }.

-spec fee(Tx :: tx()) -> Fee :: integer().
fee(#aetx{ cb = CB, tx = Tx }) ->
    CB:fee(Tx).

-spec deep_fee(Tx :: tx()) -> Fee :: integer().
deep_fee(#aetx{} = AeTx) ->
    %% If this is a generalized account meta tx we need to dig deeper
    %% into the inner transactions. Note that more than one meta tx
    %% can be wrapped around each other.
    deep_fee_(AeTx, 0).

deep_fee_(AeTx, AccFee0) ->
    AccFee = fee(AeTx) + AccFee0,
    case specialize_type(AeTx) of
        {ga_meta_tx, MetaTx} ->
            CB = type_to_cb(ga_meta_tx),
            deep_fee_(aetx_sign:tx(CB:tx(MetaTx)), AccFee);
        {paying_for_tx, PTx} ->
            CB = type_to_cb(paying_for_tx),
            deep_fee_(aetx_sign:tx(CB:tx(PTx)), AccFee);
        {_, _} ->
            AccFee
    end.

-spec deep_fee(Tx :: tx(), Trees :: aec_trees:trees()) -> Fee :: integer().
deep_fee(#aetx{} = AeTx, Trees) ->
    %% If this is a generalized account meta tx we need to dig deeper
    %% into the inner transactions. Note that more than one meta tx
    %% can be wrapped around each other.
    deep_fee(AeTx, Trees, 0).

deep_fee(AeTx, Trees, AccFee0) ->
    AccFee = fee(AeTx) + AccFee0,
    case specialize_type(AeTx) of
        {ga_meta_tx, MetaTx} ->
            CB = type_to_cb(ga_meta_tx),
            case CB:inner_tx_was_succesful(MetaTx, Trees) of
                true  ->
                    deep_fee(aetx_sign:tx(CB:tx(MetaTx)), Trees, AccFee);
                false ->
                    AccFee
            end;
        {paying_for_tx, PTx} ->
            CB = type_to_cb(paying_for_tx),
            deep_fee(aetx_sign:tx(CB:tx(PTx)), Trees, AccFee);
        {_, _} ->
            AccFee
    end.

-spec tx_min_gas(Tx :: tx(), Version :: aec_hard_forks:protocol_vsn()) -> Gas :: integer().
tx_min_gas(#aetx{type = Type, size = Size, cb = CB, tx = Tx}, Version) when ?IS_CONTRACT_TX(Type) ->
    base_gas(Type, Version, CB:abi_version(Tx)) + size_gas(Size);
tx_min_gas(#aetx{type = Type, size = Size}, Version) ->
    base_gas(Type, Version) + size_gas(Size).

%% In case 0 is returned, the tx will not be included in the micro block
%% candidate by the mempool.
-spec gas_limit(Tx :: tx(), Height :: aec_blocks:height(), Version :: aec_hard_forks:protocol_vsn()) ->
                       Gas :: non_neg_integer().
gas_limit(#aetx{type = Type, cb = CB, size = Size, tx = Tx }, Height, Version) when
      Type =:= oracle_register_tx;
      Type =:= oracle_extend_tx ->
    case ttl_delta(Height, CB:oracle_ttl(Tx)) of
        {delta, _D} = TTL ->
            base_gas(Type, Version) + size_gas(Size) + state_gas(Type, TTL);
        {error, _Rsn} ->
            0
    end;
gas_limit(#aetx{type = oracle_query_tx, size = Size, tx = Tx }, Height, Version) ->
    case ttl_delta(Height, aeo_query_tx:query_ttl(Tx)) of
        {delta, _D} = TTL ->
            base_gas(oracle_query_tx, Version) + size_gas(Size) + state_gas(oracle_query_tx, TTL);
        {error, _Rsn} ->
            0
    end;
gas_limit(#aetx{type = oracle_response_tx, size = Size, tx = Tx }, Height, Version) ->
    case ttl_delta(Height, aeo_response_tx:response_ttl(Tx)) of
        {delta, _D} = TTL ->
            base_gas(oracle_response_tx, Version) + size_gas(Size) + state_gas(oracle_response_tx, TTL);
        {error, _Rsn} ->
            0
    end;
gas_limit(#aetx{ type = ga_meta_tx, cb = CB, size = Size, tx = Tx }, Height, Version) ->
    base_gas(ga_meta_tx, Version, CB:abi_version(Tx)) + size_gas(Size) + CB:gas_limit(Tx, Height, Version);
gas_limit(#aetx{ type = paying_for_tx, cb = CB, size = Size, tx = Tx }, Height, Version) ->
    InnerTx = #aetx{ size = ISize } = aetx_sign:tx(CB:tx(Tx)),
    base_gas(paying_for_tx, Version) + size_gas(Size - ISize) + gas_limit(InnerTx, Height, Version);
gas_limit(#aetx{type = Type, size = Size, cb = CB, tx = Tx}, _Height, Version) when ?IS_CONTRACT_TX(Type) ->
    base_gas(Type, Version, CB:abi_version(Tx)) + size_gas(Size) + CB:gas(Tx);
gas_limit(#aetx{ type = Type, cb = CB, size = Size, tx = Tx }, _Height, Version) when
      Type =/= channel_offchain_tx,
      Type =/= channel_client_reconnect_tx ->
    base_gas(Type, Version) + size_gas(Size) + CB:gas(Tx);
gas_limit(#aetx{ type = channel_offchain_tx }, _Height, _Version) ->
    0;
gas_limit(#aetx{ type = channel_client_reconnect_tx }, _Height, _Version) ->
    0.

-spec used_gas(Tx :: tx(), Height :: aec_blocks:height(),
               Version :: aec_hard_forks:protocol_vsn(), Trees :: aec_trees:trees()) ->
        Gas :: non_neg_integer().
used_gas(Tx, Height, Version, Trees) ->
    used_gas(Tx, Height, Version, Trees, #{}).

used_gas(#aetx{ type = paying_for_tx, cb = CB, size = Size, tx = Tx }, Height, Version, Trees, Ctx) ->
    InnerTx = #aetx{ size = ISize } = aetx_sign:tx(CB:tx(Tx)),
    PayingForTxGas = base_gas(paying_for_tx, Version) + size_gas(Size - ISize),
    InnerTxGas = used_gas(InnerTx, Height, Version, Trees, Ctx),
    PayingForTxGas + InnerTxGas;
used_gas(#aetx{ type = ga_meta_tx, cb = CB, size = Size, tx = Tx }, Height, Version, Trees, Ctx) ->
    %% note that this is different than how gas_limit/3 works!
    Pubkey = CB:ga_pubkey(Tx),
    AuthCallId = CB:call_id(Tx, Trees),
    AuthGas = call_gas_used(Trees, Pubkey, AuthCallId),
    base_gas(ga_meta_tx, Version, CB:abi_version(Tx)) + size_gas(Size) + AuthGas +
        case CB:inner_tx_was_succesful(Tx, Trees) of
            false -> 0;
            true  -> used_gas(aetx_sign:tx(CB:tx(Tx)), Height, Version, Trees, Ctx#{ga_nonce => CB:auth_id(Tx)})
        end;
used_gas(#aetx{type = contract_create_tx, cb = CB, size = Size, tx = Tx}, _Height, Version, Trees, #{ga_nonce := GANonce}) ->
    ContractPubkey = aect_contracts:compute_contract_pubkey(CB:owner_pubkey(Tx), GANonce),
    CallId = aect_call:ga_id(GANonce, ContractPubkey),
    base_gas(contract_create_tx, Version, CB:abi_version(Tx)) + size_gas(Size) + call_gas_used(Trees, ContractPubkey, CallId);
used_gas(#aetx{type = contract_call_tx, cb = CB, size = Size, tx = Tx}, _Height, Version, Trees, #{ga_nonce := GANonce}) ->
    CtCallId = CB:ct_call_id(Tx),
    CallId = aect_call:ga_id(GANonce, CtCallId),
    base_gas(contract_call_tx, Version, CB:abi_version(Tx)) + size_gas(Size) + call_gas_used(Trees, CtCallId, CallId);
used_gas(#aetx{type = contract_call_tx, cb = CB, size = Size, tx = Tx}, _Height, Version, Trees, _Ctx) ->
    CtCallId = CB:ct_call_id(Tx),
    CallId = CB:call_id(Tx),
    base_gas(contract_call_tx, Version, CB:abi_version(Tx)) + size_gas(Size) + call_gas_used(Trees, CtCallId, CallId);
used_gas(#aetx{type = Type, cb = CB, size = Size, tx = Tx}, _Height, Version, Trees, _Ctx) when ?IS_CONTRACT_TX(Type) ->
    ContractPubkey = CB:contract_pubkey(Tx),
    CallId = CB:call_id(Tx),
    base_gas(Type, Version, CB:abi_version(Tx)) + size_gas(Size) + call_gas_used(Trees, ContractPubkey, CallId);
used_gas(#aetx{type = Type} = Aetx, Height, Version, _Trees, _Ctx) when not ?IS_CONTRACT_TX(Type) ->
    gas_limit(Aetx, Height, Version).

call_gas_used(Trees, ContractPubkey, CallId) ->
    CallsTrees = aec_trees:calls(Trees),
    Call = aect_call_state_tree:get_call(ContractPubkey, CallId, CallsTrees),
    aect_call:gas_used(Call).

-spec inner_gas_limit(Tx :: tx(), Height :: aec_blocks:height(), Version :: aec_hard_forks:protocol_vsn()) ->
                             Gas :: non_neg_integer().
inner_gas_limit(AETx = #aetx{ size = Size }, Height, Version) ->
    max(0, gas_limit(AETx, Height, Version) - size_gas(Size)).

-spec gas_price(Tx :: tx()) -> GasPrice :: non_neg_integer() | undefined.
gas_price(#aetx{ type = Type, cb = CB, tx = Tx }) when ?HAS_GAS_TX(Type) ->
    CB:gas_price(Tx);
gas_price(#aetx{}) ->
    undefined.

-spec min_gas_price(Tx :: tx(), Height :: aec_blocks:height(), Version :: aec_hard_forks:protocol_vsn()) ->
                           MinGasPrice :: non_neg_integer().
min_gas_price(AETx, Height, Version) when Version < ?IRIS_PROTOCOL_VSN ->
    min_gas_price(AETx, Height, outer, Version);
min_gas_price(AETx = #aetx{ type = Type, cb = CB, tx = Tx }, Height, Version) ->
    FeeGas = fee_gas(AETx, Height, Version),
    FeeGasPrice = (CB:fee(Tx) + FeeGas - 1) div FeeGas,
    case Type of
        ga_meta_tx ->
            InnerMinGasPrice = min_gas_price(aetx_sign:tx(CB:tx(Tx)), Height, Version),
            lists:min([CB:gas_price(Tx), FeeGasPrice, InnerMinGasPrice]);
        paying_for_tx ->
            InnerMinGasPrice = min_gas_price(aetx_sign:tx(CB:tx(Tx)), Height, Version),
            min(FeeGasPrice, InnerMinGasPrice);
        _ when ?HAS_GAS_TX(Type) ->
            min(CB:gas_price(Tx), FeeGasPrice);
        _ ->
            FeeGasPrice
    end.

min_gas_price(AETx = #aetx{ type = Type, cb = CB, tx = Tx, size = Size }, Height, Kind, Version) ->
    %% Compute a fictive gas price from the given Fee
    FeeGas = if Kind == outer -> fee_gas(AETx, Height, Version);
                Kind == inner -> fee_gas(AETx, Height, Version) - size_gas(Size)
             end,
    FeeGasPrice = (CB:fee(Tx) + FeeGas - 1) div FeeGas,
    case Type of
        ga_meta_tx ->
            %% Also compute the minimum gas price for the wrapped Tx - make sure
            %% not to count the size twice!
            InnerMinGasPrice = min_gas_price(aetx_sign:tx(CB:tx(Tx)), Height, inner, Version),
            lists:min([CB:gas_price(Tx), FeeGasPrice, InnerMinGasPrice]);
        _ when ?HAS_GAS_TX(Type) ->
            min(CB:gas_price(Tx), FeeGasPrice);
        _ ->
            FeeGasPrice
    end.

-spec min_fee(Tx :: tx(), Height :: aec_blocks:height(), Version :: aec_hard_forks:protocol_vsn()) ->
                     Fee :: non_neg_integer().
min_fee(#aetx{} = AeTx, Height, Version) ->
    fee_gas(AeTx, Height, Version) * aec_governance:minimum_gas_price(Version).

%% Returns the amount of gas that the fee should cover for a given transaction.
%% It does not consider nested transactions, or gas (if any) used by the
%% transaction.
-spec fee_gas(Tx :: tx(), Height :: aec_blocks:height(), Version :: aec_hard_forks:protocol_vsn()) ->
                     Gas :: non_neg_integer().
fee_gas(#aetx{ type = paying_for_tx, size = Size, cb = CB, tx = Tx }, _Height, Version)
        when Version >= ?IRIS_PROTOCOL_VSN ->
    #aetx{ size = ISize } = aetx_sign:tx(CB:tx(Tx)),
    base_gas(paying_for_tx, Version) + size_gas(Size - ISize);
fee_gas(#aetx{ type = ga_meta_tx, size = Size, cb = CB, tx = Tx }, _Height, Version)
        when Version >= ?IRIS_PROTOCOL_VSN ->
    #aetx{ size = ISize } = aetx_sign:tx(CB:tx(Tx)),
    base_gas(ga_meta_tx, Version, CB:abi_version(Tx)) + size_gas(Size - ISize);
fee_gas(#aetx{ type = Type, size = Size, cb = CB, tx = Tx }, _Height, Version) when ?IS_CONTRACT_TX(Type) ->
    base_gas(Type, Version, CB:abi_version(Tx)) + size_gas(Size);
fee_gas(#aetx{ type = Type, size = Size }, _Height, Version) when ?HAS_GAS_TX(Type) ->
    base_gas(Type, Version) + size_gas(Size);
fee_gas(#aetx{} = Tx, Height, Version) ->
    gas_limit(Tx, Height, Version).

-spec nonce(Tx :: tx()) -> Nonce :: non_neg_integer().
nonce(#aetx{ cb = CB, tx = Tx }) ->
    CB:nonce(Tx).

-spec origin(Tx :: tx()) -> Origin :: aec_keys:pubkey() | undefined.
origin(#aetx{ cb = CB, tx = Tx }) ->
    CB:origin(Tx).

-spec accounts(Tx :: tx()) -> [aec_keys:pubkey()].
accounts(#aetx{ cb = CB, tx = Tx }) ->
    CB:accounts(Tx).

-spec signers(Tx :: tx(), Trees :: aec_trees:trees()) ->
    {ok, [aec_keys:pubkey()]} | {error, atom()}.
signers(#aetx{ cb = CB, tx = Tx }, Trees) ->
    CB:signers(Tx, Trees).

%% We let 0 represent no TTL/infinity in order to keep the serialization as
%% short as possible. Since there are no transactions in the genesis block
%% there is little risk for confusion.
-spec ttl(Tx :: tx()) -> max_ttl | aec_blocks:height().
ttl(#aetx{ cb = CB, tx = Tx }) ->
    case CB:ttl(Tx) of
        0 -> max_ttl;
        N -> N
    end.

-spec valid_at_protocol(Protocol :: aec_hard_forks:protocol_vsn(),
                      Tx :: tx()) -> boolean().
valid_at_protocol(Protocol, #aetx{ cb = CB, tx = Tx }) ->
    CB:valid_at_protocol(Protocol, Tx).

-spec size(Tx :: tx()) -> pos_integer().
size(#aetx{ size = Size }) ->
    Size.

-spec from_db_format(tx()) -> tx().
from_db_format(#aetx{ cb = aect_call_tx, tx = Tx } = AETx) ->
    case aect_call_tx:from_db_format(Tx) of
        Tx  -> AETx;
        Tx1 -> AETx#aetx{ tx = Tx1 }
    end;
from_db_format(#aetx{ cb = aect_create_tx, tx = Tx } = AETx) ->
    case aect_create_tx:from_db_format(Tx) of
        Tx  -> AETx;
        Tx1 -> AETx#aetx{ tx = Tx1 }
    end;
from_db_format(#aetx{ cb = aesc_force_progress_tx, tx = Tx } = AETx) ->
    case aesc_force_progress_tx:from_db_format(Tx) of
        Tx  -> AETx;
        Tx1 -> AETx#aetx{ tx = Tx1 }
    end;
from_db_format(#aetx{ cb = aens_claim_tx, tx = Tx } = AETx) ->
    case aens_claim_tx:from_db_format(Tx) of
        Tx  -> AETx;
        Tx1 -> AETx#aetx{ tx = Tx1 }
    end;
from_db_format(#aetx{} = Tx) ->
    Tx.

%%%===================================================================
%%% Checking transactions
%%%===================================================================

check(Tx, Trees, Env) ->
    case aetx_env:context(Env) of
        aetx_contract    ->
            check_contract(Tx, Trees, Env);
        Ctxt when Ctxt == aetx_transaction; Ctxt == aetx_ga ->
            check_tx(Tx, Trees, Env)
    end.

check_contract(#aetx{ cb = CB, tx = Tx }, Trees, Env) ->
    CB:check(Tx, Trees, Env).

check_tx(#aetx{ cb = CB, tx = Tx } = AeTx, Trees, Env) ->
    Checks =
        [fun() -> check_minimum_fee(AeTx, Env) end,
         fun() -> check_minimum_gas_price(AeTx, Env) end,
         fun() -> check_ttl(AeTx, Env) end,
         fun() -> check_protocol(AeTx, aetx_env:consensus_version(Env)) end
        ],
    case aeu_validation:run(Checks) of
        ok             -> CB:check(Tx, Trees, Env);
        {error, _} = E -> E
    end.

check_minimum_fee(AeTx, Env) ->
    Protocol = aetx_env:consensus_version(Env),
    Height = aetx_env:height(Env),
    AeTx1 = case aetx_env:context(Env) of
                aetx_ga when Protocol < ?IRIS_PROTOCOL_VSN ->
                    %% Size is paid for by the outermost meta tx
                    AeTx#aetx{size = 0};
                Ctx when Ctx =:= aetx_transaction; Ctx =:= aetx_contract; Ctx =:= aetx_ga ->
                    AeTx
            end,
    case min_fee(AeTx1, Height, Protocol) of
        MinFee when MinFee > 0 ->
            case fee(AeTx) >= MinFee of
                true  -> ok;
                false -> {error, too_low_fee}
            end;
        0 ->
            %% Oracle txs can return (minimal) gas of 0 when
            %% the absolute TTL is lower than the current chain
            %% height.
            {error, too_low_abs_ttl}
    end.

check_minimum_gas_price(AeTx, Env) ->
    case gas_price(AeTx) of
        undefined ->
            ok;
        GasPrice when is_integer(GasPrice) ->
            Version = aetx_env:consensus_version(Env),
            case GasPrice >= aec_governance:minimum_gas_price(Version) of
                true  -> ok;
                false -> {error, too_low_gas_price}
            end
    end.

check_ttl(AeTx, Env) ->
    case ttl(AeTx) >= aetx_env:height(Env) of
        true  -> ok;
        false -> {error, ttl_expired}
    end.

check_protocol(AeTx, Protocol) ->
    case valid_at_protocol(Protocol, AeTx) of
        true  -> ok;
        false -> {error, invalid_at_protocol}
    end.

%%%===================================================================
%%% Processing transactions
%%%===================================================================

-spec process(tx(), aec_trees:trees(), aetx_env:env()) ->
                 {ok, NewTrees :: aec_trees:trees(), aetx_env:env()}
               | {error, term()}.
process(#aetx{ cb = CB, tx = Tx } = AeTx, Trees, Env) ->
    case check(AeTx, Trees, Env) of
        {ok, Trees1} ->
            case CB:process(Tx, Trees1, Env) of
                {ok, Trees2}             -> {ok, Trees2, Env};
                {ok, _Trees, _Env} = Ok  -> Ok;
                {error, _Reason} = Error -> Error
            end;
        {error, _} = Err ->
            Err
    end.

%% Call a custom callback function in the transaction module.
-spec custom_apply(atom(), tx(), aec_trees:trees(), aetx_env:env()) -> any().
custom_apply(Fun, #aetx{ cb = CB, tx = Tx}, Trees, Env) ->
    CB:Fun(Tx, Trees, Env).

%%%===================================================================
%%% Serialize/deserialize
%%%===================================================================

-spec serialize_for_client(Tx :: tx()) -> map().
serialize_for_client(#aetx{ cb = CB, type = Type, tx = Tx }) ->
    Res0 = CB:for_client(Tx),
    Res1 = Res0#{ <<"type">> => type_to_swagger_name(Type),
                  <<"version">> => CB:version(Tx) },
    case maps:get(<<"ttl">>, Res1, 0) of
        0 -> maps:remove(<<"ttl">>, Res1);
        _ -> Res1
    end.

-spec serialize_to_binary(Tx :: tx()) -> term().
serialize_to_binary(#aetx{ cb = CB, type = Type, tx = Tx }) ->
    {Vsn, Fields} = CB:serialize(Tx),
    aeser_chain_objects:serialize(
      Type,
      Vsn,
      CB:serialization_template(Vsn),
      Fields).

-spec deserialize_from_binary(Bin :: binary()) -> Tx :: tx().
deserialize_from_binary(Bin) ->
    {Type, Vsn, RawFields} =
        aeser_chain_objects:deserialize_type_and_vsn(Bin),
    CB = type_to_cb(Type),
    Template = CB:serialization_template(Vsn),
    Fields = aeserialization:decode_fields(Template, RawFields),
    #aetx{cb = CB, type = Type, size = byte_size(Bin), tx = CB:deserialize(Vsn, Fields)}.

type_to_cb(spend_tx)                  -> aec_spend_tx;
type_to_cb(oracle_register_tx)        -> aeo_register_tx;
type_to_cb(oracle_extend_tx)          -> aeo_extend_tx;
type_to_cb(oracle_query_tx)           -> aeo_query_tx;
type_to_cb(oracle_response_tx)        -> aeo_response_tx;
type_to_cb(name_preclaim_tx)          -> aens_preclaim_tx;
type_to_cb(name_claim_tx)             -> aens_claim_tx;
type_to_cb(name_transfer_tx)          -> aens_transfer_tx;
type_to_cb(name_update_tx)            -> aens_update_tx;
type_to_cb(name_revoke_tx)            -> aens_revoke_tx;
type_to_cb(contract_call_tx)          -> aect_call_tx;
type_to_cb(contract_create_tx)        -> aect_create_tx;
type_to_cb(ga_attach_tx)              -> aega_attach_tx;
type_to_cb(ga_meta_tx)                -> aega_meta_tx;
type_to_cb(paying_for_tx)             -> aec_paying_for_tx;
type_to_cb(channel_create_tx)         -> aesc_create_tx;
type_to_cb(channel_deposit_tx)        -> aesc_deposit_tx;
type_to_cb(channel_withdraw_tx)       -> aesc_withdraw_tx;
type_to_cb(channel_force_progress_tx) -> aesc_force_progress_tx;
type_to_cb(channel_close_solo_tx)     -> aesc_close_solo_tx;
type_to_cb(channel_close_mutual_tx)   -> aesc_close_mutual_tx;
type_to_cb(channel_slash_tx)          -> aesc_slash_tx;
type_to_cb(channel_settle_tx)         -> aesc_settle_tx;
type_to_cb(channel_snapshot_solo_tx)  -> aesc_snapshot_solo_tx;
type_to_cb(channel_set_delegates_tx)  -> aesc_set_delegates_tx;
type_to_cb(channel_offchain_tx)       -> aesc_offchain_tx;
type_to_cb(hc_vote_tx)                -> aec_hc_vote_tx.

type_to_swagger_name(spend_tx)                  -> <<"SpendTx">>;
type_to_swagger_name(oracle_register_tx)        -> <<"OracleRegisterTx">>;
type_to_swagger_name(oracle_extend_tx)          -> <<"OracleExtendTx">>;
type_to_swagger_name(oracle_query_tx)           -> <<"OracleQueryTx">>;
type_to_swagger_name(oracle_response_tx)        -> <<"OracleRespondTx">>;
type_to_swagger_name(name_preclaim_tx)          -> <<"NamePreclaimTx">>;
type_to_swagger_name(name_claim_tx)             -> <<"NameClaimTx">>;
type_to_swagger_name(name_transfer_tx)          -> <<"NameTransferTx">>;
type_to_swagger_name(name_update_tx)            -> <<"NameUpdateTx">>;
type_to_swagger_name(name_revoke_tx)            -> <<"NameRevokeTx">>;
type_to_swagger_name(contract_call_tx)          -> <<"ContractCallTx">>;
type_to_swagger_name(contract_create_tx)        -> <<"ContractCreateTx">>;
type_to_swagger_name(ga_attach_tx)              -> <<"GAAttachTx">>;
type_to_swagger_name(ga_meta_tx)                -> <<"GAMetaTx">>;
type_to_swagger_name(paying_for_tx)             -> <<"PayingForTx">>;
type_to_swagger_name(channel_create_tx)         -> <<"ChannelCreateTx">>;
type_to_swagger_name(channel_deposit_tx)        -> <<"ChannelDepositTx">>;
type_to_swagger_name(channel_withdraw_tx)       -> <<"ChannelWithdrawTx">>;
type_to_swagger_name(channel_force_progress_tx) -> <<"ChannelForceProgressTx">>;
type_to_swagger_name(channel_close_solo_tx)     -> <<"ChannelCloseSoloTx">>;
type_to_swagger_name(channel_close_mutual_tx)   -> <<"ChannelCloseMutualTx">>;
type_to_swagger_name(channel_slash_tx)          -> <<"ChannelSlashTx">>;
type_to_swagger_name(channel_settle_tx)         -> <<"ChannelSettleTx">>;
type_to_swagger_name(channel_snapshot_solo_tx)  -> <<"ChannelSnapshotSoloTx">>;
type_to_swagger_name(channel_set_delegates_tx)  -> <<"ChannelSetDelegatesTx">>;
%% not exposed in HTTP API:
type_to_swagger_name(channel_offchain_tx)       -> <<"ChannelOffchainTx">>;
type_to_swagger_name(hc_vote_tx)                -> <<"HCVoteTx">>.

-spec swagger_name_to_type(binary())               -> tx_type().
swagger_name_to_type(<<"SpendTx">>)                -> spend_tx;
swagger_name_to_type(<<"SpendTxInput">>)           -> spend_tx;
swagger_name_to_type(<<"OracleRegisterTx">>)       -> oracle_register_tx;
swagger_name_to_type(<<"OracleExtendTx">>)         -> oracle_extend_tx;
swagger_name_to_type(<<"OracleQueryTx">>)          -> oracle_query_tx;
swagger_name_to_type(<<"OracleRespondTx">>)        -> oracle_response_tx;
swagger_name_to_type(<<"NamePreclaimTx">>)         -> name_preclaim_tx;
swagger_name_to_type(<<"NameClaimTx">>)            -> name_claim_tx;
swagger_name_to_type(<<"NameTransferTx">>)         -> name_transfer_tx;
swagger_name_to_type(<<"NameUpdateTx">>)           -> name_update_tx;
swagger_name_to_type(<<"NameRevokeTx">>)           -> name_revoke_tx;
swagger_name_to_type(<<"ContractCallTx">>)         -> contract_call_tx;
swagger_name_to_type(<<"ContractCreateTx">>)       -> contract_create_tx;
swagger_name_to_type(<<"GAAttachTx">>)             -> ga_attach_tx;
swagger_name_to_type(<<"GAMetaTx">>)               -> ga_meta_tx;
swagger_name_to_type(<<"PayingForTx">>)            -> paying_for_tx;
swagger_name_to_type(<<"ChannelCreateTx">>)        -> channel_create_tx;
swagger_name_to_type(<<"ChannelDepositTx">>)       -> channel_deposit_tx;
swagger_name_to_type(<<"ChannelWithdrawTx">>)      -> channel_withdraw_tx;
swagger_name_to_type(<<"ChannelForceProgressTx">>) -> channel_force_progress_tx;
swagger_name_to_type(<<"ChannelCloseSoloTx">>)     -> channel_close_solo_tx;
swagger_name_to_type(<<"ChannelCloseMutualTx">>)   -> channel_close_mutual_tx;
swagger_name_to_type(<<"ChannelSlashTx">>)         -> channel_slash_tx;
swagger_name_to_type(<<"ChannelSettleTx">>)        -> channel_settle_tx;
swagger_name_to_type(<<"ChannelSnapshotSoloTx">>)  -> channel_snapshot_solo_tx;
swagger_name_to_type(<<"ChannelSetDelegatesTx">>)  -> channel_set_delegates_tx;
%% not exposed in HTTP API:
swagger_name_to_type(<<"ChannelOffchainTx">>)      -> channel_offchain_tx;
swagger_name_to_type(<<"HCVoteTx">>)               -> hc_vote_tx.

-spec specialize_type(Tx :: tx()) -> {tx_type(), tx_instance()}.
specialize_type(#aetx{ type = Type, tx = Tx }) -> {Type, Tx}.

-spec specialize_callback(Tx :: tx()) -> {module(), tx_instance()}.
specialize_callback(#aetx{ cb = CB, tx = Tx }) -> {CB, Tx}.

-spec update_tx(tx(), tx_instance()) -> tx().
update_tx(#aetx{} = Tx, NewTxI) ->
    Tx#aetx{tx = NewTxI}.

base_gas(Type, Version, ABI) ->
    aec_governance:tx_base_gas(Type, Version, ABI).

base_gas(Type, Version) ->
    aec_governance:tx_base_gas(Type, Version).

size_gas(Size) ->
    Size * aec_governance:byte_gas().

state_gas(Tag, {delta, TTL}) ->
    aec_governance_utils:state_gas(
      aec_governance:state_gas_per_block(Tag),
      TTL).

ttl_delta(_Height, {delta, _D} = TTL) ->
    {delta, aeo_utils:ttl_delta(0, TTL)};
ttl_delta(Height, {block, _H} = TTL) ->
    case aeo_utils:ttl_delta(Height, TTL) of
        TTLDelta when is_integer(TTLDelta) ->
            {delta, TTLDelta};
        {error, _Rsn} = Err ->
            Err
    end.

-ifdef(TEST).
tx(Tx) ->
    Tx#aetx.tx.
-endif.

-spec tx_type(tx()) -> tx_type().
tx_type(Tx) ->
    Tx#aetx.type.
