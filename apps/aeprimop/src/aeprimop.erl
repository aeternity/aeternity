%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%      Primitive operations to modify chain state objects
%%% @end
%%%-------------------------------------------------------------------
-module(aeprimop).

-export([ eval/3
        , eval_with_return/3
        , eval_on_primop_state/2
        ]).

%% Simple access tx instructions API
-export([ channel_create_tx_instructions/11
        , channel_close_mutual_tx_instructions/6
        , channel_deposit_tx_instructions/7
        , channel_settle_tx_instructions/6
        , channel_withdraw_tx_instructions/7
        , contract_call_from_contract_instructions/11
        , contract_call_tx_instructions/11
        , contract_create_tx_instructions/11
        , ga_attach_tx_instructions/10
        , ga_meta_tx_instructions/6
        , ga_set_meta_tx_res_instructions/3
        , name_claim_tx_instructions/5
        , name_preclaim_tx_instructions/5
        , name_revoke_tx_instructions/4
        , name_transfer_tx_instructions/5
        , name_update_tx_instructions/7
        , oracle_extend_tx_instructions/4
        , oracle_query_tx_instructions/8
        , oracle_register_tx_instructions/8
        , oracle_response_tx_instructions/6
        , spend_tx_instructions/5
        ]).

%% Direct op API (mainly for FATE).
-export([ spend_op/3
        ]).



-import(aeprimop_state, [ delete_x/3
                        , find_account/2
                        , find_auth_call/3
                        , find_channel/2
                        , find_commitment/2
                        , find_name/2
                        , find_oracle/2
                        , find_oracle_query/3
                        , get_account/2
                        , get_auth_call/3
                        , get_channel/2
                        , get_commitment/3
                        , get_contract/2
                        , get_contract_without_store/2
                        , get_name/2
                        , get_oracle/3
                        , get_oracle_query/3
                        , get_var/3
                        , new/3
                        , put_account/2
                        , put_auth_call/2
                        , put_call/2
                        , put_channel/2
                        , put_commitment/2
                        , put_contract/2
                        , put_name/2
                        , put_oracle/2
                        , put_oracle_query/2
                        ]).


-ifdef(TEST).
-export([evaluate/1, do_eval/3]).
-define(do_eval(Instr, Ts, Env),
        begin
            evaluate(Instr),
            do_eval(Instr, Ts, Env)
        end).
-else.
-define(do_eval(Instr, Ts, Env),
        do_eval(Instr, Ts, Env)).
-endif.


-include("aeprimop_state.hrl").
-include("../../aecore/include/aec_hash.hrl").
-include("../../aecontract/include/hard_forks.hrl").
-include("../../aecontract/include/aecontract.hrl").

-define(IS_HASH(_X_), (is_binary(_X_) andalso byte_size(_X_) =:= ?HASH_BYTES)).
-define(IS_VAR(_X_), (is_tuple(_X_)
                      andalso tuple_size(_X_) =:= 2
                      andalso var =:= element(1, _X_)
                      andalso is_atom(element(2, _X_)))).

-define(IS_VAR_OR_HASH(_X_), (?IS_HASH(_X_) orelse ?IS_VAR(_X_))).
-define(IS_NON_NEG_INTEGER(_X_), (is_integer(_X_) andalso _X_ >= 0)).
-define(IS_NAME_RESOLVE_TYPE(_X_), (_X_ =:= account
                                    orelse _X_ =:= name)).

-opaque op() :: {atom(), tuple()}.

-type pubkey() :: aec_keys:pubkey().
-type id()     :: aeser_id:id().
-type hash()   :: aec_hash:hash().
-type nonce()  :: non_neg_integer().
-type ttl()    :: non_neg_integer().
-type fee()    :: non_neg_integer().
-type amount() :: non_neg_integer().
-type var_or_hash() :: {var, term()} | hash().
-type oracle_type_format() :: aeo_oracles:type_format().
-type abi_version() :: aect_contracts:abi_version().
-type vm_version()  :: aect_contracts:vm_version().

-export_type([ op/0
             ]).


%%%===================================================================
%%% API
%%%===================================================================

-spec eval([op()], aec_trees:trees(), aetx_env:env()) ->
                  {ok, aec_trees:trees(), aetx_env:env()} | {error, atom()}.
eval([_|_] = Instructions, Trees, TxEnv) ->
    %% The macro below makes mocking possible in QuickCheck tests
    ?do_eval(Instructions, Trees, TxEnv).

-ifdef(TEST).
%% Keep this function, it is used in mocking in QuickCheck tests
evaluate(_Instructions) ->
    ok.
-endif.

do_eval(Instructions, Trees, TxEnv) ->
    S = aeprimop_state:new(Trees, aetx_env:height(TxEnv), TxEnv),
    case int_eval(Instructions, S) of
        {ok, S1} -> {ok, S1#state.trees, S1#state.tx_env};
        {ok, _, _} -> error(illegal_return);
        {error, _} = Err -> Err
    end.

-type return_val() :: term().
-spec eval_with_return([op()], aec_trees:trees(), aetx_env:env()) ->
                              {ok, return_val(), aec_trees:trees(), aetx_env:env()}
                                  | {error, atom()}.
eval_with_return([_|_] = Instructions, Trees, TxEnv) ->
    S = aeprimop_state:new(Trees, aetx_env:height(TxEnv), TxEnv),
    case int_eval(Instructions, S) of
        {ok, _} -> error(illegal_no_return);
        {ok, Return, S1} -> {ok, Return, S1#state.trees, S1#state.tx_env};
        {error, _} = Err -> Err
    end.

-spec eval_on_primop_state([op()], aeprimop_state:state()) ->
                                  {ok, aeprimop_state:state()}
                                      | {ok, return_val(), aeprimop_state:state()}
                                      | {error, atom()}.
eval_on_primop_state([_|_] = Instructions, State) ->
    int_eval(Instructions, State).

-spec spend_tx_instructions(pubkey(), id(), amount(), fee(), nonce()) -> [op()].
spend_tx_instructions(SenderPubkey, RecipientID, Amount, Fee, Nonce) ->
    Recipient = {var, recipient},
    {Type, RecipientHash} = specialize_account(RecipientID),
    [ inc_account_nonce_op(SenderPubkey, Nonce)
    , resolve_account_op(Type, RecipientHash, Recipient)
    , spend_fee_op(SenderPubkey, Fee)
    , spend_op(SenderPubkey, Recipient, Amount)
    ].

-spec oracle_register_tx_instructions(
        pubkey(), oracle_type_format(), oracle_type_format(), fee(), ttl(),
        abi_version(), fee(), nonce()) -> [op()].
oracle_register_tx_instructions(AccountPubkey, QFormat, RFormat, QFee,
                                DeltaTTL, ABIVersion, TxFee, Nonce) ->
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, TxFee)
    , oracle_register_op(AccountPubkey, QFormat, RFormat, QFee,
                         DeltaTTL, ABIVersion)
    ].

-spec oracle_extend_tx_instructions(pubkey(), ttl(), fee(), nonce()) -> [op()].
oracle_extend_tx_instructions(Pubkey, DeltaTTL, Fee, Nonce) ->
    [ inc_account_nonce_op(Pubkey, Nonce)
    , spend_fee_op(Pubkey, Fee)
    , oracle_extend_op(Pubkey, DeltaTTL)
    ].

-spec oracle_query_tx_instructions(pubkey(), pubkey(), binary(), fee(),
                                   ttl(), ttl(), fee(), nonce()) -> [op()].
oracle_query_tx_instructions(OraclePubkey, SenderPubkey, Query,
                             QueryFee, QTTL, RTTL, TxFee, Nonce) ->
    [ force_inc_account_nonce_op(SenderPubkey, Nonce)
    , spend_fee_op(SenderPubkey, TxFee + QueryFee)
    , oracle_query_op(OraclePubkey, SenderPubkey, Nonce,
                      Query, QueryFee, QTTL, RTTL)
    ].

-spec oracle_response_tx_instructions(pubkey(), hash(), binary(), ttl(),
                                      fee(), nonce()) -> [op()].
oracle_response_tx_instructions(OraclePubkey, QueryId, Response,
                                RTTL, Fee, Nonce) ->
    [ oracle_respond_op(OraclePubkey, QueryId, Response, RTTL)
    , inc_account_nonce_op(OraclePubkey, Nonce)
      %% NOTE: Order is important. Oracle needs to cover
      %% the fee before earning the query fee.
      %% Changing this breaks consensus.
    , spend_fee_op(OraclePubkey, Fee)
    , oracle_earn_query_fee_op(OraclePubkey, QueryId)
    ].

-spec name_preclaim_tx_instructions(pubkey(), hash(), ttl(), fee(), nonce()
                                   ) -> [op()].
name_preclaim_tx_instructions(AccountPubkey, CommitmentHash, DeltaTTL,
                              Fee, Nonce) ->
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, Fee)
    , name_preclaim_op(AccountPubkey, CommitmentHash, DeltaTTL)
    ].

-spec name_claim_tx_instructions(pubkey(), binary(), non_neg_integer(),
                                 fee(), nonce()) -> [op()].
name_claim_tx_instructions(AccountPubkey, PlainName, NameSalt, Fee, Nonce) ->
    PreclaimDelta = aec_governance:name_claim_preclaim_delta(),
    DeltaTTL = aec_governance:name_claim_max_expiration(),
    LockedFee = aec_governance:name_claim_locked_fee(),
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, Fee)
    , lock_amount_op(AccountPubkey, LockedFee)
    , name_claim_op(AccountPubkey, PlainName, NameSalt, DeltaTTL, PreclaimDelta)
    ].

-spec name_revoke_tx_instructions(pubkey(), hash(), fee(), nonce()) -> [op()].
name_revoke_tx_instructions(AccountPubkey, NameHash, Fee, Nonce) ->
    ProtectedDeltaTTL = aec_governance:name_protection_period(),
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, Fee)
    , name_revoke_op(AccountPubkey, NameHash, ProtectedDeltaTTL)
    ].

-spec name_transfer_tx_instructions(pubkey(), id(), hash(), fee(), nonce()
                                   ) -> [op()].
name_transfer_tx_instructions(OwnerPubkey, RecipientID, NameHash, Fee, Nonce) ->
    {Type, Hash} = specialize_account(RecipientID),
    [ inc_account_nonce_op(OwnerPubkey, Nonce)
    , spend_fee_op(OwnerPubkey, Fee)
    , name_transfer_op(OwnerPubkey, Type, Hash, NameHash)
    ].

-spec name_update_tx_instructions(
        pubkey(), hash(), ttl(), ttl(), [aens_pointer:pointer()],
        fee(), nonce()) -> [op()].
name_update_tx_instructions(OwnerPubkey, NameHash, DeltaTTL, ClientTTL,
                            Pointers, Fee, Nonce) ->
    MaxTTL = aec_governance:name_claim_max_expiration(),
    [ inc_account_nonce_op(OwnerPubkey, Nonce)
    , spend_fee_op(OwnerPubkey, Fee)
    , name_update_op(OwnerPubkey, NameHash, DeltaTTL, MaxTTL,
                     ClientTTL, Pointers)
    ].

-spec ga_attach_tx_instructions(pubkey(), amount(), amount(),
                                abi_version(), vm_version(),
                                binary(), binary(), binary(), fee(), nonce()) -> [op()].
ga_attach_tx_instructions(OwnerPubkey, GasLimit, GasPrice, ABIVersion, VMVersion,
                          SerializedCode, AuthFun, CallData, Fee, Nonce) ->
    [ inc_account_nonce_op(OwnerPubkey, Nonce)
    , ga_attach_op(OwnerPubkey, GasLimit, GasPrice, ABIVersion, VMVersion,
                   SerializedCode, AuthFun, CallData, Fee, Nonce)
    ].

-spec ga_meta_tx_instructions(pubkey(), binary(), abi_version(),
                              amount(), amount(), fee()) -> [op()].
ga_meta_tx_instructions(OwnerPubkey, AuthData, ABIVersion,
                        GasLimit, GasPrice, Fee) ->
    [ ga_meta_op(OwnerPubkey, AuthData, ABIVersion,
                 GasLimit, GasPrice, Fee)
    ].

-spec ga_set_meta_tx_res_instructions(pubkey(), binary(), 'ok' | {error, term()}) -> [op()].
ga_set_meta_tx_res_instructions(OwnerPubkey, AuthData, Result) ->
    [ ga_set_meta_res_op(OwnerPubkey, AuthData, Result) ].

-spec contract_create_tx_instructions(pubkey(), amount(), amount(),
                                      non_neg_integer(), non_neg_integer(),
                                      abi_version(), vm_version(),
                                      binary(), binary(),
                                      fee(), nonce()) -> [op()].
contract_create_tx_instructions(OwnerPubkey, Amount, Deposit, GasLimit, GasPrice,
                                ABIVersion, VMVersion, SerializedCode, CallData, Fee, Nonce) ->
    [ inc_account_nonce_op(OwnerPubkey, Nonce)
    , contract_create_op(OwnerPubkey, Amount, Deposit, GasLimit,
                         GasPrice, ABIVersion, VMVersion, SerializedCode,
                         CallData, Fee, Nonce)
    ].

-spec contract_call_tx_instructions(pubkey(), pubkey(), binary(),
                                    non_neg_integer(), non_neg_integer(),
                                    amount(), [binary()], abi_version(),
                                    pubkey(), fee(), nonce()) -> [op()].
contract_call_tx_instructions(CallerPubKey, ContractPubkey, CallData,
                              GasLimit, GasPrice, Amount, CallStack,
                              ABIVersion, Origin, Fee, Nonce) ->
    [ inc_account_nonce_op(CallerPubKey, Nonce)
    , contract_call_op(CallerPubKey, ContractPubkey, CallData,
                       GasLimit, GasPrice, Amount,
                       ABIVersion, Origin, CallStack, Fee, Nonce)
    ].

-spec contract_call_from_contract_instructions(
        pubkey(), pubkey(), binary(), non_neg_integer(), non_neg_integer(),
        amount(), [binary()], abi_version(), pubkey(), fee(), nonce()
       ) -> [op()].
contract_call_from_contract_instructions(CallerPubKey, ContractPubkey, CallData,
                                         GasLimit, GasPrice, Amount, CallStack,
                                         ABIVersion, Origin, Fee, Nonce) ->
    [ contract_call_op(CallerPubKey, ContractPubkey, CallData,
                       GasLimit, GasPrice, Amount,
                       ABIVersion, Origin, CallStack, Fee, Nonce)
    ].

-spec channel_create_tx_instructions(
        pubkey(), amount(), pubkey(), amount(), amount(), [pubkey()],
        hash(), ttl(), fee(), nonce(), non_neg_integer()) -> [op()].
channel_create_tx_instructions(InitiatorPubkey, InitiatorAmount,
                               ResponderPubkey, ResponderAmount,
                               ReserveAmount, DelegatePubkeys,
                               StateHash, LockPeriod, Fee, Nonce, Round) ->
    %% The force is not strictly necessary since this cannot be made
    %% from a contract.
    [ force_inc_account_nonce_op(InitiatorPubkey, Nonce)
    , spend_fee_op(InitiatorPubkey, Fee + InitiatorAmount)
    , spend_fee_op(ResponderPubkey, ResponderAmount)
    , channel_create_op(InitiatorPubkey, InitiatorAmount,
                        ResponderPubkey, ResponderAmount,
                        ReserveAmount, DelegatePubkeys,
                        StateHash, LockPeriod, Nonce, Round)
    , tx_event_op({channel, aesc_channels:pubkey(InitiatorPubkey,
                                                 Nonce,
                                                 ResponderPubkey)})
    ].

-spec channel_deposit_tx_instructions(pubkey(), pubkey(), amount(), hash(),
                                      non_neg_integer(), fee(), nonce()
                                     ) -> [op()].
channel_deposit_tx_instructions(FromPubkey, ChannelPubkey, Amount, StateHash,
                                Round, Fee, Nonce) ->
    [ inc_account_nonce_op(FromPubkey, Nonce)
    , spend_fee_op(FromPubkey, Fee + Amount)
    , channel_deposit_op(FromPubkey, ChannelPubkey, Amount, StateHash, Round)
    , tx_event_op({channel, ChannelPubkey})
    ].

-spec channel_close_mutual_tx_instructions(pubkey(), pubkey(), amount(),
                                           amount(), nonce(), fee()) -> [op()].
channel_close_mutual_tx_instructions(FromPubkey, ChannelPubkey,
                                     InitiatorAmount, ResponderAmount,
                                     Nonce, Fee) ->
    [ inc_account_nonce_op(FromPubkey, Nonce)
    , channel_close_mutual_op(FromPubkey, ChannelPubkey,
                              InitiatorAmount, ResponderAmount, Fee)
    , tx_event_op({channel, ChannelPubkey})
    ].

-spec channel_withdraw_tx_instructions(pubkey(), pubkey(), amount(), hash(),
                                       non_neg_integer(), fee(), nonce()
                                      ) -> [op()].
channel_withdraw_tx_instructions(ToPubkey, ChannelPubkey, Amount, StateHash,
                                 Round, Fee, Nonce) ->
    [ inc_account_nonce_op(ToPubkey, Nonce)
    , spend_fee_op(ToPubkey, Fee)
    , channel_withdraw_op(ToPubkey, ChannelPubkey, Amount, StateHash, Round)
    , tx_event_op({channel, ChannelPubkey})
    ].

-spec channel_settle_tx_instructions(pubkey(), pubkey(), amount(), amount(),
                                     fee(), nonce()) -> [op()].
channel_settle_tx_instructions(FromPubkey, ChannelPubkey,
                               InitiatorAmount, ResponderAmount, Fee, Nonce) ->
    [ inc_account_nonce_op(FromPubkey, Nonce)
    , spend_fee_op(FromPubkey, Fee)
    , channel_settle_op(FromPubkey, ChannelPubkey, InitiatorAmount, ResponderAmount)
    , tx_event_op({channel, ChannelPubkey})
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Instruction evaluation

int_eval(Instructions, S) ->
    try eval_instructions(Instructions, S)
    catch
        throw:{?MODULE, What} ->
            {error, What};
        error:{aeprimop_state, What} ->
            {error, What}
    end.

eval_instructions([I|Left], S) ->
    case eval_one(I, S) of
        #state{} = S1 ->
            eval_instructions(Left, S1);
        {return, Return, #state{} = S1} when Left =:= [] ->
            S2 = aeprimop_state:cache_write_through(S1),
            {ok, Return, S2};
        {return, _Return, #state{}} when Left =/= [] ->
            error(return_not_last)
    end;
eval_instructions([], S) ->
    S1 = aeprimop_state:cache_write_through(S),
    {ok, S1}.

eval_one({Op, Args}, S) ->
    case Op of
        inc_account_nonce         -> inc_account_nonce(Args, S);
        lock_amount               -> lock_amount(Args, S);
        channel_create            -> channel_create(Args, S);
        channel_deposit           -> channel_deposit(Args, S);
        channel_withdraw          -> channel_withdraw(Args, S);
        channel_close_mutual      -> channel_close_mutual(Args, S);
        channel_settle            -> channel_settle(Args, S);
        contract_call             -> contract_call(Args, S);
        contract_create           -> contract_create(Args, S);
        ga_attach                 -> ga_attach(Args, S);
        ga_meta                   -> ga_meta(Args, S);
        ga_set_meta_res           -> ga_set_meta_res(Args, S);
        name_claim                -> name_claim(Args, S);
        name_preclaim             -> name_preclaim(Args, S);
        name_revoke               -> name_revoke(Args, S);
        name_transfer             -> name_transfer(Args, S);
        name_update               -> name_update(Args, S);
        oracle_earn_query_fee     -> oracle_earn_query_fee(Args, S);
        oracle_extend             -> oracle_extend(Args, S);
        oracle_query              -> oracle_query(Args, S);
        oracle_register           -> oracle_register(Args, S);
        oracle_respond            -> oracle_respond(Args, S);
        resolve_account           -> resolve_account(Args, S);
        spend                     -> spend(Args, S);
        spend_fee                 -> spend_fee(Args, S);
        tx_event                  -> tx_event(Args, S);
        Other                     -> error({illegal_op, Other})
    end.

%%%===================================================================
%%% Operations
%%%

inc_account_nonce_op(Pubkey, Nonce) when ?IS_HASH(Pubkey),
                                         ?IS_NON_NEG_INTEGER(Nonce) ->
    {inc_account_nonce, {Pubkey, Nonce, false}}.

force_inc_account_nonce_op(Pubkey, Nonce) when ?IS_HASH(Pubkey),
                                               ?IS_NON_NEG_INTEGER(Nonce) ->
    {inc_account_nonce, {Pubkey, Nonce, true}}.

inc_account_nonce({Pubkey, Nonce, Force}, #state{} = S) ->
    {Account, S1} = get_account(Pubkey, S),
    case lists:member(Pubkey, aetx_env:ga_auth_ids(S#state.tx_env)) of
        true ->
            assert_ga_active(S),
            assert_generalized_account(Account),
            assert_ga_env(Pubkey, Nonce, S),
            S1;
        false ->
            assert_basic_account(Account),
            assert_account_nonce(Account, Nonce),
            case aetx_env:context(S#state.tx_env) of
                aetx_contract when (not Force) andalso
                                   S#state.protocol > ?ROMA_PROTOCOL_VSN ->
                    %% We have checked that the account exists, and that it has
                    %% the correct nonce. We are done here.
                    S1;
                _ ->
                    Account1 = aec_accounts:set_nonce(Account, Nonce),
                    put_account(Account1, S1)
            end
    end.

%%%-------------------------------------------------------------------

-spec spend_op(pubkey(), var_or_hash(), non_neg_integer()) -> op().
spend_op(From, To, Amount) when ?IS_HASH(From),
                                ?IS_VAR_OR_HASH(To),
                                ?IS_NON_NEG_INTEGER(Amount) ->
    {spend, {From, To, Amount}}.

spend({From, To, Amount}, #state{} = S) when is_integer(Amount), Amount >= 0 ->
    {Sender1, S1}   = get_account(From, S),
    assert_account_balance(Sender1, Amount),
    {ok, Sender2}   = aec_accounts:spend_without_nonce_bump(Sender1, Amount),
    S2              = put_account(Sender2, S1),
    {Receiver1, S3} = ensure_account(To, S2),
    {ok, Receiver2} = aec_accounts:earn(Receiver1, Amount),
    put_account(Receiver2, S3).

%%%-------------------------------------------------------------------
%%% A special form of spending is to lock an amount.

lock_amount_op(From, Amount) when ?IS_HASH(From),
                                  ?IS_NON_NEG_INTEGER(Amount) ->
    {lock_amount, {From, Amount}}.

lock_amount({From, Amount}, #state{} = S) ->
    {Account, S1} = get_account(From, S),
    assert_account_balance(Account, Amount),
    S2 = account_spend(Account, Amount, S1),
    int_lock_amount(Amount, S2).

%%%-------------------------------------------------------------------

spend_fee_op(From, Amount) when ?IS_HASH(From),
                                ?IS_NON_NEG_INTEGER(Amount) ->
    {spend_fee, {From, Amount}}.

spend_fee({From, Amount}, #state{} = S) when is_integer(Amount), Amount >= 0 ->
    {Sender1, S1}   = get_account(From, S),
    assert_account_balance(Sender1, Amount),
    {ok, Sender2}   = aec_accounts:spend_without_nonce_bump(Sender1, Amount),
    put_account(Sender2, S1).

%%%-------------------------------------------------------------------

resolve_account_op(GivenType, Hash, Var) when ?IS_NAME_RESOLVE_TYPE(GivenType),
                                              ?IS_HASH(Hash),
                                              ?IS_VAR(Var) ->
    {resolve_account, {GivenType, Hash, Var}}.

resolve_account({GivenType, Hash, Var}, S) ->
    resolve_name(account, GivenType, Hash, Var, S).

resolve_name(account, account, Pubkey, Var, S) ->
    aeprimop_state:set_var(Var, account, Pubkey, S);
resolve_name(account, name, NameHash, Var, S) ->
    {Pubkey, S1} = int_resolve_name(NameHash, S),
    aeprimop_state:set_var(Var, account, Pubkey, S1).

%%%-------------------------------------------------------------------

oracle_register_op(Pubkey, QFormat, RFormat, QFee,
                   DeltaTTL, ABIVersion) when ?IS_HASH(Pubkey),
                                             is_binary(QFormat),
                                             is_binary(RFormat),
                                             ?IS_NON_NEG_INTEGER(QFee),
                                             ?IS_NON_NEG_INTEGER(DeltaTTL),
                                             ?IS_NON_NEG_INTEGER(ABIVersion) ->
    {oracle_register, {Pubkey, QFormat, RFormat, QFee, DeltaTTL, ABIVersion}}.

oracle_register({Pubkey, QFormat, RFormat, QFee, DeltaTTL, ABIVersion}, S) ->
    assert_not_oracle(Pubkey, S),
    assert_oracle_abi_version(ABIVersion, S),
    assert_oracle_formats(QFormat, RFormat, ABIVersion, S),

    AbsoluteTTL = DeltaTTL + S#state.height,
    try aeo_oracles:new(Pubkey, QFormat, RFormat, QFee, AbsoluteTTL, ABIVersion) of
        Oracle -> put_oracle(Oracle, S)
    catch
        error:{illegal,_Field,_X} = Err ->
            lager:debug("Failed oracle register: ~p", [Err]),
            runtime_error(illegal_oracle_spec)
    end.

%%%-------------------------------------------------------------------

oracle_extend_op(Pubkey, DeltaTTL) when ?IS_HASH(Pubkey),
                                        ?IS_NON_NEG_INTEGER(DeltaTTL) ->
    {oracle_extend, {Pubkey, DeltaTTL}}.

oracle_extend({PubKey, DeltaTTL}, S) ->
    [runtime_error(zero_relative_oracle_extension_ttl) || DeltaTTL =:= 0],
    {Oracle, S1} = get_oracle(PubKey, account_is_not_an_active_oracle, S),
    Oracle1 = aeo_oracles:set_ttl(aeo_oracles:ttl(Oracle) + DeltaTTL, Oracle),
    put_oracle(Oracle1, S1).

%%%-------------------------------------------------------------------

oracle_query_op(OraclePubkey, SenderPubkey, SenderNonce, Query, QueryFee,
                QTTL, RTTL) when ?IS_HASH(OraclePubkey),
                                 ?IS_HASH(SenderPubkey),
                                 ?IS_NON_NEG_INTEGER(SenderNonce),
                                 is_binary(Query),
                                 ?IS_NON_NEG_INTEGER(QueryFee),
                                 ?IS_NON_NEG_INTEGER(QTTL),
                                 ?IS_NON_NEG_INTEGER(RTTL) ->
    {oracle_query, {OraclePubkey, SenderPubkey, SenderNonce,
                    Query, QueryFee, QTTL, RTTL}}.

oracle_query({OraclePubkey, SenderPubkey, SenderNonce,
             Query, QueryFee, QTTL, RTTL}, S) ->
    {Oracle, S1} = get_oracle(OraclePubkey, oracle_does_not_exist, S),
    assert_query_fee(Oracle, QueryFee),
    assert_query_ttl(Oracle, QTTL, RTTL, S),
    assert_oracle_format_match(Oracle, aeo_oracles:query_format(Oracle), Query),
    AbsoluteQTTL = S#state.height + QTTL,
    ResponseTTL = {delta, RTTL},
    try aeo_query:new(OraclePubkey, SenderPubkey, SenderNonce, Query, QueryFee,
                      AbsoluteQTTL, ResponseTTL) of
        QueryObject0 ->
            QueryObject =
                case aetx_env:ga_nonce(S#state.tx_env, SenderPubkey) of
                    {value, GANonce} ->
                        QId = aeo_query:ga_id(GANonce, OraclePubkey),
                        aeo_query:set_id(QId, QueryObject0);
                    none ->
                        QueryObject0
                end,
            assert_not_oracle_query(QueryObject, S),
            put_oracle_query(QueryObject, S1)
    catch
        error:{illegal,_Field,_X} = Err ->
            lager:debug("Failed oracle query: ~p", [Err]),
            runtime_error(illegal_oracle_query_spec)
    end.

%%%-------------------------------------------------------------------

oracle_respond_op(OraclePubkey, QueryId, Response, RTTL
                 ) when ?IS_HASH(OraclePubkey),
                        ?IS_HASH(QueryId),
                        ?IS_NON_NEG_INTEGER(RTTL) ->
    {oracle_respond, {OraclePubkey, QueryId, Response, RTTL}}.

oracle_respond({OraclePubkey, QueryId, Response, RTTL}, S) ->
    {QueryObject, S1} = get_oracle_query(OraclePubkey, QueryId, S),
    assert_oracle_response_ttl(QueryObject, RTTL),
    {Oracle, S2} = get_oracle(OraclePubkey, oracle_does_not_exist, S1),
    assert_query_belongs_to_oracle(QueryObject, OraclePubkey),
    assert_oracle_format_match(Oracle, aeo_oracles:response_format(Oracle), Response),
    assert_query_is_open(QueryObject),
    Height = S#state.height,
    QueryObject1 = aeo_query:add_response(Height, Response, QueryObject),
    put_oracle_query(QueryObject1, S2).

%%%-------------------------------------------------------------------

oracle_earn_query_fee_op(OraclePubkey, QueryId) when ?IS_HASH(OraclePubkey),
                                                     ?IS_HASH(QueryId) ->
    {oracle_earn_query_fee, {OraclePubkey, QueryId}}.

oracle_earn_query_fee({OraclePubkey, QueryId}, S) ->
    {Account, S1} = get_account(OraclePubkey, S),
    {Query, S2} = get_oracle_query(OraclePubkey, QueryId, S1),
    {ok, Account1} = aec_accounts:earn(Account, aeo_query:fee(Query)),
    put_account(Account1, S2).

%%%-------------------------------------------------------------------

name_preclaim_op(AccountPubkey, CommitmentHash, DeltaTTL
                ) when ?IS_HASH(AccountPubkey),
                       ?IS_HASH(CommitmentHash),
                       ?IS_NON_NEG_INTEGER(DeltaTTL) ->
    {name_preclaim, {AccountPubkey, CommitmentHash, DeltaTTL}}.

name_preclaim({AccountPubkey, CommitmentHash, DeltaTTL}, S) ->
    assert_not_commitment(CommitmentHash, S),
    Id      = aeser_id:create(commitment, CommitmentHash),
    OwnerId = aeser_id:create(account, AccountPubkey),
    Commitment = aens_commitments:new(Id, OwnerId, DeltaTTL, S#state.height),
    put_commitment(Commitment, S).

%%%-------------------------------------------------------------------

name_claim_op(AccountPubkey, PlainName, NameSalt, DeltaTTL, PreclaimDelta
             ) when ?IS_HASH(AccountPubkey),
                    is_binary(PlainName),
                    ?IS_NON_NEG_INTEGER(NameSalt),
                    ?IS_NON_NEG_INTEGER(DeltaTTL),
                    ?IS_NON_NEG_INTEGER(PreclaimDelta) ->
    {name_claim, {AccountPubkey, PlainName, NameSalt, DeltaTTL, PreclaimDelta}}.

name_claim({AccountPubkey, PlainName, NameSalt, DeltaTTL, PreclaimDelta}, S) ->
    NameAscii = name_to_ascii(PlainName),
    CommitmentHash = aens_hash:commitment_hash(NameAscii, NameSalt),
    {Commitment, S1} = get_commitment(CommitmentHash, name_not_preclaimed, S),
    assert_commitment_owner(Commitment, AccountPubkey),
    assert_preclaim_delta(Commitment, PreclaimDelta, S1#state.height),
    NameHash = aens_hash:name_hash(NameAscii),
    assert_not_name(NameHash, S1),
    Name = aens_names:new(NameHash, AccountPubkey, S1#state.height + DeltaTTL),
    S2 = delete_x(commitment, CommitmentHash, S1),
    put_name(Name, S2).

name_to_ascii(PlainName) ->
    case aens_utils:to_ascii(PlainName) of
        {error, What} ->
            runtime_error(What);
        {ok, NameAscii} ->
            NameAscii
    end.

%%%-------------------------------------------------------------------

name_revoke_op(AccountPubkey, NameHash, ProtectedDeltaTTL
              ) when ?IS_HASH(AccountPubkey),
                     ?IS_HASH(NameHash),
                     ?IS_NON_NEG_INTEGER(ProtectedDeltaTTL) ->
    {name_revoke, {AccountPubkey, NameHash, ProtectedDeltaTTL}}.

name_revoke({AccountPubkey, NameHash, ProtectedDeltaTTL}, S) ->
    {Name, S1} = get_name(NameHash, S),
    assert_name_owner(Name, AccountPubkey),
    assert_name_claimed(Name),
    Name1 = aens_names:revoke(Name, ProtectedDeltaTTL, S1#state.height),
    put_name(Name1, S1).

%%%-------------------------------------------------------------------

name_transfer_op(OwnerPubkey, RecipientType, RecipientHash, NameHash
                ) when ?IS_HASH(OwnerPubkey),
                       (RecipientType =:= name orelse RecipientType =:= account),
                       ?IS_HASH(RecipientHash),
                       ?IS_HASH(NameHash) ->
    {name_transfer, {OwnerPubkey, RecipientType, RecipientHash, NameHash}}.

name_transfer({OwnerPubkey, RecipientType, RecipientHash, NameHash}, S) ->
    {Name, S1} = get_name(NameHash, S),
    assert_name_owner(Name, OwnerPubkey),
    assert_name_claimed(Name),
    %% The check for the recipient is after the check for the name to give
    %% better error reporting.
    {RecipientPubkey, S2} =
        case RecipientType of
            account -> {RecipientHash, S1};
            name    -> int_resolve_name(RecipientHash, S1)
        end,
    Name1 = aens_names:transfer_to(RecipientPubkey, Name),
    put_name(Name1, S2).

%%%-------------------------------------------------------------------

name_update_op(OwnerPubkey, NameHash, DeltaTTL, MaxTTL, ClientTTL, Pointers
              ) when ?IS_HASH(OwnerPubkey),
                     ?IS_HASH(NameHash),
                     ?IS_NON_NEG_INTEGER(DeltaTTL),
                     ?IS_NON_NEG_INTEGER(MaxTTL),
                     ?IS_NON_NEG_INTEGER(ClientTTL),
                     is_list(Pointers) ->
    {name_update , {OwnerPubkey, NameHash, DeltaTTL, MaxTTL, ClientTTL, Pointers}}.

name_update({OwnerPubkey, NameHash, DeltaTTL, MaxTTL, ClientTTL, Pointers}, S) ->
    [runtime_error(ttl_too_high) || DeltaTTL > MaxTTL],
    {Name, S1} = get_name(NameHash, S),
    assert_name_owner(Name, OwnerPubkey),
    assert_name_claimed(Name),
    AbsoluteTTL = S#state.height + DeltaTTL,
    Name1 = aens_names:update(Name, AbsoluteTTL, ClientTTL, Pointers),
    put_name(Name1, S1).

%%%-------------------------------------------------------------------

channel_create_op(InitiatorPubkey, InitiatorAmount,
                  ResponderPubkey, ResponderAmount,
                  ReserveAmount, DelegatePubkeys,
                  StateHash, LockPeriod, Nonce, Round
                 ) when ?IS_HASH(InitiatorPubkey),
                        ?IS_NON_NEG_INTEGER(InitiatorAmount),
                        ?IS_HASH(ResponderPubkey),
                        ?IS_NON_NEG_INTEGER(ReserveAmount),
                        ?IS_NON_NEG_INTEGER(ReserveAmount),
                        is_list(DelegatePubkeys),
                        ?IS_HASH(StateHash),
                        ?IS_NON_NEG_INTEGER(LockPeriod),
                        ?IS_NON_NEG_INTEGER(Nonce),
                        ?IS_NON_NEG_INTEGER(Round) ->
    true = lists:all(fun(X) -> ?IS_HASH(X) end, DelegatePubkeys),
    {channel_create, {InitiatorPubkey, InitiatorAmount,
                      ResponderPubkey, ResponderAmount,
                      ReserveAmount, DelegatePubkeys,
                      StateHash, LockPeriod, Nonce, Round}}.

channel_create({InitiatorPubkey, InitiatorAmount,
                ResponderPubkey, ResponderAmount,
                ReserveAmount, DelegatePubkeys,
                StateHash, LockPeriod, Nonce0, Round}, S) ->
    assert_channel_reserve_amount(ReserveAmount, InitiatorAmount,
                                  ResponderAmount),
    assert_not_equal(InitiatorPubkey, ResponderPubkey, initiator_is_responder),
    Nonce = case aetx_env:ga_nonce(S#state.tx_env, InitiatorPubkey) of
                {value, NonceX} -> NonceX;
                none            -> Nonce0
            end,
    {InitAccount, S1} = get_account(InitiatorPubkey, S),
    {RespAccount, S2} = get_account(ResponderPubkey, S1),
    Channel = aesc_channels:new(InitiatorPubkey, InitiatorAmount,
                                ResponderPubkey, ResponderAmount,
                                InitAccount, RespAccount,
                                ReserveAmount, DelegatePubkeys,
                                StateHash, LockPeriod, Nonce,
                                S#state.height, Round),
    ChannelPubkey = aesc_channels:pubkey(Channel),
    assert_not_channel(ChannelPubkey, S2),
    S3 = copy_contract_state_for_auth(Channel, InitAccount, S2),
    S4 = copy_contract_state_for_auth(Channel, RespAccount, S3),
    put_channel(Channel, S4).

copy_contract_state_for_auth(Ch, Account, S) ->
    case aec_accounts:type(Account) of
        basic -> S;
        generalized ->
            {_, ContractPK} = aeser_id:specialize(aec_accounts:ga_contract(Account)),
            {Contract, S1 = #state{trees = Trees}} = get_contract(ContractPK, S),
            StoreKey = aesc_channels:auth_store_key(aec_accounts:id(Account), Ch),
            CtTree  = aec_trees:contracts(Trees),
            CtTree1 =  aect_state_tree:copy_contract_store(Contract, StoreKey, CtTree),
            S1#state{trees = aec_trees:set_contracts(Trees, CtTree1)}
    end.

%%%-------------------------------------------------------------------

channel_deposit_op(FromPubkey, ChannelPubkey, Amount, StateHash, Round) ->
    {channel_deposit, {FromPubkey, ChannelPubkey, Amount, StateHash, Round}}.

channel_deposit({FromPubkey, ChannelPubkey, Amount, StateHash, Round}, S) ->
    {Channel, S1} = get_channel(ChannelPubkey, S),
    assert_channel_active(Channel),
    assert_is_channel_peer(Channel, FromPubkey),
    assert_channel_round(Channel, Round, deposit),
    Channel1 = aesc_channels:deposit(Channel, Amount, Round, StateHash),
    put_channel(Channel1, S1).

%%%-------------------------------------------------------------------

channel_withdraw_op(ToPubkey, ChannelPubkey, Amount, StateHash, Round
                   ) when ?IS_HASH(ToPubkey),
                          ?IS_HASH(ChannelPubkey),
                          ?IS_NON_NEG_INTEGER(Amount),
                          ?IS_HASH(StateHash),
                          ?IS_NON_NEG_INTEGER(Round) ->
    {channel_withdraw, {ToPubkey, ChannelPubkey, Amount, StateHash, Round}}.

channel_withdraw({ToPubkey, ChannelPubkey, Amount, StateHash, Round}, S) ->
    {Channel, S1} = get_channel(ChannelPubkey, S),
    assert_channel_active(Channel),
    assert_is_channel_peer(Channel, ToPubkey),
    assert_channel_withdraw_amount(Channel, Amount),
    assert_channel_round(Channel, Round, withdrawal),
    Channel1 = aesc_channels:withdraw(Channel, Amount, Round, StateHash),
    {Account, S2} = get_account(ToPubkey, S1),
    S3 = account_earn(Account, Amount, S2),
    put_channel(Channel1, S3).

%%%-------------------------------------------------------------------

channel_close_mutual_op(FromPubkey, ChannelPubkey,
                        InitiatorAmount, ResponderAmount, Fee
                       ) when ?IS_HASH(FromPubkey),
                              ?IS_HASH(ChannelPubkey),
                              ?IS_NON_NEG_INTEGER(InitiatorAmount),
                              ?IS_NON_NEG_INTEGER(ResponderAmount),
                              ?IS_NON_NEG_INTEGER(Fee) ->
    {channel_close_mutual, {FromPubkey, ChannelPubkey,
                            InitiatorAmount, ResponderAmount, Fee}}.

channel_close_mutual({FromPubkey, ChannelPubkey,
                      InitiatorAmount, ResponderAmount, Fee}, S) ->
    {Channel, S1} = get_channel(ChannelPubkey, S),
    assert_channel_origin(Channel, FromPubkey),
    assert_channel_active(Channel),
    TotalAmount = InitiatorAmount + ResponderAmount + Fee,
    ChannelAmount = aesc_channels:channel_amount(Channel),
    LockAmount = ChannelAmount - TotalAmount,
    assert(LockAmount >= 0, wrong_amounts),
    {IAccount, S2} = get_account(aesc_channels:initiator_pubkey(Channel), S1),
    {RAccount, S3} = get_account(aesc_channels:responder_pubkey(Channel), S2),
    S4 = account_earn(IAccount, InitiatorAmount, S3),
    S5 = account_earn(RAccount, ResponderAmount, S4),
    S6 = int_lock_amount(LockAmount, S5),
    delete_x(channel, ChannelPubkey, S6).

%%%-------------------------------------------------------------------

channel_settle_op(FromPubkey, ChannelPubkey, InitiatorAmount, ResponderAmount
                 ) when ?IS_HASH(FromPubkey),
                        ?IS_HASH(ChannelPubkey),
                        ?IS_NON_NEG_INTEGER(InitiatorAmount),
                        ?IS_NON_NEG_INTEGER(ResponderAmount) ->
    {channel_settle, {FromPubkey, ChannelPubkey,
                      InitiatorAmount, ResponderAmount}}.

channel_settle({FromPubkey, ChannelPubkey,
                InitiatorAmount, ResponderAmount}, S) ->
    {Channel, S1} = get_channel(ChannelPubkey, S),
    assert_is_channel_peer(Channel, FromPubkey),
    assert_channel_is_solo_closed(Channel, S),
    TotalAmount = InitiatorAmount + ResponderAmount,
    ChannelAmount = aesc_channels:channel_amount(Channel),
    LockAmount = ChannelAmount - TotalAmount,
    assert(LockAmount >= 0, insufficient_channel_funds),
    assert(InitiatorAmount =:= aesc_channels:initiator_amount(Channel),
           wrong_amt),
    assert(ResponderAmount =:= aesc_channels:responder_amount(Channel),
           wrong_amt),
    ResponderPubkey = aesc_channels:responder_pubkey(Channel),
    {ResponderAccount, S2} = get_account(ResponderPubkey, S1),
    InitiatorPubkey = aesc_channels:initiator_pubkey(Channel),
    {InitiatorAccount, S3} = get_account(InitiatorPubkey, S2),
    S4 = account_earn(ResponderAccount, ResponderAmount, S3),
    S5 = account_earn(InitiatorAccount, InitiatorAmount, S4),
    S6 = int_lock_amount(LockAmount, S5),
    delete_x(channel, ChannelPubkey, S6).

%%%-------------------------------------------------------------------

contract_call_op(CallerPubKey, ContractPubkey, CallData, GasLimit, GasPrice,
                 Amount, ABIVersion, Origin, CallStack, Fee, Nonce
                ) when ?IS_HASH(CallerPubKey),
                       ?IS_HASH(ContractPubkey),
                       is_binary(CallData),
                       ?IS_NON_NEG_INTEGER(GasLimit),
                       ?IS_NON_NEG_INTEGER(GasPrice),
                       ?IS_NON_NEG_INTEGER(Amount),
                       ?IS_NON_NEG_INTEGER(ABIVersion),
                       ?IS_HASH(Origin),
                       is_list(CallStack),
                       ?IS_NON_NEG_INTEGER(Fee),
                       ?IS_NON_NEG_INTEGER(Nonce) ->
    {contract_call, {CallerPubKey, ContractPubkey, CallData, GasLimit, GasPrice,
                     Amount, ABIVersion, Origin, CallStack, Fee, Nonce}}.

contract_call({CallerPubKey, ContractPubkey, CallData, GasLimit, GasPrice,
               Amount, ABIVersion, Origin, CallStack, Fee, Nonce}, S) ->
    {CallerId, TotalAmount} = get_call_env_specific(CallerPubKey, GasLimit,
                                                    GasPrice, Amount, Fee, S),
    {CallerAccount, S1} = get_account(CallerPubKey, S),
    assert_account_balance(CallerAccount, TotalAmount),
    assert_contract_call_version(ContractPubkey, ABIVersion, S),
    assert_contract_call_stack(CallStack, S),
    Context = aetx_env:context(S#state.tx_env),
    S2 = case Context of
             aetx_contract ->
                 %% Contract as callers only bump nonce at this point.
                 %% For consensus compatibility.
                 assert_account_nonce(CallerAccount, Nonce),
                 CallerAccount1 = aec_accounts:set_nonce(CallerAccount, Nonce),
                 account_spend(CallerAccount1, TotalAmount, S1);
             Other when Other == aetx_transaction; Other == aetx_ga ->
                 account_spend(CallerAccount, TotalAmount, S1)
         end,
    {ContractAccount, S3} = get_account(ContractPubkey, S2),
    S4 = account_earn(ContractAccount, Amount, S3),
    %% Avoid writing the store back by skipping this state.
    {Contract, _} = get_contract(ContractPubkey, S4),
    {Call, S5} = run_contract(CallerId, Contract, GasLimit, GasPrice,
                              CallData, Origin, Amount, CallStack, Nonce, S4),
    case aect_call:return_type(Call) of
        ok ->
            case Context of
                aetx_contract ->
                    {return, Call, S5}; %% Return instead of store
                Other2 when Other2 == aetx_transaction; Other2 == aetx_ga ->
                    contract_call_success(Call, GasLimit, S5)
            end;
        Fail when (Fail =:= revert orelse Fail =:= error) ->
            case Context of
                aetx_contract ->
                    {return, Call, S}; %% Return instead of store
                Other2 when Other2 == aetx_transaction; Other2 == aetx_ga ->
                    contract_call_fail(Call, Fee, S)
            end
    end.

get_call_env_specific(CallerPubKey, GasLimit, GasPrice, Amount, Fee, S) ->
    case aetx_env:context(S#state.tx_env) of
        aetx_contract ->
            {aeser_id:create(contract, CallerPubKey), Amount};
        Other when Other == aetx_transaction; Other == aetx_ga ->
            {aeser_id:create(account, CallerPubKey), Fee + GasLimit * GasPrice + Amount}
    end.

%%%-------------------------------------------------------------------

ga_attach_op(OwnerPubkey, GasLimit, GasPrice, ABIVersion, VMVersion,
             SerializedCode, AuthFun, CallData, Fee, Nonce
            ) when ?IS_HASH(OwnerPubkey),
                   ?IS_NON_NEG_INTEGER(GasLimit),
                   ?IS_NON_NEG_INTEGER(GasPrice),
                   ?IS_NON_NEG_INTEGER(ABIVersion),
                   ?IS_NON_NEG_INTEGER(VMVersion),
                   is_binary(SerializedCode),
                   is_binary(CallData),
                   ?IS_NON_NEG_INTEGER(Fee),
                   ?IS_NON_NEG_INTEGER(Nonce) ->
    {ga_attach,
     {OwnerPubkey, GasLimit, GasPrice, ABIVersion, VMVersion,
      SerializedCode, AuthFun, CallData, Fee, Nonce}}.

ga_attach({OwnerPubkey, GasLimit, GasPrice, ABIVersion,
           VMVersion, SerializedCode, AuthFun, CallData, Fee, Nonce}, S) ->
    assert_ga_active(S),
    RollbackS = S,
    TotalAmount    = Fee + GasLimit * GasPrice,
    {Account, S1}  = get_account(OwnerPubkey, S),
    assert_basic_account(Account), %% No re-attach
    assert_account_balance(Account, TotalAmount),
    assert_ga_create_version(ABIVersion, VMVersion, S),
    assert_ga_attach_byte_code(ABIVersion, SerializedCode, CallData, AuthFun, S),
    %% Charge the fee, the gas (the unused portion will be refunded)
    %% and the deposit (stored in the contract) to the contract owner (caller),
    CTVersion      = #{vm => VMVersion, abi => ABIVersion},
    S2             = account_spend(Account, TotalAmount, S1),
    Contract       = aect_contracts:new(OwnerPubkey, Nonce, CTVersion,
                                        SerializedCode, 0),
    ContractPubkey  = aect_contracts:pubkey(Contract),
    {_CAccount, S3} = ensure_account(ContractPubkey, S2),
    OwnerId         = aect_contracts:owner_id(Contract),
    {InitCall, S4}  = run_contract(OwnerId, Contract, GasLimit, GasPrice,
                                   CallData, OwnerPubkey, _InitAmount = 0,
                                   _CallStack = [], Nonce, S3),
    case aect_call:return_type(InitCall) of
        error ->
            contract_call_fail(InitCall, Fee, RollbackS);
        revert ->
            contract_call_fail(InitCall, Fee, RollbackS);
        ok ->
            contract_init_call_success({attach, AuthFun}, InitCall, Contract,
                                       GasLimit, Fee, RollbackS, S4)
    end.

ga_set_meta_res_op(OwnerPubkey, AuthData, Res) ->
    {ga_set_meta_res, {OwnerPubkey, AuthData, Res}}.

ga_set_meta_res({OwnerPubkey, AuthData, Res}, S) ->
    {Account, S1} = get_account(OwnerPubkey, S),
    assert_generalized_account(Account),

    GAContractId             = aec_accounts:ga_contract(Account),
    {contract, GAContractPK} = aeser_id:specialize(GAContractId),
    AuthId                   = aega_meta_tx:auth_id(OwnerPubkey, AuthData),
    AuthCallId               = aect_call:ga_id(AuthId, GAContractPK),
    {AuthCall, S2}           = get_auth_call(OwnerPubkey, AuthCallId, S1),

    AuthCall1 =
        case Res of
            ok ->
                aect_call:set_return_type(ok, AuthCall);
            {error, Reason} ->
                aect_call:set_return_type(error,
                    aect_call:set_return_value(error_to_binary(Reason), AuthCall))
        end,
    put_auth_call(AuthCall1, S2).

error_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
error_to_binary(Bin) when is_binary(Bin) ->
    Bin;
error_to_binary(_) ->
    <<"unknown_error">>.

ga_meta_op(OwnerPubkey, AuthData, ABIVersion, GasLimit, GasPrice, Fee
          ) when ?IS_HASH(OwnerPubkey),
                 ?IS_NON_NEG_INTEGER(GasLimit),
                 ?IS_NON_NEG_INTEGER(GasPrice),
                 ?IS_NON_NEG_INTEGER(ABIVersion),
                 is_binary(AuthData),
                 ?IS_NON_NEG_INTEGER(Fee) ->
    {ga_meta,
     {OwnerPubkey, AuthData, ABIVersion, GasLimit, GasPrice, Fee}}.

ga_meta({OwnerPK, AuthData, ABIVersion, GasLimit, GasPrice, Fee}, S) ->
    assert_ga_active(S),
    {Account, S1} = get_account(OwnerPK, S),
    assert_generalized_account(Account),
    CheckAmount = Fee + GasLimit * GasPrice,
    assert_account_balance(Account, CheckAmount),
    AuthContract = aec_accounts:ga_contract(Account),
    {contract, AuthContractPK} = aeser_id:specialize(AuthContract),
    AuthFunHash = aec_accounts:ga_auth_fun(Account),

    assert_contract_call_version(AuthContractPK, ABIVersion, S),
    assert_auth_data_function(AuthData, AuthFunHash),
    S2 = account_spend(Account, CheckAmount, S1),
    {Contract, _} = get_contract(AuthContractPK, S2),
    CallerId   = aeser_id:create(account, OwnerPK),
    {Call, S3} = run_contract(CallerId, Contract, GasLimit, GasPrice,
                              AuthData, OwnerPK, _Amount = 0, _CallStack = [], _Nonce = 0, S2),
    case aect_call:return_type(Call) of
        ok ->
            case aeb_heap:from_binary(word, aect_call:return_value(Call)) of
                {ok, 1} -> %% true!
                    Refund = (GasLimit - aect_call:gas_used(Call)) * aect_call:gas_price(Call),
                    {CallerAccount, S4} = get_account(OwnerPK, S3),
                    S5 = account_earn(CallerAccount, Refund, S4),
                    AuthId = aega_meta_tx:auth_id(OwnerPK, AuthData),
                    AuthCallId = aect_call:ga_id(AuthId, aect_call:contract_pubkey(Call)),
                    Call1 = aect_call:set_id(AuthCallId, Call),
                    assert_auth_call_object_not_exist(Call1, S5),
                    put_auth_call(Call1, S5);
                {ok, 0} -> %% false
                    runtime_error(authentication_failed);
                {error, E} ->
                    lager:info("Unexpected authentication return_value ~p (~p)",
                               [aect_call:return_value(Call), E]),
                    runtime_error(authentication_failed)
            end;
        Fail when (Fail =:= revert orelse Fail =:= error) ->
            runtime_error(authentication_failed)
    end.

%%%-------------------------------------------------------------------

contract_create_op(OwnerPubkey, Amount, Deposit, GasLimit,
                   GasPrice, ABIVersion, VMVersion, SerializedCode,
                   CallData, Fee, Nonce
                  ) when ?IS_HASH(OwnerPubkey),
                         ?IS_NON_NEG_INTEGER(Amount),
                         ?IS_NON_NEG_INTEGER(Deposit),
                         ?IS_NON_NEG_INTEGER(GasLimit),
                         ?IS_NON_NEG_INTEGER(GasPrice),
                         ?IS_NON_NEG_INTEGER(ABIVersion),
                         ?IS_NON_NEG_INTEGER(VMVersion),
                         is_binary(SerializedCode),
                         is_binary(CallData),
                         ?IS_NON_NEG_INTEGER(Fee),
                         ?IS_NON_NEG_INTEGER(Nonce) ->
    {contract_create,
     {OwnerPubkey, Amount, Deposit, GasLimit,
      GasPrice, ABIVersion, VMVersion, SerializedCode,
      CallData, Fee, Nonce}}.

contract_create({OwnerPubkey, Amount, Deposit, GasLimit, GasPrice,
                 ABIVersion, VMVersion, SerializedCode, CallData, Fee, Nonce0},
                S) ->
    RollbackS = S,
    TotalAmount    = Amount + Deposit + Fee + GasLimit * GasPrice,
    {Account, S1}  = get_account(OwnerPubkey, S),
    assert_account_balance(Account, TotalAmount),
    assert_contract_create_version(ABIVersion, VMVersion, S),
    assert_contract_byte_code(ABIVersion, SerializedCode, CallData, S),
    %% Charge the fee, the gas (the unused portion will be refunded)
    %% and the deposit (stored in the contract) to the contract owner (caller),
    %% and transfer the funds (amount) to the contract account.
    CTVersion      = #{vm => VMVersion, abi => ABIVersion},
    S2             = account_spend(Account, TotalAmount, S1),
    Nonce = case aetx_env:ga_nonce(S#state.tx_env, OwnerPubkey) of
                {value, NonceX} -> NonceX;
                none            -> Nonce0
            end,
    Contract       = aect_contracts:new(OwnerPubkey, Nonce, CTVersion,
                                        SerializedCode, Deposit),
    ContractPubkey = aect_contracts:pubkey(Contract),
    {CAccount, S3} = ensure_account(ContractPubkey, S2),
    S4             = account_earn(CAccount, Amount, S3),
    OwnerId        = aect_contracts:owner_id(Contract),
    {InitCall, S5} = run_contract(OwnerId, Contract, GasLimit, GasPrice,
                                  CallData, OwnerPubkey, _InitAmount = 0,
                                  _CallStack = [], Nonce0, S4),
    case aect_call:return_type(InitCall) of
        error ->
            contract_call_fail(InitCall, Fee, RollbackS);
        revert ->
            contract_call_fail(InitCall, Fee, RollbackS);
        ok ->
            contract_init_call_success(contract, InitCall, Contract,
                                       GasLimit, Fee, RollbackS, S5)
    end.

%%%-------------------------------------------------------------------

tx_event_op(Name) ->
    {tx_event, Name}.

tx_event(Name, #state{tx_env = Env} = S) ->
    S#state{tx_env = aetx_env:tx_event(Name, Env)}.

%%%-------------------------------------------------------------------

contract_init_call_success(Type, InitCall, Contract, GasLimit, Fee, RollbackS, S) ->
    ReturnValue = aect_call:return_value(InitCall),
    %% The return value is cleared for successful init calls.
    InitCall1   = aect_call:set_return_value(<<>>, InitCall),
    case aect_contracts:ct_version(Contract) of
        #{vm := V} = CTVersion when ?IS_AEVM_SOPHIA(V) ->
            %% Set the initial state of the contract
            case aevm_eeevm_store:from_sophia_state(CTVersion, ReturnValue) of
                {ok, Store} ->
                    Contract1 = aect_contracts:set_state(Store, Contract),
                    S1 = put_contract(Contract1, S),
                    case Type of
                        contract ->
                            contract_call_success(InitCall1, GasLimit, S1);
                        {attach, AuthFun} ->
                            ga_attach_success(InitCall1, GasLimit, AuthFun, S1)
                    end;
                {error, _} ->
                    FailCall0 = aect_call:set_return_value(<<"out_of_gas">>, InitCall),
                    FailCall  = aect_call:set_return_type(error, FailCall0),
                    contract_call_fail(FailCall, Fee, RollbackS)
            end;
        #{vm := V} when ?IS_FATE_SOPHIA(V) ->
            %% TODO: For now just use the initial store since the store is not implemented yet.
            S1 = put_contract(Contract, S),
            contract_call_success(InitCall1, GasLimit, S1);
        #{vm := ?VM_AEVM_SOLIDITY_1} ->
            %% Solidity inital call returns the code to store in the contract.
            Contract1 = aect_contracts:set_code(ReturnValue, Contract),
            S1 = put_contract(Contract1, S),
            contract_call_success(InitCall1, GasLimit, S1)
    end.

%%%===================================================================
%%% Helpers for instructions

set_call_object_id(Call, #state{ tx_env = TxEnv }) ->
    case aetx_env:ga_nonce(TxEnv, aect_call:caller_pubkey(Call)) of
        {value, Nonce} ->
            CallId = aect_call:ga_id(Nonce, aect_call:contract_pubkey(Call)),
            aect_call:set_id(CallId, Call);
        none ->
            Call
    end.

contract_call_success(Call, GasLimit, S) ->
    Refund = (GasLimit - aect_call:gas_used(Call)) * aect_call:gas_price(Call),
    {CallerAccount, S1} = get_account(aect_call:caller_pubkey(Call), S),
    S2 = account_earn(CallerAccount, Refund, S1),
    put_call(set_call_object_id(Call, S2), S2).

contract_call_fail(Call, Fee, S) ->
    S1 = put_call(set_call_object_id(Call, S), S),
    UsedAmount = aect_call:gas_used(Call) * aect_call:gas_price(Call) + Fee,
    S2 = case S#state.protocol >= ?FORTUNA_PROTOCOL_VSN of
             true  -> S1;
             false ->
                 %% For backwards compatibility, the account of the contract
                 %% needs to be created
                 {_, S20} = ensure_account(aect_call:contract_pubkey(Call), S1),
                 S20
         end,
    {Account, S3} = get_account(aect_call:caller_pubkey(Call), S2),
    account_spend(Account, UsedAmount, S3).

run_contract(CallerId, Contract, GasLimit, GasPrice, CallData, Origin, Amount,
             CallStack, Nonce, S) ->
    %% We need to push all to the trees before running a contract.
    S1 = aeprimop_state:cache_write_through(S),
    ContractId = aect_contracts:id(Contract),
    Call = aect_call:new(CallerId, Nonce, ContractId, S#state.height, GasPrice),
    {_, CallerPubKey} = aeser_id:specialize(CallerId),
    CallDef = #{ caller      => CallerPubKey
               , contract    => aect_contracts:pubkey(Contract)
               , gas         => GasLimit
               , gas_price   => GasPrice
               , call_data   => CallData
               , amount      => Amount
               , call_stack  => CallStack
               , code        => aect_contracts:code(Contract)
               , store       => aect_contracts:state(Contract)
               , call        => Call
               , trees       => S1#state.trees
               , tx_env      => S1#state.tx_env
               , off_chain   => false
               , origin      => Origin
               },
    CTVersion = aect_contracts:ct_version(Contract),
    {Call1, Trees1, Env1} = aect_dispatch:run(CTVersion, CallDef),
    {Call1, S1#state{trees = Trees1, tx_env = Env1}}.

ga_attach_success(Call, GasLimit, AuthFun, S) ->
    Refund = (GasLimit - aect_call:gas_used(Call)) * aect_call:gas_price(Call),
    {CallerAccount, S1} = get_account(aect_call:caller_pubkey(Call), S),
    Contract = aeser_id:create(contract, aect_call:contract_pubkey(Call)),
    {ok, CallerAccount1} = aec_accounts:attach_ga_contract(CallerAccount, Contract, AuthFun),
    S2 = account_earn(CallerAccount1, Refund, S1),
    put_call(set_call_object_id(Call, S2), S2).

int_lock_amount(0, #state{} = S) ->
    %% Don't risk creating an account for the locked amount if there is none.
    S;
int_lock_amount(Amount, S) when ?IS_NON_NEG_INTEGER(Amount) ->
    LockPubkey = aec_governance:locked_coins_holder_account(),
    {Account, S1} = ensure_account(LockPubkey, S),
    account_earn(Account, Amount, S1).

int_resolve_name(NameHash, S) ->
    Key = <<"account_pubkey">>,
    {Name, S1} = get_name(NameHash, S),
    case aens:resolve_from_name_object(Key, Name) of
        {ok, Id} ->
            %% Intentionally admissive to allow for all kinds of IDs for
            %% backwards compatibility.
            {_Tag, Pubkey} = aeser_id:specialize(Id),
            {Pubkey, S1};
        {error, What} ->
            runtime_error(What)
    end.


account_earn(Account, Amount, S) ->
    {ok, Account1} = aec_accounts:earn(Account, Amount),
    put_account(Account1, S).

account_spend(Account, Amount, S) ->
    {ok, Account1} = aec_accounts:spend_without_nonce_bump(Account, Amount),
    put_account(Account1, S).

specialize_account(RecipientID) ->
    case aeser_id:specialize(RecipientID) of
        {name, NameHash}   -> {name, NameHash};
        {oracle, Pubkey}   -> {account, Pubkey};
        {contract, Pubkey} -> {account, Pubkey};
        {account, Pubkey}  -> {account, Pubkey}
    end.

assert(true,_Error) -> ok;
assert(false, Error) -> runtime_error(Error).

assert_not_equal(X, X, Error) -> runtime_error(Error);
assert_not_equal(_, _, _) -> ok.

assert_basic_account(Account) ->
    case aec_accounts:type(Account) of
        basic       -> ok;
        generalized -> runtime_error(not_a_basic_account)
    end.

assert_generalized_account(Account) ->
    case aec_accounts:type(Account) of
        generalized -> ok;
        basic       -> runtime_error(not_a_generalized_account)
    end.

assert_ga_env(Pubkey, Nonce, #state{tx_env = Env}) ->
    GANonce = aetx_env:ga_nonce(Env, Pubkey),
    if Nonce =/= 0     -> runtime_error(nonce_in_ga_tx_should_be_zero);
       GANonce == none -> runtime_error(bad_ga_tx_env);
       true            -> ok
    end.

assert_account_nonce(Account, Nonce) ->
    case aec_accounts:nonce(Account) of
        N when N + 1 =:= Nonce -> ok;
        N when N >= Nonce -> runtime_error(account_nonce_too_high);
        N when N < Nonce  -> runtime_error(account_nonce_too_low)
    end.

assert_account_balance(Account, Balance) ->
    case aec_accounts:balance(Account) of
        B when B >= Balance -> ok;
        B when B <  Balance -> runtime_error(insufficient_funds)
    end.

ensure_account(Key, #state{} = S) ->
    case find_account(Key, S) of
        none ->
            Pubkey = get_var(Key, account, S),
            Account = aec_accounts:new(Pubkey, 0),
            {Account, put_account(Account, S)};
        {Account, S1} ->
            {Account, S1}
    end.

assert_not_oracle(Pubkey, S) ->
    case find_oracle(Pubkey, S) of
        {_, _} -> runtime_error(account_is_already_an_oracle);
        none -> ok
    end.

assert_oracle_abi_version(ABIVersion, S) ->
    CTVersion = #{abi => ABIVersion, vm => 0}, %% VM version not used in check.
    Height = S#state.height,
    case aect_contracts:is_legal_version_at_height(oracle_register, CTVersion, Height) of
        true  -> ok;
        false -> runtime_error(bad_abi_version)
    end.

assert_oracle_formats(_QFormat,_RFormat, ?ABI_NO_VM,_S) ->
    %% The format strings can be anything
    ok;
assert_oracle_formats(QFormat, RFormat, ?ABI_AEVM_SOPHIA_1, S) ->
    case S#state.protocol >= ?MINERVA_PROTOCOL_VSN of
        false ->
            %% Backwards compatible: no check of this.
            ok;
        true ->
            assert_typerep_format(QFormat, bad_query_format),
            assert_typerep_format(RFormat, bad_response_format)
    end.

assert_typerep_format(Format, Error) ->
    try aeb_heap:from_binary(typerep, Format) of
        {ok, TypeRep} ->
            case aeb_heap:to_binary(TypeRep) of
                Format -> ok;
                _      -> runtime_error(Error)
            end;
        {error, _}->
            runtime_error(Error)
    catch _:_ ->
            runtime_error(Error)
    end.

assert_query_fee(Oracle, QueryFee) ->
    case QueryFee >= aeo_oracles:query_fee(Oracle) of
        true  -> ok;
        false -> runtime_error(query_fee_too_low)
    end.

assert_not_oracle_query(Query, S) ->
    OraclePubkey = aeo_query:oracle_pubkey(Query),
    QueryId  = aeo_query:id(Query),
    case find_oracle_query(OraclePubkey, QueryId, S) of
        {_, _} -> runtime_error(oracle_query_already_present);
        none -> ok
    end.

assert_query_ttl(Oracle, QTTL, RTTL, S) ->
    OracleTTL = aeo_oracles:ttl(Oracle),
    case S#state.height + QTTL + RTTL > OracleTTL of
        true  -> runtime_error(too_long_ttl);
        false -> ok
    end.

assert_oracle_format_match(Oracle, Format, Content) ->
    case aeo_oracles:abi_version(Oracle) of
        ?ABI_NO_VM ->
            %% No interpretation of the format, nor content.
            ok;
        ?ABI_AEVM_SOPHIA_1 ->
            %% Check that the content can be decoded as the type
            %% and that if we encoded it again, it becomes the content.
            {ok, TypeRep} = aeb_heap:from_binary(typerep, Format),
            try aeb_heap:from_binary(TypeRep, Content) of
                {ok, Res} ->
                    case aeb_heap:to_binary(Res) of
                        Content -> ok;
                        _Other -> runtime_error(bad_format)
                    end;
                {error, _} ->
                    runtime_error(bad_format)
            catch _:_ ->
                    runtime_error(bad_format)
            end
    end.

assert_oracle_response_ttl(QueryObject, RTTL) ->
    {delta, QRTTL} = aeo_query:response_ttl(QueryObject),
    case QRTTL =:= RTTL of
        true  -> ok;
        false -> runtime_error(oracle_response_has_wrong_response_ttl)
    end.

assert_query_belongs_to_oracle(QueryObject, OraclePubkey) ->
    case aeo_query:oracle_pubkey(QueryObject) =:= OraclePubkey of
        true  -> ok;
        false -> runtime_error(oracle_does_not_match_query_id)
    end.

assert_query_is_open(QueryObject) ->
    case aeo_query:is_open(QueryObject) of
        true  -> ok;
        false -> runtime_error(oracle_closed_for_response)
    end.

assert_not_commitment(CommitmentHash, S) ->
    case find_commitment(CommitmentHash, S) of
        none -> ok;
        {_, _} -> runtime_error(commitment_already_present)
    end.

assert_commitment_owner(Commitment, Pubkey) ->
    case aens_commitments:owner_pubkey(Commitment) =:= Pubkey of
        true  -> ok;
        false -> runtime_error(commitment_not_owned)
    end.

assert_preclaim_delta(Commitment, PreclaimDelta, Height) ->
    case aens_commitments:created(Commitment) + PreclaimDelta =< Height of
        true  -> ok;
        false -> runtime_error(commitment_delta_too_small)
    end.

assert_not_name(NameHash, S) ->
    case find_name(NameHash, S) of
        {_, _} -> runtime_error(name_already_taken);
        none   -> ok
    end.

assert_name_owner(Name, AccountPubKey) ->
    case aens_names:owner_pubkey(Name) =:= AccountPubKey of
        true  -> ok;
        false -> runtime_error(name_not_owned)
    end.

assert_name_claimed(Name) ->
    case aens_names:status(Name) of
        claimed -> ok;
        revoked -> runtime_error(name_revoked)
    end.

assert_ga_attach_byte_code(?ABI_AEVM_SOPHIA_1, SerializedCode, CallData, FunHash, S) ->
    try aect_sophia:deserialize(SerializedCode) of
        #{type_info := TypeInfo, contract_vsn := Vsn} ->
            case aect_sophia:is_legal_serialization_at_height(Vsn, S#state.height) of
                true ->
                    assert_contract_init_function(?ABI_AEVM_SOPHIA_1, CallData, TypeInfo),
                    assert_auth_function(FunHash, TypeInfo);
                false ->
                    runtime_error(illegal_contract_compiler_version)
            end
    catch _:_ -> runtime_error(bad_sophia_code)
    end.

assert_contract_byte_code(ABIVersion, SerializedCode, CallData, S)
  when ABIVersion =:= ?ABI_AEVM_SOPHIA_1;
       ABIVersion =:= ?ABI_FATE_SOPHIA_1 ->
    try aect_sophia:deserialize(SerializedCode) of
        #{type_info := TypeInfo,
          contract_vsn := Vsn} ->
            case aect_sophia:is_legal_serialization_at_height(Vsn, S#state.height) of
                true ->
                    assert_contract_init_function(ABIVersion, CallData, TypeInfo);
                false ->
                    runtime_error(illegal_contract_compiler_version)
            end
    catch _:_ -> runtime_error(bad_sophia_code)
    end;
assert_contract_byte_code(?ABI_SOLIDITY_1, _SerializedCode, _CallData, _S) ->
    ok.

assert_contract_init_function(?ABI_FATE_SOPHIA_1, CallData,_TypeInfo) ->
    try aeb_fate_encoding:deserialize(CallData) of
        %% TODO: Proper call data abstraction
        _ -> ok
    catch _:_ -> runtime_error(bad_init_function)
    end;
assert_contract_init_function(?ABI_AEVM_SOPHIA_1, CallData, TypeInfo) ->
    case aeb_abi:get_function_hash_from_calldata(CallData) of
        {ok, Hash} ->
            case aeb_abi:function_name_from_type_hash(Hash, TypeInfo) of
                {ok, <<"init">>} -> ok;
                _ -> runtime_error(bad_init_function)
            end;
        _Other -> runtime_error(bad_init_function)
    end.

assert_auth_data_function(AuthData, AuthFunHash) ->
    case aeb_abi:get_function_hash_from_calldata(AuthData) of
        {ok, AuthFunHash} -> ok;
        {ok, _OtherHash}  -> runtime_error(wrong_auth_function);
        _Other            -> runtime_error(bad_auth_data)
    end.

assert_auth_function(Hash, TypeInfo) ->
    case aeb_abi:typereps_from_type_hash(Hash, TypeInfo) of
        {ok, _ArgType, word}     -> ok;
        {ok, _ArgType, _OutType} -> runtime_error(bad_auth_function_return_type);
        {error, _}               -> runtime_error(bad_function_hash)
    end.

assert_contract_create_version(ABIVersion, VMVersion, S) ->
    CTVersion = #{abi => ABIVersion, vm => VMVersion},
    Height = S#state.height,
    case aect_contracts:is_legal_version_at_height(create, CTVersion, Height) of
        true  -> ok;
        false -> runtime_error(illegal_vm_version)
    end.

assert_ga_active(S) ->
    Height = S#state.height,
    case aec_hard_forks:protocol_effective_at_height(Height) of
        P when P < ?FORTUNA_PROTOCOL_VSN ->
            runtime_error(generalize_accounts_not_available_at_height);
        _P ->
            ok
    end.

assert_ga_create_version(ABIVersion, VMVersion, _S) ->
    CTVersion = #{abi => ABIVersion, vm => VMVersion},
    case aega_attach_tx:is_legal_contract_version(CTVersion) of
        true  -> ok;
        false -> runtime_error(illegal_vm_version)
    end.

assert_contract_call_version(Pubkey, ABIVersion, S) ->
    Contract  = get_contract_without_store(Pubkey, S),
    CTVersion = #{abi := CABIVersion} = aect_contracts:ct_version(Contract),
    Height    = S#state.height,
    case aect_contracts:is_legal_version_at_height(call, CTVersion, Height) of
        true when ABIVersion =:= CABIVersion -> ok;
        true                                 -> runtime_error(wrong_abi_version);
        false                                -> runtime_error(wrong_vm_version)
    end.

assert_contract_call_stack(CallStack, S) ->
    case aetx_env:context(S#state.tx_env) of
        aetx_contract    when is_list(CallStack) -> ok;
        _Other when CallStack =:= [] -> ok;
        _Other -> runtime_error(nonempty_call_stack)
    end.

assert_auth_call_object_not_exist(Call, S) ->
    AuthCallId = aect_call:id(Call),
    Pubkey     = aect_call:caller_pubkey(Call),
    case find_auth_call(Pubkey, AuthCallId, S) of
        none -> ok;
        {value, _} -> runtime_error(auth_call_object_already_exist)
    end.


assert_not_channel(ChannelPubkey, S) ->
    case find_channel(ChannelPubkey, S) of
        none -> ok;
        {value, _} -> runtime_error(channel_exists)
    end.

assert_channel_reserve_amount(ReserveAmount, InitiatorAmount, ResponderAmount) ->
    if
        InitiatorAmount < ReserveAmount ->
            runtime_error(insufficient_initiator_amount);
        ResponderAmount < ReserveAmount ->
            runtime_error(insufficient_responder_amount);
        true ->
            ok
    end.

assert_channel_origin(Channel, FromPubkey) ->
    case (aesc_channels:initiator_pubkey(Channel) =:= FromPubkey
          orelse aesc_channels:responder_pubkey(Channel) =:= FromPubkey) of
        true  -> ok;
        false -> runtime_error(illegal_origin)
    end.

assert_channel_active(Channel) ->
    case aesc_channels:is_active(Channel) of
        true  -> ok;
        false -> runtime_error(channel_not_active)
    end.


assert_channel_is_solo_closed(Channel, S) ->
    case aesc_channels:is_solo_closed(Channel, S#state.height) of
        true  -> ok;
        false -> runtime_error(channel_not_closed)
    end.

assert_is_channel_peer(Channel, Pubkey) ->
    case lists:member(Pubkey, aesc_channels:peers(Channel)) of
        true  -> ok;
        false -> runtime_error(account_not_peer)
    end.

assert_channel_round(Channel, Round, Type) ->
    case aesc_utils:check_round_greater_than_last(Channel, Round, Type) of
        ok -> ok;
        {error, old_round} -> runtime_error(old_round);
        {error, same_round} -> runtime_error(same_round)
    end.

assert_channel_withdraw_amount(Channel, Amount) ->
    ChannelAmount = aesc_channels:channel_amount(Channel),
    Reserve = aesc_channels:channel_reserve(Channel),
    assert(ChannelAmount >= 2*Reserve + Amount, not_enough_channel_funds).

%%%===================================================================
%%% Error handling

-spec runtime_error(term()) -> no_return().
runtime_error(Error) ->
    throw({?MODULE, Error}).
