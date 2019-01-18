%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_tx_processor).

-export([eval/3
        ]).

%% Simple access tx instructions API
-export([ spend_tx_instructions/5
        , name_claim_tx_instructions/5
        , name_preclaim_tx_instructions/5
        , name_revoke_tx_instructions/4
        , oracle_extend_tx_instructions/4
        , oracle_query_tx_instructions/8
        , oracle_register_tx_instructions/8
        , oracle_response_tx_instructions/6
        ]).


-include_lib("aecore/include/aec_hash.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

-record(state, { trees      :: aec_trees:trees()
               , height     :: non_neg_integer()
               , cache      :: dict:dict()
               , env        :: dict:dict()
               }).

-define(IS_HASH(_X_), (is_binary(_X_) andalso byte_size(_X_) =:= ?HASH_BYTES)).
-define(IS_VAR(_X_), (is_tuple(_X_)
                      andalso tuple_size(_X_) =:= 2
                      andalso var =:= element(1, _X_)
                      andalso is_atom(element(2, _X_)))).

-define(IS_VAR_OR_HASH(_X_), (?IS_HASH(_X_) orelse ?IS_VAR(_X_))).
-define(IS_NON_NEG_INTEGER(_X_), (is_integer(_X_) andalso _X_ >= 0)).
-define(IS_NAME_RESOLVE_TYPE(_X_), (_X_ =:= account
                                    orelse _X_ =:= name)).

%%%===================================================================
%%% API
%%%===================================================================

eval([_|_] = Instructions, Trees, Height) when is_integer(Height),
                                               Height >= 0 ->
    try {ok, eval_instructions(Instructions, new_state(Trees, Height))}
    catch
        throw:{?MODULE, What} ->
            {error, What}
    end.

spend_tx_instructions(SenderPubkey, RecipientID, Amount, Fee, Nonce) ->
    Recipient = {var, recipient},
    {Type, RecipientHash} = case aec_id:specialize(RecipientID) of
                                {name, NameHash}   -> {name, NameHash};
                                {oracle, Pubkey}   -> {account, Pubkey};
                                {contract, Pubkey} -> {account, Pubkey};
                                {account, Pubkey}  -> {account, Pubkey}
                            end,
    [ inc_account_nonce_op(SenderPubkey, Nonce)
    , resolve_account_op(Type, RecipientHash, Recipient)
    , spend_op(SenderPubkey, Recipient, Amount)
    , spend_fee_op(SenderPubkey, Fee)
    ].

oracle_register_tx_instructions(AccountPubkey, QFormat, RFormat, QFee,
                                DeltaTTL, VMVersion, TxFee, Nonce) ->
    %% TODO: Account nonce should not be increased in contract context
    %%       but this would mean a hard fork.
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, TxFee)
    , oracle_register_op(AccountPubkey, QFormat, RFormat, QFee,
                         DeltaTTL, VMVersion)
    ].

oracle_extend_tx_instructions(Pubkey, DeltaTTL, Fee, Nonce) ->
    [ inc_account_nonce_op(Pubkey, Nonce)
    , spend_fee_op(Pubkey, Fee)
    , oracle_extend_op(Pubkey, DeltaTTL)
    ].

oracle_query_tx_instructions(OraclePubkey, SenderPubkey, Query,
                             QueryFee, QTTL, RTTL, TxFee, Nonce) ->
    [ inc_account_nonce_op(SenderPubkey, Nonce)
    , spend_fee_op(SenderPubkey, TxFee + QueryFee)
    , oracle_query_op(OraclePubkey, SenderPubkey, Nonce,
                      Query, QueryFee, QTTL, RTTL)
    ].

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

name_preclaim_tx_instructions(AccountPubkey, CommitmentHash, DeltaTTL,
                              Fee, Nonce) ->
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, Fee)
    , name_preclaim_op(AccountPubkey, CommitmentHash, DeltaTTL)
    ].

name_claim_tx_instructions(AccountPubkey, PlainName, NameSalt, Fee, Nonce) ->
    PreclaimDelta = aec_governance:name_claim_preclaim_delta(),
    DeltaTTL = aec_governance:name_claim_max_expiration(),
    LockedFee = aec_governance:name_claim_locked_fee(),
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, Fee)
    , lock_amount_op(AccountPubkey, LockedFee)
    , name_claim_op(AccountPubkey, PlainName, NameSalt, DeltaTTL, PreclaimDelta)
    ].

name_revoke_tx_instructions(AccountPubkey, NameHash, Fee, Nonce) ->
    ProtectedDeltaTTL = aec_governance:name_protection_period(),
    [ inc_account_nonce_op(AccountPubkey, Nonce)
    , spend_fee_op(AccountPubkey, Fee)
    , name_revoke_op(AccountPubkey, NameHash, ProtectedDeltaTTL)
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Instruction evaluation

eval_instructions([I|Left], S) ->
    #state{} = S1 = eval_one(I, S),
    eval_instructions(Left, S1);
eval_instructions([], S) ->
    cache_write_through(S).

eval_one({Op, Args}, S) ->
    case Op of
        inc_account_nonce         -> inc_account_nonce(Args, S);
        lock_amount               -> lock_amount(Args, S);
        name_claim                -> name_claim(Args, S);
        name_preclaim             -> name_preclaim(Args, S);
        name_revoke               -> name_revoke(Args, S);
        oracle_earn_query_fee     -> oracle_earn_query_fee(Args, S);
        oracle_extend             -> oracle_extend(Args, S);
        oracle_query              -> oracle_query(Args, S);
        oracle_register           -> oracle_register(Args, S);
        oracle_respond            -> oracle_respond(Args, S);
        resolve_account           -> resolve_account(Args, S);
        spend                     -> spend(Args, S);
        spend_fee                 -> spend_fee(Args, S);
        Other                     -> error({illegal_op, Other})
    end.

new_state(Trees, Height) ->
    #state{ trees = Trees
          , cache = dict:new()
          , env   = dict:new()
          , height = Height
          }.

%%%===================================================================
%%% Operations
%%%

inc_account_nonce_op(Key, Nonce) when ?IS_VAR_OR_HASH(Key),
                                      ?IS_NON_NEG_INTEGER(Nonce) ->
    {inc_account_nonce, {Key, Nonce}}.

inc_account_nonce({Key, Nonce}, #state{} = S) ->
    {Account, S1} = get_account(Key, S),
    assert_account_nonce(Account, Nonce),
    Account1 = aec_accounts:set_nonce(Account, Nonce),
    cache_put(account, Account1, S1).

%%%-------------------------------------------------------------------

spend_op(From, To, Amount) when ?IS_VAR_OR_HASH(From),
                                ?IS_VAR_OR_HASH(To),
                                ?IS_NON_NEG_INTEGER(Amount) ->
    {spend, {From, To, Amount}}.

spend({From, To, Amount}, #state{} = S) when is_integer(Amount), Amount >= 0 ->
    {Sender1, S1}   = get_account(From, S),
    assert_account_balance(Sender1, Amount),
    {ok, Sender2}   = aec_accounts:spend_without_nonce_bump(Sender1, Amount),
    S2              = cache_put(account, Sender2, S1),
    {Receiver1, S3} = ensure_account(To, S2),
    {ok, Receiver2} = aec_accounts:earn(Receiver1, Amount),
    cache_put(account, Receiver2, S3).

%%%-------------------------------------------------------------------
%%% A special form of spending is to lock an amount.

lock_amount_op(From, Amount) when ?IS_VAR_OR_HASH(From),
                                  ?IS_NON_NEG_INTEGER(Amount) ->
    {lock_amount, {From, Amount}}.

lock_amount({_From, 0}, #state{} = S) ->
    %% Don't risk creating an account for the locked amount if there is none.
    S;
lock_amount({From, Amount}, #state{} = S) ->
    LockPubkey = aec_governance:locked_coins_holder_account(),
    spend({From, LockPubkey, Amount}, S).

%%%-------------------------------------------------------------------

spend_fee_op(From, Amount) when ?IS_VAR_OR_HASH(From),
                                ?IS_NON_NEG_INTEGER(Amount) ->
    {spend_fee, {From, Amount}}.

spend_fee({From, Amount}, #state{} = S) when is_integer(Amount), Amount >= 0 ->
    {Sender1, S1}   = get_account(From, S),
    assert_account_balance(Sender1, Amount),
    {ok, Sender2}   = aec_accounts:spend_without_nonce_bump(Sender1, Amount),
    cache_put(account, Sender2, S1).

%%%-------------------------------------------------------------------

resolve_account_op(GivenType, Hash, Var) when ?IS_NAME_RESOLVE_TYPE(GivenType),
                                              ?IS_HASH(Hash),
                                              ?IS_VAR(Var) ->
    {resolve_account, {GivenType, Hash, Var}}.

resolve_account({GivenType, Hash, Var}, S) ->
    resolve_name(account, GivenType, Hash, Var, S).

resolve_name(account, account, Pubkey, Var, S) ->
    set_var(Var, account, Pubkey, S);
resolve_name(account, name, NameHash, Var, S) ->
    Key = <<"account_pubkey">>,
    Trees = S#state.trees,
    %% TODO: Should cache the name as well.
    case aens:resolve_from_hash(Key, NameHash, aec_trees:ns(Trees)) of
        {ok, Id} ->
            %% Intentionally admissive to allow for all kinds of IDs for
            %% backwards compatibility.
            {_Tag, Pubkey} = aec_id:specialize(Id),
            set_var(Var, account, Pubkey, S);
        {error, What} ->
            runtime_error(What)
    end.

%%%-------------------------------------------------------------------

oracle_register_op(Pubkey, QFormat, RFormat, QFee,
                   DeltaTTL, VMVersion) when ?IS_HASH(Pubkey),
                                             is_binary(QFormat),
                                             is_binary(RFormat),
                                             ?IS_NON_NEG_INTEGER(QFee),
                                             ?IS_NON_NEG_INTEGER(DeltaTTL),
                                             ?IS_NON_NEG_INTEGER(VMVersion) ->
    {oracle_register, {Pubkey, QFormat, RFormat, QFee, DeltaTTL, VMVersion}}.

oracle_register({Pubkey, QFormat, RFormat, QFee, DeltaTTL, VMVersion}, S) ->
    assert_not_oracle(Pubkey, S),
    assert_oracle_vm_version(VMVersion),
    %% TODO: MINERVA We should check the format matches the vm version,
    %% but this is a hard fork
    AbsoluteTTL = DeltaTTL + S#state.height,
    try aeo_oracles:new(Pubkey, QFormat, RFormat, QFee, AbsoluteTTL, VMVersion) of
        Oracle -> cache_put(oracle, Oracle, S)
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
    cache_put(oracle, Oracle1, S1).

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
        QueryObject ->
            assert_not_oracle_query(QueryObject, S),
            cache_put(oracle_query, QueryObject, S1)
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
    cache_put(oracle_query, QueryObject1, S2).

%%%-------------------------------------------------------------------

oracle_earn_query_fee_op(OraclePubkey, QueryId) when ?IS_HASH(OraclePubkey),
                                                     ?IS_HASH(QueryId) ->
    {oracle_earn_query_fee, {OraclePubkey, QueryId}}.

oracle_earn_query_fee({OraclePubkey, QueryId}, S) ->
    {Account, S1} = get_account(OraclePubkey, S),
    {Query, S2} = get_oracle_query(OraclePubkey, QueryId, S1),
    {ok, Account1} = aec_accounts:earn(Account, aeo_query:fee(Query)),
    cache_put(account, Account1, S2).

%%%-------------------------------------------------------------------

name_preclaim_op(AccountPubkey, CommitmentHash, DeltaTTL
                ) when ?IS_HASH(AccountPubkey),
                       ?IS_HASH(CommitmentHash),
                       ?IS_NON_NEG_INTEGER(DeltaTTL) ->
    {name_preclaim, {AccountPubkey, CommitmentHash, DeltaTTL}}.

name_preclaim({AccountPubkey, CommitmentHash, DeltaTTL}, S) ->
    assert_not_commitment(CommitmentHash, S),
    Id      = aec_id:create(commitment, CommitmentHash),
    OwnerId = aec_id:create(account, AccountPubkey),
    Commitment = aens_commitments:new(Id, OwnerId, DeltaTTL, S#state.height),
    cache_put(commitment, Commitment, S).

%%%-------------------------------------------------------------------

name_claim_op(AccountPubkey, PlainName, NameSalt, DeltaTTL, PreclaimDelta) ->
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
    cache_put(name, Name, S2).

name_to_ascii(PlainName) ->
    case aens_utils:to_ascii(PlainName) of
        {error, What} ->
            runtime_error(What);
        {ok, NameAscii} ->
            NameAscii
    end.

%%%-------------------------------------------------------------------

name_revoke_op(AccountPubkey, NameHash, ProtectedDeltaTTL) ->
    {name_revoke, {AccountPubkey, NameHash, ProtectedDeltaTTL}}.

name_revoke({AccountPubkey, NameHash, ProtectedDeltaTTL}, S) ->
    {Name, S1} = get_name(NameHash, S),
    assert_name_owner(Name, AccountPubkey),
    assert_name_claimed(Name),
    Name1 = aens_names:revoke(Name, ProtectedDeltaTTL, S1#state.height),
    cache_put(name, Name1, S1).

%%%===================================================================
%%% Helpers for instructions

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
    case find_x(account, Key, S) of
        none ->
            Pubkey = get_var(Key, account, S),
            Account = aec_accounts:new(Pubkey, 0),
            {Account, cache_put(account, Account, S)};
        {Account, S1} ->
            {Account, S1}
    end.

assert_not_oracle(Pubkey, S) ->
    case find_x(oracle, Pubkey, S) of
        {_, _} -> runtime_error(account_is_already_an_oracle);
        none -> ok
    end.

assert_oracle_vm_version(?AEVM_NO_VM) -> ok;
assert_oracle_vm_version(?AEVM_01_Sophia_01) -> ok;
assert_oracle_vm_version(_) -> runtime_error(bad_vm_version).

assert_query_fee(Oracle, QueryFee) ->
    case QueryFee >= aeo_oracles:query_fee(Oracle) of
        true  -> ok;
        false -> runtime_error(query_fee_too_low)
    end.

assert_not_oracle_query(Query, S) ->
    OraclePubkey = aeo_query:oracle_pubkey(Query),
    QueryId  = aeo_query:id(Query),
    case find_x(oracle_query, {OraclePubkey, QueryId}, S) of
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
    case aeo_oracles:vm_version(Oracle) of
        ?AEVM_NO_VM ->
            %% No interpretation of the format, nor content.
            ok;
        ?AEVM_01_Sophia_01 ->
            %% Check that the content can be decoded as the type
            %% and that if we encoded it again, it becomes the content.
            {ok, TypeRep} = aeso_heap:from_binary(typerep, Format),
            try aeso_heap:from_binary(TypeRep, Content) of
                {ok, Res} ->
                    case aeso_heap:to_binary(Res) of
                        Content -> ok;
                        _Other -> runtime_error(bad_format)
                    end;
                {error, _} ->
                    runtime_error(bad_format)
            catch _:_ ->
                    {error, bad_format}
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
    case find_x(commitment, CommitmentHash, S) of
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
    case find_x(name, NameHash, S) of
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

%%%===================================================================
%%% Error handling

-spec runtime_error(term()) -> no_return().
runtime_error(Error) ->
    throw({?MODULE, Error}).

%%%===================================================================
%%% Access to cache or trees

get_account(Key, S) ->
    get_x(account, Key, account_not_found, S).

get_name(Key, S) ->
    get_x(name, Key, name_does_not_exist, S).

get_oracle(Key, Error, S) ->
    get_x(oracle, Key, Error, S).

get_oracle_query(OraclePubkey, QueryId, S) ->
    get_x(oracle_query, {OraclePubkey, QueryId}, no_matching_oracle_query, S).

get_commitment(Hash, Error, S) ->
    get_x(commitment, Hash, Error, S).

find_x(Tag, Key, S) ->
    case cache_find(Tag, Key, S) of
        none ->
            case trees_find(Tag, Key, S) of
                none -> none;
                {value, Val} ->
                    {Val, cache_put(Tag, Val, S)}
            end;
        {value, Val} ->
            {Val, S}
    end.

get_x(Tag, Key, Error, S) when is_atom(Error) ->
    case find_x(Tag, Key, S) of
        none -> runtime_error(Error);
        {_, _} = Ret -> Ret
    end.

delete_x(commitment, Hash, #state{trees = Trees} = S) ->
    S1 = cache_drop(commitment, Hash, S),
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:delete_commitment(Hash, NTree),
    S1#state{trees = aec_trees:set_ns(Trees, NTree1)}.

%%%===================================================================
%%% Access to trees

trees_find(account, Key, #state{trees = Trees} = S) ->
    ATree = aec_trees:accounts(Trees),
    aec_accounts_trees:lookup(get_var(Key, account, S), ATree);
trees_find(commitment, Key, #state{trees = Trees} = S) ->
    NTree = aec_trees:ns(Trees),
    aens_state_tree:lookup_commitment(get_var(Key, commitment, S), NTree);
trees_find(name, Key, #state{trees = Trees} = S) ->
    NTree = aec_trees:ns(Trees),
    aens_state_tree:lookup_name(get_var(Key, name, S), NTree);
trees_find(oracle, Key, #state{trees = Trees} = S) ->
    OTree = aec_trees:oracles(Trees),
    aeo_state_tree:lookup_oracle(get_var(Key, oracle, S), OTree);
trees_find(oracle_query, Key, #state{trees = Trees} = S) ->
    {OraclePubkey, QueryId} = get_var(Key, oracle_query, S),
    OTree = aec_trees:oracles(Trees),
    aeo_state_tree:lookup_query(OraclePubkey, QueryId, OTree).

%%%===================================================================
%%% Cache

-define(IS_TAG(X), ((X =:= account)
                    orelse (X =:= oracle)
                    orelse (X =:= oracle_query)
                    orelse (X =:= commitment)
                    orelse (X =:= name)
                   )
       ).

cache_find(Tag, Key, #state{cache = C} = S) when ?IS_TAG(Tag) ->
    case dict:find({Tag, get_var(Key, Tag, S)}, C) of
        {ok, Val} -> {value, Val};
        error     -> none
    end.

cache_drop(commitment, Hash, #state{cache = C} = S) ->
    S#state{cache = dict:erase({commitment, Hash}, C)}.

cache_put(account, Val, #state{cache = C} = S) ->
    Pubkey = aec_accounts:pubkey(Val),
    S#state{cache = dict:store({account, Pubkey}, Val, C)};
cache_put(commitment, Val, #state{cache = C} = S) ->
    Hash = aens_commitments:hash(Val),
    S#state{cache = dict:store({commitment, Hash}, Val, C)};
cache_put(name, Val, #state{cache = C} = S) ->
    Hash = aens_names:hash(Val),
    S#state{cache = dict:store({name, Hash}, Val, C)};
cache_put(oracle, Val, #state{cache = C} = S) ->
    Pubkey = aeo_oracles:pubkey(Val),
    S#state{cache = dict:store({oracle, Pubkey}, Val, C)};
cache_put(oracle_query, Val, #state{cache = C} = S) ->
    Pubkey = aeo_query:oracle_pubkey(Val),
    QueryId = aeo_query:id(Val),
    S#state{cache = dict:store({oracle_query, {Pubkey, QueryId}}, Val, C)}.

cache_write_through(#state{cache = C, trees = T}) ->
    dict:fold(fun cache_write_through_fun/3, T, C).

%% TODO: Should have a dirty flag.
cache_write_through_fun({account,_Pubkey}, Account, Trees) ->
    ATrees  = aec_trees:accounts(Trees),
    ATrees1 = aec_accounts_trees:enter(Account, ATrees),
    aec_trees:set_accounts(Trees, ATrees1);
cache_write_through_fun({commitment,_Hash}, Commitment, Trees) ->
    NTree  = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_commitment(Commitment, NTree),
    aec_trees:set_ns(Trees, NTree1);
cache_write_through_fun({name,_Hash}, Name, Trees) ->
    NTree  = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_name(Name, NTree),
    aec_trees:set_ns(Trees, NTree1);
cache_write_through_fun({oracle,_Pubkey}, Oracle, Trees) ->
    OTrees  = aec_trees:oracles(Trees),
    OTrees1 = aeo_state_tree:enter_oracle(Oracle, OTrees),
    aec_trees:set_oracles(Trees, OTrees1);
cache_write_through_fun({oracle_query, {_Pubkey, _Id}}, Query, Trees) ->
    OTrees  = aec_trees:oracles(Trees),
    OTrees1 = aeo_state_tree:enter_query(Query, OTrees),
    aec_trees:set_oracles(Trees, OTrees1).

%%%===================================================================
%%% Variable environment

get_var({var, X}, Tag, #state{env = E}) when is_atom(X) ->
    {Tag, Val} = dict:fetch(X, E),
    Val;
get_var({X, Y} = Res, oracle_query, #state{}) when is_binary(X),
                                                   is_binary(Y) ->
    Res;
get_var(X,_Tag, #state{}) when is_binary(X) ->
    X.

set_var({var, X}, account = Tag, Pubkey, #state{} = S) when is_atom(X),
                                                            is_binary(Pubkey) ->
    S#state{env = dict:store(X, {Tag, Pubkey}, S#state.env)};
set_var(Var, Tag, Pubkey,_S) ->
    error({illegal_assignment, Var, Tag, Pubkey}).
