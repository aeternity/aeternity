%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract objects
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts).

%% API
-export([ deserialize/2
        , store_id/1
        , is_legal_call/2
        , is_legal_version_at_protocol/3
        , new/1
        , new/5 %% For use without transaction
        , new_clone/5
        , serialize/1
        , serialize_for_client/1
        , compute_contract_pubkey/2
        , compute_contract_store_id/1
        , is_legal_state_fun/1
          %% Getters
        , id/1
        , pubkey/1
        , owner_id/1
        , owner_pubkey/1
        , abi_version/1
        , ct_version/1
        , vm_version/1
        , code/1
        , state/1
        , log/1
        , active/1
        , referrer_ids/1
        , deposit/1
          %% Setters
        , set_pubkey/2
        , set_owner/2
        , set_code/2
        , set_state/2
        , set_log/2
        , set_active/2
        , set_referrers/2
        , set_deposit/2
        , is_legal_serialization_at_protocol/3
        ]).

-export([pack_vm_abi/1, split_vm_abi/1]).

-include("aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-ifdef(TEST).
-export([internal_set_state/2]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type amount() :: non_neg_integer().
-type store() :: aect_contracts_store:store().
-type serialization_version() :: 1 | 2.

-record(contract, {
        %% Normal account fields
        id           :: aeser_id:id(),
        owner_id     :: aeser_id:id(),
        ct_version   :: version(),
        code         :: {code, binary()} | {ref, aeser_id:id()}, %% the bytecode or DIRECT reference to contract with bytecode
        store        :: store(),      %% The current state/store (stored in a subtree in mpt)
        log          :: binary(),     %% The current event log
        active       :: boolean(),    %% false when disabled, but don't remove unless referrer_ids == []
        referrer_ids :: list(aeser_id:id()), %% List of contracts depending on this contract
        deposit      :: amount(),
        version      :: serialization_version()
    }).

-opaque contract() :: #contract{}.

-type id() :: aeser_id:id().
-type pubkey() :: aec_keys:pubkey().
-type store_id() :: binary().
-type serialized() :: binary().
-type vm_version() :: 0..16#FFFF.
-type abi_version() :: 0..16#FFFF.
-type version() :: #{ vm := vm_version(), abi := abi_version() }.
-type serial_version() :: 0..16#FFFFFFFF.
-type protocol() :: aec_hard_forks:protocol_vsn().
-type vm_usage_type() ::  'call' | 'create' | 'oracle_register'.
-type ct_nonce() :: non_neg_integer() | binary().
-type code_maybe() :: {code, binary()} | {ref, id()}.

-export_type([ contract/0
             , amount/0
             , id/0
             , pubkey/0
             , serialized/0
             , version/0
             , abi_version/0
             , vm_version/0
             , store/0
             , code_maybe/0
             ]).

-define(PUB_SIZE, 32).
-define(HASH_SIZE, 32).
-define(CONTRACT_TYPE, contract).
-define(CODE_REF_VSN, 2).
-define(PRE_IRIS_VSN, 1).
-define(STORE_PREFIX, <<16:8/integer-unsigned-unit:1>>). %% To collect storage trees in one subtree.

%%%===================================================================
%%% API
%%%===================================================================

-spec is_legal_call(FromVersion :: version(), ToVersion :: version()) ->
                          boolean().
%% NOTE: Keep this up to date if the different sophia vms gets incompatible.
is_legal_call(#{vm := SophiaVM1, abi := X},
              #{vm := SophiaVM2, abi := X}) when ?IS_AEVM_SOPHIA(SophiaVM1),
                                                 ?IS_AEVM_SOPHIA(SophiaVM2),
                                                 SophiaVM1 >= SophiaVM2 ->
    true;
is_legal_call(#{vm := SophiaVM1, abi := X},
              #{vm := SophiaVM2, abi := X}) when ?IS_FATE_SOPHIA(SophiaVM1),
                                                 ?IS_FATE_SOPHIA(SophiaVM2),
                                                 SophiaVM1 >= SophiaVM2 ->
    true;
is_legal_call(_, _) -> false.

-spec is_legal_version_at_protocol(vm_usage_type(), version(), protocol()) -> boolean().
is_legal_version_at_protocol(Operation, Version, Protocol) ->
    is_legal_version_at_protocol_(Operation, Version, Protocol).

is_legal_serialization_at_protocol(?ABI_SOLIDITY_1, _, _Protocol) ->
    true;
is_legal_serialization_at_protocol(?ABI_AEVM_SOPHIA_1, Vsn, Protocol) ->
    aect_sophia:is_legal_serialization_at_protocol(Vsn, Protocol);
is_legal_serialization_at_protocol(?ABI_FATE_SOPHIA_1, Vsn, Protocol) ->
    aect_sophia:is_legal_serialization_at_protocol(Vsn, Protocol).

is_legal_version_at_protocol_(create, #{vm := ?VM_AEVM_SOPHIA_1, abi := ?ABI_AEVM_SOPHIA_1}, Protocol) ->
    case Protocol of
        ?ROMA_PROTOCOL_VSN                -> true;
        P when P >= ?MINERVA_PROTOCOL_VSN -> false
    end;
is_legal_version_at_protocol_(create, #{vm := ?VM_AEVM_SOPHIA_2, abi := ?ABI_AEVM_SOPHIA_1}, Protocol) ->
    case Protocol of
        ?ROMA_PROTOCOL_VSN             -> false;
        ?MINERVA_PROTOCOL_VSN          -> true;
        ?FORTUNA_PROTOCOL_VSN          -> true;
        P when P >= ?LIMA_PROTOCOL_VSN -> false
    end;
is_legal_version_at_protocol_(create, #{vm := ?VM_AEVM_SOPHIA_3, abi := ?ABI_AEVM_SOPHIA_1}, Protocol) ->
    case Protocol of
        ?ROMA_PROTOCOL_VSN             -> false;
        ?MINERVA_PROTOCOL_VSN          -> false;
        ?FORTUNA_PROTOCOL_VSN          -> true;
        P when P >= ?LIMA_PROTOCOL_VSN -> false
    end;
is_legal_version_at_protocol_(create, #{vm := ?VM_AEVM_SOPHIA_4, abi := ?ABI_AEVM_SOPHIA_1}, Protocol) ->
    case Protocol of
        ?ROMA_PROTOCOL_VSN            -> false;
        ?MINERVA_PROTOCOL_VSN         -> false;
        ?FORTUNA_PROTOCOL_VSN         -> false;
        ?LIMA_PROTOCOL_VSN            -> true;
        P when P > ?LIMA_PROTOCOL_VSN -> false
    end;
is_legal_version_at_protocol_(create, #{vm := ?VM_FATE_SOPHIA_1, abi := ?ABI_FATE_SOPHIA_1}, Protocol) ->
    case Protocol of
        ?ROMA_PROTOCOL_VSN            -> false;
        ?MINERVA_PROTOCOL_VSN         -> false;
        ?FORTUNA_PROTOCOL_VSN         -> false;
        ?LIMA_PROTOCOL_VSN            -> true;
        P when P > ?LIMA_PROTOCOL_VSN -> false
    end;
is_legal_version_at_protocol_(create, #{vm := ?VM_FATE_SOPHIA_2, abi := ?ABI_FATE_SOPHIA_1}, Protocol) ->
    case Protocol of
        ?ROMA_PROTOCOL_VSN            -> false;
        ?MINERVA_PROTOCOL_VSN         -> false;
        ?FORTUNA_PROTOCOL_VSN         -> false;
        ?LIMA_PROTOCOL_VSN            -> false;
        ?IRIS_PROTOCOL_VSN            -> true;
        P when P > ?IRIS_PROTOCOL_VSN -> true
    end;
is_legal_version_at_protocol_(call, #{vm := VMVersion}, Protocol) ->
    case Protocol of
        ?ROMA_PROTOCOL_VSN    when VMVersion =:= ?VM_AEVM_SOPHIA_1 ->
            true;
        ?MINERVA_PROTOCOL_VSN when (VMVersion =:= ?VM_AEVM_SOPHIA_1) or
                                   (VMVersion =:= ?VM_AEVM_SOPHIA_2) ->
            true;
        ?FORTUNA_PROTOCOL_VSN when (VMVersion =:= ?VM_AEVM_SOPHIA_1) or
                                   (VMVersion =:= ?VM_AEVM_SOPHIA_2) or
                                   (VMVersion =:= ?VM_AEVM_SOPHIA_3) ->
            true;
        P                     when (P >= ?LIMA_PROTOCOL_VSN)
                                   and
                                   (
                                   (VMVersion =:= ?VM_AEVM_SOPHIA_1) or
                                   (VMVersion =:= ?VM_AEVM_SOPHIA_2) or
                                   (VMVersion =:= ?VM_AEVM_SOPHIA_3) or
                                   (VMVersion =:= ?VM_AEVM_SOPHIA_4)
                                   ) ->
            true;
        P                     when (P >= ?LIMA_PROTOCOL_VSN)
                                   and
                                   (
                                   (VMVersion =:= ?VM_FATE_SOPHIA_1)
                                   ) ->
            true;
        P                     when (P >= ?IRIS_PROTOCOL_VSN)
                                   and
                                   (
                                   (VMVersion =:= ?VM_FATE_SOPHIA_2)
                                   ) ->
            true;
        _                     when VMVersion =:= ?VM_AEVM_SOLIDITY_1 ->
            ?VM_AEVM_SOLIDITY_1_enabled;
        _ ->
            false
    end;
is_legal_version_at_protocol_(oracle_register, #{abi := ?ABI_NO_VM}, _Protocol) ->
    true;
is_legal_version_at_protocol_(oracle_register, #{abi := ?ABI_AEVM_SOPHIA_1}, Protocol) ->
    Protocol =< ?LIMA_PROTOCOL_VSN;
is_legal_version_at_protocol_(oracle_register, #{abi := ?ABI_FATE_SOPHIA_1}, Protocol) ->
    Protocol >= ?LIMA_PROTOCOL_VSN;
is_legal_version_at_protocol_(_, _, _) ->
    false.

-spec store_id(contract()) -> store_id().
store_id(C) ->
    compute_contract_store_id(pubkey(C)).

-spec new(aect_create_tx:tx()) -> contract().
new(RTx) ->
    new(aect_create_tx:owner_pubkey(RTx),
        aect_create_tx:nonce(RTx),
        aect_create_tx:ct_version(RTx),
        aect_create_tx:code(RTx),
        aect_create_tx:deposit(RTx)).

-spec new(aec_keys:pubkey(), ct_nonce(), version(), binary(), amount()) -> contract().
%% NOTE: Should only be used for contract execution without transaction
new(Owner, Nonce, CTVersion, Code, Deposit) ->
    new_internal(Owner, Nonce, CTVersion, {code, Code}, Deposit).

%% NOTE: When cloning passed CodeRefPK has to have code={code, _} (NO INDIRECT CODE REFERENCE)
-spec new_clone(aec_keys:pubkey(), ct_nonce(), version(), aec_keys:pubkey(), amount()) -> contract().
new_clone(Owner, Nonce, CTVersion, CodeRefPK, Deposit) ->
    CodeRef = aeser_id:create(contract, CodeRefPK),
    new_internal(Owner, Nonce, CTVersion, {ref, CodeRef}, Deposit).

new_internal(Owner, Nonce, CTVersion, Code, Deposit) ->
    Pubkey = compute_contract_pubkey(Owner, Nonce),
    C = #contract{ id           = aeser_id:create(contract, Pubkey),
                   owner_id     = aeser_id:create(account, Owner),
                   ct_version   = CTVersion,
                   code         = Code,
                   store        = aect_contracts_store:new(),
                   log          = <<>>,
                   active       = true,
                   referrer_ids = [],
                   deposit      = Deposit,
                   version      = serialization_version_for_vm(CTVersion)
                 },
    C = assert_fields(C),
    C.

-spec serialize(contract()) -> serialized().
serialize(#contract{owner_id     = OwnerId,
                    ct_version   = CTVersion,
                    code         = {code, Code},
                    log          = Log,
                    active       = Active,
                    referrer_ids = ReferrerIds,
                    deposit      = Deposit,
                    version      = ?PRE_IRIS_VSN}) ->
    aeser_chain_objects:serialize(
      ?CONTRACT_TYPE,
      ?PRE_IRIS_VSN,
      serialization_template(?PRE_IRIS_VSN),
      [ {owner_id, OwnerId}
      , {ct_version, pack_vm_abi(CTVersion)}
      , {code, Code}
      , {log, Log}
      , {active, Active}
      , {referrer_ids, ReferrerIds}
      , {deposit, Deposit}
      ]);
serialize(#contract{owner_id     = OwnerId,
                    ct_version   = CTVersion,
                    code         = Code,
                    log          = Log,
                    active       = Active,
                    referrer_ids = ReferrerIds,
                    deposit      = Deposit,
                    version      = ?CODE_REF_VSN}) ->
    {CodeMaybe, RefMaybe} =
        case Code of
            {code, C} -> {C, []};
            {ref, Ref} -> {<<>>, [Ref]}
        end,
    aeser_chain_objects:serialize(
        ?CONTRACT_TYPE,
        ?CODE_REF_VSN,
        serialization_template(?CODE_REF_VSN),
        [ {owner_id, OwnerId}
        , {ct_version, pack_vm_abi(CTVersion)}
        , {code, CodeMaybe}
        , {code_ref, RefMaybe}
        , {log, Log}
        , {active, Active}
        , {referrer_ids, ReferrerIds}
        , {deposit, Deposit}
        ]).

-spec serialize_for_client(contract()) -> map().
serialize_for_client(#contract{id           = Id,
                               owner_id     = OwnerId,
                               ct_version   = CTVersion,
                               log          = _Log,
                               active       = Active,
                               referrer_ids = ReferrerIds,
                               deposit      = Deposit}) ->
    #{ <<"id">>           => aeser_api_encoder:encode(id_hash, Id)
     , <<"owner_id">>     => aeser_api_encoder:encode(id_hash, OwnerId)
     , <<"vm_version">>   => maps:get(vm, CTVersion)
     , <<"abi_version">>  => maps:get(abi, CTVersion)
     , <<"active">>       => Active
     , <<"referrer_ids">> => [aeser_api_encoder:encode(id_hash, RId) || RId <- ReferrerIds]
     , <<"deposit">>      => Deposit
     }.

-spec deserialize(pubkey(), serialized()) -> contract().
deserialize(Pubkey, Bin) ->
    {?CONTRACT_TYPE, Vsn, RawFields} = aeser_chain_objects:deserialize_type_and_vsn(Bin),
    deserialize(Pubkey, Vsn, RawFields).

deserialize(Pubkey, ?PRE_IRIS_VSN, RawFields) ->
    [ {owner_id, OwnerId}
    , {ct_version, CTVersion}
    , {code, Code}
    , {log, Log}
    , {active, Active}
    , {referrer_ids, ReferrerIds}
    , {deposit, Deposit}
    ] = aeserialization:decode_fields(
        serialization_template(?PRE_IRIS_VSN),
        RawFields
    ),
    [contract = aeser_id:specialize_type(R) || R <- ReferrerIds],
    account = aeser_id:specialize_type(OwnerId),
    #contract{ id           = aeser_id:create(contract, Pubkey)
             , owner_id     = OwnerId
             , ct_version   = split_vm_abi(CTVersion)
             , code         = {code, Code}
             , store        = aect_contracts_store:new()
             , log          = Log
             , active       = Active
             , referrer_ids = ReferrerIds
             , deposit      = Deposit
             , version      = ?PRE_IRIS_VSN
             };

deserialize(Pubkey, ?CODE_REF_VSN, RawFields) ->
    [ {owner_id, OwnerId}
    , {ct_version, CTVersion}
    , {code, CodeMaybe}
    , {code_ref, MaybeRef}
    , {log, Log}
    , {active, Active}
    , {referrer_ids, ReferrerIds}
    , {deposit, Deposit}
    ] = aeserialization:decode_fields(
        serialization_template(?CODE_REF_VSN),
        RawFields
    ),
    Code =
        case MaybeRef of
            [Ref] ->
                <<>> = CodeMaybe,
                {ref, Ref};
            [] -> {code, CodeMaybe}
        end,
    [contract = aeser_id:specialize_type(R) || R <- ReferrerIds],
    account = aeser_id:specialize_type(OwnerId),
    #contract{ id           = aeser_id:create(contract, Pubkey)
        , owner_id     = OwnerId
        , ct_version   = split_vm_abi(CTVersion)
        , code         = Code
        , store        = aect_contracts_store:new()
        , log          = Log
        , active       = Active
        , referrer_ids = ReferrerIds
        , deposit      = Deposit
        , version      = ?CODE_REF_VSN
    }.

serialization_template(?PRE_IRIS_VSN) ->
    [ {owner_id, id}
    , {ct_version, int}
    , {code, binary}
    , {log, binary}
    , {active, bool}
    , {referrer_ids, [id]}
    , {deposit, int}
    ];

serialization_template(?CODE_REF_VSN) ->
    [ {owner_id, id}
    , {ct_version, int}
    , {code, binary}
    , {code_ref, [id]}
    , {log, binary}
    , {active, bool}
    , {referrer_ids, [id]}
    , {deposit, int}
    ].

serialization_version_for_vm(#{vm := VMVersion}) when ?IS_FATE_SOPHIA(VMVersion) ->
    case VMVersion of
        ?VM_FATE_SOPHIA_1 -> ?PRE_IRIS_VSN;
        _ -> ?CODE_REF_VSN
    end;
serialization_version_for_vm(_) ->
    ?PRE_IRIS_VSN.

-spec compute_contract_pubkey(aec_keys:pubkey(), ct_nonce()) -> aec_keys:pubkey().
compute_contract_pubkey(<<_:?PUB_SIZE/binary>> = Owner, Nonce) when is_integer(Nonce), Nonce >= 0  ->
    aec_hash:hash(pubkey, <<Owner/binary, (binary:encode_unsigned(Nonce))/binary>>);
compute_contract_pubkey(<<_:?PUB_SIZE/binary>> = Owner, <<_:?HASH_SIZE/binary>> = NonceBin) ->
    aec_hash:hash(pubkey, <<Owner/binary, NonceBin/binary>>).

-spec compute_contract_store_id(aec_keys:pubkey()) -> store_id().
compute_contract_store_id(CId) ->
    %% The STORE_PREFIX is used to name the storage tree and keep
    %% all storage nodes in one subtree under the contract tree.
    << CId/binary, ?STORE_PREFIX/binary>>.

-spec is_legal_state_fun(contract()) -> fun((store()) -> boolean()).
is_legal_state_fun(Contract) ->
    fun(Store) ->
            try set_state(Store, Contract) of
                _ -> true
            catch
                _:_ -> false
            end
    end.

%%%===================================================================
%%% Getters

%% The Id if the contract.
-spec id(contract()) -> id().
id(#contract{id = Id}) ->
   Id.

%% The address of the contract account.
-spec pubkey(contract()) -> pubkey().
pubkey(#contract{id = Id}) ->
    aeser_id:specialize(Id, contract).

%% The id of the owner.
-spec owner_id(contract()) -> aeser_id:id().
owner_id(#contract{owner_id = OwnerId}) ->
    OwnerId.

%% The owner of the contract is (initially) the account that created it.
-spec owner_pubkey(contract()) -> aec_keys:pubkey().
owner_pubkey(#contract{owner_id = OwnerId}) ->
    aeser_id:specialize(OwnerId, account).

%% The ABI version used by the contract.
-spec abi_version(contract()) -> abi_version().
abi_version(#contract{ct_version = #{abi := ABIVersion}}) ->
    ABIVersion.

%% The version used by the contract.
-spec ct_version(contract()) -> version().
ct_version(#contract{ct_version = CTVersion}) ->
    CTVersion.

%% The VM version used by the contract.
-spec vm_version(contract()) -> vm_version().
vm_version(#contract{ct_version = #{vm := VmVersion}}) ->
    VmVersion.

%% The contract byte code.
-spec code(contract()) -> code_maybe().
code(#contract{code = Code}) ->
    Code.

%% The representation of the contract state data.
-spec state(contract()) -> store().
state(#contract{store = Store}) ->
    Store.

%% The log of the contract.
-spec log(contract()) -> binary().
log(#contract{log = Log}) ->
    Log.

%% Is the contract active or has it been deactivated?
-spec active(contract()) -> boolean().
active(#contract{active = Active}) ->
    Active.

%% A list of other contracts referring to this contract.
-spec referrer_ids(contract()) -> list(aeser_id:id()).
referrer_ids(#contract{referrer_ids = ReferrerIds}) ->
    ReferrerIds.

%% The amount deposited at contract creation.
-spec deposit(contract()) -> amount().
deposit(#contract{deposit = Deposit}) ->
    Deposit.

%%%===================================================================
%%% Setters

-spec set_pubkey(pubkey(), contract()) -> contract().
set_pubkey(X, C) ->
    C#contract{id = aeser_id:create(contract, assert_field(pubkey, X))}.

-spec set_owner(aec_keys:pubkey(), contract()) -> contract().
set_owner(X, C) ->
    C#contract{owner_id = aeser_id:create(account, assert_field(pubkey, X))}.

-spec set_code(binary(), contract()) -> contract().
set_code(X, C) ->
    C#contract{code = assert_field(code, {code, X})}.

-spec set_state(store(), contract()) -> contract().
set_state(X, C) ->
    internal_set_state(assert_field(store, X, C), C).

internal_set_state(X, C) ->
    C#contract{store = X}.

-spec set_log(binary(), contract()) -> contract().
set_log(X, C) ->
    C#contract{log = assert_field(log, X)}.

-spec set_active(boolean(), contract()) -> contract().
set_active(X, C) ->
    C#contract{active = assert_field(active, X)}.

-spec set_referrers([id()], contract()) -> contract().
set_referrers(X, C) ->
    C#contract{referrer_ids = [aeser_id:create(contract, Y)
                               || Y <- assert_field(referrers, X)]}.

-spec set_deposit(amount(), contract()) -> contract().
set_deposit(X, C) ->
    C#contract{deposit = assert_field(deposit, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(C) ->
    List = [ {pubkey,     pubkey(C)}
           , {owner,      owner_pubkey(C)}
           , {ct_version, C#contract.ct_version}
           , {code,       C#contract.code}
           , {store,      C#contract.store}
           , {log,        C#contract.log}
           , {active,     C#contract.active}
           , {referrers,  referrer_ids(C)}
           , {deposit,    C#contract.deposit}
           ],
    List1 = [try assert_field(X, Y, C), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> C;
        Other -> error({missing, Other})
    end.

assert_field(store = FieldKey, FieldValue, C) ->
    %% We can't afford to validate the mptree part of the store
    assert_field_store(FieldKey, aect_contracts_store:write_cache(FieldValue),
                       C#contract.ct_version),
    FieldValue;
assert_field(FieldKey, FieldValue, _) ->
    assert_field(FieldKey, FieldValue).

assert_field(pubkey, <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(owner,  <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(ct_version, X) ->
    case is_legal_version(X) of
        true  -> X;
        false -> error({illegal, ct_version, X})
    end;
assert_field(code, {code, C} = X) when is_binary(C)      -> X;
assert_field(code, {ref, Ref}) ->
    RefPK = aeser_id:specialize(Ref, contract),
    RefPK = assert_field(ref_pk, RefPK),
    {ref, Ref};
assert_field(ref_pk, <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(log, X)        when is_binary(X)            -> X;
assert_field(active, X)     when X =:= true; X =:= false -> X;
assert_field(referrers = Field, X) ->
    try [ assert_field(referrer, Y) || Y <- X ]
    catch _:_ -> error({illegal, Field, X}) end;
assert_field(referrer, <<_:?PUB_SIZE/binary>> = X)       -> X;
assert_field(deposit, X)    when is_integer(X), X >= 0   -> X;
assert_field(Field, X) -> error({illegal, Field, X}).

assert_field_store(store = Field, X, Version) when is_map(X) ->
    try
        F = fun(K, V, unused) ->
                    assert_field_store(store_k, K, Version),
                    assert_field_store(store_v, V, Version),
                    unused
            end,
        %% map iterator would limit memory usage though it is available from OTP 21.
        maps:fold(F, unused, X),
        X
    catch _:_ -> error({illegal, Field, X}) end;
assert_field_store(store_k = Field, X, #{vm := VM} = Version) when is_binary(X),
                                                                   byte_size(X) > 0,
                                                                   (?IS_AEVM_SOPHIA(VM)
                                                                    orelse
                                                                    VM =:= ?VM_AEVM_SOLIDITY_1) ->
    try true = aevm_eeevm_store:is_valid_key(Version, X)
    catch _:_ -> error({illegal, Field, X}) end;
assert_field_store(store_k, X, #{vm := VM}) when is_binary(X),
                                                 byte_size(X) > 0,
                                                 ?IS_FATE_SOPHIA(VM) ->
    X;
assert_field_store(store_v, X,_Version) when is_binary(X) -> X.

is_legal_version(#{vm := VM, abi := ABI}) ->
    case {VM, ABI} of
        {?VM_AEVM_SOPHIA_1,   ?ABI_AEVM_SOPHIA_1} -> true;
        {?VM_AEVM_SOPHIA_2,   ?ABI_AEVM_SOPHIA_1} -> true;
        {?VM_AEVM_SOPHIA_3,   ?ABI_AEVM_SOPHIA_1} -> true;
        {?VM_AEVM_SOPHIA_4,   ?ABI_AEVM_SOPHIA_1} -> true;
        {?VM_FATE_SOPHIA_1,   ?ABI_FATE_SOPHIA_1} -> true;
        {?VM_FATE_SOPHIA_2,   ?ABI_FATE_SOPHIA_1} -> true;
        {?VM_AEVM_SOLIDITY_1, ?ABI_SOLIDITY_1}    -> ?VM_AEVM_SOLIDITY_1_enabled;
        _                                         -> false
    end.

-spec split_vm_abi(serial_version()) -> version().
split_vm_abi(VMABI) ->
    <<VM:16, ABI:16>> = <<VMABI:32>>,
    case VM of
        0 -> #{vm => ABI, abi => ABI};
        _ -> #{vm => VM,  abi => ABI}
    end.

-spec pack_vm_abi(version()) -> serial_version().
pack_vm_abi(#{vm := ABI, abi := ABI}) when ABI =< ?ABI_SOLIDITY_1 ->
    ABI;
pack_vm_abi(#{vm := VM, abi := ABI}) ->
    <<VMABI:32>> = <<VM:16, ABI:16>>,
    VMABI.
