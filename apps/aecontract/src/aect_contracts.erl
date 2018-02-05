%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract objects
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([ deserialize/1
        , id/1
        , new/3
        , serialize/1
        , compute_contract_pubkey/2
          %% Getters
        , pubkey/1
        , balance/1
        , height/1
        , owner/1
        , vm_version/1
        , code/1
        , state/1
        , log/1
        , active/1
        , referers/1
        , deposit/1
          %% Setters
        , set_pubkey/2
        , set_balance/2
        , set_height/2
        , set_owner/2
        , set_vm_version/2
        , set_code/2
        , set_state/2
        , set_log/2
        , set_active/2
        , set_referers/2
        , set_deposit/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type amount() :: non_neg_integer().

-record(contract, {
        %% Normal account fields
        pubkey     :: pubkey(),
        balance    :: amount(),
        height     :: height(),
        %% Contract-specific fields
        owner      :: pubkey(),
        vm_version :: vm_version(),
        code       :: binary(),     %% The byte code
        state      :: binary(),     %% The current state
        log        :: binary(),     %% The current event log
        active     :: boolean(),    %% false when disabled, but don't remove unless referers == []
        referers   :: [pubkey()],   %% List of contracts depending on this contract
        deposit    :: amount()
    }).

-opaque contract() :: #contract{}.

-type id() :: pubkey().
-type serialized() :: binary().
-type vm_version() :: byte().

-export_type([ contract/0
             , pubkey/0
             , amount/0
             , id/0
             , serialized/0
             , vm_version/0
             ]).

-define(PUB_SIZE, 65).
-define(HASH_SIZE, 32).
-define(CONTRACT_TYPE, <<"contract">>).
-define(CONTRACT_VSN, 0).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(contract()) -> id().
id(C) ->
  pubkey(C).

-spec new(pubkey(), aect_create_tx:create_tx(), height()) -> contract().
new(ContractPubKey, RTx, BlockHeight) ->
    C = #contract{ pubkey     = ContractPubKey,
                   balance    = aect_create_tx:amount(RTx),
                   height     = BlockHeight,
                   %% Contract-specific fields
                   owner      = aect_create_tx:owner(RTx),
                   vm_version = aect_create_tx:vm_version(RTx),
                   code       = aect_create_tx:code(RTx),
                   state      = <<>>,
                   log        = <<>>,
                   active     = true,
                   referers   = [],
                   deposit    = aect_create_tx:deposit(RTx)
                 },
    C = assert_fields(C),
    %% TODO: call the init function to initialise the state
    C.

-spec serialize(contract()) -> serialized().
serialize(#contract{} = C) ->
    msgpack:pack([ #{<<"type">>       => ?CONTRACT_TYPE}
                 , #{<<"vsn">>        => ?CONTRACT_VSN}
                 , #{<<"pubkey">>     => pubkey(C)}
                 , #{<<"balance">>    => balance(C)}
                 , #{<<"height">>     => height(C)}
                 , #{<<"owner">>      => owner(C)}
                 , #{<<"vm_version">> => vm_version(C)}
                 , #{<<"code">>       => code(C)}
                 , #{<<"state">>      => state(C)}
                 , #{<<"log">>        => log(C)}
                 , #{<<"active">>     => active(C)}
                 , #{<<"referers">>   => referers(C)}
                 , #{<<"deposit">>    => deposit(C)}
                 ]).

-spec deserialize(serialized()) -> contract().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [ #{<<"type">>       := ?CONTRACT_TYPE}
    , #{<<"vsn">>        := ?CONTRACT_VSN}
    , #{<<"pubkey">>     := Pubkey}
    , #{<<"balance">>    := Balance}
    , #{<<"height">>     := Height}
    , #{<<"owner">>      := Owner}
    , #{<<"vm_version">> := VmVersion}
    , #{<<"code">>       := Code}
    , #{<<"state">>      := Store}
    , #{<<"log">>        := Log}
    , #{<<"active">>     := Active}
    , #{<<"referers">>   := Referers}
    , #{<<"deposit">>    := Deposit}
    ] = List,
    #contract{ pubkey     = Pubkey
             , balance    = Balance
             , height     = Height
             , owner      = Owner
             , vm_version = VmVersion
             , code       = Code
             , state      = Store
             , log        = Log
             , active     = Active
             , referers   = Referers
             , deposit    = Deposit
             }.

-spec compute_contract_pubkey(pubkey(), non_neg_integer()) -> pubkey().
compute_contract_pubkey(Owner, Nonce) ->
    %% TODO: do this in a less ad-hoc way?
    Hash = aec_hash:hash(pubkey, <<Nonce:64, Owner/binary>>),
    <<"0x", HexHash/binary>> = list_to_binary(aect_utils:hex_bytes(Hash)),
    <<PubKey:?PUB_SIZE/binary, _/binary>> = <<"C0DE", HexHash/binary>>,
    PubKey.

%%%===================================================================
%%% Getters

%% The address of the contract account.
-spec pubkey(contract()) -> pubkey().
pubkey(C) -> C#contract.pubkey.

%% The balance of the contract.
-spec balance(contract()) -> amount().
balance(C) -> C#contract.balance.

%% The block height of the last transaction on the contract.
-spec height(contract()) -> height().
height(C) -> C#contract.height.

%% The owner of the contract is (initially) the account that created it.
-spec owner(contract()) -> pubkey().
owner(C) -> C#contract.owner.

%% The VM version used by the contract.
-spec vm_version(contract()) -> vm_version().
vm_version(C) -> C#contract.vm_version.

%% The contract byte code.
-spec code(contract()) -> binary().
code(C) -> C#contract.code.

%% The binary representation of the contract state data.
-spec state(contract()) -> binary().
state(C) -> C#contract.state.

%% The log of the contract.
-spec log(contract()) -> binary().
log(C) -> C#contract.log.

%% Is the contract active or has it been deactivated?
-spec active(contract()) -> boolean().
active(C) -> C#contract.active.

%% A list of other contracts referring to this contract.
-spec referers(contract()) -> [pubkey()].
referers(C) -> C#contract.referers.

%% The amount deposited at contract creation.
-spec deposit(contract()) -> amount().
deposit(C) -> C#contract.deposit.

%%%===================================================================
%%% Setters

-spec set_pubkey(pubkey(), contract()) -> contract().
set_pubkey(X, C) ->
    C#contract{pubkey = assert_field(pubkey, X)}.

-spec set_balance(amount(), contract()) -> contract().
set_balance(X, C) ->
    C#contract{balance = assert_field(balance, X)}.

-spec set_height(height(), contract()) -> contract().
set_height(X, C) ->
    C#contract{height = assert_field(height, X)}.

-spec set_owner(pubkey(), contract()) -> contract().
set_owner(X, C) ->
    C#contract{owner = assert_field(owner, X)}.

-spec set_vm_version(vm_version(), contract()) -> contract().
set_vm_version(X, C) ->
    C#contract{vm_version = assert_field(vm_version, X)}.

-spec set_code(binary(), contract()) -> contract().
set_code(X, C) ->
    C#contract{code = assert_field(code, X)}.

-spec set_state(binary(), contract()) -> contract().
set_state(X, C) ->
    C#contract{state = assert_field(state, X)}.

-spec set_log(binary(), contract()) -> contract().
set_log(X, C) ->
    C#contract{log = assert_field(log, X)}.

-spec set_active(boolean(), contract()) -> contract().
set_active(X, C) ->
    C#contract{active = assert_field(active, X)}.

-spec set_referers([pubkey()], contract()) -> contract().
set_referers(X, C) ->
    C#contract{referers = assert_field(referers, X)}.

-spec set_deposit(amount(), contract()) -> contract().
set_deposit(X, C) ->
    C#contract{deposit = assert_field(deposit, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(C) ->
    List = [ {pubkey,     C#contract.pubkey}
           , {balance,    C#contract.balance}
           , {height,     C#contract.height}
           , {owner,      C#contract.owner}
           , {vm_version, C#contract.vm_version}
           , {code,       C#contract.code}
           , {state,      C#contract.state}
           , {log,        C#contract.log}
           , {active,     C#contract.active}
           , {referers,   C#contract.referers}
           , {deposit,    C#contract.deposit}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> C;
        Other -> error({missing, Other})
    end.

assert_field(pubkey, <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(balance, X)    when is_integer(X), X >= 0   -> X;
assert_field(height, X)     when is_integer(X), X > 0    -> X;
assert_field(owner,  <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(vm_version, X) when is_integer(X), X >= 0   -> X;
assert_field(code, X)       when is_binary(X)            -> X;
assert_field(state, X)      when is_binary(X)            -> X;
assert_field(log, X)        when is_binary(X)            -> X;
assert_field(active, X)     when X =:= true; X =:= false -> X;
assert_field(referers = Field, X) ->
    try [ assert_field(referer, Y) || Y <- X ]
    catch _:_ -> error({illegal, Field, X}) end;
assert_field(referer, <<_:?PUB_SIZE/binary>> = X)        -> X;
assert_field(deposit, X)    when is_integer(X), X >= 0   -> X;
assert_field(Field, X) -> error({illegal, Field, X}).
