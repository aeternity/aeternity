%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Module defining the Meta transaction for Generalized Accounts
%%% @end
%%%=============================================================================
-module(aega_meta_tx).

-behavior(aetx).

-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/include/hard_forks.hrl").

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
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
        ]).
%% Additional getters
-export([abi_version/1,
         auth_data/1,
         auth_id/1,
         auth_id/2,
         call_id/2,
         ga_id/1,
         ga_pubkey/1,
         gas_limit/3,
         gas_price/1,
         inner_tx_was_succesful/2,
         tx/1
        ]).

-export([set_tx/2
        ]).

-define(NO_TTL_VSN, 2).
-define(PRE_IRIS_VSN, 1).
-define(GA_META_TX_TYPE, ga_meta_tx).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-type amount() :: aect_contracts:amount().

-record(ga_meta_tx, {
          ga_id       :: aeser_id:id(),
          auth_data   :: binary(),
          abi_version :: aect_contracts:abi_version(),
          gas         :: amount(),
          gas_price   :: amount(),
          fee         :: amount(),
          ttl         :: aetx:tx_ttl() | no_ttl,
          tx          :: aetx_sign:signed_tx()
        }).

-opaque tx() :: #ga_meta_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Getters

-spec ga_id(tx()) -> aeser_id:id().
ga_id(#ga_meta_tx{ga_id = GAId}) ->
    GAId.

-spec ga_pubkey(tx()) -> aec_keys:pubkey().
ga_pubkey(#ga_meta_tx{ga_id = GAId}) ->
    aeser_id:specialize(GAId, account).

-spec abi_version(tx()) -> aect_contracts:abi_version().
abi_version(#ga_meta_tx{abi_version = ABI}) ->
    ABI.

-spec gas(tx()) -> amount().
gas(#ga_meta_tx{gas = Gas}) ->
    Gas.

-spec gas_limit(tx(), non_neg_integer(), aec_hard_forks:protocol_vsn()) -> amount().
gas_limit(#ga_meta_tx{gas = Gas, tx = InnerTx}, Height, Protocol) ->
    aetx:inner_gas_limit(aetx_sign:tx(InnerTx), Height, Protocol) + Gas.

-spec gas_price(tx()) -> amount().
gas_price(#ga_meta_tx{gas_price = GasPrice}) ->
    GasPrice.

-spec auth_data(tx()) -> binary().
auth_data(#ga_meta_tx{auth_data = AuthData}) ->
    AuthData.

-spec auth_id(tx()) -> aect_call:id().
auth_id(#ga_meta_tx{auth_data = AuthData} = Tx) ->
    auth_id(ga_pubkey(Tx), AuthData).

-spec auth_id(binary(), binary()) -> aect_call:id().
auth_id(GAPubkey, AuthData) ->
    aec_hash:hash(pubkey, <<GAPubkey/binary, AuthData/binary>>).

-spec tx(tx()) -> aetx_sign:signed_tx().
tx(#ga_meta_tx{tx = Tx}) ->
    Tx.

-spec set_tx(aetx_sign:signed_tx(), tx()) -> tx().
set_tx(NewTx, Tx) ->
    Tx#ga_meta_tx{tx = NewTx}.

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#ga_meta_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ga_meta_tx{ttl = no_ttl, tx = SignedTx}) ->
    Aetx = aetx_sign:tx(SignedTx),
    aetx:ttl(Aetx); %% works for nested metas as well: it fetches innermost ttl
ttl(#ga_meta_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{ga_id       := GAId,
      auth_data   := AuthData,
      abi_version := ABIVersion,
      gas         := Gas,
      gas_price   := GasPrice,
      fee         := Fee,
      tx          := InnerTx} = Args) ->
    account = aeser_id:specialize_type(GAId),
    Tx = #ga_meta_tx{ga_id       = GAId,
                     auth_data   = AuthData,
                     abi_version = ABIVersion,
                     gas         = Gas,
                     gas_price   = GasPrice,
                     fee         = Fee,
                     ttl         = maps:get(ttl, Args, no_ttl),
                     tx          = InnerTx},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?GA_META_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ga_meta_tx{}) ->
    0.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ga_meta_tx{} = Tx) ->
    ga_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ga_meta_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ga_meta_tx{}, _) ->
    {ok, []}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#ga_meta_tx{} = Tx, Trees, Env0) ->
    AuthInstructions =
        aeprimop:ga_meta_tx_instructions(
          ga_pubkey(Tx),
          auth_data(Tx),
          abi_version(Tx),
          gas(Tx),
          gas_price(Tx),
          fee(Tx),
          tx(Tx)),
    Env = add_tx(Env0, aetx_sign:tx(tx(Tx))),
    case aeprimop:eval(AuthInstructions, Trees, Env) of
        {ok, Trees1, Env1} ->
            %% Successful authentication - we have a call object in Trees1
            Env11 = set_ga_context(Env1, Tx),
            {InnerRes, Trees2, Env2} =
                case aetx_sign:verify_w_env(tx(Tx), Trees1, Env11) of
                    ok ->
                        process_inner(aetx_sign:tx(tx(Tx)), Trees1, Env11);
                    Err = {error, _Reason} ->
                        {Err, Trees1, Env1}
                end,
            Env22   = reset_ga_context(Env2, Tx, Env1),
            Trees22 = set_meta_result(InnerRes, Tx, Trees2, Env22),
            {ok, Trees22, Env22};
        Err = {error, _} ->
            Err
    end.

%% Starting from Lima catch crashes and convert to {error, Reason}
process_inner(Tx, Trees, Env) ->
    case aetx_env:consensus_version(Env) of
        ?FORTUNA_PROTOCOL_VSN ->
            case aetx:process(Tx, Trees, Env) of
                {ok, Trees1, Env1}     -> {ok, Trees1, Env1};
                Err = {error, _Reason} -> {Err, Trees, Env}
            end;
        Vsn when Vsn >= ?LIMA_PROTOCOL_VSN ->
            try aetx:process(Tx, Trees, Env) of
                {ok, Trees1, Env1}     -> {ok, Trees1, Env1};
                Err = {error, _Reason} -> {Err, Trees, Env}
            catch _:Reason ->
                    {{error, Reason}, Trees, Env}
            end
    end.

set_meta_result(ok, _Tx, Trees, _Env) ->
    Trees;
set_meta_result({error, Reason0}, Tx, Trees, Env) ->
    Reason = inner_transaction_failed,
    lager:debug("Inner transaction failed with reason: ~p", [Reason0]),
    SetInstructions = aeprimop:ga_set_meta_tx_res_instructions(
                          ga_pubkey(Tx), auth_data(Tx), {error, Reason}),
    {ok, Trees1, _Env} = aeprimop:eval(SetInstructions, Trees, Env),
    Trees1.

-spec call_id(tx(), aec_trees:trees()) -> aect_call:id().
call_id(Tx, Trees) ->
    Pubkey   = ga_pubkey(Tx),
    Account  = aec_accounts_trees:get(Pubkey, aec_trees:accounts(Trees)),
    Contract = aec_accounts:ga_contract(Account),
    aect_call:ga_id(auth_id(Tx), aeser_id:specialize(Contract, contract)).

-spec inner_tx_was_succesful(tx(), aec_trees:trees()) -> boolean().
inner_tx_was_succesful(Tx, Trees) ->
    Pubkey   = ga_pubkey(Tx),
    CallId   = call_id(Tx, Trees),
    Call     = aect_call_state_tree:get_call(Pubkey, CallId, aec_trees:calls(Trees)),
    case aect_call:return_type(Call) of
        ok    -> true;
        error -> false
    end.

serialize(#ga_meta_tx{ga_id       = GAId,
                      auth_data   = AuthData,
                      abi_version = ABIVersion,
                      fee         = Fee,
                      gas         = Gas,
                      gas_price   = GasPrice,
                      ttl         = TTL,
                      tx          = InnerTx} = Tx) ->
    SerTx = aetx_sign:serialize_to_binary(InnerTx),
    Version = version(Tx),
    case Version of
        ?PRE_IRIS_VSN ->
            {Version,
            [ {ga_id, GAId}
            , {auth_data, AuthData}
            , {abi_version, ABIVersion}
            , {fee, Fee}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {ttl, TTL}
            , {tx, SerTx}
            ]};
        ?NO_TTL_VSN ->
            {Version,
            [ {ga_id, GAId}
            , {auth_data, AuthData}
            , {abi_version, ABIVersion}
            , {fee, Fee}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {tx, SerTx}
            ]}
    end.


deserialize(?NO_TTL_VSN,
            [ {ga_id, GAId}
            , {auth_data, AuthData}
            , {abi_version, ABIVersion}
            , {fee, Fee}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {tx, SerTx}]) ->
    account = aeser_id:specialize_type(GAId),
    Tx = aetx_sign:deserialize_from_binary(SerTx),
    #ga_meta_tx{ga_id       = GAId,
                auth_data   = AuthData,
                abi_version = ABIVersion,
                fee         = Fee,
                gas         = Gas,
                gas_price   = GasPrice,
                ttl         = no_ttl,
                tx          = Tx};
deserialize(?PRE_IRIS_VSN,
            [ {ga_id, GAId}
            , {auth_data, AuthData}
            , {abi_version, ABIVersion}
            , {fee, Fee}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {ttl, TTL}
            , {tx, SerTx}]) ->
    account = aeser_id:specialize_type(GAId),
    Tx = aetx_sign:deserialize_from_binary(SerTx),
    #ga_meta_tx{ga_id       = GAId,
                auth_data   = AuthData,
                abi_version = ABIVersion,
                fee         = Fee,
                gas         = Gas,
                gas_price   = GasPrice,
                ttl         = TTL,
                tx          = Tx}.

serialization_template(?NO_TTL_VSN) ->
    [ {ga_id, id}
    , {auth_data, binary}
    , {abi_version, int}
    , {fee, int}
    , {gas, int}
    , {gas_price, int}
    , {tx, binary}
    ];
serialization_template(?PRE_IRIS_VSN) ->
    [ {ga_id, id}
    , {auth_data, binary}
    , {abi_version, int}
    , {fee, int}
    , {gas, int}
    , {gas_price, int}
    , {ttl, int}
    , {tx, binary}
    ].

for_client(#ga_meta_tx{ ga_id       = GAId,
                        auth_data   = AuthData,
                        abi_version = ABIVersion,
                        fee         = Fee,
                        gas         = Gas,
                        gas_price   = GasPrice,
                        ttl         = TTL,
                        tx          = InnerTx}) ->
    M = #{<<"ga_id">>       => aeser_api_encoder:encode(id_hash, GAId),
          <<"auth_data">>   => aeser_api_encoder:encode(contract_bytearray, AuthData),
          <<"abi_version">> => ABIVersion,
          <<"fee">>         => Fee,
          <<"gas">>         => Gas,
          <<"gas_price">>   => GasPrice,
          <<"ttl">>         => TTL,
          <<"tx">>          => aetx_sign:serialize_for_client_inner(InnerTx, #{})},
    case TTL of
        no_ttl -> maps:remove(<<"ttl">>, M);
        I when is_integer(I) -> M
    end.

-spec version(tx()) -> non_neg_integer().
version(#ga_meta_tx{ttl = no_ttl}) ->
    ?NO_TTL_VSN;
version(_) ->
    ?PRE_IRIS_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(P, #ga_meta_tx{ ttl = no_ttl}) when P < ?IRIS_PROTOCOL_VSN ->
    %% TTL is mandatory before Iris
    false;
valid_at_protocol(P, #ga_meta_tx{ ttl = TTL}) when P >= ?IRIS_PROTOCOL_VSN
                                                   andalso is_integer(TTL) ->
    %% TTL is no more from Iris on
    false;
valid_at_protocol(P, #ga_meta_tx{ tx = SignedTx }) ->
    P >= ?FORTUNA_PROTOCOL_VSN andalso
        aetx:valid_at_protocol(P, aetx_sign:tx(SignedTx)).

%%%===================================================================
%%% Internal functions

add_tx(Env0, Tx) ->
    BinForNetwork = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    aetx_env:set_ga_tx(
      aetx_env:set_ga_tx_hash(Env0, aec_hash:hash(tx, BinForNetwork)), Tx).

set_ga_context(Env0, Tx) ->
    Env1 = aetx_env:set_context(Env0, aetx_ga),
    Env2 = aetx_env:add_ga_auth_id(Env1, ga_pubkey(Tx)),
    Env3 = aetx_env:add_ga_nonce(Env2, ga_pubkey(Tx), auth_id(Tx)),
    aetx_env:set_ga_tx_hash(Env3, undefined).

reset_ga_context(Env0, Tx, OldEnv) ->
    Env1 = aetx_env:set_context(Env0, aetx_env:context(OldEnv)),
    Env2 = aetx_env:del_ga_auth_id(Env1, ga_pubkey(Tx)),
    aetx_env:del_ga_nonce(Env2, ga_pubkey(Tx)).
