%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_register_tx).

-include("oracle_txs.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         vm_version/1,
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
-export([account_id/1,
         account_pubkey/1,
         query_format/1,
         query_fee/1,
         response_format/1,
         oracle_ttl/1]).

-define(ORACLE_REGISTER_TX_VSN, 1).
-define(ORACLE_REGISTER_TX_TYPE, oracle_register_tx).

-record(oracle_register_tx, {
          account_id                                    :: aec_id:id(),
          nonce                                         :: integer(),
          query_format    = <<>>                        :: aeo_oracles:type_format(),
          response_format = <<>>                        :: aeo_oracles:type_format(),
          query_fee                                     :: integer(),
          oracle_ttl                                    :: aeo_oracles:ttl(),
          fee                                           :: integer(),
          ttl                                           :: aetx:tx_ttl(),
          vm_version = 0                                :: non_neg_integer()
          }).

-opaque tx() :: #oracle_register_tx{}.

-export_type([tx/0]).

-spec account_id(tx()) -> aec_id:id().
account_id(#oracle_register_tx{account_id = AccountId}) ->
    AccountId.

-spec account_pubkey(tx()) -> aec_keys:pubkey().
account_pubkey(#oracle_register_tx{account_id = AccountId}) ->
    aec_id:specialize(AccountId, account).

-spec query_format(tx()) -> aeo_oracles:type_format().
query_format(#oracle_register_tx{query_format = QueryFormat}) ->
    QueryFormat.

-spec response_format(tx()) -> aeo_oracles:type_format().
response_format(#oracle_register_tx{response_format = ResponseFormat}) ->
    ResponseFormat.

-spec query_fee(tx()) -> integer().
query_fee(#oracle_register_tx{query_fee = QueryFee}) ->
    QueryFee.

-spec oracle_ttl(tx()) -> aeo_oracles:ttl().
oracle_ttl(#oracle_register_tx{oracle_ttl = TTL}) ->
    TTL.

-spec fee(tx()) -> integer().
fee(#oracle_register_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#oracle_register_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#oracle_register_tx{ttl = TTL}) ->
    TTL.

-spec vm_version(tx()) -> non_neg_integer().
vm_version(#oracle_register_tx{vm_version = VMVersion}) ->
    VMVersion.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id      := AccountId,
      nonce           := Nonce,
      query_format    := QueryFormat,
      response_format := ResponseFormat,
      query_fee       := QueryFee,
      oracle_ttl      := OracleTTL,
      fee             := Fee
     } = Args) ->
    account = aec_id:specialize_type(AccountId),
    Tx = #oracle_register_tx{account_id      = AccountId,
                             nonce           = Nonce,
                             query_format    = QueryFormat,
                             response_format = ResponseFormat,
                             query_fee       = QueryFee,
                             oracle_ttl      = OracleTTL,
                             fee             = Fee,
                             vm_version      = maps:get(vm_version, Args, 0),
                             ttl             = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_REGISTER_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_register_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#oracle_register_tx{} = Tx) ->
    account_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_register_tx{}, Trees, _Env) ->
    %% Checks are in process/3.
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_register_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#oracle_register_tx{} = RTx, Trees, Env) ->
    Height = aetx_env:height(Env),
    case aeo_utils:ttl_delta(Height, oracle_ttl(RTx)) of
        {error, _} = Err -> Err;
        DeltaTTL when is_integer(DeltaTTL) ->
            Instructions =
                aec_tx_processor:oracle_register_tx_instructions(
                  account_pubkey(RTx),
                  query_format(RTx),
                  response_format(RTx),
                  query_fee(RTx),
                  DeltaTTL,
                  vm_version(RTx),
                  fee(RTx),
                  nonce(RTx)
                 ),
            aec_tx_processor:eval(Instructions, Trees, Env)
    end.

serialize(#oracle_register_tx{account_id      = AccountId,
                              nonce           = Nonce,
                              query_format    = QueryFormat,
                              response_format = ResponseFormat,
                              query_fee       = QueryFee,
                              oracle_ttl      = {TTLType0, TTLValue},
                              fee             = Fee,
                              ttl             = TTL,
                              vm_version      = VMVersion}) ->
    TTLType = case TTLType0 of
                  ?ttl_delta_atom -> ?ttl_delta_int;
                  ?ttl_block_atom -> ?ttl_block_int
              end,

    {version(),
    [ {account_id, AccountId}
    , {nonce, Nonce}
    , {query_format, QueryFormat}
    , {response_format, ResponseFormat}
    , {query_fee, QueryFee}
    , {ttl_type, TTLType}
    , {ttl_value, TTLValue}
    , {fee, Fee}
    , {ttl, TTL}
    , {vm_version, VMVersion}
    ]}.

deserialize(?ORACLE_REGISTER_TX_VSN,
           [ {account_id, AccountId}
           , {nonce, Nonce}
           , {query_format, QueryFormat}
           , {response_format, ResponseFormat}
           , {query_fee, QueryFee}
           , {ttl_type, TTLType0}
           , {ttl_value, TTLValue}
           , {fee, Fee}
           , {ttl, TTL}
           , {vm_version, VMVersion}
           ]) ->
    TTLType = case TTLType0 of
                  ?ttl_delta_int -> ?ttl_delta_atom;
                  ?ttl_block_int -> ?ttl_block_atom
              end,
    account = aec_id:specialize_type(AccountId),
    #oracle_register_tx{account_id      = AccountId,
                        nonce           = Nonce,
                        query_format    = QueryFormat,
                        response_format = ResponseFormat,
                        query_fee       = QueryFee,
                        oracle_ttl      = {TTLType, TTLValue},
                        fee             = Fee,
                        ttl             = TTL,
                        vm_version      = VMVersion
                       }.

serialization_template(?ORACLE_REGISTER_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {query_format, binary}
    , {response_format, binary}
    , {query_fee, int}
    , {ttl_type, int}
    , {ttl_value, int}
    , {fee, int}
    , {ttl, int}
    , {vm_version, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_REGISTER_TX_VSN.

for_client(#oracle_register_tx{account_id      = AccountId,
                               nonce           = Nonce,
                               query_format    = QueryFormat,
                               response_format = ResponseFormat,
                               query_fee       = QueryFee,
                               fee             = Fee,
                               ttl             = TTL,
                               vm_version      = VMVersion
                              } = Tx) ->
    {TTLType, TTLValue} = oracle_ttl(Tx),
    #{<<"account_id">>      => aehttp_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>           => Nonce,
      <<"query_format">>    => QueryFormat,
      <<"response_format">> => ResponseFormat,
      <<"query_fee">>       => QueryFee,
      <<"oracle_ttl">>      => #{<<"type">>  => TTLType,
                                 <<"value">> => TTLValue},
      <<"fee">>             => Fee,
      <<"ttl">>             => TTL,
      <<"vm_version">>      => VMVersion
     }.
