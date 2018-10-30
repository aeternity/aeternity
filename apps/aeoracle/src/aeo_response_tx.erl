%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_response_tx).

-include("oracle_txs.hrl").

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
-export([oracle_id/1,
         oracle_pubkey/1,
         query_id/1,
         response/1,
         response_ttl/1]).


-define(ORACLE_RESPONSE_TX_VSN, 1).
-define(ORACLE_RESPONSE_TX_TYPE, oracle_response_tx).

-record(oracle_response_tx, {
          oracle_id    :: aec_id:id(),
          nonce        :: integer(),
          query_id     :: aeo_query:id(),
          response     :: aeo_oracles:response(),
          response_ttl :: aeo_oracles:relative_ttl(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_response_tx{}.

-export_type([tx/0]).

-spec oracle_id(tx()) -> aec_id:id().
oracle_id(#oracle_response_tx{oracle_id = OracleId}) ->
    OracleId.

-spec oracle_pubkey(tx()) -> aec_keys:pubkey().
oracle_pubkey(#oracle_response_tx{oracle_id = OracleId}) ->
    aec_id:specialize(OracleId, oracle).

-spec query_id(tx()) -> aeo_query:id().
query_id(#oracle_response_tx{query_id = QueryId}) ->
    QueryId.

-spec response(tx()) -> aeo_query:oracle_response().
response(#oracle_response_tx{response = Response}) ->
    Response.

-spec response_ttl(tx()) -> aeo_oracles:relative_ttl().
response_ttl(#oracle_response_tx{response_ttl = ResponseTTL}) ->
    ResponseTTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{oracle_id    := OracleId,
      nonce        := Nonce,
      query_id     := QueryId,
      response     := Response,
      response_ttl := ResponseTTL,
      fee          := Fee} = Args) ->
    oracle = aec_id:specialize_type(OracleId),
    Tx = #oracle_response_tx{oracle_id    = OracleId,
                             nonce        = Nonce,
                             query_id     = QueryId,
                             response     = Response,
                             response_ttl = ResponseTTL,
                             fee          = Fee,
                             ttl          = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_RESPONSE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#oracle_response_tx{fee = F}) ->
    F.

-spec gas(tx()) -> non_neg_integer().
gas(#oracle_response_tx{}) ->
    aec_governance:tx_base_gas(oracle_response_tx).

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#oracle_response_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_response_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#oracle_response_tx{} = Tx) ->
    oracle_pubkey(Tx).

%% Oracle should exist, and have enough funds for the fee.
%% QueryId id should match oracle.
-spec check(tx(), aec_trees:trees(), aetx_env:env()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_response_tx{nonce = Nonce, query_id = QueryId,
                          fee = Fee, response_ttl = ResponseTTL} = Tx, Trees, Env) ->
    Height = aetx_env:height(Env),
    OraclePubKey = oracle_pubkey(Tx),
    case fetch_query(OraclePubKey, QueryId, Trees) of
        {value, I} ->
            QueryResponseTTL = aeo_query:response_ttl(I),
            QueryFee         = aeo_query:fee(I),
            Checks =
                [fun() -> check_response_ttl(ResponseTTL, QueryResponseTTL) end,
                 fun() -> check_oracle(OraclePubKey, Trees) end,
                 fun() -> check_query(OraclePubKey, I) end |
                 case aetx_env:context(Env) of
                     aetx_contract -> []; %% TODO: Handle fees from contracts right.
                     aetx_transaction ->
                         [fun() -> aetx_utils:check_account(OraclePubKey, Trees,
                                                            Nonce, Fee - QueryFee) end,
                          fun() -> aeo_utils:check_ttl_fee(Height, ResponseTTL,
                                                           Fee - aec_governance:minimum_tx_fee()) end]
                 end],
            case aeu_validation:run(Checks) of
                ok              -> {ok, Trees};
                {error, Reason} -> {error, Reason}
            end;
        none -> {error, no_matching_oracle_query}
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_response_tx{} = Tx, _) ->
    {ok, [oracle_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#oracle_response_tx{nonce = Nonce, query_id = QueryId,
                            response = Response, fee = Fee} = Tx,
        Trees0, Env) ->
    Height = aetx_env:height(Env),
    OraclePubKey  = oracle_pubkey(Tx),
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Query0 = aeo_state_tree:get_query(OraclePubKey, QueryId, OraclesTree0),
    Query1 = aeo_query:add_response(Height, Response, Query0),
    OraclesTree1 = aeo_state_tree:enter_query(Query1, OraclesTree0),

    OracleAccount0 = aec_accounts_trees:get(OraclePubKey, AccountsTree0),
    {ok, OracleAccount1} = aec_accounts:spend(OracleAccount0, Fee, Nonce),
    QueryFee = aeo_query:fee(Query0),
    {ok, OracleAccount2} = aec_accounts:earn(OracleAccount1, QueryFee),
    AccountsTree1 = aec_accounts_trees:enter(OracleAccount2, AccountsTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_response_tx{oracle_id    = OracleId,
                              nonce        = Nonce,
                              query_id     = QueryId,
                              response     = Response,
                              response_ttl = {?ttl_delta_atom, ResponseTTLValue},
                              fee          = Fee,
                              ttl          = TTL}) ->
    {version(),
    [ {oracle_id, OracleId}
    , {nonce, Nonce}
    , {query_id, QueryId}
    , {response, Response}
    , {response_ttl_type, ?ttl_delta_int}
    , {response_ttl_value, ResponseTTLValue}
    , {fee, Fee}
    , {ttl, TTL}
    ]}.

deserialize(?ORACLE_RESPONSE_TX_VSN,
            [ {oracle_id, OracleId}
            , {nonce, Nonce}
            , {query_id, QueryId}
            , {response, Response}
            , {response_ttl_type, ?ttl_delta_int}
            , {response_ttl_value, ResponseTTLValue}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    oracle = aec_id:specialize_type(OracleId),
    #oracle_response_tx{oracle_id    = OracleId,
                        nonce        = Nonce,
                        query_id     = QueryId,
                        response     = Response,
                        response_ttl = {?ttl_delta_atom, ResponseTTLValue},
                        fee          = Fee,
                        ttl          = TTL}.

serialization_template(?ORACLE_RESPONSE_TX_VSN) ->
    [ {oracle_id, id}
    , {nonce, int}
    , {query_id, binary}
    , {response, binary}
    , {response_ttl_type, int}
    , {response_ttl_value, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_RESPONSE_TX_VSN.

for_client(#oracle_response_tx{oracle_id = OracleId,
                               nonce     = Nonce,
                               query_id  = QueryId,
                               response  = Response,
                               fee       = Fee,
                               ttl       = TTL} = Tx) ->
    {ResponseTTLType = delta, ResponseTTLValue} = response_ttl(Tx),
    #{<<"oracle_id">>   => aec_base58c:encode(id_hash, OracleId),
      <<"nonce">>       => Nonce,
      <<"query_id">>    => aec_base58c:encode(oracle_query_id, QueryId),
      <<"response">>    => Response,
      <<"response_ttl">> => #{<<"type">>  => ResponseTTLType,
                              <<"value">> => ResponseTTLValue},
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL}.

%% -- Local functions  -------------------------------------------------------

fetch_query(OraclePubkey, QueryId, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    aeo_state_tree:lookup_query(OraclePubkey, QueryId, OraclesTree).

check_response_ttl(RTTL, RTTL)     -> ok;
check_response_ttl(_RTTL1, _RTTL2) -> {error, oracle_response_has_wrong_response_ttl}.

check_query(OraclePubKey, I) ->
    case OraclePubKey == aeo_query:oracle_pubkey(I) of
        true  ->
            case aeo_query:is_open(I) of
                true -> ok;
                false  -> {error, oracle_closed_for_response}
            end;
        false -> {error, oracle_does_not_match_query_id}
    end.

check_oracle(OraclePubKey, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(OraclePubKey, OraclesTree) of
        {value, _Oracle} -> ok;
        none -> {error, oracle_does_not_exist}
    end.

