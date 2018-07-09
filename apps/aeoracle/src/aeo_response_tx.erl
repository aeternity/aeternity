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
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Additional getters
-export([oracle/1,
         query_id/1,
         response/1]).


-define(ORACLE_RESPONSE_TX_VSN, 1).
-define(ORACLE_RESPONSE_TX_TYPE, oracle_response_tx).
-define(ORACLE_RESPONSE_TX_FEE, 2).

-record(oracle_response_tx, {
          oracle   :: aec_id:id(),
          nonce    :: integer(),
          query_id :: aeo_query:id(),
          response :: aeo_oracles:response(),
          fee      :: integer(),
          ttl      :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_response_tx{}.

-export_type([tx/0]).

-spec oracle(tx()) -> aec_id:id().
oracle(#oracle_response_tx{oracle = Oracle}) ->
    Oracle.

oracle_pubkey(#oracle_response_tx{oracle = Oracle}) ->
    aec_id:specialize(Oracle, oracle).

-spec query_id(tx()) -> aeo_query:id().
query_id(#oracle_response_tx{query_id = QId}) ->
    QId.

-spec response(tx()) -> aeo_query:oracle_response().
response(#oracle_response_tx{response = Response}) ->
    Response.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{oracle   := Oracle,
      nonce    := Nonce,
      query_id := QId,
      response := Response,
      fee      := Fee} = Args) ->
    oracle = aec_id:specialize_type(Oracle),
    Tx = #oracle_response_tx{oracle   = Oracle,
                             nonce    = Nonce,
                             query_id = QId,
                             response = Response,
                             fee      = Fee,
                             ttl      = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_RESPONSE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#oracle_response_tx{fee = F}) ->
    F.

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
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_response_tx{nonce = Nonce, query_id = QId, fee = Fee} = Tx,
      Context, Trees, Height, _ConsensusVersion) ->
    OraclePubKey = oracle_pubkey(Tx),
    case fetch_query(OraclePubKey, QId, Trees) of
        {value, I} ->
            ResponseTTL = aeo_query:response_ttl(I),
            QueryFee    = aeo_query:fee(I),
            Checks =
                [fun() -> check_oracle(OraclePubKey, Trees) end,
                 fun() -> check_query(OraclePubKey, I) end |
                 case Context of
                     aetx_contract -> []; %% TODO: Handle fees from contracts right.
                     aetx_transaction ->
                         [fun() -> aetx_utils:check_account(OraclePubKey, Trees,
                                                            Nonce, Fee - QueryFee) end,
                          fun() -> aeo_utils:check_ttl_fee(Height, ResponseTTL,
                                                           Fee - ?ORACLE_RESPONSE_TX_FEE) end]
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

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#oracle_response_tx{nonce = Nonce, query_id = QId, response = Response,
                            fee = Fee} = Tx,
        _Context, Trees0, Height, _ConsensusVersion) ->
    OraclePubKey  = oracle_pubkey(Tx),
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Query0 = aeo_state_tree:get_query(OraclePubKey, QId, OraclesTree0),
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

serialize(#oracle_response_tx{oracle   = OracleId,
                              nonce    = Nonce,
                              query_id = QId,
                              response = Response,
                              fee      = Fee,
                              ttl      = TTL}) ->
    {version(),
    [ {oracle, OracleId}
    , {nonce, Nonce}
    , {query_id, QId}
    , {response, Response}
    , {fee, Fee}
    , {ttl, TTL}
    ]}.

deserialize(?ORACLE_RESPONSE_TX_VSN,
            [ {oracle, OracleId}
            , {nonce, Nonce}
            , {query_id, QId}
            , {response, Response}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    oracle = aec_id:specialize_type(OracleId),
    #oracle_response_tx{oracle   = OracleId,
                        nonce    = Nonce,
                        query_id = QId,
                        response = Response,
                        fee      = Fee,
                        ttl      = TTL}.

serialization_template(?ORACLE_RESPONSE_TX_VSN) ->
    [ {oracle, id}
    , {nonce, int}
    , {query_id, binary}
    , {response, binary}
    , {fee, int}
    , {ttl, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_RESPONSE_TX_VSN.

for_client(#oracle_response_tx{ nonce    = Nonce,
                                query_id = QId,
                                response = Response,
                                fee      = Fee,
                                ttl      = TTL} = Tx) ->
    #{<<"data_schema">> => <<"OracleResponseTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"oracle">> => aec_base58c:encode(id_hash, oracle(Tx)),
      <<"nonce">> => Nonce,
      <<"query_id">> => aec_base58c:encode(oracle_query_id, QId),
      <<"response">> => Response,
      <<"fee">> => Fee,
      <<"ttl">> => TTL}.

%% -- Local functions  -------------------------------------------------------

fetch_query(OId, QId, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    aeo_state_tree:lookup_query(OId, QId, OraclesTree).

check_query(OraclePubKey, I) ->
    case OraclePubKey == aeo_query:oracle_address(I) of
        true  ->
            case aeo_query:is_closed(I) of
                true  -> {error, oracle_closed_for_response};
                false -> ok
            end;
        false -> {error, oracle_does_not_match_query_id}
    end.

check_oracle(OraclePubKey, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(OraclePubKey, OraclesTree) of
        {value, _Oracle} -> ok;
        none -> {error, oracle_does_not_exist}
    end.
