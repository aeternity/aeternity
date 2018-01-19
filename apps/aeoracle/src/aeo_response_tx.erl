%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_response_tx).

-include("oracle_txs.hrl").
-include_lib("apps/aecore/include/trees.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/1,
         serialize/1,
         deserialize/1,
         type/0,
         for_client/1
        ]).

%% Additional getters
-export([oracle/1,
         query_id/1,
         response/1]).


-define(ORACLE_RESPONSE_TX_TYPE, <<"oracle_response">>).
-define(ORACLE_RESPONSE_TX_VSN, 1).
-define(ORACLE_RESPONSE_TX_FEE, 2).

-opaque response_tx() :: #oracle_response_tx{}.

-export_type([response_tx/0]).

-spec oracle(response_tx()) -> pubkey().
oracle(#oracle_response_tx{oracle = OraclePubKey}) ->
    OraclePubKey.

-spec query_id(response_tx()) -> aeo_query:oracle_tx_id().
query_id(#oracle_response_tx{query_id = QId}) ->
    QId.

-spec response(response_tx()) -> aeo_query:oracle_response().
response(#oracle_response_tx{response = Response}) ->
    Response.

-spec new(map()) -> {ok, response_tx()}.
new(#{oracle   := Oracle,
      nonce    := Nonce,
      query_id := QId,
      response := Response,
      fee      := Fee}) ->
    {ok, #oracle_response_tx{oracle   = Oracle,
                             nonce    = Nonce,
                             query_id = QId,
                             response = Response,
                             fee      = Fee}}.

-spec fee(response_tx()) -> integer().
fee(#oracle_response_tx{fee = F}) ->
    F.

-spec nonce(response_tx()) -> non_neg_integer().
nonce(#oracle_response_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(response_tx()) -> pubkey().
origin(#oracle_response_tx{oracle = OraclePubKey}) ->
    OraclePubKey.

%% Oracle should exist, and have enough funds for the fee.
%% QueryId id should match oracle.
-spec check(response_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#oracle_response_tx{oracle = OraclePubKey, nonce = Nonce,
                          query_id = QId, fee = Fee}, Trees, Height) ->
    case fetch_query(OraclePubKey, QId, Trees) of
        {value, I} ->
            ResponseTTL = aeo_query:response_ttl(I),
            QueryFee    = aeo_query:fee(I),
            Checks =
                [fun() -> check_oracle(OraclePubKey, Trees) end,
                 fun() -> check_query(I, OraclePubKey) end,
                 fun() -> aetx_utils:check_account(OraclePubKey, Trees,
                                                   Height, Nonce, Fee - QueryFee) end,
                 fun() -> aeo_utils:check_ttl_fee(Height, ResponseTTL,
                                                  Fee - ?ORACLE_RESPONSE_TX_FEE) end
                ],
            case aeu_validation:run(Checks) of
                ok              -> {ok, Trees};
                {error, Reason} -> {error, Reason}
            end;
        none -> {error, no_matching_oracle_query}
    end.

-spec signers(response_tx()) -> [pubkey()].
signers(#oracle_response_tx{oracle = OraclePubKey}) ->
    [OraclePubKey].

-spec process(response_tx(), trees(), height()) -> {ok, trees()}.
process(#oracle_response_tx{oracle = OraclePubKey, nonce = Nonce,
                            query_id = QId, response = Response,
                            fee = Fee}, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Query0 = aeo_state_tree:get_query(OraclePubKey, QId, OraclesTree0),
    Query1 = aeo_query:set_response(Response, Query0),
    OraclesTree1 = aeo_state_tree:enter_query(Query1, OraclesTree0),

    OracleAccount0 = aec_accounts_trees:get(OraclePubKey, AccountsTree0),
    {ok, OracleAccount1} = aec_accounts:spend(OracleAccount0, Fee, Nonce, Height),
    QueryFee = aeo_query:fee(Query0),
    {ok, OracleAccount2} = aec_accounts:earn(OracleAccount1, QueryFee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(OracleAccount2, AccountsTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_response_tx{oracle   = OraclePubKey,
                              nonce    = Nonce,
                              query_id = QId,
                              response = Response,
                              fee      = Fee}) ->
    [#{<<"type">>     => type()},
     #{<<"vsn">>      => version()},
     #{<<"oracle">>   => OraclePubKey},
     #{<<"nonce">>    => Nonce},
     #{<<"query_id">> => QId},
     #{<<"response">> => Response},
     #{<<"fee">>      => Fee}].

deserialize([#{<<"type">>     := ?ORACLE_RESPONSE_TX_TYPE},
             #{<<"vsn">>      := ?ORACLE_RESPONSE_TX_VSN},
             #{<<"oracle">>   := OraclePubKey},
             #{<<"nonce">>    := Nonce},
             #{<<"query_id">> := QId},
             #{<<"response">> := Response},
             #{<<"fee">>      := Fee}]) ->
    #oracle_response_tx{oracle   = OraclePubKey,
                        nonce    = Nonce,
                        query_id = QId,
                        response = Response,
                        fee      = Fee}.

-spec type() -> binary().
type() ->
    ?ORACLE_RESPONSE_TX_TYPE.

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_RESPONSE_TX_VSN.

for_client(#oracle_response_tx{ oracle   = OraclePubKey,
                                nonce    = Nonce,
                                query_id = QId,
                                response = Response,
                                fee      = Fee}) ->
    #{<<"type">> => <<"OracleResponseTxObject">>, % swagger schema name
      <<"vsn">> => version(),
      <<"oracle">> => aec_base58c:encode(oracle_pubkey, OraclePubKey),
      <<"nonce">> => Nonce,
      <<"query_id">> => aec_base58c:encode(oracle_query_id, QId),
      <<"response">> => Response,
      <<"fee">> => Fee}.

%% -- Local functions  -------------------------------------------------------

fetch_query(OId, QId, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    aeo_state_tree:lookup_query(OId, QId, OraclesTree).

check_query(I, OraclePubKey) ->
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
