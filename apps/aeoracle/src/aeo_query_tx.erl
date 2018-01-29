%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_query_tx).

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
         query/1,
         query_fee/1,
         query_ttl/1,
         response_ttl/1,
         sender/1]).


-define(ORACLE_QUERY_TX_TYPE, <<"oracle_query">>).
-define(ORACLE_QUERY_TX_VSN, 1).
-define(ORACLE_QUERY_TX_FEE, 2).

-opaque query_tx() :: #oracle_query_tx{}.

-export_type([query_tx/0]).

-spec sender(query_tx()) -> pubkey().
sender(#oracle_query_tx{sender = SenderPubKey}) ->
    SenderPubKey.

-spec oracle(query_tx()) -> pubkey().
oracle(#oracle_query_tx{oracle = OraclePubKey}) ->
    OraclePubKey.

-spec query(query_tx()) -> aeo_oracles:query().
query(#oracle_query_tx{query = Query}) ->
    Query.

-spec query_fee(query_tx()) -> integer().
query_fee(#oracle_query_tx{query_fee = QueryFee}) ->
    QueryFee.

-spec query_ttl(query_tx()) -> aeo_oracles:ttl().
query_ttl(#oracle_query_tx{query_ttl = QueryTTL}) ->
    QueryTTL.

-spec response_ttl(query_tx()) -> aeo_oracles:relative_ttl().
response_ttl(#oracle_query_tx{response_ttl = ResponseTTL}) ->
    ResponseTTL.

-spec new(map()) -> {ok, query_tx()}.
new(#{sender        := SenderPubKey,
      nonce         := Nonce,
      oracle        := Oracle,
      query         := Query,
      query_fee     := QueryFee,
      query_ttl     := QueryTTL,
      response_ttl  := ResponseTTL,
      fee           := Fee}) ->
    {ok, #oracle_query_tx{sender        = SenderPubKey,
                          nonce         = Nonce,
                          oracle        = Oracle,
                          query         = Query,
                          query_fee     = QueryFee,
                          query_ttl     = QueryTTL,
                          response_ttl  = ResponseTTL,
                          fee           = Fee}}.

-spec fee(query_tx()) -> integer().
fee(#oracle_query_tx{fee = F}) ->
    F.

-spec nonce(query_tx()) -> non_neg_integer().
nonce(#oracle_query_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(query_tx()) -> pubkey().
origin(#oracle_query_tx{sender = SenderPubKey}) ->
    SenderPubKey.

%% SenderAccount should exist, and have enough funds for the fee + the query_fee.
%% Oracle should exist, and query_fee should be enough
%% Fee should cover TTL
-spec check(query_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#oracle_query_tx{sender = SenderPubKey, nonce = Nonce,
                       oracle = OraclePubKey, query_fee = QFee,
                       query_ttl = TTL, response_ttl = RTTL, fee = Fee} = Q, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(SenderPubKey, Trees, Height, Nonce, Fee + QFee) end,
         fun() -> aeo_utils:check_ttl_fee(Height, TTL, Fee - ?ORACLE_QUERY_TX_FEE) end,
         fun() -> check_oracle(OraclePubKey, Trees, QFee, Height, TTL, RTTL) end,
         fun() -> check_query(Q, Trees, Height) end
        ],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(query_tx()) -> [pubkey()].
signers(#oracle_query_tx{sender = SenderPubKey}) ->
    [SenderPubKey].

-spec process(query_tx(), trees(), height()) -> {ok, trees()}.
process(#oracle_query_tx{sender = SenderPubKey, nonce = Nonce, fee = Fee,
                         query_fee = QueryFee} = QueryTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Sender0 = aec_accounts_trees:get(SenderPubKey, AccountsTree0),
    {ok, Sender1} = aec_accounts:spend(Sender0, QueryFee + Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Sender1, AccountsTree0),

    Query = aeo_query:new(QueryTx, Height),
    OraclesTree1 = aeo_state_tree:insert_query(Query, OraclesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_query_tx{sender        = SenderPubKey,
                           nonce         = Nonce,
                           oracle        = OraclePubKey,
                           query         = Query,
                           query_fee     = QueryFee,
                           query_ttl     = {QueryTTLType, QueryTTLValue},
                           response_ttl  = {delta, ResponseTTLValue},
                           fee           = Fee}) ->
    [#{<<"type">>         => type()},
     #{<<"vsn">>          => version()},
     #{<<"sender">>       => SenderPubKey},
     #{<<"nonce">>        => Nonce},
     #{<<"oracle">>       => OraclePubKey},
     #{<<"query">>        => Query},
     #{<<"query_fee">>    => QueryFee},
     #{<<"query_ttl">>    =>
           #{<<"type">>   => QueryTTLType,
             <<"value">>  => QueryTTLValue}},
     #{<<"response_ttl">> =>
           #{<<"type">>   => <<"delta">>,
             <<"value">>  => ResponseTTLValue}},
     #{<<"fee">>          => Fee}].

deserialize([#{<<"type">>         := ?ORACLE_QUERY_TX_TYPE},
             #{<<"vsn">>          := ?ORACLE_QUERY_TX_VSN},
             #{<<"sender">>       := SenderPubKey},
             #{<<"nonce">>        := Nonce},
             #{<<"oracle">>       := OraclePubKey},
             #{<<"query">>        := Query},
             #{<<"query_fee">>    := QueryFee},
             #{<<"query_ttl">>    := #{<<"type">>  := QueryTTLType,
                                       <<"value">> := QueryTTLValue}},
             #{<<"response_ttl">> := #{<<"type">>  := <<"delta">>,
                                       <<"value">> := ResponseTTLValue}},
             #{<<"fee">>          := Fee}]) ->
    #oracle_query_tx{sender        = SenderPubKey,
                     nonce         = Nonce,
                     oracle        = OraclePubKey,
                     query         = Query,
                     query_fee     = QueryFee,
                     query_ttl     = {binary_to_existing_atom(QueryTTLType, utf8),
                                      QueryTTLValue},
                     response_ttl  = {delta, ResponseTTLValue},
                     fee           = Fee}.

-spec type() -> binary().
type() ->
    ?ORACLE_QUERY_TX_TYPE.

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_QUERY_TX_VSN.

for_client(#oracle_query_tx{sender        = SenderPubKey,
                            nonce         = Nonce,
                            oracle        = OraclePubKey,
                            query         = Query,
                            query_fee     = QueryFee,
                            query_ttl     = {QueryTLLType, QueryTTLValue},
                            response_ttl  = {delta=ResponseTTLType, ResponseTTLValue},
                            fee           = Fee}) ->
    #{<<"type">> => <<"OracleQueryTxObject">>, % swagger schema name
      <<"vsn">> => version(),
      <<"sender">> => aec_base58c:encode(account_pubkey, SenderPubKey),
      <<"nonce">> => Nonce,
      <<"oracle">> => aec_base58c:encode(oracle_pubkey, OraclePubKey),
      <<"query">> => Query,
      <<"query_fee">> => QueryFee,
      <<"query_ttl">> => #{<<"type">> => QueryTLLType,
                           <<"value">> => QueryTTLValue},
      <<"response_ttl">> => #{<<"type">> => ResponseTTLType,
                              <<"value">> => ResponseTTLValue},
      <<"fee">> => Fee}.

%% -- Local functions  -------------------------------------------------------

check_query(Q, Trees, Height) ->
    Oracles  = aec_trees:oracles(Trees),
    I        = aeo_query:new(Q, Height),
    OracleId = aeo_query:oracle_address(I),
    Id       = aeo_query:id(I),
    case aeo_state_tree:lookup_query(OracleId, Id, Oracles) of
        none       -> ok;
        {value, _} -> {error, oracle_query_already_present}
    end.

check_oracle(OraclePubKey, Trees, QueryFee, Height, QTTL, RTTL) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(OraclePubKey, OraclesTree) of
        {value, Oracle} ->
            case QueryFee >= aeo_oracles:query_fee(Oracle) of
                true  -> check_oracle_ttl(Oracle, Height, QTTL, RTTL);
                false -> {error, query_fee_too_low}
            end;
        none -> {error, oracle_does_not_exist}
    end.

check_oracle_ttl(O, Height, QTTL, RTTL) ->
    try
        Delta  = aeo_utils:ttl_delta(Height, QTTL),
        MaxTTL = aeo_utils:ttl_expiry(Height + Delta, RTTL),
        case aeo_oracles:expires(O) < MaxTTL of
            false -> ok;
            true  -> {error, too_long_ttl}
        end
    catch _:_ ->
        {error, invalid_ttl}
    end.
