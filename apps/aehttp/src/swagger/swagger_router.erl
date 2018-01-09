-module(swagger_router).

-export([get_paths/1]).

-type operations() :: #{
    Method :: binary() => swagger_api:operation_id()
}.

-type init_opts()  :: {
    Operations :: operations(),
    LogicHandler :: atom(),
    ValidatorState :: jesse_state:state()
}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: atom()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    ValidatorState = prepare_validator(),
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, ValidatorState}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

get_operations() ->
    #{ 
        'GetAccountBalance' => #{
            path => "/v2/account/balance",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetAccountsBalances' => #{
            path => "/v2/balances",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetBlockByHash' => #{
            path => "/v2/block-by-hash",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetBlockByHeight' => #{
            path => "/v2/block-by-height",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetInfo' => #{
            path => "/v2/info",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetName' => #{
            path => "/v2/name",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetTop' => #{
            path => "/v2/top",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetTxs' => #{
            path => "/v2/transactions",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetVersion' => #{
            path => "/v2/version",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'Ping' => #{
            path => "/v2/ping",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'PostBlock' => #{
            path => "/v2/block",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'PostTx' => #{
            path => "/v2/tx",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'GetActiveRegisteredOracles' => #{
            path => "/v2/oracles",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockByHashInternal' => #{
            path => "/v2/block/hash/:hash",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockByHeightInternal' => #{
            path => "/v2/block/height/:height",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockGenesis' => #{
            path => "/v2/block/genesis",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockLatest' => #{
            path => "/v2/block/latest",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockNumber' => #{
            path => "/v2/block/number",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockPending' => #{
            path => "/v2/block/pending",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockTxsCountByHash' => #{
            path => "/v2/block/txs/count/hash/:hash",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockTxsCountByHeight' => #{
            path => "/v2/block/txs/count/height/:height",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetGenesisBlockTxsCount' => #{
            path => "/v2/block/txs/count/genesis",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetLatestBlockTxsCount' => #{
            path => "/v2/block/txs/count/latest",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetOracleQuestions' => #{
            path => "/v2/oracle-questions",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetPendingBlockTxsCount' => #{
            path => "/v2/block/txs/count/pending",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetPubKey' => #{
            path => "/v2/account/pub-key",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetTransactionFromBlockHash' => #{
            path => "/v2/block/tx/hash/:hash/:tx_index",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetTransactionFromBlockHeight' => #{
            path => "/v2/block/tx/height/:height/:tx_index",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetTransactionFromBlockLatest' => #{
            path => "/v2/block/tx/latest/:tx_index",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetTxsListFromBlockRangeByHash' => #{
            path => "/v2/block/txs/list/hash",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetTxsListFromBlockRangeByHeight' => #{
            path => "/v2/block/txs/list/height",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'PostNameClaimTx' => #{
            path => "/v2/name-claim-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostNamePreclaimTx' => #{
            path => "/v2/name-preclaim-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostNameRevokeTx' => #{
            path => "/v2/name-revoke-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostNameTransferTx' => #{
            path => "/v2/name-transfer-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostNameUpdateTx' => #{
            path => "/v2/name-update-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleQueryTx' => #{
            path => "/v2/oracle-query-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleRegisterTx' => #{
            path => "/v2/oracle-register-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleResponseTx' => #{
            path => "/v2/oracle-response-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostSpendTx' => #{
            path => "/v2/spend-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        }
    }.

prepare_validator() ->
    R = jsx:decode(element(2, file:read_file(get_swagger_path()))),
    jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]).


get_swagger_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(swagger_utils:priv_dir(AppName), "swagger.json").


