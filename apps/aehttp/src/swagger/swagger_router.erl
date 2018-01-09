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
            path => "/v1/account/balance",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetAccountsBalances' => #{
            path => "/v1/balances",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetBlockByHash' => #{
            path => "/v1/block-by-hash",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetBlockByHeight' => #{
            path => "/v1/block-by-height",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetInfo' => #{
            path => "/v1/info",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetTop' => #{
            path => "/v1/top",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetTxs' => #{
            path => "/v1/transactions",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'GetVersion' => #{
            path => "/v1/version",
            method => <<"GET">>,
            handler => 'swagger_external_handler'
        },
        'Ping' => #{
            path => "/v1/ping",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'PostBlock' => #{
            path => "/v1/block",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'PostTx' => #{
            path => "/v1/tx",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'GetActiveRegisteredOracles' => #{
            path => "/v1/oracles",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockByHashInternal' => #{
            path => "/v1/block/hash/:hash",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockByHeightInternal' => #{
            path => "/v1/block/height/:height",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockGenesis' => #{
            path => "/v1/block/genesis",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockLatest' => #{
            path => "/v1/block/latest",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockNumber' => #{
            path => "/v1/block/number",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockPending' => #{
            path => "/v1/block/pending",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockTxsCountByHash' => #{
            path => "/v1/block/txs/count/hash/:hash",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetBlockTxsCountByHeight' => #{
            path => "/v1/block/txs/count/height/:height",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetGenesisBlockTxsCount' => #{
            path => "/v1/block/txs/count/genesis",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetLatestBlockTxsCount' => #{
            path => "/v1/block/txs/count/latest",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetOracleQuestions' => #{
            path => "/v1/oracle-questions",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetPendingBlockTxsCount' => #{
            path => "/v1/block/txs/count/pending",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetPubKey' => #{
            path => "/v1/account/pub-key",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleQueryTx' => #{
            path => "/v1/oracle-query-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleRegisterTx' => #{
            path => "/v1/oracle-register-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleResponseTx' => #{
            path => "/v1/oracle-response-tx",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleSubscribe' => #{
            path => "/v1/oracle-subscribe",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostOracleUnsubscribe' => #{
            path => "/v1/oracle-unsubscribe",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PostSpendTx' => #{
            path => "/v1/spend-tx",
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


