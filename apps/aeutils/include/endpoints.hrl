%% This code is generated from swagger.yaml
%% do not manually change it.


operation(Id) ->
    maps:get(Id, operations()).

operations() ->
    #{ 
        'GetAccountBalance' => #{
            path => <<"/v2/account/balance">>,
            method => get
        },
        'GetAccountsBalances' => #{
            path => <<"/v2/balances">>,
            method => get
        },
        'GetBlockByHash' => #{
            path => <<"/v2/block-by-hash">>,
            method => get
        },
        'GetBlockByHeight' => #{
            path => <<"/v2/block-by-height">>,
            method => get
        },
        'GetInfo' => #{
            path => <<"/v2/info">>,
            method => get
        },
        'GetTop' => #{
            path => <<"/v2/top">>,
            method => get
        },
        'GetTxs' => #{
            path => <<"/v2/transactions">>,
            method => get
        },
        'GetVersion' => #{
            path => <<"/v2/version">>,
            method => get
        },
        'Ping' => #{
            path => <<"/v2/ping">>,
            method => post
        },
        'PostBlock' => #{
            path => <<"/v2/block">>,
            method => post
        },
        'PostTx' => #{
            path => <<"/v2/tx">>,
            method => post
        },
        'GetActiveRegisteredOracles' => #{
            path => <<"/v2/oracles">>,
            method => get
        },
        'GetBlockByHashInternal' => #{
            path => <<"/v2/block/hash">>,
            method => get
        },
        'GetBlockByHeightInternal' => #{
            path => <<"/v2/block/height">>,
            method => get
        },
        'GetBlockGenesis' => #{
            path => <<"/v2/block/genesis">>,
            method => get
        },
        'GetBlockLatest' => #{
            path => <<"/v2/block/latest">>,
            method => get
        },
        'GetBlockNumber' => #{
            path => <<"/v2/block/number">>,
            method => get
        },
        'GetBlockPending' => #{
            path => <<"/v2/block/pending">>,
            method => get
        },
        'GetBlockTxsCountByHash' => #{
            path => <<"/v2/block/txs/count/hash">>,
            method => get
        },
        'GetBlockTxsCountByHeight' => #{
            path => <<"/v2/block/txs/count/height">>,
            method => get
        },
        'GetGenesisBlockTxsCount' => #{
            path => <<"/v2/block/txs/count/genesis">>,
            method => get
        },
        'GetLatestBlockTxsCount' => #{
            path => <<"/v2/block/txs/count/latest">>,
            method => get
        },
        'GetOracleQuestions' => #{
            path => <<"/v2/oracle-questions">>,
            method => get
        },
        'GetPendingBlockTxsCount' => #{
            path => <<"/v2/block/txs/count/pending">>,
            method => get
        },
        'GetPubKey' => #{
            path => <<"/v2/account/pub-key">>,
            method => get
        },
        'GetTransactionFromBlockHash' => #{
            path => <<"/v2/block/tx/hash">>,
            method => get
        },
        'GetTransactionFromBlockHeight' => #{
            path => <<"/v2/block/tx/height">>,
            method => get
        },
        'GetTransactionFromBlockLatest' => #{
            path => <<"/v2/block/tx/latest">>,
            method => get
        },
        'PostOracleQueryTx' => #{
            path => <<"/v2/oracle-query-tx">>,
            method => post
        },
        'PostOracleRegisterTx' => #{
            path => <<"/v2/oracle-register-tx">>,
            method => post
        },
        'PostOracleResponseTx' => #{
            path => <<"/v2/oracle-response-tx">>,
            method => post
        },
        'PostSpendTx' => #{
            path => <<"/v2/spend-tx">>,
            method => post
        }
    }.
