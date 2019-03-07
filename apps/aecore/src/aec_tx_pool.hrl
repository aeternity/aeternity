%% Shared between aec_tx_pool and aec_tx_pool_gc.
%% Really an internal format that shouldn't be included by anyone else
-record(tx, { hash :: aec_tx_pool:tx_hash()     | atom()
            , ttl  :: aetx:tx_ttl()             | atom()
            , key  :: aec_tx_pool:pool_db_key() | atom()}).
