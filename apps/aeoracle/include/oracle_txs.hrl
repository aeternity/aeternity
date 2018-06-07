-define(ttl_delta_int, 0).
-define(ttl_delta_atom, delta).
-define(ttl_block_int, 1).
-define(ttl_block_atom, block).

-record(oracle_register_tx, {
          account                                     :: aec_keys:pubkey(),
          nonce                                       :: integer(),
          query_spec    = <<"string()">>              :: aeo_oracles:type_spec(),
          response_spec = <<"boolean() | integer()">> :: aeo_oracles:type_spec(),
          query_fee                                   :: integer(),
          oracle_ttl                                  :: aeo_oracles:ttl(),
          fee                                         :: integer(),
          ttl                                         :: aetx:tx_ttl()
          }).

-record(oracle_extend_tx, {
          oracle     :: aec_keys:pubkey(),
          nonce      :: integer(),
          oracle_ttl :: aeo_oracles:relative_ttl(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
          }).

-record(oracle_query_tx, {
          sender       :: aec_keys:pubkey(),
          nonce        :: integer(),
          oracle       :: aec_keys:pubkey(),
          query        :: aeo_oracles:query(),
          query_fee    :: integer(),
          query_ttl    :: aeo_oracles:ttl(),
          response_ttl :: aeo_oracles:relative_ttl(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
          }).

-record(oracle_response_tx, {
          oracle   :: aec_keys:pubkey(),
          nonce    :: integer(),
          query_id :: aeo_query:id(),
          response :: aeo_oracles:response(),
          fee      :: integer(),
          ttl      :: aetx:tx_ttl()
          }).
