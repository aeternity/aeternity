-include_lib("apps/aecore/include/common.hrl").

-record(oracle_register_tx, {
          account                                     :: pubkey(),
          nonce                                       :: integer(),
          query_spec    = <<"string()">>              :: aeo_oracles:type_spec(),
          response_spec = <<"boolean() | integer()">> :: aeo_oracles:type_spec(),
          query_fee                                   :: integer(),
          ttl                                         :: aeo_oracles:ttl(),
          fee                                         :: integer()
          }).

-record(oracle_extend_tx, {
          oracle :: pubkey(),
          nonce  :: integer(),
          ttl    :: aeo_oracles:relative_ttl(),
          fee    :: integer()
          }).

-record(oracle_query_tx, {
          sender       :: pubkey(),
          nonce        :: integer(),
          oracle       :: pubkey(),
          query        :: aeo_oracles:query(),
          query_fee    :: integer(),
          query_ttl    :: aeo_oracles:ttl(),
          response_ttl :: aeo_oracles:relative_ttl(),
          fee          :: integer()
          }).

-record(oracle_response_tx, {
          oracle   :: pubkey(),
          nonce    :: integer(),
          query_id :: aeo_query:id(),
          response :: aeo_oracles:response(),
          fee      :: integer()
          }).
