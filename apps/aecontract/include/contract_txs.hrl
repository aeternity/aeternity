-include_lib("apps/aecore/include/common.hrl").

-type amount() :: non_neg_integer().

-record(contract_create_tx, {
          owner      :: pubkey(),
          nonce      :: non_neg_integer(),
          code       :: binary(),
          vm_version :: byte(),
          fee        :: amount(),
          deposit    :: amount(),
          amount     :: amount(),
          gas        :: amount(),
          gas_price  :: amount(),
          call_data  :: binary()
        }).

-record(contract_call_tx, {
          sender   :: pubkey(),
          nonce    :: integer(),
          contract :: pubkey(),
          fee      :: integer()
          }).

