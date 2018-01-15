-include_lib("apps/aecore/include/common.hrl").

-record(contract_create_tx, {
          owner :: pubkey(),
          nonce :: integer(),
          fee   :: integer()
        }).

-record(contract_call_tx, {
          sender   :: pubkey(),
          nonce    :: integer(),
          contract :: pubkey(),
          fee      :: integer()
          }).

