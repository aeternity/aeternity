
-record(contract_create_tx, {
          owner      :: aect_contracts:pubkey(),
          nonce      :: non_neg_integer(),
          code       :: binary(),
          vm_version :: aect_contracts:vm_version(),
          fee        :: aect_contracts:amount(),
          deposit    :: aect_contracts:amount(),
          amount     :: aect_contracts:amount(),
          gas        :: aect_contracts:amount(),
          gas_price  :: aect_contracts:amount(),
          call_data  :: binary()
        }).

-record(contract_call_tx, {
          caller     :: aect_contracts:pubkey(),
          nonce      :: integer(),
          contract   :: aect_contracts:pubkey(),
          vm_version :: aect_contracts:vm_version(),
          fee        :: integer(),
          amount     :: aect_contracts:amount(),
          gas        :: aect_contracts:amount(),
          gas_price  :: aect_contracts:amount(),
          call_data  :: binary()
          }).

