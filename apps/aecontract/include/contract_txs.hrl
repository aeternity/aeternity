
-record(contract_create_tx, {
          owner      :: aec_keys:pubkey(),
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
          caller     :: aec_keys:pubkey(),
          nonce      :: integer(),
          contract   :: aec_keys:pubkey(),
          vm_version :: aect_contracts:vm_version(),
          fee        :: integer(),
          amount     :: aect_contracts:amount(),
          gas        :: aect_contracts:amount(),
          gas_price  :: aect_contracts:amount(),
          call_data  :: binary(),
          call_stack = [] :: [non_neg_integer()]
            %% addresses (the pubkey as an integer) of contracts on the call
            %% stack
          }).

