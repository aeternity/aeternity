* Allow contracts to be called by name. The key `contract_pubkey` of the name
  is resolved while running the transaction. This has consequences for the
  contract call structure (`aect_call`) that gets a new field with the contract
  call identifier. In order to be able to find the call result without doing
  name resolution calls are stored relative to the id rather than the contract
  pubkey.

  NOTE: Names can change at any time (frontrunning is possible!) so only call
  contracts by name when you trust the owner of the name!
