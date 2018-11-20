* Introduces a dry-run API where a list of SpendTx, ContractCreateTx and ContractCallTx can be sent for off-chain evaluation. At
  the same time disable the broken off-chain (<<"sophia">> ABI) call through `debug/contracts/code/call`.
