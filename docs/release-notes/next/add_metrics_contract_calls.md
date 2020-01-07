* Added new metrics covering contract call information
  The following names act as templates for multiple specific metrics.
  The placeholders in these names are (in order) for ABI version, VM version,
  return type and actual info type.
  The return type may be `ok`, `return` or `revert`.
  The info type may be `gas_used`, `execution_time` in microseconds,
  `state_size` or `call_data_size`, both in bytes.
** `ae.epoch.aecore.contracts._._.ga_meta._._`
** `ae.epoch.aecore.contracts._._.ga_attach._._`
** `ae.epoch.aecore.contracts._._.contract_call._._`
** `ae.epoch.aecore.contracts._._.contract_create._._`
