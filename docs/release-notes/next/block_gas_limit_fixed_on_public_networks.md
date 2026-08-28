* The `aecore` `block_gas_limit` setting is no longer honoured on `ae_mainnet` or `ae_uat`.
  The value decides both which micro blocks a node admits and what the FATE and AEVM
  `GASLIMIT` opcodes report to a contract, so on a network whose limit is the network's it
  is not a local knob: both read sites now take the network's 6,000,000 there and ignore the
  setting. On every other network id - including `ae_devnet` and hyperchains - the setting
  still decides both sites together, exactly as before, and an unconfigured node on any
  network behaves as it always has.
* A node configured with `block_gas_limit` on `ae_mainnet` or `ae_uat` now refuses to start,
  reporting `block_gas_limit_override_would_fork` at boot, rather than starting up and
  quietly disagreeing with its own configuration. Remove the setting to start such a node.
