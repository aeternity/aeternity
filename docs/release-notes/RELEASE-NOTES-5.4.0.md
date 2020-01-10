# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.4.0) is a maintenance Lima release.

* Refactor fetching of (forward) generations. This avoids a slow check for
  chain inclusion and will speedup `v2/generations/height/<height>` and
  `v2/generations/hash/<kh_...>`
* Added new metrics covering extended block information
    * `ae.epoch.aemon.block.tx.total.micro` : Number of transactions in a microblock
    * `ae.epoch.aemon.block.gas.total.micro` : Gas used per microblock
    * `ae.epoch.aemon.block.gas.per_tx.micro` : Gas used per transaction in a microblock
    * `ae.epoch.aemon.block.size.per_tx.micro` : Size of transactions in a microblock in bytes
    * `ae.epoch.aecore.blocks.micro.txs_execution_time.success` : Execution time of all transaction in a microblock in microseconds
    * `ae.epoch.aecore.blocks.micro.txs_execution_time.error` : Execution time of all transaction in a microblock in microseconds, when an error was encountered
    * `ae.epoch.aecore.blocks.micro.insert_execution_time.success` : Execution time of insertion of a microblock in microseconds
    * `ae.epoch.aecore.blocks.micro.insert_execution_time.error` : Execution time of failed insertion of a microblock in microseconds
    * `ae.epoch.aecore.blocks.key.insert_execution_time.success` : Execution time of insertion of a keyblock in microseconds
    * `ae.epoch.aecore.blocks.key.insert_execution_time.error` : Execution time of failed insertion of a keyblock in microseconds
* Added new metrics covering contract call information
  The following names act as templates for multiple specific metrics.
  The placeholders in these names are (in order) for ABI version, VM version,
  return type and actual info type.
  The return type may be `ok`, `return` or `revert`.
  The info type may be `gas_used`, `execution_time` in microseconds,
  `state_size` or `call_data_size`, both in bytes.
    * `ae.epoch.aecore.contracts._._.ga_meta._._`
    * `ae.epoch.aecore.contracts._._.ga_attach._._`
    * `ae.epoch.aecore.contracts._._.contract_call._._`
    * `ae.epoch.aecore.contracts._._.contract_create._._`
* Improve the algorithm in `agree_on_height`. It now starts looking near the top, and
  it keeps the right top hash during the whole algorithm.
* The default configuration for the Erlang runtime system has been adapted to
  use less CPU cores and threads. This should improve CPU contention on
  systems with fewer cores and improve responsiveness.
  For systems with 4+ CPU cores the settings can be increased if the node 
  is experiencing any form of CPU limitations.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
