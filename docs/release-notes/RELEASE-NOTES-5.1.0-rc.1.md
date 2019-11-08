# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.1.0-rc.1) is a maintenance Lima release candidate
focused on stability:

* Adds support for community hard-forks. Miners will be able to signal if they
  support the new protocol version. Depending on the signalling result, the
  network will switch to a new protocol version or will stay with the current
  one.
* Adds protocol activated by miner signalling to HTTP /status endpoint
* Improving chain sync - avoid creating duplicate sync tasks.
* Optimize sync, by being more aggressive and drop gossiped blocks that are far
  in the future. This also reduces the amount of noise in the console.
* State Channels off-chain update requests can now all include meta information objects.
  This affects methods update, new\_contract, contract\_call, deposit and withdraw.
* State Channel FSM: allows optionally setting the fee of all on-chain
  transactions.
* State Channels: Changed calculation of block confirmation times in to be dynamic.
  Previously the confirmation times would be static for all relevant transactions
  in a channel. This has been changed to calculate the block confirmation time
  based on the chosen strategy. For more information check the protocol
  documentation.
* Adds a functionality to reject updates in State Channel environment.
* Improves stability of `channel_force_progress_tx` series before a
  solo closing sequence.
* Improves stability of `channel_slash_tx` after a `channel_close_solo_tx` with
  a payload that had been preceded by a `channel_force_progress_tx`
* Load regulation for state channel set up was corrected so that requests release the job scheduler sooner,
  thereby allowing for more channels than `regulators:sc_ws_handler:counter`. A config value,
  `channels:max_count` (default: 1000) is introduced, to limit the total number of active channels on a node.
* Fixes rare crash in FATE VM when updating nested maps in the store.
* Fixes internal API bug which manifests itself during sync on low io throughput devices

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
