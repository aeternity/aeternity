# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.3.0) is a maintenance Iris release.

It:

* Introduces a new `strict` mode in the tx-pool. It allows the node operator
  to specify different maximum attempts per error type of a transaction, thus
  allowing a higher grace period for some transactions to become valid while
  limiting other errors. All error types can be specified in the config but
  some come with sensible defaults. This feature can also be turned off in the
  config.

* Added basic support for plugin apps loaded at node startup
  
* Added a configuration parameter, `system:dev_mode` (boolean), enabling switching
  the node into development mode, with sensible defaults and on-demand consensus
  See https://github.com/aeternity/aeplugin_dev_mode

* Improved checking and defaults handling of mininum_depth parameters in State Channels.
  Also, `"minimum_depth_strategy": "plain"` is supported besides the default, adaptive,
  `txfee` strategy. With `plain`, the confirmation time will always be the number of blocks
  specified in `minimum_depth`.

* Adds node extension scripts for producing chain whitelists, and for rolling back the
  chain to a specific height, or to a height matching a provided whitelist. This functionality
  is meant to simplify testing, but can also be used to help get back to a preferred fork, e.g.
  after a netsplit.

* Introduces support for 'maintenance mode', where sync, mining, and http endpoints
  are disabled. This is primarily intended for maintenance tasks and debugging.

* Fixes a bug when fetching a transaction via the HTTP API and the flag
  `int-as-string` is raised. This used to fail the specification and the
  request was crashing.

* Introduces a command-line script to inspect and toggle maintenance mode:
  `bin/aeternity maintenance on | off | status`

* Specifies a higher OTP version: so far the minimum one was OTP 21.3. Now the
  minimum required would be 22.3.4.9. OTP version 23 is officially supported.

* Previously, if a transaction had been discarded from the tx-pool, it could re-enter the tx-pool. This
  PR provides the functionality for the node operator to define their own
  strategy if the node shall allow reentry of already discarded transactions.
  This is configurable in the config as `mempool/allow_reentry_of_txs` and the
  default is `false`.

* Improve micro block generation logic - try harder to fill the generated micro block if there are available transactions.


Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
 

