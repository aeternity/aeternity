# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.7.0) is a maintenance Iris and HyperChains release.

It:

* Improvement of the accounts tree garbage collection: A small node restart is no longer required at the end of each GC sweep.

* A new runtime mode has been added: 'offline', which disables mining and sync, but leaves HTTP endpoints active. Controlled via the command `bin/aeternity offline [on | off | status]` (GH #4036)

* The method `debug/channels/fsm-count` has been added as a debug HTTP endpoint (GH #4029)

* It is now possible to use a custom file name for pre-funded accounts for testing, e.g. when using dev mode. (GH #4025)

* Corrected nonce handling in tx events generated from oracle queries (GH #4027)

* Internal fault escalation for the core modules was improperly configured, which could lead to protracted error conditions. (GH #4023)

* When loading plugins during development, the node now automatically loads dependent applications (provided the plugin is built using rebar3) (GH #4020)

* In order to sync testnet with OTP-24 (and later) we need a pre-Iris mapping.

With regards of HyperChains it:

* Exposes configurable parent chain cache size to the config

* Changes the encoding of private keys in the config. Till now keys were
  encoded as contract byte array (cb_...) but from now on they would be hex
  instead. This is for better integration with wallets.

* Enforces staker key pairs to be valid at start time: if any of the key pairs
  is invalid, the node will not start.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).

