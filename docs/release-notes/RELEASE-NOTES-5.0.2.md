# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.0.2) is a maintenance Lima release:

* Cache calls to `Contract.creator` in FATE engine state.
* Disable unused FATE operation `INT_TO_ADDR`.
* Improves the checks and tests for close solo and force progress transactions.

The node is able to start from a database produced by `v4.*` releases, otherwise this release is not backwards compatible.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
