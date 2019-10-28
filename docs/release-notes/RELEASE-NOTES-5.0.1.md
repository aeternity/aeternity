# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.0.1) is a maintenance Lima release:

* Fixes a bug in FATE VM where `bytes(N)` as the return type of a remote call would
  cause the VM to crash.
* Fixes a bug in FATE where the stack could be modified by a remote contract.
* Fixes a bug in FATE where hashing a map could cause the VM to crash.
* Add assertion that checks that we are not using a Generalized account like a basic
  account, i.e. don't allow to use private key signature.

The node is able to start from a database produced by `v4.*` releases, otherwise this release is not backward compatible.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
