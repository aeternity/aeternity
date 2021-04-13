# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v5.11.0) is a maintenance Lima release.

It:

* Fixes an issue where some nodes are stuck and could not sync beyond a
  certain point.

* Exports a function (`aec_chain_state:grant_fees/5`) in order to be used in
  the MDW while doing dry runs.

* Improves the transaction pool to not allow in `paying_for` transactions that
  are incorrectly authenticated.

* Improves node performance: use a dirty read when possible while checking a
  signed transaction

* Introduces some refactoring that is a prerequisite in order to accomodate
  contract clone primitives.


Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
 

