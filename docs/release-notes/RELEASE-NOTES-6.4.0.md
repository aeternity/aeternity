# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.4.0) is a maintenance Iris release.

It:

* Introduces a command-line script to inspect and toggle maintenance mode:
  ```
  bin/aeternity maintenance on | off | status
  ```

* Introduces a new `cli` admin interface for the node operator to have better
  control of their node. It is to be used by
  ```
  ./bin/aeternity admin <command>
  ```
  The initial commit allows interacting with the transaction pool.
  It currently supports:
    * Push of a new transaction to the pool. It has a force option in order to
      skip the checks if the transaction had already been deleted in the past
    * Delete a transaction from the pool.
    * Check the current size of the transaction pool. It comes with options for
      showing only transactions that were visited/not visited during this
      generation.
    * Inspect a transaction currently present in the transaction pool. It
      provides number of failures and TTL.
    * A getter and a setter for the minumum gas required from this node in
      particular. This allows scripting node behaviour with varying gas price
      expectations.

* Bump devmode plugin vsn to 0.3.1
* Introduce Ceres
* Various database rollback fixes

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
 
