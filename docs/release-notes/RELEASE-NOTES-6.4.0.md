# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.4.0) is a maintenance Iris release.

It:

* Introduces a new `cli` admin interface for the node operator to have better
  control of their node. It is to be used by
  ```
  ./bin/aeternity admin <command>
  ```
  It allows interacting with the transaction pool and network peers.
  It currently supports the following pool functionalities:
  * Push of a new transaction to the pool. It has a force option in order to
    skip the checks if the transaction had already been deleted in the past
  * Delete a transaction from the pool.
  * Check the current size of the transaction pool. It comes with options for
    showing only transactions that were visited/not visited during this
    generation.
  * Inspect a transaction currently present in the transaction pool. It
    provides number of failures and TTL.
  * A getter and a setter for the minimum gas required from this node in
    particular. This allows scripting node behaviour with varying gas price
    expectations. 
  It also supports the following peers functionalities:
  * Inspect peers - currently `connected`, `verified`, `unverified` and
    `blocked`.  They are returned in a list but there is also an optional
    argument to return their count instead.
  * Add a new peer - add a new peer to the unverified list. This new peer
    will follow the same verification process as if received by other peer.
    There is also the option of adding a trusted peer - those are a special
    set of peers that are always kept in the `verified` bucket and are never
    degraded to `unverified` even if they are offline.
  * Remove a peer by their peer id.
  * Block and unblock peers - `blocked` peers are considered malicious entities
    and the node will not attempt connecting to them. It will also refuse
    incoming connections from `blocked` peers.

* Improve dev-mode to allow running it on protocols beyond `iris`. `iris` is
  still the default one but if the user wants to, they can define different
  hard fork heights.

* Fixes a crash in HTTP endpoint `/<version prefix>/names/<name>` when the
  name was invalid according to IDNA rules.
  
* Fixes a bug: when an HTTP endpoint crashes, now appropriate CORS headers are
  provided as well.

* Improves rollback script: allows rollbacks when GC is enabled. Rollback
  beyond GCed heights is still impossible.
 

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
 

