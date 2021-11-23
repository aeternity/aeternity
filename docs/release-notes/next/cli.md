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
  * A getter and a setter for the minumum gas required from this node in
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

