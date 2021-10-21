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
