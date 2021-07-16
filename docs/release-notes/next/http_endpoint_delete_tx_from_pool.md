* Adds new a group of HTTP endpoints: `node-operator`. It is to hold endpoints
  that are meant to provide functionality for thinkering of the node and
  diving into its internals. Those should be only `internal` and the group is
  disabled by default. The new group will be present only in the new `OAS3`
  API and will not be present in the old `swagger2` API.
* Adds a new HTTP endpoint: `DELETE /node/operator/mempool/hash/{hash}`. It
  deletes a transaction currently present in the mempool. It is a part of the
  internal `node-operator` API.
* Fixes a bug in the transaction pool: transactions with a TTL of 0 were not
  subject for GC of the transaction pool.
* Fixes a bug in transaction pool: GC was triggered on syncing incoming
  blocks. It was not triggered when the node mined the transaction. This is
  being refactored to be using internal events system and now GC is being
  triggered when on a top change.

