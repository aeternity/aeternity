# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.1.1) is a maintenance Iris release.

It:

* Provides a new HTTP endpoint `/accounts/{pubkey}/next-nonce` for fetching
  the next account nonce. There are two strategies to be taken: provide a
  missing nonce if there is a gap in the transactions in the mempool or fetch
  an incremented highest nonce in the transaction pool.
* Adds new a group of HTTP endpoints: `node-operator`. It is to hold endpoints
  that are meant to provide functionality for tinkering with the node and
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
* Updates the `aeternity/swagger_endpoints` dep. This allows the node to have
  RESTfull APIs.
* Exposes a functionality to calculate actual consumed gas by a transaction.
* Introduces a new endpoint for checking a transaction currently residing in
  the mempool: if it can be included by miners or if it is blocked by something:
  not enough tokens, skipped nonce or something else. The endpoint is
  `/debug/check-tx/pool/{transaction-hash}`. It is a debug endpoint and should
  not be used in production.
 


Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
 

