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

* Transaction lifecycle is: the transaction is being prepared and posted to a
  node, then if it is valid it lands in the mempool, it is gossiped to all
  peers in the network, it is picked by a miner and then included in a block.
  If a transaction stays in the pool for too long time (2 weeks) and is not
  included in a block, there might be an issue with it (ex. `origin` doesn't
  have enough tokens to spend) and it is garbage collected. There used to be a
  check ensuring GCed transactions do not end up in the mempool ever again.
  This PR disables this check.

* An optional setting for transaction garbage collect TTL is available for the
  node operator to set. It is `mempool.tx_ttl` and its default value is 2
  weeks.

* The transaction pool is being protected by DDos attacks via a caching layer
  so only new transactions ever reach the pool. There is a new setting for the
  node operator to define the cache size in amount of transactions to keep. It
  is `mempool.cache_size` with a default value of 200.

* Introduced key-header index and fast implementation of
  `find_key_headers_and_hash_at_height` using it. This greatly improves
  performance of `BLOCKHASH` opcode for both AEVM and FATE.

* Fixes a bug when using HTTP API to fetch contract call data at an already
  GCed height.

Please join the **mainnet** by following the instructions in the documentation below,
and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
 

