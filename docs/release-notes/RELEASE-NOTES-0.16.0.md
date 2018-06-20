# About this release

[This][this-release] is a maintenance release.
It:
* Improves TX-pool synchronization. It is now synced (with only missing transactions being
  pulled in) only once, at startup, after this it relies on the gossip protocol.
* Makes the transaction TTL optional (giving no TTL means the transaction is valid "forever").
* Improves memory footprint by removing an in-memory index. This affects the persisted DB.
* Removes the `/account/{account_pubkey}/txs` endpoint since it is non-essential for the node's operation, is hard to maintain, and consumes unnecessary resources.
* Reduces memory usage in case of node starting with large number of mempool transactions in persisted DB. This is achieved by deletion of temporary table after usage in mempool initialization.
* Reduces the db size by removing redundant identifiers stored in the state trees. This affects consensus and the persisted DB.
* Introduces type tags for identifiers in the serialization to make it possible to distinguish between different types of identifiers that can be used in the same position (e.g., names and account pubkeys). This affects consensus and the persisted DB.
* Makes the system more resistant against mistakes by checking sizes of identifiers as a side effect of introducing the typed identifiers.
* Creates contract call object in calls state tree even if contract create transaction init fails. This impacts consensus.
* Makes the owner of the contract create transaction lose the gas - in addition to the fee - if the init fails. This impacts consensus.
* Ensures that the create contract function calls the init function for Sophia ABI contracts. This impacts consensus.
* Rewards miner with gas used for execution of contracts, i.e. the execution of the initial call in any contract create transaction and the execution of any contract call transaction. This impacts consensus.
* Enhances mempool to consider reward miner might get by processing contract-related transactions. This impacts the persisted DB.
* Adds garbage collection of transactions, invalid transactions (wrong nonce, insufficient balance, expired TTL, etc.) are
  removed from the mempool/tx-pool periodically.
* Adds support for maps to Sophia.
* Enables retrieving the contract call object produced by the execution of the initialization call in a contract create transaction.
* Adds HTTP endpoint for contract proof of inclusion.
* Enables decoding of Sophia data into a json structure.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.16.0

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.15.x".

Please join the testnet by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/epoch/wiki/Troubleshooting).

The instructions below describe:
* [How to retrieve the released software for running a node](#retrieve-the-software-for-running-a-node);
* [How to install a node](#install-node);
* [How to join the testnet](#join-the-testnet).

## Retrieve the software for running a node

You can run a node by either:
* Installing the published [release binary][this-release] corresponding to your platform; or
* Running the published [Docker image `aeternity/epoch`][docker]; or
* [Building a release binary from source][build].

[docker]: https://github.com/aeternity/epoch/blob/v0.16.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.16.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.16.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.16.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.16.0/epoch/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet.

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://52.10.46.160:3013/v2/top
* http://18.195.109.60:3013/v2/top
* http://13.250.162.250:3013/v2/top
* http://31.13.249.70:3013/v2/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
