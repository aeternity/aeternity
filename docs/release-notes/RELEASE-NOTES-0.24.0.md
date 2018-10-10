# About this release

[This][this-release] is a maintenance release.
It:
* Upgrade IDNA library to introduce character and label validation according to IDNA 2008.
* Seed node with address 31.13.249.70 in region eu-east has been replaced with seed node with address 18.130.148.7 in region eu-west-2
* Changes /blocks/top endpoint, the returned value is more explicit about the block type.
* Makes the retries of the peer host name resolution configurable by the user. See configuration parameters `sync > resolver_max_retries` and `sync > resolver_backoff_times`.
* mempool refactored/optimized for better throughput and latency
* load regulation queues made configurable via epoch.[json|yaml]
* 'from_id' field added to close-mutual transaction record
  (changes HTTP API and serialization)
* Renames transaction's type in user HTTP API to match swagger definition name (e.g. `spend_tx` is `SpendTx` now).
* Allows state channel off-chain contracts to use on-chain data: balances,
  names and oracle responses.
* Changes the API from using hex encoded strings as contract call data, byte code, and return values to using base58c encoded byte arrays with the prefix `cb_`
* Adds Cross-Origin Resource Sharing (CORS) support
* Relased enoise version 1.0.0 - available at hex.pm and used it in node.
* Adds computation of gas of all transactions based on their size. Contract transactions also have gas for their execution.
* Adds efficient implementation of Sophia maps. This affects consensus.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.24.0

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.23.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v0.24.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.24.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.24.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.24.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.24.0/epoch/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet.

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://52.10.46.160:3013/v2/blocks/top
* http://18.195.109.60:3013/v2/blocks/top
* http://13.250.162.250:3013/v2/blocks/top
* http://18.130.148.7:3013/v2/blocks/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
