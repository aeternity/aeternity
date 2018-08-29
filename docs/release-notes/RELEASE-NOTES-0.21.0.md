# About this release

[This release][this-release] is focused on TODOFILLMEIN.
It:
* Adds support for type aliases and typed contract calls to the Sophia compiler.
* Changes the target (difficulty) calculation algorithm to use [DigiShield v3][digishield_v3]. This impacts consensus.
* Fixes miner fee reward calculations, was too generous before. This impacts consensus.
* Modifies the minimum static component of the fee of oracle transactions to `1` - as for all other transactions. This impacts consensus.
* Increases beneficiary reward delay to 180 key blocks / generations. This impacts consensus.
* Fixed sporadically seen timeout errors in sync when inet:getaddr took too much time to resolve
* Detects more possible race conditions in state channel updates, thereby also making it possible to "softly reject" an update, by requesting a competing update in response to a signing request. This should be seen as a temporary measure until support for rejecting a signing request is implemented.
* Changes the serialization format of micro headers to include the signature. This changes both what is signed by the miner, and how the block hash for micro blocks is computed. This affects consensus.
* Changes micro block gossip to use Light micro blocks, containing only Tx hashes. In most cases the receiving node
  has already seen all transactions so this saves bandwidth. This bumps the P2P_PROTOCOL_VSN.
* Fine-tunes the determinism of the computation of the dynamic component of the fee of oracle transactions related to TTL of objects (oracles, queries, responses) on state trees, moving floating point computations to integer based ones. This impacts consensus.
* Fixes coinbase instruction in aevm. This impacts consensus.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.21.0
[digishield_v3]: https://github.com/zawy12/difficulty-algorithms/issues/9

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.20.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v0.21.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.21.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.21.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.21.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.21.0/epoch/api/README.md

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
* http://31.13.249.70:3013/v2/blocks/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
