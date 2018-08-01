# About this release

[This release][this-release] is focused on smart contracts in state channels.
It:
* Removes `key_hash` field from micro blocks. This impacts consensus.
* Fixes a bug when a trusted peer changes its IP, it was crashing instead of just ignoring the change.
* Fine-tunes deposit and withdrawal channel transactions being produced for the user.
  This does not impact channels' protocol
* Adds export command to epoch. The chain can be exported in a binary format (Erlang disk_log of serialized blocks) using the command `epoch export FILENAME`. The first record in the log is a map containing the genesis hash, the hostname and the date and time; the blocks are stored from top to genesis.
* Refines status code 400 as 404 for call object retrieval API `/tx/{tx_hash}/contract-call` when transaction still pending.
* Adds a database table for caching state channel data on disk
* Fixes commitment hash calculations in naming system, to be `Hash(NameHash(name) + name_salt)` instead of `Hash(Hash(name + name_salt))`. This impacts consensus.
* Adds a functionality for contracts creation and execution in channels. Since
  no forcing progress on-chain yet - this does not impact consesus.
* Enriches channels WebSocket API with functionality for getting balances and
  proof of inclusion.
* Expands race detection in channels and adds an error_code field to the
  channel protocol error messages for future improvements in error handling.
  This does not affect the on-chain protocol.
* Change PoW to 2^30 node graph. This impacts consensus.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.19.0

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.18.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v0.19.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.19.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.19.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.19.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.19.0/epoch/api/README.md

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
