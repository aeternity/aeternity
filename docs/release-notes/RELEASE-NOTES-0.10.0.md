# About this release

[This release][this-release] is focused on an encrypted Peer-to-Peer protocol and consistent serialization of chain objects.
It:
* Moves Peer-to-Peer communication from HTTP to encrypted TCP channels (Noise is used to negoitiate cryptographic material).
* Improves chain handling by storing accumulated difficulty in the db rather than recomputing it. This impacts the persisted DB.
* Enable user configuration of HTTP API acceptors pool
* Restructure the transaction root hash in the block header for stricter validation. This impacts consensus and the persisted DB.
* Changes the binary serialization format of most of the chain objects (e.g., transactions, accounts) to make them stricter and more portable. This impacts consensus and the persisted DB.
* Improves the stability of the node.
* Does this. This impacts consensus;
* Does that. This impacts the persisted DB;
* Does that;
* Improves the stability of the testnet.
* Changes the DockerHub organization to `aeternity`

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.10.0

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.9.x".

Please join the testnet by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/epoch/wiki/Troubleshooting).

The instructions below describe:
* [How to retrieve the released software for running a node](#retrieve-the-software-for-running-a-node);
* [How to install a node](#install-node);
* [How to join the testnet](#join-the-testnet).

## Retrieve the software for running a node

You can run a node by using either:
* The published [release binary][this-release] corresponding to your platform; or
* The published [Docker image `aeternity/epoch`][docker].

[docker]: https://github.com/aeternity/epoch/blob/v0.10.0/docs/docker.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`;
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.10.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.10.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.10.0/epoch/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet.

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://31.13.249.0:3013/v2/top
* http://31.13.249.1:3013/v2/top
* http://31.13.249.120:3013/v2/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
