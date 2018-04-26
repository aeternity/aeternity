# About this release

[This release][this-release] is focused on faster sync - approx. 10 times faster.
It:
* Changes the sync strategy, making it possible to sync a chain much more efficiently by focusing the sync on syncing with individual forks instead of simply syncing with individual peers.
* Regulates the maximum number of concurrent connections (`sync` > `max_connections`) and the number of processes accepting connections (`sync` > `acceptors`).
* Exposes more configuration previously hard-coded, (`sync` > `connect_timeout`), (`sync` > `first_ping_timeout`) and (`sync` > `noise_hs_timeout`).
* Adds a payload to spend transactions. This impacts consensus.
* Improves the chain representation by keeping track of fork points. It reduces the need for block-by-block traversals, which in turn speeds up many chain operations. This impacts the persisted DB.
* Introduces database table versions and a startup check to handle old versions.
* Relocates user APIs for querying block by height or hash from the internal to the external endpoint.
* Improves the stability of the testnet.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.12.0

This release introduce a new consensus protocol version at height 7450 while the genesis block is unchanged from the previous releases "v0.11.x" and "v0.10.1", therefore:
* Ensure you keep your account key pair: after upgrading your node, you shall have your previous balance as of approx. Thu 26th Apr 2018;
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.11.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v0.12.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.12.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`;
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.12.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.12.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.12.0/epoch/api/README.md

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
