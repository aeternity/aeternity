# About this release

[This release][this-release] is focused on TODOFILLMEIN.
It:
* Redirects all the log lines related to the synchronization protocol
to the new log file `epoch_sync.log` instead of `epoch.log`; log lines with
category info and up will still be directed to the console.
* Does this. This impacts consesus.
* Does that. This impacts the persisted DB.
* Does that.
* TODO Improves the stability of the testnet.
* Stops using hashes of unsigned transactions, and uses the hash of the signed transaction instead. This reduces the memory footprint of the system (removes one index). This affects the API by removing the transaction hash when constructing unsigned transactions. This affects the API by returning the hash of the signed transaction in all other applicable places. The latter should not affects users of the API as all transaction hashes are now constructed from the signed transaction. Note that this does not affect consensus, only the http and websocket API:s. This impacts the persisted DB.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.14.0

This release removes deprecated http API endpoints block-by-height
and block-by-hash. Instead use the endpoints as defined
[online in swagger.yaml][swagger-yaml].

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.13.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v0.14.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.14.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`;
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.14.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.14.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.14.0/epoch/api/README.md

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
