# About this release

[This][this-release] is the first Lima release candidate. It marks the freeze of the Lima consensus protocol and of the user API.
It:
* Does all the things mentioned temporarily in files [/docs/release-notes/next/PT-*.md](/docs/release-notes/next/).

TODO: When preparing the release, concatenate all `/docs/release-notes/next/*` Markdown files and place them in this file. (Hint: you can use auxiliary script `scripts/cat-files-in-directory-sorted-by-committer-date` and command `git log -p -w --color-moved`.)

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v5.0.0-rc.1

The node is able to start from a database produced by v4.* releases. For the rest, this release is not backward compatible with v4.* releases.

Please join the mainnet by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

The instructions below describe:
* [How to retrieve the released software for running a node](#retrieve-the-software-for-running-a-node);
* [How to install a node](#install-node);
* [How to join the mainnet](#join-the-mainnet).
* [How to join the testnet](#join-the-testnet).

## Retrieve the software for running a node

You can run a node by either:
* Installing the published [release binary][this-release] corresponding to your platform; or
* Running the published [Docker image `aeternity/aeternity`][docker]; or
* [Building a release binary from source][build].

[docker]: https://github.com/aeternity/aeternity/blob/v5.0.0-rc.1/docs/docker.md
[build]: https://github.com/aeternity/aeternity/blob/v5.0.0-rc.1/docs/build.md

The instructions for configuring the node using the Docker image are in [the dedicated separate document][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-*/priv/swagger.json` (you will need to amend the wildcard `*` placeholder in the path with the version).
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/aeternity/blob/v5.0.0-rc.1/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/api-docs/?config=https://raw.githubusercontent.com/aeternity/aeternity/v5.0.0-rc.1/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/aeternity-node-v5.0.0-rc.1/node/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join the mainnet

### Mainnet seed nodes

The release package comes preconfigured with seed nodes if the `network_id` configuration parameter is set to `ae_mainnet` - which is the default.

### Inspect the mainnet

[Mainnet API gateway](https://mainnet.aeternity.io/v2/status) can be used to inspect current top block and see information about e.g. height or target:

https://mainnet.aeternity.io/v2/blocks/top

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

### Testnet seed nodes

In order to join testnet set the node configuration parameter `network_id` to `ae_uat`

### Inspect the testnet

[Testnet API gateway](https://testnet.aeternity.io/v2/status) can be used to inspect current top block and see information about e.g. height or target:

https://mainnet.aeternity.io/v2/blocks/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
