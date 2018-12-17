# About this release

[This][this-release] is an experimental Minerva release.
It:
* Does all the things mentioned temporarily in files [/docs/release-notes/next-minerva/PT-*.md](/docs/release-notes/next-minerva/).

TODO: When preparing the release, concatenate all `/docs/release-notes/next-minerva/*` Markdown files and place them in this file. (Hint: you can use auxiliary script `scripts/cat-files-in-directory-sorted-by-committer-date` and command `git log -p -w --color-moved`.)

[this-release]: https://github.com/aeternity/epoch/releases/tag/TODO-TO-BE-DECIDED

This release is not backward compatible with v1.*.

## Retrieve the software for running a node

You can run a node by either:
* Installing the published [release binary][this-release] corresponding to your platform; or
* Running the published [Docker image `aeternity/epoch`][docker]; or
* [Building a release binary from source][build].

[docker]: https://github.com/aeternity/epoch/blob/TODO-TO-BE-DECIDED/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/TODO-TO-BE-DECIDED/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/TODO-TO-BE-DECIDED/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/TODO-TO-BE-DECIDED/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-TODO-TO-BE-DECIDED/epoch/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join the Minerva experimental testnet

This section describes how to run a node as part of the Minerva experimental testnet - the Developer-oriented public test network of nodes running the latest Minerva unstable experimental release - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

### Minerva experimental testnet seed nodes

In order to join Minerval experimental testnet reconfigure seed nodes in the release package:

* aenode://pp_TODO@T.O.D.0:3015
* aenode://pp_TODO@T.O.D.0:3015
* aenode://pp_TODO@T.O.D.0:3015
* aenode://pp_TODO@T.O.D.0:3015

### Inspect the Minerval experimental testnet

The core nodes of the public test network are accessible from the Internet.

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://T.O.D.O:3013/v2/blocks/top
* http://T.O.D.O:3013/v2/blocks/top
* http://T.O.D.O:3013/v2/blocks/top
* http://T.O.D.O:3013/v2/blocks/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
