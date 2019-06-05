# About this release

[This][this-release] is a maintenance release.
It:
* Improves scheduling of Ping:s - make sure we keep/restart pinging after synchronization is done.
* Adds support for responding of `ping` messages in State Channels' WebSocket
  protocol. Because of browser compatibility issues, the keep alive
  functionality is being built on data frames instead of using the control
  frames.
* Enhances the HTTP endpoint for channel creation transaction to accept an optional list of delegates.
* Introduces State Channel fsm-assisted solo-close and slash, and continuous chain monitoring.
  * The state channel chain watcher now watches the chain continuously.
  * The state channel FSM stays open during the closing phase.
  * Client WS API for solo-closing, slash and settle.
  * SC FSM automatically detects when a slash is needed.
  * SC FSM reports to client any time the on-chain channel state changes.
  * The SC watcher is now fork-aware.

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v3.1.0

This release is backward compatible with the previous v3.0.1 release.

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

[docker]: https://github.com/aeternity/aeternity/blob/v3.1.0/docs/docker.md
[build]: https://github.com/aeternity/aeternity/blob/v3.1.0/docs/build.md

The instructions for configuring the node using the Docker image are in [the dedicated separate document][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-*/priv/swagger.json` (you will need to amend the wildcard `*` placeholder in the path with the version).
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/aeternity/blob/v3.1.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/api-docs/?config=https://raw.githubusercontent.com/aeternity/aeternity/v3.1.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/aeternity-node-v3.1.0/node/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join the mainnet

### Mainnet seed nodes

The release package comes preconfigured with seed nodes. Here is example subset of the seed nodes:

* aenode://pp_2L8A5vSjnkLtfFNpJNgP9HbmGLD7ZAGFxoof47N8L4yyLAyyMi@18.136.37.63:3015
* aenode://pp_2gPZjuPnJnTVEbrB9Qgv7f4MdhM4Jh6PD22mB2iBA1g7FRvHTk@52.220.198.72:3015
* aenode://pp_tVdaaX4bX54rkaVEwqE81hCgv6dRGPPwEVsiZk41GXG1A4gBN@3.16.242.93:3015
* aenode://pp_2mwr9ikcyUDUWTeTQqdu8WJeQs845nYPPqjafjcGcRWUx4p85P@3.17.30.101:3015
* aenode://pp_2CAJwwmM2ZVBHYFB6na1M17roQNuRi98k6WPFcoBMfUXvsezVU@13.58.177.66:3015
* aenode://pp_7N7dkCbg39MYzQv3vCrmjVNfy6QkoVmJe3VtiZ3HRncvTWAAX@13.53.114.199:3015
* aenode://pp_22FndjTkMMXZ5gunCTUyeMPbgoL53smqpM4m1Jz5fVuJmPXm24@13.53.149.181:3015
* aenode://pp_Xgsqi4hYAjXn9BmrU4DXWT7jURy2GoBPmrHfiCoDVd3UPQYcU@13.53.164.121:3015
* aenode://pp_vTDXS3HJrwJecqnPqX3iRxKG5RBRz9MdicWGy8p9hSdyhAY4S@13.53.77.98:3015

### Inspect the mainnet

Here are example nodes that can be used to inspect current top block and see information about e.g. height or target:

* http://18.136.37.63:3013/v2/blocks/top
* http://52.220.198.72:3013/v2/blocks/top
* http://13.53.114.199:3013/v2/blocks/top
* http://13.53.149.181:3013/v2/blocks/top

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

### Testnet seed nodes

In order to join testnet reconfigure seed nodes in the release package:

* aenode://pp_QU9CvhAQH56a2kA15tCnWPRJ2srMJW8ZmfbbFTAy7eG4o16Bf@52.10.46.160:3015
* aenode://pp_2vhFb3HtHd1S7ynbpbFnEdph1tnDXFSfu4NGtq46S2eM5HCdbC@18.195.109.60:3015
* aenode://pp_27xmgQ4N1E3QwHyoutLtZsHW5DSW4zneQJ3CxT5JbUejxtFuAu@13.250.162.250:3015
* aenode://pp_DMLqy7Zuhoxe2FzpydyQTgwCJ52wouzxtHWsPGo51XDcxc5c8@13.53.161.215:3015

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet.

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://52.10.46.160:3013/v2/blocks/top
* http://18.195.109.60:3013/v2/blocks/top
* http://13.250.162.250:3013/v2/blocks/top
* http://13.53.161.215:3013/v2/blocks/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
