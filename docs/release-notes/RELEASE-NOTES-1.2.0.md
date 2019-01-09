# About this release

[This][this-release] is a maintenance release.
It:
* Does all the things mentioned temporarily in files [/docs/release-notes/next/PT-*.md](/docs/release-notes/next/).
* The Sophia compiler has been moved to a [seprate repository](https://github.com/aeternity/aesophia/).

TODO: When preparing the release, concatenate all `/docs/release-notes/next/*` Markdown files and place them in this file. (Hint: you can use auxiliary script `scripts/cat-files-in-directory-sorted-by-committer-date` and command `git log -p -w --color-moved`.)

[this-release]: https://github.com/aeternity/epoch/releases/tag/v1.2.0

This release is backward compatible with v1.1.* and v1.0.*.

Please join the Roma network by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/epoch/wiki/Troubleshooting).

The instructions below describe:
* [How to retrieve the released software for running a node](#retrieve-the-software-for-running-a-node);
* [How to install a node](#install-node);
* [How to join the roma network](#join-roma-network).
* [How to join the testnet](#join-the-testnet).

## Retrieve the software for running a node

You can run a node by either:
* Installing the published [release binary][this-release] corresponding to your platform; or
* Running the published [Docker image `aeternity/epoch`][docker]; or
* [Building a release binary from source][build].

[docker]: https://github.com/aeternity/epoch/blob/v1.2.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v1.2.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-*/priv/swagger.json` (you will need to amend the wildcard `*` placeholder in the path with the version).
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v1.2.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v1.2.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v1.2.0/epoch/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join Roma network

### Roma network seed nodes

The release package comes preconfigured with seed nodes. Here is example subset of the seed nodes:

* aenode://pp_5mmzrsoPh9owYMfKhZSkUihufDTB6TuayD173Ng464ukVm9xU@35.178.61.73:3015
* aenode://pp_2KWhoNRdythXAmgCbM6QxFo95WM4XXGq2pjcbKitXFpUHnPQc3@35.177.192.219:3015
* aenode://pp_2L8A5vSjnkLtfFNpJNgP9HbmGLD7ZAGFxoof47N8L4yyLAyyMi@18.136.37.63:3015
* aenode://pp_2gPZjuPnJnTVEbrB9Qgv7f4MdhM4Jh6PD22mB2iBA1g7FRvHTk@52.220.198.72:3015
* aenode://pp_frAKABjDnM3QZCUygbkaFvbd8yhv6xdufazDFLgJRc4fnGy3s@52.56.252.75:3015
* aenode://pp_tVdaaX4bX54rkaVEwqE81hCgv6dRGPPwEVsiZk41GXG1A4gBN@3.16.242.93:3015
* aenode://pp_2mwr9ikcyUDUWTeTQqdu8WJeQs845nYPPqjafjcGcRWUx4p85P@3.17.30.101:3015
* aenode://pp_FLpSUrKwgBAu5uVRnB2iWKtwGAHZckxvtCbjVPeeCA3j33t3J@52.56.66.124:3015
* aenode://pp_2CAJwwmM2ZVBHYFB6na1M17roQNuRi98k6WPFcoBMfUXvsezVU@13.58.177.66:3015

### Inspect Roma network

Here are example nodes that can be used to inspect current top block and see information about e.g. height or target:

* http://35.178.61.73:3013/v2/blocks/top
* http://35.177.192.219:3013/v2/blocks/top
* http://18.136.37.63:3013/v2/blocks/top
* http://52.220.198.72:3013/v2/blocks/top

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

### Testnet seed nodes

In order to join testnet reconfigure seed nodes in the release package:

* aenode://pp_QU9CvhAQH56a2kA15tCnWPRJ2srMJW8ZmfbbFTAy7eG4o16Bf@52.10.46.160:3015
* aenode://pp_2vhFb3HtHd1S7ynbpbFnEdph1tnDXFSfu4NGtq46S2eM5HCdbC@18.195.109.60:3015
* aenode://pp_27xmgQ4N1E3QwHyoutLtZsHW5DSW4zneQJ3CxT5JbUejxtFuAu@13.250.162.250:3015
* aenode://pp_nt5N7fwae3DW8Mqk4kxkGAnbykRDpEZq9dzzianiMMPo4fJV7@18.130.148.7:3015

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
