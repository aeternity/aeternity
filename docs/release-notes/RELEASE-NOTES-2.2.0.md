# About this release

[This][this-release] is a maintenance release.

It finalized renaming process of the software.

Please refer to the notes below for details and backward compatibility.

Regarding renaming, this release:
* Changes Erlang node name from `epoch@localhost` to `aeternity@localhost`. This impacts persisted database (the node name is stored in it). **In order to use persisted database created before this release, new tool - `rename_db` - must be used.**
  * The tool `rename_db` is included in the release. It takes one argument - path to database directory. Path to your database directory is `chain` > `db_path` parameter in your user config. If it is undefined, `data` (default directory) should be passed as argument.
  * Example usage of the tool:
    * Using absolute path of database directory - `./bin/aeternity rename_db '/node/aeternity/node/my-old-db-path'`;
    * Using relative path of database directory (or when `chain` > `db_path` is not set in the config) - `./bin/aeternity rename_db data`;
    * On Windows using absolute path of database directory - `.\usr\lib\aeternity\bin\aeternity.cmd rename_db C:\node\aeternity\node\my-old-db-path`;
    * If you are running a node using Docker (assuming you either don't have `db_path` set in your config, or it is set to `/home/aeternity/node/data`) - `docker run --entrypoint=/bin/bash -v ~/.aeternity/myaedb:/home/aeternity/node/data/mnesia -v ~/.aeternity/myaeternity.yaml:/home/aeternity/.aeternity/aeternity/aeternity.yaml aeternity/aeternity -c "/home/aeternity/node/bin/aeternity rename_db /home/aeternity/node/data"`
  * Please use `rename_db` tool when your node is **not running**.
  * Note that, for some environments (e.g. Docker), the node may not be able  to start for the first time after database renaming. If that is the case, please retry to start a node, and the node should manage to start at the second attempt.
  * Before renaming process is conducted, `rename_db` tool automatically creates `schema.DAT.backup` file, next to the original `schema.DAT` file. The file `schema.DAT.backup` contains the backup of `schema.DAT`. If `rename_db` tool is interrupted, and your `schema.DAT` file gets corrupted, please restore from the backup by simply replacing corrupted `schema.DAT` with `schema.DAT.backup`. Then re-run `rename_db` tool.
  * In case your database gets badly corrupted during the process (this should not happen though), please remove all the files from your database directory, and sync again.

For the rest, this release:
* Adds `channels.get.offchain_state` method to channel websocket API.
* Adds a `channels.get.contract` method to channel WebSocket API.
* Refines the error message returned by the user API `/debug/contracts/code/decode-data` when decoding Sophia data.
* Moves the quick installer in its own repository https://github.com/aeternity/installer

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v2.2.0

Apart from the database change (described above in the renaming section), this release is backward compatible with previous `v2.*.*` releases.

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

[docker]: https://github.com/aeternity/aeternity/blob/v2.2.0/docs/docker.md
[build]: https://github.com/aeternity/aeternity/blob/v2.2.0/docs/build.md

The instructions for configuring the node using the Docker image are in [the dedicated separate document][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-*/priv/swagger.json` (you will need to amend the wildcard `*` placeholder in the path with the version).
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/aeternity/blob/v2.2.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/api-docs/?config=https://raw.githubusercontent.com/aeternity/aeternity/v2.2.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/aeternity-node-v2.2.0/node/api/README.md

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
