# About this release

[This][this-release] is the first Fortuna release candidate.
It marks the freeze of the Fortuna consensus protocol and of the user API.

Please refer to the notes below for details and backward compatibility.

Regarding the Fortuna consensus protocol upgrade in testnet, this release:
* Sets the Fortuna testnet hard fork height to block 82900 (approximately Mon 20th May 2019 at 3:30am CEST).

Regarding the Block Reward Initiative, this release:
* Introduces a mechanism to split mining reward and send to predefined address. This is consensus breaking.

Regarding introduction of generalized accounts from the Fortuna consensus protocol, this release:
* Adds generalized accounts. See
  [here](https://github.com/aeternity/protocol/blob/aeternity-node-v3.0.0-rc.1/generalized_accounts/generalized_accounts.md)
  for a description of the new feature.
* Changes the HTTP API for /transactions/info
  This HTTP endpoint used to just return a contract call object, but now returns <<"call_info">> => ContractCallObject instead, i.e. an extra tag is added.
  Additional tag provided is <<"ga_info">> which is what this endpoint returns when the provided transaction hash points to a generalized accounts transaction.
* Changes the HTTP API for /transactions by giving the kind of account of the owner of the transaction
  Either a basic account (as before) or a generalized account
  Signatures are only provided for inner transactions with basic accounts.
* Adds a mining configuration parameter `max_auth_fun_gas` - that limits the amount of gas that can be provided
  to the authentication function in a GAMetaTx.

Regarding state channels, this release:
* Introduces a pinned environment in State Channels noise protocol (off-chain), by introducing a `block_hash` field in some of the messages. This is
  not backwards compatible.
* Changes the structure of off-chain transactions: off-chain updates are moved
  out of it so the on-chain world is agnostic to the off-chain update protocol
  being used as long as force progress expectations are met. This impacts
  consensus and takes action in Fortuna hard fork.
* Introduces off-chain updates to State Channels noise session protocol. This
  impacts off-chain protocol.
* Revisits JSON serialization of off-chain updates to be in sync with the
  corresponding messages for updates' creation. This impacts API.
* Adds a serialization for the `code` element of the off-chain update for
  creating a new off-chain contract.
* Increases the base price for force progress transactions to be in a
  correspondence with contract call base price.
* Adjusts StateChannels WebSocket API broken\_encoding errors.

Regarding the Sophia language, this release:
* Adds Address.is_contract, Address.is_oracle, Oracle.check and Oracle.check_query to Sophia and AEVM.
* Adds Contract.creator to Sophia and AEVM.

Regarding feature refinements, this release:
* Changes some HTTP API fields from plain `string` to encoded strings. See `swagger.yaml` for details.
* Fixes the mempool minimum gas price (configured by miner) entrancy check for contract transactions, they incorrectly included the
  gas in the calculation before.
* Avoids creating empty contract accounts when contract creation failed. This is consensus breaking and starts from Fortuna hard fork.
* Fixes bug that caused crash when contract call had just not enough gas to pay for the memory to store the result.
* Overhauls the swagger.yaml file, including checking the `integer` valued parameters for correctness.

Regarding removal of deprecated functionalities, this release:
* Removes deprecated compiler HTTP APIs (`debug/contracts/[create/compute, call/compute, code/compile, code/call, decode-data, encode-calldata]`).
* Removes backwards compatibility w.r.t. `abi_version`/`vm_version` in HTTP API.
* Removes legacy StateChannels WebSocket protocol.
* Removes the deprecated `bin/epoch` operational script.
* Removes the deprecated `log` field from the contract object in the user APIs.

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v3.0.0-rc.1

The node is able to start from a database produced by `v2.5.*`, `v2.4.*`, `v2.3.*` and `v2.2.*` releases.
For the rest, this release is not backward compatible with `v2.*` releases.

Please join the testnet by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
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

[docker]: https://github.com/aeternity/aeternity/blob/v3.0.0-rc.1/docs/docker.md
[build]: https://github.com/aeternity/aeternity/blob/v3.0.0-rc.1/docs/build.md

The instructions for configuring the node using the Docker image are in [the dedicated separate document][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-*/priv/swagger.json` (you will need to amend the wildcard `*` placeholder in the path with the version).
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/aeternity/blob/v3.0.0-rc.1/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/api-docs/?config=https://raw.githubusercontent.com/aeternity/aeternity/v3.0.0-rc.1/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/aeternity-node-v3.0.0-rc.1/node/api/README.md

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
