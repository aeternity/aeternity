# About this release

[This][this-release] is a major Fortuna release, mainly aimed at easing development of applications using state channels in the presence of generic accounts.
It:
* Introduces State Channel FSM Generalized Accounts awareness. It now fully
  supports creation of meta transactions and verifies them.
* Changes WebSocket API regarding signing transactions: now sign requests
  provide a signed transaction to be signed. Old approach was sending unsigned
  transactions.
* Changes channel reestablish checks: for on-chain transactions authentication
  can not always be checked at current chain top. Check is done, if the last
  transaction is an on-chain one, it must be present in on-chain.
* Changes the off-chain protocol to accommodate Generalized Account
  authentication methods. This is off-chain noise protocol breaking change.
* Enhances the state Channels WebSocket API, that now supports starting a responder with `"initiator_id": "any"`.
  The responder instance will get the proper initiator Id from the `channel_open` message once a
  connection is established.
* Enables multiple different State Channel responder pubkeys to share the same listen port.
* Changes the `channel_open` and `channel_accept` messages to contain both the initiator and
  responder public keys. This is not backwards compatible for the `noise` protocol.
* Enhances State Channel's WebSocket API with providing more meaningful
  messages when failing to open a channel because of invalid opening arguments.
* Makes State Channel WebSocket API more consistent regarding the usage of
  `caller` and `contract` vs `caller_id` and `contract_id`. This is an API
  breaking change.
* Allows sending and receiving generic messages in State Channels in any FSM
  state. Until this - generic messages were allowed only in `open` state.
  `channel_id` is part of the message body and if it is unknown - the
  temporary one is used instead. This is not backwards compatible for the
  `noise` protocol. This enhances the WebSocket API accordingly.
* Fixes the value of the discriminator field (`type`) for the oracle response transaction in the user API paths returning transactions (e.g. path `/transactions/{hash}`).
* Enhances the response of the dry-run API (path `/debug/transactions/dry-run`) for contract create transaction by adding the information for the call of the initialization function e.g. the gas used.  This makes the response of the dry-run for the contract create transaction analogous to the one for the contract call transaction.

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v4.0.0

This release introduces backward incompatibilities in the channels user WebSocket API and in the channels external Noise endpoint protocol.
For the rest, this release is backward compatible with previous `v3.*` releases.

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

[docker]: https://github.com/aeternity/aeternity/blob/v4.0.0/docs/docker.md
[build]: https://github.com/aeternity/aeternity/blob/v4.0.0/docs/build.md

The instructions for configuring the node using the Docker image are in [the dedicated separate document][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-*/priv/swagger.json` (you will need to amend the wildcard `*` placeholder in the path with the version).
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/aeternity/blob/v4.0.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/api-docs/?config=https://raw.githubusercontent.com/aeternity/aeternity/v4.0.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/aeternity-node-v4.0.0/node/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to the [documentation online][docker].

## Join the mainnet

In order to join the mainnet follow the [operation instructions](../../docs/operation.md) to run the node with default configuration as mainnet is the default network.

To join the mainnet by using the Docker image, please refer to [docker documentation][docker].

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

In order to join the testnet change the [Network ID](../../docs/configuration.md#network-id) in node configuration file to `ae_uat`.

To join the testnet by using the Docker image, please refer to the [docker documentation][docker].

### Testnet seed nodes

The release package comes preconfigured with testnet seed nodes, this is the list:

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
