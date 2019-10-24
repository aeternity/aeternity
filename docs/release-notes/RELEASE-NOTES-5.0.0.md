# About this release

[This][this-release] is the stable Lima release that introduces FATE VM, as well as improving state channels

Changelog:

* We now have FATE. The initial version of FATE is released as VM version `0x05`.
* The [```channel_close_mutual```](https://github.com/aeternity/protocol/blob/master/channels/ON-CHAIN.md#channel_close_mutual)
  transaction can now be performed while the channel is not active on-chain (The channel solo closing sequence had been initiated and the channel is not yet closed),
  this is a consensus breaking change available after the Lima fork.
* The state channel FSM after the Lima fork accepts a shutdown message while the channel is being closed unilaterally.
* Introduces AEVM version `0x06`, available from Lima consensus.
  This is a fine-tuned version of the AEVM version `0x04` introduced in the Fortuna consensus.
* Extends transaction signing - also allow signatures of the transaction hash.
  The transaction serialization is potentially long, so signing the
  transaction hash means less cryptographic work.
* Obsoletes the old State Channel off-chain transaction type which contained
  the list of updates that produced the latest `state_hash`. The new
  transaction type is available since Fortuna hard fork and the FSM is
  producing such transactions ever since. This detaches the off-chain protocol
  from the on-chain one and allows development of unique off-chain protocols
  that don't need their updates to be serializable on-chain.
* For existing state channels where the latest co-authenticated state is an
  off-chain transaction based on the old version, it is suggested to make a
  new co-authenticated transaction which is using the new serialization.  For
  currently existing state channels which latest co-authenticated state is an
  old version off-chain transaction is suggested to make a new
  co-authenticated transaction that is using the new serialization. If the
  other party refuses to authenticate a new round or is simply missing, one
  can use the solo closing sequence instead.
* Metadata objects (of type binary) can now be added to an offchain update transfer request.
  These objects serve as comments and are not part of the signed transaction, nor do they
  affect the offchain state trees. This is an incompatible change in the serialization of offchain
  updates, but old offchain updates can still be deserialized. When creating a channel with a party
  running an older node version, add `version_offchain_update=1` to the channel params.
* Remove the `rename_db` migration command used to migrate Roma databases.
* Remove the custom docker entrypoint.
  To run the docker image with extra arguments the default command should be used as well, e.g.:
  `docker run aeternity/aeternity bin/aeternity console -noinput -network_id ae_uat` see the corresponding docs for details.
cat: docs/release-notes/next-lima/PT-168132312-new-name-hash: No such file or directory
* No error messages in contract call objects - they will end up under consensus.
  Run your contract in `dry-run` to get the detailed error message.
* Name preclaims are no longer allowed to use `0` as `salt` value
* New name hash computation
* New governance function determining if a name is subject to direct claim or auction
* New governance function determining initial price of a name
* New name auction mechanism using subsequent claim transactions with `salt` equal to `0`
* State Channels: Changed configuration option name `ws_handlers` to `sc_ws_handlers` which
  correctly implies that the limit applies to SC websocket connections.
* State Channels: It is now possible to abort a signing request by replying with an error code.
* State Channels: The FSMs now stay alive until a Close Mutual (shutdown) has been confirmed on-chain
  It is also possible to abort/reject a Close Mutual by rejecting the signing request.
  See https://github.com/aeternity/protocol/blob/master/node/api/channel_ws_api.md#signing-error-replies
* State Channels: On-chain tx monitoring was broken after leave/reestablish. This has been fixed.
* State Channels: Enhances the FSM with the optional pinned environment for execution
  off-chain transactions. This is to improve reaching off-chain consensus as
  both parties share a common view of what is considered to be a fork-safe
  block hash.
* State Channels: Introduces on-disk state cache encryption
    * When a user leaves a state channel the off-chain state will be persisted on disk and protected 
      with a password. The password is provided by the user when opening a channel using the `state_password` 
      parameter.
    * When re-establishing the channel the same password **MUST** be provided, otherwise 
      the operation will fail with error code `invalid_password`.
    * The password **MUST** be at least 6 characters long. Providing a shorter password 
      will fail with error code `invalid_password`.
    * The password may be changed anytime by the user through the websocket connection after the channel 
      has been opened. Please consult the documentation for more details. This operation is only allowed 
      when the channel is established. If the user has left the channel, the channel must be re-established
      first before changing the password.
    * Until the lima fork the password will be optional. By not providing a password a default value is used:
      `correct horse battery staple`. After the Lima fork the password will become mandatory. Pre-Lima off-chain states 
      will be encrypted with the default password.
    * Because the password used for encrypting the persisted state cache is mandatory after the Lima fork, 
      state channels which were opened before Lima use the default password. 
      Not providing the password in the websocket connection will result in a `missing_field` error.
    * Keep in mind that an adversary with direct RAM access to the node may still steal the off-chain state.
      This change only protects the state against an adversary with direct disk access.
* Added infrastructure for deploying contracts (with fresh tokens) during hard forks from Lima release.
  This will be used to finalize the token migration.
* Added the migrated tokens in the Phase 3 of the token migration.
* Adds one (1) percent of the total initial (ERC20) token supply to an account
  according to Pool D in https://hackmd.aepps.com/s/H1qF1w1j7#
* Set Lima testnet hardfork height to 154300 (16th Oct 2019, 09:00 UTC)
* Set Lima mainnet to 161150 (30th Oct 2019, 13:00 UTC)
* Aligns AEVM store gas pricing with FATE.
* ContractCallTX where the called contract uses FATE has a lower base cost (60% cheaper than a call to an AEVM contract).
* State Channels: The support for state cache encryption (introduced in v5.0.0-rc.2)
  has been disabled. An improved approach is being developed which greatly improves API
  usability. This feature is expected to be released with v5.1.0.
* Change top level namespace from **.aet** to **.chain**
* Better handling of some errors w.r.t. `illegal_salt_value`
* From Lima, catch crashes when processing inner transaction in GAMetaTx; the user should pay for
  a successful authentication despite the inner Tx crashing.

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v5.0.0

The node is able to start from a database produced by `v4.*` releases, otherwise this release is not backward compatible.

Please join the **mainnet** by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

The instructions below describe:

* [How to retrieve the released software for running a node](#retrieve-the-software-for-running-a-node);
* [How to install a node](#install-a-node);
* [How to join the mainnet](#join-the-mainnet).
* [How to join the testnet](#join-the-testnet).

## Retrieve the software for running a node

You can run a node by either:

* Installing the published [release binary][this-release] corresponding to your platform; or
* Running the published [Docker image `aeternity/aeternity`][docker]; or
* [Building a release binary from source][build].

[docker]: https://github.com/aeternity/aeternity/blob/v5.0.0/docs/docker.md
[build]: https://github.com/aeternity/aeternity/blob/v5.0.0/docs/build.md

The instructions for configuring the node using the Docker image are in [the dedicated separate document][docker].

The node user API is documented:

* An interactive visualization of the specification is available [online][swagger-ui].
* HTTP API endpoints are specified [in swagger.yaml][swagger-yaml];
  * A copy is located in the node at path `usr/lib/aeternity/lib/aehttp-5.0.0/priv/swagger.yaml`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/aeternity/blob/v5.0.0/apps/aehttp/priv/swagger.yaml
[swagger-ui]: https://api-docs.aeternity.io/
[api-doc]: https://github.com/aeternity/protocol/blob/aeternity-node-v5.0.0/node/api/README.md

## Install a node

The instructions for installing a node using a release binary are in [the dedicated document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to the [documentation online][docker].

## Join the mainnet

In order to join the **mainnet** follow the [operation instructions](../../docs/operation.md) to run the node with default configuration as mainnet is the default network.

To join the **mainnet** by using the Docker image, please refer to [docker documentation][docker].

### Mainnet seed nodes

The default [network_id](../../docs/configuration.md#network-id) that the node package is preconfigured with is **mainnet** with id `ae_mainnet`.

The release package comes preconfigured with seed nodes. Here is an example subset of the seed nodes:

```
aenode://pp_2L8A5vSjnkLtfFNpJNgP9HbmGLD7ZAGFxoof47N8L4yyLAyyMi@18.136.37.63:3015
aenode://pp_2gPZjuPnJnTVEbrB9Qgv7f4MdhM4Jh6PD22mB2iBA1g7FRvHTk@52.220.198.72:3015
aenode://pp_tVdaaX4bX54rkaVEwqE81hCgv6dRGPPwEVsiZk41GXG1A4gBN@3.16.242.93:3015
aenode://pp_2mwr9ikcyUDUWTeTQqdu8WJeQs845nYPPqjafjcGcRWUx4p85P@3.17.30.101:3015
aenode://pp_2CAJwwmM2ZVBHYFB6na1M17roQNuRi98k6WPFcoBMfUXvsezVU@13.58.177.66:3015
aenode://pp_7N7dkCbg39MYzQv3vCrmjVNfy6QkoVmJe3VtiZ3HRncvTWAAX@13.53.114.199:3015
aenode://pp_22FndjTkMMXZ5gunCTUyeMPbgoL53smqpM4m1Jz5fVuJmPXm24@13.53.149.181:3015
aenode://pp_Xgsqi4hYAjXn9BmrU4DXWT7jURy2GoBPmrHfiCoDVd3UPQYcU@13.53.164.121:3015
aenode://pp_vTDXS3HJrwJecqnPqX3iRxKG5RBRz9MdicWGy8p9hSdyhAY4S@13.53.77.98:3015
```

### Inspect the mainnet

Here are example nodes that can be used to inspect current top block and see information about e.g. height or target:

* http://18.136.37.63:3013/v2/blocks/top
* http://52.220.198.72:3013/v2/blocks/top
* http://13.53.114.199:3013/v2/blocks/top
* http://13.53.149.181:3013/v2/blocks/top

## Join the testnet

In order to join the **testnet** change the [network_id](../../docs/configuration.md#network-id) in the node configuration file to `ae_uat`.

To join the **testnet** by using the Docker image, please refer to the [docker documentation][docker].

### Testnet seed nodes

The release package comes preconfigured with **testnet** seed nodes, this is the list:
```
aenode://pp_QU9CvhAQH56a2kA15tCnWPRJ2srMJW8ZmfbbFTAy7eG4o16Bf@52.10.46.160:3015
aenode://pp_2vhFb3HtHd1S7ynbpbFnEdph1tnDXFSfu4NGtq46S2eM5HCdbC@18.195.109.60:3015
aenode://pp_27xmgQ4N1E3QwHyoutLtZsHW5DSW4zneQJ3CxT5JbUejxtFuAu@13.250.162.250:3015
aenode://pp_DMLqy7Zuhoxe2FzpydyQTgwCJ52wouzxtHWsPGo51XDcxc5c8@13.53.161.215:3015
```

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet.

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the **testnet** can be obtained by opening in the browser any of the following URLs:

* http://52.10.46.160:3013/v2/blocks/top
* http://18.195.109.60:3013/v2/blocks/top
* http://13.250.162.250:3013/v2/blocks/top
* http://13.53.161.215:3013/v2/blocks/top

### Setup your node

Setting up your node consists of:

* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
