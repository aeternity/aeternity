# About this release

[This release][this-release] is the second release candidate.
It:
* Changes package versioning scheme to follow the [https://semver.org](semver.org)
* Changes encoding from base58 to base64 for all binary data in the api that is not identifiers (e.g., accounts, contracts, etc). The base64 encoded string uses the same check as the checked base58c (i.e., the first 4 bytes of the twice sha256 hashed byte array is appended last before base64 encoding the data).
* Adds Sophia syntax for map lookup and update with default values.
* Refunds value transfers on failed contract calls. This affects consensus.
* Adds a minimal transaction fee check to mempool and micro block validation. The minimal fee is computed from transaction gas and minimal gas price. This affects consensus.
* Fixes micro blocks signing when key block is posted by /key-blocks HTTP API
* Aligns the gas cost of the AEVM instructions to the ones in recent Byzantium EVM for preventing known potential denial-of-service attack vectors. This affects consensus.
* No longer stores call objects for calls from another contract. This affects consensus.
* Stores contributors messages file in repo and puts its hash in the genesis block (in `prev_hash` field). This affects consensus.
* Changes naming scheme for miner executables. Used to be `lean/mean/cuda<node-bits>`, this is changed to
  `lean/mean/cuda<edge-bits>` to align with upstream cuckoo. An upstream sync is also performed. *NOTE:*
  changes in `epoch.yaml` are necessary (i.e. `mean30-generic -> mean29-generic`, etc.). Does not affect consensus.
* Fixes the deserialization of name transfer transactions with name as recipient.  This deserialization is exercised when such a transaction is exchanged among network peers or is posted by the user from the user API.
* Fixes validation of minimum gas price in state channel force progress transaction, preventing sender of force progress (on-chain) transaction from not paying the gas of the call.  This affects consensus.
* Charges gas for serialization of Sophia values in the VM. This affects consensus.
* Requires the sender of the oracle response transaction to have enough balance for paying for the transaction fee without considering the to-be-awarded oracle query fee. This aligns the balance required for the oracle response transaction to the other transactions. This affects consensus.
* Fixes a bug in channels on-chain mechanics. This impacts consensus.
* Increases the gas of oracle transactions by adding a TTL state gas. Therefore, the fees for oracle transactions are higher. This affects consensus.
* Introduces a dry-run API where a list of SpendTx, ContractCreateTx and ContractCallTx can be sent for off-chain evaluation. At
  the same time disables the broken off-chain (<<"sophia">> ABI) call through `debug/contracts/code/call`.
* Disables the Solidity EVM. It was very useful while developing, but it is not tested enough to be part of consensus, so it is disabled. This affects
  consensus.
* Changes mining rewards by height to match inflation curve. This affects consensus.
* Adds a slow start of mining rewards to stabilize before the full rewards are given.
* Fixes a performance problem with contract state containing large maps. This affects consensus.
* Details the error reason in the contract call object in case of illegal instruction. This affects consensus.
* Disables unsupported instructions from Sophia VM. These include: CREATE, SELFDESTRUCT, CALLCODE, DELEGATECALL, calldata-related opcodes, returndata-related opcodes, store-related opcodes. This affects consensus.
* Changes behaviour of Name Claim, State Channels Close Mutual and State
  Channels Settle transactions so that they do not burn tokens, and send tokens
  to a special account without private key access instead. This affects
  consensus.
* Changes behaviour in miner reward distribution: when a proof of fraud is
  received - the fraudelent does not receive any reward and the poster of the
  proof of fraud gets a fraction of it. In order for this not to skew the
  inflation, excess tokens are sent to a special account without private key
  access instead. This affects consensus.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v1.0.0-rc2

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.25.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v1.0.0-rc2/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v1.0.0-rc2/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v1.0.0-rc2/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v1.0.0-rc2/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v1.0.0-rc2/epoch/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

For running a node as part of the testnet by using the Docker image, please consult [its documentation][docker] in addition to this section.

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
