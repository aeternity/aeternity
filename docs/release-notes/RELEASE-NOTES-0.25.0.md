# About this release

[This release][this-release] is the first release candidate.
It:
* Adds response TTL to Oracle query response TX - a required parameter. This affects consensus.
* Adds JSON-RPC support in the state channel WebSocket API. The previous API remains supported for now. Support for JSON-RPC request batches implemented but not yet tested.
* Forbids certain prim-ops to be run in off-chain contracts
* Introduces minimum gas prace validation and sets minimum gas price to 1.
* Changes default state channel websocket listen address to `127.0.0.1` (previously `0.0.0.0`)
* Changes the keyblock target to be 4 bytes in serialization (was 8 bytes). This affects consensus.
* Moves all charges in contract create transaction and contract call transaction to before the execution of the contract. The changed items are: gas in both create and call transactions (unused gas is refunded); fee in call transaction; deposit in create transaction. This affects consensus.
* Removes internal websocket API (`/websocket` endpoint)
* Avoids replay attacks by naming networks (forks) with network_id and including them in transaction signature
* Optimizes the force progress transaction by turning the `poi` to a proper
  set of off-chain trees and removing the auxiliary `addresses`
* Disallows reentrant calls (i.e. contract calls into a currently executing contract) in Sophia.
* Improves the stability of the HTTP user API.
* Charges gas also for calldata size/initial heap size when doing Sophia contract calls. This affects consensus.
* Implements the Sophia abort primitive using the REVERT instruction in the aevm.
* Refactors State Channel's approach to disputes. The block height timer
  between subsequent force progress transactions is removed. This greatly
  improves the speed of forcing of progress. A co-signed state still 
  overwrites on-chain produced states. This impacts consesus.
* Sets the return value of initial call in create contract transaction to the empty byte array "<<>>" in the call state tree. This affects consensus (state tree root has changes).
* Increases the gas of the VM primitive operations that create or extend objects on the state trees, approximating that to a component proportional to the serialized equivalent transaction. This affects consensus.
* Fixes gas available to VM primops contract to be the same as for other contracts. This affects consensus.
* Charges gas for maps handling in return value. This affects consensus.
* Restricts registrars in Naming Service, to allow only `.test` registrar
* Introduces throw away keys for block mining and signing
* Changes the API for constructing contract create and call transactions, allowing calls with user-defined types.
* Changes calling convention in sophia and add type checking of remote calls. This makes the byte code smaller, the memory usage smaller, the contract calls more type safe. This affects consensus.
* Introduces the vm_version field in oracles. This affects consensus.
* Validates query and response formats in oracles. The nature of the check depends on the new vm version field.
* Makes it possible for a contract to interact with oracles with plain string queries/responses.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.25.0

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.24.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v0.25.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.25.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.25.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.25.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.25.0/epoch/api/README.md

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
