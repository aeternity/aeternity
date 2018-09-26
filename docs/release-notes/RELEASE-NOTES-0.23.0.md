# About this release

[This release][this-release] is focused on TODOFILLMEIN.
It:
* Does all the things mentioned temporarily in files [/docs/release-notes/next/PT-*.md](/docs/release-notes/next/). I.e. each mentioned item is meant to be placed in a file named as the Pivotal Tracker ticket number in which scope the mentioned item is.

TODO: When preparing the release, concatenate all `/docs/release-notes/next/PT-*.md` files and place them in this file.

[this-release]: https://github.com/aeternity/epoch/releases/tag/v0.23.0

This release introduces backward incompatible changes in the chain format:
* After upgrading your node, you will not have your previous balance (even if you keep your key pair);
* Please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.22.x".

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

[docker]: https://github.com/aeternity/epoch/blob/v0.23.0/docs/docker.md
[build]: https://github.com/aeternity/epoch/blob/v0.23.0/docs/build.md

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration).
For specifying configuration using the Docker image, please refer to [its documentation][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-0.1.0/priv/swagger.json`.
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/epoch/blob/v0.23.0/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.23.0/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/epoch-v0.23.0/epoch/api/README.md

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
* http://31.13.249.70:3013/v2/blocks/top

### Setup your node

Setting up your node consists of:
* Configuring your node - see instructions in [the dedicated separate document](../../docs/configuration.md);
* Starting your node and verifying it works as expected - see instructions in [the dedicated separate document](../../docs/operation.md).
* Does this.
* Changes difficulty from a floating point number to an integer value as seen in P2P message PING, and node status.
* Adds fixed gas limit per micro block
* Adds fixed gas to all transactions (contract create and contract call transactions use the fixed gas plus gas needed for their execution)
* Modifies mempool to select transactions up to the gas limit per micro block
* Adds Proof of Fraud (PoF) for reporting a mining leader that creates forks in a generation. This affects consensus.
* Restructures the serialization format for headers by shrinking the version field to 32 bits, and by introducing a flags field in 32 bits. This affects consensus.
* The state channel fsm will now terminate if it detects that someone is
  trying to close the channel on-chain.
* Adds built in functions `String.length` and `String.concat` for Sophia strings.
* Fixes a Sophia bug where None would match a Some(x) pattern
* Events (logs) generated by the VM are added to the call state tree. This affects consensus.
* Fixes the VM `CALL` instruction to place `0` (as opposed to `1`) on the stack if the value operand is not covered by the balance of the currently executing account.
* Fixes the Sophia VM primitive operations to check that the specified value is covered by the balance of the currently executing account - regardless of whether the value is used by the primop.
* Fixed a bug that affected syncing of large generations (> 64K).
* Moves Sophia value encoding/decoding from AEVM bytecode to Erlang code. This affects consensus.
* Makes execution of some chain related instructions in contracts more efficient.
* Makes the TIMESTAMP instruction deterministic by using the timestamp of the current micro block. This affects consensus.
* Makes the DIFFICULTY instruction deterministic by using the difficulty at previous key block. This affects consensus.
* Make the gas cost of oracle VM primitive operations proportional to the TTL of the objects (oracles, queries, responses) on state trees. This impacts consensus.
* Improves stability of the user HTTP API.
