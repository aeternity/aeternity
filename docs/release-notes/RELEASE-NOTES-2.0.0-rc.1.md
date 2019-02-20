# About this release

[This][this-release] is the first Minerva release candidate.
It contains:
* Refinements in the seed nodes of the environments.
* Changes reflecting the canonical name of the software.
* The implementation of the Minerva consensus protocol version - testnet-only in this release.
* Improvements in the Sophia language and the usage of its compiler.
* Feature refinements.

Please refer to the notes below for details and backward compatibility.

Regarding the seed nodes of the environments:
* Testnet seed node 18.130.148.7 has been replaced by 13.53.161.215
* Roma network seed nodes have been replaced according to the following table:

  Old seed nodes | New seed nodes
  --- | ---
  35.178.61.73 |     13.53.114.199
  35.177.192.219 |   13.53.149.181
  52.56.252.75 |     13.53.164.121
  52.56.66.124 |     13.53.77.98
  3.8.105.183 |      13.53.213.137
  3.8.30.66 |        13.53.51.175
  35.177.165.232 |   13.53.161.210
  35.177.212.38 |    13.53.162.212
  35.176.217.240 |   13.53.89.32
  18.130.106.60 |    13.53.78.163

  If you don't explicitly configure those nodes IP/keys, you don't need to do anything, otherwise **update your configuration**, because all old nodes will be shutdown in short period of time

Regarding the canonical name of the software (from `epoch` to `aeternity` - started in release 1.3.0), this release:
* Changes user config discovery paths, i.e. the node is looking for the user config in:
  - AETERNITY_CONFIG environment variable instead of EPOCH_CONFIG,
  - ~/.aeternity/aeternity/aeternity.yaml file instead of ~/.epoch/epoch/epoch.yaml,
  - ${AETERNITY_TOP}/aeternity.yaml file instead of ${AETERNITY_TOP}/epoch.yaml.
  Backwards compatibility is kept for now, so user config defined in old locations will be working until the next major release.
* Renames the log files:

  Old file name | New file name
  --- | ---
  "epoch.log" | "aeternity.log"
  "epoch_mining.log" | "aeternity_mining.log"
  "epoch_sync.log" | "aeternity_sync.log"
  "epoch_pow_cuckoo.log" | "aeternity_pow_cuckoo.log"
  "epoch_metrics.log" | "aeternity_metrics.log"

Regarding the Minerva consensus protocol upgrade on testnet, this release:
* Sets the MINERVA testnet (UAT) hard fork height to block 40900
* Adds token migration support for the hard fork
* Adds an optional info field to the key block/header that is allowed from the Minerva consensus version.
* Adds the info field to all key block/headers returned by the API
* Adds the info field as required in the key block post API used for mining pools.
* Set the minimum gas price to 1000000 in order to make transactions more reasonably priced.

Regarding the Sophia language and the usage of its compiler:
* Introduces a new AEVM version to contain consensus breaking changes and optimizations.
* Removes references to old planned VM versions that will not be implemented.
* Splits the old VM-version into VM-version and ABI-version. Contract calls and Oracles only deal with ABI-version. This
  changes the HTTP API. We have two VM-versions (SOPHIA_1 = Roma, and SOPHIA_2 = Minerva), but only one ABI-version.
* Adds `Crypto.ecverify` as a Primop for the aevm and in the compiler.
* Adds generic hash functions to Sophia.
* Adds bytecode instructions for bit shift (SHL, SHR, and SAR) to VM_AEVM_SOPHIA_2
* Changes AEVM semantics of arithmetic operations to fail on over/underflow.
* Replaces Sophia bit arithmetic operations with a new builtin bit field type.
* Fix Sophia Call.origin to return the original caller
* Fixes the size check applied for individual Map elements in Sophia/AEVM, previously it could give
  out of gas for not-too-big elements. Applies in VM_AEVM_SOPHIA_2.
* Deprecates all HTTP APIs that interface with the compiler. The compiler will be provided separately as a standalone "tool".
* Lock the compiler backend for (deprecated) HTTP APIs to the ROMA compiler.
* Oracle query and response formats are checked on register in the minerva protocol
* A new version for contract serialization has been added that contains the compiler name/version used to compile the contract. This serialization is only valid after Minerva hard-fork height has been reached.
* Avoid bumping nonces in for contract primops unless it is needed (from Minerva protocol)

Regarding feature refinements:
* Add debug API endpoint for getting the token supply at height
* Add api endpoint for getting the state of an account at a given height
* The utility command `./bin/aeternity keys_gen` does not compress spaces in the
    password itself anymore. Only spaces at the beginning or end of a given
    password are trimmed.
* Moves mining related code into a separate git repository - `aeternity/aeminer`. The Aeternity node uses the repository as a dependency (and other projects can use it as a dependency, too).
* Add utility command support for `./bin/aeternity UTILITY_COMMAND` on Windows.
    These commands were previously non-functional on Windows.
* Increases the maximum size of generic state channel messages to 65535 bytes.
* Add api endpoint for getting the state of an account at a block hash

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v2.0.0-rc.1

The database is backward compatible with `v1.*`.
For the rest, this release is not backward compatible with `v1.*`.

Please join the testnet by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

The instructions below describe:
* [How to retrieve the released software for running a node](#retrieve-the-software-for-running-a-node);
* [How to install a node](#install-node);
* [How to join the roma network](#join-roma-network).
* [How to join the testnet](#join-the-testnet).

## Retrieve the software for running a node

You can run a node by either:
* Installing the published [release binary][this-release] corresponding to your platform; or
* Running the published [Docker image `aeternity/aeternity`][docker]; or
* [Building a release binary from source][build].

[docker]: https://github.com/aeternity/aeternity/blob/v2.0.0-rc.1/docs/docker.md
[build]: https://github.com/aeternity/aeternity/blob/v2.0.0-rc.1/docs/build.md

The instructions for configuring the node using the Docker image are in [the dedicated separate document][docker].

The node user API is documented:
* HTTP API endpoints are specified [online in swagger.yaml][swagger-yaml];
  * A JSON version of the same specification is located in the node at path `lib/aehttp-*/priv/swagger.json` (you will need to amend the wildcard `*` placeholder in the path with the version).
  * The JSON version can be obtained from a running node using the endpoint `/api`.
  * An interactive visualization of the same specification is available [online][swagger-ui].
* WebSocket API endpoints are [specified online][api-doc];
* The intended usage of the user API (HTTP and WebSocket) is [documented online][api-doc].

[swagger-yaml]: https://github.com/aeternity/aeternity/blob/v2.0.0-rc.1/config/swagger.yaml
[swagger-ui]: https://aeternity.github.io/api-docs/?config=https://raw.githubusercontent.com/aeternity/aeternity/v2.0.0-rc.1/apps/aehttp/priv/swagger.json
[api-doc]: https://github.com/aeternity/protocol/blob/aeternity-node-v2.0.0-rc.1/node/api/README.md

## Install node

The instructions for installing a node using a release binary are in [the dedicated separate document](../../docs/installation.md).

For installation of a node using the Docker image, please refer to [its documentation online][docker].

## Join Roma network

### Roma network seed nodes

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

### Inspect Roma network

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
