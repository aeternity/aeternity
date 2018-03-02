# About this release

[This release](https://github.com/aeternity/epoch/releases/tag/v0.9.0) is focused on TODOFILLMEIN.
It:
* Does this TODOFILLMEIN. This impacts consensus;
* Does that TODOFILLMEIN. This impacts the persisted DB;
* Does that TODOFILLMEIN.

This release introduces backward incompatible changes in the chain format.
After upgrading your node, you will not have your previous balance (even if you keep your key pair).

Please join the testnet by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/epoch/wiki/Troubleshooting).

The instructions below describe:
* [How to retrieve the released software for running a node](#retrieve-the-software-for-running-a-node);
* [How to join the testnet](#join-the-testnet).

## Retrieve the software for running a node

Download the [release binary](https://github.com/aeternity/epoch/releases/tag/v0.9.0) corresponding to your platform, e.g. `epoch-0.9.0-osx-10.12.6.tar.gz`; you would normally find the downloaded package in `~/Downloads` on macOS.

The binaries are tested on the following platforms:
* Ubuntu 16.04.3 LTS (x86-64);
* macOS Sierra (x86-64);
* macOS High Sierra (x86-64).

The macOS package has a hard dependency on OpenSSL v1.0.0 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib` and also a hard
dependency on libsodium v1.0.16 installed with [Homebrew](https://brew.sh/) also in its default path `/usr/local/opt/libsodium/lib/libsodium.23.dylib`.
In case you have installed either of them in a non-default path, you could use symlink(s) to work around the issue.

To run on Ubuntu 16.04 you need to have a libsodium shared library (v1.0.16) in `/usr/local/lib/libsodium.so.23`. (`wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.16.tar.gz`,
unpack, then do `./configure && make && sudo make install && sudo ldconfig`)

Alternatively to the release binaries, you can use the published `aetrnty/epoch` Docker image by consulting its [documentation](https://github.com/aeternity/epoch/blob/v0.9.0/docs/docker.md).

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration) though the instructions below contain easy-to-use examples.

HTTP API endpoints are specified in the [swagger.yaml](https://github.com/aeternity/epoch/blob/v0.9.0/config/swagger.yaml); a swagger.json version of the same specification is present in the release binary at path `lib/aehttp-0.1.0/priv/swagger.json`, and its interactive visualization is available [online](https://aeternity.github.io/epoch-api-docs/?config=https://raw.githubusercontent.com/aeternity/epoch/v0.9.0/apps/aehttp/priv/swagger.json).
WebSocket API endpoints are [specified](https://github.com/aeternity/protocol/blob/epoch-v0.9.0/epoch/api/README.md).

The intended usage of the API (HTTP and WebSocket) of the node is [documented](https://github.com/aeternity/protocol/blob/epoch-v0.9.0/epoch/api/README.md).

## Join the testnet

This section describes how to run a node as part of the testnet - the public test network of nodes - by using the release binary.

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet.

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://31.13.249.0:3013/v2/top
* http://31.13.249.1:3013/v2/top
* http://31.13.249.120:3013/v2/top

### Setup your node

#### Deploy node

In the instructions below, the node is deployed in directory `/tmp/node`: you may prefer to deploy the node in an alternative (and less ephemeral) location - e.g. a `node` directory inside your home directory - by amending the instructions accordingly.
It is recommended that the partition where the node directory is has at least 10 GB free: this is needed for the chain and the log files.

Open a Terminal window or get to the command line.

Create a directory and unpack the downloaded package:
```
mkdir /tmp/node
cd /tmp/node
tar xf ~/Downloads/epoch-0.9.0-osx-10.12.6.tar.gz
```

#### Configure node

In order for your node to join the testnet, you need to specify in the configuration file:
* The initial network peers to join (`peers` parameter);
* How peers (on the Internet) can contact your node (`http` > `external` > `peer_address` parameter).

Please notice that, if your node is behind a firewall, you need to open a TCP port in your firewall (`http` > `external` > `peer_address` parameter) and map that port to the one the node actually listens on (`http` > `external` > `port` parameter).
The two port numbers can be distinct.

The following example configuration assumes that:
* Your public IP address is `1.2.3.4`;
* The listening TCP port on that public IP address is `8080`;
* The listening TCP port on your node is `3003`.

In order for your node to manage the correct account (able to hold tokens on the blockchain), you need to specify in the configuration file the location of your public-private key pair.
The storage of the key pair by the node is basic:
* Each node handles one key pair;
* The key pair is stored on disk;
* The location of the key pair is configurable (`keys` > `dir` parameter);
* The key pair is encrypted with a configurable password stored in clear in the configuration file (`keys` > `password` parameter);
* A fresh key pair is generated if none is found in the configured location.

You do not need to create a key pair yourself: the node will generate one (`keys` > `dir` parameter) in the configured location if none found there.
After the node generates the key pair in the configured location, you should back up that directory (and remember the password): if you destroy the node, you can setup a new node with the same account in order not to lose the tokens you had obtained by mining on the chain.
You shall not share the private key (or the password) with anyone.

Create the file `/tmp/node/epoch.yaml` with the following content (amend the `http` > `external` > `peer_address` parameter and `http` > `external` > `port` parameter with your actual values):
```yaml
---
peers:
    - "http://31.13.249.1:3013/"

keys:
    dir: keys
    password: "secret"

http:
    external:
        peer_address: http://1.2.3.4:8080/
        port: 3003
    internal:
        port: 3103

websocket:
    internal:
        port: 3104

mining:
    autostart: true

chain:
    persist: true
    db_path: ./my_db
```

(The node automatically creates the directory `db_path` for storing the blockchain.)

As of release "v0.9.0", as the chain format changed from the previous release, please ensure that you do not reuse a persisted blockchain produced by the previous releases "v0.8.x".

You can validate the configuration file before starting the node:
```
cd /tmp/node
bin/epoch check_config epoch.yaml
```
You shall read output like the following:
```
OK
```
If the file is valid YAML but does not contain a valid configuration, it prints a helpful output.

#### Start node

It is recommended that the node has at least 4 GB of memory available.

When it starts, the node checks the maximum number of open files (`ulimit -n`) and warns if below the recommended limit: proper max number of open files is essential to managing network connections and you should make sure you configure it in the session where you start the node.

Start the node:
```
cd /tmp/node
bin/epoch start
```

(You can stop the node by running `bin/epoch stop` from the same directory.)

Verify the node is up, by inspecting the current top of the blockchain as seen by the node:
```
curl http://127.0.0.1:3003/v2/top
```

If the node is unresponsive, inspect the `log` directory for errors.

Back up the key pair:
```
cp -pr /tmp/node/keys ~/my_epoch_keys
```

#### Verify that node mines

Inspect the mining log file of the node:
```
less /tmp/node/log/epoch_mining.log
```

If the node is mining, you shall read log entries like the following:
```
... Creating block candidate
... Created block candidate and nonce ...
... Starting mining
... Starting mining
```

If the node successfully mines a block, you shall read log entries like the following:
```
... Block mined: Height = 1; Hash = ...
```

#### Verify that node connected to the testnet

Verify that your node sees the same longest blockchain as the testnet.

Inspect the current top of the blockchain as seen by the testnet:
```
curl http://31.13.249.1:3013/v2/top
```

Inspect the current top of the blockchain as seen by your node:
```
curl http://127.0.0.1:3003/v2/top
```

Verify that the height is the same; it may take a few minutes for your node to catch up with the testnet blockchain.

#### Verify that node mines on same chain as the testnet

After the node is successfully connected to the testnet, you could verify that it is mining on the same chain as the rest of the network.
You can validate it observing the `hash` of the `/top` of the remote nodes:
```
curl http://31.13.249.1:3013/v2/top
{"hash":"bh$2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy","height":...}
```

This is the hash of the block being at the top of the chain of the node and it should be same as the hash in `prev_hash` of the block you're currently mining:
```
curl http://localhost:3103/v2/block/pending
{...,"height":... ,"prev_hash":"bh$2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy", ...}
```
Height would be +1 of what is in the `/top` of the remote node but this is not
as strong guarantee as the `prev_hash`.
