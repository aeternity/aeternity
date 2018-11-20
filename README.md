# Aeternity Epoch

A new blockchain for æpps.

Optimized for scalability via smart contracts inside state-channels.

Has a build-in oracle for integration with real-world data.

Comes with a naming system, for developerability.

Written in Erlang.

Please see the latest [release notes][release-notes] if you want to run the
software or just
[follow the progress of the project in Pivotal Tracker](https://www.pivotaltracker.com/n/projects/2124891).

If you are interested in participating in a security bounty, check our
[HackerOne Aeternity Bounty Program](https://hackerone.com/aeternity/).

[release-notes]: https://github.com/aeternity/epoch/tree/master/docs/release-notes

## What's on board?

We keep our protocol, APIs and research spec in separate
[protocol](https://github.com/aeternity/protocol) repository
* [Overview of the APIs](https://github.com/aeternity/protocol/blob/master/epoch/api/README.md#overview)
* [Intended usage of the user API](https://github.com/aeternity/protocol/blob/master/epoch/api/README.md#user-api---intended-usage)

## Installation

### Install an epoch node using a release binary

In order to install an epoch node using a release binary, you need to:
* Retrieve the release binary corresponding to your platform;
* Install the required dependencies;
* Deploy the node.

#### Retrieve release binary

The release binaries are published on
[GitHub](https://github.com/aeternity/epoch/releases)
and are tested on the following platforms:
* Ubuntu 16.04.3 LTS (x86-64);
* Ubuntu 18.04 LTS (x86-64);
* macOS High Sierra 10.13 (x86-64);
* macOS Mojave 10.14 (x86-64).

Each release comes with the [release notes][release-notes].

#### Install dependencies

##### Ubuntu package

The package requires a libsodium v1.0.16 as `libsodium.so.23` shared
object/library.

###### Ubuntu 18.04

Ubuntu 18.04 ships with libsodium 1.0.16, thus it can be installed with `apt`
package manager:

```bash
sudo apt-get install libsodium23
```

The Ubuntu release binaries are built with `libssl1.0.0` (default Ubuntu 18.04
version is 1.1) requirement that can be installed with:

```bash
sudo apt-get install libssl1.0.0
```

###### Ubuntu 16.04

As Ubuntu 16.04 ships with older libsodium version than required, it must be
installed from source. A C compiler and related tools must be installed
beforehand by running:

```bash
sudo apt-get install build-essential
```

then the library:

```bash
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.16.tar.gz
tar -xf libsodium-1.0.16.tar.gz && cd libsodium-1.0.16
./configure && make && sudo make install && sudo ldconfig
```

##### macOS package

The macOS package has:
* A hard dependency on OpenSSL v1.0.0 installed with
  [Homebrew](https://brew.sh/) in its default path
  `/usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib`;
* A hard dependency on libsodium v1.0.16 installed with
  [Homebrew](https://brew.sh/) in its default path
  `/usr/local/opt/libsodium/lib/libsodium.23.dylib`.

In case you have installed either of them in a non-default path, you could use
symlink(s) to work around the issue.

#### Deploy node

In the instructions below, the node is deployed in directory `/tmp/node`: you
may prefer to deploy the node in an alternative (and less ephemeral)
location - e.g. a `node` directory inside your home directory - by amending
the instructions accordingly.
It is recommended that the partition where the node directory is has at least
10 GB free: this is needed for the chain and the log files.

Open a Terminal window or get to the command line.

Create a directory and unpack the downloaded package (you may need to amend
the directory and/or file name of the package):
```bash
mkdir /tmp/node
cd /tmp/node
tar xf ~/Downloads/epoch-1.0.0-rc2-osx-10.13.6.tar.gz
```

See [configuration](#configuration) and [operation](#operation) documentation
for further instruction on running a node.

### Build and install epoch node from source

#### Ubuntu

The following instructions are for Ubuntu 16.04.4 LTS or Ubuntu 18.04 LTS.
The commands below assume you are logged in with `sudo` user.

##### Common tools and libraries

Make sure your Ubuntu version and it's packages are up to date, then install
required tools and libraries:
```bash
sudo apt-get -qq update \
&& sudo apt-get -y upgrade \
&& sudo apt-get -qq -y install git curl autoconf build-essential ncurses-dev libssl-dev
```

##### OTP installation

Required Erlang OTP version is `20.1`.

###### Ubuntu 18.04

```bash
sudo apt-get install erlang
```

###### Ubuntu 16.04

Ubuntu 16.04 ships with outdated erlang version. Version 20 can be installed
from source:

```bash
OTP_VERSION="20.2.3"
OTP_DOWNLOAD_URL="https://github.com/erlang/otp/archive/OTP-${OTP_VERSION}.tar.gz"
curl -fsSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL" \
&& mkdir otp-src \
&& tar -zxf otp-src.tar.gz -C otp-src --strip-components=1 \
&& cd otp-src \
&& export ERL_TOP=`pwd` \
&& ./otp_build autoconf && ./configure && make -j$(nproc) && sudo make install \
&& cd ..
```

##### Libsodium install

Required Libsodium version is `1.0.16`.

###### Ubuntu 18.04

Since Ubuntu 18.04 ships with libsodium version 1.0.16 it can be installed
from apt package:

```bash
sudo apt-get install libsodium-dev
```

###### Ubuntu 16.04

Ubuntu 16.04 ships with older than required version of libsodium thus it must
be installed from source running below commands:

```bash
LIBSODIUM_VERSION="1.0.16"
LIBSODIUM_DOWNLOAD_URL="https://github.com/jedisct1/libsodium/releases/download/${LIBSODIUM_VERSION}/libsodium-${LIBSODIUM_VERSION}.tar.gz"
curl -fsSL -o libsodium-src.tar.gz "$LIBSODIUM_DOWNLOAD_URL" \
&& mkdir libsodium-src \
&& tar -zxf libsodium-src.tar.gz -C libsodium-src --strip-components=1 \
&& cd libsodium-src \
&& ./configure && make -j$(nproc) \
&& sudo make install \
&& sudo ldconfig \
&& cd ..
```

For more details read the
[dedicated Libsodium documentation](https://download.libsodium.org/doc/installation/).

##### Builds

Epoch source code can be obtained by cloning the public
[GitHub repository](https://github.com/aeternity/epoch):

```bash
git clone https://github.com/aeternity/epoch.git epoch && cd epoch
```

Identify the version to be built:
```
VERSION=1.0.0-rc2
```

Checkout the version to be built:

```bash
git checkout tags/v${VERSION:?}
```

###### Production build

One can create a production build by running:
```bash
make prod-build
```

Make sure beneficiary account is set in configuration, as this is mandatory to
successfully start a node. There is no default beneficiary configured.

See [configuration documentation](#configuration) for configuration details.

If `prod-build` went fine, configuration is in place, one should be able to
navigate to the build artifacts directory and start the epoch node:
```bash
cd _build/prod/rel/epoch/
bin/epoch start
```

See [operation documentation](#operation) for more details.

###### Production package

Alternatively a production package similar to what is distributed via
[GitHub releases](https://github.com/aeternity/epoch/releases) can be created
by running:

```bash
make prod-package
```

Once the packaging is done, the package is created in the
`_build/prod/rel/epoch/` directory, e.g.
`_build/prod/rel/epoch/epoch-${VERSION:?}.tar.gz`.

To deploy the package for example in `/tmp/node` one should just unarchive it
to that directory:

```bash
mkdir /tmp/node
tar xf _build/prod/rel/epoch/epoch-${VERSION:?}.tar.gz -C /tmp/node
```

Make sure beneficiary account is set in configuration, as this is mandatory
to successfully start a node. There is no default beneficiary configured.

See [configuration documentation](#configuration) for configuration details.

Then start the node:
```bash
/tmp/node/bin/epoch start
```

See [operation documentation](#operation) for more details.

###### CUDA Miner

There is a
[separate document](https://github.com/aeternity/epoch/tree/master/docs/cuda-miner.md)
that describes how to install CUDA miner.

#### Windows

Windows build and installation is described
[here](https://github.com/aeternity/epoch/tree/master/docs/installation-windows.md).

## Configuration

This is a description how to configure your epoch node installed using
a release binary for joining a public network of nodes (e.g. testnet) knowing
an initial network peer to join.

### Notable user configuration parameters

#### Peer-to-peer network

In order for your node to join the testnet, you need to specify in the
configuration file, how peers (on the Internet) can contact your node -
specifically the TCP port (`sync` > `port` parameter).

(You do not need to specify the host at which your node can be contacted from
the Internet, as each peer you ping will infer that from the address of the
inbound TCP connection.)

Please notice that, if your node is behind a firewall, you need to open a TCP
port in your firewall (`sync` > `port` parameter) and map that port to the
one the node actually listens on (`sync` > `port` parameter - the same). If
the publicly available port needs to be different from the internal one, you
need to set (`sync` > `external_port`) accordingly.

The following example configuration assumes that:
* The listening TCP port on your public IP address is `3015`;
* The listening TCP port on your node is `3115`.

#### Channels

`epoch` provides an infrastructure for using state channes. There are two
distinct protocols involved:
* WebSocket client one
* Noise encoded one

The later is not described here.

##### Channels' WebSocket client setup

In order to connect as a WebSocket client, one must set up a port and a host
the service is to listen at. This is a private node setting that is left for
the node operator to secure its access. The TCP port can be set using
`websocket` > `channel` > `port` parameter. The address the service is to be
listening can be set using the `websocket` > `channel` > `listen_address`
parameter. Note that this address has a default value of `127.0.0.1` and thus
the WebSocket endpoint is not exposed.

#### Beneficiary account

In order to configure who receives fees from mining on a node, you must
configure a beneficiary public key.

If you don't have your public key yet, you can use provided `keys_gen` tool,
that will generate a public-private key pair for you.
The key pair will be encrypted with a password that you shall pass to
`keys_gen` tool (below assumes the node is deployed in directory `/tmp/node`):


```bash
cd /tmp/node
bin/epoch keys_gen PASSWORD
```
The output should be:
```
Generated keypair with encoded pubkey: YOUR_PUBLIC_KEY
```

Generated public-private key pair will be located in
`/tmp/node/generated_keys`, and public key is to be put in epoch configuration
file (`mining` > `beneficiary` parameter).

Do make sure you back up `/tmp/node/generated_keys` (and remember the
password): if you destroy the node, you can setup a new node and use the same
account (public key) as a beneficiary.
You shall not share the private key (or the password) with anyone.

e.g.

```bash
cd /tmp/node
bin/epoch keys_gen my_secret_password
```
```
Generated keypair with encoded pubkey: ak_2D9REvQsrAgnJgmdwPq585D8YksJC8PSAA8MscQdxinbkFC7rq
```

In the example the generated public key is
`ak_2D9REvQsrAgnJgmdwPq585D8YksJC8PSAA8MscQdxinbkFC7rq`, but do not use it in
your config!
This is just an example value to show what public key format you should expect
after running `bin/epoch keys_gen` command.

### Instructions

The instructions below assume that:
* The node is deployed in directory `/tmp/node`;
* You already know your `beneficiary` account public key (if you don't,
  see [Beneficiary account section](#beneficiary-account));
* No custom peers are specified under the `peers:` key in the config. If the
  `peers:` key is undefined, the *testnet* seed peers (built-in in the package
  source) are used.

If any of the assumptions does not hold, you need to amend the instructions
accordingly.

Create the file `/tmp/node/epoch.yaml` with the below content.
Make sure you amend:
* the `mining` > `beneficiary` parameter, i.e. replace
  `encoded_beneficiary_pubkey_to_be_replaced` with your public key;
* the `sync` > `port` parameter with your actual value:
* set `autostart: false` if you have not yet synced with the blockchain to
  improve sync performance. Change this value to `autostart: true` and
  [configure your miner](#miner-configuration) when you are in sync, then
  restart the node.

```yaml
---
sync:
    port: 3115
    external_port: 3015

keys:
    dir: keys
    peer_password: "secret"

http:
    external:
        port: 3013
    internal:
        port: 3113

websocket:
    channel:
        port: 3014

mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    autostart: true

chain:
    persist: true
    db_path: ./my_db
```

The node automatically creates the directory `db_path`, for storing the
blockchain, if not present.

Note that YAML files have significant whitespace so make sure that you indent
the file correctly and that the file ends with a newline.

You can validate the configuration file before starting the node:
```bash
cd /tmp/node
bin/epoch check_config epoch.yaml
```
You shall read output like the following:
```
OK
```
If the file is valid YAML but does not contain a valid configuration, it
prints a helpful output.

### Miner Configuration

If you want to use your node to mine, you can use the default mining by
setting
```yaml
mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    autostart: true
```
in the yaml.epoch configuration file.

Your mining setup needs to meet your hardware capacity. Therefore, you need to
make a choice in how you want to configure your miner. You can read the
[documentation on setting up CUDA mining](https://github.com/aeternity/epoch/blob/master/docs/cuda-miner.md),
or you can use all but one of the cores on your computer to mine (keep one
core available for transaction gossiping, synchronising, etc). If you have
16 cores, you could (loosely spoken) assign 14 of them to mining using the
following configuration:
```yaml
mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    cuckoo:
        miner:
            edge_bits: 29
            executable: mean29-avx2
            extra_args: -t 14
```
Read the [beneficiary account section](#beneficiary-account) on how to put
your key in this file.

## Operation

This is a description how to start your epoch node installed using a release
binary, verify that it mines and verify that it joined the configured public
network of nodes (e.g. testnet).

### Assumptions

The instructions below assume that:
* The node is deployed in directory `/tmp/node`;
* beneficiary account is set under `mining` > `beneficiary` in the config
  (see [configuration documentation](#configuration));
* No custom peers are specified under the `peers:` key in the config. If the
  `peers:` key is undefined, the *testnet* seed peers (built-in in the package
  source) are used.
* The external HTTP endpoint of the user API of the node can be contacted at
  127.0.0.1 port 3013.

If any of the assumptions does not hold, you need to amend the instructions
accordingly.

### Instructions

#### Start node

It is recommended that the node has at least 4 GB of memory available.

When it starts, the node checks the maximum number of open files (`ulimit -n`)
and warns if below the recommended limit: proper max number of open files is
essential to managing network connections and you should make sure you
configure it in the session where you start the node.

Start the node:
```bash
cd /tmp/node
bin/epoch start
```

(You can stop the node by running `bin/epoch stop` from the same directory.)

Verify the node is up, by inspecting the current top of the blockchain as seen
by the node:
```bash
curl http://127.0.0.1:3013/v2/blocks/top
```

If the node is unresponsive, inspect the `log` directory for errors.

Back up the peer key pair:
```bash
cp -pr /tmp/node/keys ~/my_epoch_keys
```

#### Verify that node mines

Inspect the mining log file of the node:
```bash
less /tmp/node/log/epoch_mining.log
```

If the node is mining, you shall read log entries like the following:
```
... Creating key block candidate on the top
... Created key block candidate ...
... Starting mining
... Starting mining
```

If the node successfully mines a block, you shall read log entries like the
following:
```
... Block mined: Height = 1; Hash = ...
```

#### Verify that node connected to the testnet

Verify that your node sees the same longest blockchain as the testnet.

Inspect the current top of the blockchain as seen by the testnet:
```bash
curl http://52.10.46.160:3013/v2/blocks/top || curl http://18.195.109.60:3013/v2/blocks/top
```

Inspect the current top of the blockchain as seen by your node:
```bash
curl http://127.0.0.1:3013/v2/blocks/top
```

Verify that the height is the same; it may take a few minutes for your node to
catch up with the testnet blockchain.

#### Verify that node mines on same chain as the testnet

After the node is successfully connected to the testnet, you could verify that
it is mining on the same chain as the rest of the network.
You can validate it observing the `hash` of the `/blocks/top` of the remote
nodes:
```bash
$ curl http://52.10.46.160:3013/v2/blocks/top
{"key_block":{"hash":"kh_2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy","height":...}}
```

This is the hash of the block being at the top of the chain of the node and it
should be same as the hash in `prev_hash` of the block you're currently
mining:
```bash
$ curl http://127.0.0.1:3013/v2/key-blocks/pending
{"key_block":{...,"height":... ,"prev_hash":"kh_2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy", ...}}
```
Height would be +1 of what is in the `/blocks/top` of the remote node but this
is not as strong guarantee as the `prev_hash`.

## Additional resources

* [Threat Model](https://github.com/aeternity/aetmodel/blob/master/ThreatModel.md)
* [Running epoch node in docker](https://github.com/aeternity/epoch/tree/master/docs/docker.md)
