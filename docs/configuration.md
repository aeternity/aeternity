# Configure an Aeternity node installed using a release binary

This document describes how to configure your Aeternity node installed using a release binary for joining a public network of nodes (e.g. testnet) knowing an initial network peer to join.

## User-provided configuration

The `aeternity` system supports user-provided parameters via a JSON- or YAML-formatted config file.

### File name and location
The format of the config file is determined from the file extension: `.json` for JSON, or `.yaml` for YAML.

The location of the file can be specified in a few different ways, in order of priority:
1. The OS environment variable `AETERNITY_CONFIG` contains a filename
2. The Erlang/OTP environment variable `-aecore config` contains a filename
3. A file named `aeternity.{yaml,json}` exists in `${HOME}/.aeternity/aeternity/`
4. A file named `aeternity.{yaml,json}` exists in `${AETERNITY_TOP}/`

If all above checks fail, no user configuration is applied.

### Validation
The contents of the config file will be validated against a JSON-Schema, located in the node at path `data/aeternity_config_schema.json`. If any parameters violate the schema, the node will fail to start.

## Notable user configuration parameters

### Peer-to-peer network

**It is very important that a node not only can connect to other nodes, but that can accept incoming connections as well: the more peer connections (both inbound+outbound) the node has, the better its overall p2p network latency (i.e. block propagation time) will be.**

By default node listen on TCP port **3015**. It can be changed by `sync` > `port` parameter in the configuration file in case for some reason that port cannot be used (e.g. already used by other service).

#### Firewalls

Example setup: `node (firewall) <-> firewall <-> Internet`

If the node is behind a firewall that port  (default 3015 or the one set in the configuration) should be opened for inbound TCP connections.


Note that the port may need to be opened both on the host machine running the node and any external device (firewall/router) that route the network traffic.
Unfortunately all firewall configurations are different and common steps cannot be provided here, one should follow the documentation of their equipment/software "how to open firewall port".

#### NAT

Example setup: `node <-> NAT (router) <-> Internet`

If the node is behind NAT (e.g. home router) the port should be forwarded on that device to accept incoming connections from Internet and route it to the node.

Unfortunately all router configurations are different and common steps cannot be provided here, one should follow the documentation of their equipment/software "how to forward ports".

##### Advanced NAT

This is advanced configuration and should be used with caution because it can cause node misconfiguration and bad p2p connectivity.
In case the sync port (3015 by default) cannot be used as external forwarding port the `sync` > `external_port` configuration parameter can be used to change it. This is the port that the node will advertise to the network to be reached over Internet.

Example scheme: `node (sync > port) <-> router (sync > external_port) <-> Internet`

#### UPnP/NAT-PMP

If you don't have port forwarding configured in your router, but your router supports UPnP or NAT-PMP, the node provides UPnP/NAT-PMP service to streamline network configuration.

In order to start UPnP/NAT-PMP service:
* make sure UPnP/NAT-PMP is enabled on your router;
* in your user configuration file, set `sync` > `upnp_enabled` parameter to `true`.

Then, the node will automatically create appropriate port mapping based on the configuration params.

#### Port Check

After you have started the node, you can verify the validity of your setup and configuration correctness by, for example, running the external node port check (assuming the default port 3015):

```bash
nc -zv $(curl -s https://api.ipify.org) 3015
```

Example output:
```
Connection to 13.53.161.215 3015 port [tcp/*] succeeded!
```

Where the IP address should be the external IP address of the node under test (it's one of the seed nodes in this example).

### Channels

The node provides an infrastructure for using state channes. There are two
distinct protocols involved:
* WebSocket client one
* Noise encoded one

The later is not subject to this document.

#### Channels' WebSocket client setup

In order to connect as a WebSocket client, one must set up a port and a host
the service is to listen at. This is a private node setting that is left for
the node operator to secure its access. The TCP port can be set using
`websocket` > `channel` > `port` parameter. The address the service is to be
listening can be set using the `websocket` > `channel` > `listen_address`
parameter. Note that this address has a default value of `127.0.0.1` and thus
the WebSocket endpoint is not exposed.

## Network ID

The release package is preconfigured with mainnet network_id. Please change the configuration to interact with testnet.
The testnet (internally called UAT) has the network ID `ae_uat` - this is set in the configuration:

```yaml
fork_management:
    network_id: ae_uat
```
For mainnet network the network ID defaults to `ae_mainnet`.

## Block reward initiative

The new schema of managing block reward can be tuned with following configuration parameters.
Mainnet values are included in the base code, however for testnet use below:

```yaml
chain:
    protocol_beneficiaries_enabled: true
    protocol_beneficiaries: ["ak_2A3PZPfMC2X7ZVy4qGXz2xh2Lbh79Q4UvZ5fdH7QVFocEgcKzU:109"]
```



## Instructions

The instructions below assume that:

* The node is deployed in directory `~/aeternity/node`;
* No custom peers are specified under the `peers` key in the config. If the `peers` key is undefined, the *mainnet* seed peers (built-in in the package source) are used.

If any of the assumptions does not hold, you need to amend the instructions accordingly.

Create the config file with the below content.
Place the config file in one of the locations specified in the [File name and location section](#file-name-and-location).
Make sure you amend the `sync` > `port` parameter with your actual value.

```yaml
---
sync:
    port: 3015

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
    autostart: false

chain:
    persist: true
    db_path: ./my_db

fork_management:
    network_id: ae_mainnet

```

The node automatically creates the directory `db_path`, for storing the blockchain, if not present.

The above sample config has `mining` > `autostart` set to `false`, so mining will not start automatically.
To configure your node to mine, go to the [Miner configuration section](#miner-configuration).

Note that YAML files have significant whitespace so make sure that you indent the file correctly and that the file ends with a newline.

You can validate the configuration file before starting the node:
```bash
cd ~/aeternity/node
bin/aeternity check_config aeternity.yaml
```
You shall read output like the following:
```
OK
```
If the file is valid YAML but does not contain a valid configuration, it prints a helpful output.

## Miner configuration

The instructions below assume that you already know your `beneficiary` account public key (if you don't, see [Beneficiary account section](#beneficiary-account)).

If you want to use your node to mine, you can use the default mining by setting
```yaml
mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    autostart: true
```
in the aeternity.yaml configuration file.

Make sure you replace `mining` > `beneficiary` parameter with your public key!

Note that in order to improve sync performance, before configuring your miner, you should start a node with `autostart: false`.
Change this value to `autostart: true` when you are in sync, then restart the node.

Your mining setup needs to meet your hardware capacity. Therefore, you need to make a choice in how you want to configure your miner. You can read the documentation on setting up CUDA mining, or you can use all but one of the cores on your computer to mine (keep one core available for transaction gossiping, synchronising, etc).
If you have 16 cores, you could (loosely spoken) assign 14 of them to mining using the following configuration:
```yaml
mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    cuckoo:
        edge_bits: 29
        miners:
            - executable: mean29-avx2
              extra_args: -t 14
```

#### Combining different miners

Your mining setup may also contain multiple miners, which will be run simultaneously by your node.
For example, to combine CPU miner with CUDA miner the following configuration can be used:
```yaml
mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    cuckoo:
        edge_bits: 29
        miners:
            - executable: mean29-generic
              extra_args: -t 2
            - executable_group: aecuckooprebuilt
              executable: cuda29
              extra_args: -t 1
              hex_encoded_header: true
```

For more details on CUDA mining go to [dedicated CUDA miner documentation](cuda-miner.md).

## Beneficiary account

In order to configure who receives fees from mining on a node, you must configure a beneficiary public key.

If you don't have your public key yet, you can generate a public/private key pair by using any one of the following tools:
* [AirGap wallet](https://airgap.it/).

#### Generating a beneficiary account for testing purposes only

An alternative tool `keys_gen` for generating a public-private key pair **for testing purposes only** is included in the package.

The key pair will be encrypted with a password that you shall pass to `keys_gen` tool (below assumes the node is deployed in directory `~/aeternity/node`).
Generated public-private key pair will be located in `~/aeternity/node/generated_keys`, and public key is to be put in the configuration file (`mining` > `beneficiary` parameter).

Do make sure you back up `~/aeternity/node/generated_keys` (and remember the password): if you destroy the node, you can setup a new node and use the same account (public key) as a beneficiary.
You shall not share the private key (or the password) with anyone.

e.g.

```bash
cd ~/aeternity/node
bin/aeternity keys_gen my_secret_password ## This way of generating a key-pair is only for testing purpose, use a proper wallet/mechanism for your mainnet tokens: e.g., [AirGap wallet](https://airgap.it/).
```
```
Generated keypair with encoded pubkey: ak_2D9REvQsrAgnJgmdwPq585D8YksJC8PSAA8MscQdxinbkFC7rq
```

In the example the generated public key is `ak_2D9REvQsrAgnJgmdwPq585D8YksJC8PSAA8MscQdxinbkFC7rq`, but **do not use it in your config**!
This is just an **example** value to show what public key format you should expect after running `bin/aeternity keys_gen` command.
