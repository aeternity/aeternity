# Configure an epoch node installed using a release binary

This document describes how to configure your epoch node installed using a release binary for joining a public network of nodes (e.g. testnet) knowing an initial network peer to join.

## Notable user configuration parameters

### Peer-to-peer network

In order for your node to join the testnet, you need to specify in the configuration file, how peers (on the Internet) can contact your node - specifically the TCP port (`sync` > `port` parameter).

(You do not need to specify the host at which your node can be contacted from the Internet, as each peer you ping will infer that from the address of the inbound TCP connection.)

Please notice that, if your node is behind a firewall, you need to open a TCP port in your firewall (`sync` > `port` parameter) and map that port to the one the node actually listens on (`sync` > `port` parameter - the same). If the publicly available port needs to be different from the internal one, you need to set (`sync` > `external_port`) accordingly.

The following example configuration assumes that:
* The listening TCP port on your public IP address is `3015`;
* The listening TCP port on your node is `3115`.

### Channels

`epoch` provides an infrastructure for using state channes. There are two
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

### Beneficiary account

In order to configure who receives fees from mining on a node, you must configure a beneficiary public key.

If you don't have your public key yet, you can generate a public/private key pair by using any one of the following tools:
* [AirGap wallet](https://airgap.it/).

#### Generating a beneficiary account for testing purposes only

An alternative tool `keys_gen` for generating a public-private key pair **for testing purposes only** is included in the package.

The key pair will be encrypted with a password that you shall pass to `keys_gen` tool (below assumes the node is deployed in directory `/tmp/node`).
Generated public-private key pair will be located in `/tmp/node/generated_keys`, and public key is to be put in epoch configuration file (`mining` > `beneficiary` parameter).

Do make sure you back up `/tmp/node/generated_keys` (and remember the password): if you destroy the node, you can setup a new node and use the same account (public key) as a beneficiary.
You shall not share the private key (or the password) with anyone.

e.g.

```bash
cd /tmp/node
bin/epoch keys_gen my_secret_password ## This way of generating a key-pair is only for testing purpose, use a proper wallet/mechanism for your mainnet tokens: e.g., [AirGap wallet](https://airgap.it/).
```
```
Generated keypair with encoded pubkey: ak_2D9REvQsrAgnJgmdwPq585D8YksJC8PSAA8MscQdxinbkFC7rq
```

In the example the generated public key is `ak_2D9REvQsrAgnJgmdwPq585D8YksJC8PSAA8MscQdxinbkFC7rq`, but **do not use it in your config**!
This is just an **example** value to show what public key format you should expect after running `bin/epoch keys_gen` command.

## Network ID

The release package is preconfigured with Roma network_id. Please change the configuration to interact with testnet.
The testnet (internally called UAT) has the network ID `ae_uat` - this is set in the configuration:

```yaml
fork_management:
    network_id: ae_uat
```

For Roma network the network ID defaults to `ae_mainnet`.

## Instructions

The instructions below assume that:
* The node is deployed in directory `/tmp/node`;
* You already know your `beneficiary` account public key (if you don't, see [Beneficiary account section](#beneficiary-account));
* No custom peers are specified under the `peers:` key in the config. If the `peers:` key is undefined, the *testnet* seed peers (built-in in the package source) are used.

If any of the assumptions does not hold, you need to amend the instructions accordingly.

Create the file `/tmp/node/epoch.yaml` with the below content.
Make sure you amend:
* the `mining` > `beneficiary` parameter, i.e. replace `encoded_beneficiary_pubkey_to_be_replaced` with your public key;
* the `sync` > `port` parameter with your actual value:
* set `autostart: false` if you have not yet synced with the blockchain to improve sync performance. Change this value to `autostart: true` and [configure your miner](#Miner-Configuration) when you are in sync, then restart the node.

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

fork_management:
    network_id: ae_mainnet

```

The node automatically creates the directory `db_path`, for storing the blockchain, if not present.

Note that YAML files have significant whitespace so make sure that you indent the file correctly and that the file ends with a newline.

You can validate the configuration file before starting the node:
```bash
cd /tmp/node
bin/epoch check_config epoch.yaml
```
You shall read output like the following:
```
OK
```
If the file is valid YAML but does not contain a valid configuration, it prints a helpful output.

## Miner Configuration

If you want to use your node to mine, you can use the default mining by setting
```yaml
mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    autostart: true
```
in the yaml.epoch configuration file.

Your mining setup needs to meet your hardware capacity. Therefore, you need to make a choice in how you want to configure your miner. You can read the documentation on setting up CUDA mining, or you can use all but one of the cores on your computer to mine (keep one core available for transaction gossiping, synchronising, etc).
If you have 16 cores, you could (loosely spoken) assign 14 of them to mining using the following configuration:
```yaml
mining:
    beneficiary: "beneficiary_pubkey_to_be_replaced"
    cuckoo:
        miner:
            edge_bits: 29
            executable: mean29-avx2
            extra_args: -t 14
```
Read the [beneficiary account section](#beneficiary-account) on how to put your key in this file.
