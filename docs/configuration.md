# Configure an epoch node installed using a release binary

This document describes how to configure your epoch node installed using a release binary for joining a public network of nodes (e.g. testnet) knowing an initial network peer to join.

## Notable user configuration parameters

### Peer-to-peer network

In order for your node to join the testnet, you need to specify in the configuration file:
* The initial network peers to join (`peers` parameter);
* How peers (on the Internet) can contact your node (`http` > `external` > `peer_address` parameter).

Please notice that, if your node is behind a firewall, you need to open a TCP port in your firewall (`http` > `external` > `peer_address` parameter) and map that port to the one the node actually listens on (`http` > `external` > `port` parameter).
The two port numbers can be distinct.

The following example configuration assumes that:
* Your public IP address is `1.2.3.4`;
* The listening TCP port on that public IP address is `8080`;
* The listening TCP port on your node is `3003`.

### Keys management

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

## Instructions

The instructions below assume that:
* The node is deployed in directory `/tmp/node`;
* The initial network peers to join are located at "31.13.249.1",
"31.13.248.97", and "31.13.249.118". These nodes have the public keys
(Base58Check encoded) "pp$2eDAWTgveKp1C4dWhy9Hg59NCrg8TPUCKSXeEgvnPdro4ra177",
"pp$CjHH611sKocFxvrXrWjGJq5nNmbAxUYGhcyNbmvg6CwGEii2p" and
"pp$2Y6u5bx6pfVAx9B4faBMG1BV7WGGwzf3hvnXkV5MDZGuDGipfy"; and they all listen to
the standard sync port 3015.

If any of the assumptions does not hold, you need to amend the instructions accordingly.

Create the file `/tmp/node/epoch.yaml` with the following content (amend the `http` > `external` > `peer_address` parameter and `http` > `external` > `port` parameter with your actual values):
```yaml
---
peers:
    - peer:
        host: "31.13.249.1"
        port: 3015
        pubkey: "pp$2eDAWTgveKp1C4dWhy9Hg59NCrg8TPUCKSXeEgvnPdro4ra177"
    - peer:
        host: "31.13.248.97"
        port: 3015
        pubkey: "pp$CjHH611sKocFxvrXrWjGJq5nNmbAxUYGhcyNbmvg6CwGEii2p"
    - peer:
        host: "31.13.249.118"
        port: 3015
        pubkey: "pp$2Y6u5bx6pfVAx9B4faBMG1BV7WGGwzf3hvnXkV5MDZGuDGipfy"

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

The node automatically creates the directory `db_path`, for storing the blockchain, if not present.

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
