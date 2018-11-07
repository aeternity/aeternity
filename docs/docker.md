# Running an epoch node on Docker

This document describes:
* [How to join the testnet using Docker](#testnet);
* [How to run a local network using Docker](#localnet).

You must have [docker installed](https://docs.docker.com/engine/installation/) on a host machine.

## Testnet

### Docker Image

Docker image is automatically build and published on [DockerHub](https://hub.docker.com/r/aeternity/epoch/). All tagged source code versions have their own docker image tag as well. Latest tagged ("stable") version is tagged with `latest`.
Master branch of the source code is tagged as `dev`.

To pull the latest "stable" image run:
```bash
docker pull aeternity/epoch
```

### Configuration

Please prepare node configuration.

The minimal configuration to join the testnet is the following (mind that `beneficiary` must be changed to valid account public key, see [configuration documentation](configuration.md) for more details):
```
---
keys:
    peer_password: "top secret"
    dir: ./keys

chain:
    persist: true

mining:
    autostart: true
    beneficiary: "encoded_beneficiary_pubkey_to_be_replaced"
```

#### Generating beneficiary account for the first time

If you don't have your beneficiary account public key yet, but you have Docker image, you can use `keys_gen` tool to generate public-private keypair from the inside of the Docker container.

Assuming:
* `/tmp/generated_keys` is a directory on you local machine;
* you change `my_password` in the command below to your password that will protect your public-private keypair,

run:
```
docker run --entrypoint=/bin/bash \
    -v /tmp/generated_keys:/home/epoch/node/generated_keys \
    aeternity/epoch \
    -c './bin/epoch keys_gen my_password'
```
Your generated keypair will be located in `/tmp/generated_keys` on your machine, and your public key to be put in the configuration file will be printed in the console.

For more details see [configuration documentation beneficiary section](configuration.md#beneficiary-account).

#### External Peer Address

Please note that, if your node is behind a firewall, you need to open and map the TCP port defined by (`sync` > `port` with default `3015`) option in your firewall to the container port.
If the publicly available port has to be different from the internal port it as to be reflected in the configuration with the (`sync` > `external_port`) option.

#### Peer addresses

Docker image has packaged the addresses of testnet seed peers in the configuration. To change the peers configuration (e.g. join other network) change the configuration file.

### Start a Node

Assuming configuration file location is `~/.aeternity/myepoch.yaml`:

To start a docker node and join the testnet run:
```bash
docker run -d --name epoch_node0 -p 3013:3013 \
    -v ~/.aeternity/myepoch.yaml:/home/epoch/myepoch.yaml \
    -e EPOCH_CONFIG=/home/epoch/myepoch.yaml \
    aeternity/epoch
```

Verify the node is running:
```bash
curl localhost:3013/v2/blocks/top
```

#### Node arguments

Arguments can also be passed to epoch node, for example to enable API debug endpoints:
```bash
docker run -d -p 3013:3013 \
    -v ~/.aeternity/myepoch.yaml:/home/epoch/myepoch.yaml \
    -e EPOCH_CONFIG=/home/epoch/myepoch.yaml \
    aeternity/epoch -aehttp enable_debug_endpoints true
```

### Stop a Node

To stop a docker node run:
```bash
docker stop epoch_node0
```

### Execute command on a running node

To execute any command on a running node use:
```bash
docker exec epoch_node0 CMD
```

For example to check what's in the epoch logs run:
```bash
docker exec epoch_node0 tail log/epoch.log
```

### Persisting Data

To persist blockchain data and node keys between container runs, use [Docker volumes](https://docs.docker.com/engine/admin/volumes/volumes/). Replace `~/.aeternity/db` with location of your choice.


```bash
docker run -d -p 3013:3013 \
    -v ~/.aeternity/myepoch.yaml:/home/epoch/myepoch.yaml \
    -e EPOCH_CONFIG=/home/epoch/myepoch.yaml \
    --hostname node0 \
    -v ~/.aeternity/db:/home/epoch/node/data/mnesia \
    -v ~/.aeternity/keys:/home/epoch/node/keys \
    aeternity/epoch
```

**Note: make sure `hostname` option is set when reusing the mnesia data directory**

## Localnet

Small local network (*not* connected to testnet) can be created with `docker-compose`.
It runs three nodes using the `mean15-generic` miner (fastest generic miner) and a proxy server to allow CORS.

All local network nodes have `ak_25eTK8PaiLpREqBkP3yDNWJAwXjWSR8tbn3zu8SXaNx824A1AJ` set as node beneficiary (for more details on beneficiary see [configuration documentation](configuration.md#beneficiary-account)).
Public-private keypair of `ak_25eTK8PaiLpREqBkP3yDNWJAwXjWSR8tbn3zu8SXaNx824A1AJ` beneficiary can be found [here](/docker/keys/beneficiary): as the private key is publicly available, this setup must *not* be connected on the live network.
Base58Check-encoded form of private key, which matches above beneficiary public key: `7eFs4YMo1bXS7Z5sFvmgb2WpJrGpoWFqVZY4mb4o99c7aAbxdTko1tV3toCbumDRpbSpSceDqTYLRnE5Utr869KhWshSo`.

Both external and internal API are exposed to the docker host, the URL pattern is as follows:
- external API - http://$DOCKER_HOST_ADDRESS:$NODE_PORT/
- internal API - http://$DOCKER_HOST_ADDRESS:$NODE_PORT/internal

Websocket API is exposed to the docker host with following URL pattern:
- channels API - ws://$DOCKER_HOST_ADDRESS:$NODE_PORT/channel

Node ports:
- `node1` - port 3001
- `node2` - port 3002
- `node3` - port 3003

For example to access `node2` peer public key, assuming docker host address is `localhost`:

```bash
curl http://localhost:3002/v2/peers/pubkey
```

To start the network:

```bash
docker-compose up -d
```

To destroy the network:

```bash
docker-compose down
```

To cleanup the associated docker volumes, `-v` option could be used:

```bash
docker-compose down -v
```

More details can be found in [`docker-compose` documentation](https://docs.docker.com/compose/reference/).

### Image Version

Docker compose uses the `aeternity/epoch:latest` image, it will be pulled from [docker hub](https://hub.docker.com/r/aeternity/epoch/) if it's not found locally.
To create a network with the source code in this repository, one should build a local image beforehand:

```bash
docker-compose build
```

### Mining Rate

By default the localnet has set default mine rate of 1 block per 15 seconds.
It can be changed by setting `EPOCH_MINE_RATE` environment variable.
The variable is in milliseconds, so to set 1 block per 10 seconds use:

```bash
EPOCH_MINE_RATE=10000 docker-compose up
```
