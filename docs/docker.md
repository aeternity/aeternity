# Introduction

This document describes:

* [How to join the mainnet using Docker](#mainnet);
* [How to use the Docker image](#docker-image);
* [How to join the testnet using Docker](#testnet);
* [How to run a local network using Docker](#localnet).

You must have [docker installed](https://docs.docker.com/engine/installation/) on a host machine and **be familiar [how to operate docker containers](https://docs.docker.com)**.

## Mainnet

The default node configuration is sufficient to join the mainnet:

```bash
mkdir -p ~/.aeternity/maindb
docker pull aeternity/aeternity
docker run -p 3013:3013 -p 3015:3015 \
    -v ~/.aeternity/maindb:/home/aeternity/node/data/mnesia \
    aeternity/aeternity
```

You should see the console output of the running node and a lot of information related to syncing with the network.

[Verify if your node is connected to mainnet.](operation.md#verify-that-node-is-connected-to-the-mainnet)

## Testnet

To join the testnet a network_id with value `ae_uat` argument must be passed:


```bash
mkdir -p ~/.aeternity/testdb
docker run -p 3013:3013 -p 3015:3015 \
    -e AE__FORK_MANAGEMENT__NETWORK_ID=ae_uat \
    -v ~/.aeternity/testdb:/home/aeternity/node/data/mnesia \
    aeternity/aeternity
```

You should see the console output of the running node and a lot of information related to syncing with the network.

See [how to persist the blockchain data](#persisting-data) and [how to enable mining](#mining) below.

## Localnet

To run small local network for development and testing purposes, please refer to the [localnet repository](https://github.com/aeternity/localnet)

## Docker Image

Docker image is automatically build and published on [DockerHub](https://hub.docker.com/r/aeternity/aeternity/).

Please note that all the **examples** below:

- use the Docker `-P` which [publish all exposed ports to the host interfaces](https://docs.docker.com/engine/reference/run/#expose-incoming-ports), for good network connectivity refer to [networking documentation](configuration.md#peer-to-peer-network) how to setup firewall and/or port mapping to the host machine
- run the container in [foreground mode](https://docs.docker.com/engine/reference/run/#detached-vs-foreground) for easier debugging (console output).

### Version

All releases have their own docker image tag as well. Latest release is published behind `latest` docker image tag.
Master branch of the source code is tagged as `master`.

To pull the latest release docker image run:
```bash
docker pull aeternity/aeternity
```

Always make sure you have the latest docker image prior running any of the below commands.

### Node Configuration

Тo change the node configuration, a [Docker bind mount](https://docs.docker.com/storage/bind-mounts/) should be used
to mount the configuration file to a special path on the container (`/home/aeternity/.aeternity/aeternity/aeternity.yaml`).
For example, assuming your configuration file is located at `~/.aeternity/myaeternity.yaml` on the host machine:

```bash
docker run -p 3013:3013 -p 3015:3015 \
    -v ~/.aeternity/maindb:/home/aeternity/node/data/mnesia \
    -v ~/.aeternity/myaeternity.yaml:/home/aeternity/.aeternity/aeternity/aeternity.yaml \
    aeternity/aeternity
```

Configuration can also be changed by providing environment variables to the container:

```bash
mkdir -p ~/.aeternity/testdb
docker run -p 3013:3013 -p 3015:3015 \
    -e AE__FORK_MANAGEMENT__NETWORK_ID=ae_uat \
    -v ~/.aeternity/testdb:/home/aeternity/node/data/mnesia \
    aeternity/aeternity
```

More details about node configuration can be found in [configuration documentation](configuration.md).

### Persisting Data

The blockchain data is persisted by default at `/home/aeternity/node/data/mnesia`, inside the Docker container.
In order to persist the data in a directory on the host machine, use [Docker volumes](https://docs.docker.com/engine/admin/volumes/volumes/).

Replace `~/.aeternity/myaedb` with location of your choice where the data will be stored in.

```bash
mkdir -p ~/.aeternity/myaedb
docker run -p 3013:3013 -p 3015:3015 \
  -v ~/.aeternity/myaedb:/home/aeternity/node/data/mnesia \
  aeternity/aeternity
```

** Note that you cannot switch networks using the same database **

### Mining

Mining is disabled by default. If you want to mine with docker you have to enable it in the configuration:

```yaml
# ... SNIP
mining:
    autostart: true
    beneficiary: "encoded_beneficiary_pubkey_to_be_replaced"
    cuckoo:
        miner:
            executable: lean29-generic
            extra_args: ""
            edge_bits: 29
# SNIP ...
```

The example above uses the less memory intensive lean miner, if you want to use the default (memory intensive mean) miner, remove the `mining.cuckoo` section and increase the docker container memory at least 4GB.

You also need to provide beneficiary account in the configuration, please refer to [the beneficiary section in the configuration documentation](configuration.md#beneficiary-account) how to create one if you don't have yet.

For more information see [miner configuration documentation](configuration.md#miner-configuration).
