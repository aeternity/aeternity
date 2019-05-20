# Running an aeternity node on Docker

This document describes:
* [How to join the mainnet using Docker](#mainnet);
* [How to use the Docker image](#docker-image);
* [How to join the testnet using Docker](#testnet);
* [How to run a local network using Docker](#localnet).

You must have [docker installed](https://docs.docker.com/engine/installation/) on a host machine and **be familiar [how to operate docker containers](https://docs.docker.com)**.

## Mainnet

The default node configuration is sufficient to join the mainnet:

```bash
docker pull aeternity/aeternity
docker run -p 3013:3013 -p 3015:3015 aeternity/aeternity
```

You should see the console output of the running node and a lot of information related to syncing with the network.

[Verify if your node is connected to mainnet.](operation.md#verify-that-node-is-connected-to-the-mainnet)


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
docker run -p 3013:3013 -p 3015:3015 -v ~/.aeternity/myaeternity.yaml:/home/aeternity/.aeternity/aeternity/aeternity.yaml aeternity/aeternity
```

Arguments can also be passed to the node, for example to change expected mine rate:

```bash
docker run -p 3013:3013 -p 3015:3015 aeternity/aeternity -aecore expected_mine_rate 100000
```

More details about node configuration can be found in [configuration documentation](configuration.md).

### Persisting Data

The blockchain data is persisted by default, inside the Docker container.
In order to persist the data in a directory on the host machine, use [Docker volumes](https://docs.docker.com/engine/admin/volumes/volumes/).

Assuming your configuration file path is `~/.aeternity/myaeternity.yaml` on host machine.
Replace `~/.aeternity/myaedb` with location of your choice where the data will be stored in.

```bash
mkdir -p ~/.aeternity/myaedb
docker run -p 3013:3013 -p 3015:3015 \
  -v ~/.aeternity/myaeternity.yaml:/home/aeternity/.aeternity/aeternity/aeternity.yaml \
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

For more information see [miner configuration documentation](configuration#miner-configuration).

## Testnet

The minimal configuration to join the testnet needs a list of seed peers, network identifier and block reward beneficiary (with number of shares):

```yaml
---
peers:
    # UAT
    - aenode://pp_QU9CvhAQH56a2kA15tCnWPRJ2srMJW8ZmfbbFTAy7eG4o16Bf@52.10.46.160:3015
    - aenode://pp_2vhFb3HtHd1S7ynbpbFnEdph1tnDXFSfu4NGtq46S2eM5HCdbC@18.195.109.60:3015
    - aenode://pp_27xmgQ4N1E3QwHyoutLtZsHW5DSW4zneQJ3CxT5JbUejxtFuAu@13.250.162.250:3015
    - aenode://pp_DMLqy7Zuhoxe2FzpydyQTgwCJ52wouzxtHWsPGo51XDcxc5c8@13.53.161.215:3015

fork_management:
    network_id: ae_uat

chain:
    protocol_beneficiaries: ["ak_2A3PZPfMC2X7ZVy4qGXz2xh2Lbh79Q4UvZ5fdH7QVFocEgcKzU:109"]

```

Assuming your configuration file is located at `~/.aeternity/myaeternity.yaml` on the host machine:

```bash
docker run -p 3013:3013 -p 3015:3015 -v ~/.aeternity/myaeternity.yaml:/home/aeternity/.aeternity/aeternity/aeternity.yaml aeternity/aeternity
```

You should see the console output of the running node and a lot of information related to syncing with the network.

See [how to persist the blockchain data](#persisting-data) and [how to enable mining](#mining) above.


## Localnet

Small local network (*not* connected to public networks) can be created with `docker-compose`.
It runs three nodes using the `mean15-generic` miner (fastest generic miner) and a proxy server to allow CORS.
As the beneficiary key-pair is publicly available, this setup should *not* be connected to public networks.

All local network nodes are configured with the same beneficiary account (for more details on beneficiary see [configuration documentation](configuration.md#beneficiary-account)):
- public key: ak_twR4h7dEcUtc2iSEDv8kB7UFJJDGiEDQCXr85C3fYF8FdVdyo
- private key secret: `secret`
- key-pair binaries can be found [here](/docker/keys/beneficiary)

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

Docker compose uses the `aeternity/aeternity:latest` image, it will be pulled from [docker hub](https://hub.docker.com/r/aeternity/aeternity/) if it's not found locally.
To create a network with the source code in this repository, one should build a local image beforehand:

```bash
docker-compose build
```

### Mining Rate

By default the localnet has set default mine rate of 1 block per 15 seconds.
It can be changed by setting `AETERNITY_MINE_RATE` environment variable.
The variable is in milliseconds, so to set 1 block per 10 seconds use:

```bash
AETERNITY_MINE_RATE=10000 docker-compose up
```
