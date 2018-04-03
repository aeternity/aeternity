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

### Start a Node

To start a docker node and join the testnet run:
```bash
docker run -d --name epoch_node0 -p 3013:3013 aeternity/epoch
```

Verify the node is running:
```bash
curl localhost:3013/v2/top
```

#### Node arguments

Arguments can also be passed to epoch node, for example to enable API debug endpoints:
```bash
docker run -d --name epoch_node0 -p 3013:3013 aeternity/epoch -aehttp enable_debug_endpoints true
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

### Configuration

The container will use the default configuration unless other configuration file is specified (see below).

#### External Peer Address

Please note that, if your node is behind a firewall, you need to open and map a TCP port `3015` (default sync > port parameter) in your firewall to the container port `3015`.

#### Peer addresses

Docker image has packaged the addresses of testnet seed peers in the configuration. To change the peers configuration (e.g. join other network) change the configuration file (see below).

#### Changing the configuration file

Assuming the new configuration file location is `~/.aeternity/myepoch.yaml`:

```bash
docker run -d -p 3013:3013 \
    -v ~/.aeternity/myepoch.yaml:/home/epoch/myepoch.yaml \
    -e EPOCH_CONFIG=/home/epoch/myepoch.yaml \
    aeternity/epoch
```

### Persisting Data

To persist blockchain data and node keys between container runs, use [Docker volumes](https://docs.docker.com/engine/admin/volumes/volumes/). Replace `~/.aeternity/db` with location of your choice.


```bash
docker run -d -p 3013:3013 --hostname node0 \
    -v ~/.aeternity/db:/home/epoch/node/data/mnesia \
    -v ~/.aeternity/keys:/home/epoch/node/keys \
    aeternity/epoch
```

**Note: make sure `hostname` option is set when reusing the mnesia data directory**

## Localnet

Small local network (not connected to testnet) can be created with `docker-compose`.
It runs three nodes using the `mean16s-generic` miner (fastest generic miner) and a proxy server to allow CORS.

Both external and internal API are exposed to the docker host, the URL pattern is as follows:
- external API - http://$DOCKER_HOST_ADDRESS:$NODE_PORT/
- internal API - http://$DOCKER_HOST_ADDRESS:$NODE_PORT/internal

Node ports:
- `node1` - port 3001
- `node2` - port 3002
- `node3` - port 3003

For example to access `node2` public key, assuming docker host address is `localhost`:

```bash
curl http://localhost:3002/internal/v2/account/pub-key
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
