# Join the testnet with Docker

You must have [docker installed](https://docs.docker.com/engine/installation/) on a host machine.

## Docker Image

Docker image is automatically build and published on [DockerHub](https://hub.docker.com/r/aetrnty/epoch/). All tagged source code versions have their own docker image tag as well. Latest tagged ("stable") version is tagged with `latest`.
Master branch of the source code is tagged as `dev`.

To pull the latest "stable" image run:
```bash
docker pull aetrnty/epoch
```

## Start a Node

To start a docker node and join the testnet run:
```bash
docker run -d --name epoch_node0 -p 3013:3013 aetrnty/epoch
```

Verify the node is running:
```bash
curl localhost:3013/v2/top
```

## Stop a Node

To stop a docker node run:
```bash
docker stop epoch_node0
```

## Execute command on a running node

To execute any command on a running node use:
```bash
docker exec epoch_node0 CMD
```

For example to check what's in the epoch logs run:
```bash
docker exec epoch_node0 tail log/epoch.log
```

## Configuration

### External Peer Address

Your public IP address will be automatically determined and used in `peer_address` configuration option.
Make sure you have a working port forwarding setup on your firewall to be able to fully participate in the testnet.

Docker environment variable `EXTERNAL_PEER_ADDRESS` may be used in case external peer address shall be changed e.g. different port mapping or different inbound IP:

```bash
docker run -d -p 3013:3013 -e EXTERNAL_PEER_ADDRESS=http://1.2.3.4:3013/ aetrnty/epoch
```

### Peer addresses

Docker image has packaged the address of one of the testnet nodes in the configuration. This can be changed by setting `PEERS_ADDRESS_0` Docker environment variable:

```bash
docker run -d -p 3013:3013 -e PEERS_ADDRESS_0=http://31.13.248.103:3013/ aetrnty/epoch
```

### Changing the configuration file

Assuming the new configuration file location is `~/.aeternity/myepoch.yaml`:

```bash
docker run -d -p 3013:3013 \
    -v ~/.aeternity/myepoch.yaml:/home/epoch/myepoch.yaml \
    -e EPOCH_CONFIG=/home/epoch/myepoch.yaml \
    aetrnty/epoch
```

## Persisting Data

To persist blockchain data and node keys between container runs, use [Docker volumes](https://docs.docker.com/engine/admin/volumes/volumes/). Replace `~/.aeternity/db` with location of your choice.


```bash
docker run -d -p 3013:3013 \
    -v ~/.aeternity/db:/home/epoch/node/data/mnesia \
    -v ~/.aeternity/keys:/home/epoch/node/keys \
    aetrnty/epoch
```
