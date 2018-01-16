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
docker run -d -p 3013:3013 aetrnty/epoch
```

- *Your public IP address will be automatically determined and used in `peer_address` configuration option*
- *Make sure you have a working port forwarding setup on your firewall to be able to fully participate in the testnet*

## Persisting Data

To persist blockchain data and node keys between container runs, use [Docker volumes](https://docs.docker.com/engine/admin/volumes/volumes/). Replace `~/.aeternity/.chain` with location of your choice.


```bash
docker run -d -p 3013:3013 \
    -v ~/.aeternity/.chain:/home/epoch/node/.chain \
    -v ~/.aeternity/keys:/home/epoch/node/keys \
    aetrnty/epoch
```
