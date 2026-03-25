# Running an Aeternity Node with Custom Build

## 1. Build the Docker Image

```bash
cd /path/to/aeternity
make docker
```

This compiles your current source code and produces the image `aeternity/aeternity:local`.

## 2. Download a Mainnet Snapshot (optional, speeds up sync)

```bash
rm -rf ~/.aeternity/maindb/ && mkdir -p ~/.aeternity/maindb/

curl -o ~/.aeternity/mnesia_main_v-1_latest.tar.zst \
    https://aeternity-database-backups.s3.eu-central-1.amazonaws.com/main_v1_full_20260324T133353.tar.zst

CHECKSUM=$(curl -s https://aeternity-database-backups.s3.eu-central-1.amazonaws.com/main_v1_full_20260324T133353.tar.zst.md5)

diff -qs <(echo "$CHECKSUM") <(openssl md5 -r ~/.aeternity/mnesia_main_v-1_latest.tar.zst | awk '{ print $1; }')

test $? -eq 0 && tar --use-compress-program=unzstd -xf ~/.aeternity/mnesia_main_v-1_latest.tar.zst -C ~/.aeternity/maindb/
```

If you skip the snapshot, the node will sync from genesis (takes much longer).

## 3. Fix Volume Permissions

The container runs as user `aeternity` (uid 1000), so the host directory must be writable:

```bash
chmod 777 ~/.aeternity/maindb
```

## 4. Run the Node

**Without mining:**

```bash
docker run -d --name ae-mainnet \
    -p 3013:3013 -p 3015:3015 -p 3113:3113 \
    -v ~/.aeternity/maindb:/home/aeternity/node/data/mnesia \
    aeternity/aeternity:local
```

**With mining enabled:**

```bash
docker run -d --name ae-mainnet \
    -p 3013:3013 -p 3015:3015 -p 3113:3113 \
    -e AE__MINING__AUTOSTART=true \
    -e AE__MINING__BENEFICIARY=ak_2jcBjVRQC993ko3Goh39VMCXAuUfGKnixUykTX81cqQeXu2GGB \
    -v ~/.aeternity/maindb:/home/aeternity/node/data/mnesia \
    aeternity/aeternity:local
```

## 5. Verify

```bash
curl http://localhost:3013/v3/status
```

## 6. Manage the Container

```bash
# View logs
docker logs -f ae-mainnet

# Stop and remove
docker stop ae-mainnet && docker rm ae-mainnet
```

## Ports

| Port | Purpose |
|------|---------|
| 3013 | External HTTP API |
| 3015 | P2P sync (not HTTP) |
| 3113 | Internal HTTP API |
