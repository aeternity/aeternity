# Operating an epoch node installed using a release binary

This document describes how to start your epoch node installed using a release binary, verify that it mines and verify that it joined the configured public network of nodes (e.g. testnet).

## Assumptions

The instructions below assume that:
* The node is deployed in directory `/tmp/node`;
* No custom peers are specified under the `peers:` key in the config. If the `peers:` key is undefined, the *testnet* seed peers (built-in in the package source) are used.
* The external HTTP endpoint of the user API of the node can be contacted at 127.0.0.1 port 3013.

If any of the assumptions does not hold, you need to amend the instructions accordingly.

## Instructions

### Start node

It is recommended that the node has at least 4 GB of memory available.

When it starts, the node checks the maximum number of open files (`ulimit -n`) and warns if below the recommended limit: proper max number of open files is essential to managing network connections and you should make sure you configure it in the session where you start the node.

Start the node:
```bash
cd /tmp/node
bin/epoch start
```

(You can stop the node by running `bin/epoch stop` from the same directory.)

Verify the node is up, by inspecting the current top of the blockchain as seen by the node:
```bash
curl http://127.0.0.1:3013/v2/top
```

If the node is unresponsive, inspect the `log` directory for errors.

Back up the key pair:
```bash
cp -pr /tmp/node/keys ~/my_epoch_keys
```

### Verify that node mines

Inspect the mining log file of the node:
```bash
less /tmp/node/log/epoch_mining.log
```

If the node is mining, you shall read log entries like the following:
```
... New candidate generated
... Starting mining
... Starting mining
```

If the node successfully mines a block, you shall read log entries like the following:
```
... Block mined: Height = 1; Hash = ...
```

### Verify that node connected to the testnet

Verify that your node sees the same longest blockchain as the testnet.

Inspect the current top of the blockchain as seen by the testnet:
```bash
curl http://31.13.249.70:3013/v2/top
```

Inspect the current top of the blockchain as seen by your node:
```bash
curl http://127.0.0.1:3013/v2/top
```

Verify that the height is the same; it may take a few minutes for your node to catch up with the testnet blockchain.

### Verify that node mines on same chain as the testnet

After the node is successfully connected to the testnet, you could verify that it is mining on the same chain as the rest of the network.
You can validate it observing the `hash` of the `/top` of the remote nodes:
```bash
$ curl http://31.13.249.70:3013/v2/top
{"hash":"bh$2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy","height":...}
```

This is the hash of the block being at the top of the chain of the node and it should be same as the hash in `prev_hash` of the block you're currently mining:
```bash
$ curl http://127.0.0.1:3013/v2/block/pending
{...,"height":... ,"prev_hash":"bh$2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy", ...}
```
Height would be +1 of what is in the `/top` of the remote node but this is not
as strong guarantee as the `prev_hash`.
