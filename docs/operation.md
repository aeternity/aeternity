# Introduction

This document describes how to start your Aeternity node installed using a release binary, verify that it mines and verify that it joined the configured public network of nodes (e.g. testnet).

The instructions below assume that:

* The node is deployed in directory `~/aeternity/node`;
* beneficiary account is set under `mining` > `beneficiary` in the config (see [configuration documentation](configuration.md));
* No custom peers are specified under the `peers:` key in the config. If the `peers:` key is undefined, the *testnet* or *mainnet* seed peers (built-in in the package source) are used.
* The external HTTP endpoint of the user API of the node can be contacted at 127.0.0.1 port 3013.
* The location of the keys is set under `keys` > `dir` in the config (see [configuration documentation](configuration.md)).

If any of the assumptions does not hold, you need to amend the instructions accordingly.

## Start the node

It is recommended that the node has at least 4 GB of memory available.

When it starts, the node checks the maximum number of open files (`ulimit -n`) and warns if below the recommended limit: proper max number of open files is essential to managing network connections and you should make sure you configure it in the session where you start the node.

Start the node:
```bash
cd ~/aeternity/node
bin/aeternity start
```

(You can stop the node by running `bin/aeternity stop` from the same directory.)

Verify the node is up, by inspecting the current top of the blockchain as seen by the node:
```bash
curl http://127.0.0.1:3013/v2/blocks/top
```

If the node is unresponsive, inspect the `log` directory for errors.

Back up the peer key pair:
```bash
cp -pr ~/aeternity/node/keys ~/my_aeternity_keys
```

## Mainnet connection

To verify that node is connected to the mainnet, your node should see the same longest blockchain as the mainnet.

Inspect the current top of the blockchain as seen by the mainnet:
```bash
curl https://mainnet.aeternity.io/v2/blocks/top
```

Inspect the current top of the blockchain as seen by your node:
```bash
curl http://127.0.0.1:3013/v2/blocks/top
```

Verify that the height is the same; it may take a few minutes for your node to catch up with the mainnet blockchain.

## Mining

To verify that node mines, inspect the mining log file of the node:
```bash
less ~/aeternity/node/log/aeternity_mining.log
```

If the node is mining, you shall read log entries like the following:
```
... Creating key block candidate on the top
... Created key block candidate ...
... Starting miner ...
... Starting miner ...
```

If the node successfully mines a block, you shall read log entries like the following:
```
... Block mined: Height = 1; Hash = ...
```

## Mainnet mining

After the node is successfully connected to the mainnet, you could verify that it is mining on the same chain as the rest of the network.
You can validate it observing the `hash` of the `/blocks/top` of the remote nodes:
```bash
$ curl https://mainnet.aeternity.io/v2/blocks/top
{"key_block":{"hash":"kh_2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy","height":...}}
```

This is the hash of the block being at the top of the chain of the node and it should be same as the hash in `prev_hash` of the block you're currently mining:
```bash
$ curl http://127.0.0.1:3013/v2/key-blocks/pending
{"key_block":{...,"height":... ,"prev_hash":"kh_2UWBL9BciGC1w2FUukJZinchGRrCuwEuFTkcVvpZcfcpjiAbUy", ...}}
```
Height would be +1 of what is in the `/blocks/top` of the remote node but this is not
as strong guarantee as the `prev_hash`.
