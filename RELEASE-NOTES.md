# About this release

[This release](https://github.com/aeternity/epoch/releases/tag/v0.3.3-big-spenders) is focused on sending tokens from an account to another.
It also marks the launch of the testnet - the public test network of nodes.

Please follow the instructions below and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).

The instructions below describe:
* How to retrieve the released software for running a node;
* How to join the testnet.

The instructions below also describe the simplest way to test sending tokens between two accounts, that is to set up a local network of two nodes - disconnected from the testnet.

## Retrieve the software for running a node

Download the [release binary](https://github.com/aeternity/epoch/releases/tag/v0.3.3-big-spenders) corresponding to your platform, e.g. `epoch-0.3.3-osx-10.12.6.tar.gz`; you would normally find the downloaded package in `~/Downloads` on macOS.

The binaries are tested on the following platforms:
* Ubuntu 16.04.3 LTS;
* macOS Sierra;
* macOS High Sierra.

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration) though the instructions below contain easy-to-use examples.

## Join the testnet

This section describes how to run a node as part of the testnet.

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet and expose [an HTTP API](https://github.com/aeternity/epoch/blob/v0.3.3-big-spenders/config/swagger.yaml).

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://31.13.248.103:3013/v1/top
* http://31.13.248.102:3013/v1/top
* http://31.13.248.105:3013/v1/top

### Setup your node

#### Deploy node

Open a Terminal window or get to the command line.

Create a directory and unpack the downloaded package:
```
mkdir /tmp/node
cd /tmp/node
tar xf ~/Downloads/epoch-0.3.3-osx-10.12.6.tar.gz
```

#### Configure node

In order for your node to join the testnet, you need to specify in the configuration file:
* The initial network peers to join (`peers` parameter);
* How peers (on the Internet) can contact your node (`http` > `external` > `peer_address` parameter).

Please notice that, if your node is behind a firewall, you need to open a TCP port in your firewall (`http` > `external` > `peer_address` parameter) and map that port to the one the node actually listens on (`http` > `external` > `port` parameter).
The two port numbers can be distinct.

The following example configuration assumes that:
* Your public IP address is `1.2.3.4`;
* The listening TCP port on that public IP address is `8080`;
* The listening TCP port on your node is `3003`.

In order for your node to manage the correct account (able to hold tokens on the blockchain), you need to specify in the configuration file the location of your public-private key pair.
The storage of the key pair by the node is basic:
* Each node handles one key pair;
* The key pair is stored on disk;
* The location of the key pair is configurable (`keys` > `dir` parameter);
* The key pair is encrypted with a configurable password stored in clear in the configuration file (`keys` > `password` parameter);
* A fresh key pair is generated if none is found in the configured location.

You do not need to create a key pair yourself: the node will generate one (`keys` > `dir` parameter) in the configured location if none found there.
After the node generates the key pair in the configured location, you should backup of that directory (and remember the password): if you destroy the node, you can setup a new node with the same account in order to lose the tokens you had obtained by mining on the chain.
You shall not share the private key (or the password) with anyone.

Create the file `/tmp/node/epoch.yaml` with the following content (amend the `http` > `external` > `peer_address` parameter and `http` > `external` > `port` parameter with your actual values):
```yaml
---
peers:
    - "http://31.13.248.102:3013/"

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
    db_path: ./db
```

Ensure the configured path for storing the blockchain exists:
```
mkdir /tmp/node/db
```

#### Start node

It is recommended that at least 1024 files can be opened in the session where you start the node: this is needed not only for files on disk (e.g. persisted chain) but also for network connections with peers.
You can check your max number of open files:
* On Ubuntu, by running `ulimit -n`;
* On macOS, by running `ulimit -n`.

If your max number of open files is lower than 1024, you are recommended to raise such configuration:
* On Ubuntu, `ulimit -n 1024` shall be sufficient;
* On macOS, `ulimit -n 1024` shall be sufficient; if not sufficient on your OS, please report also the output of `sysctl -a | grep files`.

Start the node:
```
cd /tmp/node
bin/epoch start
```

Verify the node is up, by inspecting the current top of the blockchain as seen by the node:
```
curl http://127.0.0.1:3003/v1/top
```

If the node is unresponsive, inspect the `log` directory for errors.

Backup the key pair:
```
cp -pr /tmp/node/keys ~/my_epoch_keys
```

#### Verify that node mines

Inspect the mining log file of the node:
```
less /tmp/node/log/epoch_mining.log
```

If the node is mining, you shall read log entries like the following:
```
2017-11-30 18:02:40.705 [info] <0.833.0>@aec_conductor:create_block_candidate:591 Creating block candidate
2017-11-30 18:02:40.708 [info] <0.833.0>@aec_conductor:handle_block_candidate_reply:605 Created block candidate and nonce (max 17995532323347653506, current 17995532323347653507).
2017-11-30 18:02:40.708 [info] <0.833.0>@aec_conductor:start_mining:475 Starting mining
2017-11-30 18:02:51.804 [info] <0.833.0>@aec_conductor:start_mining:475 Starting mining
```

If the node successfully mines a block, you shall read log entries like the following:
```
2017-11-30 18:04:41.131 [info] <0.833.0>@aec_conductor:handle_mining_reply:489 Miner <0.833.0> finished with {ok,{block,1,...
2017-11-30 18:04:41.138 [info] <0.833.0>@aec_conductor:save_mined_block:519 Block inserted: Height = 1
Hash = 19261bf11ff381b45c756b6fba907d9812a42c078cc71e6ef9d592d82982ea42
```

#### Verify that node connected to the testnet

Verify that your node sees the same longest blockchain as the testnet.

Inspect the current top of the blockchain as seen by the testnet:
```
curl http://31.13.248.102:3013/v1/top
```

Inspect the current top of the blockchain as seen by your node:
```
curl http://127.0.0.1:3003/v1/top
```

Verify that the height is the same; it may take a few minutes for your node to catch up with the testnet blockchain.

### Manage account

#### Retrieve your public key

Retrieve the public key of your node:
```
curl http://127.0.0.1:3103/v1/account/pub-key
```
You shall read output like the following:
```
{"pub_key":"BNngPLE0UBkgJN+z3JR6jOO5Z/7Cz9zseB1JBzexa+ru3x3y/P1hDh5BL7QRfzZ0Mb0Y7PLcVUKik7JDKE7SEo4="}
```

#### Retrieve your balance

Retrieve the balance associated to the retrieved public key (replace the public key):
```
curl -G http://127.0.0.1:3003/v1/account/balance --data-urlencode "pub_key=BNngPLE0UBkgJN+z3JR6jOO5Z/7Cz9zseB1JBzexa+ru3x3y/P1hDh5BL7QRfzZ0Mb0Y7PLcVUKik7JDKE7SEo4="
```
You shall read output like the following...
```
{"balance":80}
```
... or - if you have not yet mined a block successfully - the following:
```
{"reason":"Account not found"}
```

#### Send tokens to another account

In order to send tokens, you need to have tokens i.e. a positive (non-zero) balance.
You obtain tokens after having mined successfully.

You need to know the public key to send tokens to.

In order to instruct your node to sign and broadcast a transaction sending tokens to the public key of the other account (recipient - replace the public key):
```
curl -X POST -H "Content-Type: application/json" -d '{"recipient_pubkey":"BJ6H04FSfz0KwteJCcSNo5tXTWr5Tw9eg4QlxYAlTbPqAcmPMl2KNZ+1SsTT7PTRDNseSemh7YlNNZBx/SxCyXM=", "amount":2, "fee":1}' http://127.0.0.1:3103/v1/spend-tx
```

## Send tokens between two accounts in local network of two nodes

This section describes the simplest way for testing sending tokens, that is setting up a local network of two nodes - disconnected from the testnet.

The reason for testing spending tokens in a local network - rather than the testnet - is that in order to spend tokens from your account you need to obtain tokens in the first place, and in order to obtain tokens you need to mine a block faster than all other miners in the network.
Testing spending tokens in a local network enables obtaining tokens without competing with other miners.

### Setup minimal local network (two nodes) for testing spending tokens

In order to test sending tokens from an account to another, you need two nodes - each handling one distinct account - connected in a network.
At least one of the two nodes needs to mine - in order to have tokens to spend.

#### Deploy node #1

Open a Terminal window or get to the command line.

Create a directory and unpack the downloaded package:
```
mkdir /tmp/node1
cd /tmp/node1
tar xf ~/Downloads/epoch-0.3.3-osx-10.12.6.tar.gz
```

#### Configure node #1

Make the name of the node more specific (in order to allow running multiple nodes on the same host):
```
sed -ibkp 's/-sname epoch/-sname epoch1/g' releases/0.3.3/vm.args
```

Create the file `/tmp/node1/epoch.yaml` with the following content:
```yaml
---
keys:
    dir: keys
    password: "secret"

http:
    external:
        peer_address: http://127.0.0.1:3013/
        port: 3013
    internal:
        port: 3113

websocket:
    internal:
        port: 3114

mining:
    autostart: true

chain:
    persist: true
    db_path: ./db
```

Ensure the configured path for storing the blockchain exists:
```
mkdir /tmp/node1/db
```

The configuration above assumes that node #2 will be deployed on the same host as node #1 hence will be able to contact node #1 on `127.0.0.1` (hence the `127.0.0.1` in `peer_address`).

#### Start node #1

Start the node:
```
cd /tmp/node1
bin/epoch start
```

Verify the node is up, by inspecting the current top of the blockchain as seen by the node:
```
curl http://127.0.0.1:3013/v1/top
```

If the node is unresponsive, inspect the `log` directory for errors.

#### Verify that node #1 mines

Inspect the mining log file of node #1:
```
less /tmp/node1/log/epoch_mining.log
```

If node #1 is mining, you shall read log entries like the following:
```
2017-11-30 18:02:40.705 [info] <0.833.0>@aec_conductor:create_block_candidate:591 Creating block candidate
2017-11-30 18:02:40.708 [info] <0.833.0>@aec_conductor:handle_block_candidate_reply:605 Created block candidate and nonce (max 17995532323347653506, current 17995532323347653507).
2017-11-30 18:02:40.708 [info] <0.833.0>@aec_conductor:start_mining:475 Starting mining
2017-11-30 18:02:51.804 [info] <0.833.0>@aec_conductor:start_mining:475 Starting mining
```

If node #1 successfully mines a block, you shall read log entries like the following:
```
2017-11-30 18:04:41.131 [info] <0.833.0>@aec_conductor:handle_mining_reply:489 Miner <0.833.0> finished with {ok,{block,1,...
2017-11-30 18:04:41.138 [info] <0.833.0>@aec_conductor:save_mined_block:519 Block inserted: Height = 1
Hash = 19261bf11ff381b45c756b6fba907d9812a42c078cc71e6ef9d592d82982ea42
```

#### Verify that node #1 grows the blockchain

Inspect the current top of the blockchain as seen by node #1:
```
curl http://127.0.0.1:3013/v1/top
```
You should read output like the following (notice the height):
```
{"hash":"wIQE6fLDYRddMstJ2KJnS7WL9xobu11Hsl8yBJCA1VY=","height":5,"nonce":11228710977662701344,"pow":[3163772,12622152,13180320,15172567,23396377,27602022,32189820,37035758,37219800,45743288,46212250,50413716,50723534,51550929,52693542,53736624,60372878,62355084,62785601,63504943,66574526,68170296,69524762,71373548,73825657,75657983,79925230,81501898,92408340,93673484,96204078,97231847,97408180,99544106,100677140,113558365,116131039,121840818,122009850,123677375,127920354,128442764],"prev_hash":"/uJnyiA9VtjGyG/YF6A4JDaVjJ60tzqVkClAufN0AcE=","state_hash":"GClPJ6CP8FMLZtQ6bAUTF5NvKJ2Wzo1zaCdox82Wm7Y=","target":553713663,"time":1512065872371,"txs_hash":"Gq26FjZwTY8ESDLZ3bHHE3G+94IR3FY2yAgnMw3L8vo=","version":1}
```

Ensure the height of the chain is positive before moving forward in the instructions.

#### Verify that the account of node #1 holds tokens

As node #1 is so far mininig in isolation, a positive chain height indicates that node #1 mined at least a block.
The account of node #1 shall then have a positive balance.

In order to check the balance of node #1, retrieve its public key then the balance associated to that public key.

Retrieve the public key of node #1:
```
curl http://127.0.0.1:3113/v1/account/pub-key
```
You shall read output like the following:
```
{"pub_key":"BNngPLE0UBkgJN+z3JR6jOO5Z/7Cz9zseB1JBzexa+ru3x3y/P1hDh5BL7QRfzZ0Mb0Y7PLcVUKik7JDKE7SEo4="}
```

Retrieve the balance associated to the retrieved public key (replace the public key retrieved in the previous command):
```
curl -G http://127.0.0.1:3013/v1/account/balance --data-urlencode "pub_key=BNngPLE0UBkgJN+z3JR6jOO5Z/7Cz9zseB1JBzexa+ru3x3y/P1hDh5BL7QRfzZ0Mb0Y7PLcVUKik7JDKE7SEo4="
```
You shall read output like the following:
```
{"balance":80}
```

#### Deploy node #2

Open a Terminal window or get to the command line.

Create a directory and unpack the downloaded package:
```
mkdir /tmp/node2
cd /tmp/node2
tar xf ~/Downloads/epoch-0.3.3-osx-10.12.6.tar.gz
```

#### Configure node #2

Make the name of the node more specific (in order to allow running multiple nodes on the same host):
```
sed -ibkp 's/-sname epoch/-sname epoch2/g' releases/0.3.3/vm.args
```

Create the file `/tmp/node2/epoch.yaml` with the following content:
```yaml
---
peers:
    - "http://127.0.0.1:3013/"

keys:
    dir: keys
    password: "secret"

http:
    external:
        peer_address: http://127.0.0.1:3023/
        port: 3023
    internal:
        port: 3123

websocket:
    internal:
        port: 3124

mining:
    autostart: true

chain:
    persist: true
    db_path: ./db
```

Ensure the configured path for storing the blockchain exists:
```
mkdir /tmp/node2/db
```

#### Start node #2

Start the node:
```
cd /tmp/node2
bin/epoch start
```

Verify the node is up, by inspecting the current top of the blockchain as seen by the node:
```
curl http://127.0.0.1:3023/v1/top
```

If the node is unresponsive, inspect the `log` directory for errors.

#### Verify that nodes #1 and #2 are connected in a network

Inspect the top of the blockchain as seen by nodes #1 and #2: you shall notice that node #2 is at the same height as node #1 - meaning that node #2 synced with node #1.
```
curl http://127.0.0.1:3013/v1/top
curl http://127.0.0.1:3023/v1/top
```

### Spend tokens from account of node #1 to account of node #2

Account on node #1 has a positive balance (as check in previous instructions) hence can send tokens to the account on node #2 by knowing its public key.

Retrieve the public key of node #2:
```
curl http://127.0.0.1:3123/v1/account/pub-key
```
You shall read output like the following:
```
{"pub_key":"BJ6H04FSfz0KwteJCcSNo5tXTWr5Tw9eg4QlxYAlTbPqAcmPMl2KNZ+1SsTT7PTRDNseSemh7YlNNZBx/SxCyXM="}
```

Instruct node #1 to sign and broadcast a transaction sending tokens to the public key of node #2 (replace the public key retrieved in the previous command):
```
curl -X POST -H "Content-Type: application/json" -d '{"recipient_pubkey":"BJ6H04FSfz0KwteJCcSNo5tXTWr5Tw9eg4QlxYAlTbPqAcmPMl2KNZ+1SsTT7PTRDNseSemh7YlNNZBx/SxCyXM=", "amount":2, "fee":1}' http://127.0.0.1:3113/v1/spend-tx
```

Node #1 will share the transaction with other nodes in the block and also consider it for inclusion block for future mining.

Inspect the main log file of node #1:
```
less /tmp/node1/log/epoch.log
```
You shall read a log entry like the following:
```
2017-11-30 20:31:21.805 [info] <0.1702.0> Attempt to process operation: 'PostSpendTx'
```

Inspect the main log file of node #1:
```
less /tmp/node2/log/epoch.log
```
You shall read a log entry like the following:
```
2017-11-30 20:31:21.828 [info] <0.1644.0> Attempt to process operation: 'PostTx'
```

Retrieve the balance of node #1 until you notice that tokens have been subtracted i.e. 2 tokens to account of node #2 and 1 token to the miner - either node #1 or #2 (replace the public key retrieved by command `curl http://127.0.0.1:3113/v1/account/pub-key`):
```
curl -G http://127.0.0.1:3013/v1/account/balance --data-urlencode "pub_key=BNngPLE0UBkgJN+z3JR6jOO5Z/7Cz9zseB1JBzexa+ru3x3y/P1hDh5BL7QRfzZ0Mb0Y7PLcVUKik7JDKE7SEo4="
```
You shall read output like the following:
```
{"balance":167}
```

Retrieve the balance of node #2 (replace the public key retrieved by command `curl http://127.0.0.1:3123/v1/account/pub-key`):
```
curl -G http://127.0.0.1:3023/v1/account/balance --data-urlencode "pub_key=BJ6H04FSfz0KwteJCcSNo5tXTWr5Tw9eg4QlxYAlTbPqAcmPMl2KNZ+1SsTT7PTRDNseSemh7YlNNZBx/SxCyXM="
```
You shall read output like the following:
```
{"balance":13}
```

In this case node #2 mined the block with the transaction (it could have been node #1) so node #2 got both the 2 tokens transferred by node #1 and the 1 token by node #1 for the miner.

### Stop local network

Please remember to stop the nodes.

#### Stop node #1

Stop the node - hence its mining efforts:
```
cd /tmp/node1
bin/epoch stop
```

#### Stop node #2

Stop the node - hence its mining efforts:
```
cd /tmp/node2
bin/epoch stop
```
