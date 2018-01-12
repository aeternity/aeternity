# About this release

[This release](https://github.com/aeternity/epoch/releases/tag/v0.4.1.1) is focused on stability of the testnet - the public test network of nodes.

Please follow the instructions below and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/epoch/issues).

The instructions below describe:
* How to retrieve the released software for running a node;
* How to join the testnet.

The instructions below also describe the simplest way to test sending tokens between two accounts, that is to set up a local network of two nodes - disconnected from the testnet.

## Retrieve the software for running a node

Download the [release binary](https://github.com/aeternity/epoch/releases/tag/v0.4.1.1) corresponding to your platform, e.g. `epoch-0.4.1.1-osx-10.12.6.tar.gz`; you would normally find the downloaded package in `~/Downloads` on macOS.

The binaries are tested on the following platforms:
* Ubuntu 16.04.3 LTS (x86-64);
* macOS Sierra (x86-64);
* macOS High Sierra (x86-64).

The macOS package has a hard dependency on OpenSSL v1.0.0 installed with [Homebrew](https://brew.sh/) in its default path `/usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib`.
In case you have installed it in a non-default path, you could use a symlink to work around the issue.

The user configuration is documented in the [wiki](https://github.com/aeternity/epoch/wiki/User-provided-configuration) though the instructions below contain easy-to-use examples.

## Join the testnet

This section describes how to run a node as part of the testnet.

### Inspect the testnet

The core nodes of the public test network are accessible from the Internet and expose [an HTTP API](https://github.com/aeternity/epoch/blob/v0.4.1.1/config/swagger.yaml).

Information, e.g. height, of the top block of the longest chain as seen by these core nodes of the testnet can be obtained by opening in the browser any of the following URLs:
* http://31.13.248.103:3013/v1/top
* http://31.13.248.102:3013/v1/top
* http://31.13.248.105:3013/v1/top

### Setup your node

#### Deploy node

In the instructions below, the node is deployed in directory `/tmp/node`: you may prefer to deploy the node in an alternative (and less ephemeral) location - e.g. a `node` directory inside your home directory - by amending the instructions accordingly.
It is recommended that the partition where the node directory is has at least 10 GB free: this is needed for the chain and the log files.

Open a Terminal window or get to the command line.

Create a directory and unpack the downloaded package:
```
mkdir /tmp/node
cd /tmp/node
tar xf ~/Downloads/epoch-0.4.1.1-osx-10.12.6.tar.gz
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
    db_path: ./my_db
```

Ensure the configured path for storing the blockchain exists:
```
mkdir /tmp/node/my_db
```

You can validate the configuration file before starting the node:
```
cd /tmp/node
bin/epoch check_config epoch.yaml
```
You shall read output like the following:
```
Res = {ok,[{<<"chain">>,[{<<"db_path">>,<<"./my_db">>},{<<"persist">>,true}]},
           {<<"http">>,
            [{<<"external">>,
              [{<<"peer_address">>,<<"http://1.2.3.4:8080/">>},
               {<<"port">>,3003}]},
             {<<"internal">>,[{<<"port">>,3103}]}]},
           {<<"keys">>,[{<<"dir">>,<<"keys">>},{<<"password">>,<<"secret">>}]},
           {<<"mining">>,[{<<"autostart">>,true}]},
           {<<"peers">>,[<<"http://31.13.248.102:3013/">>]},
           {<<"websocket">>,[{<<"internal">>,[{<<"port">>,3104}]}]}]}
```
If the file is valid YAML but does not contain a valid configuration, it prints a helpful output.

#### Start node

It is recommended that the node has at least 4 GB of memory available.

When it starts, the node checks the maximum number of open files (`ulimit -n`) and warns if below the recommended limit: proper max number of open files is essential to managing network connections and you should make sure you configure it in the session where you start the node.

Start the node:
```
cd /tmp/node
bin/epoch start
```

(You can stop the node by running `bin/epoch stop` from the same directory.)

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
2017-12-08 16:41:54.179 [info] <0.847.0>@aec_conductor:create_block_candidate:731 Creating block candidate
2017-12-08 16:41:54.182 [info] <0.847.0>@aec_conductor:handle_block_candidate_reply:746 Created block candidate and nonce (max 12950827015446283813, current 12950827015446283814).
2017-12-08 16:41:54.182 [info] <0.847.0>@aec_conductor:start_mining:648 Starting mining
2017-12-08 16:41:57.265 [info] <0.847.0>@aec_conductor:start_mining:648 Starting mining
```

If the node successfully mines a block, you shall read log entries like the following:
```
2017-12-08 16:42:03.524 [info] <0.847.0>@aec_conductor:handle_mined_block:774 Block mined: Height = 1; Hash = 02764f5478587e34c04428e1b985925ca87a35258324412cf4749cce8e568136
```

##### Troubleshooting node failing mining attempts

If the node attempts to mine though fails to do so, you shall read error log entries in `/tmp/node/log/epoch_mining.log`.

You may read log entries in `/tmp/node/log/epoch_mining.log` like the following...
```
2018-01-03 10:18:23.812 [info] <0.903.0>@aec_conductor:create_block_candidate:728 Creating block candidate
2018-01-03 10:18:23.815 [info] <0.903.0>@aec_conductor:handle_block_candidate_reply:744 Created block candidate and nonce (max 13078180597498667020, current 13078180597498667021).
2018-01-03 10:18:23.815 [info] <0.903.0>@aec_conductor:start_mining:643 Starting mining
2018-01-03 10:18:25.871 [error] <0.903.0>@aec_conductor:handle_mining_reply:670 Failed to mine block, runtime error; retrying with different nonce (was 13078180597498667021). Error: {execution_failed,{signal,sigkill,false}}
2018-01-03 10:18:25.872 [info] <0.903.0>@aec_conductor:start_mining:643 Starting mining
2018-01-03 10:18:26.230 [error] <0.903.0>@aec_conductor:handle_mining_reply:670 Failed to mine block, runtime error; retrying with different nonce (was 13078180597498667022). Error: {execution_failed,{signal,sigabrt,true}}
2018-01-03 10:18:26.230 [info] <0.903.0>@aec_conductor:start_mining:643 Starting mining
2018-01-03 10:18:26.371 [error] <0.903.0>@aec_conductor:handle_mining_reply:670 Failed to mine block, runtime error; retrying with different nonce (was 13078180597498667023). Error: {execution_failed,{signal,sigabrt,true}}
```
... - notice "signal,sigabrt" - and you may read corresponding log entries in `/tmp/node/log/epoch_pow_cuckoo.log` like the following...
```
2018-01-03 10:18:23.816 [info] <0.913.0>@aec_pow_cuckoo:generate_int:156 Executing cmd: "env LD_LIBRARY_PATH=../lib:$LD_LIBRARY_PATH ./mean28s-generic -h uXkXZrU2tPmyYThehkTmZf6fqOuc6pvxCc87gv/BV8U=DWBQVvYHf7U= -t 5"
2018-01-03 10:18:25.859 [error] <0.913.0>@aec_pow_cuckoo:wait_for_result:362 OS process died: {signal,sigkill,false}
2018-01-03 10:18:25.880 [info] <0.1209.0>@aec_pow_cuckoo:generate_int:156 Executing cmd: "env LD_LIBRARY_PATH=../lib:$LD_LIBRARY_PATH ./mean28s-generic -h uXkXZrU2tPmyYThehkTmZf6fqOuc6pvxCc87gv/BV8U=DmBQVvYHf7U= -t 5"
2018-01-03 10:18:25.935 [error] <0.1209.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: terminate called after throwing an instance of '
2018-01-03 10:18:25.938 [error] <0.1209.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: std::bad_alloc
2018-01-03 10:18:25.939 [error] <0.1209.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: '

2018-01-03 10:18:25.940 [error] <0.1209.0>@aec_pow_cuckoo:wait_for_result:347 ERROR:   what():
2018-01-03 10:18:25.941 [error] <0.1209.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: std::bad_alloc
2018-01-03 10:18:25.942 [error] <0.1209.0>@aec_pow_cuckoo:wait_for_result:347 ERROR:

2018-01-03 10:18:25.942 [debug] <0.1209.0>@aec_pow_cuckoo:parse_generation_result:420 Looking for 42-cycle on cuckoo28("uXkXZrU2tPmyYThehkTmZf6fqOuc6pvxCc87gv/BV8U=DmBQVvYHf7U=",0) with 50% edges
2018-01-03 10:18:26.229 [error] <0.1209.0>@aec_pow_cuckoo:wait_for_result:362 OS process died: {signal,sigabrt,true}
2018-01-03 10:18:26.230 [info] <0.1211.0>@aec_pow_cuckoo:generate_int:156 Executing cmd: "env LD_LIBRARY_PATH=../lib:$LD_LIBRARY_PATH ./mean28s-generic -h uXkXZrU2tPmyYThehkTmZf6fqOuc6pvxCc87gv/BV8U=D2BQVvYHf7U= -t 5"
2018-01-03 10:18:26.233 [error] <0.1211.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: terminate called after throwing an instance of '
2018-01-03 10:18:26.234 [error] <0.1211.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: std::bad_alloc
2018-01-03 10:18:26.235 [error] <0.1211.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: '

2018-01-03 10:18:26.235 [error] <0.1211.0>@aec_pow_cuckoo:wait_for_result:347 ERROR:   what():
2018-01-03 10:18:26.235 [error] <0.1211.0>@aec_pow_cuckoo:wait_for_result:347 ERROR: std::bad_alloc
2018-01-03 10:18:26.236 [error] <0.1211.0>@aec_pow_cuckoo:wait_for_result:347 ERROR:

2018-01-03 10:18:26.236 [debug] <0.1211.0>@aec_pow_cuckoo:parse_generation_result:420 Looking for 42-cycle on cuckoo28("uXkXZrU2tPmyYThehkTmZf6fqOuc6pvxCc87gv/BV8U=D2BQVvYHf7U=",0) with 50% edges
2018-01-03 10:18:26.371 [error] <0.1211.0>@aec_pow_cuckoo:wait_for_result:362 OS process died: {signal,sigabrt,true}
```
... - notice "bad_alloc".
These are symptoms of memory allocation issues.
In presence of memory constrains, you can configure a less memory-intensive (though usually slower) algorithm than the default one.
Amend the `mining` section in file `/tmp/node/epoch.yaml` from:
```yaml
mining:
    autostart: true
```
... to ...
```yaml
mining:
    autostart: true
    cuckoo:
        miner:
            executable: lean28
            extra_args: ""
            node_bits: 28
```
... then stop and start the node (`( cd /tmp/node; bin/epoch stop; bin/epoch start; )`).

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
tar xf ~/Downloads/epoch-0.4.1.1-osx-10.12.6.tar.gz
```

#### Configure node #1

Make the name of the node more specific (in order to allow running multiple nodes on the same host):
```
sed -ibkp 's/-sname epoch/-sname epoch1/g' releases/0.4.1.1/vm.args
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
2017-12-08 16:41:54.179 [info] <0.847.0>@aec_conductor:create_block_candidate:731 Creating block candidate
2017-12-08 16:41:54.182 [info] <0.847.0>@aec_conductor:handle_block_candidate_reply:746 Created block candidate and nonce (max 12950827015446283813, current 12950827015446283814).
2017-12-08 16:41:54.182 [info] <0.847.0>@aec_conductor:start_mining:648 Starting mining
2017-12-08 16:41:57.265 [info] <0.847.0>@aec_conductor:start_mining:648 Starting mining
```

If node #1 successfully mines a block, you shall read log entries like the following:
```
2017-12-08 16:42:03.524 [info] <0.847.0>@aec_conductor:handle_mined_block:774 Block mined: Height = 1; Hash = 02764f5478587e34c04428e1b985925ca87a35258324412cf4749cce8e568136
```

#### Verify that node #1 grows the blockchain

Inspect the current top of the blockchain as seen by node #1:
```
curl http://127.0.0.1:3013/v1/top
```
You should read output like the following (notice the height):
```
{"hash":"qqPLyK093XaWOZe4YcTbcwzPHkQK5TGcU+vp5Qw3Fag=","height":5,"nonce":7821096667232785961,"pow":[340321,2769210,3673699,8891706,11786207,13326693,13672516,18699923,22569678,23898960,27664733,30643973,35647379,38895200,38980078,42616399,48396844,49052362,55932950,61508848,64933095,65724370,69325927,75341060,78321416,78959171,79339559,94170176,99040323,101302936,105773261,106189191,113842187,117224226,118763679,120291864,121221440,126743329,127038311,129825088,131231728,131287996],"prev_hash":"fXCizRK2xE6Nz6XgwqE1RB0F5qlBTndQA8FdOf2REsg=","state_hash":"YvAGFaJPchSwgqrnT8P2nU+FmIV27Dbo7g3jrrNzWq8=","target":553713663,"time":1512751562955,"txs_hash":"KY3iFlEKQ6FrTx+64YLzEuJdxQYE46Gp4/zEwroSY3I=","version":2}
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
tar xf ~/Downloads/epoch-0.4.1.1-osx-10.12.6.tar.gz
```

#### Configure node #2

Make the name of the node more specific (in order to allow running multiple nodes on the same host):
```
sed -ibkp 's/-sname epoch/-sname epoch2/g' releases/0.4.1.1/vm.args
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

Node #1 will share the transaction with other nodes in the block and also consider it for inclusion in block for future mining.

Inspect the main log file of node #1:
```
less /tmp/node1/log/epoch.log
```
You shall read a log entry like the following:
```
2017-12-08 16:49:24.057 [info] <0.1611.0> Attempt to process operation: 'PostSpendTx'
```

Inspect the main log file of node #1:
```
less /tmp/node2/log/epoch.log
```
You shall read a log entry like the following:
```
2017-12-08 16:49:24.080 [info] <0.1644.0> Attempt to process operation: 'PostTx'
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
